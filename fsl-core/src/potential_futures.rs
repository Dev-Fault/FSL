use futures::future::BoxFuture;

use crate::{
    error::{SpannedError, ToSpannedError},
    span::Span,
};

pub enum PotentialFuture<T, E> {
    Sync(T),
    Async(BoxFuture<'static, Result<T, E>>),
}

impl<T: 'static, E: 'static> PotentialFuture<T, E> {
    pub fn map<F, T2>(self, f: F) -> PotentialFuture<T2, E>
    where
        F: FnOnce(T) -> T2 + Send + 'static,
    {
        match self {
            PotentialFuture::Sync(t) => PotentialFuture::Sync(f(t)),
            PotentialFuture::Async(t) => {
                PotentialFuture::Async(Box::pin(async move { Ok(f(t.await?)) }))
            }
        }
    }

    pub fn map_err<F, E2>(self, f: F) -> PotentialFuture<T, E2>
    where
        F: FnOnce(E) -> E2 + Send + 'static,
    {
        match self {
            PotentialFuture::Sync(t) => PotentialFuture::Sync(t),
            PotentialFuture::Async(t) => {
                PotentialFuture::Async(Box::pin(async { t.await.map_err(f) }))
            }
        }
    }

    pub fn map_result<F, T2, E2>(self, f: F) -> PotentialFutureResult<T2, E2>
    where
        F: FnOnce(T) -> PotentialFutureResult<T2, E2> + Send + 'static,
        T: Send + 'static,
        E: Into<E2> + Send + 'static,
        E2: Send + 'static,
        T2: Send + 'static,
    {
        match self {
            PotentialFuture::Sync(t) => f(t),
            PotentialFuture::Async(t) => Ok(PotentialFuture::Async(Box::pin(async move {
                match f(t.await.map_err(|e| e.into())?)? {
                    PotentialFuture::Sync(value) => Ok(value),
                    PotentialFuture::Async(pin) => pin.await,
                }
            }))),
        }
    }
}

pub type PotentialFutureResult<T, E> = Result<PotentialFuture<T, E>, E>;

pub trait SpannedPotentialFutureResult<T> {
    fn span_future(self, span: Span) -> PotentialFutureResult<T, SpannedError>;
}

impl<T: 'static, E: ToSpannedError + 'static> SpannedPotentialFutureResult<T>
    for PotentialFutureResult<T, E>
{
    fn span_future(self, span: Span) -> PotentialFutureResult<T, SpannedError> {
        match self {
            Ok(pf) => Ok(pf.map_err(move |e| e.span(span))),
            Err(_) => self
                .map(|pf| pf.map_err(move |e| e.span(span)))
                .map_err(|e| e.span(span)),
        }
    }
}

#[macro_export]
macro_rules! execute_command {
    ($command:expr, $data:expr) => {{
        match $command.execute($data) {
            Ok(o) => match o {
                $crate::potential_futures::PotentialFuture::Sync(t) => Ok(t),
                $crate::potential_futures::PotentialFuture::Async(pin) => pin.await,
            },
            Err(e) => Err(e),
        }
    }};
}

#[macro_export]
macro_rules! potential_future {
    ($expr:expr) => {
        match $expr {
            $crate::potential_futures::PotentialFuture::Sync(value) => value,
            $crate::potential_futures::PotentialFuture::Async(pin) => pin.await?,
        }
    };
}
