use std::future::Future;

pub mod builder;

pub trait Service<Req> {
    type Response;
    type Error;
    type Future: Future<Output = Result<Self::Response, Self::Error>>;

    fn call(&self, req: Req) -> Self::Future;
}
pub trait AsyncService<Req> {
    type Response;
    type Error;
    fn call_async(
        &self,
        req: Req,
    ) -> impl Future<Output = Result<Self::Response, Self::Error>> + Send;
}

impl<S: ?Sized, Req> Service<Req> for Box<S>
where
    S: Service<Req>,
{
    type Response = S::Response;
    type Error = S::Error;
    type Future = S::Future;
    #[inline]
    fn call(&self, req: Req) -> Self::Future {
        S::call(self, req)
    }
}
impl<S: ?Sized, Req> AsyncService<Req> for Box<S>
where
    S: AsyncService<Req>,
{
    type Response = S::Response;
    type Error = S::Error;
    #[inline]
    fn call_async(&self, req: Req) -> impl Future<Output = Result<Self::Response, Self::Error>> {
        S::call_async(self, req)
    }
}
impl<S: ?Sized, Req> Service<Req> for std::rc::Rc<S>
where
    S: Service<Req>,
{
    type Response = S::Response;
    type Error = S::Error;
    type Future = S::Future;
    #[inline]
    fn call(&self, req: Req) -> Self::Future {
        S::call(self, req)
    }
}
impl<S: ?Sized, Req> AsyncService<Req> for std::rc::Rc<S>
where
    S: AsyncService<Req>,
{
    type Response = S::Response;
    type Error = S::Error;
    #[inline]
    fn call_async(&self, req: Req) -> impl Future<Output = Result<Self::Response, Self::Error>> {
        S::call_async(self, req)
    }
}
impl<S: ?Sized, Req> Service<Req> for std::sync::Arc<S>
where
    S: Service<Req>,
{
    type Response = S::Response;
    type Error = S::Error;
    type Future = S::Future;
    #[inline]
    fn call(&self, req: Req) -> Self::Future {
        S::call(self, req)
    }
}
impl<S: ?Sized, Req> AsyncService<Req> for std::sync::Arc<S>
where
    S: AsyncService<Req>,
{
    type Response = S::Response;
    type Error = S::Error;
    #[inline]
    fn call_async(&self, req: Req) -> impl Future<Output = Result<Self::Response, Self::Error>> {
        S::call_async(self, req)
    }
}

pub trait OnceLayer<S> {
    type Service;
    fn layer_once(self, inner: S) -> Self::Service;
}
