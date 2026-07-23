use std::future::Future;

pub mod builder;

pub trait Service<Req> {
    type Response;
    type Error;
    type Future: Future<Output = Result<Self::Response, Self::Error>>;

    fn call(&self, req: Req) -> Self::Future;
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

pub trait OnceLayer<S> {
    type Service;
    fn layer_once(self, inner: S) -> Self::Service;
}
