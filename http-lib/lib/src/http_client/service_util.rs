#![allow(dead_code)]

#[derive(Debug, Clone)]
pub struct ToTowerService<S>(pub S);
impl<S, Req> tower_service::Service<Req> for ToTowerService<S>
where
    S: webar_core::service::Service<Req>,
{
    type Response = S::Response;
    type Error = S::Error;
    type Future = S::Future;
    #[inline]
    fn poll_ready(
        &mut self,
        _: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        std::task::Poll::Ready(Ok(()))
    }
    #[inline]
    fn call(&mut self, req: Req) -> Self::Future {
        self.0.call(req)
    }
}
