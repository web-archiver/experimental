use crate::service::OnceLayer;

pub struct Identity;
impl<S> OnceLayer<S> for Identity {
    type Service = S;
    fn layer_once(self, inner: S) -> Self::Service {
        inner
    }
}
pub struct Stack<Inner, Outer> {
    inner: Inner,
    outer: Outer,
}
impl<I, O> Stack<I, O> {
    pub fn new(inner: I, outer: O) -> Self {
        Self { inner, outer }
    }
    pub fn layer<L>(self, outer: L) -> Stack<Self, L> {
        Stack { inner: self, outer }
    }
}
impl<Inner, Outer, S> OnceLayer<S> for Stack<Inner, Outer>
where
    Inner: OnceLayer<S>,
    Outer: OnceLayer<Inner::Service>,
{
    type Service = Outer::Service;
    fn layer_once(self, inner: S) -> Self::Service {
        self.outer.layer_once(self.inner.layer_once(inner))
    }
}

pub struct ServiceBuilder<T>(T);
impl<S> ServiceBuilder<S> {
    pub fn new(inner: S) -> Self {
        Self(inner)
    }
    pub fn once_layer<Outer>(self, l: Outer) -> ServiceBuilder<Outer::Service>
    where
        Outer: OnceLayer<S>,
    {
        ServiceBuilder(l.layer_once(self.0))
    }
    pub fn arc(self) -> ServiceBuilder<std::sync::Arc<S>> {
        ServiceBuilder(std::sync::Arc::new(self.0))
    }
    pub fn build(self) -> S {
        self.0
    }
}
