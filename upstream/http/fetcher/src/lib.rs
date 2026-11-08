use std::{ffi::CStr, fmt::Write, path::PathBuf};

use webar_core::codec::gcbor::GCborCodec;
use webar_http_lib::http_client::MessageInfo;

#[derive(GCborCodec)]
pub struct HttpData<U, R> {
    url: U,
    response: R,
}

pub const PACKAGE: webar_core::object::Package<&'static str> = webar_core::object::Package {
    id: uuid::uuid!("c8d716a5-e078-49d9-bb6f-5c2e59d01f3a"),
    name: "webar.upstream.http",
    version: webar_core::object::Version(1, 0),
};

#[derive(Debug, thiserror::Error)]
enum InnerClientError {
    #[error("url parse error: {0}")]
    Url(#[source] webar_http_lib::http_client::UrlParseError),
    #[error("invalid host header: {0}")]
    InvalidHost(#[source] http::header::InvalidHeaderValue),
    #[error("missing host")]
    MissingHost,
    #[error("request build error: {0}")]
    Request(#[source] http::Error),
    #[error("http error: {0}")]
    Http(#[source] webar_http_lib::http_client::HttpError),
}
#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct ClientError(Box<InnerClientError>);
impl From<InnerClientError> for ClientError {
    fn from(value: InnerClientError) -> Self {
        Self(Box::new(value))
    }
}
#[derive(Debug, thiserror::Error)]
enum InnerFetchError {
    #[error("client error: {0}")]
    Client(#[source] ClientError),
    #[error("io error: {0}")]
    Io(#[source] std::io::Error),
}
#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct FetchError(#[from] InnerFetchError);
impl FetchError {
    pub fn into_client_error(self) -> Result<ClientError, Self> {
        match self.0 {
            InnerFetchError::Client(e) => Ok(e),
            e => Err(Self(e)),
        }
    }
}

pub struct Fetcher {
    seq: usize,
    data_tar: webar_http_lib::data_writer::DataTar,
    object_file: webar_http_lib::data_writer::CborFile,
    runtime: tokio::runtime::Handle,
    client: webar_http_lib::http_client::Client,
    path_buf: PathBuf,
}
impl Fetcher {
    pub fn new(
        client: webar_http_lib::http_client::Client,
        runtime: tokio::runtime::Handle,
        data_name: &CStr,
        make_writer: &mut webar_http_lib::data_writer::MakeWriter,
    ) -> anyhow::Result<Self> {
        let data_writer = make_writer.new_writer(
            data_name,
            PACKAGE,
            uuid::uuid!("25853ac6-e242-4dc1-89e9-26439f3c7bc8"),
            &(),
            &(),
        )?;
        let data_tar = data_writer.objects_tar()?;
        let object_file = data_writer.objects_cbor()?;
        data_writer.finish()?;
        Ok(Self {
            seq: 0,
            data_tar,
            object_file,
            client,
            runtime,
            path_buf: PathBuf::new(),
        })
    }
    pub fn get_url_str(
        &mut self,
        url: &str,
    ) -> Result<webar_http_lib::http_client::Response, FetchError> {
        fn call_client(
            rt: &tokio::runtime::Handle,
            client: &mut webar_http_lib::http_client::Client,
            url: &str,
        ) -> Result<webar_http_lib::http_client::Response, ClientError> {
            let url =
                webar_http_lib::http_client::Url::parse(url).map_err(InnerClientError::Url)?;
            let host = url
                .host_header()
                .map_err(InnerClientError::InvalidHost)?
                .ok_or_else(|| ClientError::from(InnerClientError::MissingHost))?;
            let req = client
                .request(http::Method::GET, url)
                .header(http::header::HOST, host)
                .build(webar_http_lib::http_client::ReqBody::empty())
                .map_err(InnerClientError::Request)?;
            Ok(rt
                .block_on(client.execute(req))
                .map_err(InnerClientError::Http)?)
        }
        let seq = self.seq;
        self.seq += 1;

        let resp =
            call_client(&self.runtime, &mut self.client, url).map_err(InnerFetchError::Client)?;

        self.object_file
            .add(&HttpData {
                url,
                response: MessageInfo::new(&resp),
            })
            .map_err(InnerFetchError::Io)?;

        self.path_buf.clear();
        let _ = std::write!(self.path_buf.as_mut_os_string(), "http/{seq:08x}");

        self.path_buf.push("url.txt");
        self.data_tar
            .add_file(self.path_buf.as_path(), url.as_bytes())
            .map_err(InnerFetchError::Io)?;

        self.path_buf.set_file_name("response");
        self.data_tar
            .add_blob_data(self.path_buf.as_path(), resp.body_digest())
            .map_err(InnerFetchError::Io)?;

        Ok(resp)
    }
    pub fn finish(self) -> anyhow::Result<()> {
        self.data_tar.finish()?;
        Ok(())
    }
}
