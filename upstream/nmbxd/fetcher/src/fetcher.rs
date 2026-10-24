use std::{ffi::CStr, fmt::Write, path::PathBuf, sync::LazyLock};

use indicatif::ProgressStyle;
use tracing_indicatif::span_ext::IndicatifSpanExt;
use webar_http_lib::{error_field, http_client::MessageInfo};

type ObjData<'a> = crate::ObjectData<&'a str, &'a MessageInfo, &'a [MessageInfo]>;

const SPINNER_TEMPLATE: &str = "{span_child_prefix} {spinner} {msg}";
const BAR_TEMPLATE: &str = "{span_child_prefix} {wide_bar} {pos}/{len} {percent}% {msg}";

static SPINNER_STYLE: LazyLock<ProgressStyle> =
    LazyLock::new(|| ProgressStyle::with_template(SPINNER_TEMPLATE).unwrap());
static BAR_STYLE: LazyLock<ProgressStyle> =
    LazyLock::new(|| ProgressStyle::with_template(BAR_TEMPLATE).unwrap());

pub struct Fetcher {
    api_client: crate::client::ApiClient,
    img_client: crate::client::ImageClient,
    object_file: webar_http_lib::data_writer::CborFile,
    data_tar: webar_http_lib::data_writer::DataTar,
    path_buf: PathBuf,
    seq: usize,
}
impl Fetcher {
    pub fn new(
        api_client: crate::client::ApiClient,
        img_client: crate::client::ImageClient,
        data_name: &CStr,
        make_writer: &mut webar_http_lib::data_writer::MakeWriter,
    ) -> anyhow::Result<Self> {
        let data_writer = make_writer.new_writer(
            data_name,
            crate::PACKAGE,
            uuid::uuid!("00efac1e-32f3-4273-978a-48966046e86e"),
            &(),
            &(),
        )?;
        let data_tar = data_writer.objects_tar()?;
        let object_file = data_writer.objects_cbor()?;
        data_writer.finish()?;

        Ok(Self {
            api_client,
            img_client,
            object_file,
            data_tar,
            path_buf: PathBuf::new(),
            seq: 0,
        })
    }
    fn get_image(&mut self, img: &str, ext: &str) -> anyhow::Result<()> {
        let _span = tracing::info_span!("get_image", img, ext).entered();

        let seq = self.seq;
        self.seq += 1;

        tracing::info!("fetching image");

        let resp = match self.img_client.get_image(img, ext) {
            Ok(r) => r,
            Err(e) => {
                tracing::error!(err = error_field(&e), "failed to fetch image");
                return Ok(());
            }
        };

        self.object_file.add(&ObjData::Image(crate::ImageData {
            image: img,
            ext,
            response: &resp,
        }))?;

        self.path_buf.clear();
        let _ = std::write!(self.path_buf.as_mut_os_string(), "image/{seq:08x}");

        self.path_buf.push("name.txt");
        self.data_tar
            .add_file(self.path_buf.as_path(), img.as_bytes())?;

        self.path_buf.pop();
        let _ = std::write!(self.path_buf.as_mut_os_string(), "/data{ext}");
        self.data_tar
            .add_blob_data(self.path_buf.as_path(), resp.body_digest())?;

        Ok(())
    }

    fn fetch_thread_images(&mut self, thr: &crate::model::Thread) -> anyhow::Result<()> {
        for r in thr.replies.iter() {
            if !r.img.is_empty() {
                self.get_image(&r.img, &r.ext)?;
            }
        }
        Ok(())
    }
    pub fn fetch_thread(&mut self, id: usize) -> anyhow::Result<()> {
        let span = tracing::info_span!(
            "fetch_thread",
            id,
            indicatif.pb_show = tracing::field::Empty
        )
        .entered();

        let mut resp_buf = Vec::new();

        let seq = self.seq;
        self.seq += 1;

        span.pb_set_message("fetching thread");
        span.pb_set_style(&SPINNER_STYLE);

        tracing::info!(page = 1usize, "fetching page");
        let (msg, thr) = match self.api_client.get_thread(id, 1) {
            Ok(r) => r,
            Err(e) => {
                tracing::error!(page = 1usize, err = error_field(&e), "failed to get thread");
                return Ok(());
            }
        };
        if !thr.img.is_empty() {
            self.get_image(&thr.img, &thr.ext)?;
        }
        resp_buf.push(msg);
        self.fetch_thread_images(&thr)?;

        span.pb_set_length(thr.reply_count as u64);
        span.pb_set_position(thr.replies.len() as u64);
        span.pb_set_style(&BAR_STYLE);

        let mut page = 1;
        loop {
            page += 1;
            tracing::debug!(page, "fetching page");
            let (msg, thr) = match self.api_client.get_thread(id, page) {
                Ok(r) => r,
                Err(e) => {
                    tracing::error!(page, err = error_field(&e), "failed to get page");
                    break;
                }
            };
            resp_buf.push(msg);
            self.fetch_thread_images(&thr)?;
            let len_without_tips = thr.replies.iter().filter(|v| v.id != 9999999).count();
            span.pb_inc(len_without_tips as u64);
            if len_without_tips == 0 {
                break;
            }
        }

        self.object_file
            .add(&ObjData::Api(crate::ApiData::GetThread {
                id: id as u64,
                responses: &resp_buf,
            }))?;

        self.path_buf.clear();
        let _ = std::write!(
            self.path_buf.as_mut_os_string(),
            "api/get-thread/{seq:08x}_{id}"
        );
        self.data_tar
            .add_message_info_seq(&mut self.path_buf, "json", &resp_buf)?;

        Ok(())
    }

    pub fn list_forums(&mut self) -> anyhow::Result<()> {
        let _span = tracing::info_span!("list_forums").entered();

        let seq = self.seq;
        self.seq += 1;

        tracing::info!("fetching forums");

        let (msg, _) = match self.api_client.list_forums() {
            Ok(r) => r,
            Err(e) => {
                tracing::error!(err = error_field(&e), "failed to list forums");
                return Ok(());
            }
        };

        self.object_file
            .add(&ObjData::Api(crate::ApiData::ListForums { response: &msg }))?;

        self.path_buf.clear();
        let _ = std::write!(
            self.path_buf.as_mut_os_string(),
            "api/list-forums/{seq:08x}/data.json"
        );
        self.data_tar
            .add_blob_data(self.path_buf.as_path(), msg.body_digest())?;

        Ok(())
    }
    pub fn finish(self) -> anyhow::Result<()> {
        self.data_tar.finish()?;
        Ok(())
    }
}
