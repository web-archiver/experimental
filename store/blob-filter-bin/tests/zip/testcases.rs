use std::{collections::BTreeSet, convert::Infallible, io::Write, num::NonZeroU32};

use webar_core::{bytes::Bytes, digest::Digest};

pub struct TestIndex(BTreeSet<Digest>);
impl webar_archive_blob_filter_bin::BlobIndex for TestIndex {
    type Error = Infallible;
    fn blob_exists(&mut self, digest: &webar_core::digest::Digest) -> Result<bool, Self::Error> {
        Ok(self.0.contains(digest))
    }
}

pub struct TestCase {
    pub index: TestIndex,
    pub input: Vec<u8>,
    pub output: Vec<u8>,
}
#[allow(dead_code)]
impl TestCase {
    pub fn test(mut self) {
        let ret = webar_archive_blob_filter_bin::filter_zip(
            &mut self.index,
            std::io::Cursor::new(self.input),
            std::io::Cursor::new(Vec::with_capacity(self.output.len())),
        )
        .unwrap()
        .into_inner();
        assert_eq!(
            Bytes::new(ret.as_slice()),
            Bytes::new(self.output.as_slice())
        );
    }
}

#[derive(Clone, Copy)]
struct ExtraData<'a> {
    id: u16,
    data: &'a [u8],
}
#[derive(Clone, Copy)]
struct ZipOptions<'ed, 'd> {
    system: zip::System,
    compression: zip::CompressionMethod,
    compression_level: Option<i64>,
    mode: Option<NonZeroU32>,
    comment: Option<&'d str>,
    extra_data: &'ed [ExtraData<'d>],
}
impl<'ed, 'd> ZipOptions<'ed, 'd> {
    const fn new() -> Self {
        Self {
            system: zip::System::Unix,
            compression: zip::CompressionMethod::Stored,
            compression_level: None,
            mode: None,
            comment: None,
            extra_data: &[],
        }
    }
    fn to_in_options(self) -> zip::write::FullFileOptions<'static> {
        let mut opt = zip::write::FullFileOptions::default()
            .compression_method(self.compression)
            .compression_level(self.compression_level)
            .system(self.system)
            .last_modified_time(zip::DateTime::DEFAULT);
        if let Some(m) = self.mode {
            opt = opt.unix_permissions(m.get());
        }
        if let Some(c) = self.comment {
            opt = opt.with_file_comment(c.to_string().into_boxed_str());
        }
        for d in self.extra_data {
            opt.add_extra_data(d.id, d.data, false).unwrap();
        }
        opt
    }
    fn to_out_options(self) -> zip::write::FullFileOptions<'static> {
        let mut opt = zip::write::FileOptions::default()
            .compression_method(self.compression)
            .last_modified_time(zip::DateTime::DEFAULT);
        if let Some(m) = self.mode {
            opt = opt.unix_permissions(m.get());
        }
        if let Some(c) = self.comment {
            opt = opt.with_file_comment(c.to_string().into_boxed_str());
        }
        for d in self.extra_data {
            opt.add_extra_data(d.id, d.data, false).unwrap()
        }
        opt
    }
}

struct TestBuilder {
    index: BTreeSet<Digest>,
    path_buf: String,
    store_path_buf: webar_store_backend_fs::blob::store::PathBuf,
    input: zip::ZipWriter<std::io::Cursor<Vec<u8>>>,
    output: zip::ZipWriter<std::io::Cursor<Vec<u8>>>,
}
#[allow(dead_code)]
impl TestBuilder {
    fn new() -> Self {
        Self {
            index: BTreeSet::new(),
            path_buf: String::new(),
            store_path_buf: webar_store_backend_fs::blob::store::PathBuf::new(),
            input: zip::ZipWriter::new(std::io::Cursor::new(Vec::new())),
            output: zip::ZipWriter::new(std::io::Cursor::new(Vec::new())),
        }
    }
    fn set_archive_comment(&mut self, comment: &str) {
        self.input.set_comment(comment).unwrap();
        self.output.set_comment(comment).unwrap();
    }
    fn add_dir(&mut self, path: &str, opt: ZipOptions<'_, '_>) {
        self.input.add_directory(path, opt.to_in_options()).unwrap();
        self.output
            .add_directory(path, opt.to_out_options())
            .unwrap();
    }
    fn add_symlink(&mut self, path: &str, target: &str, opt: ZipOptions<'_, '_>) {
        self.input
            .add_symlink(path, target, opt.to_in_options())
            .unwrap();
        self.output
            .add_symlink(path, target, opt.to_out_options())
            .unwrap();
    }
    fn add_replaced_file(
        &mut self,
        path: &str,
        link_base: &str,
        content: &[u8],
        opt: ZipOptions<'_, '_>,
    ) {
        self.input.start_file(path, opt.to_in_options()).unwrap();
        self.input.write_all(content).unwrap();

        let digest = Digest::hash_buf(content);

        self.path_buf.clear();
        self.path_buf.push_str(link_base);
        self.path_buf.push('/');
        self.path_buf
            .push_str(self.store_path_buf.digest_path_str(&digest));

        self.output
            .add_symlink(
                path,
                self.path_buf.as_str(),
                opt.to_out_options()
                    .compression_method(zip::CompressionMethod::Stored)
                    .compression_level(None)
                    .unix_permissions(0o777),
            )
            .unwrap();

        self.index.insert(digest);
    }
    fn add_regular(&mut self, path: &str, content: &[u8], opt: ZipOptions<'_, '_>) {
        self.input.start_file(path, opt.to_in_options()).unwrap();
        self.input.write_all(content).unwrap();
        self.output.start_file(path, opt.to_out_options()).unwrap();
        self.output.write_all(content).unwrap();
    }
    fn build(self) -> TestCase {
        TestCase {
            index: TestIndex(self.index),
            input: self.input.finish().unwrap().into_inner(),
            output: self.output.finish().unwrap().into_inner(),
        }
    }

    fn build_fn(f: impl FnOnce(&mut Self)) -> TestCase {
        let mut ret = Self::new();
        f(&mut ret);
        ret.build()
    }
}

pub fn dir_plain() -> TestCase {
    TestBuilder::build_fn(|b| {
        b.add_dir("dir", ZipOptions::new());
    })
}
pub fn dir_with_comment() -> TestCase {
    TestBuilder::build_fn(|b| {
        b.add_dir(
            "dir",
            ZipOptions {
                system: zip::System::Dos,
                comment: Some("comment"),
                ..(ZipOptions::new())
            },
        );
    })
}
pub fn dir_with_extra_data() -> TestCase {
    TestBuilder::build_fn(|b| {
        b.add_dir(
            "dir",
            ZipOptions {
                extra_data: &[
                    ExtraData {
                        id: 4,
                        data: b"data3",
                    },
                    ExtraData {
                        id: 5,
                        data: b"data4",
                    },
                ],
                ..(ZipOptions::new())
            },
        );
    })
}

pub fn symlink_plain() -> TestCase {
    TestBuilder::build_fn(|b| {
        b.add_symlink("sym_link", "symlink_target0", ZipOptions::new());
    })
}
pub fn symlink_with_comment() -> TestCase {
    TestBuilder::build_fn(|b| {
        b.add_symlink(
            "symlink",
            "symlink_target0",
            ZipOptions {
                comment: Some("comment"),
                ..ZipOptions::new()
            },
        );
    })
}
pub fn symlink_multi() -> TestCase {
    TestBuilder::build_fn(|b| {
        b.add_symlink("link0", "link_target0", ZipOptions::new());
        b.add_symlink("link1", "link_target1", ZipOptions::new());
    })
}

pub fn regular_keep0() -> TestCase {
    TestBuilder::build_fn(|b| {
        b.add_regular("file_keep0", b"content", ZipOptions::new());
    })
}
pub fn regular_keep_with_comment() -> TestCase {
    TestBuilder::build_fn(|b| {
        b.add_regular(
            "file_keep0",
            b"content",
            ZipOptions {
                comment: Some("comment0"),
                ..ZipOptions::new()
            },
        );
    })
}
pub fn regular_replace0() -> TestCase {
    TestBuilder::build_fn(|b| {
        b.add_replaced_file(
            "file_replace0",
            "blob-store",
            b"replaced_content0",
            ZipOptions::new(),
        );
    })
}
pub fn regular_replaced_with_comment() -> TestCase {
    TestBuilder::build_fn(|b| {
        b.add_replaced_file(
            "file_replaced",
            "blob-store",
            b"replaced_content0",
            ZipOptions {
                comment: Some("comment"),
                ..ZipOptions::new()
            },
        );
    })
}
pub fn regular_replaced_in_dir() -> TestCase {
    TestBuilder::build_fn(|b| {
        b.add_dir("dir", ZipOptions::new());
        b.add_replaced_file(
            "dir/replaced",
            "../blob-store",
            b"replaced-content0",
            ZipOptions::new(),
        );
    })
}

pub fn file_multi() -> TestCase {
    TestBuilder::build_fn(|b| {
        b.add_replaced_file(
            "replaced0",
            "blob-store",
            b"replaced-content0",
            ZipOptions::new(),
        );
        b.add_dir("dir0", ZipOptions::new());
        b.add_dir("dir0/nested_dir", ZipOptions::new());
        b.add_regular("dir0/keep0", b"keep-content0", ZipOptions::new());
        b.add_replaced_file(
            "dir0/replaced0",
            "../blob-store",
            b"replaced-content-dir0",
            ZipOptions::new(),
        );
    })
}
