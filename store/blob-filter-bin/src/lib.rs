use std::io::Read;

use anyhow::Context;

#[derive(Debug)]
struct InvalidPath;
impl std::fmt::Display for InvalidPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("invalid path")
    }
}
impl std::error::Error for InvalidPath {}

fn blob_link_path(
    // zip add_symlink_from_path is broken, see https://github.com/zip-rs/zip2/issues/969
    buf: &mut String,
    digest_path_buf: &mut webar_store_backend_fs::blob::store::PathBuf,
    path: &std::path::Path,
    digest: &webar_core::digest::Digest,
) -> Result<(), InvalidPath> {
    buf.clear();

    for comp in path.parent().ok_or(InvalidPath)?.components() {
        match comp {
            std::path::Component::CurDir => (),
            std::path::Component::ParentDir => {
                if buf.ends_with("../") {
                    buf.truncate(buf.len() - "../".len());
                }
            }
            std::path::Component::Normal(_) => {
                buf.push_str("../");
            }
            std::path::Component::RootDir => return Err(InvalidPath),
            std::path::Component::Prefix(_) => return Err(InvalidPath),
        }
    }
    buf.push_str("blob-store/");
    buf.push_str(digest_path_buf.digest_path_str(digest));

    Ok(())
}

pub trait BlobIndex {
    type Error: std::error::Error + Send + Sync + 'static;
    fn blob_exists(&mut self, digest: &webar_core::digest::Digest) -> Result<bool, Self::Error>;
}
impl BlobIndex for webar_store_backend_fs::blob::index::Handle<'_> {
    type Error = webar_store_backend_fs::blob::index::Error;
    fn blob_exists(&mut self, digest: &webar_core::digest::Digest) -> Result<bool, Self::Error> {
        self.exists(digest)
    }
}
impl<T: BlobIndex> BlobIndex for &mut T {
    type Error = T::Error;
    #[inline]
    fn blob_exists(&mut self, digest: &webar_core::digest::Digest) -> Result<bool, Self::Error> {
        T::blob_exists(self, digest)
    }
}

pub fn filter_zip<W: std::io::Write + std::io::Seek>(
    mut index: impl BlobIndex,
    input: impl std::io::Read + std::io::Seek,
    output: W,
) -> anyhow::Result<W> {
    let mut input = zip::ZipArchive::new(input).context("failed to read input file")?;
    let mut out = zip::ZipWriter::new(output);
    if !input.comment().is_empty() {
        out.set_raw_comment(input.comment().to_vec().into_boxed_slice())?;
    }
    if let Some(dat) = input.raw_zip64_extensible_data_sector() {
        out.set_raw_zip64_extensible_data_sector(dat.to_vec().into_boxed_slice());
    }
    let mut path_buf = String::new();
    let mut digest_path_buf = webar_store_backend_fs::blob::store::PathBuf::new();
    let mut symlink_target_buf = String::new();

    fn options_from_file<'a, R: std::io::Read>(
        f: &zip::read::ZipFile<'a, R>,
    ) -> anyhow::Result<zip::write::FullFileOptions<'a>> {
        let mut opt = zip::write::SimpleFileOptions::default()
            .compression_method(f.compression())
            .last_modified_time(f.last_modified().unwrap_or(zip::DateTime::DEFAULT))
            .into_full_options();
        if let Some(mode) = f.unix_mode() {
            opt = opt.unix_permissions(mode);
        }
        if !f.comment().is_empty() {
            opt = opt.with_file_comment(f.comment());
        }
        if let Some(mut d) = f.extra_data() {
            while !d.is_empty() {
                let (id, data, tail) = d
                    .split_first_chunk()
                    .and_then(|(id, tail)| {
                        let (sz, tail) = tail.split_first_chunk()?;
                        let id = u16::from_le_bytes(*id);
                        let size = u16::from_le_bytes(*sz);
                        let (data, tail) =
                            tail.split_at_checked(size as usize).unwrap_or((tail, b""));
                        Some((id, data, tail))
                    })
                    .context("failed to decode extra data")?;
                opt.add_extra_data(id, data, false)
                    .context("failed to add extra data")?;
                d = tail;
            }
        }
        Ok(opt)
    }

    for idx in 0..input.len() {
        let mut file = input.by_index(idx)?;
        println!("{}", file.name().escape_default());
        if file.is_file() {
            let mut hasher = webar_core::digest::Hasher::new();
            std::io::copy(&mut file, &mut hasher)?;
            let digest = hasher.finalize();
            if index.blob_exists(&digest)? {
                blob_link_path(
                    &mut path_buf,
                    &mut digest_path_buf,
                    file.enclosed_name().context("invalid file path")?.as_path(),
                    &digest,
                )?;
                out.add_symlink(
                    file.name(),
                    &path_buf,
                    options_from_file(&file)?
                        .compression_method(zip::CompressionMethod::Stored)
                        .compression_level(None)
                        .unix_permissions(0o777),
                )?;
            } else {
                out.start_file(file.name(), options_from_file(&file)?)?;
                std::mem::drop(file);
                std::io::copy(&mut input.by_index(idx)?, &mut out)?;
            }
        } else if file.is_dir() {
            out.add_directory(file.name(), options_from_file(&file)?)?;
        } else if file.is_symlink() {
            symlink_target_buf.clear();
            file.read_to_string(&mut symlink_target_buf)?;
            out.add_symlink(file.name(), &symlink_target_buf, options_from_file(&file)?)?;
        } else {
            unreachable!()
        }
    }

    Ok(out.finish()?)
}

#[cfg(test)]
mod test {
    mod link_path {
        use webar_core::digest::{Digest, Sha256};

        fn test_ok(path: &str, digest: Digest, expected: &str) {
            let mut buf = String::new();
            let mut digest_path_buf = webar_store_backend_fs::blob::store::PathBuf::new();
            crate::blob_link_path(&mut buf, &mut digest_path_buf, path.as_ref(), &digest).unwrap();
            assert_eq!(buf, expected);
        }

        macro_rules! test_sha256_ok {
            ($p:literal, $base:literal, $sha256:literal) => {
                test_ok(
                    $p,
                    Digest::Sha256(Sha256(hex_literal::hex!($sha256))),
                    std::concat!($base, "blob-store/sha256/", $sha256),
                )
            };
        }

        #[test]
        fn no_dir() {
            test_sha256_ok!(
                "file",
                "",
                "a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3"
            )
        }

        #[test]
        fn in_dir() {
            test_sha256_ok!(
                "dir/file",
                "../",
                "a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3"
            )
        }

        #[test]
        fn dot_in_path() {
            test_sha256_ok!(
                "dir/./file",
                "../",
                "a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3"
            )
        }

        #[test]
        fn multiple_slash() {
            test_sha256_ok!(
                "dir//file",
                "../",
                "a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3"
            )
        }

        #[test]
        fn dotdot_in_path() {
            test_sha256_ok!(
                "dir0/dir1/../file",
                "../",
                "a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3"
            )
        }
    }
}
