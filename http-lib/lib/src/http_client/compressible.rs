use std::str::FromStr;

use http::header;

fn check_mime(ctx: &'static str, m_str: &str) -> Option<bool> {
    if let Some(r) = mime_data::lookup_mime_info(m_str).and_then(|d| d.compressible) {
        return Some(r);
    }
    let m = match mime::Mime::from_str(m_str) {
        Ok(m) => m,
        Err(e) => {
            tracing::warn!(
                mime = m_str,
                context = ctx,
                err = &e as &dyn std::error::Error,
                "invalid mime type in {ctx}: {e}"
            );
            return None;
        }
    };
    if m.type_().as_str() == "text" {
        return Some(true);
    }
    if let Some(s) = m.suffix() {
        match s.as_str() {
            "json" | "yaml" | "text" | "xml" => return Some(true),
            "zip" | "rar" | "gzip" => return Some(false),
            _ => (),
        }
    }
    None
}

static INFER: infer::Infer = infer::Infer::new();

pub fn check(hdr: &header::HeaderMap, body: &[u8]) -> Option<bool> {
    if hdr.contains_key(header::CONTENT_ENCODING) {
        return Some(true);
    }
    if let Some(ct) = hdr.get(header::CONTENT_TYPE) {
        match ct.to_str() {
            Ok(c) => {
                if let ret @ Some(_) = check_mime("content-type", c) {
                    return ret;
                }
            }
            Err(e) => tracing::warn!(
                err = &e as &dyn std::error::Error,
                "content-type \"{}\" is not string: {e}",
                ct.as_bytes().escape_ascii()
            ),
        }
    }
    if let Some(ty) = INFER.get(body) {
        if let ret @ Some(_) = check_mime("infer-body", ty.mime_type()) {
            return ret;
        }
    }
    None
}
