#[derive(serde::Deserialize)]
#[serde(rename_all = "snake_case")]
enum SameSite {
    NoRestriction,
    Lax,
    Strict,
}

#[derive(serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct Cookie<'a> {
    // double slash and double quote are not allowed in cookie name and value
    name: &'a str,
    value: &'a str,
    domain: &'a str,
    path: &'a str,
    secure: bool,
    http_only: bool,
    same_site: Option<SameSite>,
    session: bool,
    expiration_date: Option<i64>,
}

#[derive(Debug, thiserror::Error)]
pub(super) enum Error {
    #[error("failed to set url host: {0}")]
    SetHost(#[source] url::ParseError),
    #[error("failed to parse json: {0}")]
    Json(#[source] serde_json::Error),
    #[error("invalid expiration date: {0}")]
    InvalidExpiration(i64),
    #[error("failed to insert cookie: {0}")]
    Store(#[source] cookie_store::CookieError),
}

pub(super) fn import_json(store: &mut cookie_store::CookieStore, json: &[u8]) -> Result<(), Error> {
    let mut u = url::Url::parse("https://example.com").unwrap();
    for c in serde_json::from_slice::<Vec<Cookie<'_>>>(json).map_err(Error::Json)? {
        let mut builder = cookie_store::RawCookie::build((c.name, c.value))
            .domain(c.domain)
            .path(c.path)
            .secure(c.secure);
        if let Some(ss) = c.same_site {
            builder = builder.same_site(match ss {
                SameSite::NoRestriction => cookie::SameSite::None,
                SameSite::Lax => cookie::SameSite::Lax,
                SameSite::Strict => cookie::SameSite::Strict,
            });
        }
        if c.session {
            builder = builder.expires(cookie::Expiration::Session)
        } else if let Some(e) = c.expiration_date {
            builder = builder.expires(cookie::Expiration::DateTime(
                time::OffsetDateTime::from_unix_timestamp(e)
                    .map_err(|_| Error::InvalidExpiration(e))?,
            ))
        }
        u.set_host(Some(c.domain)).map_err(Error::SetHost)?;
        store
            .insert_raw(&builder.http_only(c.http_only).build(), &u)
            .map_err(Error::Store)?;
    }
    Ok(())
}
