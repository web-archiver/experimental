use serde::{Deserialize, de::IgnoredAny};

#[derive(Deserialize)]
pub(crate) struct Reply {
    pub(crate) id: usize,
    pub(crate) img: String,
    pub(crate) ext: String,
}

#[derive(Deserialize)]
pub struct Thread {
    pub id: usize,
    #[serde(rename = "ReplyCount")]
    pub(crate) reply_count: usize,
    pub(crate) img: String,
    pub(crate) ext: String,
    #[serde(rename = "Replies")]
    pub(crate) replies: Vec<Reply>,
}

#[derive(Deserialize)]
#[serde(transparent)]
pub struct ForumList(IgnoredAny);

#[derive(Deserialize)]
pub struct CdnPath {
    pub(crate) rate: f32,
    pub(crate) url: String,
}
