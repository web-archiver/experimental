/*! String in normal form

Currently only ascii character is allowed.
*/
use std::{fmt::Display, mem::transmute};

pub use crate::nf_str;

#[derive(Debug, thiserror::Error)]
#[error("Str contains non-ascii character")]
pub struct NFStrError();

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct NFStr(str);
impl Display for NFStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}
impl NFStr {
    pub fn new(s: &str) -> Result<&Self, NFStrError> {
        Self::new_const(s)
    }
    pub const fn as_str(&self) -> &str {
        &self.0
    }
    pub fn to_nf_string(&self) -> NFString {
        NFString(self.0.to_owned())
    }

    const fn new_const(s: &str) -> Result<&Self, NFStrError> {
        if s.is_ascii() {
            Ok(unsafe { transmute::<&str, &Self>(s) })
        } else {
            Err(NFStrError())
        }
    }

    #[doc(hidden)]
    pub const fn new_const_throw(s: &str) -> &Self {
        match Self::new_const(s) {
            Ok(v) => v,
            Err(_) => panic!("invalid normalized string"),
        }
    }
}

#[macro_export]
macro_rules! nf_str {
    ($l:expr) => {
        const { $crate::text::normalized::NFStr::new_const_throw($l) }
    };
}

#[derive(Debug, thiserror::Error)]
#[error("String contains non-ascii character")]
pub struct NFStringError();
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NFString(String);
impl Display for NFString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0.as_str())
    }
}
impl NFString {
    pub fn from_string(s: String) -> Result<Self, NFStringError> {
        if s.is_ascii() {
            Ok(Self(s))
        } else {
            Err(NFStringError())
        }
    }
    pub fn as_str(&self) -> &str {
        &self.0
    }
    pub fn as_nf_str(&self) -> &NFStr {
        unsafe { transmute(self.0.as_str()) }
    }
}
