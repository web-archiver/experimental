#[inline]
pub fn has_unassigned(s: &str) -> bool {
    s.chars().any(|c| {
        icu_properties::maps::general_category().get(c)
            == icu_properties::GeneralCategory::Unassigned
    })
}

#[inline]
pub fn is_normalized(s: &str) -> bool {
    icu_normalizer::ComposingNormalizer::new_nfc().is_normalized(s)
}
