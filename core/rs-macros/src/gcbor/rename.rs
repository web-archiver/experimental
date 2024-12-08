// code adopted from haskell aeson camelTo2

fn split_begin(mut input: &[char]) -> Vec<char> {
    let mut ret = Vec::new();
    while !input.is_empty() {
        match input {
            [] => break,
            [x, u, l, xs @ ..] if u.is_uppercase() && l.is_lowercase() => {
                ret.push(*x);
                ret.push('_');
                ret.push(*u);
                ret.push(*l);
                input = xs;
            }
            [x, xs @ ..] => {
                ret.push(*x);
                input = xs;
            }
        }
    }
    ret
}
fn split_after(mut input: &[char]) -> String {
    let mut ret = String::new();
    while !input.is_empty() {
        match input {
            [] => break,
            [l, u, xs @ ..] if l.is_lowercase() && u.is_uppercase() => {
                ret.push(*l);
                ret.push('_');
                ret.push(*u);
                input = xs;
            }
            [x, xs @ ..] => {
                ret.push(*x);
                input = xs;
            }
        }
    }
    ret
}

pub fn camel_to_snake(input: &str) -> String {
    split_after(&split_begin(&input.chars().collect::<Vec<_>>())).to_lowercase()
}

#[cfg(test)]
mod test {
    use super::camel_to_snake;

    #[test]
    fn sample() {
        assert_eq!(camel_to_snake("CamelAPICase").as_str(), "camel_api_case")
    }
}
