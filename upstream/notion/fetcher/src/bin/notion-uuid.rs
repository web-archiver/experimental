fn main() {
    let i = {
        let mut a = std::env::args();
        a.next().unwrap();
        a.next().expect("missing input")
    };
    let u = uuid::Uuid::parse_str(&i).unwrap();
    println!("{}\n{}", u.as_hyphenated(), u.as_simple());
}
