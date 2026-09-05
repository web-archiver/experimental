use std::{
    collections::BTreeSet,
    io::{Read, Write},
};

fn main() {
    let path = {
        let mut args = std::env::args();
        args.next().unwrap();
        args.next()
    };
    let input = match path {
        Some(p) => std::fs::read(p).unwrap(),
        None => {
            let mut ret = Vec::new();
            std::io::stdin().read_to_end(&mut ret).unwrap();
            ret
        }
    };
    let data: webar_upstream_notion_fetcher::model::collect_uuids::CollectUuids =
        serde_json::from_slice(&input).unwrap();
    let ret: BTreeSet<uuid::Uuid> = BTreeSet::from_iter(data.iter().copied());
    std::io::stdout()
        .write_all(&serde_json::to_vec_pretty(&ret).unwrap())
        .unwrap();
}
