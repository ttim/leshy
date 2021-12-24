use std::path::Path;

mod lang;

fn main() {
    println!("Hello, leshy!");

    lang::parser::parse(Path::new("../examples/factorial.lsh"))
        .iter()
        .for_each(|func| println!("{:?}", func))
}
