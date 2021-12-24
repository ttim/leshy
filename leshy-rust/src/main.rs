use std::path::Path;

mod example;
mod lang;
mod node;

fn main() {
    println!("Hello, leshy!");

    lang::parser::parse(Path::new("../examples/factorial.lsh"))
        .iter()
        .for_each(|func| println!("{:?}", func))
}
