use std::{io::read_to_string, fs::File};

use zephyr_lexer::Lexer;
use zephyr_parser::Parser;
use zephyr_tycheck::type_check;

fn main() {
    let input = read_to_string(File::open("main.zhy").unwrap()).unwrap();
    let lexer = Lexer::new(input.as_str());
    let parser = Parser::new(lexer);
    let prg = parser.parse().unwrap();
    let prg = type_check(prg);
    println!("{:#?}", prg);
}
