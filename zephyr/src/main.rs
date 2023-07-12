use std::io::read_to_string;

use zephyr_lexer::Lexer;
use zephyr_parser::Parser;

fn main() {
    let input = read_to_string(std::io::stdin()).unwrap();
    let lexer = Lexer::new(input.as_str());
    let parser = Parser::new(lexer);
    println!("{:#?}", parser.parse());
}
