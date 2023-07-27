use std::{io::read_to_string, fs::File, env::args};

use zephyr_lexer::Lexer;
use zephyr_parser::Parser;
use zephyr_span::Spannable;
use zephyr_tycheck::type_check;

fn main() {
    let filename = args().nth(1).unwrap();
    let input = read_to_string(File::open(filename).unwrap()).unwrap();
    let lexer = Lexer::new(input.as_str());
    let parser = Parser::new(lexer);
    let prg = match parser.parse() {
        Ok(prg) => prg,
        Err(e) => {
            println!("{}", &input[e.span().offset()..e.span().offset() + e.span().len()]);
            return;
        }
    };
    let prg = type_check(prg);
    println!("{:#?}", prg);
}
