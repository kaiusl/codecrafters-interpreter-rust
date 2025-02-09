#![deny(rust_2018_idioms)]

use std::env;
use std::fs;

mod interpreter;
mod lexer;
mod parser;

use self::lexer::Lexer;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            // You can use print statements as follows for debugging, they'll be visible when running tests.
            eprintln!("Logs from your program will appear here!");

            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            let lexer = Lexer::new(&file_contents);
            let mut had_lexical_errors = false;
            for token in lexer {
                match token {
                    Ok(token) => println!("{}", token.fmt_as_book()),
                    Err(err) => {
                        had_lexical_errors = true;
                        eprintln!("{}", err)
                    }
                }
            }

            if had_lexical_errors {
                std::process::exit(65);
            }
        }
        "parse" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });
            let lexer = Lexer::new(&file_contents);
            let mut parser = parser::Parser::new(lexer);
            let ast = parser.parse_expr();
            match ast {
                Ok(ast) => println!("{}", ast),
                Err(err) => {
                    eprintln!("{}", err);
                    std::process::exit(65);
                }
            }
        }
        "evaluate" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            match interpreter::eval(&file_contents) {
                Ok(Ok(_)) => {
                    // interpreter prints the result itselt
                }
                Ok(Err(err)) => {
                    // runtime error
                    eprintln!("{}", err);
                    std::process::exit(70);
                }
                Err(err) => {
                    // lexer/parser error
                    eprintln!("{}", err);
                    std::process::exit(65);
                }
            }
        }
        "run" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });
            let result = interpreter::interpret(&file_contents);
            match result {
                Ok(Ok(_)) => {}
                Ok(Err(err)) => {
                    // runtime error
                    eprintln!("{}", err);
                    std::process::exit(70);
                }
                Err(err) => {
                    // lexer/parser error
                    for err in err {
                        eprintln!("{}", err);
                    }
                    std::process::exit(65);
                }
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command)
        }
    }
}
