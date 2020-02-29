#[macro_use]
extern crate failure;

mod lexer;
mod token;

mod ast;
mod parser;

mod vm;

use clap::Clap;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

#[derive(Debug, Clap)]
#[clap(name = "lox")]
struct Opt {
    #[clap(long, help = "Dump the tokens")]
    dump_tokens: bool,

    #[clap(long, help = "Dump the AST")]
    dump_ast: bool,

    #[clap(name = "FILE", parse(from_os_str))]
    file: PathBuf,
}

fn main() -> Result<(), failure::Error> {
    let opt = Opt::parse();
    let mut file = File::open(&opt.file)?;
    let mut src = String::new();
    file.read_to_string(&mut src)?;

    let mut files: codespan::Files<&str> = codespan::Files::new();
    let file_id = files.add(&opt.file, &src);

    if opt.dump_tokens {
        let mut lexer = lexer::Lexer::new(&src);
        while lexer.token.kind != token::TokenKind::Eof {
            lexer.advance();
            println!("{:?}", lexer.token);
        }
    }

    let ast = parser::Parser::parse(&files, file_id);
    match ast {
        Err(err) => {
            err.emit(&files);
            return Err(err.into());
        }
        _ => {
            if opt.dump_ast {
                println!("{:#?}", &ast);
            }
        }
    };

    Ok(())
}
