#[macro_use]
extern crate failure;

mod lexer;
mod token;

mod ast;
mod parser;

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
        Err(parser::ParseError::Diag(d)) => {
            let writer =
                codespan_reporting::term::termcolor::StandardStream::stderr(
                    codespan_reporting::term::termcolor::ColorChoice::Auto,
                );
            let config = codespan_reporting::term::Config::default();
            codespan_reporting::term::emit(
                &mut writer.lock(),
                &config,
                &files,
                &d,
            ).unwrap();
        }
        _ => {
            println!("{:#?}", &ast);
        }
    };

    Ok(())
}
