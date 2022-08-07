use crate::scanner::{Scanner, Token, TokenType};

pub struct Compiler<'src> {
    pub scanner: Scanner<'src>,
}

impl<'src> Compiler<'src> {
    pub fn new(source: &'src str) -> Self {
        let scanner = Scanner::new(source);
        Compiler { scanner }
    }

    pub fn compile(&mut self) {
        let mut line = 0;

        loop {
            let token = self.scanner.scan_token();

            if token.line != line {
                print!("{:4} ", token.line);
                line = token.line;
            } else {
                print!("   | ");
            }

            println!("{:2?} '{}'", token.token_type, token.lexeme);

            if token.token_type == TokenType::Eof {
                break;
            }
        }
    }
}
