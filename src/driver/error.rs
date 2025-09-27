use termion::color;

use crate::parser::{ParseError, Spanned};
use crate::runtime::RuntimeError;

#[derive(Debug)]
pub enum InterpreterError {
    RuntimeError(RuntimeError),
    ParseError(ParseError)
}

impl InterpreterError {
    pub fn display_runtime_error(input: &str, err: RuntimeError) {
        let mut line_start = 0;
        let mut line_num = 1;
        let lines: Vec<&str> = input.lines().collect();
        let blue = color::Fg(color::Blue);
        let red = color::Fg(color::Red);
        let reset = color::Fg(color::Reset);
        let file_name = Some("temp");

        for line in lines.iter() {
            let line_end = line_start + line.len();
            let span = err.span.unwrap();

            while span.start < line_end {
                if span.start >= line_start && span.start < line_end {
                    println!("{}Runtime Error{}: {:?}", red, reset, err.kind);
                    if let Some(msg) = err.message {
                        println!("msg: {}", msg);
                    }

                    if let Some(ref file_name) = file_name {
                        println!(
                            "{}-->{} {}:{}:{}",
                            blue,
                            reset,
                            file_name,
                            line_num,
                            span.start - line_start
                        );
                        println!("   {}|{}", blue, reset);
                        println!("{}{} |{} {}", blue, line_num, reset, line);
                    } else {
                        println!("   {}|{}", blue, reset);
                        println!("   | {}", line);
                    }

                    let mut highlight_line = format!("   {}|{} ", blue, reset);
                    for (i, _) in line.chars().enumerate() {
                        let pos = line_start + i;
                        if pos >= span.start && pos < span.end {
                            highlight_line.push_str(&format!("{}^{}", red, reset));
                        } else {
                            highlight_line.push(' ');
                        }
                    }

                    println!("{}", highlight_line);
                    println!();
                    return;
                }
            }

            line_start = line_end + 1;
            line_num += 1;
        }
    }

    fn display_parse_errors(input: &str, errors: &[Spanned<ParseError>], file_name: Option<String>) {
        let mut line_start = 0;
        let mut line_num = 1;
        let mut error_idx = 0;
        let lines: Vec<&str> = input.lines().collect();
        let blue = color::Fg(color::Blue);
        let red = color::Fg(color::Red);
        let reset = color::Fg(color::Reset);

        for line in lines.iter() {
            let line_end = line_start + line.len();

            while error_idx < errors.len() && errors[error_idx].span.0 < line_end {
                let error = &errors[error_idx];
                if error.span.0 >= line_start && error.span.0 < line_end {
                    println!("{}parser error{}: {:?}", red, reset, error.item);

                    if let Some(ref file_name) = file_name {
                        println!(
                            "{}-->{} {}:{}:{}",
                            blue,
                            reset,
                            file_name,
                            line_num,
                            error.span.0 - line_start
                        );
                        println!("   {}|{}", blue, reset);
                        println!("{}{} |{} {}", blue, line_num, reset, line);
                    } else {
                        println!("   {}|{}", blue, reset);
                        println!("   | {}", line);
                    }

                    let mut highlight_line = format!("   {}|{} ", blue, reset);
                    for (i, _) in line.chars().enumerate() {
                        let pos = line_start + i;
                        if pos >= error.span.0 && pos < error.span.1 {
                            highlight_line.push_str(&format!("{}^{}", red, reset));
                        } else {
                            highlight_line.push(' ');
                        }
                    }

                    println!("{}", highlight_line);
                    println!();
                }
                error_idx += 1;
            }

            line_start = line_end + 1;
            line_num += 1;
        }
    }
}
