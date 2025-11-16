use crate::runtime::Runtime;

use std::io::{stdin, stdout, Write};

use termion::cursor::DetectCursorPos;
use termion::event::{Event, Key};
use termion::input::TermRead;
use termion::raw::IntoRawMode;

fn interpret_input(runtime: &mut Runtime, input: &str) {
    runtime.clear_program();

    if let Err(err) = runtime.load_inline(input) {
        eprintln!("{}", err.render());
        return;
    }

    match runtime.run() {
        Ok(()) => {
            // Print the return value with "=> " prefix
            let mut stdout = std::io::stdout();
            print!("=> ");
            if let Err(err) = runtime.print(&mut stdout) {
                eprintln!("{}", err.render());
            }
        },
        Err(err) => eprintln!("{}", err.render()),
    }
}

pub fn run_repl(mut runtime: Runtime) -> Result<(), std::io::Error> {
    println!("ENTERING REPL");
    let stdin = stdin();
    let mut stdout = match stdout().into_raw_mode() {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Failed to enable raw mode: {}. Terminal features may be limited.", e);
            return Err(e);
        }
    };
    let mut input = String::new();
    let mut inputs: Vec<String> = vec![];
    let mut history_index = None;
    let mut input_pos = 0;

    write!(stdout, "> ")?;
    stdout.flush()?;

    for evt in stdin.events() {
        let evt = match evt {
            Ok(e) => e,
            Err(e) => {
                eprintln!("\nTerminal I/O error: {}", e);
                return Err(e);
            }
        };
        match evt {
            Event::Key(Key::Char('\n')) if input.trim().is_empty() => {
                write!(stdout, "\r\n> ")?;
                input_pos = 0;
            }
            Event::Key(Key::Char('\n')) => {
                write!(stdout, "\r\n")?;
                stdout.flush()?;

                let _ = stdout.suspend_raw_mode();

                interpret_input(&mut runtime, input.as_str());

                let _ = stdout.activate_raw_mode();

                inputs.push(input.clone());
                history_index = Some(inputs.len());
                input.clear();
                write!(stdout, "\r\n> ")?;
                input_pos = 0;
            }
            Event::Key(Key::Up) => {
                if let Some(idx) = history_index {
                    if idx > 0 {
                        history_index = Some(idx - 1);
                        input = inputs[idx - 1].clone();
                    }
                } else if !inputs.is_empty() {
                    history_index = Some(inputs.len() - 1);
                    if let Some(last_input) = inputs.last() {
                        input = last_input.clone();
                    }
                }
                write!(stdout, "\r{}\r> {}", " ".repeat(80), input)?;
                input_pos = input.len();
            }
            Event::Key(Key::Down) => {
                if let Some(idx) = history_index {
                    if idx < inputs.len() - 1 {
                        history_index = Some(idx + 1);
                        input = inputs[idx + 1].clone();
                    } else {
                        history_index = None;
                        input.clear();
                    }
                }
                write!(stdout, "\r{}\r> {}", " ".repeat(80), input)?;
                input_pos = input.len();
            }
            Event::Key(Key::Left) => {
                let left = termion::cursor::Left(1);
                let cursor_pos_result = stdout.cursor_pos();

                if let Ok((x, _)) = cursor_pos_result {
                    if x <= 3 {
                        continue;
                    }
                    write!(stdout, "{}", left)?;
                    input_pos -= 1;
                } else if input_pos > 0 {
                    // Fallback if cursor position detection fails
                    write!(stdout, "{}", left)?;
                    input_pos -= 1;
                }
            }
            Event::Key(Key::Right) => {
                let right = termion::cursor::Right(1);
                write!(stdout, "{}", right)?;
                input_pos += 1;
                if input_pos >= input.len() {
                    input.push(' ');
                }
            }
            Event::Key(Key::Char(c)) => {
                if let Some((i, _)) = input.char_indices().nth(input_pos) {
                    input.insert(i, c);
                } else {
                    input.push(c);
                }

                input_pos += 1;

                write!(
                    stdout,
                    "\r{}\r> {} {}",
                    " ".repeat(80),
                    input,
                    termion::cursor::Left(((input.len() + 1) - input_pos) as u16)
                )?;
            }
            Event::Key(Key::Backspace) => {
                if !input.is_empty() {
                    input_pos -= 1;
                    if let Some((i, _)) = input.char_indices().nth(input_pos) {
                        input.remove(i);
                    } else {
                        input.pop();
                    }
                    write!(
                        stdout,
                        "\r{}\r> {} {}",
                        " ".repeat(80),
                        input,
                        termion::cursor::Left(((input.len() + 1) - input_pos) as u16)
                    )?;
                }
            }
            Event::Key(Key::Ctrl('c')) | Event::Key(Key::Ctrl('d')) => {
                writeln!(stdout, "\nExiting REPL")?;
                break;
            }
            Event::Key(Key::Ctrl('e')) => {
                input_pos = input.len();
                write!(stdout, "\r{}\r> {}", " ".repeat(80), input)?;
            }
            Event::Key(Key::Ctrl('a')) => {
                input_pos = 0;
                write!(
                    stdout,
                    "\r{}\r> {} {}",
                    " ".repeat(80),
                    input,
                    termion::cursor::Left(((input.len() + 1) - input_pos) as u16)
                )?;
            }
            _ => {}
        }
        stdout.flush()?;
    }

    Ok(())
}
