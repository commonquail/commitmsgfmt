#[macro_use]
extern crate clap;
extern crate encoding;
#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
extern crate regex;
extern crate unicode_segmentation;

use clap::App;
use clap::Arg;
use clap::ArgMatches;
use std::error::Error;
use std::fmt;
use std::io;
use std::num::ParseIntError;

mod commitmsgfmt;
mod parser;

type CliResult<T> = Result<T, CliError>;

#[derive(Debug)]
enum CliError {
    ArgWidthNaN(ParseIntError),
    ArgWidthOutOfBounds(i32),
    Io(io::Error),
    Other(String),
}

impl From<io::Error> for CliError {
    fn from(err: io::Error) -> CliError {
        CliError::Io(err)
    }
}

impl From<ParseIntError> for CliError {
    fn from(err: ParseIntError) -> CliError {
        CliError::ArgWidthNaN(err)
    }
}

impl<'a> From<&'a str> for CliError {
    fn from(err: &'a str) -> CliError {
        CliError::Other(err.to_owned())
    }
}

impl From<String> for CliError {
    fn from(err: String) -> CliError {
        CliError::Other(err)
    }
}

impl fmt::Display for CliError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            CliError::ArgWidthNaN(ref err) => write!(f, "--width: {}", err),
            CliError::ArgWidthOutOfBounds(ref w) => {
                write!(f, "--width must be greater than 0, was: '{}'", w)
            }
            CliError::Io(ref err) => err.fmt(f),
            CliError::Other(ref s) => s.fmt(f),
        }
    }
}

impl Error for CliError {
    fn description(&self) -> &str {
        match *self {
            CliError::ArgWidthNaN(ref err) => err.description(),
            CliError::ArgWidthOutOfBounds(_) => "--width out of bounds",
            CliError::Io(ref err) => err.description(),
            CliError::Other(ref err) => err,
        }
    }
}

pub struct Config {
    width: usize,
}

impl Config {
    fn new(m: &ArgMatches) -> CliResult<Config> {
        use std::str::FromStr;

        let width = m.value_of("width").map(i32::from_str).unwrap()?;

        if width < 1 {
            return Err(CliError::ArgWidthOutOfBounds(width));
        }

        let cfg = Config {
            width: width as usize,
        };

        Ok(cfg)
    }
}

fn main() {
    let m = App::new(crate_name!())
        .about(crate_description!())
        .version(crate_version!())
        .arg(
            Arg::with_name("width")
                .short("w")
                .long("width")
                .takes_value(true)
                .default_value("72")
                .hide_default_value(true)
                .help("The message body max paragraph width. Default: 72.")
                .long_help(
                    r#"The max width, in graphemes, of regular paragraphs. Text will be wrapped at
the last space up to this limit, or at the limit, and list item continuations
indented. Default: 72.

Some text is exempt from wrapping:

- The subject line ignores this setting and is instead broken down into a
  subject line and a paragraph after 90 graphemes. The subject line works best
  across different formats when forced into a single line but this harsh
  behaviour necessitates a laxer limit on its length to avoid rejecting too
  many valid subjects.

- Text indented at least 4 spaces or 1 tab; trailers; and lines beginning with
  "[<alnum>]" are printed unchanged."#,
                ).value_name("WIDTH"),
        ).get_matches();

    let cfg = Config::new(&m);
    if let Err(ref e) = cfg {
        fatal(e);
    }
    let cfg = cfg.unwrap();

    let commitmsgfmt = commitmsgfmt::CommitMsgFmt::new(cfg.width);

    let result = read_all_bytes_from_stdin()
        .and_then(to_utf8)
        .and_then(|text| to_stdout(&commitmsgfmt.filter(&text)));

    match result {
        Ok(()) => (),
        Err(ref err) => match *err {
            CliError::Io(ref e) if e.kind() == io::ErrorKind::BrokenPipe => {
                std::process::exit(141 /* 128 + 13 */);
            }
            _ => {
                fatal(err);
            }
        },
    }
}

fn read_all_bytes_from_stdin() -> CliResult<Vec<u8>> {
    let mut buf: Vec<u8> = Vec::new();
    let stdin = io::stdin();
    io::copy(&mut stdin.lock(), &mut buf)?;

    Ok(buf)
}

fn to_utf8(bytes: Vec<u8>) -> CliResult<String> {
    String::from_utf8(bytes).or_else(|e| from_iso_8859_1(&e.into_bytes()))
}

fn from_iso_8859_1(bytes: &[u8]) -> CliResult<String> {
    use encoding::types::Encoding;
    encoding::all::ISO_8859_1
        .decode(bytes, encoding::DecoderTrap::Replace)
        .map_err(|s| CliError::Other(s.into()))
}

fn to_stdout(msg: &str) -> CliResult<()> {
    use io::Write;
    let stdout = io::stdout();
    let mut stdout = stdout.lock();
    stdout.write_all(msg.as_bytes())?;

    Ok(())
}

fn fatal(e: &CliError) {
    eprintln!("fatal: {}", e);
    ::std::process::exit(1);
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::process::Child;
    use std::process::Command;
    use std::process::Stdio;

    fn cargo_run_cmd(w: &str) -> Vec<String> {
        let mut cmd: Vec<String> = Vec::with_capacity(8);
        cmd.push("cargo".to_owned());
        cmd.push("run".to_owned());
        cmd.push("--quiet".to_owned());

        if let Ok(t) = std::env::var("TARGET") {
            cmd.push("--target".to_owned());
            cmd.push(t);
        }

        cmd.push("--".to_owned());
        cmd.push("--width".to_owned());
        cmd.push(w.to_owned());

        cmd
    }

    fn target_binary_with_width(w: &str) -> Command {
        let cargo_run = cargo_run_cmd(w);
        let mut cmd = Command::new(&cargo_run[0]);
        cmd.args(&cargo_run[1..]);

        cmd
    }

    #[test]
    fn to_utf8_converts_utf_8_bytes_to_utf_8() {
        let some_utf_8_str = "å";
        let expected = some_utf_8_str.to_owned();

        assert_eq!(
            expected,
            to_utf8(Vec::from(some_utf_8_str)).expect("multibyte character in test")
        );
    }

    #[test]
    fn to_utf8_converts_iso_8859_1_bytes_to_utf_8() {
        let some_latin1_bytes = [229];
        let str_from_latin1 = to_utf8(some_latin1_bytes.to_vec()).unwrap();

        assert_ne!(some_latin1_bytes, str_from_latin1.as_bytes());
        assert_eq!("å", str_from_latin1);
    }

    #[test]
    fn from_iso_8859_1_converts_iso_8859_1_bytes_to_utf_8() {
        let some_latin1_bytes = [229];
        let str_from_latin1 = from_iso_8859_1(&some_latin1_bytes).unwrap();

        assert_ne!(some_latin1_bytes, str_from_latin1.as_bytes());
        assert_eq!("å", str_from_latin1);
    }

    #[test]
    fn width_0_exits_with_code_1() {
        let output = target_binary_with_width("0")
            .output()
            .expect("run debug binary");

        assert_eq!(1, output.status.code().unwrap());

        let err = String::from_utf8_lossy(&output.stderr);
        assert!(err.contains("--width"));
        assert!(err.contains("greater"));
    }

    #[test]
    fn width_0foo_exits_with_code_1() {
        let output = target_binary_with_width("0foo")
            .output()
            .expect("run debug binary");

        assert_eq!(1, output.status.code().unwrap());

        let err = String::from_utf8_lossy(&output.stderr);
        assert!(err.contains("--width"));
        assert!(err.contains("invalid digit"));
    }

    #[test]
    fn width_1_wraps_body_at_width_1_and_exits_successfully() {
        use std::io::Write;

        let mut cmd: Child = target_binary_with_width("1")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("spawn debug binary");

        cmd.stdin
            .as_mut()
            .expect("child stdin")
            .write_all(b"subject\nbody")
            .expect("write to child stdin");

        let output = cmd.wait_with_output().expect("run debug binary");

        assert!(output.status.success());

        let out = String::from_utf8_lossy(&output.stdout);
        assert_eq!(
            "subject

b
o
d
y
",
            out
        );

        let err = String::from_utf8_lossy(&output.stderr);
        assert!(err.is_empty());
    }

    #[cfg(unix)]
    #[test]
    fn broken_pipe_exits_with_code_141() {
        // Here be dragons!
        //
        // Sometimes when piping from one command to another, the first command
        // experiences a "broken pipe": it tries to write to a destination that
        // is no longer listening. The first command should be able to handle
        // this gracefully.
        //
        // One way to demonstrate the effect is to send a lot of input (to avoid
        // buffering issues) into a command that wants to read very little and
        // then inspecting the PIPESTATUS array of status codes. For instance,
        // this command sends a bunch of dictionaries into a command that reads
        // zero lines:
        //
        //  $ cat /usr/share/dict/* | head -n0
        //  $ echo "${PIPESTATUS[@]}"
        //  141 0
        //
        // A command that exits due to a signal shall exit with a status code
        // equal to 128 + the signal value. The writing command is terminated
        // with SIGPIPE, whose value is 13 (signal(7)) => 128 + 13 = 141.
        //
        // Now for the dragons.
        //
        // I can't figure out how to wire together two Commands so one pipes its
        // standard output directly into the other's standard input. I've seen
        // examples of accumulating the output and manually writing it back but
        // that won't do: it sidesteps the actual pipe that everything revolves
        // around.
        //
        // But I know how to trigger it in Bash, how to make Bash exit with the
        // status code of the terminating process, and how to run a single Bash
        // process with a specific program.
        //
        // Aside from being a horrendous hack, the two biggest risks here are
        // 1) that Bash is not available, and 2) that the input fits in all the
        // buffers in play so there are no more writes after the first read.
        // Except for on macOS, where all bets are off and we'll just have to
        // cross our fingers this works.

        let gnu_head: &str = std::env::var("TARGET")
            .ok()
            .and_then(|t| {
                if t.contains("darwin") {
                    Some("ghead")
                } else {
                    None
                }
            }).unwrap_or("head");

        let output = Command::new("bash")
            .args(&[
                "-c",
                &format!(
                    "set -e
                     set -o pipefail
                     {} < Cargo.lock |
                     {} -n0",
                    cargo_run_cmd("72").join(" "),
                    gnu_head
                ),
            ]).stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .output()
            .expect("trigger broken pipe for debug binary");

        assert_eq!(141, output.status.code().unwrap());

        let err = String::from_utf8_lossy(&output.stderr);
        assert!(err.is_empty());
    }
}
