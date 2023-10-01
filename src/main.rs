use clap::crate_description;
use clap::crate_name;
use clap::crate_version;
use clap::App;
use clap::Arg;
use clap::ArgMatches;
use std::error::Error;
use std::fmt;
use std::io;
use std::num::ParseIntError;

mod commitmsgfmt;
mod parser;
mod worditer;

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
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match *self {
            CliError::ArgWidthNaN(ref err) => Some(err),
            CliError::ArgWidthOutOfBounds(_) => None,
            CliError::Io(ref err) => Some(err),
            CliError::Other(_) => None,
        }
    }
}

pub struct Config {
    width: usize,
    comment_char: char,
}

impl Config {
    fn new(m: &ArgMatches) -> CliResult<Config> {
        use std::str::FromStr;

        let width = m
            .values_of("width")
            .unwrap()
            .last()
            .map(i32::from_str)
            .unwrap()?;

        if width < 1 {
            return Err(CliError::ArgWidthOutOfBounds(width));
        }

        let cfg = Config {
            width: width as usize,
            comment_char: Config::comment_char_from_git(),
        };

        Ok(cfg)
    }

    fn comment_char_from_git() -> char {
        use std::process::Command;

        let output: Vec<u8> = Command::new("git")
            .args(&["config", "core.commentChar"])
            .output()
            .map(|o| o.stdout)
            .unwrap_or_else(|_| "#".into());

        // The setting is either unset, "auto", or precisely 1 ASCII character;
        // Git won't commit with an invalid configuration value. "auto" support
        // can be added on-demand, it requires at least 2 passes.
        if output.is_empty() || output == b"auto" {
            '#'
        } else {
            output[0].into()
        }
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
                .number_of_values(1)
                .multiple(true)
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

- Text indented at least 4 spaces or 1 tab, and trailers, are printed unchanged."#,
                )
                .value_name("WIDTH"),
        )
        .get_matches();

    let cfg = Config::new(&m);
    if let Err(ref e) = cfg {
        fatal(e);
    }
    let cfg = cfg.unwrap();

    let commitmsgfmt = commitmsgfmt::CommitMsgFmt::new(cfg.width, cfg.comment_char);

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
    use std::io::Write;
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

    use pretty_assertions::assert_eq;
    use pretty_assertions::assert_ne;

    use std::process::Child;
    use std::process::Command;
    use std::process::Output;
    use std::process::Stdio;

    fn cargo_run_cmd() -> Vec<String> {
        let mut cmd: Vec<String> = Vec::with_capacity(6);
        cmd.push("cargo".to_owned());
        cmd.push("run".to_owned());
        cmd.push("--quiet".to_owned());

        if let Ok(t) = std::env::var("TARGET") {
            cmd.push("--target".to_owned());
            cmd.push(t);
        }

        cmd.push("--".to_owned());

        cmd
    }

    fn target_binary() -> Command {
        let cargo_run = cargo_run_cmd();
        let mut cmd = Command::new(&cargo_run[0]);
        cmd.args(&cargo_run[1..]);

        cmd
    }

    fn target_binary_with_width(w: &str) -> Command {
        let mut cmd = target_binary();
        cmd.arg("--width");
        cmd.arg(w);

        cmd
    }

    fn run_debug_binary_no_input(mut cmd: Command) -> Output {
        cmd.output().expect("run debug binary")
    }

    fn run_debug_binary_with_input(mut cmd: Command, input: &[u8]) -> Output {
        use std::io::Write;
        let mut cmd: Child = cmd
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("spawn debug binary");

        cmd.stdin
            .as_mut()
            .expect("child stdin")
            .write_all(input)
            .expect("write to child stdin");

        cmd.wait_with_output().expect("run debug binary")
    }

    fn assert_cmd_success(output: &Output) {
        assert!(output.status.success());
        assert_stderr_empty(&output);
    }

    fn assert_stderr_empty(output: &Output) {
        let err = String::from_utf8_lossy(&output.stderr);
        assert!(err.is_empty());
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
    fn arg_width_0_exits_with_code_1() {
        let output = run_debug_binary_no_input(target_binary_with_width("0"));

        assert_eq!(1, output.status.code().unwrap());

        let err = String::from_utf8_lossy(&output.stderr);
        assert!(err.contains("--width"));
        assert!(err.contains("greater"));
    }

    #[test]
    fn arg_width_0foo_exits_with_code_1() {
        let output = run_debug_binary_no_input(target_binary_with_width("0foo"));

        assert_eq!(1, output.status.code().unwrap());

        let err = String::from_utf8_lossy(&output.stderr);
        assert!(err.contains("--width"));
        assert!(err.contains("invalid digit"));
    }

    #[test]
    fn arg_width_with_multiple_values_exits_with_code_1() {
        let mut cmd = target_binary();
        cmd.args(&["-w", "1", "2"]);

        let output = run_debug_binary_no_input(cmd);

        assert_eq!(1, output.status.code().unwrap());

        let err = String::from_utf8_lossy(&output.stderr);
        assert!(err.contains(r#"Found argument '2'"#));
    }

    #[test]
    fn arg_width_1_wraps_body_at_width_1_and_exits_successfully() {
        let cmd = target_binary_with_width("1");
        let output = run_debug_binary_with_input(cmd, b"subject\nb o d y");

        assert_cmd_success(&output);

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
    }

    #[test]
    fn arg_width_short_form_is_w() {
        let mut cmd = target_binary();
        cmd.args(&["-w", "1"]);
        let output = run_debug_binary_with_input(cmd, b"subject\nb o d y");

        assert_cmd_success(&output);

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
    }

    #[test]
    fn arg_width_only_last_specified_matters() {
        let mut cmd = target_binary_with_width("string");
        cmd.args(&["-w", "1"]);
        cmd.args(&["-w", "100"]);
        let output = run_debug_binary_with_input(cmd, b"subject\nb o d y");

        assert_cmd_success(&output);

        let out = String::from_utf8_lossy(&output.stdout);
        assert_eq!(
            "subject

b o d y
",
            out
        );
    }

    // Sometime after the release of v1.2.0, external changes to Travis CI have
    // caused this test to begin failing consistently. Anecdotally, it likewise
    // has a higher failure rate on Ubuntu 19.10 than in the past.
    //
    // The test is still correct, if awful, and it still represents desired
    // functionality, so I would like to keep it. However, it also exercises a
    // code path that practically has never changed, wherefore executing the
    // test is less valuable, or even completely worthless as in the case of
    // Travis CI, so disabling it is an acceptable compromise.
    #[ignore]
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
            })
            .unwrap_or("head");

        let mut cargo_run_cmd = cargo_run_cmd();
        cargo_run_cmd.push("--width".into());
        cargo_run_cmd.push("72".into());
        let output = Command::new("bash")
            .args(&[
                "-c",
                &format!(
                    "set -e
                     set -o pipefail
                     {} < Cargo.lock |
                     {} -n0",
                    cargo_run_cmd.join(" "),
                    gnu_head
                ),
            ])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .output()
            .expect("trigger broken pipe for debug binary");

        assert_eq!(141, output.status.code().unwrap());
    }
}
