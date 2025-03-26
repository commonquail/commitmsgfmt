use std::borrow::Cow;
use std::error::Error;
use std::fmt;
use std::io;
use std::process::ExitCode;
use std::process::Termination;

mod commitmsgfmt;
mod parser;
mod worditer;

// Hard-code program name in version output
macro_rules! str_version {
    () => {
        concat!("commitmsgfmt ", env!("CARGO_PKG_VERSION"))
    };
}

// Use dynamic binary name in help output
macro_rules! str_help_common {
    () => {
        r#"Usage: {} [OPTIONS]
Formats commit messages better than fmt(1) and Vim

  -h, --help                 Prints help information
  -V, --version              Prints version information"#
    };
}

macro_rules! str_help_short {
    () => {
        concat!(
            str_help_common!(),
            r#"
  -w, --width <NUM>          The message body max paragraph width. Default: 72.
  -c, --comment-string <STR> The string to detect a comment. Default taken from git config."#
        )
    };
}

macro_rules! str_help_long {
    () => {
        concat!(
            str_help_common!(),
            r#"
  -w, --width <NUM>
            The max width, in graphemes, of regular paragraphs. Text will be
            wrapped at the last space up to this limit, or at the limit, and
            list item continuations indented. Default: 72.

            Some text is exempt from wrapping:

            - The subject line ignores this setting and is instead broken down
              into a subject line and a paragraph after 90 graphemes. The
              subject line works best across different formats when forced into
              a single line but this harsh behaviour necessitates a laxer limit
              on its length to avoid rejecting too many valid subjects.

            - Text indented at least 4 spaces or 1 tab; trailers; and block
              quotes are printed unchanged.

  -c, --comment-string <STR>
            The string used to detect a comment. By default it queries git config
            for the value and defaults to `#`. Supply this to use a different value
            such as `JJ` for files that are to be commited in a jujutsu repository."#
        )
    };
}

type CliResult<'a, T> = Result<T, CliError<'a>>;

#[derive(Debug)]
enum CliError<'a> {
    ArgUnrecognized(Cow<'a, str>),
    ArgWidthNaN(Cow<'a, str>),
    ArgWidthOutOfBounds(i32),
    EarlyExit(Cow<'a, str>),
    Io(io::Error),
    Other(Cow<'a, str>),
}

impl<'a> From<io::Error> for CliError<'a> {
    fn from(err: io::Error) -> CliError<'a> {
        CliError::Io(err)
    }
}

impl fmt::Display for CliError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            CliError::ArgUnrecognized(ref s) => write!(f, "Found argument '{}'", s),
            CliError::ArgWidthNaN(ref w) => {
                write!(f, "--width: must be a positive integer, was: '{}'", w)
            }
            CliError::ArgWidthOutOfBounds(ref w) => {
                write!(f, "--width: must be greater than 0, was: '{}'", w)
            }
            CliError::EarlyExit(ref s) => s.fmt(f),
            CliError::Io(ref err) => err.fmt(f),
            CliError::Other(ref s) => s.fmt(f),
        }
    }
}

impl Error for CliError<'_> {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match *self {
            CliError::ArgUnrecognized(_) => None,
            CliError::ArgWidthNaN(_) => None,
            CliError::ArgWidthOutOfBounds(_) => None,
            CliError::EarlyExit(_) => None,
            CliError::Io(ref err) => Some(err),
            CliError::Other(_) => None,
        }
    }
}

impl Termination for CliError<'_> {
    fn report(self) -> ExitCode {
        match self {
            CliError::EarlyExit(s) => {
                println!("{}", s);
                ExitCode::SUCCESS
            }
            CliError::Io(ref e) if e.kind() == io::ErrorKind::BrokenPipe => {
                let ret = 128 + 13;
                debug_assert!(ret == 141);
                ExitCode::from(ret)
            }
            _ => {
                eprintln!("fatal: {}", self);
                ExitCode::FAILURE
            }
        }
    }
}

enum CliArgument<'a> {
    HelpShort,
    HelpLong,
    Version,
    Unsupported(&'a str),
    Config(ConfigArgument<'a>),
}

enum ConfigArgument<'a> {
    Width(Option<&'a str>),
    CommentString(Option<&'a str>),
}

#[derive(Debug, Eq, PartialEq)]
pub struct Config {
    width: usize,
    comment_string: String,
}

impl Config {
    fn new(args: Vec<ConfigArgument<'_>>) -> CliResult<'_, Config> {
        let mut width: Option<&str> = None;
        let mut comment_string: Option<&str> = None;
        for arg in args {
            match arg {
                ConfigArgument::Width(s) => width = s,
                ConfigArgument::CommentString(s) => comment_string = s,
            }
        }

        let width = width
            .map(|w| w.parse().map_err(|_| CliError::ArgWidthNaN(w.into())))
            .transpose()?
            .unwrap_or(72);

        if width < 1 {
            return Err(CliError::ArgWidthOutOfBounds(width));
        }

        let comment_string = match comment_string {
            None => parse_git_config_commentchar(git_config_commentchar()),
            Some(comment) => comment.to_string(),
        };
        let cfg = Config {
            width: width as usize,
            comment_string,
        };

        Ok(cfg)
    }
}

fn git_config_commentchar() -> Result<Vec<u8>, io::Error> {
    std::process::Command::new("git")
        .args(["config", "core.commentChar"])
        .output()
        .map(|o| o.stdout)
}

fn parse_git_config_commentchar(git_output: Result<Vec<u8>, io::Error>) -> String {
    let output: Vec<u8> = git_output.unwrap_or_else(|_| "#".into());

    // The setting is either unset, "auto", or precisely 1 ASCII character;
    // Git won't commit with an invalid configuration value. "auto" support
    // can be added on-demand, it requires at least 2 passes.
    if output.is_empty() || output == b"auto" {
        "#".into()
    } else {
        (output[0] as char).into()
    }
}

fn main() -> ExitCode {
    let command_line = std::env::args().collect::<Vec<String>>();
    let cfg = try_config_from_command_line(&command_line);
    if let Err(e) = cfg {
        return e.report();
    }
    let cfg = cfg.unwrap();

    let commitmsgfmt = commitmsgfmt::CommitMsgFmt::new(cfg.width, &cfg.comment_string);

    let result = read_all_bytes_from_stdin()
        .and_then(to_utf8)
        .map(|text| commitmsgfmt.filter(&text))
        .and_then(to_stdout);

    match result {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => e.report(),
    }
}

fn try_config_from_command_line(args: &'_ [String]) -> CliResult<'_, Config> {
    let (binary_path, args) = args.split_first().expect("binary has name");
    parse_args(args)
        .into_iter()
        .try_fold(vec![], |mut acc, arg| match arg {
            CliArgument::HelpLong => Err(CliError::EarlyExit(
                format!(str_help_long!(), binary_path).into(),
            )),
            CliArgument::HelpShort => Err(CliError::EarlyExit(
                format!(str_help_short!(), binary_path).into(),
            )),
            CliArgument::Version => Err(CliError::EarlyExit(str_version!().into())),
            CliArgument::Unsupported(a) => Err(CliError::ArgUnrecognized(a.into())),
            CliArgument::Config(a) => {
                acc.push(a);
                Ok(acc)
            }
        })
        .and_then(Config::new)
}

fn parse_args(args: &'_ [String]) -> Vec<CliArgument<'_>> {
    let mut parsed_args = Vec::with_capacity(args.len());

    let mut iter = args.iter().map(String::as_str);
    while let Some(arg) = iter.next() {
        if arg.starts_with("--") {
            // "--width=42"
            let mut kv = arg.splitn(2, '=');
            let longopt_key = kv.next().expect("SplitN[0] exists");
            let mut longopt_value = || kv.next().or_else(|| iter.next());
            let parsed_arg = match longopt_key {
                "--help" => CliArgument::HelpLong,
                "--version" => CliArgument::Version,
                "--comment-string" => {
                    let p = longopt_value();
                    CliArgument::Config(ConfigArgument::CommentString(p))
                }
                "--width" => {
                    let w = longopt_value();
                    CliArgument::Config(ConfigArgument::Width(w))
                }
                _ => CliArgument::Unsupported(arg),
            };
            parsed_args.push(parsed_arg);
        } else if arg.starts_with('-') {
            for (ix, c) in arg.char_indices().skip(1) {
                match c {
                    'h' => parsed_args.push(CliArgument::HelpShort),
                    'V' => parsed_args.push(CliArgument::Version),
                    'w' => {
                        let w = if ix < arg.len() - 1 {
                            // "-w42"
                            Some(&arg[ix + 1..])
                        } else {
                            // "-w 42"
                            iter.next()
                        };
                        parsed_args.push(CliArgument::Config(ConfigArgument::Width(w)));
                        break;
                    }
                    _ => parsed_args.push(CliArgument::Unsupported(arg)),
                }
            }
        } else {
            parsed_args.push(CliArgument::Unsupported(arg));
        }
    }

    parsed_args
}

fn read_all_bytes_from_stdin<'a>() -> CliResult<'a, Vec<u8>> {
    let mut buf: Vec<u8> = Vec::new();
    let stdin = io::stdin();
    io::copy(&mut stdin.lock(), &mut buf)?;

    Ok(buf)
}

fn to_utf8<'a>(bytes: Vec<u8>) -> CliResult<'a, String> {
    String::from_utf8(bytes).or_else(|e| from_iso_8859_1(&e.into_bytes()))
}

fn from_iso_8859_1<'a>(bytes: &[u8]) -> CliResult<'a, String> {
    use encoding::types::Encoding;
    encoding::all::ISO_8859_1
        .decode(bytes, encoding::DecoderTrap::Replace)
        .map_err(CliError::Other)
}

fn to_stdout<'a>(msg: String) -> CliResult<'a, ()> {
    use std::io::Write;
    let stdout = io::stdout();
    let mut stdout = stdout.lock();
    stdout.write_all(msg.as_bytes())?;

    Ok(())
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
        assert_stderr_empty(output);
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
    fn impl_clierror_display_debug() {
        #[derive(Debug, PartialEq)]
        struct Expect<'a> {
            display: Cow<'a, str>,
            debug: Cow<'a, str>,
        }
        impl<'a> Expect<'a> {
            fn display_debug(display: Cow<'a, str>, debug: Cow<'a, str>) -> Self {
                Self { display, debug }
            }
        }
        let errs = vec![
            (
                CliError::ArgUnrecognized("foo".into()),
                Expect::display_debug(
                    r#"Found argument 'foo'"#.into(),
                    r#"ArgUnrecognized("foo")"#.into(),
                ),
            ),
            (
                CliError::ArgWidthNaN("nan".into()),
                Expect::display_debug(
                    "--width: must be a positive integer, was: 'nan'".into(),
                    r#"ArgWidthNaN("nan")"#.into(),
                ),
            ),
            (
                CliError::ArgWidthOutOfBounds(0),
                Expect::display_debug(
                    "--width: must be greater than 0, was: '0'".into(),
                    "ArgWidthOutOfBounds(0)".into(),
                ),
            ),
            (
                CliError::EarlyExit("help".into()),
                Expect::display_debug(r#"help"#.into(), r#"EarlyExit("help")"#.into()),
            ),
            (
                CliError::from(io::Error::from(io::ErrorKind::BrokenPipe)),
                Expect::display_debug(r#"broken pipe"#.into(), r#"Io(Kind(BrokenPipe))"#.into()),
            ),
            (
                CliError::Other("other".into()),
                Expect::display_debug(r#"other"#.into(), r#"Other("other")"#.into()),
            ),
        ];
        let (actual, expected): (Vec<_>, Vec<_>) = errs
            .into_iter()
            .map(|(input, ex)| {
                let actual_display = format!("{}", &input);
                let actual_debug = format!("{:?}", &input);
                let actual = Expect::display_debug(actual_display.into(), actual_debug.into());
                (actual, ex)
            })
            .unzip();
        assert_eq!(expected, actual);
    }

    #[test]
    fn impl_clierror_termination() {
        let errs = vec![
            (CliError::ArgUnrecognized("foo".into()), ExitCode::FAILURE),
            (CliError::ArgWidthNaN("nan".into()), ExitCode::FAILURE),
            (CliError::ArgWidthOutOfBounds(0), ExitCode::FAILURE),
            (CliError::EarlyExit("help".into()), ExitCode::SUCCESS),
            (
                CliError::from(io::Error::from(io::ErrorKind::BrokenPipe)),
                ExitCode::from(141),
            ),
            (CliError::Other("other".into()), ExitCode::FAILURE),
        ];
        let (actual, expected): (Vec<_>, Vec<_>) = errs
            .into_iter()
            .map(|(input, ex)| (format!("{:?}", input.report()), format!("{:?}", ex)))
            .unzip();
        assert_eq!(expected, actual);
    }

    #[test]
    fn smoke_decide_behavior_from_command_line() {
        use crate::CliError::*;

        let mut matrix = vec![];
        matrix.push((
            vec!["binary"],
            Ok(Config {
                width: 72,
                comment_string: "#".into(),
            }),
        ));
        matrix.push((
            vec!["binary", "--width"],
            Ok(Config {
                width: 72,
                comment_string: "#".into(),
            }),
        ));
        matrix.push((
            vec!["binary", "--width", "10"],
            Ok(Config {
                width: 10,
                comment_string: "#".into(),
            }),
        ));
        matrix.push((
            vec!["binary", "--width=21"],
            Ok(Config {
                width: 21,
                comment_string: "#".into(),
            }),
        ));
        matrix.push((
            vec!["binary", "-w"],
            Ok(Config {
                width: 72,
                comment_string: "#".into(),
            }),
        ));
        matrix.push((
            vec!["binary", "-w37"],
            Ok(Config {
                width: 37,
                comment_string: "#".into(),
            }),
        ));
        matrix.push((
            vec!["binary", "-w37", "-w42"],
            Ok(Config {
                width: 42,
                comment_string: "#".into(),
            }),
        ));
        matrix.push((
            vec!["binary", "--nonexistent"],
            Err(ArgUnrecognized("--nonexistent".into())),
        ));
        matrix.push((
            vec!["binary", "nonexistent"],
            Err(ArgUnrecognized("nonexistent".into())),
        ));
        matrix.push((
            vec!["binary", "-uw42"],
            Err(ArgUnrecognized("-uw42".into())),
        ));
        matrix.push((vec!["binary", "-w0"], Err(ArgWidthOutOfBounds(0))));
        matrix.push((
            vec!["binary", "--width=nan"],
            Err(ArgWidthNaN("nan".into())),
        ));
        matrix.push((
            vec!["path1", "--help"],
            Err(EarlyExit(format!(str_help_long!(), "path1").into())),
        ));
        matrix.push((
            vec!["path2", "--version"],
            Err(EarlyExit(str_version!().into())),
        ));
        matrix.push((
            vec!["/path3", "-h"],
            Err(EarlyExit(format!(str_help_short!(), "/path3").into())),
        ));
        matrix.push((vec!["./path4", "-V"], Err(EarlyExit(str_version!().into()))));
        matrix.push((
            vec!["p/ath/5", "-Vh"],
            Err(EarlyExit(str_version!().into())),
        ));
        matrix.push((
            vec!["./path/6", "-hV"],
            Err(EarlyExit(format!(str_help_short!(), "./path/6").into())),
        ));

        for (args, expected) in matrix {
            let args = args.iter().map(|s| s.to_string()).collect::<Vec<_>>();
            let actual = try_config_from_command_line(&args);

            match (expected, actual) {
                (Ok(expected), Ok(actual)) => {
                    assert_eq!(actual, expected);
                }
                (Err(ArgUnrecognized(expected)), Err(ArgUnrecognized(actual))) => {
                    assert_eq!(actual, expected);
                }
                (Err(ArgWidthNaN(expected)), Err(ArgWidthNaN(actual))) => {
                    assert_eq!(actual, expected);
                }
                (Err(ArgWidthOutOfBounds(expected)), Err(ArgWidthOutOfBounds(actual))) => {
                    assert_eq!(actual, expected);
                }
                (Err(EarlyExit(expected)), Err(EarlyExit(actual))) => {
                    assert_eq!(actual, expected);
                }
                (expected, actual) => {
                    panic!(
                        "induce failure for debugging: for {:?} expected {:?}, was {:?}",
                        args, expected, actual
                    );
                }
            }
        }
    }

    #[test]
    fn parses_git_config_commentchar() {
        let matrix = vec![
            (Ok("".into()), "#"),
            (Ok("auto".into()), "#"),
            (Ok("#".into()), "#"),
            (Ok("xy".into()), "x"),
            (Err(io::Error::from(io::ErrorKind::PermissionDenied)), "#"),
        ];
        let (actual, expected): (Vec<_>, Vec<_>) = matrix
            .into_iter()
            .map(|(input, expected)| {
                let x = format!("{:?}", &input);
                let actual = parse_git_config_commentchar(input);
                ((x.clone(), actual), (x, expected.into()))
            })
            .unzip();
        assert_eq!(expected, actual);
    }

    #[test]
    fn arg_width_0_exits_with_code_1() {
        let output = run_debug_binary_no_input(target_binary_with_width("0"));

        assert_eq!(1, output.status.code().unwrap());

        let err = String::from_utf8_lossy(&output.stderr);
        assert!(err.contains("--width"));
    }

    #[test]
    fn arg_width_0foo_exits_with_code_1() {
        let output = run_debug_binary_no_input(target_binary_with_width("0foo"));

        assert_eq!(1, output.status.code().unwrap());

        let err = String::from_utf8_lossy(&output.stderr);
        assert!(err.contains("--width"));
    }

    #[test]
    fn arg_width_with_multiple_values_exits_with_code_1() {
        let mut cmd = target_binary();
        cmd.args(["-w", "1", "2"]);

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
        cmd.args(["-w1"]);
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
        cmd.args(["-w", "1"]);
        cmd.args(["-w", "100"]);
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

    #[test]
    fn arg_help_prints_long_help() {
        let mut cmd = target_binary();
        cmd.arg("--help");
        let output = run_debug_binary_no_input(cmd);

        assert_cmd_success(&output);

        let out = String::from_utf8_lossy(&output.stdout);
        let out_ci = out.to_lowercase();
        assert!(out_ci.contains("usage:"));
        assert!(out.contains("\n  -h, --help"));
        assert!(out.contains("\n  -V, --version"));
        assert!(out.contains("\n  -w, --width"));
        assert!(out.contains("exempt"));
    }

    #[test]
    fn arg_h_prints_short_help() {
        let mut cmd = target_binary();
        cmd.arg("-h");
        let output = run_debug_binary_no_input(cmd);

        assert_cmd_success(&output);

        let out = String::from_utf8_lossy(&output.stdout);
        let out_ci = out.to_lowercase();
        assert!(out_ci.contains("usage:"));
        assert!(out.contains("\n  -h, --help"));
        assert!(out.contains("\n  -V, --version"));
        assert!(out.contains("\n  -w, --width"));
        assert!(!out.contains("exempt"));
    }

    #[test]
    fn arg_help_disables_arg_width() {
        let mut cmd = target_binary();
        cmd.args(["--width", "10"]);
        cmd.arg("--help");
        let output = run_debug_binary_no_input(cmd);

        assert_cmd_success(&output);

        let out = String::from_utf8_lossy(&output.stdout);
        let out_ci = out.to_lowercase();
        assert!(out_ci.contains("usage:"));
    }

    #[test]
    fn arg_help_before_arg_version_prints_help() {
        let mut cmd = target_binary();
        cmd.arg("--help");
        cmd.arg("--version");
        let output = run_debug_binary_no_input(cmd);

        assert_cmd_success(&output);

        let out = String::from_utf8_lossy(&output.stdout);
        let out = out.to_lowercase();
        assert!(out.contains("usage"));
    }

    #[test]
    fn arg_version_prints_version() {
        let mut cmd = target_binary();
        cmd.arg("--version");
        let output = run_debug_binary_no_input(cmd);

        assert_cmd_success(&output);

        let out = String::from_utf8_lossy(&output.stdout);
        let out = out.trim();

        let parts: Vec<&str> = out.splitn(2, ' ').collect();
        assert_eq!(2, parts.len());

        let part_name = parts[0];
        assert_eq!(part_name, "commitmsgfmt");

        let part_ver = parts[1];
        let char_is_version_specifier_allowed = |c: char| match c {
            '.' | '-' | 'G' | 'I' | 'T' => true,
            _ => c.is_ascii_digit(),
        };
        let satisfies_version_part_format = part_ver.chars().all(char_is_version_specifier_allowed);
        assert!(satisfies_version_part_format);
    }

    #[test]
    fn arg_version_short_form_is_capital_v() {
        // "-V" is legacy from "clap". I would have used lowercase "-v".

        let mut cmd = target_binary();
        cmd.arg("-V");
        let output = run_debug_binary_no_input(cmd);

        assert_cmd_success(&output);

        let out = String::from_utf8_lossy(&output.stdout);

        let parts: Vec<&str> = out.splitn(2, ' ').collect();
        assert_eq!(2, parts.len());

        let part_name = parts[0];
        assert_eq!(part_name, "commitmsgfmt");
    }

    #[test]
    fn arg_version_disables_arg_width() {
        let mut cmd = target_binary();
        cmd.args(["--width", "10"]);
        cmd.arg("--version");
        let output = run_debug_binary_no_input(cmd);

        assert_cmd_success(&output);

        let out = String::from_utf8_lossy(&output.stdout);
        assert!(out.contains("commitmsgfmt"));
    }

    #[test]
    fn arg_version_before_arg_help_prints_version() {
        let mut cmd = target_binary();
        cmd.arg("--version");
        cmd.arg("--help");
        let output = run_debug_binary_no_input(cmd);

        assert_cmd_success(&output);

        let out = String::from_utf8_lossy(&output.stdout);
        let out = out.to_lowercase();
        assert!(!out.contains("usage"));
    }

    #[test]
    fn args_parse_combined_shortform() {
        let mut cmd = target_binary();
        cmd.arg("-hV");
        let output = run_debug_binary_no_input(cmd);

        assert_cmd_success(&output);

        let out = String::from_utf8_lossy(&output.stdout);
        let out = out.to_lowercase();
        assert!(out.contains("usage"));
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
            .args([
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
