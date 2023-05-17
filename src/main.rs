use clap::{App, Arg};
use std::fs;

use nessa::context::*;

fn main() {
    /*
        ╒══════════════════════════╕
        │ Console argument parsing │
        ╘══════════════════════════╛
    */

    let args = App::new("Nessa Interpreter")
        .version("0.1 alpha")
        .author("Javier Castillo <javier.castillo.dev@gmail.com>")
        .about("Executes Nessa code")
        .arg(Arg::with_name("INPUT")
            .help("Specifies the file you want to execute")
            .required(true)
            .index(1))
        .get_matches();

    /*
        ╒══════════════╕
        │ File reading │
        ╘══════════════╛
    */

    let path = args.value_of("INPUT").expect("No input file was provided");
    let mut file = fs::read_to_string(path).expect("Unable to locate file");
    file.push('\n');

    /*
        ╒════════════════╕
        │ Code execution │
        ╘════════════════╛
    */

    let mut ctx = standard_ctx();

    let res = ctx.parse_and_execute_nessa_module(&file);

    if let Err(err) = res {
        err.emit();
    }
}