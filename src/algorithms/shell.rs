use std::io::{BufRead, BufReader};

use subprocess::{Exec, Redirection};

pub fn execute_command(command: &str) -> bool {
    let mut process = Exec::shell(command)
        .stdout(Redirection::Pipe)
        .stderr(Redirection::Merge)
        .popen()
        .expect("Failed to start process");

    let stdout = process.stdout.take().expect("Failed to capture stdout");

    let reader = BufReader::new(stdout);
    
    for line in reader.lines() {
        match line {
            Ok(l) => println!("{}", l),
            Err(e) => eprintln!("Error reading line: {}", e),
        }
    }

    return process.wait().expect("Process wasn't running").success();
}