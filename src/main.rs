use std::{fs, collections::HashMap, path::Path};

use clap::{Arg, Command};
use inquire::{Text, required, validator::StringValidator};
use regex::Regex;

use nessa::{context::*, config::NessaConfig};

#[derive(Clone)]
struct RegexValidator<'a> {
    regex: Regex,
    message: &'a str
}

impl<'a> StringValidator for RegexValidator<'a> {
    fn validate(&self, input: &str) -> Result<inquire::validator::Validation, inquire::CustomUserError> {
        if self.regex.is_match(input) {
            return Ok(inquire::validator::Validation::Valid);
        }

        return Ok(inquire::validator::Validation::Invalid(self.message.into()));
    }
}

impl<'a> RegexValidator<'a> {
    pub fn new(regex: &str, message: &'a str) -> Self {
        return RegexValidator {
            regex: Regex::new(regex).unwrap(), 
            message: message
        };
    }
}

const DEFAULT_CODE: &str = "print(\"Hello, world!\");";
const SEMVER_REGEX: &str = r"^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$";

fn main() {
    /*
        ╒══════════════════════════╕
        │ Console argument parsing │
        ╘══════════════════════════╛
    */

    let args = Command::new("Nessa Interpreter")
        .version("0.1 alpha")
        .author("Javier Castillo <javier.castillo.dev@gmail.com>")
        .about("Executes Nessa code")
        .subcommand(
            Command::new("run")        
                .arg(Arg::new("INPUT")
                .help("Specifies the file you want to execute")
                .required(true)
                .index(1))
        )
        .subcommand(Command::new("new"))
        .get_matches();

    /*
        ╒═══════════════════╕
        │ Command selection │
        ╘═══════════════════╛
    */

    match args.subcommand() {
        Some(("run", run_args)) => {
            let path = run_args.get_one::<String>("INPUT").expect("No input file was provided");
            let file = fs::read_to_string(path).expect("Unable to locate file");

            let mut ctx = standard_ctx();
            let res = ctx.parse_and_execute_nessa_module(&file);
            
            if let Err(err) = res {
                err.emit();
            }
        }

        Some(("new", _)) => {
            let name = Text::new("Project name:")
                .with_validator(required!("Project name must not be empty"))
                .with_validator(RegexValidator::new("^[a-zA-Z0-9_]+$", "Project name contains invalid characters"))
                .with_placeholder("my_new_project")
                .with_help_message("This is the name that you will use to import your library")
                .prompt().unwrap();

            let version = Text::new("Initial version:")
                .with_default("0.0.1")
                .with_validator(required!("Initial version must not be empty"))
                .with_validator(RegexValidator::new(SEMVER_REGEX, "Version does not follow SemVer"))
                .with_help_message("Versions can be changed later and must follow SemVer")
                .prompt().unwrap();

            let modules = Text::new("Modules path:")
                .with_default("libs")
                .with_validator(RegexValidator::new("^([a-zA-Z0-9_ ]+/?)+$", "Modules path contains invalid characters"))
                .with_placeholder("path/to/modules")
                .with_help_message("The interpreter will look for any imported libraries in this folder (you can add more in nessa_config.yml)")
                .prompt().unwrap().trim().to_string();

            let module_path = Path::new(&name);

            if module_path.exists() {
                panic!("Project folder already exists!");
            }

            fs::create_dir(&name).expect("Unable to create project directory");

            let config = NessaConfig {
                module_name: name.clone(),
                hash: "".into(),
                version: version,
                module_paths: vec!(modules.into()),
                modules: HashMap::new(),
            };

            fs::write(module_path.join(Path::new("nessa_config.yml")), serde_yaml::to_string(&config).unwrap()).expect("Unable to write configuration file");
            fs::write(module_path.join(Path::new("main.nessa")), DEFAULT_CODE).expect("Unable to write main file");
        }

        _ => panic!("Invalid subcommand")
    };
}