use std::{fs, collections::{HashMap, HashSet}, path::Path};

use clap::{Arg, Command, ArgAction};
use colored::Colorize;
use inquire::{Text, required, validator::StringValidator, Autocomplete};
use regex::Regex;
use glob::glob;

use nessa::{context::*, config::{NessaConfig, ModuleInfo}};
use serde_yaml::{ from_str, to_string };

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

#[derive(Clone)]
struct OptionsAutocompleter {
    options: HashSet<String>
}

impl Autocomplete for OptionsAutocompleter {
    fn get_suggestions(&mut self, input: &str) -> Result<Vec<String>, inquire::CustomUserError> {
        return Ok(self.options.iter().filter(|i| i.starts_with(input)).cloned().collect());
    }

    fn get_completion(
        &mut self,
        input: &str,
        _highlighted_suggestion: Option<String>,
    ) -> Result<inquire::autocompletion::Replacement, inquire::CustomUserError> {
        let matches = self.options.iter().filter(|i| i.starts_with(input)).cloned().collect::<Vec<_>>();

        if matches.is_empty() {
            return Ok(inquire::autocompletion::Replacement::None);
        }

        let min_length = matches.iter().map(String::len).min().unwrap();
        let mut max_common = input.to_string();

        // Get maximum common start
        for i in input.len()..min_length {
            let substrs = matches.iter().map(|j| j[..=i].to_string()).collect::<HashSet<_>>();

            if substrs.len() > 1 {
                break;
            }

            max_common = substrs.into_iter().next().unwrap().to_string();
        }

        return Ok(inquire::autocompletion::Replacement::Some(max_common))
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
        .version("0.1.0")
        .author("Javier Castillo <javier.castillo.dev@gmail.com>")
        .about("Executes Nessa code")
        .subcommand(
            Command::new("run")      
                .about("Run Nessa project")
                .arg(
                    Arg::new("INPUT")
                    .help("Specifies the file you want to execute")
                    .required(false)
                    .default_value(".")
                    .index(1)
                )
                .arg(
                    Arg::new("recompile")
                    .help("Force recompilation")
                    .long("recompile")
                    .short('r')
                    .action(ArgAction::SetTrue)
                    .default_value("false")
                )
        )
        .subcommand(
            Command::new("new")
            .about("Create Nessa project with config files")
            .arg(
                Arg::new("name")
                .help("Project name")
                .required(false)
                .long("name")
                .short('n')
            )
            .arg(
                Arg::new("version")
                .help("Project version")
                .required(false)
                .long("version")
                .short('v')
            )
            .arg(
                Arg::new("modules")
                .help("Modules path")
                .required(false)
                .long("modules")
                .short('m')
            )
        )
        .subcommand(
            Command::new("add")
            .about("Add dependency to a Nessa project")
            .arg(
                Arg::new("name")
                .help("Module name")
                .required(false)
                .long("name")
                .short('n')
            )
            .arg(
                Arg::new("version")
                .help("Project version")
                .required(false)
                .long("version")
                .short('v')
            )
        )
        .get_matches();

    /*
        ╒═══════════════════╕
        │ Command selection │
        ╘═══════════════════╛
    */

    match args.subcommand() {
        Some(("run", run_args)) => {
            let path = run_args.get_one::<String>("INPUT").expect("No input folder was provided");
            let force_recompile = *run_args.get_one::<bool>("recompile").expect("Invalid recompilation flag");

            let mut ctx = standard_ctx();
            let res = ctx.parse_and_execute_nessa_project(path.into(), force_recompile);
            
            if let Err(err) = res {
                err.emit();
            }
        }

        Some(("new", run_args)) => {
            let name;
            let version;
            let modules;

            if let Some(n) = run_args.get_one::<String>("name") {
                name = n.clone();

            } else {
                name = Text::new("Project name:")
                    .with_validator(required!("Project name must not be empty"))
                    .with_validator(RegexValidator::new("^[a-zA-Z0-9_]+$", "Project name contains invalid characters"))
                    .with_placeholder("my_new_project")
                    .with_help_message("This is the name that you will use to import your module")
                    .prompt().unwrap();
            }

            if let Some(v) = run_args.get_one::<String>("version") {
                version = v.clone();

            } else {
                version = Text::new("Initial version:")
                    .with_default("0.1.0")
                    .with_validator(required!("Initial version must not be empty"))
                    .with_validator(RegexValidator::new(SEMVER_REGEX, "Version does not follow SemVer"))
                    .with_help_message("Versions can be changed later and must follow SemVer")
                    .prompt().unwrap();
            }

            if let Some(m) = run_args.get_one::<String>("modules") {
                modules = m.clone();

            } else {
                modules = Text::new("Modules path:")
                    .with_default("libs")
                    .with_validator(RegexValidator::new("^((([a-zA-Z0-9_ ]+)|(\\.\\.))/?)+$", "Modules path contains invalid characters"))
                    .with_placeholder("path/to/modules")
                    .with_help_message("The interpreter will look for any imported modules in this folder (you can add more in nessa_config.yml)")
                    .prompt().unwrap().trim().to_string();
            }

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

        Some(("add", run_args)) => {
            let module_path = Path::new(".");

            let config_path = module_path.join(Path::new("nessa_config.yml"));
            let main_path = module_path.join(Path::new("main.nessa"));

            if !config_path.exists() {
                panic!("No project config file!");
            }

            if !main_path.exists() {
                panic!("No main nessa file!");
            }

            let config = fs::read_to_string(&config_path).expect("Unable to read config file");
            let mut config_yml: NessaConfig = from_str(&config).expect("Unable to parse config file");

            let mut module_versions = HashMap::<String, HashSet<_>>::new();
            let mut paths = HashMap::new();

            for path in &config_yml.module_paths {
                for file in glob(format!("{}/*/nessa_config.yml", path).as_str()).expect("Error while reading module path") {
                    match file {
                        Ok(f) => {
                            let config_f = fs::read_to_string(f.clone()).expect("Unable to read config file");
                            let config_yml_f: NessaConfig = from_str(&config_f).expect("Unable to parse config file");
                            module_versions.entry(config_yml_f.module_name.clone()).or_default().insert(config_yml_f.version.clone());

                            paths.insert((config_yml_f.module_name, config_yml_f.version), f.parent().unwrap().to_str().unwrap().to_string());
                        },
        
                        _ => {}
                    }
                }    
            }

            let name;
            let version;

            if let Some(n) = run_args.get_one::<String>("name") {
                name = n.clone();

            } else {
                name = Text::new("Module name:")
                    .with_validator(required!("Module name must not be empty"))
                    .with_validator(RegexValidator::new("^[a-zA-Z0-9_]+$", "Module name contains invalid characters"))
                    .with_placeholder("my_module")
                    .with_help_message("This is the name of the module you are importing")
                    .with_autocomplete(OptionsAutocompleter {
                        options: module_versions.keys().cloned().collect()
                    })
                    .prompt().unwrap();
            }

            if let Some(v) = run_args.get_one::<String>("version") {
                version = v.clone();

            } else {
                version = Text::new("Version:")
                    .with_validator(required!("Module version must not be empty"))
                    .with_validator(RegexValidator::new(SEMVER_REGEX, "Version does not follow SemVer"))
                    .with_help_message("Versions can be changed later and must follow SemVer")
                    .with_autocomplete(OptionsAutocompleter {
                        options: module_versions.get(&name).cloned().unwrap_or(HashSet::new()).into_iter().collect()
                    })
                    .prompt().unwrap();
            }

            if paths.get(&(name.clone(), version.clone())).is_none() {
                println!(
                    "{} module {} {} was not found. Setting empty module path...",
                    "Warning:".yellow(),
                    name.green(),
                    format!("v{version}").cyan()
                )
            }

            config_yml.modules.insert(name.clone(), ModuleInfo {
                path: paths.get(&(name, version.clone())).cloned().unwrap_or("".into()),
                version: version,
                dependencies: HashSet::new(),
            });

            fs::write(config_path, to_string(&config_yml).unwrap()).expect("Unable to update configuration file");
        }

        _ => unreachable!()
    };
}