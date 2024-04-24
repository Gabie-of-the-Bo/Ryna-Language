use std::{collections::{HashMap, HashSet}, fs, path::Path};

use clap::{Arg, Command, ArgAction};
use colored::Colorize;
use inquire::{Text, required, validator::StringValidator, Autocomplete, Confirm};
use regex::Regex;
use glob::glob;

use nessa::{config::{generate_docs, ModuleInfo, NessaConfig, CONFIG}, context::*, git::{install_prelude, install_repo, uninstall_repo}, nessa_error, nessa_warning};
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

        Ok(inquire::validator::Validation::Invalid(self.message.into()))
    }
}

impl<'a> RegexValidator<'a> {
    pub fn new(regex: &str, message: &'a str) -> Self {
        RegexValidator {
            regex: Regex::new(regex).unwrap(), 
            message
        }
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

        Ok(inquire::autocompletion::Replacement::Some(max_common))
    }
}

const DEFAULT_CODE: &str = "print(\"Hello, world!\");";
const DEFAULT_GITIGNORE: &str = "nessa_cache\nnessa_config.yml";
const SEMVER_REGEX: &str = r"^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$";
const PATH_REGEX: &str = r"^((([a-zA-Z0-9_ -]+)|(\.\.)|([A-Z]:(\/|\\)))(\/|\\)?)+$";

fn main() {
    /*
        ╒══════════════════════════╕
        │ Console argument parsing │
        ╘══════════════════════════╛
    */

    let mut cli = Command::new("nessa")
        .version(env!("CARGO_PKG_VERSION"))
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
                    Arg::new("PROGRAM_INPUT")
                    .help("Program input")
                    .required(false)
                    .index(2)
                    .num_args(0..)
                )
                .arg(
                    Arg::new("recompile")
                    .help("Force recompilation")
                    .long("recompile")
                    .short('r')
                    .action(ArgAction::SetTrue)
                    .default_value("false")
                )
                .arg(
                    Arg::new("optimize")
                    .help("Optimize code")
                    .long("optimize")
                    .short('o')
                    .action(ArgAction::SetTrue)
                    .default_value("false")
                )
                .arg(
                    Arg::new("profile")
                    .help("Profile code")
                    .long("profile")
                    .short('p')
                    .action(ArgAction::SetTrue)
                    .default_value("false")
                )
                .arg(
                    Arg::new("test")
                    .help("Run tests")
                    .long("test")
                    .short('t')
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
            .arg(
                Arg::new("no-gitignore")
                .help("Do not create a .gitignore file")
                .long("no-gitignore")
                .short('g')
                .action(ArgAction::SetTrue)
                .default_value("false")
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
        .subcommand(
            Command::new("setup")
            .about("Set up global configuration and install prelude")
        )
        .subcommand(
            Command::new("install")
            .about("Install a library pack from a git repository")
            .arg(
                Arg::new("REPOSITORY")
                .help("Specifies the file you want to execute")
                .required(true)
                .index(1)
            )
            .arg(
                Arg::new("NAME")
                .help("Name of the library reprository that you want to install")
                .required(true)
                .index(2)
            )
        )
        .subcommand(
            Command::new("uninstall")
            .about("Uninstall a library pack")
            .arg(
                Arg::new("NAME")
                .help("Name of the library reprository that you want to install")
                .required(true)
                .index(1)
            )
        )
        .subcommand(
            Command::new("docs")
            .about("Generate documentation")
            .arg(
                Arg::new("INPUT")
                .help("Specifies the project for which you want to generate documentation")
                .required(false)
                .default_value(".")
                .index(1)
            )
        )
        .subcommand(
            Command::new("save-deps")
            .about("Create project requirements file")
        )
        .subcommand(
            Command::new("load-deps")
            .about("Create project requirements file")
            .arg(
                Arg::new("modules")
                .help("Modules path")
                .required(false)
                .long("modules")
                .short('m')
            )
        );
        
    let args = cli.clone().get_matches();

    /*
        ╒═══════════════════╕
        │ Command selection │
        ╘═══════════════════╛
    */

    match args.subcommand() {
        Some(("run", run_args)) => {
            let path = run_args.get_one::<String>("INPUT").expect("No input folder was provided");
            let force_recompile = *run_args.get_one::<bool>("recompile").expect("Invalid recompilation flag");
            let optimize = *run_args.get_one::<bool>("optimize").unwrap_or(&false);
            let profile = *run_args.get_one::<bool>("profile").unwrap_or(&false);
            let test = *run_args.get_one::<bool>("test").unwrap_or(&false);

            let program_input = match run_args.get_many::<String>("PROGRAM_INPUT") {
                Some(i) => i.cloned().collect::<Vec<_>>(),
                None => vec!(),
            };

            let res = if profile {
                NessaContext::parse_and_execute_nessa_project::<true>(path.into(), force_recompile || profile, optimize, test, &program_input)

            } else {
                NessaContext::parse_and_execute_nessa_project::<false>(path.into(), force_recompile || profile, optimize, test, &program_input)
            };
            
            match res {
                Ok(ex) => {
                    if ex.profiling_info.is_some() {
                        let proj_path = Path::new(path);
                        let prof_path = proj_path.join("nessa_cache/prof.json");
    
                        let prof_file = serde_json::to_string(&ex.profiling_info).expect("Unable to serialize profiling information");
    
                        fs::write(prof_path, prof_file).expect("Unable to write profiling information file");    
                    }
                },

                Err(err) => err.emit(),
            }
        }

        Some(("new", run_args)) => {
            let name;
            let version;
            let mut modules = vec!();

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
                modules.push(m.clone());

            } else {
                if !CONFIG.read().unwrap().modules_path.is_empty() {
                    let add_env = Confirm::new("Default modules path was detected. Add it to module paths?").prompt().unwrap();

                    if add_env {
                        modules.push(CONFIG.read().unwrap().modules_path.clone());
                    }
                
                } else {
                    nessa_warning!(
                        "Default modules path was not found. Skipping this dependency folder..."
                    );    
                }

                let mut res = true;

                if !modules.is_empty() {
                    res = Confirm::new("Add a secondary modules folder?").prompt().unwrap();
                }

                if res {
                    modules.push(
                        Text::new("Modules path:")
                        .with_default("libs")
                        .with_validator(RegexValidator::new(PATH_REGEX, "Modules path contains invalid characters"))
                        .with_placeholder("path/to/modules")
                        .with_help_message("The interpreter will look for any imported modules in this folder (you can add more in nessa_config.yml)")
                        .prompt().unwrap().trim().to_string()
                    );
                }
            }

            let module_path = Path::new(&name);

            if module_path.exists() {
                nessa_error!("Project folder already exists!");
            }

            fs::create_dir(&name).expect("Unable to create project directory");

            let config = NessaConfig {
                module_name: name.clone(),
                hash: "".into(),
                version,
                module_paths: modules,
                modules: HashMap::new(),
            };

            fs::write(module_path.join(Path::new("nessa_config.yml")), serde_yaml::to_string(&config).unwrap()).expect("Unable to write configuration file");
            fs::write(module_path.join(Path::new("main.nessa")), DEFAULT_CODE).expect("Unable to write main file");

            let gitignore = !run_args.get_one::<bool>("no-gitignore").expect("Invalid no-gitignore flag");

            if gitignore {
                fs::write(module_path.join(Path::new(".gitignore")), DEFAULT_GITIGNORE).expect("Unable to write .gitignore");
            }
        }

        Some(("docs", run_args)) => {
            let path = run_args.get_one::<String>("INPUT").expect("No input folder was provided");

            if let Err(err) = generate_docs(path) {
                err.emit();
            }
        }

        Some(("add", run_args)) => {
            let module_path = Path::new(".");

            let config_path = module_path.join(Path::new("nessa_config.yml"));
            let main_path = module_path.join(Path::new("main.nessa"));

            if !config_path.exists() {
                nessa_error!("No project config file!");
            }

            if !main_path.exists() {
                nessa_error!("No main nessa file!");
            }

            let config = fs::read_to_string(&config_path).expect("Unable to read config file");
            let mut config_yml: NessaConfig = from_str(&config).expect("Unable to parse config file");

            let mut module_versions = HashMap::<String, HashSet<_>>::new();
            let mut paths = HashMap::new();

            for path in &config_yml.module_paths {
                for f in glob(format!("{}/**/nessa_config.yml", path).as_str()).expect("Error while reading module path").flatten() {
                    let config_f = fs::read_to_string(f.clone()).expect("Unable to read config file");
                    let config_yml_f: NessaConfig = from_str(&config_f).expect("Unable to parse config file");
                    module_versions.entry(config_yml_f.module_name.clone()).or_default().insert(config_yml_f.version.clone());

                    paths.insert((config_yml_f.module_name, config_yml_f.version), f.parent().unwrap().to_str().unwrap().to_string());
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
                nessa_warning!(
                    "Module {} {} was not found. Setting empty module path...",
                    name.green(),
                    format!("v{version}").cyan()
                );
            }

            config_yml.modules.insert(name.clone(), ModuleInfo {
                path: paths.get(&(name, version.clone())).cloned().unwrap_or("".into()),
                version,
                is_local: false,
                dependencies: HashSet::new(),
            });

            fs::write(config_path, to_string(&config_yml).unwrap()).expect("Unable to update configuration file");
        }

        Some(("setup", _)) => {
            let value = Text::new("Libraries path:")
                .with_default("libs")
                .with_validator(RegexValidator::new(PATH_REGEX, "Modules path contains invalid characters"))
                .with_placeholder("path/to/modules")
                .with_help_message("The interpreter will install modules in this folder by default")
                .prompt().unwrap();

            println!("Updating global configuration...");

            CONFIG.write().unwrap().modules_path = value;
            CONFIG.write().unwrap().save().unwrap();

            println!("Installing prelude...");

            match install_prelude() {
                Ok(_) => {},
                Err(err) => nessa_error!("{}", err),
            }
        }

        Some(("install", run_args)) => {
            let repo_url = run_args.get_one::<String>("REPOSITORY").expect("No repository URL was provided");
            let pack_name = run_args.get_one::<String>("NAME").expect("No pack name was provided");

            match install_repo(repo_url, pack_name) {
                Ok(_) => {},
                Err(err) => nessa_error!("{}", err),
            }
        }

        Some(("uninstall", run_args)) => {
            let pack_name = run_args.get_one::<String>("NAME").expect("No pack name was provided");

            match uninstall_repo(pack_name) {
                Ok(_) => {},
                Err(err) => nessa_error!("{}", err),
            }
        }

        Some(("save-deps", _)) => {
            let module_path = Path::new(".");

            let config_path = module_path.join(Path::new("nessa_config.yml"));

            if !config_path.exists() {
                nessa_error!("No project config file!");
            }

            let config = fs::read_to_string(&config_path).expect("Unable to read config file");
            let mut config_yml: NessaConfig = from_str(&config).expect("Unable to parse config file");

            // Anonymize
            config_yml.hash.clear();
            config_yml.module_paths.clear();
            config_yml.modules.iter_mut().for_each(|m| {
                m.1.dependencies.clear();
                m.1.path.clear();
            });

            fs::write(module_path.join(Path::new("nessa_deps.yml")), serde_yaml::to_string(&config_yml).unwrap()).expect("Unable to write configuration file");
        }

        Some(("load-deps", run_args)) => {
            let module_path = Path::new(".");

            let deps_path = module_path.join(Path::new("nessa_deps.yml"));

            if !deps_path.exists() {
                nessa_error!("No project requirements file!");
            }

            let deps = fs::read_to_string(&deps_path).expect("Unable to read requirements file");
            let mut deps_yml: NessaConfig = from_str(&deps).expect("Unable to parse requirements file");

            if !CONFIG.read().unwrap().modules_path.is_empty() {
                deps_yml.module_paths.push(CONFIG.read().unwrap().modules_path.clone());
            
            } else {
                nessa_error!("Default modules path was not found! Try executing nessa setup");    
            }

            if let Some(m) = run_args.get_one::<String>("modules") {
                deps_yml.module_paths.push(m.clone());
            }

            let mut module_versions = HashMap::<String, HashSet<_>>::new();
            let mut paths = HashMap::new();

            for path in &deps_yml.module_paths {
                for f in glob(format!("{}/**/nessa_config.yml", path).as_str()).expect("Error while reading module path").flatten() {
                    let config_f = fs::read_to_string(f.clone()).expect("Unable to read config file");
                    let config_yml_f: NessaConfig = from_str(&config_f).expect("Unable to parse config file");
                    module_versions.entry(config_yml_f.module_name.clone()).or_default().insert(config_yml_f.version.clone());

                    paths.insert((config_yml_f.module_name, config_yml_f.version), f.parent().unwrap().to_str().unwrap().to_string());
                }    
            }

            for module in deps_yml.modules.iter_mut() {
                if !module_versions.contains_key(module.0) {
                    nessa_error!("Module {} not found!", module.0.green());    
                }
                
                if !module_versions.get(module.0).unwrap().contains(&module.1.version) {
                    nessa_error!("Version {} for module {} not found!", format!("v{}", module.1.version).cyan(), module.0.green());    
                }

                module.1.path = paths.get(&(module.0.clone(), module.1.version.clone())).unwrap().clone();
            }

            fs::write(module_path.join(Path::new("nessa_config.yml")), serde_yaml::to_string(&deps_yml).unwrap()).expect("Unable to write configuration file");
        }

        _ => {
            cli.print_long_help().unwrap();
        }
    };
}