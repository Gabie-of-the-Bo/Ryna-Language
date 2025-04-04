use std::{collections::{HashMap, HashSet}, fs, path::{Path, PathBuf}};

use clap::{Arg, Command, ArgAction};
use colored::Colorize;
use inquire::{Text, required, Confirm};
use glob::glob;

use ryna::{config::{generate_docs, ModuleInfo, RynaConfig, CONFIG}, context::*, dependencies::{get_lib_versions, get_library_index, index_topological_order, select_lib_version, select_uninstall_version, OptionsAutocompleter, RegexValidator, MIN_SEMVER, SEMVER_REGEX}, git::{install_prelude, install_repo, uninstall_repo, update_library_index}, ryna_error, ryna_warning, shell::execute_command};
use serde_yaml::{ from_str, to_string };

const DEFAULT_CODE: &str = "print(\"Hello, world!\");";
const DEFAULT_GITIGNORE: &str = "ryna_cache\nryna_config.yml";
const PATH_REGEX: &str = r"^((([a-zA-Z0-9_ -]+)|(\.\.)|([A-Z]:(\/|\\)))(\/|\\)?)+$";

fn main() {
    /*
        ╒══════════════════════════╕
        │ Console argument parsing │
        ╘══════════════════════════╛
    */

    let mut cli = Command::new("ryna")
        .version(env!("CARGO_PKG_VERSION"))
        .author("Javier Castillo <javier.castillo.dev@gmail.com>")
        .about("Executes Ryna code")
        .subcommand(
            Command::new("run")
                .about("Run Ryna project")
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
            .about("Create Ryna project with config files")
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
            .about("Add dependency to a Ryna project")
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
            .arg(
                Arg::new("modules")
                .help("Modules path")
                .required(false)
                .long("modules")
                .short('m')
            )
        )
        .subcommand(
            Command::new("install")
            .about("Install a library pack from a git repository")
            .arg(
                Arg::new("NAME")
                .help("Name of the library that you want to install")
                .required(true)
                .index(1)
            )
            .arg(
                Arg::new("repository")
                .help("Repository URL")
                .required(false)
                .long("repository")
                .short('r')
            )
            .arg(
                Arg::new("version")
                .help("Library version to install")
                .required(false)
                .long("version")
                .short('v')
            )
            .arg(
                Arg::new("execute-build")
                .help("Execute build scripts by default")
                .long("execute-build")
                .short('b')
                .action(ArgAction::SetTrue)
                .default_value("false")
            )
        )
        .subcommand(
            Command::new("search")
            .about("Look for a library in the Ryna Library Index")
            .arg(
                Arg::new("NAME")
                .help("Name of the library that you want to install")
                .required(true)
                .index(1)
            )
            .arg(
                Arg::new("versions")
                .help("Fetch available versions")
                .long("versions")
                .short('v')
                .action(ArgAction::SetTrue)
                .default_value("false")
            )
        )
        .subcommand(
            Command::new("build")
            .about("Execute the build script for a library")
            .arg(
                Arg::new("INPUT")
                .help("Specifies the file you want to execute")
                .required(false)
                .default_value(".")
                .index(1)
            )
        )
        .subcommand(
            Command::new("uninstall")
            .about("Uninstall a library pack")
            .arg(
                Arg::new("NAME")
                .help("Name of the library that you want to install")
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
                RynaContext::parse_and_execute_ryna_project::<true>(path.into(), force_recompile || profile, optimize, test, &program_input)

            } else {
                RynaContext::parse_and_execute_ryna_project::<false>(path.into(), force_recompile || profile, optimize, test, &program_input)
            };
            
            match res {
                Ok(ex) => {
                    if ex.profiling_info.is_some() {
                        let proj_path = Path::new(path);
                        let prof_path = proj_path.join("ryna_cache/prof.json");
    
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
                    .with_default(MIN_SEMVER)
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
                    ryna_warning!(
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
                        .with_help_message("The interpreter will look for any imported modules in this folder (you can add more in ryna_config.yml)")
                        .prompt().unwrap().trim().to_string()
                    );
                }
            }

            let module_path = Path::new(&name);

            if module_path.exists() {
                ryna_error!("Project folder already exists!");
            }

            fs::create_dir(&name).expect("Unable to create project directory");

            let config = RynaConfig {
                module_name: name.clone(),
                hash: "".into(),
                version,
                build: String::new(),
                module_paths: modules,
                modules: HashMap::new(),
            };

            fs::write(module_path.join(Path::new("ryna_config.yml")), serde_yaml::to_string(&config).unwrap()).expect("Unable to write configuration file");
            fs::write(module_path.join(Path::new("main.ryna")), DEFAULT_CODE).expect("Unable to write main file");

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

            let config_path = module_path.join(Path::new("ryna_config.yml"));
            let main_path = module_path.join(Path::new("main.ryna"));

            if !config_path.exists() {
                ryna_error!("No project config file!");
            }

            if !main_path.exists() {
                ryna_error!("No main ryna file!");
            }

            let config = fs::read_to_string(&config_path).expect("Unable to read config file");
            let mut config_yml: RynaConfig = from_str(&config).expect("Unable to parse config file");

            let mut module_versions = HashMap::<String, HashSet<_>>::new();
            let mut paths = HashMap::new();

            for path in &config_yml.module_paths {
                for f in glob(format!("{}/**/ryna_config.yml", path).as_str()).expect("Error while reading module path").flatten() {
                    let config_f = fs::read_to_string(f.clone()).expect("Unable to read config file");
                    let config_yml_f: RynaConfig = from_str(&config_f).expect("Unable to parse config file");
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
                ryna_warning!(
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

        Some(("setup", run_args)) => {
            let value;

            if let Some(v) = run_args.get_one::<String>("modules") {
                value = v.clone();

            } else {
                value = Text::new("Libraries path:")
                    .with_default("libs")
                    .with_validator(RegexValidator::new(PATH_REGEX, "Modules path contains invalid characters"))
                    .with_placeholder("path/to/modules")
                    .with_help_message("The interpreter will install modules in this folder by default")
                    .prompt().unwrap();
            }

            println!("Updating global configuration...");

            CONFIG.write().unwrap().modules_path = value;
            CONFIG.write().unwrap().save().unwrap();

            println!("Installing prelude...");

            match install_prelude() {
                Ok(_) => {},
                Err(err) => ryna_error!("{}", err),
            }
        }

        Some(("install", run_args)) => {
            let pack_name = run_args.get_one::<String>("NAME").expect("No pack name was provided");
            let lib_version = run_args.get_one::<String>("version");
            let execute_build = run_args.get_one::<bool>("execute-build").expect("No build script config");

            let mut libs_to_install = vec!();

            match run_args.get_one::<String>("repository") {
                Some(url) => {
                    let (selected_version, branch_name) = select_lib_version(url, pack_name, lib_version);

                    libs_to_install.push((
                        pack_name.clone(),
                        url.clone(),
                        selected_version.clone(),
                        branch_name.clone()
                    ));    
                },

                None => {
                    println!("{}", "\nUpdating library index...".bold());
                    
                    if let Err(err) = update_library_index() {
                        ryna_error!("{}", err);
                    }
                    
                    match get_library_index() {
                        Ok(index) => {
                            if !index.contains_key(pack_name) {
                                ryna_error!("Library index does not contain {}", pack_name.green());
                            }

                            let lib = index.get(pack_name).unwrap();
                            let (selected_version, branch_name) = select_lib_version(&lib.repository, pack_name, lib_version);

                            println!(" - Resolving dependency tree...");

                            libs_to_install.push((
                                pack_name.clone(),
                                lib.repository.clone(),
                                selected_version.clone(),
                                branch_name.clone()
                            ));

                            index_topological_order(&index, &mut libs_to_install);

                            if libs_to_install.len() > 1 {
                                for (pack_name, _, version, _) in &libs_to_install[..libs_to_install.len() - 1] {
                                    println!("   * Dependency: {} {}", pack_name.green(), format!("v{}", version).cyan());
                                }
                            }

                            println!(" - Done!");
                        },

                        Err(err) => ryna_error!("{}", err),
                    }
                },
            };

            println!("{}", "\nInstalling dependencies...".bold());

            for (pack_name, repo_url, version, branch) in libs_to_install {
                // Check already installed libs
                let module_path = Path::new(&CONFIG.write().unwrap().modules_path).join(&pack_name).join(format!("v{}", version));

                if module_path.exists() {
                    println!(" - Skipping {} {} (already installed)", pack_name.green(), format!("v{}", version).cyan());
                    continue;

                } else {
                    println!(" - Installing {} {}...", pack_name.green(), format!("v{}", version).cyan());
                }

                // Install repository
                match install_repo(&repo_url, &pack_name, &version, &branch) {
                    Ok(_) => {},
                    Err(err) => ryna_error!("{}", err),
                }
    
                // Check install script
                let config_path = module_path.join(Path::new("ryna_deps.yml"));
    
                if !config_path.exists() {
                    ryna_warning!("Could not find ryna_deps.yml at the root of the library (perhaps you installed multiple libraries at once?)");
                    continue;
                }
    
                let config = fs::read_to_string(&config_path).expect("Unable to read config file");
                let config_yml: RynaConfig = from_str(&config).expect("Unable to parse config file");
    
                if !config_yml.build.is_empty() {
                    let mut has_build = *execute_build;
                    
                    if !has_build {
                        has_build = Confirm::new(&format!("Build script for {} was detected. Do you want to execute it?", pack_name.green())).prompt().unwrap();
                        println!();
                    }
    
                    if has_build {
                        if !execute_command(&config_yml.build, &module_path) {
                            println!("Build script failed. Cleaning up...");
    
                            match uninstall_repo(&pack_name, &version) {
                                Ok(_) => {},
                                Err(err) => ryna_error!("{}", err),
                            }
    
                            ryna_error!("Build command failed for {}", pack_name.green());
                        }
                    }
                }   
            }

            println!(" - Done!");
        }

        Some(("uninstall", run_args)) => {
            let pack_name = run_args.get_one::<String>("NAME").expect("No pack name was provided");

            println!("\n{}", format!("Uninstalling {}...", pack_name.green()).bold());

            let version = match select_uninstall_version(pack_name) {
                Ok(p) => p,
                Err(err) => ryna_error!("{}", err),
            };

            match uninstall_repo(pack_name, &version) {
                Ok(_) => {},
                Err(err) => ryna_error!("{}", err),
            }

            println!(" - Done!");
        }

        Some(("search", run_args)) => {
            let pack_name = run_args.get_one::<String>("NAME").expect("No pack name was provided");
            let versions = run_args.get_one::<bool>("versions").expect("No versions param provided");

            println!("{}", "\nUpdating library index...".bold());
            
            if let Err(err) = update_library_index() {
                ryna_error!("{}", err);
            }
            
            let index = match get_library_index() {
                Ok(i) => i,
                Err(err) => ryna_error!("{}", err),
            };

            println!(" - Done!");

            let matches = index.iter().filter(|(i, _)| i.contains(pack_name)).collect::<Vec<_>>();

            println!("{}", "\nMatches found:".bold());
            
            for (name, info) in matches {
                println!(" - {}", name.green());
                
                if *versions {
                    match get_lib_versions(&info.repository) {
                        Ok(available_versions) => {
                            for v in available_versions {
                                println!("   * {}", format!("v{}", v.0).cyan());
                            }                                    
                        },

                        Err(err) => {
                            println!("   * Unable to fetch versions: {}", err);
                        },
                    }
                }
            }
        }

        Some(("build", run_args)) => {
            // Check install script
            let path = run_args.get_one::<String>("INPUT").expect("No input folder was provided");
            let module_path = PathBuf::from(path);
            let config_path = module_path.join(Path::new("ryna_config.yml"));

            if !config_path.exists() {
                ryna_error!("No project config file!");
            }

            let config = fs::read_to_string(&config_path).expect("Unable to read config file");
            let config_yml: RynaConfig = from_str(&config).expect("Unable to parse config file");

            if !config_yml.build.is_empty() {
                if !execute_command(&config_yml.build, &module_path) {
                    ryna_error!("Build command failed for {}", config_yml.module_name.green());
                }

            } else {
                ryna_error!("No build command was found");
            }
        }

        Some(("save-deps", _)) => {
            let module_path = Path::new(".");

            let config_path = module_path.join(Path::new("ryna_config.yml"));

            if !config_path.exists() {
                ryna_error!("No project config file!");
            }

            let config = fs::read_to_string(&config_path).expect("Unable to read config file");
            let mut config_yml: RynaConfig = from_str(&config).expect("Unable to parse config file");

            // Anonymize
            config_yml.hash.clear();
            config_yml.module_paths.clear();
            config_yml.modules.iter_mut().for_each(|m| {
                m.1.dependencies.clear();
                m.1.path.clear();
            });

            fs::write(module_path.join(Path::new("ryna_deps.yml")), serde_yaml::to_string(&config_yml).unwrap()).expect("Unable to write configuration file");
        }

        Some(("load-deps", run_args)) => {
            let module_path = Path::new(".");

            let deps_path = module_path.join(Path::new("ryna_deps.yml"));

            if !deps_path.exists() {
                ryna_error!("No project requirements file!");
            }

            let deps = fs::read_to_string(&deps_path).expect("Unable to read requirements file");
            let mut deps_yml: RynaConfig = from_str(&deps).expect("Unable to parse requirements file");

            if !CONFIG.read().unwrap().modules_path.is_empty() {
                deps_yml.module_paths.push(CONFIG.read().unwrap().modules_path.clone());
            
            } else {
                ryna_error!("Default modules path was not found! Try executing ryna setup");    
            }

            if let Some(m) = run_args.get_one::<String>("modules") {
                deps_yml.module_paths.push(m.clone());
            }

            let mut module_versions = HashMap::<String, HashSet<_>>::new();
            let mut paths = HashMap::new();

            for path in &deps_yml.module_paths {
                for f in glob(format!("{}/**/ryna_config.yml", path).as_str()).expect("Error while reading module path").flatten() {
                    let config_f = fs::read_to_string(f.clone()).expect("Unable to read config file");
                    let config_yml_f: RynaConfig = from_str(&config_f).expect("Unable to parse config file");
                    module_versions.entry(config_yml_f.module_name.clone()).or_default().insert(config_yml_f.version.clone());

                    paths.insert((config_yml_f.module_name, config_yml_f.version), f.parent().unwrap().to_str().unwrap().to_string());
                }    
            }

            for module in deps_yml.modules.iter_mut() {
                if !module_versions.contains_key(module.0) {
                    ryna_error!("Module {} not found!", module.0.green());    
                }
                
                if !module_versions.get(module.0).unwrap().contains(&module.1.version) {
                    ryna_error!("Version {} for module {} not found!", format!("v{}", module.1.version).cyan(), module.0.green());    
                }

                module.1.path = paths.get(&(module.0.clone(), module.1.version.clone())).unwrap().clone();
            }

            fs::write(module_path.join(Path::new("ryna_config.yml")), serde_yaml::to_string(&deps_yml).unwrap()).expect("Unable to write configuration file");
        }

        _ => {
            cli.print_long_help().unwrap();
        }
    };
}