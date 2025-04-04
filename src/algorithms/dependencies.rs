use std::{collections::HashSet, path::Path};

use colored::Colorize;
use inquire::{required, validator::StringValidator, Autocomplete, Text};
use rustc_hash::FxHashMap;
use regex::Regex;
use serde::Deserialize;
use semver::Version;
use glob::glob;

use crate::{config::CONFIG, git::get_branch_names, ryna_error};

pub const MIN_SEMVER: &str = "0.1.0";
pub const SEMVER_REGEX: &str = r"^(0|[1-9]\d*)\.(0|[1-9]\d*)\.(0|[1-9]\d*)(?:-((?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*)(?:\.(?:0|[1-9]\d*|\d*[a-zA-Z-][0-9a-zA-Z-]*))*))?(?:\+([0-9a-zA-Z-]+(?:\.[0-9a-zA-Z-]+)*))?$";

#[derive(Clone)]
pub struct RegexValidator<'a> {
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
pub struct OptionsAutocompleter {
    pub options: HashSet<String>
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

#[derive(Clone, Deserialize)]
pub struct LibraryItem {
    pub repository: String,
    pub dependencies: FxHashMap<String, FxHashMap<String, String>> // Version: {Library: Version}
}

pub fn get_lib_versions(repo_url: &str) -> Result<FxHashMap<String, String>, String> {
    let branches = get_branch_names(repo_url)?;
    let mut res = FxHashMap::default();

    if branches.len() == 1 {
        res.insert(MIN_SEMVER.into(), branches.first().unwrap().clone());
    
    } else {
        let semver = Regex::new(SEMVER_REGEX).unwrap();

        for b in branches {
            if b.starts_with("v") && semver.is_match(&b[1..]) {
                res.insert(b[1..].into(), b.clone());
            }
        }
    }

    if res.is_empty() {
        return Err(format!("No valid version branches found in {}", repo_url));
    }

    Ok(res)
}

pub fn get_latest_lib_version(repo_url: &str) -> Result<(String, String), String> {
    let mut versions = get_lib_versions(repo_url)?.into_iter().collect::<Vec<_>>();

    versions.sort_by_key(|i| Version::parse(&i.0).unwrap());

    let latest = versions.last().unwrap();

    Ok((latest.0.clone(), latest.1.clone()))
}

pub fn select_lib_version(repo_url: &str, pack_name: &str, lib_version: Option<&String>) -> (String, String) {
    let available_versions = match get_lib_versions(&repo_url) {
        Ok(vs) => vs,
        Err(err) => ryna_error!("{}", err)
    };

    let selected_version = match lib_version {
        Some(v) => {
            if !available_versions.contains_key(v) {
                ryna_error!("Version v{} is not available for library {}", format!("v{}", v).cyan(), pack_name.green());
            }

            v.clone()
        },

        None => {
            let v;
            
            if available_versions.len() > 1 {
                v = Text::new("Select a version to install:")
                    .with_validator(required!("Module version must not be empty"))
                    .with_validator(RegexValidator::new(SEMVER_REGEX, "Version does not follow SemVer"))
                    .with_autocomplete(OptionsAutocompleter {
                        options: available_versions.keys().cloned().collect()
                    })
                    .prompt().unwrap();
            
            } else {                
                v = available_versions.iter().next().unwrap().0.clone();
                
                println!(" - Only one version found in repository: {}", format!("v{}", v).cyan());
            }

            if !available_versions.contains_key(&v) {
                ryna_error!("Version v{} is not available for library {}", format!("v{}", v).cyan(), pack_name.green());
            }

            v.clone()
        }
    };

    (selected_version.clone(), available_versions.get(&selected_version).unwrap().clone())
}

pub fn select_uninstall_version(pack_name: &str) -> Result<String, String> {
    let modules_path = &CONFIG.write().unwrap().modules_path;

    let installed_versions = glob(format!("{modules_path}/{pack_name}/*").as_str())
        .expect("Error while reading module path")
        .flatten()
        .map(|version_folder| {
            version_folder.file_name().and_then(|i| i.to_str()).unwrap()[1..].to_string()
        })
        .collect::<Vec<_>>();

    if installed_versions.is_empty() {
        return Err(format!("Pack \"{}\" is not installed", pack_name));
    }

    let version_to_uninstall;
    
    if installed_versions.len() > 1 {
        version_to_uninstall = Text::new("Select a version to uninstall:")
            .with_validator(required!("Module version must not be empty"))
            .with_validator(RegexValidator::new(SEMVER_REGEX, "Version does not follow SemVer"))
            .with_autocomplete(OptionsAutocompleter {
                options: installed_versions.iter().cloned().collect()
            })
            .prompt().unwrap();

    } else {                
        version_to_uninstall = installed_versions.iter().next().unwrap().clone();

        println!(" - Only one version found in repository: {}", format!("v{}", version_to_uninstall).cyan());
    }

    Ok(version_to_uninstall)
}

pub fn get_library_index() -> Result<FxHashMap<String, LibraryItem>, String> {
    let path = Path::new(&CONFIG.write().unwrap().modules_path).join(".index").join("index.yml");

    let index_file = match std::fs::read_to_string(&path) {
        Ok(f) => f,
        Err(_) => return Err("Unable to read index file".into()),
    };

    match serde_yaml::from_str(&index_file) {
        Ok(res) => Ok(res),
        Err(_) => return Err("Malformed index file".into()),
    } 
}

pub fn index_topological_order(index: &FxHashMap<String, LibraryItem>, libs: &mut Vec<(String, String, String, String)>) {
    let mut stack = vec!(libs.get(0).unwrap().clone());
    let mut seen = [(stack[0].0.clone(), stack[0].2.clone())].iter().cloned().collect::<HashSet<_>>();

    while !stack.is_empty() {
        let mut new_items = vec!();

        for (name, _, version, _) in &stack {
            if let Some(lib) = index.get(name) {
                if let Some(dep) = lib.dependencies.get(&format!("v{}", version)) {                    
                    for (dep_name, dep_version) in dep {
                        if !seen.contains(&(dep_name.clone(), dep_version[1..].into())) {
                            let repository = &index.get(dep_name).unwrap().repository;
                            let versions = get_lib_versions(repository).unwrap();
    
                            seen.insert((dep_name.clone(), dep_version[1..].into()));

                            new_items.push((
                                dep_name.clone(),
                                repository.clone(),
                                dep_version[1..].into(),
                                versions.get(dep_version[1..].into()).unwrap().clone()
                            ));
                        }
                    }
                }
            }
        }

        libs.extend(new_items.iter().cloned());
        stack = new_items;
    }

    libs.reverse();
}