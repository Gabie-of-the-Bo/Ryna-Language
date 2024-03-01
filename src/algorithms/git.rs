use std::path::Path;

use git2::build::RepoBuilder;
use regex::Regex;

use crate::config::CONFIG;

const NAME_REGEX: &str = r"^[a-zA-Z0-9_ -]+$";

pub fn install_repo(repo_url: &str, pack_name: &str) -> Result<(), String> {
    let name_regex = Regex::new(NAME_REGEX).unwrap();

    if !name_regex.is_match(pack_name) {
        return Err("Pack name contains invalid characters".to_string());
    }

    let path = Path::new(&CONFIG.write().unwrap().modules_path).join(pack_name);

    if path.exists() {
        return Err(format!("pack name \"{}\" is already taken", pack_name));
    }

    match RepoBuilder::new().clone(repo_url, &path) {
        Ok(_) => Ok(()),
        Err(_) => {
            Err(format!("Unable to download library pack from {}", repo_url))
        },
    }
}

pub fn uninstall_repo(pack_name: &str) -> Result<(), String> {
    let name_regex = Regex::new(NAME_REGEX).unwrap();

    if !name_regex.is_match(pack_name) {
        return Err("Pack name contains invalid characters".to_string());
    }

    let path = Path::new(&CONFIG.write().unwrap().modules_path).join(pack_name);

    if !path.exists() {
        return Err(format!("Pack \"{}\" is not installed", pack_name));
    }

    // Sanity check
    let path_git = path.join(".git");

    if !path_git.exists() {
        return Err(format!("Pack \"{}\" is not a git repository (maybe your configuration is wrong?)", pack_name));
    }

    std::fs::remove_dir_all(path).unwrap();

    Ok(())
}

/*
    Standard libraries
*/

pub fn install_prelude() -> Result<(), String> {
    const PRELUDE_URL: &str = "https://github.com/Gabie-of-the-Bo/Nessa-prelude.git";

    install_repo(PRELUDE_URL, "prelude")
}