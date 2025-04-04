use std::{fs, path::Path};

use colored::Colorize;
use git2::{build::RepoBuilder, Direction, Cred, FetchOptions, RemoteCallbacks, Repository};
use regex::Regex;
use tempfile::TempDir;

use crate::{config::CONFIG, dependencies::get_latest_lib_version};

const NAME_REGEX: &str = r"^[a-zA-Z0-9_ -]+$";

pub fn get_branch_names(repo_url: &str) -> Result<Vec<String>, String> {
    let tmp_dir = TempDir::new().expect("failed to create temp dir");
    let repo = Repository::init_bare(tmp_dir.path()).expect("Unable to init bare temp repository");
    
    let mut remote = repo.remote_anonymous(repo_url).unwrap();
    remote.connect(Direction::Fetch).unwrap();
    
    let mut res = Vec::new();

    for head in remote.list().unwrap() {
        let full_name = head.name();

        if full_name.starts_with("refs/heads/") {
            let branch = &full_name["refs/heads/".len()..];
            res.push(branch.to_string());
        }
    }
    
    remote.disconnect().unwrap();
    
    Ok(res)
}

pub fn install_repo(repo_url: &str, pack_name: &str, version: &str, branch: &str) -> Result<(), String> {
    let name_regex = Regex::new(NAME_REGEX).unwrap();

    if !name_regex.is_match(pack_name) {
        return Err("Pack name contains invalid characters".to_string());
    }

    let lib_path = Path::new(&CONFIG.write().unwrap().modules_path).join(pack_name);

    if !lib_path.exists() {
        fs::create_dir(&lib_path).expect("Unable to create library directory");
    }

    let path = lib_path.join(format!("v{}", version));

    if path.exists() {
        return Err(format!("Pack name \"{}\" is already taken", pack_name));
    }

    match RepoBuilder::new().branch(branch).clone(repo_url, &path) {
        Ok(_) => Ok(()),
        Err(_) => {
            Err(format!("Unable to download library pack from {}", repo_url))
        },
    }
}

pub fn uninstall_repo(pack_name: &str, version: &str) -> Result<(), String> {
    let name_regex = Regex::new(NAME_REGEX).unwrap();

    if !name_regex.is_match(pack_name) {
        return Err("Pack name contains invalid characters".to_string());
    }

    let library_path = Path::new(&CONFIG.write().unwrap().modules_path).join(pack_name);

    if !library_path.exists() {
        return Err(format!("Pack \"{}\" is not installed", pack_name));
    }

    let path = library_path.join(format!("v{}", version));

    if !path.exists() {
        return Err(format!("Version {} for {} is not installed", version.cyan(), pack_name.green()));
    }

    // Sanity check
    let path_git = path.join(".git");

    if !path_git.exists() {
        return Err(format!("Pack \"{}\" is not a git repository (maybe your configuration is wrong?)", pack_name));
    }

    std::fs::remove_dir_all(&path).unwrap();

    // Check if it is empty
    if fs::read_dir(&library_path).unwrap().next().is_none() {
        println!(" - No more versions of {} installed, cleaning folder...", pack_name.green());
        std::fs::remove_dir_all(&library_path).unwrap();
    }

    Ok(())
}

/*
    Library index
*/

pub fn update_library_index() -> Result<(), String> {
    const INDEX_URL: &str = "https://github.com/Gabie-of-the-Bo/Ryna-lib-index.git";
    let path = Path::new(&CONFIG.write().unwrap().modules_path).join(".index");

    if path.join(".git").exists() {
        let repo = match Repository::open(path) {
            Ok(r) => r,
            Err(_) => return Err("Unable to open library index repository".into()),
        };

        let mut callbacks = RemoteCallbacks::new();
        callbacks.credentials(|_url, username_from_url, _allowed_types| {
            Cred::ssh_key_from_agent(username_from_url.unwrap())
        });

        let mut fetch_options = FetchOptions::new();
        fetch_options.remote_callbacks(callbacks);

        let mut remote = match repo.find_remote("origin") {
            Ok(r) => r,
            Err(_) => return Err("Unable to find library index remote".into()),
        };
        
        match remote.fetch(&["refs/heads/*:refs/remotes/origin/*"], Some(&mut fetch_options), None) {
            Err(_) => return Err("Unable to fetch library index repository".into()),
            _ => {}
        };

        // Should not fail after this step
        let fetch_head = repo.find_reference("FETCH_HEAD").unwrap();
        let fetch_commit = repo.reference_to_annotated_commit(&fetch_head).unwrap();

        let (analysis, _) = repo.merge_analysis(&[&fetch_commit]).unwrap();
        if analysis.is_fast_forward() {
            let refname = "refs/heads/master";
            let mut reference = repo.find_reference(refname).unwrap();
            reference.set_target(fetch_commit.id(), "Fast-Forward").unwrap();
            repo.set_head(refname).unwrap();
            repo.checkout_head(Some(git2::build::CheckoutBuilder::default().force())).unwrap();
        }

        Ok(())

    } else {
        match RepoBuilder::new().clone(INDEX_URL, &path) {
            Ok(_) => Ok(()),
            Err(_) => {
                Err(format!("Unable to download library index from {}", INDEX_URL))
            },
        }
    }
}

/*
    Standard libraries
*/

pub fn install_prelude() -> Result<(), String> {
    const PRELUDE_URL: &str = "https://github.com/Gabie-of-the-Bo/Ryna-prelude.git";

    let (latest_version, latest_branch) = get_latest_lib_version(PRELUDE_URL)?;

    install_repo(PRELUDE_URL, "prelude", &latest_version, &latest_branch)
}