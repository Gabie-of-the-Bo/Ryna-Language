use std::collections::{ HashMap, HashSet };
use std::fs;
use std::path::Path;
use std::str::FromStr;

use glob::glob;
use md5::compute;
use serde::{Serialize, Deserialize};
use serde_yaml::from_str;

use crate::context::*;
use crate::parser::*;

#[derive(Debug, Serialize, Deserialize)]
pub struct ModuleInfo {
    path: String,
    version: String,
    hash: String,
    dependencies: HashSet<String>
}

#[derive(Debug, Serialize, Deserialize)]
pub struct NessaConfig {
    module_name: String,
    module_paths: Vec<String>,
    modules: HashMap<String, ModuleInfo>
}

fn get_nessa_files(module_paths: &Vec<String>, curr_module_path: &String) -> Result<HashMap<String, ModuleInfo>, String> {
    let mut res = HashMap::new();

    let mut analyze_path = |f: std::path::PathBuf| {
        let path_str = f.display().to_string();

        let file = fs::read_to_string(&path_str).unwrap();
        let module_descriptors = nessa_module_header_parser(&file).unwrap().1;

        if module_descriptors.len() != 1 {
            return Err(format!("Invalid number of module descriptors in {} (found {}, expected 1)", &path_str, module_descriptors.len()));
        }

        let imports = nessa_module_imports_parser(&file).unwrap().1;

        res.entry(module_descriptors[0].0.clone()).or_insert(ModuleInfo {
            path: f.parent().unwrap().display().to_string(),
            version: module_descriptors[0].1.clone(),
            hash: format!("{:x}", compute(file)),
            dependencies: imports.into_iter().map(|(k, _)| k).collect()
        });

        return Ok(());
    };

    let curr_module_main = format!("{}/main.nessa", curr_module_path);

    analyze_path(std::path::PathBuf::from_str(&curr_module_main).expect("Unable to parse current module path"))?;

    for path in module_paths.iter() {
        for file in glob(format!("{}/*/main.nessa", path).as_str()).expect("Error while reading module path") {
            match file {
                Ok(f) => {
                    analyze_path(f)?;
                },

                Err(_) => {
                    return Err("Unable to extract file from module path".into())
                }
            }
        }
    }

    return Ok(res);
}

impl NessaConfig {
    pub fn new(name: String, module_paths: Vec<String>, curr_module_path: &String) -> Result<NessaConfig, String> {
        let modules = get_nessa_files(&module_paths, curr_module_path)?;

        let defined_modules = modules.iter().map(|(n, _)| n).collect::<HashSet<_>>();
        let referenced_modules = modules.iter().flat_map(|(_, m)| &m.dependencies).collect::<HashSet<_>>();

        for i in referenced_modules.difference(&defined_modules) {
            return Err(format!("Unable to find module with name \"{}\"", i));
        }

        return Ok(NessaConfig {
            module_name: name,
            module_paths: module_paths,
            modules: modules
        });
    }

    pub fn get_imports_topological_order(&self) -> Result<Vec<String>, String> {
        fn topological_order(config: &NessaConfig, node: &String, res: &mut Vec<String>, temp: &mut HashSet<String>, perm: &mut HashSet<String>) -> Result<(), String> {
            if perm.contains(node) {
                return Ok(());
            }

            if temp.contains(node) {
                return Err("Dependency tree is cyclic".into());
            }

            temp.insert(node.clone());

            println!("Fetching {}", node);
            for n in &config.modules.get(node).unwrap().dependencies {
                topological_order(config, n, res, temp, perm)?;
            }

            temp.remove(node);
            perm.insert(node.clone());
            res.push(node.clone());

            return Ok(());
        }

        let mut res = vec!();
        let mut temp = HashSet::new();
        let mut perm = HashSet::new();

        topological_order(self, &self.module_name, &mut res, &mut temp, &mut perm)?;

        return Ok(res);
    }

    fn is_outdated(&self) -> bool {
        return true; // TODO
    }
}

fn parse_nessa_module_with_config_aux(path: &String, already_compiled: &mut HashMap<String, (NessaContext, Vec<NessaExpr>)>) -> Result<(NessaContext, Vec<NessaExpr>), String> {
    let config_path = format!("{}/nessa_config.yml", &path);
    let main_path = format!("{}/main.nessa", &path);

    if !Path::new(&config_path).is_file() {
        return Err(format!("Configuration file ({}) does not exist", &config_path));
    }

    if !Path::new(&main_path).is_file() {
        return Err(format!("Main file ({}) does not exist", &main_path));
    }

    let config = fs::read_to_string(&config_path).expect("Error while reading config file");
    let main = fs::read_to_string(&main_path).expect("Error while reading main file");

    let mut config_yml: NessaConfig = from_str(&config).expect("Unable to parse configuration file");

    // Refresh configuration and recompile if it is outdated
    if config_yml.is_outdated() {
        // Refresh configuration file
        config_yml = NessaConfig::new(config_yml.module_name, config_yml.module_paths, path)?;

        let mut ctx = standard_ctx();
        println!("{:?}", config_yml.modules.iter().map(|(i, _)| i).cloned().collect::<Vec<_>>());
        let topological_order = config_yml.get_imports_topological_order()?;
    
        for dep in &topological_order {
            if *dep != config_yml.module_name && !already_compiled.contains_key(dep) {
                let module = config_yml.modules.get(dep).unwrap();
                let compiled_module = parse_nessa_module_with_config_aux(&module.path, already_compiled)?;
    
                already_compiled.entry(dep.clone()).or_insert(compiled_module);
            }
        }
    
        let module = ctx.parse_and_precompile_with_dependencies(&main, &already_compiled)?;
    
        return Ok((ctx, module));

    } else {
        unimplemented!();
    }
}


pub fn precompile_nessa_module_with_config(path: &String) -> Result<(NessaContext, Vec<NessaExpr>), String> {
    let (mut ctx, mut lines) = parse_nessa_module_with_config_aux(path, &mut HashMap::new())?;

    ctx.precompile_module(&mut lines)?;

    println!("\n\n{}\n\n", path);

    for i in &lines {
        println!("{:?}", i);
    }

    return Ok((ctx, lines));
}