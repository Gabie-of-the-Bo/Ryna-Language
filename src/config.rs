use std::collections::{ HashMap, HashSet };
use std::fs;
use std::path::Path;

use colored::Colorize;
use glob::glob;
use md5::compute;
use serde::{Serialize, Deserialize};
use serde_yaml::{from_str, to_string};

use crate::compilation::NessaError;
use crate::context::*;
use crate::graph::DirectedGraph;
use crate::parser::*;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ModuleInfo {
    path: String,
    version: String,

    #[serde(skip)]
    dependencies: HashSet<String>
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct NessaConfig {
    module_name: String,

    #[serde(default = "default_version")]
    version: String,

    #[serde(default)]
    hash: String,

    module_paths: Vec<String>,
    modules: HashMap<String, ModuleInfo>
}

fn default_version() -> String {
    return "1.0".into();
}

pub type Imports = HashMap<ImportType, HashSet<String>>;
pub type ImportMap = HashMap<String, Imports>;
pub type InnerDepGraph = DirectedGraph<(ImportType, usize), ()>;

type FileCache = HashMap<String, (NessaConfig, HashMap<String, HashMap<ImportType, HashSet<String>>>, String)>;

pub struct NessaModule {
    pub name: String,
    pub ctx: NessaContext,
    pub code: Vec<NessaExpr>,
    pub source: Vec<String>, 
    pub imports: ImportMap,
    pub inner_dependencies: InnerDepGraph
}

impl NessaModule {
    pub fn new(
        name: String,
        ctx: NessaContext,
        code: Vec<NessaExpr>,
        source: Vec<String>, 
        imports: ImportMap,
        inner_dependencies: InnerDepGraph
    ) -> NessaModule {
        return NessaModule { name, ctx, code, source, imports, inner_dependencies };
    }
}

impl NessaConfig {
    pub fn get_imports_topological_order(&self, all_modules: &HashMap<String, ModuleInfo>) -> Result<Vec<String>, NessaError> {
        fn topological_order(config: &NessaConfig, node: &String, res: &mut Vec<String>, temp: &mut HashSet<String>, perm: &mut HashSet<String>, all_modules: &HashMap<String, ModuleInfo>) -> Result<(), NessaError> {
            if perm.contains(node) {
                return Ok(());
            }

            if temp.contains(node) {
                return Err(NessaError::module_error("Dependency tree is cyclic".into()));
            }

            temp.insert(node.clone());

            match all_modules.get(node) {
                Some(m) => {
                    for n in &m.dependencies {
                        topological_order(config, n, res, temp, perm, all_modules)?;
                    }
                },

                None => {
                    return Err(NessaError::module_error(format!("Module with name {} was not found", node.green())));
                },
            }

            temp.remove(node);
            perm.insert(node.clone());
            res.push(node.clone());

            return Ok(());
        }

        let mut res = vec!();
        let mut temp = HashSet::new();
        let mut perm = HashSet::new();

        topological_order(self, &self.module_name, &mut res, &mut temp, &mut perm, all_modules)?;

        return Ok(res);
    }

    fn is_outdated(&self) -> bool {
        return true; // TODO: compilation cache
    }
}

fn parse_nessa_module_with_config<'a>(path: &String, already_compiled: &mut HashMap<String, NessaModule>, all_modules: &HashMap<String, ModuleInfo>, file_cache: &FileCache) -> Result<NessaModule, NessaError> {
    let (config_yml, imports, main) = file_cache.get(path).unwrap();

    // Refresh configuration and recompile if it is outdated
    if config_yml.is_outdated() {
        let mut ctx = standard_ctx();
        let topological_order = config_yml.get_imports_topological_order(all_modules)?;
    
        for dep in &topological_order {
            if *dep != config_yml.module_name && !already_compiled.contains_key(dep) {
                let module = all_modules.get(dep).unwrap();
                let compiled_module = parse_nessa_module_with_config(&module.path, already_compiled, all_modules, file_cache)?;

                already_compiled.entry(dep.clone()).or_insert(compiled_module);
            }
        }
    
        let (module, source) = ctx.parse_with_dependencies(&config_yml.module_name, &main, &already_compiled)?;
        let graph = ctx.get_inner_dep_graph(&module)?;
    
        return Ok(NessaModule::new(config_yml.module_name.clone(), ctx, module, source, imports.clone(), graph));

    } else {
        unimplemented!();
    }
}

pub fn get_all_modules_cascade_aux(module_path: &Path, seen_paths: &mut HashSet<String>, modules: &mut HashMap<String, ModuleInfo>, file_cache: &mut FileCache) -> Result<(), NessaError> {
    let main_path = module_path.join(Path::new("main.nessa"));

    if !main_path.is_file() {
        return Err(NessaError::module_error(format!("Main file ({}) does not exist", main_path.to_str().unwrap())));
    }

    let config_path = module_path.join(Path::new("nessa_config.yml"));

    if !config_path.is_file() {
        return Err(NessaError::module_error(format!("Config file ({}) does not exist", config_path.to_str().unwrap())));
    }

    let config = fs::read_to_string(&config_path).expect("Error while reading config file");
    let main = fs::read_to_string(&main_path).expect("Error while reading main file");

    let mut config_yml: NessaConfig = from_str(&config).expect("Unable to parse configuration file");
    let imports = nessa_module_imports_parser(Span::new(&main)).unwrap().1;
    let new_hash = format!("{:x}", compute(&main));

    if config_yml.hash != new_hash {
        config_yml.hash = new_hash;
        fs::write(config_path, to_string(&config_yml).unwrap()).expect("Unable to update configuration file");
    }

    file_cache.insert(module_path.to_str().unwrap().into(), (config_yml.clone(), imports.clone(), main.clone()));

    modules.entry(config_yml.module_name).or_insert(ModuleInfo { 
        path: module_path.to_str().unwrap().into(), 
        version: config_yml.version, 
        dependencies: imports.into_keys().collect()
    });

    for path in config_yml.module_paths {
        if !seen_paths.contains(&path) {
            seen_paths.insert(path.clone());

            for file in glob(format!("{}/*/nessa_config.yml", path).as_str()).expect("Error while reading module path") {
                match file {
                    Ok(f) if f.is_file() => {
                        get_all_modules_cascade_aux(f.parent().unwrap(), seen_paths, modules, file_cache)?;
                    },
    
                    _ => {
                        return Err(NessaError::module_error("Unable to extract file from module path".into()));
                    }
                }
            }
        }
    }

    return Ok(());
}

pub fn get_all_modules_cascade(module_path: &Path) -> Result<(HashMap<String, ModuleInfo>, FileCache), NessaError> {
    let mut res = HashMap::new();
    let mut file_cache = HashMap::new();

    get_all_modules_cascade_aux(module_path, &mut HashSet::new(), &mut res, &mut file_cache)?;

    return Ok((res, file_cache));
}

pub fn precompile_nessa_module_with_config(path: &String) -> Result<(NessaContext, Vec<NessaExpr>), NessaError> {
    let (all_modules, file_cache) = get_all_modules_cascade(Path::new(&path)).unwrap();
    let mut module = parse_nessa_module_with_config(path, &mut HashMap::new(), &all_modules, &file_cache)?;

    module.ctx.precompile_module(&mut module.code)?;

    return Ok((module.ctx, module.code));
}