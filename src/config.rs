use std::collections::{ HashMap, HashSet };
use std::fs;
use std::path::Path;

use colored::Colorize;
use glob::glob;
use md5::compute;
use regex::{Regex, Captures};
use serde::{Serialize, Deserialize};
use serde_yaml::{from_str, to_string};

use crate::compilation::NessaError;
use crate::context::*;
use crate::graph::DirectedGraph;
use crate::parser::*;
use crate::serialization::CompiledNessaModule;

const ENV_VAR_REGEX: &str = r"\$\{\s*([a-zA-Z0-9_]+)\s*\}";

pub const NESSA_MODULES_ENV_VAR: &str = "NESSA_MODULES_PATH"; 

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ModuleInfo {
    pub path: String,
    pub version: String,

    #[serde(skip)]
    pub is_local: bool,

    #[serde(skip)]
    pub dependencies: HashSet<(String, String)>
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct NessaConfig {
    pub module_name: String,

    #[serde(default = "default_version")]
    pub version: String,

    #[serde(skip_serializing_if = "String::is_empty")]
    #[serde(default)]
    pub hash: String,

    pub module_paths: Vec<String>,
    pub modules: HashMap<String, ModuleInfo>
}

fn default_version() -> String {
    "0.1.0".into()
}

pub type Imports = HashMap<ImportType, HashSet<String>>;
pub type ImportMap = HashMap<String, Imports>;
pub type InnerDepGraph = DirectedGraph<(ImportType, usize), ()>;
pub type VersionModCache = HashMap<(String, String), ModuleInfo>;

type FileCache = HashMap<String, (NessaConfig, HashMap<String, HashMap<ImportType, HashSet<String>>>, String)>;

pub struct NessaModule {
    pub name: String,
    pub hash: String,
    pub ctx: NessaContext,
    pub code: Vec<NessaExpr>,
    pub source: Vec<String>, 
    pub imports: ImportMap,
    pub inner_dependencies: InnerDepGraph
}

impl NessaModule {
    pub fn new(
        name: String,
        hash: String,
        ctx: NessaContext,
        code: Vec<NessaExpr>,
        source: Vec<String>, 
        imports: ImportMap,
        inner_dependencies: InnerDepGraph
    ) -> NessaModule {
        NessaModule { name, hash, ctx, code, source, imports, inner_dependencies }
    }
}

impl NessaConfig {
    pub fn get_imports_topological_order(&self, all_modules: &VersionModCache) -> Result<Vec<(String, String)>, NessaError> {
        fn topological_order(node: &(String, String), res: &mut Vec<(String, String)>, temp: &mut HashSet<(String, String)>, perm: &mut HashSet<(String, String)>, all_modules: &VersionModCache) -> Result<(), NessaError> {
            if perm.contains(node) {
                return Ok(());
            }

            if temp.contains(node) {
                return Err(NessaError::module_error("Dependency tree is cyclic".into()));
            }

            temp.insert(node.clone());

            match all_modules.get(node) {
                Some(m) => {
                    for (n, v) in &m.dependencies {
                        topological_order(&(n.clone(), v.clone()), res, temp, perm, all_modules)?;
                    }
                },

                None => {
                    return Err(NessaError::module_error(format!("Module {} {} was not found", node.0.green(), format!("v{}", node.1).cyan())));
                },
            }

            temp.remove(node);
            perm.insert(node.clone());
            res.push(node.clone());

            Ok(())
        }

        let mut res = vec!();
        let mut temp = HashSet::new();
        let mut perm = HashSet::new();

        topological_order(&(self.module_name.clone(), self.version.clone()), &mut res, &mut temp, &mut perm, all_modules)?;

        Ok(res)
    }

    fn is_outdated(&self) -> bool {
        true// TODO: compilation cache
    }
}

fn parse_nessa_module_with_config(path: &String, already_compiled: &mut HashMap<(String, String), NessaModule>, all_modules: &VersionModCache, file_cache: &FileCache) -> Result<NessaModule, NessaError> {
    let (config_yml, imports, main) = file_cache.get(path).unwrap();

    // Refresh configuration and recompile if it is outdated
    if config_yml.is_outdated() {
        let mut ctx = standard_ctx();
        let topological_order = config_yml.get_imports_topological_order(all_modules)?;

        // Forbid multiple versions of same module
        
        let mut module_versions = HashMap::<&String, Vec<_>>::new();

        for (name, ver) in &topological_order {
            module_versions.entry(name).or_default().push(ver);
        }

        for (name, vers) in module_versions {
            if vers.len() > 1 {
                let all_but_last_vers = &vers[..vers.len() - 1];

                return Err(NessaError::module_error(
                    format!(
                        "Multiple versions needed for module {} ({} and {})", 
                        name,
                        all_but_last_vers.iter().map(|i| format!("{}", i.cyan())).collect::<Vec<_>>().join(", "),
                        vers.last().unwrap().cyan()
                    )
                ));
            }
        }

        // Compile modules

        for dep in &topological_order {
            if *dep.0 != config_yml.module_name && !already_compiled.contains_key(dep) {
                let module = all_modules.get(dep).unwrap();
                let compiled_module = parse_nessa_module_with_config(&module.path, already_compiled, all_modules, file_cache)?;

                already_compiled.entry(dep.clone()).or_insert(compiled_module);
            }
        }

        // Select modules to send to compilation routine
        
        let module_dependencies = topological_order.iter()
                                                   .filter(|i| i.0 != config_yml.module_name)
                                                   .map(|i| (i.0.clone(), i.1.clone()))
                                                   .map(|i| (i.0.clone(), already_compiled.get(&i).unwrap()))
                                                   .collect();

        let (module, source) = ctx.parse_with_dependencies(&config_yml.module_name, main, &module_dependencies)?;
        let graph = ctx.get_inner_dep_graph(&module)?;
    
        Ok(NessaModule::new(config_yml.module_name.clone(), config_yml.hash.clone(), ctx, module, source, imports.clone(), graph))

    } else {
        unimplemented!();
    }
}

pub fn get_all_modules_cascade_aux(module_path: &Path, seen_paths: &mut HashSet<String>, modules: &mut VersionModCache, file_cache: &mut FileCache) -> Result<(), NessaError> {
    let main_path = module_path.join(Path::new("main.nessa"));

    if !main_path.exists() {
        return Err(NessaError::module_error(format!("Main file ({}) does not exist", main_path.to_str().unwrap())));
    }

    let config_path = module_path.join(Path::new("nessa_config.yml"));

    if !config_path.exists() {
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

    let norm_mod_path = normalize_path(module_path)?;

    for local_file in glob(format!("{}/**/*.nessa", module_path.to_str().unwrap()).as_str()).expect("Error while reading module path") {
        let path = local_file.unwrap();
        let full_import_path = normalize_path(&path)?;
        let import_name = full_import_path[norm_mod_path.len()..full_import_path.len() - 6].replace('\\', "/");

        if import_name != "/main" {
            config_yml.modules.entry(import_name.clone()).or_insert(ModuleInfo {
                path: full_import_path.clone(),
                version: config_yml.version.clone(),
                is_local: true,
                dependencies: HashSet::new(),
            });
        }
    }

    let all_deps = config_yml.modules.iter().map(|i| (i.0.clone(), i.1.version.clone())).collect::<HashMap<_, _>>();
    let mut local_imports = HashMap::new();

    for (module_name, info) in config_yml.modules.iter_mut() {
        if info.is_local {
            let local_main = fs::read_to_string(&info.path).expect("Error while reading main file");
            let local_file_imports = nessa_module_imports_parser(Span::new(&local_main)).unwrap().1;

            local_imports.entry(module_name.clone()).or_insert((local_file_imports.clone(), local_main));

            for module in local_file_imports.keys() {
                if !all_deps.contains_key(module) {
                    return Err(NessaError::module_error(format!("Module with name {} was not found", module.green())));
                }
            }

            info.dependencies = local_file_imports.keys()
                                                  .map(|i| (i.clone(), all_deps.get(i).unwrap().clone()))
                                                  .collect();

            modules.entry((module_name.clone(), config_yml.version.clone())).or_insert(info.clone());
        }
    }

    for (module_name, info) in config_yml.modules.iter() {
        if info.is_local {
            let mut local_yml = config_yml.clone();
            local_yml.module_name = module_name.clone();

            let (local_imports, local_main) = local_imports.get(module_name).unwrap().clone();

            file_cache.insert(info.path.clone(), (local_yml, local_imports, local_main));
        }
    }

    file_cache.insert(normalize_path(module_path)?, (config_yml.clone(), imports.clone(), main.clone()));

    for module in imports.keys() {
        if !config_yml.modules.contains_key(module) {
            return Err(NessaError::module_error(format!("Module with name {} was not found", module.green())));
        }
    }

    modules.entry((config_yml.module_name, config_yml.version.clone())).or_insert(ModuleInfo { 
        path: normalize_path(module_path)?, 
        version: config_yml.version, 
        is_local: false,
        dependencies: config_yml.modules.into_iter().map(|i| (i.0, i.1.version)).filter(|(i, _)| imports.contains_key(i)).collect()
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

    Ok(())
}

pub fn get_all_modules_cascade(module_path: &Path) -> Result<(VersionModCache, FileCache), NessaError> {
    let mut res = HashMap::new();
    let mut file_cache = HashMap::new();

    get_all_modules_cascade_aux(module_path, &mut HashSet::new(), &mut res, &mut file_cache)?;

    Ok((res, file_cache))
}

pub fn precompile_nessa_module_with_config(path: &String, all_modules: VersionModCache, file_cache: FileCache) -> Result<(NessaContext, Vec<NessaExpr>), NessaError> {
    let mut module = parse_nessa_module_with_config(&normalize_path(Path::new(path))?, &mut HashMap::new(), &all_modules, &file_cache)?;

    module.ctx.precompile_module(&mut module.code)?;

    Ok((module.ctx, module.code))
}

pub fn compute_project_hash(path: &String) -> Result<(String, VersionModCache, FileCache), NessaError> {
    let module_path = Path::new(path);
    let (all_modules, file_cache) = get_all_modules_cascade(module_path)?;

    let config_yml = &file_cache.get(&normalize_path(module_path)?).unwrap().0;

    let mut final_hash = config_yml.hash.clone();

    let mut sorted_modules = config_yml.modules.values().collect::<Vec<_>>();
    sorted_modules.sort_by_key(|i| &i.path); // This should be unique and allow the same order every time

    // Add the hashes of all submodules
    for info in sorted_modules {
        final_hash = format!("{}{}", final_hash, file_cache.get(&normalize_path(Path::new(&info.path))?).unwrap().0.hash);
    }

    Ok((format!("{:x}", compute(&final_hash)), all_modules, file_cache))
}

pub fn read_compiled_cache(path: &String) -> Option<CompiledNessaModule> {
    let module_path = Path::new(path);
    let cache_path = module_path.join(Path::new("nessa_cache"));

    if !cache_path.exists() {
        return None;
    }

    let code_path = cache_path.join(Path::new("main.nessac"));

    if !code_path.exists() {
        return None;
    }

    let code = CompiledNessaModule::from_file(&code_path);

    Some(code)
}

pub fn save_compiled_cache(path: &String, module: &CompiledNessaModule) -> Result<(), NessaError> {
    let module_path = Path::new(path);
    let cache_path = module_path.join(Path::new("nessa_cache"));

    if !cache_path.exists() {
        fs::create_dir(&cache_path).expect("Unable to create cache directory");
    }

    let code_path = cache_path.join(Path::new("main.nessac"));
    module.write_to_file(&code_path);

    Ok(())
}

// Taken from regex's docs
fn replace_all_fallible<E>(
    re: &Regex,
    haystack: &str,
    replacement: impl Fn(&Captures) -> Result<String, E>,
) -> Result<String, E> {
    let mut new = String::with_capacity(haystack.len());
    let mut last_match = 0;
    for caps in re.captures_iter(haystack) {
        let m = caps.get(0).unwrap();
        new.push_str(&haystack[last_match..m.start()]);
        new.push_str(&replacement(&caps)?);
        last_match = m.end();
    }
    new.push_str(&haystack[last_match..]);
    Ok(new)
}

pub fn normalize_path(path: &Path) -> Result<String, NessaError> {
    let path_slashes = path.to_str().unwrap().replace('\\', "/");
    let sub_path = parse_env_vars_and_normalize(&path_slashes)?;
    
    return match Path::new(&sub_path).canonicalize() {
        Ok(p) => Ok(p.to_str().unwrap().into()),
        Err(_) => Err(NessaError::module_error(format!(
            "Unable to normalize path: {} (does it exist?)",
            path_slashes.green()
        ))),
    };
}

pub fn parse_env_vars_and_normalize(path: &str) -> Result<String, NessaError> {
    let res = path.to_owned();
    let env_var_regex = Regex::new(ENV_VAR_REGEX).unwrap();

    let replacement = |caps: &Captures| {
        let cap = caps.get(1).unwrap().as_str();
        
        if let Ok(var) = std::env::var(cap) {
            Ok(var)
 
        } else {
            Err(NessaError::module_error(format!("Unable to find {} environment variable", cap)))
        }
    };
    
    return replace_all_fallible(&env_var_regex, res.as_str(), replacement);
}

pub fn get_nessa_modules_var() -> Option<String> {
    if let Ok(var) = std::env::var(NESSA_MODULES_ENV_VAR) {
        Some(var)

    } else {
        None
    }
}