use std::{fs, path::Path};

use crate::{cache::NessaCache, compilation::{CompiledNessaExpr, NessaError, NessaInstruction}, config::{ImportMap, InnerDepGraph, NessaModule}, context::{standard_ctx, NessaContext, NUM_STD_BINOPS, NUM_STD_FNS, NUM_STD_INTS, NUM_STD_INT_IMPL, NUM_STD_MACROS, NUM_STD_NARYOPS, NUM_STD_TYPES, NUM_STD_UNOPS}, execution::ExecutionInfo, functions::Function, interfaces::{Interface, InterfaceImpl}, macros::NessaMacro, operations::Operator, parser::NessaExpr, types::TypeTemplate};

use serde::{Serialize, Deserialize};
use bitcode;

#[derive(Clone, Serialize, Deserialize)]
pub struct ReducedNessaModule {
    pub name: String,
    pub hash: String,

    pub code: Vec<NessaExpr>,
    pub source: Vec<String>,
    pub imports: ImportMap,
    pub inner_dependencies: InnerDepGraph,

    pub type_templates: Vec<TypeTemplate>, 
    pub interfaces: Vec<Interface>,
    pub interface_impls: Vec<InterfaceImpl>,

    pub unary_ops: Vec<Operator>,
    pub binary_ops: Vec<Operator>,
    pub nary_ops: Vec<Operator>,

    pub functions: Vec<Function>,

    pub macros: Vec<NessaMacro>,

    pub cache: NessaCache
}

impl NessaModule {
    pub fn get_reduced_module(&self) -> ReducedNessaModule {
        let reduced_types = self.ctx.type_templates[*NUM_STD_TYPES.lock().unwrap().borrow()..].to_vec();
        let reduced_ints = self.ctx.interfaces[*NUM_STD_INTS.lock().unwrap().borrow()..].to_vec();
        let reduced_int_impls = self.ctx.interface_impls[*NUM_STD_INT_IMPL.lock().unwrap().borrow()..].to_vec();
        let reduced_macros = self.ctx.macros[*NUM_STD_MACROS.lock().unwrap().borrow()..].to_vec();

        let func_map = NUM_STD_FNS.lock().unwrap();
        let unop_map = NUM_STD_UNOPS.lock().unwrap();
        let binop_map = NUM_STD_BINOPS.lock().unwrap();
        let naryop_map = NUM_STD_NARYOPS.lock().unwrap();

        let reduced_functions = self.ctx.functions.iter().map(|f| {
            let mut f_cpy = f.clone();
            
            if let Some(inner) = func_map.borrow().get(&f_cpy.id) {
                f_cpy.overloads = f_cpy.overloads[*inner..].to_vec();
            }

            f_cpy
        })
        .collect();

        let reduced_unops = self.ctx.unary_ops.iter().filter_map(|o| {
            let mut o_cpy = o.clone();
            
            if let Operator::Unary { id, operations, .. } = &mut o_cpy {
                if let Some(inner) = unop_map.borrow().get(id) {
                    *operations = operations[*inner..].to_vec();
                }

                return Some(o_cpy);
            }

            unreachable!();
        }).collect();

        let reduced_binops = self.ctx.binary_ops.iter().filter_map(|o| {
            let mut o_cpy = o.clone();
            
            if let Operator::Binary { id, operations, .. } = &mut o_cpy {
                if let Some(inner) = binop_map.borrow().get(id) {
                    *operations = operations[*inner..].to_vec();
                }

                return Some(o_cpy);
            }

            unreachable!();
        }).collect();

        let reduced_naryops = self.ctx.nary_ops.iter().filter_map(|o| {
            let mut o_cpy = o.clone();
            
            if let Operator::Nary { id, operations, .. } = &mut o_cpy {
                if let Some(inner) = naryop_map.borrow().get(id) {
                    *operations = operations[*inner..].to_vec();
                }

                return Some(o_cpy);
            }

            unreachable!();
        }).collect();

        ReducedNessaModule {
            name: self.name.clone(),
            hash: self.hash.clone(),
            code: self.code.clone(), 
            source: self.source.clone(),
            imports: self.imports.clone(),
            inner_dependencies: self.inner_dependencies.clone(),
            type_templates: reduced_types,
            interfaces: reduced_ints,
            interface_impls: reduced_int_impls,
            unary_ops: reduced_unops,
            binary_ops: reduced_binops,
            nary_ops: reduced_naryops,
            functions: reduced_functions,
            macros: reduced_macros,
            cache: self.ctx.cache.clone(),
        }
    }
}

impl ReducedNessaModule {
    pub fn recover_module(mut self) -> NessaModule {
        let mut std_ctx = standard_ctx();

        // Reconstruct original context
        std_ctx.cache = self.cache;

        std_ctx.type_templates.append(&mut self.type_templates);
        std_ctx.interfaces.append(&mut self.interfaces);
        std_ctx.interface_impls.append(&mut self.interface_impls);
        std_ctx.macros.append(&mut self.macros);

        for mut f in self.functions {
            if f.id < std_ctx.functions.len() {
                std_ctx.functions[f.id].overloads.append(&mut f.overloads);

            } else {
                std_ctx.functions.push(f);
            }
        }

        for mut o in self.unary_ops {
            if let Operator::Unary { id, operations, .. } = &mut o {
                if *id < std_ctx.unary_ops.len() {
                    if let Operator::Unary { operations: operations_std, .. } = &mut std_ctx.unary_ops[*id] {
                        operations_std.append(operations);
                    }
    
                } else {
                    std_ctx.unary_ops.push(o);
                }

            } else {
                unreachable!();
            }
        }

        for mut o in self.binary_ops {
            if let Operator::Binary { id, operations, .. } = &mut o {
                if *id < std_ctx.binary_ops.len() {
                    if let Operator::Binary { operations: operations_std, .. } = &mut std_ctx.binary_ops[*id] {
                        operations_std.append(operations);
                    }
    
                } else {
                    std_ctx.binary_ops.push(o);
                }

            } else {
                unreachable!();
            }
        }

        for mut o in self.nary_ops {
            if let Operator::Nary { id, operations, .. } = &mut o {
                if *id < std_ctx.nary_ops.len() {
                    if let Operator::Nary { operations: operations_std, .. } = &mut std_ctx.nary_ops[*id] {
                        operations_std.append(operations);
                    }
    
                } else {
                    std_ctx.nary_ops.push(o);
                }

            } else {
                unreachable!();
            }
        }

        NessaModule {
            name: self.name.clone(),
            hash: self.hash.clone(),
            ctx: std_ctx,
            code: self.code,
            source: self.source,
            imports: self.imports,
            inner_dependencies: self.inner_dependencies,
        }
    }

    pub fn deserialize(data: &[u8]) -> Self {
        bitcode::deserialize(data).expect("Unable to deserialize module")
    }

    pub fn serialize(&self) -> Vec<u8> {
        bitcode::serialize(self).expect("Unable to serialize module")
    }

    pub fn from_file(path: &Path) -> Self {
        let data = fs::read(path).expect("Unable to read serialized module from file");
        ReducedNessaModule::deserialize(&data)
    }

    pub fn write_to_file(&self, path: &Path) {
        fs::write(path, self.serialize()).expect("Unable to write serialized module to file");
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct CompiledNessaModule {
    pub hash: String,
    type_templates: Vec<TypeTemplate>,
    interface_impls: Vec<InterfaceImpl>,
    instructions: Vec<CompiledNessaExpr>
}

impl NessaContext {
    pub fn get_serializable_module(&self, hash: String, instructions: &[NessaInstruction]) -> CompiledNessaModule {
        return CompiledNessaModule {
            hash, 
            type_templates: self.type_templates[*NUM_STD_TYPES.lock().unwrap().borrow()..].to_vec(), 
            interface_impls: self.interface_impls[*NUM_STD_INT_IMPL.lock().unwrap().borrow()..].to_vec(), 
            instructions: instructions.iter().map(|i| i.instruction.clone()).collect()
        };
    }
}

impl CompiledNessaModule {
    pub fn deserialize(data: &[u8]) -> Self {
        bitcode::deserialize(data).expect("Unable to deserialize code")
    }

    pub fn serialize(&self) -> Vec<u8> {
        bitcode::serialize(self).expect("Unable to serialize code")
    }

    pub fn from_file(path: &Path) -> Self {
        let data = fs::read(path).expect("Unable to read serialized code from file");
        CompiledNessaModule::deserialize(&data)
    }

    pub fn write_to_file(&self, path: &Path) {
        fs::write(path, self.serialize()).expect("Unable to write serialized code to file");
    }

    pub fn execute<const DEBUG: bool>(&mut self, program_input: &[String]) -> Result<ExecutionInfo, NessaError> {
        let mut ctx = standard_ctx();

        ctx.type_templates.append(&mut self.type_templates);
        ctx.interface_impls.append(&mut self.interface_impls);

        ctx.program_input = program_input.to_vec();

        ctx.execute_compiled_code::<DEBUG>(&self.instructions, &[])
    }
}