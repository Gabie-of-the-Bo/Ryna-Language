use colored::Colorize;
use derive_builder::Builder;
use rustc_hash::FxHashSet;

use crate::{context::NessaContext, operations::Operator, parser::NessaExpr, types::Type};

// Instruction-level debug information

#[derive(Clone, Debug, Default, PartialEq, Builder)]
pub struct DebugInfo {
    #[builder(default)]
    pub comment: String,
    
    #[builder(default)]
    pub var_type: Option<Type>,
    
    #[builder(default)]
    pub labels: FxHashSet<usize>,
    
    #[builder(default)]
    pub functions: FxHashSet<String>,
    
    #[builder(default)]
    pub lines: FxHashSet<usize>
}

impl DebugInfo {
    pub fn merge_with(&mut self, other: &DebugInfo) {
        self.functions.extend(other.functions.iter().cloned());
        self.lines.extend(&other.lines);
        self.labels.extend(&other.labels);

        if self.comment.is_empty() && !other.comment.is_empty() {
            self.comment = other.comment.clone();
        }
    }

    pub fn set_line(&mut self, line: usize) {
        self.lines.insert(line);
    }
}

// Expression printing

impl NessaContext {
    pub fn to_string(&self, expr: &NessaExpr) -> String  {
        match expr {
            NessaExpr::NameReference(_, n) |
            NessaExpr::Variable(_, _, n, _) => n.clone().cyan().to_string(),
            NessaExpr::CompiledVariableDefinition(_, _, n, t, e) => format!("let {}: {} = {}", n.cyan(), t.get_name(self), self.to_string(e)),
            NessaExpr::CompiledVariableAssignment(_, _, n, _, e) => format!("{} = {}", n.cyan(), self.to_string(e)),

            NessaExpr::Literal(_, obj) => obj.to_debug_string().magenta().to_string(),
            NessaExpr::Tuple(_, args) => format!("({})", args.iter().map(|i| self.to_string(i)).collect::<Vec<_>>().join(", ")),

            NessaExpr::FunctionCall(_, id, t, args) => {
                let temp = if t.is_empty() {
                    String::new()

                } else {
                    format!("<{}>", t.iter().map(|t| t.get_name(self)).collect::<Vec<_>>().join(", "))
                };

                format!(
                    "{}{}({})", 
                    self.functions[*id].name.green(), 
                    temp,
                    args.iter().map(|i| self.to_string(i)).collect::<Vec<_>>().join(", ")
                )
            },

            NessaExpr::UnaryOperation(_, id, t, expr) => {
                let temp = if t.is_empty() {
                    String::new()

                } else {
                    format!("<{}>", t.iter().map(|t| t.get_name(self)).collect::<Vec<_>>().join(", "))
                };

                if let Operator::Unary { representation, prefix, .. } = &self.unary_ops[*id] {
                    if *prefix {
                        return format!("{}{}{}", representation, temp, self.to_string(expr))

                    } else {
                        return format!("{}{}{}", self.to_string(expr), temp, representation)
                    }
                }

                unreachable!()
            },

            NessaExpr::BinaryOperation(_, id, t, a, b) => {
                let temp = if t.is_empty() {
                    String::new()

                } else {
                    format!("<{}>", t.iter().map(|t| t.get_name(self)).collect::<Vec<_>>().join(", "))
                };

                format!("{} {}{} {}", self.to_string(a), temp, self.binary_ops[*id].get_repr(), self.to_string(b))
            },

            NessaExpr::NaryOperation(_, id, t, a, b) => {
                let temp = if t.is_empty() {
                    String::new()

                } else {
                    format!("<{}>", t.iter().map(|t| t.get_name(self)).collect::<Vec<_>>().join(", "))
                };

                if let Operator::Nary { open_rep, close_rep, .. } = &self.nary_ops[*id] {
                    return format!("{}{}{}{}{}", self.to_string(a), temp, open_rep, b.iter().map(|i| self.to_string(i)).collect::<Vec<_>>().join(", "), close_rep)
                }

                unreachable!()
            },

            NessaExpr::Return(_, expr) => format!("return {}", self.to_string(expr)),

            _ => todo!("Unable to convert {:?} to pretty string", expr)
        }
    }
}