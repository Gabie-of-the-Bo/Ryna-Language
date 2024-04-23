use std::fs::OpenOptions;
use std::{fs::File, path::Path};
use std::io::Write;

use crate::interfaces::Interface;
use crate::operations::Operator;
use crate::types::TypeTemplate;
use crate::{annotations::Annotation, config::NessaModule, html_ext::HTMLColorable, types::Type};

pub fn default_markdown_style() -> String {
    [
        "<style> ",
        "span { font-family: monospace; } ",
        "h2 { background: rgb(30,30,30); padding: 0.15em; border-radius: 0.25em; color: rgb(212,212,212); line-height: 1em; } ",
        "</style>\n\n"
    ].join("")
}

pub fn create_markdown_file(base: &String, name: &str) -> File {
    let docs_path = Path::new(base).join("docs");

    if !docs_path.is_dir() {
        std::fs::create_dir(&docs_path).expect("Unable to create docs folder");
    }

    let file_path = docs_path.join(name);

    if file_path.is_file() {
        std::fs::remove_file(&file_path).expect("Unable to remove docs file");
    }

    let mut file = OpenOptions::new()
        .write(true)
        .append(true)
        .create(true)
        .open(docs_path.join(name))
        .unwrap();

    write!(file, "{}", default_markdown_style()).expect("Error while writing to docs file");

    file
}

pub fn write_args_and_ret(file: &mut File, annot: &Annotation) {
    write!(
        file,
        "### Parameters\n\n",
    ).expect("Error while writing to docs file");

    for arg in &annot.args {
        if arg.0.parse::<usize>().is_err() {
            write!(
                file, 
                "* `{}`: {}\n", arg.0, arg.1
            ).expect("Error while writing to docs file");
        }
    }

    write!(
        file,
        "\n### Description\n{}\n\n",
        annot.args.get("0").unwrap()
    ).expect("Error while writing to docs file");

    write!(
        file,
        "### Return\n{}\n\n",
        annot.args.get("1").unwrap()
    ).expect("Error while writing to docs file");
}

pub fn write_function_overload_docs(file: &mut File, module: &NessaModule, f: &str, t: usize, args: &Type, ret: &Type, annot: &Annotation) {
    write!(
        file, 
        "## {} {}{}{} -> {}\n\n", 
        "fn".html_magenta(),
        f.html_yellow(), if t > 0 { 
            format!("&lt;{}&gt;", (0..t).into_iter()
                                        .map(|i| format!("T_{}", i).html_blue())
                                        .collect::<Vec<_>>()
                                        .join(", ")
            ) 
        } else { "".into() },
        args.get_name_html(&module.ctx), ret.get_name_html(&module.ctx)
    ).expect("Error while writing to docs file");

    write_args_and_ret(file, annot);
}

pub fn write_unary_operation_docs(file: &mut File, module: &NessaModule, op: &str, t: usize, args: &Type, ret: &Type, annot: &Annotation, prefix: bool) {
    if prefix {
        write!(
            file, 
            "## {} {}{}({}) -> {}\n\n", 
            "op".html_magenta(),
            op.html_yellow(), if t > 0 { 
                format!("&lt;{}&gt;", (0..t).into_iter()
                                            .map(|i| format!("T_{}", i).html_blue())
                                            .collect::<Vec<_>>()
                                            .join(", ")
                ) 
            } else { "".into() },
            args.get_name_html(&module.ctx), ret.get_name_html(&module.ctx)
        ).expect("Error while writing to docs file");

    } else {
        write!(
            file, 
            "## {} ({}){}{} -> {}\n\n", 
            "op".html_magenta(),
            args.get_name_html(&module.ctx),
            if t > 0 { 
                format!("&lt;{}&gt;", (0..t).into_iter()
                                            .map(|i| format!("T_{}", i).html_blue())
                                            .collect::<Vec<_>>()
                                            .join(", ")
                ) 
            } else { "".into() },
            op.html_yellow(), ret.get_name_html(&module.ctx)
        ).expect("Error while writing to docs file");    
    }

    write_args_and_ret(file, annot);
}

pub fn write_binary_operation_docs(file: &mut File, module: &NessaModule, op: &str, t: usize, args: &Type, ret: &Type, annot: &Annotation) {
    if let Type::And(args_t) = args {
        write!(
            file, 
            "## {} ({}) {}{} ({}) -> {}\n\n", 
            "op".html_magenta(),
            args_t[0].get_name_html(&module.ctx),
            op.html_yellow(), if t > 0 { 
                format!("&lt;{}&gt;", (0..t).into_iter()
                                            .map(|i| format!("T_{}", i).html_blue())
                                            .collect::<Vec<_>>()
                                            .join(", ")
                ) 
            } else { "".into() },
            args_t[1].get_name_html(&module.ctx),
            ret.get_name_html(&module.ctx)
        ).expect("Error while writing to docs file");    
    
    } else {
        unreachable!()
    }

    write_args_and_ret(file, annot);
}

pub fn write_nary_operation_docs(file: &mut File, module: &NessaModule, op_open: &str, op_close: &str, t: usize, args: &Type, ret: &Type, annot: &Annotation) {
    if let Type::And(args_t) = args {
        let args_b = &args_t[1..];

        write!(
            file, 
            "## {} ({}){}{}{}{} -> {}\n\n", 
            "op".html_magenta(),
            args_t[0].get_name_html(&module.ctx),
            if t > 0 { 
                format!("&lt;{}&gt;", (0..t).into_iter()
                                            .map(|i| format!("T_{}", i).html_blue())
                                            .collect::<Vec<_>>()
                                            .join(", ")
                ) 
            } else { "".into() },
            op_open.html_yellow(),
            args_b.into_iter()
                  .map(|i| i.get_name_html(&module.ctx))
                  .collect::<Vec<_>>()
                  .join(", "),
            op_close.html_yellow(),
            ret.get_name_html(&module.ctx)
        ).expect("Error while writing to docs file");    
    
    } else {
        unreachable!()
    }

    write_args_and_ret(file, annot);
}

pub fn write_class_docs(file: &mut File, template: &TypeTemplate, annot: &Annotation) {
    write!(
        file,
        "## {} {}{}",
        "class".html_magenta(),
        template.name.html_green(),
        if template.params.len() > 0 { 
            format!("&lt;{}&gt;", template.params.iter()
                                                 .map(|i| format!("T_{}", i).html_blue())
                                                 .collect::<Vec<_>>()
                                                 .join(", ")
            ) 
        } else { "".into() }
    ).expect("Error while writing to docs file");

    write!(
        file,
        "\n\n### Attributes\n\n",
    ).expect("Error while writing to docs file");

    for arg in &annot.args {
        if arg.0.parse::<usize>().is_err() {
            write!(
                file, 
                "* `{}`: {}\n", arg.0, arg.1
            ).expect("Error while writing to docs file");
        }
    }
}

pub fn write_syntax_docs(file: &mut File, name: &String, annot: &Annotation) {
    write!(
        file,
        "## {} {}",
        "syntax".html_magenta(),
        name.html_green(),
    ).expect("Error while writing to docs file");

    write!(
        file,
        "\n\n### Description\n{}\n\n",
        annot.args.get("0").unwrap()
    ).expect("Error while writing to docs file");
}

pub fn write_interface_docs(file: &mut File, module: &NessaModule, interface: &Interface, annot: &Annotation) {
    write!(
        file,
        "# {} {}",
        "interface".html_magenta(),
        interface.name.html_green(),
    ).expect("Error while writing to docs file");

    write!(
        file,
        "\n\n## Description\n{}\n\n",
        annot.args.get("0").unwrap()
    ).expect("Error while writing to docs file");

    for f in &interface.fns {
        for a in &f.0 {
            if a.name == "doc" {
                let args = Type::And(f.3.iter().map(|(_, i)| i).cloned().collect());
                let templates = f.2.as_ref().map(Vec::len).unwrap_or_default();

                write_function_overload_docs(file, module, &f.1, templates, &args, &f.4, a);
                break;
            }
        }
    }

    for u in &interface.uns {
        for a in &u.0 {
            if a.name == "doc" {
                if let Operator::Unary { representation, prefix, .. } = &module.ctx.unary_ops[u.1] {
                    write_unary_operation_docs(file, module, &representation, u.2.len(), &u.4, &u.5, a, *prefix);
                    break;    
                }
            }
        }
    }

    for b in &interface.bin {
        for a in &b.0 {
            if a.name == "doc" {
                if let Operator::Binary { representation, .. } = &module.ctx.binary_ops[b.1] {
                    let args = Type::And(vec!(b.3.1.clone(), b.4.1.clone()));
                    write_binary_operation_docs(file, module, &representation, b.2.len(), &args, &b.5, a);
                    break;    
                }
            }
        }
    }

    for n in &interface.nary {
        for a in &n.0 {
            if a.name == "doc" {
                if let Operator::Nary { open_rep, close_rep, .. } = &module.ctx.nary_ops[n.1] {
                    let mut args = vec!(n.3.1.clone());
                    args.extend(n.4.iter().map(|(_, i)| i).cloned());
                    
                    write_nary_operation_docs(file, module, &open_rep, &close_rep, n.2.len(), &Type::And(args), &n.5, a);
                    break;    
                }
            }
        }
    }
}

pub fn generate_all_function_overload_docs(project_path: &String, module: &NessaModule) {
    let mut functions_file = create_markdown_file(project_path, "functions.md");

    for f in &module.ctx.functions {
        for ov in &f.overloads {
            for annot in &ov.annotations {
                if annot.name == "doc" {
                    write_function_overload_docs(&mut functions_file, &module, &f.name, ov.templates, &ov.args, &ov.ret, annot);
                    break;
                }    
            }
        }
    }
}

pub fn generate_all_operation_docs(project_path: &String, module: &NessaModule) {
    let mut operations_file = create_markdown_file(project_path, "operations.md");

    write!(operations_file, "# Unary operations\n\n").expect("Error while writing to docs file");

    for o in &module.ctx.unary_ops {
        if let Operator::Unary { representation, prefix, operations, .. } = o {
            for ov in operations {
                for annot in &ov.annotations {
                    if annot.name == "doc" {
                        write_unary_operation_docs(&mut operations_file, &module, representation, ov.templates, &ov.args, &ov.ret, annot, *prefix);
                        break;
                    }    
                }
            }    
        }
    }

    write!(operations_file, "# Binary operations\n\n").expect("Error while writing to docs file");

    for o in &module.ctx.binary_ops {
        if let Operator::Binary { representation, operations, .. } = o {
            for ov in operations {
                for annot in &ov.annotations {
                    if annot.name == "doc" {
                        write_binary_operation_docs(&mut operations_file, &module, representation, ov.templates, &ov.args, &ov.ret, annot);
                        break;
                    }    
                }
            }    
        }
    }

    write!(operations_file, "# N-ary operations\n\n").expect("Error while writing to docs file");

    for o in &module.ctx.nary_ops {
        if let Operator::Nary { open_rep, close_rep, operations, .. } = o {
            for ov in operations {
                for annot in &ov.annotations {
                    if annot.name == "doc" {
                        write_nary_operation_docs(&mut operations_file, &module, &open_rep, &close_rep, ov.templates, &ov.args, &ov.ret, annot);
                        break;
                    }    
                }
            }    
        }
    }
}

pub fn generate_all_class_docs(project_path: &String, module: &NessaModule) {
    let mut classes_file = create_markdown_file(project_path, "classes.md");

    for c in &module.ctx.type_templates {
        for annot in &c.annotations {
            if annot.name == "doc" {
                write_class_docs(&mut classes_file, c, annot);
                break;
            }
        }
    }
}

pub fn generate_all_syntax_docs(project_path: &String, module: &NessaModule) {
    let mut syntaxes_file = create_markdown_file(project_path, "syntaxes.md");

    for c in &module.ctx.macros {
        for annot in &c.0 {
            if annot.name == "doc" {
                write_syntax_docs(&mut syntaxes_file, &c.1, annot);
                break;
            }
        }
    }
}

pub fn generate_all_interface_docs(project_path: &String, module: &NessaModule) {
    let mut interfaces_file = create_markdown_file(project_path, "interfaces.md");

    for c in &module.ctx.interfaces {
        for annot in &c.annotations {
            if annot.name == "doc" {
                write_interface_docs(&mut interfaces_file, module, c, annot);
                break;
            }
        }
    }
}