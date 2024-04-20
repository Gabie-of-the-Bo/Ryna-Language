use std::fs::OpenOptions;
use std::{fs::File, path::Path};
use std::io::Write;

use crate::operations::Operator;
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

pub fn write_unary_operation_docs(file: &mut File, module: &NessaModule, f: &str, t: usize, args: &Type, ret: &Type, annot: &Annotation, prefix: bool) {
    if prefix {
        write!(
            file, 
            "## {} {}{}({}) -> {}\n\n", 
            "op".html_magenta(),
            f.html_yellow(), if t > 0 { 
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
            f.html_yellow(), ret.get_name_html(&module.ctx)
        ).expect("Error while writing to docs file");    
    }

    write_args_and_ret(file, annot);
}

pub fn write_binary_operation_docs(file: &mut File, module: &NessaModule, f: &str, t: usize, args: &Type, ret: &Type, annot: &Annotation) {
    if let Type::And(args_t) = args {
        write!(
            file, 
            "## {} ({}) {}{} ({}) -> {}\n\n", 
            "op".html_magenta(),
            args_t[0].get_name_html(&module.ctx),
            f.html_yellow(), if t > 0 { 
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

pub fn generate_all_function_overload_docs(project_path: &String, module: &NessaModule) {
    let mut functions_file = create_markdown_file(project_path, "functions.md");

    for f in &module.ctx.functions {
        for ov in &f.overloads {
            for annot in &ov.0 {
                if annot.name == "doc" {
                    write_function_overload_docs(&mut functions_file, &module, &f.name, ov.1, &ov.2, &ov.3, annot);
                    break;
                }    
            }
        }
    }
}

pub fn generate_all_operation_docs(project_path: &String, module: &NessaModule) {
    let mut operations_file = create_markdown_file(project_path, "operations.md");

    for o in &module.ctx.unary_ops {
        if let Operator::Unary { representation, prefix, operations, .. } = o {
            for ov in operations {
                for annot in &ov.0 {
                    if annot.name == "doc" {
                        write_unary_operation_docs(&mut operations_file, &module, representation, ov.1, &ov.2, &ov.3, annot, *prefix);
                        break;
                    }    
                }
            }    
        }
    }

    for o in &module.ctx.binary_ops {
        if let Operator::Binary { representation, operations, .. } = o {
            for ov in operations {
                for annot in &ov.0 {
                    if annot.name == "doc" {
                        write_binary_operation_docs(&mut operations_file, &module, representation, ov.1, &ov.2, &ov.3, annot);
                        break;
                    }    
                }
            }    
        }
    }
}