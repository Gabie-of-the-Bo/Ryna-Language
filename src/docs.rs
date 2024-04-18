use std::fs::OpenOptions;
use std::{fs::File, path::Path};
use std::io::Write;

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

pub fn write_function_overload_docs(file: &mut File, module: &NessaModule, f: &str, t: usize, args: &Type, ret: &Type, annot: &Annotation) {
    write!(
        file, 
        "## {}{}{} -> {}\n\n", 
        f.html_yellow(), if t > 0 { 
            format!("&lt;{}&gt;", (0..t).into_iter()
                                        .map(|i| format!("T_{}", i).html_blue())
                                        .collect::<Vec<_>>()
                                        .join(", ")
            ) 
        } else { "".into() },
        args.get_name_html(&module.ctx), ret.get_name_html(&module.ctx)
    ).expect("Error while writing to docs file");

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