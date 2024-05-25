use colored::Colorize;

// Basic formats

fn check_camel_case(name: &str, instance: &str) -> Result<(), String> {
    if name.chars().next().unwrap().is_lowercase() {
        return Err(format!("{instance} name {} should begin with an uppercase letter", name.green()));
    }

    if name.contains("_") {
        return Err(format!("{instance} name {} should not contain underscores", name.green()));
    }

    Ok(())
}

fn check_snake_case(name: &str, instance: &str) -> Result<(), String> {
    if name.chars().next().unwrap().is_uppercase() {
        return Err(format!("{instance} name {} should begin with a lowercase letter", name.green()));
    }

    let chars = name.chars().collect::<Vec<_>>();

    if chars.windows(2).any(|a| a[0].is_lowercase() && a[1].is_uppercase()) {
        return Err(format!("{instance} name {} should not use camel case", name.green()));
    }

    Ok(())
}

// Special cases

pub fn check_class_name(name: &str) -> Result<(), String> {
    check_camel_case(name, "Class")
}

pub fn check_interface_name(name: &str) -> Result<(), String> {
    check_camel_case(name, "Interface")
}

pub fn check_template_name(name: &str) -> Result<(), String> {
    check_camel_case(name, "Template")
}

pub fn check_fn_name(name: &str) -> Result<(), String> {
    check_snake_case(name, "Function")
}