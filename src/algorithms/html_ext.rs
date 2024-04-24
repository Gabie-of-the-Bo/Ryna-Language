pub fn html_color(s: &String, color: &str) -> String {
    format!("<span style=\"color: {};\">{}</span>", color, s)
}

pub fn html_rgb(s: &String, r: u8, g: u8, b: u8) -> String {
    format!("<span style=\"color: rgb({}, {}, {});\">{}</span>", r, g, b, s)
}
pub trait HTMLColorable {
    fn html_color(&self, color: &str) -> String;
    fn html_rgb(&self, r: u8, g: u8, b: u8) -> String;
    
    fn html_cyan(&self) -> String {
        self.html_rgb(0, 255, 255)
    }
    
    fn html_green(&self) -> String {
        self.html_rgb(78, 201, 176)
    }
    
    fn html_magenta(&self) -> String {
        self.html_rgb(197, 134, 192)
    }
    
    fn html_yellow(&self) -> String {
        self.html_rgb(220, 220, 170)
    }
    
    fn html_blue(&self) -> String {
        self.html_rgb(86, 156, 214)
    }
}

impl HTMLColorable for &str {
    fn html_color(&self, color: &str) -> String {
        html_color(&self.to_string(), color)
    }
    
    fn html_rgb(&self, r: u8, g: u8, b: u8) -> String {
        html_rgb(&self.to_string(), r, g, b)
    }
}

impl HTMLColorable for String {
    fn html_color(&self, color: &str) -> String {
        html_color(self, color)
    }
    
    fn html_rgb(&self, r: u8, g: u8, b: u8) -> String {
        html_rgb(self, r, g, b)
    }
}