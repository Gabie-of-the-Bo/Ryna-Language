use std::collections::HashMap;

lazy_static! {
    static ref VALID_SYMBOLS: Vec<char> = "dlLaAsq".chars().collect::<Vec<_>>();
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Pattern{
    // Markers
    Arg(Box<Pattern>, String),

    // Tail patterns
    Str(String),
    Range(char, char),
    Symbol(char), // Digit (d), Letter (l/L), Alphabetic (a), Alphanumeric (A), Space (s), quote (q)

    // Combination patterns
    Or(Vec<Pattern>),
    And(Vec<Pattern>),
    Optional(Box<Pattern>),
    Repeat(Box<Pattern>, Option<usize>, Option<usize>)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PatternNFANodeType<'a>{
    // Markers
    Start,
    End,
    ArgStart(String),
    ArgEnd,
    
    // Tail nodes
    Str(String),
    Range(char, char),
    Symbol(char),
    
    // Uncompiled nodes
    Uncompiled(&'a Pattern)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PatternNFANode<'a>{
    pattern_type: PatternNFANodeType<'a>,
    outputs: Vec<usize>
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PatternNFAState{
    node: usize,
    pos: usize,
    args: Vec<(String, usize, usize)>, // (Identifier, from, to)
    curr_arg: Option<(String, usize)> // (Identifier, from)
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PatternNFA<'a>{
    nodes: Vec<PatternNFANode<'a>>
}

impl<'a> PatternNFANode<'a>{
    fn start_node() -> PatternNFANode<'a>{
        return PatternNFANode{
            pattern_type: PatternNFANodeType::Start,
            outputs: vec!(1) // Starts connected to the second node
        };
    }

    fn end_node() -> PatternNFANode<'a>{
        return PatternNFANode{
            pattern_type: PatternNFANodeType::End,
            outputs: vec!()
        };
    }

    fn arg_end_node() -> PatternNFANode<'a>{
        return PatternNFANode{
            pattern_type: PatternNFANodeType::ArgEnd,
            outputs: vec!()
        };
    }

    fn uncompiled_node(p: &Pattern) -> PatternNFANode{
        return PatternNFANode{
            pattern_type: PatternNFANodeType::Uncompiled(p),
            outputs: vec!()
        };
    }

    fn matches(&self, text: &[char]) -> Option<usize>{
        return match &self.pattern_type {
            PatternNFANodeType::Str(s) => {
                let chars = &s.as_str().chars().collect::<Vec<_>>()[..];

                if text.starts_with(chars) { 
                    Some(s.len()) 
                
                } else {
                    None 
                }
            }
            
            PatternNFANodeType::Range(start, end) => {
                if text.len() > 0 && text[0] as u32 >= *start as u32 && text[0] as u32 <= *end as u32 { 
                    Some(1) 
                
                } else {
                    None 
                }
            }
            
            PatternNFANodeType::Symbol(s) => {
                let matches = text.len() > 0 && match s {
                    'd' => text[0].is_digit(10),
                    'l' => text[0].is_lowercase(),
                    'L' => text[0].is_uppercase(),
                    'a' => text[0].is_alphabetic(),
                    'A' => text[0].is_alphanumeric(),
                    's' => text[0].is_whitespace(),
                    'q' => text[0] == '\'',
                    
                    _ => unreachable!()
                };

                if matches { Some(1) } else { None }
            }

            _ => Some(0)
        }
    }
}

impl Pattern{
    pub fn compile(&self) -> PatternNFA{
        let mut res = PatternNFA{
            nodes: vec!(PatternNFANode::start_node(), PatternNFANode::uncompiled_node(self), PatternNFANode::end_node())
        };

        res.nodes[1].outputs.push(2);
        res.compile(1);

        return res;
    }
}

impl PatternNFAState{
    fn new(node: usize, pos: usize, args: Vec<(String, usize, usize)>, curr_arg: Option<(String, usize)>) -> PatternNFAState{
        return PatternNFAState{
            node: node,
            pos: pos,
            args: args,
            curr_arg: curr_arg
        }
    }

    fn has_ended(&self, machine: &PatternNFA, text: &[char]) -> bool {
        return machine.nodes[self.node].outputs.len() == 0 && self.pos == text.len();
    }

    fn update_args(&mut self, machine: &PatternNFA) {
        match &machine.nodes[self.node].pattern_type {
            PatternNFANodeType::ArgStart(n) => {
                self.curr_arg = Some((n.clone(), self.pos));
            },

            PatternNFANodeType::ArgEnd => {
                let (name, start) = self.curr_arg.as_ref().unwrap(); 

                self.args.push((name.clone(), *start, self.pos));
                self.curr_arg = None;
            },

            _ => { }
        }
    }

    fn args_map(&self, text: &[char]) -> HashMap<String, Vec<String>> {
        let mut res = HashMap::new();

        for (name, start, end) in &self.args {
            res.entry(name.clone()).or_insert(vec!()).push(text[*start..*end].iter().collect::<String>());
        }

        return res;
    }

    fn advance(&self, machine: &PatternNFA, text: &[char]) -> Vec<PatternNFAState> {
        let mut res = vec!();

        for out in &machine.nodes[self.node].outputs{
            if let Some(n) = machine.nodes[*out].matches(&text[self.pos..]) {
                res.push(PatternNFAState::new(*out, self.pos + n, self.args.clone(), self.curr_arg.clone()));
            }
        }

        return res;
    }
}

impl<'a> PatternNFA<'a>{
    pub fn matches(&self, text: &str) -> Option<usize> {
        let characters = text.chars().collect::<Vec<_>>();
        
        let mut states = vec!(PatternNFAState::new(0, 0, vec!(), None));

        while states.len() > 0{
            let mut new_states = vec!();

            for state in &mut states {
                if state.has_ended(&self, &characters[..]) {
                    return Some(state.pos);
    
                } else {
                    new_states.extend(state.advance(&self, &characters[..]));
                }
            }

            states = new_states;
        }

        return None;
    }

    pub fn extract(&self, text: &str) -> Option<(usize, HashMap<String, Vec<String>>)> {
        let characters = text.chars().collect::<Vec<_>>();
        
        let mut states = vec!(PatternNFAState::new(0, 0, vec!(), None));

        while states.len() > 0{
            let mut new_states = vec!();

            for state in &mut states {
                state.update_args(&self);

                if state.has_ended(&self, &characters[..]) {
                    return Some((state.pos, state.args_map(&characters[..])));
    
                } else {
                    new_states.extend(state.advance(&self, &characters[..]));
                }
            }

            states = new_states;
        }

        return None;
    }

    fn compile(&mut self, i: usize){
        if let PatternNFANodeType::Uncompiled(p) = self.nodes[i].pattern_type {
            match p {
                Pattern::Str(s) => {
                    self.nodes[i].pattern_type = PatternNFANodeType::Str(s.clone());
                }

                Pattern::Range(start, end) => {
                    self.nodes[i].pattern_type = PatternNFANodeType::Range(*start, *end);
                }

                Pattern::Symbol(s) => {
                    self.nodes[i].pattern_type = PatternNFANodeType::Symbol(*s);
                }

                Pattern::And(v) => {
                    // Prepare first node
                    let new_nodes_start = self.nodes.len();
                    let outs = self.nodes[i].outputs.iter().copied().collect::<Vec<_>>();

                    self.nodes[i].pattern_type = PatternNFANodeType::Uncompiled(&v[0]);
                    self.nodes[i].outputs = vec!(self.nodes.len());

                    self.nodes.push(PatternNFANode::uncompiled_node(&v[1]));

                    let mut curr = self.nodes[i].outputs[0];

                    // Create a node for each element in the pattern
                    for j in 1..(v.len() - 1) {
                        let new_node_idx = self.nodes.len();                 
                        self.nodes[curr].outputs.push(new_node_idx);
                        self.nodes.push(PatternNFANode::uncompiled_node(&v[j + 1]));

                        curr = new_node_idx;
                    }

                    let new_nodes_end = self.nodes.len();

                    // Set last node's outputs
                    self.nodes[curr].outputs = outs;

                    // Compile new nodes
                    self.compile(i);
                    
                    for new_node in new_nodes_start..new_nodes_end{
                        self.compile(new_node);
                    }
                }

                Pattern::Or(v) => {
                    // Prepare first node
                    self.nodes[i].pattern_type = PatternNFANodeType::Uncompiled(&v[0]);

                    // Create new nodes
                    let outs = self.nodes[i].outputs.iter().copied().collect::<Vec<_>>();

                    let new_nodes_start = self.nodes.len();
                    self.nodes.extend(v[1..].iter().map(|i| PatternNFANode::uncompiled_node(i)));

                    for new_node in new_nodes_start..self.nodes.len(){
                        self.nodes[new_node].outputs.extend(outs.iter().copied());
                    }

                    // Update outputs and compile new nodes
                    let new_nodes_end = self.nodes.len();

                    for node in &mut self.nodes{
                        if node.outputs.contains(&i) {
                            node.outputs.extend(new_nodes_start..new_nodes_end);
                        }
                    }

                    // Compile new nodes
                    self.compile(i);

                    for new_node in new_nodes_start..new_nodes_end{
                        self.compile(new_node);
                    }
                }

                Pattern::Repeat(p, min, max) => {
                    // Prepare first node
                    let new_nodes_start = self.nodes.len();
                    let outs = self.nodes[i].outputs.iter().copied().collect::<Vec<_>>();

                    self.nodes[i].pattern_type = PatternNFANodeType::Uncompiled(p);

                    let mut curr = i;

                    // Create a node for the minimum repetitions
                    if let Some(m) = min {
                        for _ in 1..*m {
                            let new_node_idx = self.nodes.len();                 
                            self.nodes[curr].outputs = vec!(new_node_idx);
                            self.nodes.push(PatternNFANode::uncompiled_node(p));
    
                            curr = new_node_idx;
                        }

                    } else{
                        for node in &mut self.nodes{
                            if node.outputs.contains(&i) {
                                node.outputs.extend(outs.iter().copied());
                            }
                        }
                    }

                    // Create a node for the maximum repetitions
                    if let Some(m) = max {
                        for _ in min.unwrap_or(1)..*m {
                            let new_node_idx = self.nodes.len();                 
                            self.nodes[curr].outputs = vec!(new_node_idx);
                            self.nodes[curr].outputs.extend(outs.iter().copied());

                            self.nodes.push(PatternNFANode::uncompiled_node(p));
    
                            curr = new_node_idx;
                        }

                    } else{
                        self.nodes[curr].outputs.push(curr);
                    }

                    let new_nodes_end = self.nodes.len();

                    // Set last node's outputs
                    self.nodes[curr].outputs.extend(outs);

                    // Compile new nodes
                    self.compile(i);

                    for new_node in new_nodes_start..new_nodes_end{
                        self.compile(new_node);
                    }
                }

                Pattern::Optional(p) => {
                    let outs = self.nodes[i].outputs.iter().copied().collect::<Vec<_>>();

                    // Update pattern reference
                    self.nodes[i].pattern_type = PatternNFANodeType::Uncompiled(p);

                    // Update references to be able to skip this node
                    for node in &mut self.nodes{
                        if node.outputs.contains(&i) {
                            node.outputs.extend(outs.iter().copied());
                        }
                    }

                    // Compile new node
                    self.compile(i);
                }

                Pattern::Arg(p, n) => {
                    // Set up indices
                    let outs = self.nodes[i].outputs.iter().copied().collect::<Vec<_>>();

                    let curr = self.nodes.len();
                    let end = curr + 1;
                    
                    // Modify current node to be a start node
                    self.nodes.push(self.nodes[i].clone());
                    self.nodes[i].pattern_type = PatternNFANodeType::ArgStart(n.clone());

                    // Modify the next node to point to the inner pattern
                    self.nodes[curr].pattern_type = PatternNFANodeType::Uncompiled(p);
                    self.nodes[i].outputs = vec!(curr);

                    // Set up outputs
                    self.nodes[curr].outputs = vec!(end);

                    self.nodes.push(PatternNFANode::arg_end_node());
                    self.nodes[end].outputs = outs;

                    // Compile the new node
                    self.compile(curr);
                }
            }

        } else{
            panic!("Unable to recompile pattern");
        }
    }
}

fn enclosing_split(string: &[char], s: char) -> Vec<&[char]> {
    let mut depth_par: i32 = 0;
    let mut depth_bra: i32 = 0;
    let mut depth_cur: i32 = 0;
    let mut depth_str: i32 = 0;

    let mut reps = false;
    let mut idx = 0;
    let mut indices = vec!();

    for (i, c) in string.iter().enumerate() {
        if string[idx] != *c {
            reps = false;
        }

        match *c {
            '\'' => depth_str = 1 - depth_str,

            _ if depth_str == 0 => match *c {
                '(' => depth_par += 1,
                ')' => depth_par -= 1,
                '[' => depth_bra += 1,
                ']' => depth_bra -= 1,
                '{' => depth_cur += 1,
                '}' => depth_cur -= 1,
    
                cr if !reps && cr == s && depth_str == 0 && depth_par == 0 && depth_bra == 0 && depth_cur == 0 => {
                    indices.push(&string[idx..i]); 
                    idx = i + 1;
                    reps = true;
                },

                _ => { }
            }

            _ => { }
        }
    }

    indices.push(&string[idx..]); 

    return indices;
}

fn is_enclosed_from_start(string: &[char]) -> bool {
    let mut depth_par: i32 = 0;
    let mut depth_bra: i32 = 0;
    let mut depth_cur: i32 = 0;
    let mut depth_str: i32 = 0;

    for (i, c) in string.iter().enumerate() {
        match *c {
            '\'' => depth_str = 1 - depth_str,

            _ if depth_str == 0 => match *c {
                '(' => depth_par += 1,
                ')' => depth_par -= 1,
                '[' => depth_bra += 1,
                ']' => depth_bra -= 1,
                '{' => depth_cur += 1,
                '}' => depth_cur -= 1,

                _ => { }
            }

            _ => { }
        }

        // Return false when there is nothing enclosing the current position
        if i < string.len() - 1 && depth_str == 0 && depth_par == 0 && depth_bra == 0 && depth_cur == 0 {
            return false;
        }
    }

    return depth_str == 0 && depth_par == 0 && depth_bra == 0 && depth_cur == 0;
}

fn parse_pattern(mut string: &[char]) -> Result<Pattern, String>{
    if string.len() == 0{
        return Err("Unable to parse empty string".into());
    }

    // Trim slice
    while string[0].is_whitespace() { string = &string[1..] }
    while string[string.len() - 1].is_whitespace() { string = &string[..string.len() - 1] }

    // Symbol pattern
    if string.len() == 1 && VALID_SYMBOLS.contains(&string[0]) {
        return Ok(Pattern::Symbol(string[0]));
    }

    // Range pattern
    if string.len() == 5 && string[0] == '[' && string[2] == '-' && string[4] == ']' && string[1] != string[3] {
        return Ok(Pattern::Range(string[1], string[3]));
    }

    // String pattern
    if string.len() > 2 && string[0] == '\'' && string[string.len() - 1] == '\'' && string.iter().filter(|&i| *i == '\'').count() == 2 {
        return Ok(Pattern::Str(string[1..(string.len() - 1)].iter().collect()));
    } 

    // Optional pattern
    if string[0] == '[' && string[string.len() - 1] == ']' && is_enclosed_from_start(string) {
        return Ok(Pattern::Optional(Box::new(parse_pattern(&string[1..(string.len() - 1)]).unwrap())));
    }

    // Repetition pattern
    let first_dig = string.iter().enumerate().filter(|(_, c)| !c.is_digit(10)).map(|(i, _)| i).next().unwrap_or(0);
    let last_dig = string.iter().enumerate().rev().filter(|(_, c)| !c.is_digit(10)).map(|(i, _)| i).next().unwrap_or(string.len() - 1);

    let r_string = &string[first_dig..=last_dig];

    if r_string[0] == '{' && r_string[r_string.len() - 1] == '}' && is_enclosed_from_start(r_string) {
        let mut start = None;
        let mut end = None;

        // Start and end iteration limits
        if first_dig > 0 {
            start = Some(string[..first_dig].iter().collect::<String>().parse::<usize>().unwrap());
        }

        if last_dig < string.len() - 1 {
            end = Some(string[(last_dig + 1)..].iter().collect::<String>().parse::<usize>().unwrap());
        }
        
        return Ok(Pattern::Repeat(Box::new(parse_pattern(&r_string[1..(r_string.len() - 1)]).unwrap()), start, end));
    }

    // Argument marker
    if string.len() > 5 && string[0] == 'A' && string[1] == 'r' && string[2] == 'g' && string[3] == '(' && string[string.len() - 1] == ')' {
        let inner = &string[3..];

        if is_enclosed_from_start(inner){
            let args = enclosing_split(&inner[1..(inner.len() - 1)], ',');

            if args.len() == 2 {
                let name = args[1].iter().collect::<String>();
                let pattern = parse_pattern(args[0]).unwrap();

                return Ok(Pattern::Arg(Box::new(pattern), name.trim().into()));
            }

            return Err(format!("Invalid number of arguments in Argument Marker (expected 2, got {})", args.len()));
        }
    }

    // Alternative pattern
    let alternatives = enclosing_split(string, '|');

    if alternatives.len() > 1 {
        return Ok(Pattern::Or(alternatives.into_iter().map(parse_pattern).map(Result::unwrap).collect::<Vec<_>>()));
    }

    // Composition pattern
    let patterns = enclosing_split(string, ' ');

    if patterns.len() > 1 {
        return Ok(Pattern::And(patterns.into_iter().map(parse_pattern).map(Result::unwrap).collect::<Vec<_>>()));
    }

    return Err(format!("Unable to parse pattern \"{}\"", string.iter().collect::<String>()));
}

impl std::str::FromStr for Pattern{
    type Err = String;

    fn from_str(string: &str) -> Result<Pattern, Self::Err>{
        return parse_pattern(&string.chars().collect::<Vec<_>>());
    }
}