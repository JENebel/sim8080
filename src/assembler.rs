pub struct AssemblerError {
    pub line: usize,
    pub message: String,
}

pub struct AsmLine {
    pub line: usize,
    pub label: Option<String>,
    /// Indicates whether the label is a 'name', eg. not seperated by a colon
    pub is_name: bool,
    pub instruction: String,
    pub args: Vec<String>,
}

impl AsmLine {
    fn parse()
}

pub fn assemble(lines: Vec<String>) -> Result<Vec<u8>, AssemblerError> {
    let lines = preprocess(lines);


    Ok(Vec::new())
}

pub fn preprocess(mut lines: Vec<String>) -> Vec<String> {
    let new_lines = Vec::new();
    

    todo!()
}