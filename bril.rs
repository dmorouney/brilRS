pub mod structure;
pub mod lvn;
pub mod dce;
pub mod data_flow;

use structure::*;

use::std::collections::{HashMap};

/// Function outputs a control flow graph for a bril function. 
/// the control flow graph or cfg contains 3 data structures
///   1. blocks : HashMap<label: String, instructions: Vec<BrilOperation>> 
///         The instructions of the function organized into control blocks 
///         whereby each control block terminates at a control flow instruction
///         (branch, jmp, return) or begins when a label is found.  
///   2. edges : HashMap<block_label: String, destinations: Vec<block_label:String>> 
///         Each block has a label (see above) and a list of possible destinations 
///         it may terminate too. These are stored in this structure. 
///   3. order : Vec<block_label : String>
///         A list of block labels added in order used when the original order of the code 
///         is needed for esecution.  This is faster than storing values in an indexed hashmap 
///         and more convienent.
///
/// use `let cfg_function = bril_to_cfg(bril_function)`
///
pub fn bril_to_cfg(func : BrilFunction) -> CFGFunction {
    let mut blocks : HashMap<String, Vec<BrilOperation>> = HashMap::new();
    let mut edges : HashMap<String, Vec<String>> = HashMap::new();
    let mut order : Vec<String> = Vec::new();
    let mut b_string = String::from("ENTRY");
    edges.insert(b_string.to_owned(), Vec::new());
    blocks.insert(b_string.to_owned(), Vec::new());
    order.push(b_string.to_owned());
    let mut b_cnt = 1;
    let mut need_block = false;
    for instr in func.instrs.iter() {
        match instr {
            BrilInstruction::Label(lbl) => {    
                if let Some(vec) = edges.get_mut(b_string.as_str()) {
                    if vec.is_empty() {
                        vec.push(lbl.label.to_owned());
                    }
                }
                b_string = lbl.label.to_owned();
                blocks.insert(b_string.to_owned(), Vec::new());
                edges.insert(b_string.to_owned(), Vec::new());
                order.push(b_string.to_owned());
                need_block = false
            },

            BrilInstruction::Operation(op) => {
                if need_block {
                    if let Some(vec) = edges.get_mut(b_string.as_str()) {
                        if vec.is_empty() {
                            vec.push(format!("block{b_cnt}"));
                        }
                    }
                    b_string = format!("block{b_cnt}");
                    blocks.insert(b_string.to_owned(), Vec::new());
                    edges.insert(b_string.to_owned(), Vec::new());
                    order.push(b_string.to_owned());
                    b_cnt += 1;
                    need_block = false;
                }
                if let Some(last_val) = blocks.get_mut(&b_string){
                    last_val.push(op.to_owned());
                }

                if let BrilOperation::Effect(eff) = op {
                    need_block = true;
                    match eff.op.as_str() {
                        "jmp" | "br" => { 
                            if let Some(lbls) = eff.labels.as_ref() {
                                for lbl in lbls {
                                    edges.entry(b_string.clone())
                                         .or_insert_with(Vec::new)
                                         .push(lbl.to_owned());
                                }
                            }
                        },
                        "ret" => {
                            if let Some(vec) = edges.get_mut(b_string.as_str()) {
                                vec.push(String::from("EXIT"));
                            }
                        },
                        _ => {need_block = false;},
                    }
                }
            }
        }
    }
    for edge in edges.values_mut() {
        if edge.is_empty() {
            edge.push(String::from("EXIT"));
        }
    }
    let args :Vec<String> = func
        .args
        .unwrap_or(Vec::new())
        .iter()
        .map(|arg| arg.name.to_owned())
        .collect();
    
    CFGFunction {
        name: func.name,
        blocks,
        edges,
        order,
        args,
    }
}

#[allow(dead_code)]
/// Prints the Control Flow Graph in a grpahviz format to create a PDF with 
/// command $ ./self | dot -Tpdf -o filename
pub fn print_cfg(cfg_func : CFGFunction) {
    println!("digraph {} {{", cfg_func.name);
    for key in cfg_func.blocks.keys() {
        println!("{key};");
    }
    println!("EXIT;");
    for (key, es) in cfg_func.edges {
        for e in es {
            println!("{} -> {};", key, e);
        } 
    }
    println!("}}");
}


/// Helper function to output the string representation of a bril type
/// this will be updated as more parameterized types (ptr) are added or in the 
/// eventuality another base type is added (float)
///
/// TODO: Add ptr and float :P
///
/// use: `let string_type = get_bril_type(&op);`
///
fn get_bril_type(op: &BrilOperation) -> Option<String> {
    match op {
        BrilOperation::Value(BrilValue { type_, .. }) |
        BrilOperation::Constant(BrilConstant { type_, .. }) => {
                if let BrilType::SimpleType(bt) = type_ {
                    Some(bt.to_owned())
                } else {
                    panic!("Parameterized types not implemented (get_bril_type)");
                }
            },
        _ => None,
    }
}

/// This function prints the ascii representation of a bril function 
/// in standard bril notation as dictated by the language reference. 
/// this is an effect operation and changes no values it only prints the 
/// corresponding code.
/// use `print_bril(&cfg)`
pub fn print_bril(cfg: &CFGFunction) {
    println!("@{} {{", cfg.name.to_owned());
    for block_label in &cfg.order { // make sure the blocks appear in the correct ordering. 
        if let Some(block) = cfg.blocks.get(block_label) {
            println!("  .{}", block_label);
            for op in block {

                // For each operation type build the corresponding bril instruction. 
                match op {
                    BrilOperation::Constant(cop) => {
                        if let Some(typ) = get_bril_type(op){
                            println!("    {} : {} = {} {};",
                                cop.dest.to_owned(),
                                typ.to_owned(),
                                cop.op.to_owned(),
                                cop.value.to_owned()
                            );

                        }
                    },
                    BrilOperation::Value(vop) => {
                        if let Some(typ) = get_bril_type(op){
                            let my_args : String;
                            if let Some(args) = &vop.args {
                                my_args = args.join(" ").to_owned();
                            } else {
                                my_args = String::from("");
                            }
                            println!("    {} : {} = {} {};",
                                vop.dest.to_owned(),
                                typ.to_owned(),
                                vop.op.to_owned(),
                                my_args
                            );
                        }
                    },
                    BrilOperation::Effect(eop) => {
                        let my_args : String;
                        if let Some(args) = &eop.args {
                            my_args = args.join(" ").to_owned();
                        } else {
                            my_args = String::from("");
                        }
                        let my_labels : String; 
                        if let Some(labels) = &eop.labels {
                            my_labels = labels.join(" ").to_owned();
                        } else {
                            my_labels = String::from("");
                        }


                        println!("    {} {} {}", 
                            eop.op.to_owned(),
                            my_args,
                            my_labels
                        );
                    },

                }
            }
        }
    }
    println!("}}");
}



pub fn successors(block_label: &String, cfg: &CFGFunction) -> Vec<String> {
    let mut output : Vec<String> = Vec::new();
    if let Some(block_list) = cfg.edges.get(block_label) { 
        output = block_list.to_owned()
    }
    output
}

pub fn predecessors(block_label: &String, cfg: &CFGFunction) -> Vec<String> {
    let mut output : Vec<String> = Vec::new();
    for (block, edges) in &cfg.edges {
        if edges.contains(block_label) { 
            output.push(block.to_owned()); 
        }
    }
    output
}
