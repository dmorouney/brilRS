use crate::bril::structure::*;
use crate::bril::get_bril_type;
use std::collections::HashMap;
use serde_json::Value;

/// Performs local value numbering on a control flow graph. The control flow graph is represented 
/// using a CFGFunction struct, which contains information about the basic blocks, their order, 
/// and the edges between them.
///
/// Local value numbering is a technique for optimizing code by assigning the same value number to 
/// identical expressions. This technique is applied locally to each basic block in the control flow 
/// graph.
///
/// The LVNValue and LVal enums are used to represent values in the local value numbering table. The 
/// LVNRow struct represents a row in the table, and the LVNTable struct represents the table as a 
/// whole.
///
/// The `local_value_numbering` function iterates over each block in the control flow graph, and 
/// then over each instruction in the block. For each instruction, it checks whether it is a constant, 
/// a value, or an effect. If it is a constant or a value, it constructs a tuple representing the 
/// instruction and its operands, and assigns it a value number. If it is an effect, it assigns it no 
/// value number.
pub fn local_value_numbering(cfg: &CFGFunction, constant_folding: bool) -> CFGFunction {
    let mut blocks : HashMap<String, Vec<BrilOperation>> = HashMap::new();
    for (block_label, block) in &cfg.blocks {
        let mut new_block : Vec<BrilOperation> = Vec::new();
        let mut lvn_rows : Vec<LVNRow> = Vec::new();
        let mut environment : HashMap<String, usize> = HashMap::new();
        for op in block.iter() {
            let mut val_tuple : Option<LVNValue>;
            let dest_var : String;
            let op_type : String;
            // Get the type of the operation with helper function get_bril_type
            if let Some(safe_type) = get_bril_type(op) {
                op_type = safe_type;
            } else {
                op_type = String::from("");
            }
            // For each type of operation (Effect, Constant, Value) build a tuple to be 
            // added to the value column of the lvn table. The tuple will have the following 
            // format: 
            //   constant => (Int|Bool(value)) : value = i64 or boolean value. 
            //   value    => (operation, (arg1, ... , argN)) : operation = string and arg1..N = usize 
            //   effect   => None
            // Arguments for value operations are indices of other values in the lvn table
            match op {
                BrilOperation::Constant(const_op) => {
                    val_tuple = Some(build_const_tuple(const_op));
                    dest_var = const_op.dest.to_owned();
                },
                BrilOperation::Value(value_op) => {
                    val_tuple = Some(build_value_tuple(value_op, &environment));
                    dest_var = value_op.dest.to_owned();
                    // id value functions assign a variable to a known value
                    // I leave these operations out as they are redundant 
                    // only the environmentironment is updated with a pointer to the 
                    // value and a reference to the new name. 
                    if value_op.op.eq("id") {
                        if let Some(LVNValue::Op(_, ids)) = val_tuple {
                            environment.insert(value_op.dest.to_owned(), ids[0]);
                        }
                        val_tuple = None;
                    } 
                },
                // Effect operations are an interesting case.  Since they do not operate
                // on a value there is no need to create a value tuple. However in the some cases
                // like print and branch an effect operation can take an argument.  Since this
                // argument may point to a value that may have changed the argument or arguments must
                // be updated. As above the below code is destructive and should be replaced by outputing 
                // a new CFG rather than editing a mut reference to one in place. 
                BrilOperation::Effect(eff_op) => {
                    val_tuple = None;
                    dest_var = String::from("");
                    let mut new_op = eff_op.to_owned();
                    if let Some(eff_args) = &eff_op.args {
                        let mut new_args : Vec<String> = Vec::new();
                        for arg in eff_args {
                            if let Some(eff_arg_idx) = environment.get(arg) {
                                if let Some(new_eff_arg_var) = lvn_var_name_frm_idx(*eff_arg_idx,&lvn_rows) {
                                    new_args.push(new_eff_arg_var);
                                }
                            }
                        }
                        new_op.args = Some(new_args);
                    }

                    
                    new_block.push(BrilOperation::Effect(new_op));
            }  }
            // Now that we have a value tuple we need to make sure it is not already in the LVN table.
            // If it is in the table then we will need to update this instructions dest field to the 
            // value that was previously added.  This is the heart of the LVN function as described above. 
            // This function is destructive to the CFG and changes the dest var in place. 
            println!("----\nOP = {:?}\nVAL TUPLE = {:?}", op,val_tuple);
            if let Some(val) = &mut val_tuple {
                // Look to see if val in table 
                
                if let Some(idx) = search_lvn_rows(val, &lvn_rows) { 

                // found it!
                    environment.insert(dest_var.to_owned(),idx);// update environmentironment with pointer to value
                } else {   
                // Not in table. must add
                    let i = lvn_rows.len();

                    // CONSTANT FOLDING
                    // fold value operation 
                    let mut folded = false;
                    if constant_folding {
                        if let LVNValue::Op(_,_) = val  { 
                            if let Some(new_val) = constant_fold(val, &lvn_rows){
                                *val = new_val;
                                folded = true;
                    }   }   } 
                    let mut new_instr = op.to_owned();
                    let row : LVNRow = LVNRow { idx: i, value: val.to_owned(), var: dest_var.to_owned(), typ: op_type.to_owned()};
                    lvn_rows.push(row.to_owned());
                    environment.insert(dest_var.to_owned(), i);
                    // Create new Instruction.
                    if folded {
                        if let LVNValue::Const(LVal::Int(i)) = val {
                            new_instr = BrilOperation::Constant(BrilConstant { 
                                op: String::from("const"), 
                                dest: dest_var.to_owned(), 
                                type_: BrilType::SimpleType(op_type.to_owned()), 
                                value: Value::from(*i)});
                        } else if let LVNValue::Const(LVal::Bool(b)) = val {
                            new_instr = BrilOperation::Constant(BrilConstant { 
                                op: String::from("const"), 
                                dest: dest_var.to_owned(), 
                                type_: BrilType::SimpleType(op_type.to_owned()), 
                                value: Value::from(*b)});
                        }
                    } else {
                        match new_instr {
                            BrilOperation::Value(ref mut new_op) => {
                                if let LVNValue::Op(_, new_args) = val {
                                    let mut new_arg_list : Vec<String> = Vec::new();
                                    for new_a in new_args {
                                        if let Some(a) = lvn_var_name_frm_idx(*new_a, &lvn_rows) {
                                            new_arg_list.push(a);
                                        }
                                    }
                                    new_op.args = Some(new_arg_list);
                                }
                            },
                            BrilOperation::Effect(_) => {},
                            BrilOperation::Constant(ref mut new_op) => {
                                new_op.dest = dest_var.to_owned();
                            },
                    }   }
                    new_block.push(new_instr.to_owned());
        }   }   }
        blocks.insert(block_label.to_owned(), new_block);
    }
    CFGFunction { 
        name: cfg.name.to_owned(), 
        blocks, 
        edges: cfg.edges.to_owned(), 
        order: cfg.order.to_owned(),
        args: cfg.args.to_owned(),
    }
}

/// constant_fold that takes two arguments:
///     value: a reference to a LVNValue enum variant.
///     lvn_table: a slice of LVNRow structs.
///
/// The function returns an Option<LVNValue> type.
///
/// The function first checks if value is of the LVNValue::Op variant, which 
/// means it is an operation that needs to be evaluated. If it is not, the 
/// function returns None.
///
/// If it is an operation, the function creates an empty vector named 
/// argument_values to store the arguments of the operation.
///
/// The function then iterates over the arguments vector of the LVNValue::Op 
/// variant. For each argument, it checks if the corresponding LVNRow is 
/// present in lvn_table using the get method. If the LVNRow exists, it checks 
/// if the value of the LVNValue is a constant using the LVNValue::Const 
/// variant. If it is, the constant value is pushed to the argument_values 
/// vector.
///
/// After collecting the constant arguments, the function uses a match expression 
/// to match the operation string to one of the possible arithmetic, comparison, 
/// or logical operations. For each operation, a corresponding function is called 
/// to perform the operation on the argument_values vector. These functions are 
/// named lvn_value_fold_arith, lvn_value_fold_comp, and lvn_value_fold_logic1 
/// and lvn_value_fold_logic2. 
/// These functions take a closure that defines the specific operation to perform 
/// on the vector.
///
/// If the operation string does not match any of the possible operations, 
/// the function panics with a message indicating that the operation is unknown.
///
/// Finally, if value is not an LVNValue::Op, the function returns None.
fn constant_fold(value : &LVNValue, lvn_table: &[LVNRow]) -> Option<LVNValue> {
    if let LVNValue::Op(operation, arguments) = value {
        let mut argument_values : Vec<LVal> = Vec::new();
        for argument in arguments {
            if let Some(lvn_row) = lvn_table.get(*argument) {
                if let LVNValue::Const(const_val) = &lvn_row.value {
                    argument_values.push(const_val.to_owned());
                }
            }
        }
        match operation.as_str() {
            // Arithmatic 
            "add" => lvn_value_fold_arith(|x,y|{x+y}, &argument_values),
            "sub" => lvn_value_fold_arith(|x,y|{x-y}, &argument_values),
            "mul" => lvn_value_fold_arith(|x,y|{x*y}, &argument_values),
            "div" => lvn_value_fold_arith(|x,y|{x/y}, &argument_values),
            // Comparison
            "eq" => lvn_value_fold_comp(|x,y|{x==y}, &argument_values),
            "lt" => lvn_value_fold_comp(|x,y|{x<y}, &argument_values),
            "gt" => lvn_value_fold_comp(|x,y|{x>y}, &argument_values),
            "le" => lvn_value_fold_comp(|x,y|{x<=y}, &argument_values),
            "ge" => lvn_value_fold_comp(|x,y|{x>=y}, &argument_values),
            // Logic
            "not" => lvn_value_fold_logic1(|x|{!x}, &argument_values),
            "and" => lvn_value_fold_logic2(|x,y|{x&y}, &argument_values),
            "or" => lvn_value_fold_logic2(|x,y|{x|y}, &argument_values),
            _ => {
                panic!("Unkown operation! Cannot unfold! {}", operation.as_str());
            }
        }
    } else {
        None
    }
} 

fn lvn_value_fold_arith<F>(f: F, a: &[LVal]) -> Option<LVNValue> where F: Fn(i64, i64) -> i64 {
    assert!(a.len() == 2, "error: wrong number of arguments, arguments needed 2 arguments given {}", a.len());
    if let (Some(LVal::Int(v1)), Some(LVal::Int(v2))) = (a.get(0), a.get(1)) {
        Some(LVNValue::Const(LVal::Int(f(*v1,*v2))))
    } else {
        None
    }
}

fn lvn_value_fold_comp<F>(f: F, a: &Vec<LVal>) -> Option<LVNValue> where F: Fn(i64, i64) -> bool {
    assert!(a.len() == 2, "error: wrong number of arguments");
    if let (Some(LVal::Int(v1)), Some(LVal::Int(v2))) = (a.get(0), a.get(1)) {
        Some(LVNValue::Const(LVal::Bool(f(*v1,*v2))))
    } else {
        None
    }
}

fn lvn_value_fold_logic2<F>(f: F, a: &Vec<LVal>) -> Option<LVNValue> where F: Fn(bool, bool) -> bool {
    assert!(a.len() == 2, "error: wrong number of arguments");
    if let (Some(LVal::Bool(v1)), Some(LVal::Bool(v2))) = (a.get(0), a.get(1)) {
        Some(LVNValue::Const(LVal::Bool(f(*v1,*v2))))
    } else {
        None
    }
}

fn lvn_value_fold_logic1<F>(f: F, a: &Vec<LVal>) -> Option<LVNValue> where F: Fn(bool) -> bool {
    assert!(a.len() == 1, "error: wrong number of arguments");
    if let Some(LVal::Bool(v1)) = a.get(0) {
        Some(LVNValue::Const(LVal::Bool(f(*v1))))
    } else {
        None
    }
}

fn lvn_var_name_frm_idx(idx: usize, lvn_rows: &Vec<LVNRow>) -> Option<String> {
    for row in lvn_rows {
        if row.idx == idx {
            return Some(row.var.to_owned());
        }
    }
    None
}
pub trait CompareLVNValue {
    fn compare(&self, other: &LVNValue) -> bool;
}

impl CompareLVNValue for LVNValue {
    fn compare(&self, other: &LVNValue) -> bool {
        match (self, other) {
            (LVNValue::Const(fst), LVNValue::Const(snd)) => {
                match (fst,snd) {
                    (LVal::Int(a), LVal::Int(b)) => a == b,
                    (LVal::Bool(a), LVal::Bool(b)) => a == b,
                    _ => false,
                }
            },
            (LVNValue::Op(a, b), LVNValue::Op(c, d)) => a.eq(c) && b == d,
            _ => false,
        }
    }
}


fn build_const_tuple(c: &BrilConstant) -> LVNValue {
    let lvn_value: LVNValue;
    match c.type_ {
        BrilType::SimpleType(_) => {
            match &c.value {
                serde_json::Value::Bool(b) => 
                    lvn_value = LVNValue::Const(LVal::Bool(b.to_owned())),
                serde_json::Value::Number(n) => {
                    if let Some(n64) = n.as_i64() {
                        lvn_value = LVNValue::Const(LVal::Int(n64.to_owned()));
                    } else {
                        panic!("value not found in number type? {:?}", c);
                    }
                },
                _ => panic!("unimplemented type value caused panic. {:?}", c),
            }
        },
        BrilType::ParameterizedType { .. } => {
            panic!("parameterized types have not been implemented."); 
        },
    }
    lvn_value
}

fn build_value_tuple(v: &BrilValue, env: &HashMap<String, usize>) -> LVNValue {
    let mut vargs: Vec<usize> = Vec::new();
    if let Some(args) = &v.args {
        for arg in args {
            if let Some(idx) = env.get(arg.as_str()) {
                vargs.push(idx.to_owned());
            }
        }
    }
    LVNValue::Op(v.op.to_owned(), vargs)
}

#[allow(dead_code)]
fn build_effect_tuple(e: &BrilEffect, env: &HashMap<String, usize>) -> LVNValue {
    let mut eargs : Vec<usize> = Vec::new();
    if let Some(args) = &e.args {
        for arg in args {
            if let Some(idx) = env.get(arg.as_str()) {
                eargs.push(idx.to_owned());
            }
        }
    }
    LVNValue::Op(e.op.to_owned(), eargs)
}

// Bril Value functions all take 1 or 2 variable names as args and 
// 1 variable name as a destination
/// searches lvn tbl for LVal and returns the index of LVal if 
/// found and or None if the LVal is not present in the table. 
fn search_lvn_rows(val: &LVNValue, tbl: &[LVNRow]) -> Option<usize> {
    for (i,row) in tbl.iter().enumerate() {
        if row.value.compare(val) { return Some(i); }
    }
    None
}

/*********************************************************************************************************
* # Local Value Numbering
*   /Killing 3 optimization birds with one stone./
*
*   DEAD CODE ELIMINATION (DCE) : eliminates redundant assignments, see code below 
*                                 (remove_redundant_assignments)
*
*   COPY PROPAGATION (CP):        If the same value is copied through a program it may be easier to just use that
*                                 value directly. 
*                                 Ex:
*                                 ```bril
*                                      main {
*                                        x: int = const 4;
*                                        copy1: int = id x;
*                                        copy2: int = id copy1;
*                                        copy3: int = id copy2;
*                                        print copy3;
*                                      }
*                                 ```
*                                 After copy propagation becomes:
*                                 ```bril
*                                      main {
*                                        x: int = const 4;
*                                        print x;
*                                      }
*                                  ```
*
*   COMMON SUBEXPRESSION ELIMINATION (CSE) : 
*                                 Best demonstrated with an example (see below) CSE aims to remove reundant
*                                 subexpressions.  So if >=2 VALUE instructions use the same arguments, only
*                                 one instance is needed. EX:
*                                 ```bril
*                                      main {
*                                        a: int = const 4;
*                                        b: int = const 2;
*                                        sum1: int = add a b;
*                                        sum2: int = add b a;
*                                        prod: int = mul sum1 sum2;
*                                        print prod;
*                                      }
*                                 ```
*                                 After CSE will become:
*                                 ```bril
*                                     main {
*                                       a: int = const 4;
*                                       b: int = const 2;
*                                       sum1: int = add a b;
*                                       prod: int = mul sum1 sum1;
*                                       print prod;
*                                     }
*
*   These can all be eliminated with value numbering which is a concept that looks at the values being 
*   passed around as opposed to their respective variables.  By looking only at the values a large amount
*   of optimization can be acheived relatively easily. 
*
*   ## High-level overview of VALUE NUMBERING: 
*
*       Build a table to track unique canonical source for every value we compute. This table will be the 
*       main data structure used for the entire process of value numbering.
*
*      `#  |  VALUE (expression)  | CANONICAL VARIABLE NAME
*       -----------------------------------------------------
* a ->  1  |                      |    x                      
* b ->  2  | #1 + #1              |    a
*       3  | #2 * #1              |    d
* c ->  4  | #3 + #2              |    c 
*
*       Step through the code keeping track of the value number for each variable at a given point in time.
*       Using this method we can avoid making redundant calculations which will save computation. This will
*       be done by steping through the code and keeping track of an imaginary environment where the values
*       are tracked through their various computations. Not by doing the computations themselves but by 
*       keeping a map of values like the table above.  The environment is a list that relates vars used in
*       the program to the value table. 
*
*       Example: 
*           ```bril
*                main {
*                  a: int = const 4;
*                  a: int = const 2;          <---- PC @ step 1
*                  sum1: int = add a b;       <---- PC @ step 2
*                  sum2: int = add b a;       <---- PC @ step 3
*                  prod: int = mul sum1 sum2; <---- PC @ step 4
*                  print prod;                <---- PC @ step 5
*                }
*           ```
*
*           ### STEP 1: 
*
*          `#  |  VALUE             | VARIABLE             ENVIRONMENT:
*          ----+--------------------+----------            ----------------
*           1  |  (const,4)         |   a         <-----    a           
*           2  |  (const,2)         |   b         <-----    b
*
*           When the program is at line 3 the table and environment look like above. The next step is to 
*           interperated the add instruction, 
*
*           ### STEP 2: 
*
*          `#  |  VALUE             | VARIABLE             ENVIRONMENT: 
*          ----+--------------------+----------            ----------------
*           1  |  (const,4)         |   a         <-----    a           
*           2  |  (const,2)         |   b         <-----    b
*           3  |  (add, #1, #2)     |   sum1      <-----    sum1 
*           
*           In this step we see an add instruction so we generate a tuple with the instruction and arguments 
*           this tuple is then propagated with the index numbers from the value table of the related variables 
*           in the arguments and declared in the environment.  If the value, after construction, is unique add 
*           it to the table and fill in the rest of the columns. 
*           
*           ### STEP 3: 
*
*           Frist we construct the tuple with a sorted list of arguments:
*           a + b -> (add, a, b) -> (add, #2, #1) -> (add, #1, #2)
*
*           We then search through the value portion of the value table and find that sum1 has the same tuple 
*           as the one constructed above. The tuple is not added to the table and instead it is added to the 
*           environment with a pointer to the 3rd value (sum1) in the table. 
*
*          `#  |  VALUE             | VARIABLE             ENVIRONMENT: 
*          ----+--------------------+----------            ----------------
*           1  |  (const,4)         |   a         <-----    a           
*           2  |  (const,2)         |   b         <-----    b
*           3  |  (add, #1, #2)     |   sum1      <-----    sum1, sum2  
*           
*           ### STEP 4: 
*
*           Frist we construct the tuple with a sorted list of arguments: 
*           sum1 * sum2 -> (mul, sum1, sum2) -> (mul, #3, #3)
*
*           Searching the table we see the value has not appeard before so we add it to the table 
*
*
*          `#  |  VALUE             | VARIABLE             ENVIRONMENT: 
*          ----+--------------------+----------            ----------------
*           1  |  (const,4)         |   a         <-----    a           
*           2  |  (const,2)         |   b         <-----    b
*           3  |  (add, #1, #2)     |   sum1      <-----    sum1, sum2
*           4  |  (mul, #3, #3)     |   prod      <-----    prod
*
*
*           ### STEP 5
*           In the final step all that is done for the print instruction is to look at the arguments 
*           to print and see that it takes the prod variable.  Look this variable up in the environment
*           table and confirm that the value #4 from the table should be printed. 
*
*           look at instr 
*           if instr is const 
*           22:37 in lec3b -- gg
*
**********************************************************************************************************/
