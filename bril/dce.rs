use crate::bril::structure::*;
use std::collections::HashMap;

/*******************************************************************************************
Dead Code Eliminiation Notes: 


```bril
@main {
  a : int = const 100;  <- dead code needs to be eliminated 
  a : int = const 42;
  print a;
}
`
delting unused variables can be done with the following psudo code:

pseudo
```
used = {}
for block in func:
  for instr in block:
    used += instr.args

for block in func:
  for instr in block:
    if instr.dest & instr.dest not in used:
      delete instr
```

This relies on a handy property of bril where by if an object assigns to a value
it does not have side effects. That means instructions in bril are 1 of 2 things,
either an effect operation which doesn't produce results or a value operation
that does not produce side effects and only produce results.

The above pseudocode will fail to fully optimize dead code with the following bril.
Notice that the c and e can be removed but the algorithm above only remove e

bril
```
@main {
   a: int = const 4;
   b: int = const 2;
   c: int = const 1; <-- missed with above algorithm
   d: int = add a b;
   e: int = add c d;
   print d;
}
```

We can solve this by making multiple passes of the same algorithm over the data
looping until the data no longer changes after a full pass of the algorithm.
Adding this to the above pseudocode gives:

pseudo
```

changed = true
while changed:
  used = {}

  changed = false

  for block in func:
    for instr in block:
      used += instr.args

  for block in func:
    renamed = {}
    inc = 1
    seen = {}
    for instr in block:
      if instr.dest:
        seen[instr.dest] += 1 if seen[instr.dest] else seen[instr.dest] = 1
      if instr.dest & instr.dest not in used:
        delete instr
        changed = true

     ```

While the above solves the problem of dead code in the last bril example the next example
shows obviously dead code which is missed by the above algorithm:

bril
```
@main {
   a: int = const 100; <-- Will not be deleted.
   a: int = const 42;
   print a;
}
```

This is a much more difficult problem to solve since in order to delete a
redundent assignment the order of operation must be accounted for. This can be
solved in many different ways.  In the video the instructor keeps a dictionary
of assignments and deletes any that come with two before an effect function. 
This method can be found in redundant assignments 
*/

/// The below code implenets the pseudo code in reverse by first removing 
/// redundant assignments then performing dead code elimination. 
pub fn optimize_blocks(cfg: &mut CFGFunction) {
    for block in cfg.blocks.values_mut() {
        remove_redundant_assignments(block);
    }
    dead_code_elimination(cfg);
    
}

/// This function performs Dead Code Eliminiation by
/// looping over the code multiple times until any unused variables and their assignments 
/// are removed.  This includes value functions who's value is never accessed by an effect function
/// this dirastically reduces code but is destructive and should not be
/// rather than modifying the cfg passed to it a new CFG should be output to make the code cleaner. 
fn dead_code_elimination(cfg: &mut CFGFunction) {
    let mut changed = true;
    while changed {
        let mut used_args : HashMap<String,u32> = HashMap::new();
        for block in cfg.blocks.values() {
            for (key,val) in get_used_args(block) {
                let entry = used_args.entry(key).or_insert(0);
                *entry += val;
            }
        }
        changed = false;
        for block in cfg.blocks.values_mut() {
            let len_before = block.len();
            block.retain(|op| {
                match op {
                    BrilOperation::Effect(_) => true,
                    BrilOperation::Value(BrilValue { dest, .. }) | 
                    BrilOperation::Constant(BrilConstant { dest, .. }) => 
                        used_args.contains_key(dest.as_str())
                }
            });
            changed |= block.len() != len_before;  
        }
    }
}

/// The key to this function is a stack.  As assignments are seen they are
/// added to a stack then when any assiged varable is used in an effect or value
/// operation we can pop the assignments off the stack deleting all but the 
/// most recent value.  The problem is we cannot destruct the vector or bril operations
/// while looping through it. To circumvent this a list of indicies to delete is generated
/// by poping assignments off of the stack and at the end the uneeded assignments can be 
/// removed in a destructive way.  This code would benefit from a re-write whereby a 
/// vector of bril operations is output rather than destroying and modifying the operations
/// in place as is done below. 
fn remove_redundant_assignments(block: &mut Vec<BrilOperation>) {
    let mut var_tbl: HashMap<String, Vec<usize>> = HashMap::new();
    let mut removal_idxs : Vec<usize> = Vec::new();

    for (i,op) in block.iter_mut().enumerate() {
        match op {
            BrilOperation::Constant(BrilConstant { dest, .. }) => {
                var_tbl.entry(dest.to_string()).or_insert(Vec::new()).push(i);
            }
            BrilOperation::Value(BrilValue { args, .. }) |
            BrilOperation::Effect(BrilEffect { args, .. }) => {
                if let Some(safe_args) = args {
                    for a in safe_args.iter() {
                        if let Some(var) = var_tbl.get_mut(a) {
                            if var.len() > 1 {
                                { var.pop(); }
                                while !var.is_empty() {
                                    if let Some(v) = var.pop() {
                                        removal_idxs.push(v);
                                    }
                                }
                            }
                        }
                    }
                }
            },
        }
    }
    // destructive part of fuction .  Likely unneeded. 
    // TODO: make new vector and output rather than modifying.
    let mut new_block : Vec<BrilOperation> = Vec::new();
    for (i,op) in block.iter().enumerate() {
        if !removal_idxs.contains(&i) {
            new_block.push(op.clone());
        }
    }
    *block = new_block;
    // end of destruvtive function.  
}


/// Returns a dictionary of the arguments used in effect and value operations with their counts. 
/// helper function called by remove_redundant_assignments
///
fn get_used_args(blocks : &Vec<BrilOperation>) -> HashMap<String,u32> {
    let mut used_effect : HashMap<String, u32> = HashMap::new();
    for op in blocks {
        match op {
            BrilOperation::Value(BrilValue { args, .. }) |
            BrilOperation::Effect(BrilEffect { args, .. }) => {
                if let Some(safe_args) = args.as_ref() {
                    for arg in safe_args {
                        *used_effect.entry(arg.to_owned()).or_insert(0) += 1;
                    }
                }
            },
            BrilOperation::Constant(_) => continue,
        }
    } 
    used_effect
}
