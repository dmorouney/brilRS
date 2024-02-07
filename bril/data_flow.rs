/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
*                                                                             *
*                      4:  D A T A   F L O W                                  *
*                                                                             *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
use crate::bril::structure::*;     // all of the structs for working with bril
use::std::collections::{HashMap,HashSet};
use crate::bril::{successors, predecessors};
// use crate::bril::get_bril_type; // ex of how to import fn from bril.rs

#[derive(Debug, Clone)]
pub struct DFMap {
    pub ins : HashMap<String, HashSet<String>>,
    pub outs: HashMap<String, HashSet<String>>,
    pub labels: Vec<String>,
}

#[allow(dead_code)]
fn worklist<MergeFunction, TransferFunction>(
    cfg : &CFGFunction, 
    init: &HashSet<String>, 
    merge: MergeFunction, 
    transfer: TransferFunction) -> DFMap
    where MergeFunction: 
            Fn(HashSet<String>, HashSet<String>) -> HashSet<String>,
          TransferFunction: 
            Fn(&[BrilOperation], &HashSet<String>) -> HashSet<String> 
//code start: 
{
    let entry = cfg.order[0].to_owned();

//in_set[entry] = init
    let mut in_set : HashMap<String, HashSet<String>> = cfg.order
        .iter()
        .map(|l| (l.to_owned(), HashSet::new()))
        .collect();
    in_set.insert(entry, init.clone());

//out_set[*] = init:w
    let mut out_set : HashMap<String, HashSet<String>> = cfg.blocks
      .keys()
      .map(|label| (label.to_owned(), init.clone()))
      .collect();

//worklist = all blocks
    let mut worklist : Vec<String> = cfg.order.clone().into_iter().rev().collect();

//while worklist is not empty:
//block = pick any block from worklist
    while let Some(block) = worklist.pop() {

//in_set[block] = merge(out[p] for every predecessor p of block)
        let in_values: HashSet<String> = predecessors(&block, cfg)
            .into_iter()
            .map(|p| out_set[&p].clone())
            .fold(HashSet::new(), &merge);
        in_set.insert(block.to_owned(), merge(in_set[&block].clone(), in_values.clone()));
        
//out_set[b] = transfer(block, in_set[block])
        let out_t = transfer(&cfg.blocks[&block], &in_set[&block]);
        
// rename overwritten variables assigned in multiple blocks with unique names 
        let out_values = out_t
            .into_iter()
            .map(|v| { 
                if v.ends_with('_') { 
                    format!("{}.{block}",v.strip_suffix('_').unwrap())
                } else {
                    v
                }})
            .collect();

//  if out_set[block] changed:
        if out_set[&block] != out_values {
            out_set.insert(block.to_owned(), out_values.clone());
            for successor in successors(&block, cfg) {
//  worklist += successors of block
                if successor != "EXIT"  { 
                    worklist.push(successor); 
                }
            }
        }
    }

    DFMap { 
        ins: in_set, 
        outs: out_set, 
        labels: cfg.order.to_owned() 
    }
}


fn union_merge(x: HashSet<String>, y: HashSet<String>) -> HashSet<String> {
    x.union(&y).cloned().collect()
}

fn rdef_transfer(block: &[BrilOperation], in_set: &HashSet<String>) -> HashSet<String> {

    let mut base_set : HashSet<String> = HashSet::new(); 
    let mut in_map :HashMap<String, HashSet<String>> = HashMap::new();

    in_set.iter().for_each(|i| {
        let base = i.split('.')
            .next()
            .unwrap_or(i)
            .to_string();
        in_map.entry(base.clone())
            .or_default()
            .insert(i.to_owned());
        base_set.insert(base);
        });

    let def_set : HashSet<String> = block.iter()
        .filter_map( |op| op.get_dest() )
        .collect();
    
    let kill_list : HashSet<String> = base_set.intersection(&def_set).cloned().collect();

    let def_set_ : HashSet<String> = def_set
        .iter()
        .map(|s| format!("{s}_"))
        .collect();

    for kill in &kill_list {
        in_map.remove(kill);
    }

    let out_set : HashSet<String> = in_map.values()
        .fold(HashSet::new(), |acc, set| {
            acc.union(set).cloned().collect()
        });
    
    out_set.union(&def_set_).cloned().collect()
       
}

pub fn reaching_definitions(cfg: &CFGFunction) -> DFMap {
    let init : HashSet<String> = cfg
        .args
        .clone()
        .into_iter()
        .collect();
    worklist(cfg, &init, union_merge, rdef_transfer)
}


//‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\\
/*   Data flow analysis : a way to reason about the entire CFG rather than just 
*    a block.  To demonstrate the basic idea we can start with this bril 
*    program:
*
*    ```bril                          +----------------------+
*    @main(cond: bool) {              |  fn main(cond):      |    
*      a: int = const 47;             |     a = 47           |  
*      b: int = const 42;             |     b = 42           |
*      br cond .left .right;          |     if cond:         |    
*    .left:                           |        b = 1         |   
*      b: int = const 1;        --->  |        c = 5         |  
*      c: int = const 5;              |     else:            |  
*      jmp .end;                      |        a = 2         |   
*    .right:                          |        c = 10        |  
*      a: int = const 2;              |                      |  
*      c: int = const 10;             |     print (a - c)    |   
*      jmp .end;                      +----------------------+
*    .end:
*      d: int = sub a c;
*      print d;
*    }
*    ```
*    Imagine the task is to do some constant propagation whereby we want to 
*    know what `a` and `c` will be when `d` is printed out. To accomplish this
*    the entire CFG must be analized to determine at then block `end` which 
*    of the assignments for `a` and `c` are relavant. This cannot be done 
*    with the techniques from `lvn.rs` alone because of the ordering of this
*    program it is hard to know until runtime which branch will be taken. If 
*    the entire call stack can be analyzed and we see that `main` is always 
*    called with `cond = true` then and only then can we optimize away a branch
*
*    Data flow provides a way to reason about the CFG and follow data through 
*    each control block along the edges and propagate each structure as needed.
*    
*    To solve this we will use a 'Reaching Definition' defined as: 
*    
*    Reaching Definition:
*    An instruction `d` defining a variable `v` *reaches* an instruction `u` 
*    iff ∃ a path in the CFG from `d` to `u`,
*    where, along that path, there are no other assignments to `v`.
*
*    Instrucytions can be described as:
*      - *use*: An instruction uses all of its arguments.
*      - *definition*: An instruction defines the variable it writes to. 
*      - *availible*: Definitions that reach a given program point are 
*                     availible there.
*      - *kill*: Any definition kills all of the currently availible 
*                definitions
*    
*    To put it simply, for every instruction that reads or writes to/from a 
*    variable we want to see what value is avalible for that use. With the 
*    above bril as an example we can see that when `a` and `c` are printed 
*    there are multiple possible values avalible. 
*
*    This can be thought of as a matrix where the x-axis and y-axis are each
*    instruction in the program and the matrix is filled with 1s and 0s which
*    denote which instruction definitions are avalible at each insturction use. 
*
*                                 Definitions
*                     + Instr₁ | Instr₂ | . . . | Instr 
*           u   Instr₁ |    0       1       ...      0
*           s   Instr₂ |    1       1       ...      1
*           e    . . . |   ...     ...      ...     ...
*           s    Instr |    0       0       ...      1
*
*  Using the above program we can mark out all the definitions and uses
*
*    ```bril                           (def|use)  : (name|avalible)
*    --------------------------+--------------------------------------
*                              |
*    @main(cond: bool) {       | <==   DEFINITION : COND1
*    .enty:                    |
*      a: int = const 47;      | <==   DEFINITION : A1
*      b: int = const 42;      | <==   DEFINITION : B1
*      br cond .left .right;   | <==   USE        : COND1, A1, B1  
*    .left:                    |     
*      b: int = const 1;       | <==   DEFINITION : B2   
*      c: int = const 5;       | <==   DEFINITION : C1      
*      jmp .end;               |                     
*    .right:                   |        
*      a: int = const 2;       | <==   DEFINITION : A2       
*      c: int = const 10;      | <==   DEFINITION : C2            
*      jmp .end;               |      
*    .end:                     |     +------------------------------------------
*      d: int = sub a c;       | <== | USE        : COND1=TRUE, A1, B2, C1 
*                              |     |            | COND1=FALSE, A2, B1, C2
*                              |     |            &
*                              |     | DEFINITION : D1
*                              |     +------------------------------------------
*      print d;                | <==   USE        : COND1=TRUE, A1, B2, C1, D1 
*                              |                  | COND1=FALSE, A2, B1, C2, D1
*    }
*    ```
*
*    In a small program like this it is possible to do this and keep track of 
*    the uses and branches by hand but in a program that contains cycles this 
*    can get very difficult as the program grows larger and larger. 
*
*    To handle general graphs and arbitrary problems including the reaching 
*    problem there is a simple recipe (framework) that one can utilize: 
*    (note this uses a 3rd party off the shelf dataflow solver™)
*
*      The Data Flow Framework
*        1. Figure out the thing you want to know at entry and exit of each 
*           block. 
*        2. Write an equation for every block relation to the entry and exit.
*        3. Add equalities according to edges in the CFG.
*        4. Solve the system of equations. 
*    
*    For each block in the program (entry, left, right, end) we need to 
*    determine the definitions availible at the the start of the block which we
*    denote as `IN<block_name>` and those availible at the end which we call 
*    `OUT<block_name>` finally for blocks that have multiple inflowing edges
*    we need a function `merge(OUT<block_1>, ... OUT<block_N>)` which computes
*    the combinations of the OUT blocks from multiple locations. 
*    
*
*    *~= FOR EACH BLOCK FILL THIS SIMPLE FORM =~*
*
*    IN<block>:
*    TRANSFER FUNCTION | IN<block> -> OUT<block> :
*    OUT<block> :
*    MERGE : 
*    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*    EX: 
*
*    IN<entry>: {COND1}
*    TRANSFER FUNCTION: {COND1} ∪ {A1, B1}
*    OUT<entry>: {COND1, A1, B1} 
*
*    IN<left>: {COND1, A1, B1}
*    TRANSFER FUNCTION: ( {COND1, A1, B1} \ {B1} ) ∪ {B2, C1}
*    OUT<left>: {COND1, A1, B2, C1}
*    
*    IN<right>: {COND1, A1, B1}
*    TRANSFER FUNCTION: ({COND1, A1, B1} \ {A1}) ∪ {A2, C2}
*    OUT<right>: {COND1, B1, A2, C2}
*    
*    IN<end>: MERGE(OUT<left>, OUT<right>)
*    TRANSFER FUNCTION: MERGE(OUT<left>, OUT<right>) ∪ {D1}
*    OUT<end>: {TRUE, A1, B2, C1, D1} | {FALSE, B1, A2, C2, D1}
*    MERGE:= {OUT<left> ∩ OUT<right>}[COND1] ? OUT<left> : OUT<right>
*   
*    NOTE: 
*
*    X \ Y is the set of elements that belong to X but not in Y read as 
*    X remove Y.
*
*    X ∪ Y is the set of elements after the unification of sets X and Y.
*    read as X union Y.
*
*    - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*    
*    Transfer: 
*    Out = ( In \ A ) ∪ (D) : In -> The avalible assignments at begining of 
*                                   block.
*                           : A  -> The overlapping assignents in the block.
*                           : D  -> The new definitions in block.
*
*    This can be written as a semmetric difference (video is wrong) 
*
*    Out = In Δ D 
*          
*          while this is technically correct it does not convey the fact that 
*          despite these being overlapping sets we are changing the varable
*          names as we go to clearly trace what definitions are being used at
*          what place.  Separating A (the kill, or overlapping, assignments)
*          and D (new definitions in the block) makes this more clear to me
*          but may be confusing to others.
*
*    *~*~*~ NAILED IT! ~*~*~*
*
*    Technically the merge function which we will be considering is just ∪ 
*    but this is for generic solvers. I think my above definition is more 
*    descriptive.  Keep in mind for the general solvers the formula for merge
*    is:
*
*    merge := ∪(∀x, OUT<x>)
*    
*    ## Worklist Algorithm For Data Flow
*    Given a cfg, an initial value init, and functions merge and transfer.
*
*    ```pseudo
*    in[entry] = init
*    out[*] = init
*
*    worklist = all blocks
*    while worklist is not empty:
*       b = pick any block from worklist
*       in[b] = merge(out[p] for every predecessor p of b)
*       out[b] = transfer(b, in[b])
*       if out[b] changed:
*           worklist += successors of b
*   ```
*   ** note : this is the "forward" version. For the "backward" version, 
*             reverse the predecessors and successors, and reverse in and out.
*
*   The workflow algorithm is especially impressive because not only is 
*   it stable no matter what order the blocks are stored it also holds the 
*   property that it will converge in programs with cycles or loops and still 
*   give the correct answer. 
*
*   ## Types of Analysis possible with Data Flow: 
*                         +----------------+-----------+-----+----------------+
*                         |     Domain     ¦ Direction ¦Merge¦  Transfer      |
*  +----------------------+----------------+-----------+-----+----------------+
*  |reaching definitions  | sets of defs   ¦ forward   ¦  ∪  ¦ In \ A ∪ Def   |   
*  +----------------------+ - - - - - - - -+- - - - - -+- - -+- - - - - - - - +
*  |live variable         | sets of vars   ¦ backward  ¦  ∪  ¦ Out \ A ∪ Used |
*  +----------------------+ - - - - - - - -+- - - - - -+- - -+- - - - - - - - +
*  |constant propagation  | valuation or T ¦           ¦     ¦                |   
*  +----------------------+ - - - - - - - -+- - - - - -+- - -+- - - - - - - - +
*  |available expressions | sets of exprs  ¦           ¦     ¦                |
*  +----------------------+ - - - - - - - -+- - - - - -+- - -+- - - - - - - - +
*  |initialized variables | sets of vars   ¦           ¦     ¦                |
*  +----------------------+ - - - - - - - -+- - - - - -+- - -+- - - - - - - - +
*  |interval analysis     | interval vals  ¦           ¦     ¦                |
*  +----------------------+----------------+-----------+-----+----------------+
*    
* 
*    Lesson was accessed from - https://vod.video.cornell.edu/media/1_72tqupsb


SCRATCH 


idea to handle loops with visit count. 

    let visit_count : HashMap<String, usize> = cfg.blocks
        .keys()
        .map(|label| (label.to_owned(), 0))
        .collect();

Within the while loop:

    let count = visit_count.entry(block).or_insert(0);
    *count += 1;
    for s in successors(&b, iblocks) {
        in_set.entry(s.to_owned()).and_modify(|set| {
            *set = merge(set.clone(), out_values.clone());
        });
        let s_count = visit_count.entry(s.to_owned()).or_insert(0);
        if *s_count == predecessors(&s, iblocks).len() {
            worklist.push(s.to_owned());
            *s_count = 0;
        }
    }

    if *count < predecessors(&b, iblocks).len() {
        worklist.push(b.to_owned());
* * * * * * * * * * * * * * * * * * * * *  * * * * * * * * * * * * * * * * * */  

