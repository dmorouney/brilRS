
use::std::collections::{HashMap, HashSet};
use crate::bril::{predecessors};
use crate::bril::structure::*;

macro_rules! debugln {
    ($fmt:expr $(, $($arg:tt)+)?) => {
        if cfg!(debug_assertions) {
            println!(concat!("[{}:{}] ", $fmt), file!(), line!() $(, $($arg)+)?);
        }
    };
}

pub fn dominators
    (cfg: &CFGFunction) -> HashMap<String, HashSet<String>> {
    debugln!("CFG EDGES = {:?}", cfg.edges);
    let order = {
        let mut order = cfg.order.clone();
        order.extend(["EXIT".to_string()]);
        order 
    };
    let entry_set : HashSet<String> = [String::from("ENTRY")].into_iter().collect();
    let mut doms : HashMap<String, HashSet<String>> = order
        .iter()
        .map(|label| (label.clone(), entry_set.clone()))
        .collect();
    let mut changed : bool = true;
    while changed {
        changed = false;
        for block_label in &order {
            debugln!("visit from {}", block_label);
            let self_set : HashSet<String> = [block_label.clone()]
                .iter()
                .cloned()
                .collect();
            debugln!("\tself-set = {:?}", self_set);
            debugln!("\tpredecessors = {:?}", predecessors(block_label, cfg));
            let pred_doms : Vec<HashSet<String>> = predecessors(block_label, cfg)
                .iter()
                .filter_map(|d| doms.get(d) )
                .cloned()
                .collect();
            debugln!("\tpred_doms = {:?}", pred_doms);
            let new_doms = if pred_doms.is_empty() {
                self_set.clone()
            } else {
                pred_doms.iter().skip(1)
                    .fold(pred_doms[0].clone(), 
                        |acc, set| acc.intersection(set)
                            .cloned()
                            .collect())
                    .union(&self_set)
                    .cloned()
                    .collect()
            };
            debugln!("\tnew_doms = {:?}", new_doms);
            if let Some(ds) = doms.get(block_label) {
                if *ds != new_doms {
                    debugln!("\t\tupdating doms with\n\t\told = {:?}", ds);
                    doms.insert(block_label.clone(), new_doms.clone());
                    changed = true;
                }
            }
        }
    }
    doms
}


 /* * * * * n o t e s * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *  # Global analysis
 *
 *  ## Definitions:
 *      successors
 *      predecessors
 *      path 
 *      dominators: A cfg vertex `A` dominates vertex `B` iff all paths from the 
 *                  entry of `B` include `A`.
 *                  Eg. Given the following control flow graph on the left we 
 *                      show the list of dominators at each vertex to the right
 *                      
 *                     +-------+               Dominators:
 *                     | entry |               entry : {entry}      
 *                     +---+---+               loop  : {entry, loop} 
 *                         🠗                   body  : {entry, loop, body}
 *                     +--------+              then  : {entry, loop, body, then}
 *                     |  loop  | <---------+  endif : {entry, loop, body, endif} 
 *                     +--------+           |  exit  : {entry, loop, exit}
 *                    ↙          ↘          |
 *               +--------+    +------+     |
 *               |  body  |    | exit |     |
 *               +---+----+    +------+     |
 *                   ⭣     \                |
 *               +--------+ \    +-------+  |
 *               |  then  |  +-->| endif |--+
 *               +---+----+      +-------+
 *                   |               ⭡
 *                   +---------------+
 *
 *      `A` strictly dominates `B` iff: `A` dominates `B` and `A != B`.
 *      
 *      `A` immediately dominates `B` iff: A dominates `B` and `A` does not 
 *      strictly dominate any other node that strictly dominates `B`. 
 *      
 *      `A`'s domination frontier contains `B` if `A` does not dominate `B` but 
 *      `A` dominates a predecessor of `B`.
 *
 *      `A` post-domiates `B` iff all paths from `B` to the exit include `A`
 *
 *  
 *                     +-------+               Post- Dominators:
 *                     | entry |               entry : {entry, loop, exit }      
 *                     +---+---+               loop  : {loop, exit} 
 *                         🠗                   body  : {body, then, endif, loop, exit} 
 *                     +--------+              then  : {then, endif, loop, exit}
 *                     |  loop  | <---------+  endif : {loop, exit}
 *                     +--------+           |  exit  : {exit}
 *                    ↙          ↘          |
 *               +--------+    +------+     |
 *               |  body  |    | exit |     |
 *               +---+----+    +------+     |
 *                   ⭣     \                |
 *               +--------+ \    +-------+  |
 *               |  then  |  +-->| endif |--+
 *               +---+----+      +-------+
 *                   |               ⭡
 *                   +---------------+
 *
 *  Pseudo code for implementing an algorith to find the dominators in a given
 *  CFG. 
 *
 *  ```pseudo
 *  dom = { map of vertices to serts of verties }
 *  while dom is still changing: 
 *  for vertex in CFG: 
 *      dom[vertex] = self ∪ ( ∏{ dom[p] for p in predessors[p] } )
 *
 *      to finish this you will need some set of verties 
 *      take a few moments and think of an algorithm using 
 *      set operations which takes the above scaffolding and 
 *      implements it figuring out the dominators of each 
 *      vertex and updating them until the solution converges
 *      and no dominators are changed on a pass over the CFG.
*      ⋂
 *
 *      ~* ~* pause @ 13:58 *~ *~ 
 *
 * * * * * n o t e s * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
