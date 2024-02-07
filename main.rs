use std::io::{self, Read};
use std::env;
use std::error::Error;
mod bril;
use bril::structure::*;
use bril::{print_bril,print_cfg,bril_to_cfg};
use bril::lvn::local_value_numbering;
use bril::dce::optimize_blocks;
use bril::data_flow::*;

use crate::global::dominators;
mod global;

fn main() ->  Result<(), Box<dyn Error>> {
    let mut buffer = String::new();
    let mut handle = io::stdin().lock();
    handle.read_to_string(&mut buffer)?;
    let  bril : BrilProgram = serde_json::from_str(&buffer)?;
    let cfgs: Vec<CFGFunction> = bril.functions
                                     .iter()
                                     .map(|func| 
                                          bril_to_cfg(func.to_owned()))
                                     .collect(); 
    assert!(!cfgs.is_empty(), "Something went wrong. No CFG created for data.");
    let mut argv : Vec<String> = Vec::new();
    for argument in env::args() { argv.push(argument.to_owned()); }
    assert!(argv.len() > 1, "Use with: brilrs <cfg|graph|dce|lvn|df>");
    for cfg in cfgs { 
        match argv[1].as_str() {
            "bril" => print_bril(&cfg),
            "cfg" => println!("{:?}", &cfg),
            "graph" => print_cfg(cfg),
            "dce" => {
                let mut mcfg = cfg.to_owned();
                optimize_blocks(&mut mcfg);
                print_bril(&mcfg);
            },
            "lvn" => {
                assert!(argv.len() == 3, "Use with: brilrs lvn <true|false>");
                let lvn : CFGFunction = match argv[2].as_str() {
                    "true" => local_value_numbering(&cfg, true),
                    "false" => local_value_numbering(&cfg, false),
                    _ => panic!("Use with: brilrs lvn <true|false>"),
                };
                //optimize_blocks(&mut lvn);
                print_bril(&lvn);
            },
            "dom" => {
                let d_map = dominators(&cfg);
                println!("dominators:");
                for b in cfg.order {  
                    println!("\t{b}: {:?}", d_map[&b]);
                }
                    println!("\tEXIT: {:?}", d_map["EXIT"]);
            },
            "df" => {
                assert!(argv.len() == 3, "Use with: brilrs df <rdef|..>");
                match argv[2].as_str() {
                    "rdef" => {
                        let rd = reaching_definitions(&cfg);
                        for label in rd.labels {
                            let in_vec : Vec<String> = rd.ins
                                .get(&label)
                                .unwrap()
                                .clone()
                                .into_iter()
                                .collect();
                            println!("In[{}]: {}", label, in_vec.join(", "));
                            let out_vec : Vec<String> = rd.outs
                                .get(&label)
                                .unwrap()
                                .clone()
                                .into_iter()
                                .collect();
                            println!("Out[{}]: {}", label, out_vec.join(", "));
                            println!("---------------------------");
                        }  
                    },
                    _ => println!("Use with: brilrs df <rdef>"),
                }
            }
            _ => println!("Use with: brilrs <cfg|graph|dce|lvn|df>"),
        }
    }
    Ok(())
}

/*
*   Helpful functions to remeber: 
*
*   SERIALIZE : c: String = serde_json::to_string(&struct).unwrap());
*   DESERIALIZE : s: Struct = serde_json::from_str(&buffer)?;
*/
