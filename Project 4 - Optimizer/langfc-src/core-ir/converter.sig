(* langfc-src/core-ir/converter.sig
 *
 * COPYRIGHT (c) 2011-2015 Matthew Fluet (http://www.cs.rit.edu/~mtf)
 * All rights reserved.
 *
 * Rochester Institute of Technology
 * 4005-711,CSCI-742
 * Q20112,Q20122,S20135,S20145
 *
 * COPYRIGHT (c) 2009 Matthew Fluet (http://tti-c.org/fluet)
 * All rights reserved.
 *
 * University of Chicago
 * CMSC 22610
 * Winter 2009
 *
 * Conversion from abstract syntax tree representation to core
 * intermediate representation in the LangF compiler (langfc).
 *)

signature CORE_IR_CONVERTER =
sig
   val convert : AbsSynTree.Prog.t -> CoreIR.Prog.t
end
