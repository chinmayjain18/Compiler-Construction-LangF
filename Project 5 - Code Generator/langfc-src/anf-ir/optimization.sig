(* langfc-src/anf-ir/optimization.sig
 *
 * COPYRIGHT (c) 2015 Matthew Fluet (http://www.cs.rit.edu/~mtf)
 * All rights reserved.
 *
 * Rochester Institute of Technology
 * CSCI-742
 * S20135,S20145
 *
 * An optimization pass for A-normal form intermediate representation.
 *
 *)

signature ANF_IR_OPTIMIZATION =
sig
   val xform : AnfIR.Prog.t -> AnfIR.Prog.t
end
