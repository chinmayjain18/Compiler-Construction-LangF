(* langfc-src/anf-ir/type-checker.sig
 *
 * COPYRIGHT (c) 2015 Matthew Fluet (http://www.cs.rit.edu/~mtf)
 * All rights reserved.
 *
 * Rochester Institute of Technology
 * CSCI-742
 * S20135,S20145
 *
 * Type-checker for A-normal form intermediate representation.
 *)

signature ANF_IR_TYPE_CHECKER =
sig
   val typeCheck : AnfIR.Prog.t -> unit
end
