(* langfc-src/core-ir/optimization.sig
 *
 * COPYRIGHT (c) 2015 Matthew Fluet (http://www.cs.rit.edu/~mtf)
 * All rights reserved.
 *
 * Rochester Institute of Technology
 * CSCI-742
 * S20135,S20145
 *
 * An optimization pass for core intermediate representation.
 *)

signature CORE_IR_OPTIMIZATION =
sig
   val xform : CoreIR.Prog.t -> CoreIR.Prog.t
end
