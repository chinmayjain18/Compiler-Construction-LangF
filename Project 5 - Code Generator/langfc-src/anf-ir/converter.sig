(* langfc-src/anf-ir/converter.sig
 *
 * COPYRIGHT (c) 2015 Matthew Fluet (http://www.cs.rit.edu/~mtf)
 * All rights reserved.
 *
 * Rochester Institute of Technology
 * CSCI-742
 * S20135,S20145
 *
 * Conversion from core intermediate representation to A-normal form
 * intermediate representation in the LangF compiler (langfc).
 *)

signature ANF_IR_CONVERTER =
sig
   val convert : CoreIR.Prog.t -> AnfIR.Prog.t
end
