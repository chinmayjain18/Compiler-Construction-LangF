(* langfc-src/anf-ir/interpreter.sig
 *
 * COPYRIGHT (c) 2015 Matthew Fluet (http://www.cs.rit.edu/~mtf)
 * All rights reserved.
 *
 * Rochester Institute of Technology
 * CSCI-742
 * S20135,S20145
 *
 * Interpreter for A-normal form intermediate representation.
 *)

signature ANF_IR_INTERPRETER =
sig
   structure Clos :
      sig
         type t
      end
   structure Addr :
      sig
         type t
      end
   structure Value :
      sig
         datatype t =
            V_Integer of IntInf.int
          | V_String of String.string
          | V_DaCon of {dacon: AnfIR.DaCon.t, args: t list}
          | V_Clos of Clos.t
          | V_Addr of Addr.t
      end
   structure Result :
      sig
         datatype t =
            R_Value of Value.t
          | R_Fail of String.string
      end
   val interpret : AnfIR.Prog.t * string list -> Result.t
end
