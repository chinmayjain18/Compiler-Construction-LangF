(* langfc-src/vm-codegen/code-stream.sig
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
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * VM code streams in the LangF compiler (langfc).
 *)

signature CODE_STREAM =
sig
   type t

   (* create a new code stream *)
   val new : unit -> t

   (* emit an instruction to the stream *)
   val emit : t -> Instruction.t -> unit

   (* bind the label to the current location *)
   val defineLabel : t -> Label.t -> unit

   (* map a string literal to an index in the literal table *)
   val string : (t * string) -> int

   (* map a C function's name to an index in the C-function table *)
   val c_function : (t * string) -> int

   (* dump the contents of the code stream (in a readable format) to the specified output stream *)
   val output : (TextIO.outstream * t) -> unit

   (* assemble the code and write the object file *)
   val finish : (BinIO.outstream * t) -> unit

end
