(* langfc-src/vm-codegen/instructions.sig
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
 * VM code instructions in the LangF compiler (langfc).
 *)

signature INSTRUCTION =
sig

   type t

   val layout : t -> Layout.t
   val toString : t -> string

   val add : t
   val sub : t
   val mul : t
   val div : t
   val mod : t
   val neg : t
   val equ : t
   val less : t
   val lesseq : t
   val not : t
   val boxed : t
   val alloc : int -> t
   val repalloc : t
   val explode : t
   val length : t
   val select : int -> t
   val index : t
   val update : t
   val int : int -> t
   val integer : IntInf.int -> t
   val literal : int -> t
   val label : Label.t -> t
   val swap : t
   val swapn : int -> t
   val pop : t
   val popn : int -> t
   val dup : t
   val pushn : int -> t
   val loadlocal : int -> t
   val storelocal : int -> t
   val loadglobal : int -> t
   val pushep : t
   val popep : t
   val jmp : Label.t -> t
   val jmpif : Label.t -> t
   val call : t
   val tailcall : t
   val entry : int -> t
   val ret : t
   val ccall : int -> t
   val nop : t
   val halt : t

   structure Info :
      sig
         type t = {opc: word,
                   name: string,
                   minSz: int,
                   maxSz: int}
      end
   structure Param :
      sig
         datatype t =
            NoParam
          | Int of {signed: bool} * IntInf.int
          | Label of Label.t
      end
   val dest : t -> {info: Info.t, param: Param.t}

end
