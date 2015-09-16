(* langfc-src/common/error-stream.sig
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
 * Common error reporting utility module in the LangF compiler
 * (langfc).
 *)

signature ERROR_STREAM =
sig

   type t

   (* make an error stream *)
   val mkErrorStream : string -> t

   val getSourceMap : t -> Source.Map.t

   (* add an error message to the error stream *)
   val addError : t * string -> unit
   val addErrorAt : t * Source.Span.t * string -> unit

   (* add a warning message to the error stream *)
   val addWarning : t * string -> unit
   val addWarningAt : t * Source.Span.t * string -> unit

   (* print messages to an output stream *)
   val report : t -> unit

   (* any messages in the error stream *)
   val anyMessages : t -> bool

end
