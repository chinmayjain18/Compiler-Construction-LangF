(* langfc-src/common/source.sig
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
 * Source positions, spans, locations, and maps in the LangF compiler
 * (langfc).
 *
 * Source positions, spans, locations, and maps are identical to
 * AntlrStreamPos.{pos,span,sourceloc,sourcemap}, but rebound here to
 * less biased names.
 *)

signature SOURCE =
sig
   structure Map :
      sig
         type t = AntlrStreamPos.sourcemap
         val new : string -> t
      end
   structure Loc :
      sig
         type t = {fileName: string option, lineNo: int, colNo: int}
      end
   structure Pos :
      sig
         type t = Position.int
         val bogus : t
         val eof : t
         val compare : t * t -> order
         val forward : t * int -> t
         val toString: Map.t -> t -> string
      end
   structure Span :
      sig
         type t = Pos.t * Pos.t
         val bogus: t
         val eof: t
         val compare : t * t -> order
         val toString: Map.t -> t -> string
      end
   val markNewLineAt : Map.t -> Pos.t -> unit
end
