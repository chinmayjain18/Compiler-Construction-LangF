(* langfc-src/scanner-parser/parser.sig
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
 * Generic parser as used by the LangF compiler (langfc) driver.
 *)

signature PARSER =
sig
   val parse :
      ErrorStream.t *
      (Tokens.token * Source.Span.t,
       WrappedScanner.Stream.t) StringCvt.reader *
      WrappedScanner.Stream.t ->
      ParseTree.Prog.t option
end
