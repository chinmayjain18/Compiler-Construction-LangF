(* langfc-src/scanner-parser/scanner.sig
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
 * Generic scanner as used by the LangF compiler (langfc) driver.
 *)

signature SCANNER =
sig
   structure Stream :
      sig
         type t
      end
   val scan :
      ErrorStream.t * TextIO.instream ->
      (Tokens.token, Stream.t) StringCvt.reader * Stream.t
end
