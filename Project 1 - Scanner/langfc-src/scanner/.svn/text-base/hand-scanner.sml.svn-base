(* langfc-src/scanner-parser/hand-scanner.sml
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
 * Convert a hand scanner to a generic scanner.
 *
 * This module converts the TextIO.instream into a stream suitable for
 * the hand scanner and uses forward-chained buffers to allow the scan
 * reader to be run over a stream multiple times.
 *)

structure HandScanner : SCANNER =
struct
   structure Stream =
      struct
         datatype t =
            T of TextIO.StreamIO.instream *
                 (Tokens.token * t) option option ref
      end

   fun scan (errStrm : ErrorStream.t, inStrm: TextIO.instream) :
            (Tokens.token, Stream.t) StringCvt.reader * Stream.t =
      let
         fun reportError msg =
            ErrorStream.addError (errStrm, msg)
         val scan =
            LangFHandScanner.scan {reportError = reportError}

         fun scanRdr (Stream.T (inStrm, resRef)) =
            case !resRef of
               NONE =>
                  (case scan TextIO.StreamIO.input1 inStrm of
                      NONE =>
                         let
                            val res = NONE
                            val () = resRef := SOME res
                         in
                            res
                         end
                    | SOME (tok, inStrm) =>
                         let
                            val scanStrm = Stream.T (inStrm, ref NONE)
                            val res = SOME (tok, scanStrm)
                            val () = resRef := SOME res
                         in
                            res
                         end)
             | SOME res => res
         val scanStrm = Stream.T (TextIO.getInstream inStrm, ref NONE)
      in
         (scanRdr, scanStrm)
      end

   val scan =
      Control.mkTracePass
      {msgPre = NONE,
       msgPost = NONE,
       passName = "hand-scan",
       pass = scan}
end
