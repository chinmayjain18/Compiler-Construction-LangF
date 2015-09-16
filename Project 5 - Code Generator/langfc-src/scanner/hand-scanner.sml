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
                 ((Tokens.token * Source.Span.t) * t) option option ref
         fun getPos (T (inStrm, _)) = TextIO.StreamIO.filePosIn inStrm
      end

   fun scan (errStrm : ErrorStream.t, inStrm: TextIO.instream) :
            (Tokens.token * Source.Span.t, Stream.t) StringCvt.reader * Stream.t =
      let
         fun getPos inStrm =
            (TextIO.StreamIO.filePosIn inStrm) - 1
         val forwardPos = Source.Pos.forward
         fun reportErrorAt (pos, msg) =
            ErrorStream.addErrorAt (errStrm, (pos,pos), msg)
         val scan =
            LangFHandScanner.scan {getPos = getPos,
                                   forwardPos = forwardPos,
                                   reportErrorAt = reportErrorAt}

         val srcMap = ErrorStream.getSourceMap errStrm
         fun nlRdr inStrm =
            case TextIO.StreamIO.input1 inStrm of
               res as SOME (#"\n", _) =>
                  let
                     val pos = TextIO.StreamIO.filePosIn inStrm
                     val () = Source.markNewLineAt srcMap pos
                  in
                     res
                  end
             | res => res

         fun scanRdr (Stream.T (inStrm, resRef)) =
            case !resRef of
               NONE =>
                  (case scan nlRdr inStrm of
                      NONE =>
                         let
                            val res = NONE
                            val () = resRef := SOME res
                         in
                            res
                         end
                    | SOME ((tok, span), inStrm) =>
                         let
                            val scanStrm = Stream.T (inStrm, ref NONE)
                            val res = SOME ((tok, span), scanStrm)
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
