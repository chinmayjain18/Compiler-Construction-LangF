(* langfc-src/scanner-parser/ulex-scanner.sml
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
 * Convert an ml-ulex scanner to a generic scanner.
 *
 * This module converts the TextIO.instream into a stream suitable for
 * the ml-ulex scanner and converts the ml-ulex output to a scan
 * reader.
 *)

structure ULexScanner : SCANNER =
struct
   structure Stream =
      struct
         type t = LangFULexScanner.strm
         val getPos = LangFULexScanner.getPos
      end

   val scan : ErrorStream.t * TextIO.instream ->
              (Tokens.token * Source.Span.t, Stream.t) StringCvt.reader * Stream.t =
      fn (errStrm, inStrm) =>
      let
         fun reportErrorAt (pos, msg) =
            ErrorStream.addErrorAt (errStrm, (pos,pos), msg)

         val ulexFn =
            LangFULexScanner.lex (ErrorStream.getSourceMap errStrm)
                                 {reportErrorAt = reportErrorAt}

         val ulexStrm =
            LangFULexScanner.streamifyInstream inStrm

         fun ulexRdr ulexStrm =
            case ulexFn ulexStrm of
               (NONE, _, _) => NONE
             | (SOME tok, (left,right), ulexStrm) =>
                  SOME ((tok, (left-1,right-1)), ulexStrm)
      in
         (ulexRdr, ulexStrm)
      end

   val scan =
      Control.mkTracePass
      {msgPre = NONE,
       msgPost = NONE,
       passName = "ulex-scan",
       pass = scan}
end
