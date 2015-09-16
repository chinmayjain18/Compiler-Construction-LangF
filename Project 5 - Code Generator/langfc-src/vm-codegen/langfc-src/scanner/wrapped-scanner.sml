(* langfc-src/scanner-parser/wrapped-scanner.sml
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
 * Scanner as used by the LangF compiler (langfc) driver.
 *
 * This module calls a generic scanner, installs the 'scanner' control
 * that selects between the ML-ULex and the hand-written scanner, runs
 * the scanner over the entire input (to report errors), installs
 * the 'keep-scan' control that saves the token stream to a file, and
 * installs the 'stop-scan' control that stops compilation after
 * scanning.
 *)

structure WrappedScanner : WRAPPED_SCANNER =
struct
   structure ScannerCtl =
      struct
         datatype t = Hand | ULex
         val fromString =
            fn "hand" => SOME Hand
             | "ulex" => SOME ULex
             | _ => NONE
         val toString =
            fn Hand => "hand"
             | ULex => "ulex"
         val value_cvt =
            {tyName = "scanner-ctl",
             fromString = fromString,
             toString = toString}
      end

   val scannerCtl : ScannerCtl.t Controls.control =
      Controls.genControl
      {name = "scanner",
       pri = [],
       obscurity = 1,
       help = "select between hand-written and ml-ulex scanner",
       default = ScannerCtl.ULex}
   val () =
      ControlRegistry.register Control.topRegistry
      {ctl = Controls.stringControl ScannerCtl.value_cvt scannerCtl,
       envName = NONE}

   structure Stream =
      struct
         type 'strm u = (Tokens.token * Source.Span.t, 'strm) StringCvt.reader * 'strm
         datatype t =
            Hand of HandScanner.Stream.t u
          | ULex of ULexScanner.Stream.t u
         fun getPos strm =
            case strm of
               Hand (_, handStrm) => HandScanner.Stream.getPos handStrm
             | ULex (_, ulexStrm) => ULexScanner.Stream.getPos ulexStrm
         fun scanFn strm =
            case strm of
               Hand (handRdr, handStrm) =>
                  (case handRdr handStrm of
                      NONE => NONE
                    | SOME ((tok, span), handStrm) =>
                         SOME ((tok, span), Hand (handRdr, handStrm)))
             | ULex (ulexRdr, ulexStrm) =>
                  (case ulexRdr ulexStrm of
                      NONE => NONE
                    | SOME ((tok, span), ulexStrm) =>
                         SOME ((tok, span), ULex (ulexRdr, ulexStrm)))
      end

   fun handScan (errStrm, inStrm) =
      let
         val (handRdr, handStrm) = HandScanner.scan (errStrm, inStrm)
      in
         (Stream.scanFn, Stream.Hand (handRdr, handStrm))
      end
   fun ulexScan (errStrm, inStrm) =
      let
         val (ulexRdr, ulexStrm) = ULexScanner.scan (errStrm, inStrm)
      in
         (Stream.scanFn, Stream.ULex (ulexRdr, ulexStrm))
      end

   fun scan (errStrm, inStrm) =
      case Controls.get scannerCtl of
         ScannerCtl.Hand => handScan (errStrm, inStrm)
       | ScannerCtl.ULex => ulexScan (errStrm, inStrm)

   (* Run the scan reader over the entire stream.
    * allows us to report any errors in scanning.
    *)
   val scan = fn (errStrm, inStrm) =>
      let
         val (scanRdr, scanStrm) = scan (errStrm, inStrm)
         fun loop scanStrm =
            case scanRdr scanStrm of
               NONE => ()
             | SOME ((_, _), scanStrm) => loop scanStrm
         val () = loop scanStrm
      in
         (scanRdr, scanStrm)
      end

   local
      val srcMap = ref (Source.Map.new "")
      fun output (outStrm, (scanRdr, scanStrm)) =
         let
            fun loop scanStrm =
               case scanRdr scanStrm of
                  NONE => ()
                | SOME ((tok, span), scanStrm) =>
                     let
                        val () =
                           TextIO.output
                           (outStrm,
                            concat
                            [Tokens.toString tok,
                             " (* ",
                             Source.Span.toString (!srcMap) span,
                             " *)\n"])
                     in
                        loop scanStrm
                     end
         in
            loop scanStrm
         end
   in
      val scan = fn (errStrm, inStrm) =>
         let
            val () = srcMap := ErrorStream.getSourceMap errStrm
         in
            scan (errStrm, inStrm)
         end
      val scan =
         Control.mkKeepCtlPass
         {keepPre = NONE,
          keepPost = SOME {output = output,
                           ext = "toks"},
          passName = "scan",
          pass = scan}
   end

   val scan =
      Control.mkTracePass
      {msgPre = NONE,
       msgPost = NONE,
       passName = "scan",
       pass = scan}

   val scan =
      Control.mkStopCtlPass
      {passName = "scan",
       pass = scan}
end
