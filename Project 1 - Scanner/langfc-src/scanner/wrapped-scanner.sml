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
 * This module calls the hand-written scanner, runs the scanner over
 * the entire input (to report errors), installs the 'keep-scan'
 * control that saves the token stream to a file, and installs the
 * 'stop-scan' control that stops compilation after scanning.
 *)

structure WrappedScanner : WRAPPED_SCANNER =
struct
   structure Stream =
      struct
         type t = HandScanner.Stream.t
      end

   fun handScan (errStrm, inStrm) =
      let
         val (handRdr, handStrm) = HandScanner.scan (errStrm, inStrm)
      in
         (handRdr, handStrm)
      end

   fun scan (errStrm, inStrm) =
      handScan (errStrm, inStrm)

   (* Run the scan reader over the entire stream.
    * allows us to report any errors in scanning.
    *)
   val scan = fn (errStrm, inStrm) =>
      let
         val (scanRdr, scanStrm) = scan (errStrm, inStrm)
         fun loop scanStrm =
            case scanRdr scanStrm of
               NONE => ()
             | SOME (_, scanStrm) => loop scanStrm
         val () = loop scanStrm
      in
         (scanRdr, scanStrm)
      end

   local
      fun output (outStrm, (scanRdr, scanStrm)) =
         let
            fun loop scanStrm =
               case scanRdr scanStrm of
                  NONE => ()
                | SOME (tok, scanStrm) =>
                     let
                        val () =
                           TextIO.output
                           (outStrm,
                            concat
                            [Tokens.toString tok,
                             "\n"])
                     in
                        loop scanStrm
                     end
         in
            loop scanStrm
         end
   in
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
