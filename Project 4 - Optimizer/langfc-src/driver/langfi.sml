(* langfc-src/driver/langfc.sml
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
 * 'main' function for the LangF interpreter (langfi).
 *)

structure LangFInterp :
sig
   val main : string * string list -> OS.Process.status
end =
struct

   fun errMsg s = TextIO.output (TextIO.stdErr, s)

   fun checkErrStrm (errStrm, th) =
      if ErrorStream.anyMessages errStrm
         then let val () = ErrorStream.report errStrm
              in NONE
              end
      else th ()

   fun doFrontEnd (errStrm, inStrm) =
      let
         fun doTypeCheck (errStrm, ptProg) =
            let
               val (astProg, stop) =
                  WrappedTypeChecker.typeCheck
                  (errStrm, ptProg)
            in
               checkErrStrm (errStrm, fn () =>
                             let
                                val astProg = valOf astProg
                             in
                                if stop
                                   then NONE
                                else SOME astProg
                             end)
            end

         fun doParse (errStrm, inStrm, scanRdr, scanStrm) =
            let
               val (ptProg, stop) =
                  WrappedParser.parse (errStrm, scanRdr, scanStrm)
               val () = TextIO.closeIn inStrm
            in
               checkErrStrm (errStrm, fn () =>
                             let
                                val ptProg = valOf ptProg
                             in
                                if stop
                                   then NONE
                                else doTypeCheck (errStrm, ptProg)
                             end)
            end

         fun doScan (errStrm, inStrm) =
            let
               val ((scanRdr, scanStrm), stop) =
                  WrappedScanner.scan (errStrm, inStrm)
            in
               checkErrStrm (errStrm, fn () =>
                             if stop
                                then NONE
                             else doParse (errStrm, inStrm, scanRdr, scanStrm))
            end
      in
         doScan (errStrm, inStrm)
      end
   val doFrontEnd =
      Control.mkTracePass
      {msgPre = NONE, msgPost = NONE,
       pass = doFrontEnd, passName = "front-end"}
   val doFrontEnd =
      Control.mkStopCtlPass
      {passName = "front-end",
       pass = doFrontEnd}

   fun doMiddleEnd astProg =
      let
         val coreProg = CoreIRConverter.convert astProg
         val () = CoreIRTypeChecker.typeCheck coreProg
         val coreProg = CoreIROptimizer.xform coreProg
         val () = CoreIRTypeChecker.typeCheck coreProg
         val anfProg = AnfIRConverter.convert coreProg
         val () = AnfIRTypeChecker.typeCheck anfProg
         val anfProg = AnfIROptimizer.xform anfProg
         val () = AnfIRTypeChecker.typeCheck anfProg
      in
         anfProg
      end
   val doMiddleEnd =
      Control.mkTracePass
      {msgPre = NONE, msgPost = NONE,
       pass = doMiddleEnd, passName = "middle-end"}
   val doMiddleEnd =
      Control.mkStopCtlPass
      {passName = "middle-end",
       pass = doMiddleEnd}

   fun doInterpret (file, args) : OS.Process.status =
      let
         val {base, ...} = OS.Path.splitBaseExt file
         val () = Controls.set (Control.baseName, base)

         val inStrm = TextIO.openIn file
         val errStrm = ErrorStream.mkErrorStream file
      in
         case doFrontEnd (errStrm, inStrm) of
            (NONE, _) => OS.Process.failure
          | (SOME astProg, true) => OS.Process.success
          | (SOME astProg, false) =>
               (case doMiddleEnd astProg of
                   (anfProg, true) => OS.Process.success
                 | (anfProg, false) =>
                      (case AnfIRInterpreter.interpret (anfProg, file::args) of
                          AnfIRInterpreter.Result.R_Value _ => OS.Process.success
                        | AnfIRInterpreter.Result.R_Fail _ => OS.Process.failure))
      end
   val doInterpret =
      Control.mkTracePass
      {msgPre = NONE,
       msgPost = NONE,
       pass = doInterpret,
       passName = "langfi"}


   fun quit b =
      OS.Process.exit
      (if b then OS.Process.success else OS.Process.failure)

   fun bad msg =
      let
         val () = errMsg msg
         val () = errMsg "!* try '-H' for help\n"
      in
         quit false
      end

   fun processControl arg =
      let
         val spec = Substring.extract (arg, 2, NONE)
         val (name, value) =
            Substring.splitl (fn c => c <> #"=") spec
         val name = Substring.string name
         val names = String.fields (fn c => c = #".") name
         val value = if Substring.size value > 0
                        then Substring.string (Substring.slice (value, 1, NONE))
                     else ""
      in
         if name = "" orelse value = ""
            then bad (concat ["!* ill-formed -C option: '", arg, "'\n"])
         else (case ControlRegistry.control Control.topRegistry names of
                  NONE => bad (concat ["!* unknown control: ", name, "\n"])
                | SOME sctl =>
                     (Controls.set (sctl, value)
                      handle Controls.ValueSyntax vse =>
                         bad (concat ["!* unable to parse value '",
                                      value, "' for control ", name, " : ", #tyName vse,
                                      "\n"])))
      end


   val versionMsg = "langfi (CSCI-742: S20145)\n"
   val usageMsg = "\
      \usage: langfi [options] <file>.lgf [args]\n\
      \  options:\n\
      \    -C<control>=<v>  (set named control)\n\
      \    -H               (produce help listing)\n\
      \    -version         (show version)\n\
      \"

   fun main (name, args) =
      let
         val () = Controls.set (Control.verbose, false)

         fun version () = (errMsg versionMsg; quit true)
         fun usage () = (errMsg usageMsg; quit false)
         fun help () =
            let
               val () = errMsg usageMsg
               val () = Control.report TextIO.stdErr NONE
            in
               quit true
            end

         fun processOptions args =
            (case args of
                arg :: args =>
                   if String.size arg > 0 andalso String.sub (arg, 0) = #"-"
                      then processOption (arg, args)
                   else arg :: args
              | _ => usage ())

         and processOption (arg, args) =
            if String.isPrefix "-C" arg
               then (processControl arg; processOptions args)
            else if arg = "-H"
               then help ()
            else if arg = "-version"
               then version ()
            else bad (concat ["!* bad-option: '", arg, "'\n"])

         val args = processOptions args
         val (file, args) =
            case args of
               file::args => (file, args)
             | _ => usage ()
      in
         doInterpret (file, args)
         handle exn =>
            let
               val _ =
                  errMsg (concat ["uncaught exception ",
                                  General.exnName exn,
                                  " [", General.exnMessage exn, "]\n"])
               val _ =
                  List.app (fn s => errMsg (concat ["  raised at ", s, "\n"]))
                           (SMLofNJ.exnHistory exn)
            in
               quit false
            end
      end
end
