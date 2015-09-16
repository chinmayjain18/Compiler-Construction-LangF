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
 * 'main' function for the LangF compiler (langfc).
 *)

structure LangFComp :
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
         fun doScan (errStrm, inStrm) =
            let
               val ((scanRdr, scanStrm), stop) =
                  WrappedScanner.scan (errStrm, inStrm)
               val () = TextIO.closeIn inStrm
            in
               checkErrStrm (errStrm, fn () =>
                             if stop
                                then NONE
                             else SOME ())
            end
      in
         doScan (errStrm, inStrm)
      end
   val doFrontEnd =
      Control.mkTracePass
      {msgPre = NONE, msgPost = NONE,
       pass = doFrontEnd, passName = "front-end"}

   fun doCompile file : OS.Process.status =
      let
         val {base, ...} = OS.Path.splitBaseExt file
         val () = Controls.set (Control.baseName, base)

         val inStrm = TextIO.openIn file
         val errStrm = ErrorStream.mkErrorStream file
      in
         case doFrontEnd (errStrm, inStrm) of
            NONE => OS.Process.failure
          | SOME () => OS.Process.success
      end
   val doCompile =
      Control.mkTracePass
      {msgPre = NONE,
       msgPost = NONE,
       pass = doCompile,
       passName = "langfc"}


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


   val versionMsg = "langfc (CSCI-742: S20145)\n"
   val usageMsg = "\
      \usage: langfc [options] <file>.lgf\n\
      \  options:\n\
      \    -C<control>=<v>  (set named control)\n\
      \    -H               (produce help listing)\n\
      \    -version         (show version)\n\
      \"

   fun main (name, args) =
      let
         val () = Controls.set (Control.verbose, true)

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
         val file =
            case args of
               [file] => file
             | _ => usage ()
      in
         doCompile file
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
