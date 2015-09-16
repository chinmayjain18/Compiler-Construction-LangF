(* langfc-src/common/control.sml
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
 * Global controls for the LangF compiler (langfc).
 *)

structure Control :> CONTROL =
struct

   val topRegistry = ControlRegistry.new {help = "controls"}

   val verbose : bool Controls.control =
      Controls.genControl
      {name = "verbose",
       pri = [],
       obscurity = 0,
       help = "controls verbosity of diagnostic messages",
       default = true}
   val () =
      ControlRegistry.register topRegistry
      {ctl = Controls.stringControl ControlUtil.Cvt.bool verbose,
       envName = NONE}

    val debugObscurity = 2

    val baseName : string Controls.control =
       Controls.genControl
       {name = "baseName",
        pri = [],
        obscurity = debugObscurity + 1,
        help = "",
        default = ""}

   local
      val indent = ref 0
      val verboseCtl = verbose
      fun push () = indent := !indent + 4
      fun pop () = indent := !indent - 4
      fun pr s =
         (TextIO.print (CharVector.tabulate (!indent, fn _ => #" "));
          TextIO.print s;
          TextIO.print "\n")
      fun openOut (msg, fileName) =
         let
            val outStrm = TextIO.openOut fileName
            val () =
               if Controls.get verboseCtl
                  then pr (concat ["dumping ", msg, " to ", fileName])
               else ()
         in
            outStrm
         end
   in
      fun ('pre, 'post) mkTracePass {msgPre: ('pre -> string) option,
                                     msgPost: ('post -> string) option,
                                     passName: string,
                                     pass: 'pre -> 'post
                                     } : 'pre -> 'post =
         fn pre =>
         let
            val msg = Controls.get verboseCtl
            val () =
               if msg
                  then pr (concat [passName, " starting",
                                   case msgPre of
                                      NONE => ""
                                    | SOME msgPre =>
                                         concat [", ", msgPre pre]])
               else ()
            val () = push ()
            val post =
               (pass pre)
               handle exn =>
                  let
                     val () = pop ()
                     val () = pr (concat [passName, " raised exception ",
                                          General.exnName exn,
                                          " [", General.exnMessage exn, "]"])
                  in
                     raise exn
                  end
            val () = pop ()
            val () =
               if msg
                  then pr (concat [passName,  " finished",
                                   case msgPost of
                                      NONE => ""
                                    | SOME msgPost =>
                                         concat [", ", msgPost post]])
               else ()
         in
            post
         end

      fun ('pre, 'post) mkKeepPass {keepPre : {output : TextIO.outstream * 'pre -> unit,
                                               ext : string} option,
                                    keepPost : {output : TextIO.outstream * 'post -> unit,
                                                ext: string} option,
                                    passName : string,
                                    pass : 'pre -> 'post
                                    } : 'pre -> 'post =
         fn pre =>
         let
            val fileName = concat [ Controls.get baseName, ".", passName ]
            val () =
               case keepPre of
                  NONE => ()
                | SOME {output, ext} =>
                     let
                        val outStrm =
                           openOut (concat [passName, " input"],
                                    concat [fileName, ".", ext])
                        val () = output (outStrm, pre)
                        val () = TextIO.closeOut outStrm
                     in
                        ()
                     end
            val post = pass pre
            val () =
               case keepPost of
                  NONE => ()
                | SOME {output, ext} =>
                     let
                        val outStrm =
                           openOut (concat [passName, " output"],
                                    concat [fileName, ".", ext])
                        val () = output (outStrm, post)
                        val () = TextIO.closeOut outStrm
                     in
                        ()
                     end
         in
            post
         end

      fun mkKeepPassSimple {keep: {output: TextIO.outstream * 'a -> unit,
                                   ext: string} option,
                            passName: string,
                            pass: 'a -> 'a
                            } : 'a -> 'a =
         let
            val (keepPre, keepPost) =
               case keep of
                  NONE => (NONE, NONE)
                | SOME {output, ext} =>
                     (SOME {output = output,
                            ext = "pre." ^ ext},
                      SOME {output = output,
                            ext = "post." ^ ext})
         in
            mkKeepPass {keepPre = keepPre,
                        keepPost = keepPost,
                        passName = passName,
                        pass = pass}
         end

      fun ('pre, 'post) mkKeepCtlPass {keepPre : {output : TextIO.outstream * 'pre -> unit,
                                                  ext : string} option,
                                       keepPost : {output : TextIO.outstream * 'post -> unit,
                                                   ext: string} option,
                                       passName : string,
                                       pass : 'pre -> 'post
                                       } : 'pre -> 'post =
         let
            val keepPassCtl =
               Controls.genControl
               {name = "keep-" ^ passName,
                pri = [],
                obscurity = 1,
                help = concat["keep ",  passName, " pass"],
                default = false}
            val _ =
               ControlRegistry.register
               topRegistry
               {ctl = Controls.stringControl ControlUtil.Cvt.bool keepPassCtl,
                envName = NONE}
         in
            fn pre =>
            if Controls.get keepPassCtl
               then mkKeepPass {keepPre = keepPre,
                                keepPost = keepPost,
                                passName = passName,
                                pass = pass}
                               pre
            else pass pre
         end

      fun mkKeepCtlPassSimple {keep: {output: TextIO.outstream * 'a -> unit,
                                      ext: string} option,
                               passName: string,
                               pass: 'a -> 'a
                               } : 'a -> 'a =
         let
            val (keepPre, keepPost) =
               case keep of
                  NONE => (NONE, NONE)
                | SOME {output, ext} =>
                     (SOME {output = output,
                            ext = "pre." ^ ext},
                      SOME {output = output,
                            ext = "post." ^ ext})
         in
            mkKeepCtlPass {keepPre = keepPre,
                           keepPost = keepPost,
                           passName = passName,
                           pass = pass}
         end
   end

   fun mkStopCtlPass {passName: string,
                      pass: 'pre -> 'post
                      } : 'pre -> 'post * bool =
      let
         val stopPassCtl =
            Controls.genControl
            {name = "stop-" ^ passName,
             pri = [],
             obscurity = 1,
             help = concat["stop after ",  passName, " pass"],
             default = false}
         val _ =
            ControlRegistry.register
            topRegistry
            {ctl = Controls.stringControl ControlUtil.Cvt.bool stopPassCtl,
             envName = NONE}
      in
         fn pre =>
         (pass pre, Controls.get stopPassCtl)
      end

   fun report outStrm level =
      let
         fun output s = TextIO.output (outStrm, s)
         fun getarg {ctl, info} = Controls.name ctl
         fun getval {ctl, info} =
            concat ["(", #help (Controls.info ctl),
                    "; ", Controls.get ctl, ")"]
         fun walk indent (ControlRegistry.RTree rt) =
            let
               val sp = CharVector.tabulate (indent, fn _ => #" ")
               val {help, ctls, subregs, path} = rt
               val ctls = ListMergeSort.sort (fn (ci1, ci2) => getarg ci1 > getarg ci2) ctls
               fun one ci =
                  let
                     val arg = concat (foldr (fn (s, r) => s :: "." :: r)
                                             [getarg ci]
                                             path)
                  in
                     output (concat [sp,"  ",arg," : ",getval ci,"\n"])
                  end
            in
               (output (concat [sp, help, ":\n"])
                ; app one ctls
                ; app (walk (indent + 2)) subregs)
            end
      in
         walk 2 (ControlRegistry.controls (topRegistry, level))
      end

end
