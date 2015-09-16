(* langfc-src/anf-ir/optimizer.sml
 *
 * COPYRIGHT (c) 2015 Matthew Fluet (http://www.cs.rit.edu/~mtf)
 * All rights reserved.
 *
 * Rochester Institute of Technology
 * CSCI-742
 * S20135,S20145
 *
 * Optimizer for A-normal form intermediate representation.
 *)

structure AnfIROptimizer : ANF_IR_OPTIMIZATION =
struct

   val allAnfOpts =
      {pass = CopyProp.xform, passName = "copy-prop"} ::
      {pass = CopyPropEnv.xform, passName = "copy-prop-env"} ::
      {pass = CopyPropTbl.xform, passName = "copy-prop-tbl"} ::
      {pass = FlattenFail.xform, passName = "flatten-fail"} ::
      {pass = Inline.xform, passName = "inline"} ::
      {pass = SimplifyCase.xform, passName = "simplify-case"} ::
      {pass = Unused.xform, passName = "unused"} ::
      {pass = ValueNum.xform, passName = "value-num"} ::
      {pass = fn p => p, passName = "nop"} ::
      nil

   val defaultAnfOpts =
      {pass = Inline.xform, passName = "inline"} ::
      {pass = Unused.xform, passName = "unused"} ::
      {pass = FlattenFail.xform, passName = "flatten-fail"} ::
      {pass = ValueNum.xform, passName = "value-num"} ::
      {pass = SimplifyCase.xform, passName = "simplify-case"} ::
      {pass = CopyProp.xform, passName = "copy-prop"} ::
      {pass = Unused.xform, passName = "unused"} ::
      nil

   val anfOptsCtl : {pass: AnfIR.Prog.t -> AnfIR.Prog.t, passName: string} list Controls.control =
      Controls.genControl
      {name = "anf-opts",
       pri = [],
       obscurity = 0,
       help = "anf optimization passes",
       default = defaultAnfOpts}
   val () =
      ControlRegistry.register Control.topRegistry
      {ctl = let
                val fromString =
                   (List.foldr
                    (fn (s, anfOpts) =>
                     case (List.find (fn {pass, passName} => passName = s) allAnfOpts, anfOpts) of
                        (SOME {pass, passName}, SOME anfOpts) =>
                           SOME ({pass = pass, passName = passName} :: anfOpts)
                      | _ => NONE)
                    (SOME [])) o
                   (String.fields (fn c => c = #","))
                val toString =
                   (String.concatWith ",") o
                   (List.map (fn {pass, passName} => passName))
                val passList =
                   {tyName = "pass list",
                    fromString = fromString,
                    toString = toString}
             in
                Controls.stringControl passList anfOptsCtl
             end,
       envName = NONE}

   val anfOptsDropCtl : string list Controls.control =
      Controls.genControl
      {name = "drop-anf-opts",
       pri = [],
       obscurity = 0,
       help = "drop anf optimization passes",
       default = []}
   val () =
      ControlRegistry.register Control.topRegistry
      {ctl = Controls.stringControl ControlUtil.Cvt.stringList anfOptsDropCtl,
       envName = NONE}

   val anfOptsKeepCtl : string list Controls.control =
      Controls.genControl
      {name = "keep-anf-opts",
       pri = [],
       obscurity = 0,
       help = "keep anf optimization passes",
       default = []}
   val () =
      ControlRegistry.register Control.topRegistry
      {ctl = Controls.stringControl ControlUtil.Cvt.stringList anfOptsKeepCtl,
       envName = NONE}
      
   val sizeMsg = SOME (fn p => "program size: " ^ (Int.toString (AnfIR.Prog.size p)))

   val xform = fn prog =>
      let
         val anfOpts = Controls.get anfOptsCtl
         val anfOptsDrop = Controls.get anfOptsDropCtl
         val anfOptsKeep = Controls.get anfOptsKeepCtl

         val next = let val ctr = ref 0
                    in fn () => (ctr := !ctr + 1 ; !ctr)
                    end

         val prog =
            List.foldl
            (fn ({pass, passName}, prog) =>
             let
                val passName = passName ^ "." ^ (Int.toString (next ()))
                val equalsPassName = fn pn => passName = pn
             in
                if List.exists equalsPassName anfOptsDrop
                   then prog
                else let
                        val keep =
                           if List.exists equalsPassName anfOptsKeep
                              then SOME {output = AnfIR.Prog.output,
                                         ext = "anf"}
                           else NONE
                        val pass =
                           Control.mkKeepPassSimple
                           {keep = keep,
                            pass = pass,
                            passName = passName}
                        val pass =
                           Control.mkTracePass
                           {msgPre = sizeMsg,
                            msgPost = sizeMsg,
                            pass = pass,
                            passName = passName}
                        val prog = pass prog
                        val () = AnfIRTypeChecker.typeCheck prog
                     in
                        prog
                     end
             end)
            prog
            anfOpts
      in
         prog
      end

   val xform =
      Control.mkKeepCtlPass
      {keepPre = NONE,
       keepPost = SOME {output = AnfIR.Prog.output,
                        ext = "anf"},
       passName = "optimize-anf",
       pass = xform}

   val xform =
      Control.mkTracePass
      {msgPre = sizeMsg,
       msgPost = sizeMsg,
       passName = "optimize-anf",
       pass = xform}
end
