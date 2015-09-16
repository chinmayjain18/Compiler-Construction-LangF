(* langfc-src/core-ir/optimizer.sml
 *
 * COPYRIGHT (c) 2015 Matthew Fluet (http://www.cs.rit.edu/~mtf)
 * All rights reserved.
 *
 * Rochester Institute of Technology
 * CSCI-742
 * S20135,S20145
 *
 * Optimizer for core intermediate representation.
 *)

structure CoreIROptimizer : CORE_IR_OPTIMIZATION =
struct

   val allCoreOpts =
      {pass = DeadCode.xform, passName = "dead-code"} ::
      {pass = fn p => p, passName = "nop"} ::
      nil

   val defaultCoreOpts =
      {pass = DeadCode.xform, passName = "dead-code"} ::
      nil

   val coreOptsCtl : {pass: CoreIR.Prog.t -> CoreIR.Prog.t, passName: string} list Controls.control =
      Controls.genControl
      {name = "core-opts",
       pri = [],
       obscurity = 0,
       help = "core optimization passes",
       default = defaultCoreOpts}
   val () =
      ControlRegistry.register Control.topRegistry
      {ctl = let
                val fromString =
                   (List.foldr
                    (fn (s, coreOpts) =>
                     case (List.find (fn {pass, passName} => passName = s) allCoreOpts, coreOpts) of
                        (SOME {pass, passName}, SOME coreOpts) =>
                           SOME ({pass = pass, passName = passName} :: coreOpts)
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
                Controls.stringControl passList coreOptsCtl
             end,
       envName = NONE}

   val coreOptsDropCtl : string list Controls.control =
      Controls.genControl
      {name = "drop-core-opts",
       pri = [],
       obscurity = 0,
       help = "drop core optimization passes",
       default = []}
   val () =
      ControlRegistry.register Control.topRegistry
      {ctl = Controls.stringControl ControlUtil.Cvt.stringList coreOptsDropCtl,
       envName = NONE}

   val coreOptsKeepCtl : string list Controls.control =
      Controls.genControl
      {name = "keep-core-opts",
       pri = [],
       obscurity = 0,
       help = "keep core optimization passes",
       default = []}
   val () =
      ControlRegistry.register Control.topRegistry
      {ctl = Controls.stringControl ControlUtil.Cvt.stringList coreOptsKeepCtl,
       envName = NONE}
      
   val sizeMsg = NONE

   val xform = fn prog =>
      let
         val coreOpts = Controls.get coreOptsCtl
         val coreOptsDrop = Controls.get coreOptsDropCtl
         val coreOptsKeep = Controls.get coreOptsKeepCtl

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
                if List.exists equalsPassName coreOptsDrop
                   then prog
                else let
                        val keep =
                           if List.exists equalsPassName coreOptsKeep
                              then SOME {output = CoreIR.Prog.output,
                                         ext = "core"}
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
                        val () = CoreIRTypeChecker.typeCheck prog
                     in
                        prog
                     end
             end)
            prog
            coreOpts
      in
         prog
      end

   val xform =
      Control.mkKeepCtlPass
      {keepPre = NONE,
       keepPost = SOME {output = CoreIR.Prog.output,
                        ext = "core"},
       passName = "optimize-core",
       pass = xform}

   val xform =
      Control.mkTracePass
      {msgPre = sizeMsg,
       msgPost = sizeMsg,
       passName = "optimize-core",
       pass = xform}
end
