(* langfc-src/type-checker/wrapped-type-checker.sml
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
 * Front-end type checker in the LangF compiler (langfc).
 *
 * This module calls the type checker, installs the 'keep-type-check'
 * control that saves the abstract syntax tree to a file, and installs
 * the 'stop-type-check' control that stops compilation after type
 * checking.
 *)

structure WrappedTypeChecker :> WRAPPED_TYPE_CHECKER =
struct
   structure AST = AbsSynTree

   val typeCheck = TypeChecker.typeCheck

   local
      fun output (outStrm, prog : AST.Prog.t option) =
         case prog of
            NONE => ()
          | SOME prog => AST.Prog.output (outStrm, prog)
   in
      val typeCheck =
         Control.mkKeepCtlPass
         {keepPre = NONE,
          keepPost = SOME {output = output,
                           ext = "ast"},
          passName = "type-check",
          pass = typeCheck}
   end

   val typeCheck =
      Control.mkTracePass
      {msgPre = NONE,
       msgPost = NONE,
       passName = "type-check",
       pass = typeCheck}

   val typeCheck =
      Control.mkStopCtlPass
      {passName = "type-check",
       pass = typeCheck}
end
