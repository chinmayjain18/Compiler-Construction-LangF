(* langfc-src/type-checker/type-checker.sig
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
 * Front-end type-checker in the LangF compiler (langfc).
 *)

signature TYPE_CHECKER =
sig
   val typeCheck :
      ErrorStream.t * ParseTree.Prog.t ->
      AbsSynTree.Prog.t option
end
