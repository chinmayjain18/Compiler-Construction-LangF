(* langfc-src/scanner-parser/wrapped-parser.sml
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
 * Parser as used by the LangF compiler (langfc) driver.
 *
 * This module calls a generic parser, and installs the 'parser'
 * control that selects between the ML-Yacc and the ML-Antlr parser,
 * installs the 'keep-parse' control that saves the parse tree to a
 * file, and installs the 'stop-parse' control that stops compilation
 * after parsing.
 *)

structure WrappedParser : WRAPPED_PARSER =
struct
   structure ParserCtl =
      struct
         datatype t = Antlr | Yacc
         val fromString =
            fn "antlr" => SOME Antlr
             | "yacc" => SOME Yacc
             | _ => NONE
         val toString =
            fn Antlr => "antlr"
             | Yacc => "yacc"
         val value_cvt =
            {tyName = "parser-ctl",
             fromString = fromString,
             toString = toString}
      end

   val parserCtl : ParserCtl.t Controls.control =
      Controls.genControl
      {name = "parser",
       pri = [],
       obscurity = 1,
       help = "select between ml-antlr and ml-yacc parser",
       default = ParserCtl.Antlr}
   val () =
      ControlRegistry.register Control.topRegistry
      {ctl = Controls.stringControl ParserCtl.value_cvt parserCtl,
       envName = NONE}

   val antlrParse = AntlrParser.parse
   val yaccParse = YaccParser.parse

   fun parse (errStrm, scanRdr, scanStrm) =
      case Controls.get parserCtl of
         ParserCtl.Antlr => antlrParse (errStrm, scanRdr, scanStrm)
       | ParserCtl.Yacc => yaccParse (errStrm, scanRdr, scanStrm)

   local
      fun output (outStrm, prog) =
         case prog of
            NONE => ()
          | SOME prog => ParseTree.Prog.output (outStrm, prog)
   in
      val parse =
         Control.mkKeepCtlPass
         {keepPre = NONE,
          keepPost = SOME {output = output,
                           ext = "pt"},
          passName = "parse",
          pass = parse}
   end

   val parse =
      Control.mkTracePass
      {msgPre = NONE,
       msgPost = NONE,
       passName = "parse",
       pass = parse}

   val parse =
      Control.mkStopCtlPass
      {passName = "parse",
       pass = parse}
end
