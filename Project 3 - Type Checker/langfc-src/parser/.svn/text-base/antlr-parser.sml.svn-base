(* langfc-src/scanner-parser/antlr-parser.sml
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
 * Convert an ml-antlr parser to a generic parser.
 *
 * This module converts the WrappedScanner types and functions into
 * ones suitable for the ml-antlr parser.
 *)

structure AntlrParser : PARSER =
struct
   structure AntlrTokens =
      struct
         open LangFAntlrTokens
         fun toString tok =
            case tok of
               TYVAR_NAME a => concat ["TYVAR_NAME (", Atom.toString a, ")"]
             | CON_NAME a => concat ["CON_NAME (", Atom.toString a, ")"]
             | VAR_NAME a => concat ["VAR_NAME (", Atom.toString a, ")"]
             | INTEGER i => concat ["INTEGER (", IntInf.toString i, ")"]
             | STRING s => concat ["STRING (\"", String.toString s, "\")"]
             | tok => LangFAntlrTokens.toString tok
         val listToString =
            (String.concatWith " ") o (List.map toString)

         fun fromTokens tok =
            case tok of
               Tokens.KW_and => KW_and
             | Tokens.KW_andalso => KW_andalso
             | Tokens.KW_case => KW_case
             | Tokens.KW_datatype => KW_datatype
             | Tokens.KW_else => KW_else
             | Tokens.KW_end => KW_end
             | Tokens.KW_fn => KW_fn
             | Tokens.KW_fun => KW_fun
             | Tokens.KW_if => KW_if
             | Tokens.KW_in => KW_in
             | Tokens.KW_let => KW_let
             | Tokens.KW_of => KW_of
             | Tokens.KW_orelse => KW_orelse
             | Tokens.KW_then => KW_then
             | Tokens.KW_type => KW_type
             | Tokens.KW_val => KW_val
             | Tokens.PLUS => PLUS
             | Tokens.MINUS => MINUS
             | Tokens.ASTERISK => ASTERISK
             | Tokens.SLASH => SLASH
             | Tokens.PERCENT => PERCENT
             | Tokens.TILDE => TILDE
             | Tokens.EQEQ => EQEQ
             | Tokens.LTGT => LTGT
             | Tokens.LTEQ => LTEQ
             | Tokens.LT => LT
             | Tokens.GTEQ => GTEQ
             | Tokens.GT => GT
             | Tokens.CARET => CARET
             | Tokens.HASH => HASH
             | Tokens.BANG => BANG
             | Tokens.COLON_EQ => COLON_EQ
             | Tokens.LPAREN => LPAREN
             | Tokens.RPAREN => RPAREN
             | Tokens.LSBRACK => LSBRACK
             | Tokens.RSBRACK => RSBRACK
             | Tokens.LCBRACK => LCBRACK
             | Tokens.RCBRACK => RCBRACK
             | Tokens.MINUS_ARROW => MINUS_ARROW
             | Tokens.EQ_ARROW => EQ_ARROW
             | Tokens.EQ => EQ
             | Tokens.COLON => COLON
             | Tokens.COMMA => COMMA
             | Tokens.SEMI => SEMI
             | Tokens.VBAR => VBAR
             | Tokens.UNDERSCORE => UNDERSCORE
             | Tokens.TYVAR_NAME a => TYVAR_NAME a
             | Tokens.CON_NAME a => CON_NAME a
             | Tokens.VAR_NAME a => VAR_NAME a
             | Tokens.INTEGER i => INTEGER i
             | Tokens.STRING s => STRING s
      end

   structure AntlrLexer : ANTLR_LEXER =
      struct
         type pos = Source.Pos.t
         type strm = WrappedScanner.Stream.t option
         fun getPos strmOpt =
            case strmOpt of
               NONE => Source.Pos.eof
             | SOME strm => WrappedScanner.Stream.getPos strm
      end

   structure LangFAntlrParser = LangFAntlrParseFn (AntlrLexer)

   fun parse (errStrm : ErrorStream.t,
              scanRdr : (Tokens.token * Source.Span.t,
                         WrappedScanner.Stream.t) StringCvt.reader,
              scanStrm : WrappedScanner.Stream.t)
             : ParseTree.Prog.t option =
      let
         fun scanFn scanStrm =
            (case scanStrm of
                NONE => (AntlrTokens.EOF, Source.Span.eof, NONE)
              | SOME scanStrm =>
                   (case scanRdr scanStrm of
                       NONE => (AntlrTokens.EOF, Source.Span.eof, NONE)
                     | SOME ((tok, span), scanStrm) =>
                          let
                             val tok = AntlrTokens.fromTokens tok
                          in
                             (tok, span, SOME scanStrm)
                          end))

         val (prog, _, errs) =
            LangFAntlrParser.parse scanFn (SOME scanStrm)

         val () =
            List.app (fn (pos, repair) =>
                      let
                         val msg =
                            case repair of
                               AntlrRepair.Insert toks =>
                                  ["syntax error; try inserting \"",
                                   AntlrTokens.listToString toks, "\""]
                             | AntlrRepair.Delete toks =>
                                  ["syntax error; try deleting \"",
                                   AntlrTokens.listToString toks, "\""]
                             | AntlrRepair.Subst{old, new} =>
                                  ["syntax error; try substituting \"",
                                   AntlrTokens.listToString new, "\" for \"",
                                   AntlrTokens.listToString old, "\""]
                             | AntlrRepair.FailureAt tok =>
                                  ["syntax error at \"",
                                   AntlrTokens.toString tok, "\""]
                      in
                         ErrorStream.addErrorAt
                         (errStrm, (pos, pos), String.concat msg)
                      end)
                     errs
      in
         prog
      end

   val parse =
      Control.mkTracePass
      {msgPre = NONE,
       msgPost = NONE,
       passName = "antlr-parse",
       pass = parse}
end
