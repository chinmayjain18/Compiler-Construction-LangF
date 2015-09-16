(* langfc-src/scanner-parser/yacc-parser.sml
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
 * Convert an ml-yacc parser to a generic parser.
 *
 * This module converts the WrappedScanner types and functions into
 * ones suitable for the ml-yacc parser.
 *)

structure YaccParser : PARSER =
struct
   structure YaccLrVals =
      LangFYaccLrValsFun (structure Token = LrParser.Token)

   structure YaccTokens =
      struct
         open YaccLrVals.Tokens
         fun fromTokens (tok, span as (posl,posr)) =
            case tok of
               Tokens.KW_and => KW_and span
             | Tokens.KW_andalso => KW_andalso span
             | Tokens.KW_case => KW_case span
             | Tokens.KW_datatype => KW_datatype span
             | Tokens.KW_else => KW_else span
             | Tokens.KW_end => KW_end span
             | Tokens.KW_fn => KW_fn span
             | Tokens.KW_fun => KW_fun span
             | Tokens.KW_if => KW_if span
             | Tokens.KW_in => KW_in span
             | Tokens.KW_let => KW_let span
             | Tokens.KW_of => KW_of span
             | Tokens.KW_orelse => KW_orelse span
             | Tokens.KW_then => KW_then span
             | Tokens.KW_type => KW_type span
             | Tokens.KW_val => KW_val span
             | Tokens.PLUS => PLUS span
             | Tokens.MINUS => MINUS span
             | Tokens.ASTERISK => ASTERISK span
             | Tokens.SLASH => SLASH span
             | Tokens.PERCENT => PERCENT span
             | Tokens.TILDE => TILDE span
             | Tokens.EQEQ => EQEQ span
             | Tokens.LTGT => LTGT span
             | Tokens.LTEQ => LTEQ span
             | Tokens.LT => LT span
             | Tokens.GTEQ => GTEQ span
             | Tokens.GT => GT span
             | Tokens.CARET => CARET span
             | Tokens.HASH => HASH span
             | Tokens.BANG => BANG span
             | Tokens.COLON_EQ => COLON_EQ span
             | Tokens.LPAREN => LPAREN span
             | Tokens.RPAREN => RPAREN span
             | Tokens.LSBRACK => LSBRACK span
             | Tokens.RSBRACK => RSBRACK span
             | Tokens.LCBRACK => LCBRACK span
             | Tokens.RCBRACK => RCBRACK span
             | Tokens.MINUS_ARROW => MINUS_ARROW span
             | Tokens.EQ_ARROW => EQ_ARROW span
             | Tokens.EQ => EQ span
             | Tokens.COLON => COLON span
             | Tokens.COMMA => COMMA span
             | Tokens.SEMI => SEMI span
             | Tokens.VBAR => VBAR span
             | Tokens.UNDERSCORE => UNDERSCORE span
             | Tokens.TYVAR_NAME a => TYVAR_NAME (a, posl, posr)
             | Tokens.CON_NAME a => CON_NAME (a, posl, posr)
             | Tokens.VAR_NAME a => VAR_NAME (a, posl, posr)
             | Tokens.INTEGER i => INTEGER (i, posl, posr)
             | Tokens.STRING s => STRING (s, posl, posr)
      end

   structure YaccLexer =
      struct
         structure UserDeclarations =
            struct
               type svalue = YaccTokens.svalue
               type ('svalue,'pos) token = ('svalue,'pos) YaccTokens.token
               type pos = Source.Pos.t
            end
         fun makeLexer _ _ =
            raise Fail "YaccLex.lexer"
      end

   structure YaccParser =
      Join
      (structure ParserData = YaccLrVals.ParserData
       structure Lex = YaccLexer
       structure LrParser = LrParser)

   fun parse (errStrm : ErrorStream.t,
              scanRdr : (Tokens.token * Source.Span.t,
                         WrappedScanner.Stream.t) StringCvt.reader,
              scanStrm : WrappedScanner.Stream.t)
             : ParseTree.Prog.t option =
      let
         val scanStrm =
            YaccParser.Stream.streamify
            (let
                val scanStrmOptRef = ref (SOME scanStrm)
             in
                fn () =>
                case !scanStrmOptRef of
                   NONE => YaccTokens.EOF Source.Span.eof
                 | SOME scanStrm =>
                      (case scanRdr scanStrm of
                          NONE =>
                             let val () = scanStrmOptRef := NONE
                             in YaccTokens.EOF Source.Span.eof
                             end
                        | SOME ((tok,span),scanStrm) =>
                             let val () = scanStrmOptRef := SOME scanStrm
                             in YaccTokens.fromTokens (tok, span)
                             end)
             end)

         val (prog, _) =
            YaccParser.parse
            (15,
             scanStrm,
             fn (msg, posl, posr) =>
             ErrorStream.addErrorAt (errStrm, (posl, posr), msg),
             ())
      in
         SOME prog
      end handle YaccParser.ParseError => NONE

   val parse =
      Control.mkTracePass
      {msgPre = NONE,
       msgPost = NONE,
       passName = "yacc-parse",
       pass = parse}
end
