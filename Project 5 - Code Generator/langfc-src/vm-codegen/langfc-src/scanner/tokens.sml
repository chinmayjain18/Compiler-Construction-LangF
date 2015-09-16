(* langfc-src/scanner-parser/tokens.sml
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
 * Representation of tokens in the LangF compiler (langfc).
 *)

structure Tokens : TOKENS =
struct
   datatype token =
      KW_and
    | KW_andalso
    | KW_case
    | KW_datatype
    | KW_else
    | KW_end
    | KW_fn
    | KW_fun
    | KW_if
    | KW_in
    | KW_let
    | KW_of
    | KW_orelse
    | KW_then
    | KW_type
    | KW_val
    | PLUS | MINUS | ASTERISK | SLASH | PERCENT | TILDE
    | EQEQ | LTGT | LTEQ | LT | GTEQ | GT
    | CARET
    | HASH (* # *) | BANG (* ! *) | COLON_EQ (* := *)
    | LPAREN (* ( *) | RPAREN (* ) *)
    | LSBRACK (* [ *) | RSBRACK (* ] *)
    | LCBRACK (* { *) | RCBRACK (* } *)
    | MINUS_ARROW (* -> *) | EQ_ARROW (* => *)
    | EQ | COLON | COMMA | SEMI | VBAR (* | *) | UNDERSCORE
    | TYVAR_NAME of Atom.atom
    | CON_NAME of Atom.atom
    | VAR_NAME of Atom.atom
    | INTEGER of IntInf.int
    | STRING of String.string

   fun toString tok =
      case tok of
         KW_and => "and"
       | KW_andalso => "andalso"
       | KW_case => "case"
       | KW_datatype => "datatype"
       | KW_else => "else"
       | KW_end => "end"
       | KW_fn => "fn"
       | KW_fun => "fun"
       | KW_if => "if"
       | KW_in => "in"
       | KW_let => "let"
       | KW_of => "of"
       | KW_orelse => "orelse"
       | KW_then => "then"
       | KW_type => "type"
       | KW_val => "val"
       | PLUS => "+"
       | MINUS => "-"
       | ASTERISK => "*"
       | SLASH => "/"
       | PERCENT => "%"
       | TILDE => "~"
       | EQEQ => "=="
       | LTGT => "<>"
       | LTEQ => "<="
       | LT => "<"
       | GTEQ => ">="
       | GT => ">"
       | CARET => "^"
       | HASH => "#"
       | BANG => "!"
       | COLON_EQ => ":="
       | LPAREN => "("
       | RPAREN => ")"
       | LSBRACK => "["
       | RSBRACK => "]"
       | LCBRACK => "{"
       | RCBRACK => "}"
       | MINUS_ARROW => "->"
       | EQ_ARROW => "=>"
       | EQ => "="
       | COLON => ":"
       | COMMA => ","
       | SEMI => ";"
       | VBAR => "|"
       | UNDERSCORE => "_"
       | TYVAR_NAME a => concat ["(* TYVAR_NAME *) ", Atom.toString a]
       | CON_NAME a => concat ["(* CON_NAME *) ", Atom.toString a]
       | VAR_NAME a => concat ["(* VAR_NAME *) ", Atom.toString a]
       | INTEGER i => concat ["(* INTEGER *) ", IntInf.toString i]
       | STRING s => concat ["(* STRING *) \"", String.toString s, "\""]
end
