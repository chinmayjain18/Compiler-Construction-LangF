(* langfc-src/scanner-parser/tokens.sig
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

signature TOKENS =
sig
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

   val toString : token -> string
end
