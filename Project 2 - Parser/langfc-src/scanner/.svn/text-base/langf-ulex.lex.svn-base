(* langf-src/scanner-parser/langf-ulex.lex
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
 * Scanner as used by the LangF compiler (langfc) driver.
 *
 * LangF lexical specification for ml-ulex.
 *)

%name LangFULexScanner;

%defs (
  structure T = Tokens

  (* Track comment starting position. *)
  val commentPos : Source.Pos.t option ref = ref NONE
  (* Track comment nesting depth. *)
  val commentDepth = ref 0

  (* Track string starting position. *)
  val stringPos : Source.Pos.t option ref = ref NONE
  (* List of string fragments to concatenate. *)
  val stringBuf : string list ref = ref []
  (* Add a string to the string buffer *)
  fun addStr str = (stringBuf := str :: !stringBuf)
  (* Make a string from the string buffer. *)
  fun mkString () = (SOME (T.STRING(String.concat(List.rev(!stringBuf))))
                     before stringBuf := [])

  (* Required by ml-ulex. *)
  type lex_arg = {reportErrorAt: Source.Pos.t * string -> unit}
  type lex_result = Tokens.token option
  (* Required by ml-ulex. *)
  fun eof () = NONE
);

%arg ( {reportErrorAt, ...}: UserDeclarations.lex_arg );

%states INITIAL STRING COMMENT;

%let lower_letter = [a-z];
%let upper_letter = [A-Z];
%let letter = {lower_letter}|{upper_letter};
%let digit = [0-9];
%let id_char = {letter}|{digit}|"_"|"'";

<INITIAL> "and"      => (SOME T.KW_and);
<INITIAL> "andalso"  => (SOME T.KW_andalso);
<INITIAL> "case"     => (SOME T.KW_case);
<INITIAL> "datatype" => (SOME T.KW_datatype);
<INITIAL> "else"     => (SOME T.KW_else);
<INITIAL> "end"      => (SOME T.KW_end);
<INITIAL> "fn"       => (SOME T.KW_fn);
<INITIAL> "fun"      => (SOME T.KW_fun);
<INITIAL> "if"       => (SOME T.KW_if);
<INITIAL> "in"       => (SOME T.KW_in);
<INITIAL> "let"      => (SOME T.KW_let);
<INITIAL> "of"       => (SOME T.KW_of);
<INITIAL> "orelse"   => (SOME T.KW_orelse);
<INITIAL> "then"     => (SOME T.KW_then);
<INITIAL> "type"     => (SOME T.KW_type);
<INITIAL> "val"      => (SOME T.KW_val);
<INITIAL> "+"        => (SOME T.PLUS);
<INITIAL> "-"        => (SOME T.MINUS);
<INITIAL> "*"        => (SOME T.ASTERISK);
<INITIAL> "/"        => (SOME T.SLASH);
<INITIAL> "%"        => (SOME T.PERCENT);
<INITIAL> "~"        => (SOME T.TILDE);
<INITIAL> "=="       => (SOME T.EQEQ);
<INITIAL> "<>"       => (SOME T.LTGT);
<INITIAL> "<="       => (SOME T.LTEQ);
<INITIAL> "<"        => (SOME T.LT);
<INITIAL> ">="       => (SOME T.GTEQ);
<INITIAL> ">"        => (SOME T.GT);
<INITIAL> "^"        => (SOME T.CARET);
<INITIAL> "#"        => (SOME T.HASH);
<INITIAL> "!"        => (SOME T.BANG);
<INITIAL> ":="       => (SOME T.COLON_EQ);
<INITIAL> "("        => (SOME T.LPAREN);
<INITIAL> ")"        => (SOME T.RPAREN);
<INITIAL> "["        => (SOME T.LSBRACK);
<INITIAL> "]"        => (SOME T.RSBRACK);
<INITIAL> "{"        => (SOME T.LCBRACK);
<INITIAL> "}"        => (SOME T.RCBRACK);
<INITIAL> "->"       => (SOME T.MINUS_ARROW);
<INITIAL> "=>"       => (SOME T.EQ_ARROW);
<INITIAL> "="        => (SOME T.EQ);
<INITIAL> ":"        => (SOME T.COLON);
<INITIAL> ","        => (SOME T.COMMA);
<INITIAL> ";"        => (SOME T.SEMI);
<INITIAL> "|"        => (SOME T.VBAR);
<INITIAL> "_"        => (SOME T.UNDERSCORE);

<INITIAL> "'"{lower_letter}{id_char}* => (SOME (T.TYVAR_NAME (Atom.atom yytext)));
<INITIAL> {upper_letter}{id_char}* => (SOME (T.CON_NAME (Atom.atom yytext)));
<INITIAL> {lower_letter}{id_char}* => (SOME (T.VAR_NAME (Atom.atom yytext)));

(* LangF integers *)
<INITIAL> "~"?{digit}+ => (SOME (T.INTEGER(valOf (IntInf.fromString yytext))));

(* LangF strings *)
<INITIAL> "\"" => (
  let
     val () = YYBEGIN STRING
     val () = stringPos := SOME (yygetPos () - 2)
  in
     continue ()
  end);
(* symbolic escape sequences. *)
%let sym_esc_seq = "\\"[abfnrtv\\\"];
<STRING> {sym_esc_seq} => (
  let
     val () = addStr(valOf(String.fromString yytext))
  in
     continue ()
  end);
(* numeric escape sequences. *)
(* %let num_esc_seq = "\\"([01]{digit}{2}|2([01234]{digit}|5[012345])); *)
%let num_esc_seq = "\\"{digit}{3};
<STRING> {num_esc_seq} => (
  let
     val nstr = String.extract(yytext,1,NONE)
     val n = valOf (Int.fromString nstr)
     val () =
        if n > 255
           then reportErrorAt
                (yygetPos () - 5,
                 concat ["escape sequence '\\",
                         nstr, "'",
                         " too big in string literal"])
        else addStr (String.str (Char.chr n))
  in
     continue ()
  end);
<STRING> "\\". => (
  reportErrorAt (yygetPos () - 3,
                 concat ["bad escape sequence '\\",
                         String.toString (String.extract(yytext,1,NONE)),
                         "'",
                         " in string literal"]);
  continue ());
(* printable characters. *)
%let print_char = [\032-\126];
<STRING> ({print_char}&[^\"\\])+ => (
  let
     val () = addStr yytext
  in
     continue ()
  end);
<STRING> "\"" => (
  let
     val () = YYBEGIN INITIAL
     val () = stringPos := NONE
  in
     mkString ()
  end);
<STRING> . => (
  reportErrorAt (yygetPos () - 2,
                 concat ["bad character '", String.toString yytext, "'",
                         " in string literal"]);
  continue ());
<STRING> <<EOF>> => (
  let
    val () = reportErrorAt (valOf (!stringPos), "unterminated string literal at eof");
    val () = stringPos := NONE
    val () = YYBEGIN INITIAL
  in
    mkString ()
  end);

(* LangF white space *)
%let white_space = " "|[\t\n\v\f\r];
<INITIAL> {white_space} => (skip ());

(* LangF comments *)
<INITIAL> "(*" => (
  let
     val () = YYBEGIN COMMENT
     val () = commentPos := SOME (yygetPos () - 3)
     val () = commentDepth := 1
  in
     skip ()
  end);
<COMMENT> "(*" => (
  let
     val () = commentDepth := !commentDepth + 1
  in
     skip ()
  end);
<COMMENT> "*)" => (
  let
     val () = commentDepth := !commentDepth - 1
     val () = if !commentDepth = 0
                 then (commentPos := NONE; YYBEGIN INITIAL)
                 else ()
  in
     skip ()
  end);
<COMMENT> . => (skip ());
<COMMENT> <<EOF>> => (
  let
     val () = reportErrorAt (valOf (!commentPos), "unterminated comment at eof");
     val () = YYBEGIN INITIAL
     val () = commentPos := NONE
  in
     skip ()
  end);

(* LangF *)
<INITIAL> . => (
  reportErrorAt (yygetPos () - 2, concat ["bad character '", String.toString yytext, "'"]);
  skip ());
<INITIAL> <<EOF>> => (eof ());
