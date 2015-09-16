(* langfc-src/scanner-parser/langf-hand-scanner.sml
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
 * Hand-written LangF scanner as a StringCvt.reader.
 *)

structure LangFHandScanner: LANGF_HAND_SCANNER =
struct
   structure T = Tokens
   fun scan {getPos: 'strm -> 'pos,
             forwardPos: 'pos * int -> 'pos,
             reportErrorAt: 'pos * string -> unit}
            (charRdr: (char, 'strm) StringCvt.reader) :
            (Tokens.token * ('pos * 'pos), 'strm) StringCvt.reader =
     let
        fun badChar (pos, c) =
           reportErrorAt (pos, concat ["bad character '", Char.toString c, "'"])

        val keywords =
           [("and", T.KW_and),
            ("andalso", T.KW_andalso),
            ("case", T.KW_case),
            ("datatype", T.KW_datatype),
            ("else", T.KW_else),
            ("end", T.KW_end),
            ("fn", T.KW_fn),
            ("fun", T.KW_fun),
            ("if", T.KW_if),
            ("in", T.KW_in),
            ("let", T.KW_let),
            ("of", T.KW_of),
            ("orelse", T.KW_orelse),
            ("then", T.KW_then),
            ("type", T.KW_type),
            ("val", T.KW_val)]
        fun tyvarName s = T.TYVAR_NAME (Atom.atom s)
        fun conName s = T.CON_NAME (Atom.atom s)
        fun varName s = T.VAR_NAME (Atom.atom s)

        fun digitToInt d =
           (Char.ord d) - (Char.ord #"0")

        fun scanIdent (strm0, acc, lastPos) =
           let
              fun done strm = (strm, String.implode (List.rev acc), forwardPos (lastPos, 1))
           in
              case charRdr strm0 of
                 NONE => done strm0
               | SOME (c0, strm1) =>
                    if Char.isAlphaNum c0
                       orelse c0 = #"_"
                       orelse c0 = #"'"
                       then scanIdent (strm1, c0::acc, getPos strm0)
                    else done strm0
           end

        fun scanInteger (strm0, acc, lastPos) =
           let
              fun done strm = 
                 (strm, valOf (IntInf.fromString (String.implode (List.rev acc))), forwardPos (lastPos, 1))
           in
              case charRdr strm0 of
                 NONE => done strm0
               | SOME (c0, strm1) =>
                    if Char.isDigit c0
                       then scanInteger (strm1, c0::acc, getPos strm0)
                    else done strm0
           end

        fun scanString (stringStartPos, strm0, acc) =
           let
              fun badChar (pos, c) =
                 reportErrorAt (pos, concat ["bad character '", Char.toString c, "' in string literal"])
              fun badEscSeq (pos, c) =
                 reportErrorAt (pos, concat ["bad escape sequence '\\", Char.toString c, "' in string literal"])
              fun done (strm, lastPos) =
                 (strm, String.implode (List.rev acc), lastPos)
              val pos0 = getPos strm0
              fun posN n = forwardPos (pos0, n)
           in
              case charRdr strm0 of
                 NONE => (reportErrorAt (stringStartPos, "unterminated string literal at eof")
                          ; done (strm0, pos0))
               | SOME (#"\"", strm1) => done (strm1, posN 1)
               | SOME (c0 as #"\\", strm1) =>
                    (case charRdr strm1 of
                        SOME (#"a", strm2) =>
                           scanString (stringStartPos, strm2, #"\a"::acc)
                      | SOME (#"b", strm2) =>
                           scanString (stringStartPos, strm2, #"\b"::acc)
                      | SOME (#"f", strm2) =>
                           scanString (stringStartPos, strm2, #"\f"::acc)
                      | SOME (#"n", strm2) =>
                           scanString (stringStartPos, strm2, #"\n"::acc)
                      | SOME (#"r", strm2) =>
                           scanString (stringStartPos, strm2, #"\r"::acc)
                      | SOME (#"t", strm2) =>
                           scanString (stringStartPos, strm2, #"\t"::acc)
                      | SOME (#"v", strm2) =>
                           scanString (stringStartPos, strm2, #"\v"::acc)
                      | SOME (#"\\", strm2) =>
                           scanString (stringStartPos, strm2, #"\\"::acc)
                      | SOME (#"\"", strm2) =>
                           scanString (stringStartPos, strm2, #"\""::acc)
                      | SOME (c1, strm2) =>
                           let
                              fun bad () =
                                let
                                   val () = badEscSeq (pos0, c1)
                                in
                                   scanString (stringStartPos, strm2, acc)
                                end
                           in
                              if not (Char.isDigit c1)
                                 then bad ()
                              else (case charRdr strm2 of
                                       NONE => bad ()
                                     | SOME (c2, strm3) =>
                                          if not (Char.isDigit c2)
                                             then bad ()
                              else (case charRdr strm3 of
                                       NONE => bad ()
                                     | SOME (c3, strm4) =>
                                          if not (Char.isDigit c3)
                                             then bad ()
                              else let
                                      val i =
                                         valOf (Int.fromString (String.implode [c1,c2,c3]))
                                   in
                                       if i < 256
                                          then scanString
                                               (stringStartPos, strm4, (Char.chr i)::acc)
                                       else (reportErrorAt (pos0, concat ["escape sequence '\\", Int.toString i, "' too big in string literal"])
                                             ; scanString (stringStartPos, strm4, acc))
                                   end))
                           end
                      | NONE => (badChar (pos0, c0); scanString (stringStartPos, strm1, acc)))
               | SOME (c0, strm1) =>
                    if #" " <= c0 andalso c0 <= #"~"
                       then scanString (stringStartPos, strm1, c0::acc)
                    else (badChar (pos0, c0); scanString (stringStartPos, strm1, acc))
           end

        fun scanComment (commentStartPos, strm0, lvl) =
           if lvl = 0
              then strm0
           else (case charRdr strm0 of
                    NONE => (reportErrorAt (commentStartPos, "unterminated comment at eof")
                             ; strm0)
                  | SOME (#"(", strm1) =>
                       (case charRdr strm1 of
                           SOME (#"*", strm2) => scanComment (commentStartPos, strm2, lvl + 1)
                         | _ => scanComment (commentStartPos, strm1, lvl))
                  | SOME (#"*", strm1) =>
                           (case charRdr strm1 of
                               SOME (#")", strm2) => scanComment (commentStartPos, strm2, lvl - 1)
                             | _ => scanComment (commentStartPos, strm1, lvl))
                  | SOME (c0, strm1) => scanComment (commentStartPos, strm1, lvl))

        fun scanTok strm0 =
           let
              val pos0 = getPos strm0
              fun posN n = forwardPos (pos0, n)
           in
              case charRdr strm0 of
                 NONE => NONE
               | SOME (#"+", strm1) => SOME ((T.PLUS, (pos0, posN 1)), strm1)
               | SOME (#"-", strm1) =>
                    (case charRdr strm1 of
                        SOME (#">", strm2) => SOME ((T.MINUS_ARROW, (pos0, posN 2)), strm2)
                      | _ => SOME ((T.MINUS, (pos0, posN 1)), strm1))
               | SOME (#"*", strm1) => SOME ((T.ASTERISK, (pos0, posN 1)), strm1)
               | SOME (#"/", strm1) => SOME ((T.SLASH, (pos0, posN 1)), strm1)
               | SOME (#"%", strm1) => SOME ((T.PERCENT, (pos0, posN 1)), strm1)
               | SOME (#"~", strm1) =>
                    (case charRdr strm1 of
                        SOME (c1, strm2) =>
                           if Char.isDigit c1
                              then let
                                      val (strm', i, pos') =
                                         scanInteger (strm2, [c1], getPos strm1)
                                   in
                                      SOME ((T.INTEGER (~i), (pos0, pos')), strm')
                                   end
                           else SOME ((T.TILDE, (pos0, posN 1)), strm1)
                      | _ => SOME ((T.TILDE, (pos0, posN 1)), strm1))
               | SOME (#"=", strm1) =>
                    (case charRdr strm1 of
                        SOME (#"=", strm2) => SOME ((T.EQEQ, (pos0, posN 2)), strm2)
                      | SOME (#">", strm2) => SOME ((T.EQ_ARROW, (pos0, posN 2)), strm2)
                      | _ => SOME ((T.EQ, (pos0, posN 1)), strm1))
               | SOME (#"<", strm1) =>
                    (case charRdr strm1 of
                        SOME (#">", strm2) => SOME ((T.LTGT, (pos0, posN 2)), strm2)
                      | SOME (#"=", strm2) => SOME ((T.LTEQ, (pos0, posN 2)), strm2)
                      | _ => SOME ((T.LT, (pos0, posN 1)), strm1))
               | SOME (#">", strm1) =>
                    (case charRdr strm1 of
                        SOME (#"=", strm2) => SOME ((T.GTEQ, (pos0, posN 2)), strm2)
                      | _ => SOME ((T.GT, (pos0, posN 1)), strm1))
               | SOME (#"^", strm1) => SOME ((T.CARET, (pos0, posN 1)), strm1)
               | SOME (#"#", strm1) => SOME ((T.HASH, (pos0, posN 1)), strm1)
               | SOME (#"!", strm1) => SOME ((T.BANG, (pos0, posN 1)), strm1)
               | SOME (#":", strm1) =>
                    (case charRdr strm1 of
                        SOME (#"=", strm2) => SOME ((T.COLON_EQ, (pos0, posN 2)), strm2)
                      | _ => SOME ((T.COLON, (pos0, posN 1)), strm1))
               | SOME (#"(", strm1) =>
                    (case charRdr strm1 of
                        SOME (#"*", strm2) =>
                           let val strm' = scanComment (pos0, strm2, 1)
                           in scanTok strm'
                           end
                      | _ => SOME ((T.LPAREN, (pos0, posN 1)), strm1))
               | SOME (#")", strm1) => SOME ((T.RPAREN, (pos0, posN 1)), strm1)
               | SOME (#"[", strm1) => SOME ((T.LSBRACK, (pos0, posN 1)), strm1)
               | SOME (#"]", strm1) => SOME ((T.RSBRACK, (pos0, posN 1)), strm1)
               | SOME (#"{", strm1) => SOME ((T.LCBRACK, (pos0, posN 1)), strm1)
               | SOME (#"}", strm1) => SOME ((T.RCBRACK, (pos0, posN 1)), strm1)
               | SOME (#",", strm1) => SOME ((T.COMMA, (pos0, posN 1)), strm1)
               | SOME (#";", strm1) => SOME ((T.SEMI, (pos0, posN 1)), strm1)
               | SOME (#"|", strm1) => SOME ((T.VBAR, (pos0, posN 1)), strm1)
               | SOME (#"_", strm1) => SOME ((T.UNDERSCORE, (pos0, posN 1)), strm1)
               | SOME (#"\"", strm1) =>
                    let
                       val (strm', s, pos') = scanString (pos0, strm1, [])
                    in
                       SOME ((T.STRING s, (pos0, pos')), strm')
                    end
               | SOME (c0 as #"'", strm1) =>
                    let
                       fun bad () = (badChar (pos0, c0); scanTok strm1)
                    in
                       case charRdr strm1 of
                          SOME (c1, strm2) =>
                             if Char.isLower c1
                                then let
                                        val (strm', id, pos') =
                                           scanIdent (strm2, [c1, c0], getPos strm1)
                                     in
                                        SOME ((tyvarName id, (pos0, pos')), strm')
                                     end
                             else bad ()
                        | NONE => bad ()
                    end
               | SOME (c0, strm1) =>
                    if Char.isUpper c0
                       then let
                               val (strm', id, pos') = scanIdent (strm1, [c0], pos0)
                            in
                               SOME ((conName id, (pos0, pos')), strm')
                            end
                    else if Char.isLower c0
                       then let
                               val (strm', id, pos') = scanIdent (strm1, [c0], pos0)
                            in
                               case List.find (fn (kw,tok) => kw = id) keywords of
                                  SOME (kw,tok) => SOME ((tok, (pos0, pos')), strm')
                                | NONE => SOME ((varName id, (pos0, pos')), strm')
                            end
                    else if Char.isDigit c0
                       then let
                               val (strm', i, pos') = scanInteger (strm1, [c0], pos0)
                            in
                               SOME ((T.INTEGER i, (pos0, pos')), strm')
                            end
                    else if Char.isSpace c0
                       then scanTok strm1
                    else (badChar (pos0, c0); scanTok strm1)
           end
     in
        scanTok
     end
end
