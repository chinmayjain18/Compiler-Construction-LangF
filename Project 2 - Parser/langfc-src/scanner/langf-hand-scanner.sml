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

        (* add more scan??? functions for specific complex tokens *)

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
               (* add more matches for other specific initial characters *)
               | SOME (c0, strm1) =>
                    if Char.isSpace c0
                       then scanTok strm1
                    (* add more 'else if's for other classes of initial character *)
                    else (badChar (pos0, c0); scanTok strm1)
           end
     in
        scanTok
     end
end
