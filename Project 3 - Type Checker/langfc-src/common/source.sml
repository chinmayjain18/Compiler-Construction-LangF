(* langfc-src/common/source.sml
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
 * Source positions, spans, locations, and maps in the LangF compiler
 * (langfc).
 *
 * Source positions, spans, locations, and maps are identical to
 * AntlrStreamPos.{pos,span,sourceloc,sourcemap}, but rebound here to
 * less biased names.
 *)

structure Source :> SOURCE =
struct
   structure Loc =
      struct
         type t = {fileName: string  option, lineNo: int, colNo: int}
      end
   structure Pos =
      struct
         type t = AntlrStreamPos.pos

         fun compare (pos1: t, pos2: t) = Position.compare (pos1, pos2)

         val bogus : t = ~3
         val eof : t = ~2
         val bof : t = ~1

         val forward = AntlrStreamPos.forward

         val toLoc = AntlrStreamPos.sourceLoc
         fun toString map (pos: t) =
            if pos = bogus
               then "<bogus>"
            else
            let
               val {fileName, lineNo, colNo} = toLoc map pos
               val lineNoColNo =
                  if pos = eof
                     then "eof"
                  else concat [Int.toString lineNo, ".", Int.toString colNo]
            in
               concat [(case fileName of NONE => "<?>" | SOME fileName => fileName),
                       ":", lineNoColNo]
            end
      end
   structure Span =
      struct
         type t = Pos.t * Pos.t

         val bogus = (Pos.bogus, Pos.bogus)
         val eof = (Pos.eof, Pos.eof)

         fun compare ((pos1l, pos1r): t, (pos2l, pos2r): t) =
            case Position.compare (pos1l, pos2l) of
               LESS => LESS
             | EQUAL => Position.compare (pos1r, pos2r)
             | GREATER => GREATER

         fun toString srcMap ((pos1, pos2): t) =
            if pos1 = pos2
               then Pos.toString srcMap pos1
            else concat [Pos.toString srcMap pos1, "-", Pos.toString srcMap pos2]
      end
   structure Map =
      struct
         type t = AntlrStreamPos.sourcemap
         val new = fn fileName =>
            let
               val srcMap = AntlrStreamPos.mkSourcemap' fileName
               val () =
                  AntlrStreamPos.resynch srcMap
                                         (Pos.bof,
                                          {fileName = NONE,
                                           lineNo = ~1, colNo = 0})
               val () =
                  AntlrStreamPos.resynch srcMap
                                         (Pos.eof,
                                          {fileName = SOME fileName,
                                           lineNo = ~1, colNo = 0})
               val () =
                  AntlrStreamPos.resynch srcMap
                                         (Pos.bof,
                                          {fileName = SOME fileName,
                                           lineNo = 1, colNo = 0})
            in
               srcMap
            end
      end

   val markNewLineAt = AntlrStreamPos.markNewLine
end
