(* langfc-src/common/error-stream.sml
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
 * Common error reporting utility module in the LangF compiler
 * (langfc).
 *)

structure ErrorStream :> ERROR_STREAM =
struct
   structure Kind =
      struct
         datatype t = Error | Warning
         fun toString k =
            case k of Error => "Error" | Warning => "Warning"
      end
   structure Entry =
      struct
         datatype t = T of {span: Source.Span.t option,
                            kind: Kind.t,
                            msg: string}

         fun toString srcMap (T {span, kind, msg}) =
            concat [case span of
                       NONE => "<?>"
                     | SOME span => Source.Span.toString srcMap span,
                    " ",
                    Kind.toString kind,
                    ": ",
                    msg]
         fun compare (T {span = span1, ...}, T {span = span2, ...}) =
            case (span1, span2) of
               (NONE, NONE) => EQUAL
             | (NONE, SOME _) => LESS
             | (SOME _, NONE) => GREATER
             | (SOME span1, SOME span2) => Source.Span.compare (span1, span2)
      end

   datatype t =
      T of {srcFile : string,
            srcMap: Source.Map.t,
            entries: Entry.t list ref}

   fun mkErrorStream srcFile =
      T {srcFile = srcFile,
         srcMap = Source.Map.new srcFile,
         entries = ref []}

   fun getSourceMap (T {srcMap, ...}) = srcMap

   fun addEntry (T {entries, ...}, span, kind, msg) =
      entries := (Entry.T {span = span, kind = kind, msg = msg}) :: !entries

   fun addError (errStrm, msg) =
      addEntry (errStrm, NONE, Kind.Error, msg)
   fun addErrorAt (errStrm, span, msg) =
      addEntry (errStrm, SOME span, Kind.Error, msg)

   fun addWarning (errStrm, msg) =
      addEntry (errStrm, NONE, Kind.Warning, msg)
   fun addWarningAt (errStrm, span, msg) =
      addEntry (errStrm, SOME span, Kind.Warning, msg)

   val errStrmReportFileCtl : (string * TextIO.outstream) Controls.control =
      Controls.genControl
      {name = "err-strm-report-file",
       pri = [],
       obscurity = 1,
       help = "report front-end errors to file",
       default = ("<stderr>", TextIO.stdErr)}
   val () =
      ControlRegistry.register Control.topRegistry
      {ctl = Controls.stringControl {tyName = "TextIO.outstream",
                                     fromString = fn f => SOME (f, TextIO.openOut f)
                                                          handle IO.Io _ => NONE,
                                     toString = fn (f, outStrm) => f}
                                    errStrmReportFileCtl,
       envName = NONE}

   fun report (T {srcMap, entries, ...}) =
      let
         val (_, outStrm) = Controls.get errStrmReportFileCtl
      in
         List.app (fn entry => (TextIO.output (outStrm, Entry.toString srcMap entry);
                                TextIO.output (outStrm, "\n")))
                  (ListMergeSort.sort (fn (entry1, entry2) =>
                                       Entry.compare (entry2, entry1) = LESS)
                                      (!entries))
      end

   fun anyMessages (T {entries, ...}) =
      not (List.null (!entries))

end
