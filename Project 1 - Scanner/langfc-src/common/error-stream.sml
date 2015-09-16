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
         datatype t = T of {kind: Kind.t,
                            msg: string}

         fun toString (T {kind, msg}) =
            concat ["<?>",
                    " ",
                    Kind.toString kind,
                    ": ",
                    msg]
      end

   datatype t =
      T of {srcFile : string,
            entries: Entry.t list ref}

   fun mkErrorStream srcFile =
      T {srcFile = srcFile,
         entries = ref []}

   fun addEntry (T {entries, ...}, kind, msg) =
      entries := (Entry.T {kind = kind, msg = msg}) :: !entries

   fun addError (errStrm, msg) =
      addEntry (errStrm, Kind.Error, msg)

   fun addWarning (errStrm, msg) =
      addEntry (errStrm, Kind.Warning, msg)

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

   fun report (T {entries, ...}) =
      let
         val (_, outStrm) = Controls.get errStrmReportFileCtl
      in
         List.app (fn entry => (TextIO.output (outStrm, Entry.toString entry);
                                TextIO.output (outStrm, "\n")))
                  (List.rev (!entries))
      end

   fun anyMessages (T {entries, ...}) =
      not (List.null (!entries))

end
