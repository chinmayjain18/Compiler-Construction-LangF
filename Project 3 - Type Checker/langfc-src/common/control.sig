(* langfc-src/common/control.sig
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
 * Global controls for the LangF compiler (langfc).
 *)

signature CONTROL =
sig

   (* the top-level registery of the compiler *)
   val topRegistry : ControlRegistry.registry

   (* controls verbosity of diagnostic messages. *)
   val verbose : bool Controls.control

   (* base name for pass output files; set based on compilation unit. *)
   val baseName : string Controls.control

   (* wrap a 'pre -> 'post pass with a tracing diagnostic,
    * controlled by the "verbose" control.
    *)
    val mkTracePass : {msgPre: ('pre -> string) option,
                       msgPost: ('post -> string) option,
                       passName: string,
                       pass: 'pre -> 'post
                       } -> 'pre -> 'post

    (* wrap a 'pre -> 'post pass with debug output.
     *)
    val mkKeepPass : {keepPre: {output: TextIO.outstream * 'pre -> unit,
                                ext: string} option,
                      keepPost: {output: TextIO.outstream * 'post -> unit,
                                 ext: string} option,
                      passName: string,
                      pass: 'pre -> 'post
                      } -> 'pre -> 'post

    val mkKeepPassSimple : {keep: {output: TextIO.outstream * 'a -> unit,
                                   ext: string} option,
                            passName: string,
                            pass: 'a -> 'a
                            } -> 'a -> 'a

    (* wrap a 'pre -> 'post pass with debug output,
     * controlled by a new "keep" control.
     *)
    val mkKeepCtlPass : {keepPre: {output: TextIO.outstream * 'pre -> unit,
                                   ext: string} option,
                         keepPost: {output: TextIO.outstream * 'post -> unit,
                                    ext: string} option,
                         passName: string,
                         pass: 'pre -> 'post
                         } -> 'pre -> 'post

    val mkKeepCtlPassSimple : {keep: {output: TextIO.outstream * 'a -> unit,
                                      ext: string} option,
                               passName: string,
                               pass: 'a -> 'a
                               } -> 'a -> 'a

    (* extend a 'pre -> 'post pass with a stop output,
     * controlled by a new "stop" control.
     *)
    val mkStopCtlPass : {passName: string,
                         pass: 'pre -> 'post
                         } -> 'pre -> 'post * bool

    (* report all registered controls. *)
    val report : TextIO.outstream -> int option -> unit

end
