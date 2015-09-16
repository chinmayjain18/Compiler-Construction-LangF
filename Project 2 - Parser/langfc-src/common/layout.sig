(* langfc-src/common/layout.sig
 *
 * MODIFIED 2009 Matthew Fluet (http://tti-c.org/fluet)
 * All rights reserved.
 *
 * University of Chicago
 * CMSC 22610
 * Winter 2009
 *
 *)

(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

signature LAYOUT =
   sig
      type t

      (* layout the objects on separate lines *)
      val align: t list -> t
      val alignPrefix: t list * string -> t
      val array: t array -> t
      val empty: t
      val ignore: 'a -> t
      val isEmpty: t -> bool
      val makeOutput: ('a -> t) -> TextIO.outstream * 'a -> unit
      val makeOutputWidth: int * ('a -> t) -> TextIO.outstream * 'a -> unit
      (* layout the objects on separate lines, if necessary *)
      val mayAlign: t list -> t
      val namedRecord: string * (string * t) list -> t
      (* indent the entire object *)
      val indent: t * int -> t
      val list: t list -> t
      val output: TextIO.outstream * t -> unit
      val outputWidth: TextIO.outstream * int * t -> unit
      val paren: t -> t
      val record: (string * t) list -> t
      val schemeList: t list -> t
      (* put string between elements *)
      val separate: t list * string -> t list
      (* adds string at beginning of all objects except first *)
      val separateLeft: t list * string -> t list
      (* adds string at the end of all objects except last *)
      val separateRight: t list * string -> t list
      (* layout the objects on the same line *)
      val seq: t list -> t
      val space: t
      (* convert a string to a layout object *)
      val str: string -> t
      val toString: t -> string
      val tuple: t list -> t
      val tuple2: ('a -> t) * ('b -> t) -> 'a * 'b -> t
      val tuple3: ('a -> t) * ('b -> t) * ('c -> t) -> 'a * 'b * 'c -> t
      val tuple4: ('a -> t) * ('b -> t) * ('c -> t) * ('d -> t)
         -> 'a * 'b * 'c * 'd -> t
      val tuple5: ('a -> t) * ('b -> t) * ('c -> t) * ('d -> t) * ('e -> t)
         -> ('a * 'b * 'c * 'd * 'e) -> t
      val vector: t vector -> t
   end
