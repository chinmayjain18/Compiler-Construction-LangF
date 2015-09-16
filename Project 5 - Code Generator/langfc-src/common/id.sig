(* langfc-src/common/id.sig
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
 * Identifiers are names in the abstract syntax tree.
 *)

signature ID =
sig

   (* Type of identifiers.
    * Most identifiers will be printed with a base name and a
    * uniquifying hexidecimal suffix.
    *)
   type t

   (* Create a fresh identifier with a base name.  The new identifier
    * will not be equal to any previously created identifiers.  It
    * will be printed with a uniquifying suffix.
    *)
   val new : string -> t

   (* Create a fresh identifier with a base name.  The new identifier
    * will not be equal to any previously created identifiers.  It
    * will not be printed with a uniquifying suffix.
    *)
   val newSpecial : string -> t

   val compare : t * t -> order
   val equals : t * t -> bool
   val hash : t -> word

   (* Return the base name of an identifier. *)
   val name : t -> string

   val layout : t -> Layout.t
   val toString : t -> string

   structure Set : ORD_SET where type Key.ord_key = t
   structure Map : ORD_MAP where type Key.ord_key = t
   structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

end
