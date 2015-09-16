(* langfc-src/common/stamp.sig
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
 * Stamps are locally unique objects used in the compiler to
 * distinguish different identifiers (variables, etc.).
 *)

signature STAMP =
sig

   type t

   val new : unit -> t

   val compare : t * t -> order
   val equals : t * t -> bool
   val hash : t -> word

   val toString : t -> string

   structure Set : ORD_SET where type Key.ord_key = t
   structure Map : ORD_MAP where type Key.ord_key = t
   structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

end

