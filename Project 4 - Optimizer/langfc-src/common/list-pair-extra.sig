(* langfc-src/common/list-pair-extra.sig
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
 * Extra convenience functions on lists.
 *)

signature LIST_PAIR_EXTRA =
sig

   include LIST_PAIR

   val mapAndFoldl : ('a1 * 'a2 * 'b -> 'c * 'b) -> 'b -> 'a1 list * 'a2 list -> 'c list * 'b
   val mapAndFoldr : ('a1 * 'a2 * 'b -> 'c * 'b) -> 'b -> 'a1 list * 'a2 list -> 'c list * 'b
end
