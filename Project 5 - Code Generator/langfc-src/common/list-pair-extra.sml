(* langfc-src/common/list-pair-extra.sml
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

structure ListPairExtra : LIST_PAIR_EXTRA =
struct

   open ListPair

   fun mapAndFoldl (f: 'a1 * 'a2 * 'b -> 'c * 'b) (acc: 'b) (l1: 'a1 list, l2: 'a2 list) : ('c list * 'b) =
      let
         val (rev_l, acc) =
            foldl (fn (x, y, (rev_l, acc)) =>
                   let val (z, acc) = f (x, y, acc)
                   in (z :: rev_l, acc)
                   end)
                  ([],acc)
                  (l1, l2)
      in
         (rev rev_l, acc)
      end
 
   fun mapAndFoldr (f: 'a1 * 'a2 * 'b -> 'c * 'b) (acc: 'b) (l1: 'a1 list, l2: 'a2 list) : ('c list * 'b) =
      let
         val (l, acc) =
            foldr (fn (x, y, (l, acc)) =>
                   let val (z, acc) = f (x, y, acc)
                   in (z :: l, acc)
                   end)
                  ([],acc)
                  (l1, l2)
      in
         (l, acc)
      end

end
