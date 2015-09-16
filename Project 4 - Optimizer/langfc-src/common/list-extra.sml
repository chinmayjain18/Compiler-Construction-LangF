(* langfc-src/common/list-extra.sml
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

structure ListExtra : LIST_EXTRA =
struct

   open List

   fun mapAndFoldl (f: 'a * 'b -> 'c * 'b) (acc: 'b) (l: 'a list) : ('c list * 'b) =
      let
         val (rev_l, acc) =
            foldl (fn (x, (rev_l, acc)) =>
                   let val (y, acc) = f (x, acc)
                   in (y :: rev_l, acc)
                   end)
                  ([],acc)
                  l
      in
         (rev rev_l, acc)
      end

   fun mapAndFoldr (f: 'a * 'b -> 'c * 'b) (acc: 'b) (l: 'a list) : ('c list * 'b) =
      let
         val (l, acc) =
            foldr (fn (x, (l, acc)) =>
                   let val (y, acc) = f (x, acc)
                   in (y :: l, acc)
                   end)
                  ([],acc)
                  l
      in
         (l, acc)
      end

end
