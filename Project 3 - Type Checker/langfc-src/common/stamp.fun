(* langfc-src/common/stamp.fun
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

functor Stamp () :> STAMP =
struct

   datatype t = T of {id : Word.word}

   val cnt = ref 0w0

   fun new () =
      let
         val w = !cnt
         val () = cnt := !cnt + 0w1;
      in
         T {id = w}
      end

   fun equals (T {id = id1, ...}, T {id = id2, ...}) = id1 = id2
   fun compare (T {id = id1, ...}, T {id = id2, ...}) = Word.compare (id1, id2)
   fun hash (T {id, ...}) = id

   fun toString (T {id, ...}) =
      concat ["__", StringCvt.padLeft #"0" 4 (Word.toString id)]

   structure OrdKey =
      struct
         type ord_key = t
         val compare = compare
      end
   structure Map = RedBlackMapFn (OrdKey)
   structure Set = RedBlackSetFn (OrdKey)

   structure HashKey =
      struct
        type hash_key = t
        val hashVal = hash
        val sameKey = equals
      end
    structure Tbl = HashTableFn (HashKey)

end
