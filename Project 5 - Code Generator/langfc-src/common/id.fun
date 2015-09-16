(* langfc-src/common/id.fun
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

structure IdControls =
struct

   val canonicalIdentifiersCtl : bool Controls.control =
      Controls.genControl
      {name = "canonical-ids",
       pri = [],
       obscurity = 1,
       help = "print identifiers in a canonical form (independent of base name and creation order)",
       default = false}
   val () =
      ControlRegistry.register Control.topRegistry
      {ctl = Controls.stringControl ControlUtil.Cvt.bool canonicalIdentifiersCtl,
       envName = NONE}
      
end

functor Id (val defaultName: string) :> ID =
struct
   open IdControls

   structure Stamp = Stamp ()
   structure CStamp = Stamp ()

   datatype t = T of {name: string,
                      stamp: Stamp.t,
                      cstamp: CStamp.t option ref,
                      suffixp: bool}

   local
      fun mk sel (T id) = sel id
   in
      val name = mk #name
      val stamp = mk #stamp
      val cstamp = mk #cstamp
      val suffixp = mk #suffixp
   end

   fun new name =
      T {name = name, stamp = Stamp.new (), cstamp = ref NONE, suffixp = true}

   fun newSpecial name =
      T {name = name, stamp = Stamp.new (), cstamp = ref NONE, suffixp = false}

   fun compare (id1, id2) = Stamp.compare (stamp id1, stamp id2)
   fun equals (id1, id2) = Stamp.equals (stamp id1, stamp id2)
   fun hash id = Stamp.hash (stamp id)

   fun toString id =
      if suffixp id
         then if Controls.get canonicalIdentifiersCtl
                 then (case !(cstamp id) of 
                          NONE => (cstamp id := SOME (CStamp.new ()); toString id)
                        | SOME s => concat [defaultName, CStamp.toString s])
              else concat [name id, Stamp.toString (stamp id)]
      else name id
   fun layout id = Layout.str (toString id)

   structure OrdKey =
      struct
         type ord_key = t
         val compare = compare
      end
   structure Set = RedBlackSetFn (OrdKey)
   structure Map = RedBlackMapFn (OrdKey)

   structure HashKey =
      struct
        type hash_key = t
        val hashVal = hash
        val sameKey = equals
      end
    structure Tbl = HashTableFn (HashKey)

end
