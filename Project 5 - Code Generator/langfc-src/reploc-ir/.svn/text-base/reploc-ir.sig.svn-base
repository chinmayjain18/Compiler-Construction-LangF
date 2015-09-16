(* langfc-src/reploc-ir/reploc-ir.sig
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
 * Representation&Location intermediate representation in the
 * LangF compiler (langfc).
 *)

signature REPLOC_IR =
sig

   structure DaConRep :
      sig
         datatype t =
            UnboxedTag of int  (* integer tag for nullary *)
          | TaggedBox of int   (* heap-allocated args (with tag) *)
(*
          | Boxed              (* heap-allocated args (no tag) *)
          | Transparent        (* arg *)
*)
         val layout : t -> Layout.t
      end
   structure DaConName :
      sig
         include ID

         (* Pre-defined data constructors. *)
         val unit : t
         val truee : t
         val falsee : t
      end
   structure DaCon :
      sig
         datatype t = DaCon of {name: DaConName.t, rep: DaConRep.t}
         val layout : t -> Layout.t

         (* Pre-defined data constructors. *)
         val unit : t
         val truee : t
         val falsee : t
      end

   structure Func : ID

   structure Slot :
      sig
         type t = int
         val layout : t -> Layout.t
         val toString : t -> string

         structure Set : ORD_SET where type Key.ord_key = t
         structure Map : ORD_MAP where type Key.ord_key = t
         structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t
      end

   structure VarLoc :
      sig
         datatype t =
            Self of Func.t     (* variable is a function
                                * from current function's recursive group *)
          | Param              (* variable is current function's parameter *)
          | Local of Slot.t    (* variable is from current function's frame *)
          | Global of Slot.t   (* variable is from current function's environment *)
         val layout : t -> Layout.t
      end

   structure Pat :
      sig
         datatype t =
            P_DaCon of {dacon: DaCon.t,
                        slots: Slot.t list  (* store matched arguments in local slots *)
                       }
          | P_Var of {slot: Slot.t          (* store matched argument in local slot *)
                     }

         val layout : t -> Layout.t
      end

   structure Prim :
      sig
         datatype t =
            Add
          | Arg
          | Argc
          | Array
          | Concat
          | Div
          | Eq
          | Fail
          | Gt
          | Gte
          | Idx
          | Len
          | Lt
          | Lte
          | Mod
          | Mul
          | NEq
          | Neg
          | Print
          | Size
          | Sub
          | Subscript
          | Upd

         val layout : t -> Layout.t
      end

   structure Exp_MatchRule_Decl :
      sig
         datatype exp =
            E_Prim of {prim: Prim.t, args: exp list}
          | E_DaCon of {dacon: DaCon.t, args: exp list}
          | E_Apply of {func: exp, arg: exp}
          | E_Var of {loc: VarLoc.t}
          | E_Integer of IntInf.int
          | E_String of String.string
          | E_Let of {decl: decl, body: exp}
          | E_Case of {arg: exp, matchrules: matchrule list}

         and matchrule = MatchRule of {pat: Pat.t, body: exp}

         and decl =
            D_Val of {slot: Slot.t,  (* store evaluated expression in local slot *)
                      exp: exp
                     }
          | D_Fun of {env: VarLoc.t list,        (* shared environment *)
                      fun_decls: {slot: Slot.t,  (* store function closure in local slot *)
                                  func: Func.t
                                 } list
                     }
      end
   structure Exp :
      sig
         datatype t = datatype Exp_MatchRule_Decl.exp

         val layout : t -> Layout.t
      end
   structure MatchRule :
      sig
         datatype t = datatype Exp_MatchRule_Decl.matchrule

         val layout : t -> Layout.t
      end
   structure Decl :
      sig
         datatype t = datatype  Exp_MatchRule_Decl.decl

         val layout : t -> Layout.t
      end

   structure Function :
      sig
         datatype t = Function of {func: Func.t,
                                   nLocals: int,    (* maximum number of live local variables *)
                                   body: Exp.t}

         val layout : t -> Layout.t
      end

   structure Prog :
      sig
         datatype t = Prog of {main: Func.t,
                               functions: Function.t list}

         val layout : t -> Layout.t
         val output : TextIO.outstream * t -> unit
      end
end
