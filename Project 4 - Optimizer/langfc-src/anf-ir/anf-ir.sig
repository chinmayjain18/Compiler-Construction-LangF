(* langfc-src/anf-ir/anf-ir.sig
 *
 * COPYRIGHT (c) 2015 Matthew Fluet (http://www.cs.rit.edu/~mtf)
 * All rights reserved.
 *
 * Rochester Institute of Technology
 * CSCI-742
 * S20135,S20145
 *
 * A-normal form intermediate representation in the LangF compiler (langfc).
 *)

signature ANF_IR =
sig

   structure TyVar : ID

   structure TyCon :
      sig
         include ID

         (* Pre-defined type constructors. *)
         val unit : t
         val bool : t
         val integer : t
         val string : t
         val array : t
      end
   structure DaCon :
      sig
         include ID

         (* Pre-defined data constructors. *)
         val unit : t
         val truee : t
         val falsee : t
      end

   structure Var : ID

   structure Type :
      sig
         datatype t =
            T_TyFn of {tyvar: TyVar.t, res: t}
          | T_Fn of {arg: t, res: t}
          | T_TyCon of {tycon: TyCon.t, tyargs: t list}
          | T_TyVar of {tyvar: TyVar.t}

         val compare : t * t -> order
         val equals : t * t -> bool
         val hash : t -> word

         val size : t -> int

         val layout : t -> Layout.t

         val freeIds : t -> (TyVar.Set.set * TyCon.Set.set * DaCon.Set.set * Var.Set.set)
         val freeTyVars : t -> TyVar.Set.set
         val freeTyCons : t -> TyCon.Set.set
         val freeDaCons : t -> DaCon.Set.set
         val freeVars : t -> Var.Set.set

         val clone : t -> t

         val tySubst : t * (t * TyVar.t) -> t
         val tySubstL : t list * (t * TyVar.t) -> t list
         val tySubsts : t * (t * TyVar.t) list -> t
         val tySubstsL : t list * (t * TyVar.t) list -> t list

         structure Set : ORD_SET where type Key.ord_key = t
         structure Map : ORD_MAP where type Key.ord_key = t
         structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t

         val bool : t
         val unit : t
         val integer : t
         val string : t
         val array : t -> t
      end

   structure Pat :
      sig
         datatype t =
            P_DaCon of {dacon: DaCon.t,
                        tyargs: Type.t list,
                        binds: {var: Var.t, var_ty: Type.t} list}
          | P_Var of {var: Var.t, var_ty: Type.t}

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

         val compare : t * t -> order
         val equals : t * t -> bool
         val hash : t -> word

         val layout : t -> Layout.t
      end

   structure Exp_RHS_Lam_MatchRule_Decl :
      sig
         datatype exp = Exp of {decls: decl list,
                                var: Var.t,
                                ty: Type.t}

         and rhs =
            R_Fn of {lam: lam}
          | R_Prim of {prim: Prim.t, tyargs: Type.t list, args: Var.t list}
          | R_DaCon of {dacon: DaCon.t, tyargs: Type.t list, args: Var.t list}
          | R_VApply of {func: Var.t, arg: Var.t}
          | R_TApply of {func: Var.t, tyarg: Type.t}
          | R_Var of {var: Var.t}
          | R_Integer of IntInf.int
          | R_String of String.string
          | R_Case of {arg: Var.t, matchrules: matchrule list}

         and lam =
            L_VLam of {var: Var.t, var_ty: Type.t, body: exp}
          | L_TLam of {tyvar: TyVar.t, body: exp}

         and matchrule = MatchRule of {pat: Pat.t, body: exp}

         and decl =
            D_Data of {data_decls: {tycon: TyCon.t,
                                    tyvars: TyVar.t list,
                                    dacon_decls: {dacon: DaCon.t,
                                                  arg_tys: Type.t list} list} list}
          | D_Val of {var: Var.t, var_ty: Type.t, rhs: rhs}
          | D_Fun of {fun_decls: {func: Var.t, func_ty: Type.t, lam: lam} list}
      end
   structure Exp :
      sig
         datatype t = datatype Exp_RHS_Lam_MatchRule_Decl.exp

         val size : t -> int

         val layout : t -> Layout.t

         val freeIds : t -> (TyVar.Set.set * TyCon.Set.set * DaCon.Set.set * Var.Set.set)
         val freeTyVars : t -> TyVar.Set.set
         val freeTyCons : t -> TyCon.Set.set
         val freeDaCons : t -> DaCon.Set.set
         val freeVars : t -> Var.Set.set

         val clone : t -> t
         val tySubst : t * (Type.t * TyVar.t) -> t
      end
   structure RHS :
      sig
         datatype t = datatype Exp_RHS_Lam_MatchRule_Decl.rhs

         val size : t -> int

         val layout : t -> Layout.t

         val freeIds : t -> (TyVar.Set.set * TyCon.Set.set * DaCon.Set.set * Var.Set.set)
         val freeTyVars : t -> TyVar.Set.set
         val freeTyCons : t -> TyCon.Set.set
         val freeDaCons : t -> DaCon.Set.set
         val freeVars : t -> Var.Set.set

         val clone : t -> t
         val tySubst : t * (Type.t * TyVar.t) -> t
      end
   structure Lam :
      sig
         datatype t = datatype Exp_RHS_Lam_MatchRule_Decl.lam

         val size : t -> int

         val layout : t -> Layout.t

         val freeIds : t -> (TyVar.Set.set * TyCon.Set.set * DaCon.Set.set * Var.Set.set)
         val freeTyVars : t -> TyVar.Set.set
         val freeTyCons : t -> TyCon.Set.set
         val freeDaCons : t -> DaCon.Set.set
         val freeVars : t -> Var.Set.set

         val clone : t -> t
         val tySubst : t * (Type.t * TyVar.t) -> t
      end
   structure MatchRule :
      sig
         datatype t = datatype Exp_RHS_Lam_MatchRule_Decl.matchrule

         val size : t -> int

         val layout : t -> Layout.t
      end
   structure Decl :
      sig
         datatype t = datatype Exp_RHS_Lam_MatchRule_Decl.decl

         val size : t -> int

         val layout : t -> Layout.t

         val freeIds : t -> (TyVar.Set.set * TyCon.Set.set * DaCon.Set.set * Var.Set.set)
         val freeTyVars : t -> TyVar.Set.set
         val freeTyCons : t -> TyCon.Set.set
         val freeDaCons : t -> DaCon.Set.set
         val freeVars : t -> Var.Set.set

         val tySubst : t * (Type.t * TyVar.t) -> t
         val tySubstL : t list * (Type.t * TyVar.t) -> t list
      end

   structure Prog :
      sig
         datatype t = Prog of {decls: Decl.t list, var: Var.t, ty: Type.t}

         val size : t -> int

         val layout : t -> Layout.t
         val output : TextIO.outstream * t -> unit
      end
end
