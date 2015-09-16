(* langfc-src/core-ir/core-ir.sig
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
 * Core intermediate representation in the LangF compiler (langfc).
 *)

signature CORE_IR =
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

         val layout : t -> Layout.t

         val equals : t * t -> bool
         val freeIds : t -> (TyVar.Set.set * TyCon.Set.set)
         val freeTyVars : t -> TyVar.Set.set
         val freeTyCons : t -> TyCon.Set.set
         val subst : t * (t * TyVar.t) -> t
         val substs : t * (t * TyVar.t) list -> t
         val substL : t list * (t * TyVar.t) -> t list
         val substsL : t list * (t * TyVar.t) list -> t list
         val alphaRename : t -> t

         val bool : t
         val unit : t
         val integer : t
         val string : t
         val array : t -> t
      end

   structure Param :
      sig
         datatype t =
            P_Var of {var: Var.t, var_ty: Type.t}
          | P_TyVar of {tyvar: TyVar.t}

         val layout : t -> Layout.t
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

         val layout : t -> Layout.t
      end

   structure Exp_Lam_ApplyArg_MatchRule_Decl :
      sig
         datatype exp = Exp of {node: exp_node, ty: Type.t}
         and exp_node =
            E_Fn of {lam: lam}
          | E_Prim of {prim: Prim.t, tyargs: Type.t list, args: exp list}
          | E_DaCon of {dacon: DaCon.t, tyargs: Type.t list, args: exp list}
          | E_Apply of {func: exp, applyarg: applyarg}
          | E_Var of {var: Var.t}
          | E_Integer of IntInf.int
          | E_String of String.string
          | E_Let of {decl: decl, body: exp}
          | E_Case of {arg: exp, matchrules: matchrule list}

         and lam = Lam of {param: Param.t, body: exp}

         and applyarg =
            A_Exp of exp
          | A_Type of Type.t

         and matchrule = MatchRule of {pat: Pat.t, body: exp}

         and decl =
            D_Data of {data_decls: {tycon: TyCon.t,
                                    tyvars: TyVar.t list,
                                    dacon_decls: {dacon: DaCon.t,
                                                  arg_tys: Type.t list} list} list}
          | D_Val of {var: Var.t, var_ty: Type.t, exp: exp}
          | D_Fun of {fun_decls: {func: Var.t, func_ty: Type.t, lam: lam} list}
      end
   structure Exp :
      sig
         datatype t = datatype Exp_Lam_ApplyArg_MatchRule_Decl.exp
         datatype node = datatype Exp_Lam_ApplyArg_MatchRule_Decl.exp_node

         val make : node * Type.t -> t
         val node : t -> node
         val ty : t -> Type.t
         val layout : t -> Layout.t
         val layoutNode : node -> Layout.t

         val freeIds : t -> (TyVar.Set.set * TyCon.Set.set * DaCon.Set.set * Var.Set.set)
         val freeTyVars : t -> TyVar.Set.set
         val freeTyCons : t -> TyCon.Set.set
         val freeDaCons : t -> DaCon.Set.set
         val freeVars : t -> Var.Set.set
      end
   structure Lam :
      sig
         datatype t = datatype Exp_Lam_ApplyArg_MatchRule_Decl.lam

         val layout : t -> Layout.t
      end
   structure ApplyArg :
      sig
         datatype t = datatype Exp_Lam_ApplyArg_MatchRule_Decl.applyarg

         val layout : t -> Layout.t
      end
   structure MatchRule :
      sig
         datatype t = datatype Exp_Lam_ApplyArg_MatchRule_Decl.matchrule

         val layout : t -> Layout.t
      end
   structure Decl :
      sig
         datatype t = datatype Exp_Lam_ApplyArg_MatchRule_Decl.decl

         val layout : t -> Layout.t

         val freeIds : t -> (TyVar.Set.set * TyCon.Set.set * DaCon.Set.set * Var.Set.set)
         val freeTyVars : t -> TyVar.Set.set
         val freeTyCons : t -> TyCon.Set.set
         val freeDaCons : t -> DaCon.Set.set
         val freeVars : t -> Var.Set.set
      end

   structure Prog :
      sig
         datatype t = Prog of {decls: Decl.t list, exp: Exp.t}

         val layout : t -> Layout.t
         val output : TextIO.outstream * t -> unit
      end
end
