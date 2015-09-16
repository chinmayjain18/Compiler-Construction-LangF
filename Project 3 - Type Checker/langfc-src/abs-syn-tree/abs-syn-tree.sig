(* langfc-src/abs-syn-tree/abs-syn-tree.sig
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
 * Abstract syntax tree representation in the LangF compiler (langfc).
 *)

signature ABS_SYN_TREE =
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

   structure Var :
      sig
         include ID

         (* Pre-defined variables. *)
         val argc : t
         val arg : t
         val fail : t
         val print : t
         val size : t
         val subscript : t
         val array : t
      end

   structure Type :
      sig
         datatype t =
            T_TyFn of TyVar.t * t
          | T_Fn of t * t
          | T_TyCon of TyCon.t * t list
          | T_TyVar of TyVar.t

         val layout : t -> Layout.t

         val equals : t * t -> bool
         val freeTyVars : t -> TyVar.Set.set
         val subst : t * (t * TyVar.t) -> t
         val substs : t * (t * TyVar.t) list -> t
         val substL : t list * (t * TyVar.t) -> t list
         val substsL : t list * (t * TyVar.t) list -> t list
         val alphaRename : t -> t
         val tycons : t -> TyCon.Set.set

         val bool : t
         val unit : t
         val integer : t
         val string : t
         val array : t -> t
      end

   structure Param :
      sig
         datatype t =
            P_Var of Var.t * Type.t
          | P_TyVar of TyVar.t

         val layout : t -> Layout.t
         val layouts : t list -> Layout.t
      end

   structure SimplePat :
      sig
         datatype t =
            P_Var of Var.t * Type.t
          | P_Wild of Type.t

         val layout : t -> Layout.t
      end
   structure Pat :
      sig
         datatype t =
            P_DaCon of DaCon.t * Type.t list * SimplePat.t list
          | P_SimplePat of SimplePat.t

         val layout : t -> Layout.t
      end

   structure TernOp :
      sig
         datatype t =
            Upd of Type.t
         val layout : t -> Layout.t * Layout.t
      end
   structure BinOp :
      sig
         datatype t =
            Eq
          | NEq
          | Lt
          | Lte
          | Gt
          | Gte
          | Concat
          | Add
          | Sub
          | Mul
          | Div
          | Mod
          | Idx of Type.t
         val layout : t -> Layout.t
      end
   structure UnOp :
      sig
         datatype t = 
            Neg 
          | Len of Type.t
         val layout : t -> Layout.t
      end

   (*
    * Unfortunately, SML/NJ deviates from the Definition of Standard
    * ML and gives the error "dependency cycle in instantiate" if one
    * tries to specify the mutual recursion via sharing constraints.
    * Instead, we must specify the whole mutually recursive datatype
    * and then rebind/replicate into separate structures.
    *)
   structure Exp_MatchRule_ApplyArg_Decl :
      sig
         datatype exp = Exp of {node: exp_node, ty: Type.t}
         and exp_node =
            E_Fn of Param.t list * exp
          | E_If of exp * exp * exp
          | E_Orelse of exp * exp
          | E_Andalso of exp * exp
          | E_TernOp of TernOp.t * exp * exp * exp
          | E_BinOp of BinOp.t * exp * exp
          | E_UnOp of UnOp.t * exp
          | E_DaCon of DaCon.t * Type.t list * exp list
          | E_Apply of exp * applyarg
          | E_Var of Var.t
          | E_Integer of IntInf.int
          | E_String of String.string
          | E_Seq of exp list
          | E_Let of decl list * exp list
          | E_Case of exp * matchrule list
         and matchrule = MatchRule of (Pat.t * exp)
         and applyarg =
            A_Exp of exp
          | A_Type of Type.t
         and decl =
            D_Data of (TyCon.t * TyVar.t list *
                       (DaCon.t * Type.t list) list) list
          | D_Val of SimplePat.t * exp
          | D_Fun of (Var.t * Param.t list * Type.t * exp) list
      end
   structure Exp :
      sig
         datatype node = datatype Exp_MatchRule_ApplyArg_Decl.exp_node
         datatype t = datatype Exp_MatchRule_ApplyArg_Decl.exp

         val make : node * Type.t -> t
         val node : t -> node
         val ty : t -> Type.t
         val layout : t -> Layout.t
         val layoutNode : node -> Layout.t

         val unit : t
      end
   structure MatchRule :
      sig
         datatype t = datatype Exp_MatchRule_ApplyArg_Decl.matchrule

         val layout : t -> Layout.t
      end
   structure ApplyArg :
      sig
         datatype t = datatype Exp_MatchRule_ApplyArg_Decl.applyarg

         val layout : t -> Layout.t
      end
   structure Decl :
      sig
         datatype t = datatype Exp_MatchRule_ApplyArg_Decl.decl

         val layout : t -> Layout.t
      end

   structure Prog :
      sig
         datatype t = Prog of Decl.t list * Exp.t

         val layout : t -> Layout.t
         val output : TextIO.outstream * t -> unit
      end

end
