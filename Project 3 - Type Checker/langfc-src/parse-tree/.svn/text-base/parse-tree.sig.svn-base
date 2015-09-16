(* langfc-src/parse-tree/parse-tree.sig
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
 * Parse-tree representation in the LangF compiler (langfc).
 *)

signature PARSE_TREE =
sig

   (* Mark a parse-tree node with position information. *)
   structure Mark :
      sig
         type 'a t = {node: 'a, span: Source.Span.t}
      end

   structure TyVarName :
      sig
         datatype t = TyVarName of Atom.atom Mark.t

         val make : Atom.atom * Source.Span.t -> t
         val node : t -> Atom.atom
         val span : t -> Source.Span.t
         val layout : t -> Layout.t

         (* These data structures ignore the 'span' field
          * when using a TyVarName.t as a key.
          *)
         structure Set : ORD_SET where type Key.ord_key = t
         structure Map : ORD_MAP where type Key.ord_key = t
         structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t
      end
   structure TyConName :
      sig
         datatype t = TyConName of Atom.atom Mark.t

         val make : Atom.atom * Source.Span.t -> t
         val node : t -> Atom.atom
         val span : t -> Source.Span.t
         val layout : t -> Layout.t

         (* Pre-defined type constructors. *)
         val unit : t
         val bool : t
         val integer : t
         val string : t
         val array : t

         (* These data structures ignore the 'span' field
          * when using a TyConName.t as a key.
          *)
         structure Set : ORD_SET where type Key.ord_key = t
         structure Map : ORD_MAP where type Key.ord_key = t
         structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t
      end
   structure DaConName :
      sig
         datatype t = DaConName of Atom.atom Mark.t

         val make : Atom.atom * Source.Span.t -> t
         val node : t -> Atom.atom
         val span : t -> Source.Span.t
         val layout : t -> Layout.t

         (* Pre-defined data constructors. *)
         val unit : t
         val truee : t
         val falsee : t

         (* These data structures ignore the 'span' field
          * when using a DaConName.t as a key.
          *)
         structure Set : ORD_SET where type Key.ord_key = t
         structure Map : ORD_MAP where type Key.ord_key = t
         structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t
      end
   structure VarName :
      sig
         datatype t = VarName of Atom.atom Mark.t

         val make : Atom.atom * Source.Span.t -> t
         val node : t -> Atom.atom
         val span : t -> Source.Span.t
         val layout : t -> Layout.t

         (* Pre-defined variables. *)
         val argc : t
         val arg : t
         val fail : t
         val print : t
         val size : t
         val subscript : t
         val array : t

         (* These data structures ignore the 'span' field
          * when using a VarName.t as a key.
          *)
         structure Set : ORD_SET where type Key.ord_key = t
         structure Map : ORD_MAP where type Key.ord_key = t
         structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t
      end

   structure Type :
      sig
         datatype t = Type of node Mark.t
         and node =
            T_TyFn of TyVarName.t * t
          | T_Fn of t * t
          | T_TyCon of TyConName.t * t list
          | T_TyVar of TyVarName.t

         val make : node * Source.Span.t -> t
         val node : t -> node
         val span : t -> Source.Span.t
         val layout : t -> Layout.t
      end

   structure Param :
      sig
         datatype t = Param of node Mark.t
         and node =
            P_VarName of VarName.t * Type.t
          | P_TyVarName of TyVarName.t

         val make : node * Source.Span.t -> t
         val node : t -> node
         val span : t -> Source.Span.t
         val layout : t -> Layout.t
         val layouts : t list -> Layout.t
      end

   structure SimplePat :
      sig
         datatype t = SimplePat of node Mark.t
         and node =
            P_VarName of VarName.t
          | P_Wild

         val make : node * Source.Span.t -> t
         val node : t -> node
         val span : t -> Source.Span.t
         val layout : t -> Layout.t
      end
   structure Pat :
      sig
         datatype t = Pat of node Mark.t
         and node =
            P_DaCon of DaConName.t * Type.t list * SimplePat.t list
          | P_SimplePat of SimplePat.t

         val make : node * Source.Span.t -> t
         val node : t -> node
         val span : t -> Source.Span.t
         val layout : t -> Layout.t
      end

   structure TernOp :
      sig
         datatype t =
            Upd
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
          | Idx
         val layout : t -> Layout.t
      end
   structure UnOp :
      sig
         datatype t = Neg | Len
         val layout : t -> Layout.t
      end

   (*
    * Unfortunately, SML/NJ deviates from the Definition of Standard
    * ML and gives the error "dependency cycle in instantiate" if one
    * tries to specify the mutual recursion via sharing constraints;
    * see the desired signature (accepted by MLton) at the bottom of
    * the file.  Instead, we must specify the whole mutually recursive
    * datatype and then rebind/replicate into separate structures.
    *)
   structure Exp_MatchRule_ApplyArg_Decl :
      sig
         datatype exp = Exp of exp_node Mark.t
         and exp_node =
            E_Fn of Param.t list * exp
          | E_If of exp * exp * exp
          | E_Orelse of exp * exp
          | E_Andalso of exp * exp
          | E_Constraint of exp * Type.t
          | E_TernOp of TernOp.t * exp * exp * exp
          | E_BinOp of BinOp.t * exp * exp
          | E_UnOp of UnOp.t * exp
          | E_DaCon of DaConName.t * Type.t list * exp list
          | E_Apply of exp * applyarg
          | E_VarName of VarName.t
          | E_Integer of IntInf.int
          | E_String of String.string
          | E_Seq of exp list
          | E_Let of decl list * exp list
          | E_Case of exp * matchrule list
         and matchrule = MatchRule of (Pat.t * exp) Mark.t
         and applyarg = ApplyArg of applyarg_node Mark.t
         and applyarg_node =
            A_Exp of exp
          | A_Type of Type.t
         and decl = Decl of decl_node Mark.t
         and decl_node =
            D_Type of TyConName.t * TyVarName.t list * Type.t
          | D_Data of (TyConName.t * TyVarName.t list *
                       (DaConName.t * Type.t list) list) list
          | D_Val of SimplePat.t * Type.t option * exp
          | D_Fun of (VarName.t * Param.t list * Type.t * exp) list
      end
   structure Exp :
      sig
         datatype t = datatype Exp_MatchRule_ApplyArg_Decl.exp
         datatype node = datatype Exp_MatchRule_ApplyArg_Decl.exp_node

         val make : node * Source.Span.t -> t
         val node : t -> node
         val span : t -> Source.Span.t
         val layout : t -> Layout.t
      end
   structure MatchRule :
      sig
         datatype t = datatype Exp_MatchRule_ApplyArg_Decl.matchrule
         type node = Pat.t * Exp.t

         val make : node * Source.Span.t -> t
         val node : t -> node
         val span : t -> Source.Span.t
         val layout : t -> Layout.t
      end
   structure ApplyArg :
      sig
         datatype t = datatype Exp_MatchRule_ApplyArg_Decl.applyarg
         datatype node = datatype Exp_MatchRule_ApplyArg_Decl.applyarg_node

         val make : node * Source.Span.t -> t
         val node : t -> node
         val span : t -> Source.Span.t
         val layout : t -> Layout.t
      end
   structure Decl :
      sig
         datatype t = datatype Exp_MatchRule_ApplyArg_Decl.decl
         datatype node = datatype Exp_MatchRule_ApplyArg_Decl.decl_node

         val make : node * Source.Span.t -> t
         val node : t -> node
         val span : t -> Source.Span.t
         val layout : t -> Layout.t
      end

   structure Prog :
      sig
         type node = Decl.t list * Exp.t
         datatype t = Prog of node Mark.t

         val make : node * Source.Span.t -> t
         val node : t -> (Decl.t list * Exp.t)
         val span : t -> Source.Span.t
         val layout : t -> Layout.t
         val output : TextIO.outstream * t -> unit
      end

end

(*
 * The Definition of Standard ML admits the following signature, which
 * concisely specifies the mutual recursion and avoids introducing the
 * whole mutually recursive datatype in the signature.
 *

   structure Exp :
      sig
         type matchrule
         type applyarg
         type decl
         datatype t = Exp of node Mark.t
         and node =
            E_Let of decl list * t list
          | E_If of t * t * t
          | E_Case of t * matchrule list
          | E_DaCon of DaConName.t * Type.t list * t list
          | E_Fn of Param.t list * t
          | E_Orelse of t * t
          | E_Andalso of t * t
          | E_Constraint of t * Type.t
          | E_TernOp of TernOp.t * t * t * t
          | E_BinOp of BinOp.t * t * t
          | E_UnOp of UnOp.t * t
          | E_Apply of t * applyarg
          | E_VarName of VarName.t
          | E_Seq of t list
          | E_Integer of IntInf.int
          | E_String of String.string

         val make : node * Source.Span.t -> t
         val node : t -> node
         val span : t -> Source.Span.t
         val layout : t -> Layout.t
      end
   structure MatchRule :
      sig
         type exp
         type node = Pat.t * exp
         datatype t = MatchRule of node Mark.t

         val make : node * Source.Span.t -> t
         val node : t -> node
         val span : t -> Source.Span.t
         val layout : t -> Layout.t
      end
   structure ApplyArg :
      sig
         type exp
         datatype t = ApplyArg of node Mark.t
         and node =
            A_Exp of exp
          | A_Type of Type.t

         val make : node * Source.Span.t -> t
         val node : t -> node
         val span : t -> Source.Span.t
         val layout : t -> Layout.t
      end
   structure Decl :
      sig
         type exp
         datatype t = Decl of node Mark.t
         and node =
            D_Type of TyConName.t * TyVarName.t list * Type.t
          | D_Data of (TyConName.t * TyVarName.t list *
                       (DaConName.t * Type.t list) list) list
          | D_Val of SimplePat.t * Type.t option * exp
          | D_Fun of (VarName.t * Param.t list * Type.t * exp) list

         val make : node * Source.Span.t -> t
         val node : t -> node
         val span : t -> Source.Span.t
         val layout : t -> Layout.t
     end
   sharing type Exp.t = MatchRule.exp = ApplyArg.exp = Decl.exp
   sharing type MatchRule.t = Exp.matchrule
   sharing type ApplyArg.t = Exp.applyarg
   sharing type Decl.t = Exp.decl

 *
 *)
