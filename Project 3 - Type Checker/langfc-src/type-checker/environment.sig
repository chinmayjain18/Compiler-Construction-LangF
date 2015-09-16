(* langfc-src/type-checker/environment.sig
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
 * Environments for front-end type-checker in the LangF compiler
 * (langfc).
 *)

signature ENVIRONMENT =
sig
   (* The combined environment is composed of a type variable
    * environment, a type constructor environment, a data constructor
    * environment, and a variable environment.  Each individual
    * environment has its own domain and co-domain.
    *)
   structure TyVarEnv :
      sig
         type dom = ParseTree.TyVarName.t
         type cod = {tyvar: AbsSynTree.TyVar.t}
      end
   structure TyConEnv :
      sig
         type dom = ParseTree.TyConName.t
         structure Type :
            sig
               type cod = {tyvars: AbsSynTree.TyVar.t list,
                           ty: AbsSynTree.Type.t}
            end
         structure Data :
            sig
               type cod = {arity: int,
                           tycon: AbsSynTree.TyCon.t}
            end
         datatype cod =
            Type of Type.cod
          | Data of Data.cod
      end
   structure DaConEnv :
      sig
         type dom = ParseTree.DaConName.t
         type cod =
            {tyvars: AbsSynTree.TyVar.t list,
             arg_tys: AbsSynTree.Type.t list,
             tycon: AbsSynTree.TyCon.t,
             dacon: AbsSynTree.DaCon.t}
      end
   structure VarEnv :
      sig
         type dom = ParseTree.VarName.t
         (* For simple binding checking, we can take the co-domain
          * to be 'unit', since we only need to know whether or not
          * the variable is in the environment.
          *)
         type cod = unit
         (* For type checking (without producing an abstract syntax
          * tree), we can take the co-domain to be 
          * '{var_ty: AbsSynTree.Type.t}', recording the semantic type
          * of the variable in the environment.
          *)
         (*
          * type cod = {var_ty: AbsSynTree.Type.t}
          *)
         (* For type checking and producing an abstract syntax tree,
          * we can take the co-domain to be 
          * '{var_ty: AbsSynTree.Type.t, var: AbsSynTree.Var.t}',
          * recording both the semantic type of the variable and the 
          * abstract-syntax-tree variable to which the variable is
          * translated.
          *)
         (*
          * type cod = {var_ty: AbsSynTree.Type.t,
          *             var: AbsSynTree.Var.t}
          *)
      end

   type t

   (* The empty environment {}. *)
   val empty : t

   (* Create an environment with a single TyVar entry. *)
   val singletonTyVar : TyVarEnv.dom * TyVarEnv.cod -> t
   (* Lookup a TyVar in the environment. *)
   val lookupTyVar : t * TyVarEnv.dom -> TyVarEnv.cod option

   (* Create an environment with a single TyCon entry. *)
   val singletonTyCon : TyConEnv.dom * TyConEnv.cod -> t
   val singletonTyConType : TyConEnv.dom * TyConEnv.Type.cod -> t
   val singletonTyConData : TyConEnv.dom * TyConEnv.Data.cod -> t
   (* Lookup a TyCon in the environment. *)
   val lookupTyCon : t * TyConEnv.dom -> TyConEnv.cod option

   (* Create an environment with a single DaCon entry. *)
   val singletonDaCon : DaConEnv.dom * DaConEnv.cod -> t
   (* Lookup a DaCon in the environment. *)
   val lookupDaCon : t * DaConEnv.dom -> DaConEnv.cod option

   (* Create an environment with a single Var entry. *)
   val singletonVar : VarEnv.dom * VarEnv.cod -> t
   (* Lookup a Var in the environment. *)
   val lookupVar : t * VarEnv.dom -> VarEnv.cod option

   (* Implements E1 (+) E2. *)
   val extend : t * t -> t

   (* Implements \Theta(E). *)
   val tycons : t -> AbsSynTree.TyCon.Set.set

   (* The initial environment E_0. *)
   val initial: t
end
