(* langfc-src/type-checker/environment.sml
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

structure Environment :> ENVIRONMENT =
struct
   structure PT = ParseTree
   structure AST = AbsSynTree

   structure TyVarEnv =
      struct
         type dom = PT.TyVarName.t
         type cod = {tyvar: AST.TyVar.t}
         type t = cod PT.TyVarName.Map.map
         val empty : t = PT.TyVarName.Map.empty
         val singleton : dom * cod -> t = PT.TyVarName.Map.singleton
         val lookup : t * dom -> cod option = PT.TyVarName.Map.find
         val extend : t * t -> t = PT.TyVarName.Map.unionWith #2
         val domain : t -> PT.TyVarName.Set.set =
            PT.TyVarName.Set.fromList o PT.TyVarName.Map.listKeys
      end
   structure TyVarEnvRev =
      struct
         type dom = AST.TyVar.t
         type cod = {tyvar: PT.TyVarName.t}
         type t = cod AST.TyVar.Map.map
         val empty : t = AST.TyVar.Map.empty
         val singleton : dom * cod -> t = AST.TyVar.Map.singleton
         val lookup : t * dom -> cod option = AST.TyVar.Map.find
         val extend : t * t -> t = AST.TyVar.Map.unionWith #2
         val domain : t -> AST.TyVar.Set.set =
            AST.TyVar.Set.fromList o AST.TyVar.Map.listKeys
      end
   structure TyConEnv =
      struct
         type dom = PT.TyConName.t
         structure Type =
            struct
               type cod = {tyvars: AST.TyVar.t list,
                           ty: AST.Type.t}
            end
         structure Data =
            struct
               type cod = {tycon: AST.TyCon.t}
            end
         datatype cod =
            Type of Type.cod
          | Data of Data.cod
         type t = cod PT.TyConName.Map.map
         val empty : t = PT.TyConName.Map.empty
         val singleton : dom * cod -> t = PT.TyConName.Map.singleton
         val lookup : t * dom -> cod option = PT.TyConName.Map.find
         val extend : t * t -> t = PT.TyConName.Map.unionWith #2
         val domain : t -> PT.TyConName.Set.set =
            PT.TyConName.Set.fromList o PT.TyConName.Map.listKeys
      end
   structure TyConEnvRev =
      struct
         type dom = AST.TyCon.t
         type cod = {arity: int,
                     tycon: PT.TyConName.t,
                     dacons: AST.DaCon.t list}
         type t = cod AST.TyCon.Map.map
         val empty : t = AST.TyCon.Map.empty
         val singleton : dom * cod -> t = AST.TyCon.Map.singleton
         val lookup : t * dom -> cod option = AST.TyCon.Map.find
         val extend : t * t -> t = AST.TyCon.Map.unionWith #2
         val domain : t -> AST.TyCon.Set.set =
            AST.TyCon.Set.fromList o AST.TyCon.Map.listKeys
      end
   structure DaConEnv =
      struct
         type dom = PT.DaConName.t
         type cod =
            {tyvars: AST.TyVar.t list,
             arg_tys: AST.Type.t list,
             tycon: AST.TyCon.t,
             dacon: AST.DaCon.t}
         type t = cod PT.DaConName.Map.map
         val empty : t = PT.DaConName.Map.empty
         val singleton : dom * cod -> t = PT.DaConName.Map.singleton
         val lookup : t * dom -> cod option = PT.DaConName.Map.find
         val extend : t * t -> t = PT.DaConName.Map.unionWith #2
         val domain : t -> PT.DaConName.Set.set =
            PT.DaConName.Set.fromList o PT.DaConName.Map.listKeys
      end
   structure DaConEnvRev =
      struct
         type dom = AST.DaCon.t
         type cod = {dacon: PT.DaConName.t}
         type t = cod AST.DaCon.Map.map
         val empty : t = AST.DaCon.Map.empty
         val singleton : dom * cod -> t = AST.DaCon.Map.singleton
         val lookup : t * dom -> cod option = AST.DaCon.Map.find
         val extend : t * t -> t = AST.DaCon.Map.unionWith #2
         val domain : t -> AST.DaCon.Set.set =
            AST.DaCon.Set.fromList o AST.DaCon.Map.listKeys
      end
   structure VarEnv =
      struct
         type dom = PT.VarName.t
         (*
          * For type checking and producing an abstract syntax tree,
          * we can take the co-domain to be 
          * '{var_ty: AbsSynTree.Type.t, var: AbsSynTree.Var.t}',
          * recording both the semantic type of the variable and the 
          * abstract-syntax-tree variable to which the variable is
          * translated.
          *)
         type cod = {var_ty: AST.Type.t,
                     var: AST.Var.t}
         type t = cod PT.VarName.Map.map
         val empty : t = PT.VarName.Map.empty
         val singleton : dom * cod -> t = PT.VarName.Map.singleton
         val lookup : t * dom -> cod option = PT.VarName.Map.find
         val extend : t * t -> t = PT.VarName.Map.unionWith #2
         val domain : t -> PT.VarName.Set.set =
            PT.VarName.Set.fromList o PT.VarName.Map.listKeys
      end

   datatype t =
      Env of {tyvarEnv: TyVarEnv.t,
              tyvarEnvRev: TyVarEnvRev.t,
              tyconEnv: TyConEnv.t,
              tyconEnvRev: TyConEnvRev.t,
              daconEnv: DaConEnv.t,
              daconEnvRev: DaConEnvRev.t,
              varEnv: VarEnv.t}

   val empty =
      Env {tyvarEnv = TyVarEnv.empty,
           tyvarEnvRev = TyVarEnvRev.empty,
           tyconEnv = TyConEnv.empty,
           tyconEnvRev = TyConEnvRev.empty,
           daconEnv = DaConEnv.empty,
           daconEnvRev = DaConEnvRev.empty,
           varEnv = VarEnv.empty}

   fun fromTyVarEnv tyvarEnv =
      Env {tyvarEnv = tyvarEnv,
           tyvarEnvRev = TyVarEnvRev.empty,
           tyconEnv = TyConEnv.empty,
           tyconEnvRev = TyConEnvRev.empty,
           daconEnv = DaConEnv.empty,
           daconEnvRev = DaConEnvRev.empty,
           varEnv = VarEnv.empty}
   val singletonTyVar = fromTyVarEnv o TyVarEnv.singleton
   fun lookupTyVar (Env {tyvarEnv, ...}, tyvar) =
      TyVarEnv.lookup (tyvarEnv, tyvar)
   fun domainTyVar (Env {tyvarEnv, ...}) =
      TyVarEnv.domain tyvarEnv

   fun fromTyVarEnvRev tyvarEnvRev =
      Env {tyvarEnv = TyVarEnv.empty,
           tyvarEnvRev = tyvarEnvRev,
           tyconEnv = TyConEnv.empty,
           tyconEnvRev = TyConEnvRev.empty,
           daconEnv = DaConEnv.empty,
           daconEnvRev = DaConEnvRev.empty,
           varEnv = VarEnv.empty}
   val singletonTyVarRev = fromTyVarEnvRev o TyVarEnvRev.singleton
   fun lookupTyVarRev (Env {tyvarEnvRev, ...}, tyvar) =
      TyVarEnvRev.lookup (tyvarEnvRev, tyvar)
   fun domainTyVarRev (Env {tyvarEnvRev, ...}) =
      TyVarEnvRev.domain tyvarEnvRev

   fun fromTyConEnv tyconEnv =
      Env {tyvarEnv = TyVarEnv.empty,
           tyvarEnvRev = TyVarEnvRev.empty,
           tyconEnv = tyconEnv,
           tyconEnvRev = TyConEnvRev.empty,
           daconEnv = DaConEnv.empty,
           daconEnvRev = DaConEnvRev.empty,
           varEnv = VarEnv.empty}
   val singletonTyCon = fromTyConEnv o TyConEnv.singleton
   val singletonTyConType = fn (tycon, arg) => singletonTyCon (tycon, TyConEnv.Type arg)
   val singletonTyConData = fn (tycon, arg) => singletonTyCon (tycon, TyConEnv.Data arg)
   fun lookupTyCon (Env {tyconEnv, ...}, tycon) =
      TyConEnv.lookup (tyconEnv, tycon)
   fun domainTyCon (Env {tyconEnv, ...}) =
      TyConEnv.domain tyconEnv

   fun fromTyConEnvRev tyconEnvRev =
      Env {tyvarEnv = TyVarEnv.empty,
           tyvarEnvRev = TyVarEnvRev.empty,
           tyconEnv = TyConEnv.empty,
           tyconEnvRev = tyconEnvRev,
           daconEnv = DaConEnv.empty,
           daconEnvRev = DaConEnvRev.empty,
           varEnv = VarEnv.empty}
   val singletonTyConRev = fromTyConEnvRev o TyConEnvRev.singleton
   fun lookupTyConRev (Env {tyconEnvRev, ...}, tycon) =
      TyConEnvRev.lookup (tyconEnvRev, tycon)
   fun domainTyConRev (Env {tyconEnvRev, ...}) =
      TyConEnvRev.domain tyconEnvRev

   fun fromDaConEnv daconEnv =
      Env {tyvarEnv = TyVarEnv.empty,
           tyvarEnvRev = TyVarEnvRev.empty,
           tyconEnv = TyConEnv.empty,
           tyconEnvRev = TyConEnvRev.empty,
           daconEnv = daconEnv,
           daconEnvRev = DaConEnvRev.empty,
           varEnv = VarEnv.empty}
   val singletonDaCon = fromDaConEnv o DaConEnv.singleton
   fun lookupDaCon (Env {daconEnv, ...}, dacon) =
      DaConEnv.lookup (daconEnv, dacon)
   fun domainDaCon (Env {daconEnv, ...}) =
      DaConEnv.domain daconEnv

   fun fromDaConEnvRev daconEnvRev =
      Env {tyvarEnv = TyVarEnv.empty,
           tyvarEnvRev = TyVarEnvRev.empty,
           tyconEnv = TyConEnv.empty,
           tyconEnvRev = TyConEnvRev.empty,
           daconEnv = DaConEnv.empty,
           daconEnvRev = daconEnvRev,
           varEnv = VarEnv.empty}
   val singletonDaConRev = fromDaConEnvRev o DaConEnvRev.singleton
   fun lookupDaConRev (Env {daconEnvRev, ...}, dacon) =
      DaConEnvRev.lookup (daconEnvRev, dacon)
   fun domainDaConRev (Env {daconEnvRev, ...}) =
      DaConEnvRev.domain daconEnvRev

   fun fromVarEnv varEnv =
      Env {tyvarEnv = TyVarEnv.empty,
           tyvarEnvRev = TyVarEnvRev.empty,
           tyconEnv = TyConEnv.empty,
           tyconEnvRev = TyConEnvRev.empty,
           daconEnv = DaConEnv.empty,
           daconEnvRev = DaConEnvRev.empty,
           varEnv = varEnv}
   val singletonVar = fromVarEnv o VarEnv.singleton
   fun lookupVar (Env {varEnv, ...}, var) =
      VarEnv.lookup (varEnv, var)
   fun domainVar (Env {varEnv, ...}) =
      VarEnv.domain varEnv

   fun extend (Env {tyvarEnv = tyvarEnv1,
                    tyvarEnvRev = tyvarEnvRev1,
                    tyconEnv = tyconEnv1,
                    tyconEnvRev = tyconEnvRev1,
                    daconEnv = daconEnv1,
                    daconEnvRev = daconEnvRev1,
                    varEnv = varEnv1},
               Env {tyvarEnv = tyvarEnv2,
                    tyvarEnvRev = tyvarEnvRev2,
                    tyconEnv = tyconEnv2,
                    tyconEnvRev = tyconEnvRev2,
                    daconEnv = daconEnv2,
                    daconEnvRev = daconEnvRev2,
                    varEnv = varEnv2}) =
      Env {tyvarEnv = TyVarEnv.extend (tyvarEnv1, tyvarEnv2),
           tyvarEnvRev = TyVarEnvRev.extend (tyvarEnvRev1, tyvarEnvRev2),
           tyconEnv = TyConEnv.extend (tyconEnv1, tyconEnv2),
           tyconEnvRev = TyConEnvRev.extend (tyconEnvRev1, tyconEnvRev2),
           daconEnv = DaConEnv.extend (daconEnv1, daconEnv2),
           daconEnvRev = DaConEnvRev.extend (daconEnvRev1, daconEnvRev2),
           varEnv = VarEnv.extend (varEnv1, varEnv2)}

   (* Create an environment with a single TyVar entry
    * and the corresponding reverse entry. *)
   val singletonTyVarWithRev = fn (tyvarPT, cod as {tyvar = tyvarAST, ...}) =>
      let
         val env1 = singletonTyVar (tyvarPT, cod)
         val env2 = singletonTyVarRev (tyvarAST, {tyvar = tyvarPT})
      in
         extend (env1, env2)
      end

   (* Create an environment with a single TyCon entry
    * and the coresponding reverse entry. *)
   val singletonTyConDataWithRev = fn (tyconPT, cod as {tycon = tyconAST, ...}, {arity, dacons}) =>
      let
         val env1 = singletonTyCon (tyconPT, TyConEnv.Data cod)
         val env2 = singletonTyConRev (tyconAST, {arity = arity, tycon = tyconPT, dacons = dacons})
      in
         extend (env1, env2)
      end

   (* Create an environment with a single DaCon entry
    * and the corresponding reverse entry. *)
   val singletonDaConWithRev = fn (daconPT, cod as {dacon = daconAST, ...}) =>
      let
         val env1 = singletonDaCon (daconPT, cod)
         val env2 = singletonDaConRev (daconAST, {dacon = daconPT})
      in
         extend (env1, env2)
      end

   (* Implements \Theta(E). *)
   fun tycons (Env {tyconEnv, ...}) =
      PT.TyConName.Map.foldl
      (fn (item, tycon_set) =>
       case item of
          TyConEnv.Type _ => tycon_set
        | TyConEnv.Data {tycon, ...} =>
             AST.TyCon.Set.add (tycon_set, tycon))
      AST.TyCon.Set.empty
      tyconEnv

   (* The initial environment E_0. *)
   val initial = empty
   (* Pre-defined type constructors. *)
   val initial =
      List.foldl
      (fn ((tyconPT, tyconAST, arity, dacons), env) =>
       extend (env, singletonTyConDataWithRev (tyconPT, {tycon = tyconAST}, {arity = arity, dacons = dacons})))
      initial
      [(PT.TyConName.array, AST.TyCon.array, 1, []),
       (PT.TyConName.bool, AST.TyCon.bool, 0, [AST.DaCon.falsee, AST.DaCon.truee]),
       (PT.TyConName.integer, AST.TyCon.integer, 0, []),
       (PT.TyConName.string, AST.TyCon.string, 0, []),
       (PT.TyConName.unit, AST.TyCon.unit, 0, [AST.DaCon.unit])]
   (* Pre-defined data constructors. *)
   val initial =
      List.foldl
      (fn ((dacon, item), env) => extend (env, singletonDaConWithRev (dacon, item)))
      initial
      [(PT.DaConName.falsee, {dacon = AST.DaCon.falsee,
                              tyvars = [], arg_tys = [],
                              tycon = AST.TyCon.bool}),
       (PT.DaConName.truee, {dacon = AST.DaCon.truee,
                             tyvars = [], arg_tys = [],
                             tycon = AST.TyCon.bool}),
       (PT.DaConName.unit, {dacon = AST.DaCon.unit,
                            tyvars = [], arg_tys = [],
                            tycon = AST.TyCon.unit})]
   (* Pre-defined variables. *)
   val initial =
      List.foldl
      (fn ((var, item), env) => extend (env, singletonVar (var, item)))
      initial
      [(PT.VarName.argc, {var_ty = AST.Type.T_Fn (AST.Type.unit, AST.Type.integer),
                          var = AST.Var.argc}),
       (PT.VarName.arg, {var_ty = AST.Type.T_Fn (AST.Type.integer, AST.Type.string),
                         var = AST.Var.arg}),
       (PT.VarName.array, {var_ty = let
                                       val a = AST.TyVar.new "'a"
                                    in
                                       AST.Type.T_TyFn (a,
                                       AST.Type.T_Fn (AST.Type.integer,
                                       AST.Type.T_Fn (AST.Type.T_TyVar a,
                                                      AST.Type.array (AST.Type.T_TyVar a))))
                                    end,
                           var = AST.Var.array}),
       (PT.VarName.fail, {var_ty = let
                                      val a = AST.TyVar.new "'a"
                                   in
                                      AST.Type.T_TyFn (a,
                                      AST.Type.T_Fn (AST.Type.string,
                                                     AST.Type.T_TyVar a))
                                   end,
                          var = AST.Var.fail}),
       (PT.VarName.print, {var_ty = AST.Type.T_Fn (AST.Type.string, AST.Type.unit),
                           var = AST.Var.print}),
       (PT.VarName.size, {var_ty = AST.Type.T_Fn (AST.Type.string, AST.Type.integer),
                          var = AST.Var.size}),
       (PT.VarName.subscript, {var_ty = AST.Type.T_Fn (AST.Type.string,
                                        AST.Type.T_Fn (AST.Type.integer,
                                                       AST.Type.integer)),
                               var = AST.Var.subscript})]
end
