(* langfc-src/core-ir/convert.sml
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
 * Conversion from abstract syntax tree representation to core
 * intermediate representation in the LangF compiler (langfc).
 *)

structure CoreIRConverter :> CORE_IR_CONVERTER =
struct
   structure AST = AbsSynTree
   structure C = CoreIR
   structure C =
      struct
         open C
         structure Pat =
            struct
               open Pat
               val falsee =
                  P_DaCon {dacon = DaCon.falsee,
                           tyargs = [],
                           binds = []}
               val truee =
                  P_DaCon {dacon = DaCon.truee,
                           tyargs = [],
                           binds = []}
            end
         structure Exp =
            struct
               open Exp
               val falsee =
                  make (E_DaCon {dacon = DaCon.falsee,
                                 tyargs = [],
                                 args = []},
                        Type.bool)
               val truee =
                  make (E_DaCon {dacon = DaCon.truee,
                                 tyargs = [],
                                 args = []},
                        Type.bool)
               val unitNode =
                  E_DaCon {dacon = DaCon.unit,
                           tyargs = [],
                           args = []}
               val unit = make (unitNode, Type.unit)
            end
      end

   structure Env =
      struct
         structure TyVarEnv =
            struct
               type dom = AST.TyVar.t
               type cod = C.TyVar.t
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
               type dom = AST.TyCon.t
               type cod = C.TyCon.t
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
               type dom = AST.DaCon.t
               type cod = C.DaCon.t
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
               type dom = AST.Var.t
               type cod = C.Var.t
               type t = cod AST.Var.Map.map
               val empty : t = AST.Var.Map.empty
               val singleton : dom * cod -> t = AST.Var.Map.singleton
               val lookup : t * dom -> cod option = AST.Var.Map.find
               val extend : t * t -> t = AST.Var.Map.unionWith #2
               val domain : t -> AST.Var.Set.set =
                  AST.Var.Set.fromList o AST.Var.Map.listKeys
            end

         datatype t =
            Env of {tyvarEnv: TyVarEnv.t,
                    tyconEnv: TyConEnv.t,
                    daconEnv: DaConEnv.t,
                    varEnv: VarEnv.t}

         val empty =
            Env {tyvarEnv = TyVarEnv.empty,
                 tyconEnv = TyConEnv.empty,
                 daconEnv = DaConEnv.empty,
                 varEnv = VarEnv.empty}

         fun fromTyVarEnv tyvarEnv =
            Env {tyvarEnv = tyvarEnv,
                 tyconEnv = TyConEnv.empty,
                 daconEnv = DaConEnv.empty,
                 varEnv = VarEnv.empty}
         val singletonTyVar = fromTyVarEnv o TyVarEnv.singleton
         fun lookupTyVar (Env {tyvarEnv, ...}, tyvar) =
            TyVarEnv.lookup (tyvarEnv, tyvar)
         fun domainTyVar (Env {tyvarEnv, ...}) =
            TyVarEnv.domain tyvarEnv

         fun fromTyConEnv tyconEnv =
            Env {tyvarEnv = TyVarEnv.empty,
                 tyconEnv = tyconEnv,
                 daconEnv = DaConEnv.empty,
                 varEnv = VarEnv.empty}
         val singletonTyCon = fromTyConEnv o TyConEnv.singleton
         fun lookupTyCon (Env {tyconEnv, ...}, tycon) =
            TyConEnv.lookup (tyconEnv, tycon)
         fun domainTyCon (Env {tyconEnv, ...}) =
            TyConEnv.domain tyconEnv

         fun fromDaConEnv daconEnv =
            Env {tyvarEnv = TyVarEnv.empty,
                 tyconEnv = TyConEnv.empty,
                 daconEnv = daconEnv,
                 varEnv = VarEnv.empty}
         val singletonDaCon = fromDaConEnv o DaConEnv.singleton
         fun lookupDaCon (Env {daconEnv, ...}, dacon) =
            DaConEnv.lookup (daconEnv, dacon)
         fun domainDaCon (Env {daconEnv, ...}) =
            DaConEnv.domain daconEnv

         fun fromVarEnv varEnv =
            Env {tyvarEnv = TyVarEnv.empty,
                 tyconEnv = TyConEnv.empty,
                 daconEnv = DaConEnv.empty,
                 varEnv = varEnv}
         val singletonVar = fromVarEnv o VarEnv.singleton
         fun lookupVar (Env {varEnv, ...}, var) =
            VarEnv.lookup (varEnv, var)
         fun domainVar (Env {varEnv, ...}) =
            VarEnv.domain varEnv

         fun extend (Env {tyvarEnv = tyvarEnv1,
                          tyconEnv = tyconEnv1,
                          daconEnv = daconEnv1,
                          varEnv = varEnv1},
                     Env {tyvarEnv = tyvarEnv2,
                          tyconEnv = tyconEnv2,
                          daconEnv = daconEnv2,
                          varEnv = varEnv2}) =
            Env {tyvarEnv = TyVarEnv.extend (tyvarEnv1, tyvarEnv2),
                 tyconEnv = TyConEnv.extend (tyconEnv1, tyconEnv2),
                 daconEnv = DaConEnv.extend (daconEnv1, daconEnv2),
                 varEnv = VarEnv.extend (varEnv1, varEnv2)}

         (* The initial environment E_0. *)
         val initial = empty
         (* Pre-defined type constructors. *)
         val initial =
            List.foldl
            (fn ((tyconAST, tyconC), env) =>
             extend (env, singletonTyCon (tyconAST, tyconC)))
            initial
            [(AST.TyCon.array, C.TyCon.array),
             (AST.TyCon.bool, C.TyCon.bool),
             (AST.TyCon.integer, C.TyCon.integer),
             (AST.TyCon.string, C.TyCon.string),
             (AST.TyCon.unit, C.TyCon.unit)]
         (* Pre-defined data constructors. *)
         val initial =
            List.foldl
            (fn ((daconAST, daconC), env) =>
             extend (env, singletonDaCon (daconAST, daconC)))
            initial
            [(AST.DaCon.falsee, C.DaCon.falsee),
             (AST.DaCon.truee, C.DaCon.truee),
             (AST.DaCon.unit, C.DaCon.unit)]
      end

   fun toStringTyVar tyvar =
      Layout.toString (AST.TyVar.layout tyvar)
   fun toStringTyCon tycon =
      Layout.toString (AST.TyCon.layout tycon)
   fun toStringDaCon dacon =
      Layout.toString (AST.DaCon.layout dacon)
   fun toStringVar var =
      Layout.toString (AST.Var.layout var)

   fun convert (prog : AST.Prog.t) : C.Prog.t =
      let
         exception ConvertError
         fun raiseConvertError msgs =
            (List.map (fn msg => (TextIO.output (TextIO.stdErr, msg))) ("Convert Error:\n" :: msgs)
             ; TextIO.output1 (TextIO.stdErr, #"\n")
             ; raise ConvertError)

         fun convertType (env : Env.t) (ty : AST.Type.t)
                         : C.Type.t =
            case ty of
               AST.Type.T_TyFn (tyvar, res) =>
                  let
                     val tyvarC = C.TyVar.new (AST.TyVar.name tyvar)
                     val resC =
                        convertType
                        (Env.extend
                         (env,
                          Env.singletonTyVar (tyvar, tyvarC)))
                        res
                  in
                     C.Type.T_TyFn {tyvar = tyvarC, res = resC}
                  end
             | AST.Type.T_Fn (arg, res) =>
                  let
                     val argC = convertType env arg
                     val resC = convertType env res
                  in
                     C.Type.T_Fn {arg = argC, res = resC}
                  end
             | AST.Type.T_TyCon (tycon, tyargs) =>
                  let
                     val tyargsC =
                        List.map (convertType env) tyargs
                  in
                     case Env.lookupTyCon (env, tycon) of
                        NONE =>
                           raiseConvertError
                           (["Unbound type constructor: ",
                             toStringTyCon tycon, ".\n"])
                      | SOME tyconC =>
                           C.Type.T_TyCon {tycon = tyconC, tyargs = tyargsC}
                  end
             | AST.Type.T_TyVar tyvar =>
                  (case Env.lookupTyVar (env, tyvar) of
                      NONE =>
                         raiseConvertError
                         (["Unbound type variable: ",
                           toStringTyVar tyvar, ".\n"])
                    | SOME tyvarC =>
                         C.Type.T_TyVar {tyvar = tyvarC})
         val convertType = fn env =>
            C.Type.alphaRename o (convertType env)

         fun convertParam (env: Env.t) (param: AST.Param.t)
                          : Env.t * (C.Type.t -> C.Type.t) * (C.Exp.t -> C.Lam.t) =
            case param of
               AST.Param.P_Var (var, var_ty) =>
                  let
                     val varC = C.Var.new (AST.Var.name var)
                     val var_tyC = convertType env var_ty
                     val paramC = C.Param.P_Var {var = varC, var_ty = var_tyC}
                     val env' = Env.singletonVar (var, varC)
                     val mkTy = fn res_tyC =>
                        C.Type.alphaRename (C.Type.T_Fn {arg = var_tyC, res = res_tyC})
                     val mkLam = fn expC =>
                        C.Lam.Lam {param = paramC, body = expC}
                  in
                     (env', mkTy, mkLam)
                  end
             | AST.Param.P_TyVar tyvar =>
                  let
                     val tyvarC = C.TyVar.new (AST.TyVar.name tyvar)
                     val paramC = C.Param.P_TyVar {tyvar = tyvarC}
                     val env' =
                        Env.singletonTyVar (tyvar, tyvarC)
                     val mkTy = fn res_tyC =>
                        C.Type.alphaRename (C.Type.T_TyFn {tyvar = tyvarC, res = res_tyC})
                     val mkLam = fn expC =>
                        C.Lam.Lam {param = paramC, body = expC}
                  in
                     (env', mkTy, mkLam)
                  end

         fun convertParams (env: Env.t) (params: AST.Param.t list)
                           : Env.t * (C.Type.t -> C.Type.t) * (C.Exp.t -> C.Lam.t) =
            let
               val (env', mkTy, mkLam) =
                  List.foldl
                  (fn (param_i, (env', mkTy, mkLam)) =>
                   let
                      val (env'_i, mkTy_i, mkLam_i) =
                         convertParam (Env.extend (env, env')) param_i
                   in
                      (Env.extend (env', env'_i),
                       mkTy o mkTy_i,
                       case mkLam of
                          NONE => SOME mkLam_i
                        | SOME mkLam =>
                             SOME (fn expC =>
                                   mkLam (C.Exp.make (C.Exp.E_Fn {lam = mkLam_i expC},
                                                      mkTy_i (C.Exp.ty expC)))))
                   end)
                  (Env.empty,
                   fn ty => ty,
                   NONE)
                  params
            in
               (env', mkTy, valOf mkLam)
            end

         fun convertSimplePat (env: Env.t)
                              (pat: AST.SimplePat.t)
                              : Env.t * {var: C.Var.t, var_ty: C.Type.t} =
            case pat of
               AST.SimplePat.P_Var (var, var_ty) =>
                  let
                     val varC = C.Var.new (AST.Var.name var)
                     val var_tyC = convertType env var_ty
                     val env' =
                        Env.singletonVar (var, varC)
                  in
                     (env',
                      {var = varC, var_ty = var_tyC})
                  end
             | AST.SimplePat.P_Wild ty =>
                  let
                     val tyC = convertType env ty
                     val env' = Env.empty
                  in
                     (env',
                      {var = C.Var.new "zzz", var_ty = tyC})
                  end

         fun convertPat (env: Env.t) (pat: AST.Pat.t)
                        : Env.t * C.Pat.t =
            case pat of
               AST.Pat.P_DaCon (dacon, actual_tyargs, actual_pats) =>
                  let
                     val actual_tyargsC =
                        List.map (convertType env) actual_tyargs
                     val (actual_bindsC, env') =
                        ListExtra.mapAndFoldl
                        (fn (actual_pat_i, env') =>
                         let
                            val (env'_i, bindC) =
                               convertSimplePat env actual_pat_i
                         in
                            (bindC,
                             Env.extend (env', env'_i))
                         end)
                        Env.empty
                        actual_pats
                  in
                     case Env.lookupDaCon (env, dacon) of
                        NONE =>
                           raiseConvertError
                           (["Unbound data constructor: ",
                             toStringDaCon dacon, ".\n"])
                      | SOME daconC =>
                           (env',
                            C.Pat.P_DaCon {dacon = daconC,
                                           tyargs = actual_tyargsC,
                                           binds = actual_bindsC})
                  end
             | AST.Pat.P_SimplePat pat =>
                  let
                     val (env', bindC) =
                        convertSimplePat env pat
                  in
                     (env', C.Pat.P_Var bindC)
                  end

         fun convertDaConDecl (env: Env.t)
                              (dacon_decl: AST.DaCon.t * AST.Type.t list)
                              : Env.t * {dacon: C.DaCon.t, arg_tys: C.Type.t list} =
            let
               val (dacon, arg_tys) =  dacon_decl
               val daconC = C.DaCon.new (AST.DaCon.name dacon)
               val arg_tysC =
                  List.map (convertType env) arg_tys
               val env' =
                  Env.singletonDaCon (dacon, daconC)
            in
               (env',
                {dacon = daconC, arg_tys = arg_tysC})
            end
         fun convertDaConDecls (env: Env.t)
                               (dacon_decls: (AST.DaCon.t * AST.Type.t list) list)
                               : Env.t * {dacon: C.DaCon.t, arg_tys: C.Type.t list} list =
            let
               val (dacon_declsC, env') =
                  ListExtra.mapAndFoldl
                  (fn (dacon_decl_i, env') =>
                   let
                      val (env'_i, dacon_declC_i) =
                         convertDaConDecl env
                                          dacon_decl_i
                   in
                      (dacon_declC_i,
                       Env.extend (env', env'_i))
                   end)
                  Env.empty
                  dacon_decls
            in
               (env', dacon_declsC)
            end

         fun convertExp (env: Env.t) (exp: AST.Exp.t)
                        : C.Exp.t =
            C.Exp.make (convertExpNode env (AST.Exp.node exp),
                        convertType env (AST.Exp.ty exp))
         and convertExpNode (env: Env.t) (node: AST.Exp.node)
                            : C.Exp.node =
            case node of
               AST.Exp.E_Fn (params, body) =>
                  let
                     val (env', mkTy, mkLam) =
                        convertParams env params
                     val bodyC = convertExp (Env.extend (env, env')) body
                  in
                     C.Exp.E_Fn
                     {lam = mkLam bodyC}
                  end
             | AST.Exp.E_If (expIf, expThen, expElse) =>
                  let
                     val expIfC = convertExp env expIf
                     val expThenC = convertExp env expThen
                     val expElseC = convertExp env expElse
                  in
                     C.Exp.E_Case
                     {arg = expIfC,
                      matchrules =
                      [C.MatchRule.MatchRule
                       {pat = C.Pat.truee, body = expThenC},
                       C.MatchRule.MatchRule
                       {pat = C.Pat.falsee, body = expElseC}]}
                  end
             | AST.Exp.E_Orelse (expl, expr) =>
                  let
                     val explC = convertExp env expl
                     val exprC = convertExp env expr
                  in
                     C.Exp.E_Case
                     {arg = explC,
                      matchrules =
                      [C.MatchRule.MatchRule
                       {pat = C.Pat.truee, body = C.Exp.truee},
                       C.MatchRule.MatchRule
                       {pat = C.Pat.falsee, body = exprC}]}
                  end
             | AST.Exp.E_Andalso (expl, expr) =>
                  let
                     val explC = convertExp env expl
                     val exprC = convertExp env expr
                  in
                     C.Exp.E_Case
                     {arg = explC,
                      matchrules =
                      [C.MatchRule.MatchRule
                       {pat = C.Pat.truee, body = exprC},
                       C.MatchRule.MatchRule
                       {pat = C.Pat.falsee, body = C.Exp.falsee}]}
                  end
             | AST.Exp.E_TernOp (AST.TernOp.Upd ty, expl, expm, expr) =>
                  let
                     val tyarg = convertType env ty
                     val explC = convertExp env expl
                     val expmC = convertExp env expm
                     val exprC = convertExp env expr
                     val a = C.Var.new "a"
                     val a_ty = C.Exp.ty explC
                     val a_exp = C.Exp.make (C.Exp.E_Var {var = a}, a_ty)
                     val a_decl =
                        C.Decl.D_Val
                        {var = a, var_ty = a_ty, exp = explC}
                     val i = C.Var.new "i"
                     val i_ty = C.Exp.ty expmC
                     val i_exp = C.Exp.make (C.Exp.E_Var {var = i}, i_ty)
                     val i_decl =
                        C.Decl.D_Val
                        {var = i, var_ty = i_ty, exp = expmC}
                     val v = C.Var.new "v"
                     val v_ty = C.Exp.ty exprC
                     val v_exp = C.Exp.make (C.Exp.E_Var {var = v}, v_ty)
                     val v_decl = 
                        C.Decl.D_Val
                        {var = v, var_ty = v_ty, exp = exprC}
                     val msgExp =
                        C.Exp.make
                        (C.Exp.E_String "invalid array index (!/:=)",
                         C.Type.string)
                     val failExp =
                        C.Exp.make
                        (C.Exp.E_Prim {prim = C.Prim.Fail,
                                       tyargs = [tyarg],
                                       args = [msgExp]},
                         tyarg)
                     val zeroExp =
                        C.Exp.make
                        (C.Exp.E_Integer 0,
                         C.Type.integer)
                     val zeroLteIExp =
                        C.Exp.make
                        (C.Exp.E_Prim {prim = C.Prim.Lte,
                                       tyargs = [],
                                       args = [zeroExp, i_exp]},
                         C.Type.bool)
                     val lenExp =
                        C.Exp.make
                        (C.Exp.E_Prim {prim = C.Prim.Len,
                                       tyargs = [tyarg],
                                       args = [a_exp]},
                         C.Type.integer)
                     val iLtLenExp =
                        C.Exp.make
                        (C.Exp.E_Prim {prim = C.Prim.Lt,
                                       tyargs = [],
                                       args = [i_exp, lenExp]},
                         C.Type.bool)
                     val updExp =
                        C.Exp.make
                        (C.Exp.E_Prim
                         {prim = C.Prim.Upd,
                          tyargs = [tyarg],
                          args = [a_exp, i_exp, v_exp]},
                         tyarg)
                     val bodyExp =
                        C.Exp.make
                        (C.Exp.E_Case
                         {arg = zeroLteIExp,
                          matchrules =
                          [C.MatchRule.MatchRule 
                           {pat = C.Pat.P_DaCon {dacon = C.DaCon.truee,
                                                 tyargs =[],
                                                 binds = []},
                            body =
                            C.Exp.make
                            (C.Exp.E_Case
                             {arg = iLtLenExp,
                              matchrules =
                              [C.MatchRule.MatchRule
                               {pat = C.Pat.P_DaCon {dacon = C.DaCon.truee,
                                                     tyargs =[],
                                                     binds = []},
                                body = updExp},
                               C.MatchRule.MatchRule
                               {pat = C.Pat.P_DaCon {dacon = C.DaCon.falsee,
                                                     tyargs =[],
                                                     binds = []},
                                body = failExp}]},
                             tyarg)},                               
                           C.MatchRule.MatchRule
                           {pat = C.Pat.P_DaCon {dacon = C.DaCon.falsee,
                                                 tyargs =[],
                                                 binds = []},
                            body = failExp}]},
                         tyarg)
                  in
                     C.Exp.E_Let
                     {decl = a_decl,
                      body =
                      C.Exp.make
                      (C.Exp.E_Let
                       {decl = i_decl,
                        body =
                        C.Exp.make
                        (C.Exp.E_Let
                         {decl = v_decl,
                          body = bodyExp},
                         tyarg)},
                       tyarg)}
                  end
             | AST.Exp.E_BinOp (AST.BinOp.Div, expl, expr) =>
                  let
                     val explC = convertExp env expl
                     val exprC = convertExp env expr
                     val n = C.Var.new "n"
                     val n_ty = C.Exp.ty explC
                     val n_exp = C.Exp.make (C.Exp.E_Var {var = n}, n_ty)
                     val n_decl = 
                        C.Decl.D_Val
                        {var = n, var_ty = n_ty, exp = explC}
                     val d = C.Var.new "d"
                     val d_ty = C.Exp.ty exprC
                     val d_exp = C.Exp.make (C.Exp.E_Var {var = d}, d_ty)
                     val d_decl = 
                        C.Decl.D_Val
                        {var = d, var_ty = d_ty, exp = exprC}
                     val msgExp =
                        C.Exp.make
                        (C.Exp.E_String "division by 0 (/)",
                         C.Type.string)
                     val failExp =
                        C.Exp.make
                        (C.Exp.E_Prim {prim = C.Prim.Fail,
                                       tyargs = [C.Type.integer],
                                       args = [msgExp]},
                         C.Type.integer)
                     val zeroExp =
                        C.Exp.make
                        (C.Exp.E_Integer 0,
                         C.Type.integer)
                     val zeroEqDExp =
                        C.Exp.make
                        (C.Exp.E_Prim {prim = C.Prim.Eq,
                                       tyargs = [],
                                       args = [zeroExp, d_exp]},
                         C.Type.bool)
                     val divExp =
                        C.Exp.make
                        (C.Exp.E_Prim {prim = C.Prim.Div,
                                       tyargs = [],
                                       args = [n_exp, d_exp]},
                         C.Type.integer)
                     val bodyExp =
                        C.Exp.make
                        (C.Exp.E_Case
                         {arg = zeroEqDExp,
                          matchrules =
                          [C.MatchRule.MatchRule 
                           {pat = C.Pat.P_DaCon {dacon = C.DaCon.truee,
                                                 tyargs =[],
                                                 binds = []},
                            body = failExp},
                           C.MatchRule.MatchRule
                           {pat = C.Pat.P_DaCon {dacon = C.DaCon.falsee,
                                                 tyargs =[],
                                                 binds = []},
                            body = divExp}]},
                         C.Type.integer)
                  in
                     C.Exp.E_Let
                     {decl = n_decl,
                      body =
                      C.Exp.make
                      (C.Exp.E_Let
                       {decl = d_decl,
                        body = bodyExp},
                       C.Type.integer)}
                  end
             | AST.Exp.E_BinOp (AST.BinOp.Mod, expl, expr) =>
                  let
                     val explC = convertExp env expl
                     val exprC = convertExp env expr
                     val n = C.Var.new "n"
                     val n_ty = C.Exp.ty explC
                     val n_exp = C.Exp.make (C.Exp.E_Var {var = n}, n_ty)
                     val n_decl = 
                        C.Decl.D_Val
                        {var = n, var_ty = n_ty, exp = explC}
                     val d = C.Var.new "d"
                     val d_ty = C.Exp.ty exprC
                     val d_exp = C.Exp.make (C.Exp.E_Var {var = d}, d_ty)
                     val d_decl = 
                        C.Decl.D_Val
                        {var = d, var_ty = d_ty, exp = exprC}
                     val msgExp =
                        C.Exp.make
                        (C.Exp.E_String "division by 0 (%)",
                         C.Type.string)
                     val failExp =
                        C.Exp.make
                        (C.Exp.E_Prim {prim = C.Prim.Fail,
                                       tyargs = [C.Type.integer],
                                       args = [msgExp]},
                         C.Type.integer)
                     val zeroExp =
                        C.Exp.make
                        (C.Exp.E_Integer 0,
                         C.Type.integer)
                     val zeroEqDExp =
                        C.Exp.make
                        (C.Exp.E_Prim {prim = C.Prim.Eq,
                                       tyargs = [],
                                       args = [zeroExp, d_exp]},
                         C.Type.bool)
                     val modExp =
                        C.Exp.make
                        (C.Exp.E_Prim {prim = C.Prim.Mod,
                                       tyargs = [],
                                       args = [n_exp, d_exp]},
                         C.Type.integer)
                     val bodyExp =
                        C.Exp.make
                        (C.Exp.E_Case
                         {arg = zeroEqDExp,
                          matchrules =
                          [C.MatchRule.MatchRule 
                           {pat = C.Pat.P_DaCon {dacon = C.DaCon.truee,
                                                 tyargs =[],
                                                 binds = []},
                            body = failExp},
                           C.MatchRule.MatchRule
                           {pat = C.Pat.P_DaCon {dacon = C.DaCon.falsee,
                                                 tyargs =[],
                                                 binds = []},
                            body = modExp}]},
                         C.Type.integer)
                  in
                     C.Exp.E_Let
                     {decl = n_decl,
                      body =
                      C.Exp.make
                      (C.Exp.E_Let
                       {decl = d_decl,
                        body = bodyExp},
                       C.Type.integer)}
                  end
             | AST.Exp.E_BinOp (AST.BinOp.Idx ty, expl, expr) =>
                  let
                     val tyarg = convertType env ty
                     val explC = convertExp env expl
                     val exprC = convertExp env expr
                     val a = C.Var.new "a"
                     val a_ty = C.Exp.ty explC
                     val a_param = C.Param.P_Var {var = a, var_ty = a_ty}
                     val a_exp = C.Exp.make (C.Exp.E_Var {var = a}, a_ty)
                     val a_decl = 
                        C.Decl.D_Val
                        {var = a, var_ty = a_ty, exp = explC}
                     val i = C.Var.new "i"
                     val i_ty = C.Exp.ty exprC
                     val i_param = C.Param.P_Var {var = i, var_ty = i_ty}
                     val i_exp = C.Exp.make (C.Exp.E_Var {var = i}, i_ty)
                     val i_decl = 
                        C.Decl.D_Val
                        {var = i, var_ty = i_ty, exp = exprC}
                     val msgExp =
                        C.Exp.make
                        (C.Exp.E_String "invalid array index (!)",
                         C.Type.string)
                     val failExp =
                        C.Exp.make
                        (C.Exp.E_Prim {prim = C.Prim.Fail,
                                       tyargs = [tyarg],
                                       args = [msgExp]},
                         tyarg)
                     val zeroExp =
                        C.Exp.make
                        (C.Exp.E_Integer 0,
                         C.Type.integer)
                     val zeroLteIExp =
                        C.Exp.make
                        (C.Exp.E_Prim {prim = C.Prim.Lte,
                                       tyargs = [],
                                       args = [zeroExp, i_exp]},
                         C.Type.bool)
                     val lenExp =
                        C.Exp.make
                        (C.Exp.E_Prim {prim = C.Prim.Len,
                                       tyargs = [tyarg],
                                       args = [a_exp]},
                         C.Type.integer)
                     val iLtLenExp =
                        C.Exp.make
                        (C.Exp.E_Prim {prim = C.Prim.Lt,
                                       tyargs = [],
                                       args = [i_exp, lenExp]},
                         C.Type.bool)
                     val idxExp =
                        C.Exp.make
                        (C.Exp.E_Prim
                         {prim = C.Prim.Idx,
                          tyargs = [tyarg],
                          args = [a_exp, i_exp]},
                         tyarg)
                     val bodyExp =
                        C.Exp.make
                        (C.Exp.E_Case
                         {arg = zeroLteIExp,
                          matchrules =
                          [C.MatchRule.MatchRule 
                           {pat = C.Pat.P_DaCon {dacon = C.DaCon.truee,
                                                 tyargs =[],
                                                 binds = []},
                            body =
                            C.Exp.make
                            (C.Exp.E_Case
                             {arg = iLtLenExp,
                              matchrules =
                              [C.MatchRule.MatchRule
                               {pat = C.Pat.P_DaCon {dacon = C.DaCon.truee,
                                                     tyargs =[],
                                                     binds = []},
                                body = idxExp},
                               C.MatchRule.MatchRule
                               {pat = C.Pat.P_DaCon {dacon = C.DaCon.falsee,
                                                     tyargs =[],
                                                     binds = []},
                                body = failExp}]},
                             tyarg)},                               
                           C.MatchRule.MatchRule
                           {pat = C.Pat.P_DaCon {dacon = C.DaCon.falsee,
                                                 tyargs =[],
                                                 binds = []},
                            body = failExp}]},
                         tyarg)
                  in
                     C.Exp.E_Let
                     {decl = a_decl,
                      body =
                      C.Exp.make
                      (C.Exp.E_Let
                       {decl = i_decl,
                        body = bodyExp},
                       tyarg)}
                  end
             | AST.Exp.E_BinOp (binop, expl, expr) =>
                  let
                     val (prim, tyargs) =
                        case binop of
                           AST.BinOp.Eq => (C.Prim.Eq, [])
                         | AST.BinOp.NEq => (C.Prim.NEq, [])
                         | AST.BinOp.Lt => (C.Prim.Lt, [])
                         | AST.BinOp.Lte => (C.Prim.Lte, [])
                         | AST.BinOp.Gt => (C.Prim.Gt, [])
                         | AST.BinOp.Gte => (C.Prim.Gte, [])
                         | AST.BinOp.Concat => (C.Prim.Concat, [])
                         | AST.BinOp.Add => (C.Prim.Add, [])
                         | AST.BinOp.Sub => (C.Prim.Sub, [])
                         | AST.BinOp.Mul => (C.Prim.Mul, [])
                         | AST.BinOp.Div => raise Fail "AST.BinOp.Div"
                         | AST.BinOp.Mod => raise Fail "AST.BinOp.Mod"
                         | AST.BinOp.Idx _ => raise Fail "AST.BinOp.Idx"
                     val explC = convertExp env expl
                     val exprC = convertExp env expr
                  in
                     C.Exp.E_Prim
                     {prim = prim,
                      tyargs = tyargs,
                      args = [explC, exprC]}
                  end
             | AST.Exp.E_UnOp (unop, expo) =>
                  let
                     val (prim, tyargs) =
                        case unop of
                           AST.UnOp.Neg => (C.Prim.Neg, [])
                         | AST.UnOp.Len ty => (C.Prim.Len, [convertType env ty])
                     val expoC = convertExp env expo
                  in
                     C.Exp.E_Prim
                     {prim = prim,
                      tyargs = tyargs,
                      args = [expoC]}
                  end
             | AST.Exp.E_DaCon (dacon, tyargs, args) =>
                  let
                     val tyargsC =
                        List.map (convertType env) tyargs
                     val argsC =
                        List.map (convertExp env) args
                  in
                     case Env.lookupDaCon (env, dacon) of
                        NONE =>
                           raiseConvertError
                           (["Unbound data constructor: ",
                             toStringDaCon dacon, ".\n"])
                       | SOME daconC =>
                           C.Exp.E_DaCon
                           {dacon = daconC,
                            tyargs = tyargsC,
                            args = argsC}
                  end
             | AST.Exp.E_Apply (func, applyarg) =>
                  let
                     val funcC = convertExp env func
                     val applyargC = convertApplyArg env applyarg
                  in
                     C.Exp.E_Apply
                     {func = funcC,
                      applyarg = applyargC}
                  end
             | AST.Exp.E_Var var =>
                  (case Env.lookupVar (env, var) of
                      NONE =>
                         raiseConvertError
                         (["Unbound variable: ",
                           toStringVar var, ".\n"])
                    | SOME varC =>
                         C.Exp.E_Var
                         {var = varC})
             | AST.Exp.E_Integer i => C.Exp.E_Integer i
             | AST.Exp.E_String s => C.Exp.E_String s
             | AST.Exp.E_Seq exps => C.Exp.node (convertExps env exps)
             | AST.Exp.E_Let (decls, body) =>
                  let
                     val (env', declsC) =
                        convertDecls env decls
                     val bodyC = convertExps (Env.extend (env, env')) body
                  in
                     C.Exp.node
                     (List.foldl
                      (fn (declC, bodyC) =>
                       C.Exp.make
                       (C.Exp.E_Let
                        {decl = declC, body = bodyC},
                        C.Exp.ty bodyC))
                      bodyC
                      (List.rev declsC))
                  end
             | AST.Exp.E_Case (arg, matchrules) =>
                  let
                     val argC = convertExp env arg
                     val matchrulesC =
                        List.map (convertMatchRule env) matchrules
                     fun nonCase () =
                        case matchrulesC of
                           [C.MatchRule.MatchRule {pat = C.Pat.P_Var (bindC as {var_ty = var_tyC, ...}),
                                                   body = bodyC}] =>
                              let
                                 val body_tyC = C.Exp.ty bodyC
                                 val lamC =
                                    C.Lam.Lam
                                    {param = C.Param.P_Var bindC,
                                     body = bodyC}
                                 val lam_tyC = C.Type.T_Fn {arg = var_tyC, res = body_tyC}
                              in
                                 C.Exp.E_Apply
                                 {func = C.Exp.make (C.Exp.E_Fn {lam = lamC}, lam_tyC),
                                  applyarg = C.ApplyArg.A_Exp argC}
                              end
                         | _ =>
                              raiseConvertError
                              (["Bad match rules in 'case':\n",
                                Layout.toString (AST.Exp.layoutNode node), "\n"])
                  in
                     case C.Exp.ty argC of
                        C.Type.T_TyCon {tycon, ...} =>
                           if C.TyCon.equals (tycon, C.TyCon.array)
                              orelse C.TyCon.equals (tycon, C.TyCon.integer)
                              orelse C.TyCon.equals (tycon, C.TyCon.string)
                              then nonCase ()
                           else C.Exp.E_Case
                                {arg = argC,
                                 matchrules = matchrulesC}
                      | _ => nonCase ()
                  end
         and convertExps (env: Env.t) (exps: AST.Exp.t list)
                         : C.Exp.t =
            let
               val expsC = List.map (convertExp env) exps
            in
               case List.rev expsC of
                  nil => C.Exp.unit
                | expC::expsC =>
                     List.foldl
                     (fn (expC, bodyC) =>
                      C.Exp.make
                      (C.Exp.E_Let
                       {decl = C.Decl.D_Val
                               {var = C.Var.new "zzz",
                                var_ty = C.Exp.ty expC,
                                exp = expC},
                               body = bodyC},
                       C.Exp.ty bodyC))
                     expC
                     expsC
            end
         and convertApplyArg (env: Env.t) (applyarg: AST.ApplyArg.t)
                             : C.ApplyArg.t =
            case applyarg of
               AST.ApplyArg.A_Exp exp =>
                  C.ApplyArg.A_Exp (convertExp env exp)
             | AST.ApplyArg.A_Type ty =>
                  C.ApplyArg.A_Type (convertType env ty)
         and convertMatchRule (env: Env.t) (matchrule: AST.MatchRule.t)
                              : C.MatchRule.t =
            case matchrule of
               AST.MatchRule.MatchRule (pat, body) =>
                  let
                     val (env', patC) =
                        convertPat env pat
                     val bodyC = convertExp (Env.extend (env, env')) body
                  in
                     C.MatchRule.MatchRule
                     {pat = patC,
                      body = bodyC}
                  end
         and convertDecl env (decl: AST.Decl.t) :
                         Env.t * C.Decl.t =
            case decl of
               AST.Decl.D_Data data_decls =>
                  let
                     val (data_declsAux, envTC) =
                        ListExtra.mapAndFoldl
                        (fn ((tycon, tyvars, dacon_decls), envTC) =>
                         let
                            val tyconC = C.TyCon.new (AST.TyCon.name tycon)
                            val (tyvarsC, envTV) =
                               ListExtra.mapAndFoldl
                               (fn (tyvar, envTV) =>
                                let
                                   val tyvarC = C.TyVar.new (AST.TyVar.name tyvar)
                                   val envTV_i =
                                      Env.singletonTyVar (tyvar, tyvarC)
                                in
                                   (tyvarC,
                                    Env.extend (envTV, envTV_i))
                                end)
                               Env.empty
                               tyvars
                            val envTC_i = Env.singletonTyCon (tycon, tyconC)
                         in
                            ((tyconC, tyvarsC, envTV, dacon_decls),
                             Env.extend (envTC, envTC_i))
                         end)
                        Env.empty
                        data_decls
                     val (data_declsC, envDC) =
                        ListExtra.mapAndFoldl
                        (fn ((tyconC, tyvarsC, envTV, dacon_decls), envDC) =>
                         let
                            val (envDC_i, dacon_declsC) =
                               convertDaConDecls (Env.extend (env, (Env.extend (envTC, envTV))))
                                                 dacon_decls
                         in
                            ({tycon = tyconC,
                              tyvars = tyvarsC,
                              dacon_decls = dacon_declsC},
                             (Env.extend (envDC, envDC_i)))
                         end)
                        Env.empty
                        data_declsAux
                     val env' = Env.extend (envTC, envDC)
                  in
                     (env',
                      C.Decl.D_Data {data_decls = data_declsC})
                  end
             | AST.Decl.D_Val (pat, exp) =>
                  let
                     val (env', {var = varC, var_ty = var_tyC}) =
                        convertSimplePat env pat
                     val expC = convertExp env exp
                  in
                     (env',
                      C.Decl.D_Val
                      {var = varC,
                       var_ty = var_tyC,
                       exp = expC})
                  end
             | AST.Decl.D_Fun fun_decls =>
                  let
                     val (fun_declsAux, env') =
                        ListExtra.mapAndFoldl
                        (fn ((func, params, res_ty, body), env') =>
                         let
                            val funcC = C.Var.new (AST.Var.name func)
                            val (envP, mkTy, mkLam) =
                               convertParams env params
                            val res_tyC =
                               convertType (Env.extend (env, envP)) res_ty
                            val func_tyC = mkTy res_tyC
                            val envF = Env.singletonVar (func, funcC)
                         in
                            ((funcC, func_tyC, envP, mkLam, body),
                             Env.extend (env', envF))
                         end)
                        Env.empty
                        fun_decls
                     val fun_declsC =
                        List.map
                        (fn (funcC, func_tyC, envP, mkLam, body) =>
                         let
                            val body_env =
                               Env.extend (env, Env.extend (env', envP))
                            val bodyC = convertExp body_env body
                         in
                            {func = funcC,
                             func_ty = func_tyC,
                             lam = mkLam bodyC}
                         end)
                        fun_declsAux
                  in
                     (env', C.Decl.D_Fun {fun_decls = fun_declsC})
                  end
         and convertDecls (env: Env.t) (decls: AST.Decl.t list)
                          : Env.t * C.Decl.t list =
            let
               val (declsC, env') =
                  ListExtra.mapAndFoldl
                  (fn (decl_i, env') =>
                   let
                      val (env'_i, declC_i) =
                         convertDecl (Env.extend (env, env')) decl_i
                   in
                      (declC_i,
                       Env.extend (env', env'_i))
                   end)
                  Env.empty
                  decls
            in
               (env', declsC)
            end

         fun convertProg (prog: AST.Prog.t) : C.Prog.t =
            let
               val env0 = Env.initial
               val (primDeclsC, envP) =
                  ListExtra.mapAndFoldl
                  (fn ((funcAST, paramsC, res_tyC, bodyNodeC), envP) =>
                   let
                      val funcC = C.Var.new (AST.Var.name funcAST)
                      val (mkTy, mkLam) =
                         List.foldl
                         (fn (paramC_i, (mkTy, mkLam)) =>
                          let
                             val mkTy_i =
                                case paramC_i of
                                   C.Param.P_Var {var, var_ty} =>
                                      (fn res_ty =>
                                       C.Type.alphaRename (C.Type.T_Fn {arg = var_ty,
                                                                        res = res_ty}))
                                 | C.Param.P_TyVar {tyvar} =>
                                      (fn res_ty =>
                                       C.Type.alphaRename (C.Type.T_TyFn {tyvar = tyvar,
                                                                          res = res_ty}))
                             val mkLam_i = fn expC =>
                                C.Lam.Lam {param = paramC_i, body = expC}
                          in
                             (mkTy o mkTy_i,
                              case mkLam of
                                 NONE => SOME mkLam_i
                               | SOME mkLam =>
                                    SOME (fn expC =>
                                          mkLam (C.Exp.make (C.Exp.E_Fn {lam = mkLam_i expC},
                                                             mkTy_i (C.Exp.ty expC)))))

                          end)
                         (fn ty => ty, NONE)
                         paramsC
                      val tyC = mkTy res_tyC
                      val lamC = (valOf mkLam) (C.Exp.make (bodyNodeC, res_tyC))
                      val decl =
                         C.Decl.D_Val
                         {var = funcC,
                          var_ty = tyC,
                          exp = C.Exp.make (C.Exp.E_Fn {lam = lamC}, tyC)}
                      val env' =
                         Env.singletonVar (funcAST, funcC)
                   in
                      (decl, Env.extend (envP, env'))
                   end)
                  Env.empty
                  (let
                      val argc =
                         let
                            val u = C.Var.new "u"
                            val u_ty = C.Type.unit
                            val u_param = C.Param.P_Var {var = u, var_ty = u_ty}
                            val u_exp = C.Exp.make (C.Exp.E_Var {var = u}, u_ty)
                            val res_ty = C.Type.integer
                         in
                            (AST.Var.argc,
                             [u_param],
                             res_ty,
                             C.Exp.E_Prim {prim = C.Prim.Argc,
                                           tyargs = [],
                                           args = [u_exp]})
                         end
                      val arg =
                         let
                            val i = C.Var.new "i"
                            val i_ty = C.Type.integer
                            val i_param = C.Param.P_Var {var = i, var_ty = i_ty}
                            val i_exp = C.Exp.make (C.Exp.E_Var {var = i}, i_ty)
                            val res_ty = C.Type.string
                         in
                            (AST.Var.arg,
                             [i_param],
                             res_ty,
                             C.Exp.E_Prim {prim = C.Prim.Arg,
                                           tyargs = [],
                                           args = [i_exp]})
                         end
                      val fail =
                         let
                            val a = C.TyVar.new "'a"
                            val a_param = C.Param.P_TyVar {tyvar = a}
                            val a_ty = C.Type.T_TyVar {tyvar = a}
                            val s = C.Var.new "s"
                            val s_ty = C.Type.string
                            val s_param = C.Param.P_Var {var = s, var_ty = s_ty}
                            val s_exp = C.Exp.make (C.Exp.E_Var {var = s}, s_ty)
                            val res_ty = a_ty
                         in
                            (AST.Var.fail,
                             [a_param, s_param],
                             res_ty,
                             C.Exp.E_Prim {prim = C.Prim.Fail,
                                           tyargs = [a_ty],
                                           args = [s_exp]})
                         end
                      val print =
                         let
                            val s = C.Var.new "s"
                            val s_ty = C.Type.string
                            val s_param = C.Param.P_Var {var = s, var_ty = s_ty}
                            val s_exp = C.Exp.make (C.Exp.E_Var {var = s}, s_ty)
                            val res_ty = C.Type.unit
                         in
                            (AST.Var.print,
                             [s_param],
                             res_ty,
                             C.Exp.E_Prim {prim = C.Prim.Print,
                                           tyargs = [],
                                           args = [s_exp]})
                         end
                      val size =
                         let
                            val s = C.Var.new "s"
                            val s_ty = C.Type.string
                            val s_param = C.Param.P_Var {var = s, var_ty = s_ty}
                            val s_exp = C.Exp.make (C.Exp.E_Var {var = s}, s_ty)
                            val res_ty = C.Type.integer
                         in
                            (AST.Var.size,
                             [s_param],
                             res_ty,
                             C.Exp.E_Prim {prim = C.Prim.Size,
                                           tyargs = [],
                                           args = [s_exp]})
                         end
                      val subscript =
                         let
                            val s = C.Var.new "s"
                            val s_ty = C.Type.string
                            val s_param = C.Param.P_Var {var = s, var_ty = s_ty}
                            val s_exp = C.Exp.make (C.Exp.E_Var {var = s}, s_ty)
                            val i = C.Var.new "i"
                            val i_ty = C.Type.integer
                            val i_param = C.Param.P_Var {var = i, var_ty = i_ty}
                            val i_exp = C.Exp.make (C.Exp.E_Var {var = i}, i_ty)
                            val res_ty = C.Type.integer
                         in
                            (AST.Var.subscript,
                             [s_param, i_param],
                             res_ty,
                             C.Exp.E_Prim {prim = C.Prim.Subscript,
                                           tyargs = [],
                                           args = [s_exp, i_exp]})
                         end
                      val array =
                         let
                            val a = C.TyVar.new "'a"
                            val a_param = C.Param.P_TyVar {tyvar = a}
                            val a_ty = C.Type.T_TyVar {tyvar = a}
                            val i = C.Var.new "i"
                            val i_ty = C.Type.integer
                            val i_param = C.Param.P_Var {var = i, var_ty = i_ty}
                            val i_exp = C.Exp.make (C.Exp.E_Var {var = i}, i_ty)
                            val v = C.Var.new "v"
                            val v_ty = a_ty
                            val v_param = C.Param.P_Var {var = v, var_ty = v_ty}
                            val v_exp = C.Exp.make (C.Exp.E_Var {var = v}, v_ty)
                            val res_ty = C.Type.array a_ty
                            val msgExp =
                               C.Exp.make
                               (C.Exp.E_String "invalid array length (array)",
                                C.Type.string)
                            val failExp =
                               C.Exp.make
                               (C.Exp.E_Prim {prim = C.Prim.Fail,
                                              tyargs = [C.Type.array a_ty],
                                              args = [msgExp]},
                                C.Type.array a_ty)
                            val zeroExp =
                               C.Exp.make
                               (C.Exp.E_Integer 0,
                                C.Type.integer)
                            val zeroLteIExp =
                               C.Exp.make
                               (C.Exp.E_Prim {prim = C.Prim.Lte,
                                              tyargs = [],
                                              args = [zeroExp, i_exp]},
                                C.Type.bool)
                            val arrayExp =
                               C.Exp.make
                               (C.Exp.E_Prim {prim = C.Prim.Array,
                                              tyargs = [a_ty],
                                              args = [i_exp, v_exp]},
                                C.Type.array a_ty)
                            val bodyNode =
                               C.Exp.E_Case
                               {arg = zeroLteIExp,
                                matchrules =
                                [C.MatchRule.MatchRule 
                                 {pat = C.Pat.P_DaCon {dacon = C.DaCon.truee,
                                                       tyargs =[],
                                                       binds = []},
                                  body = arrayExp},
                                 C.MatchRule.MatchRule
                                 {pat = C.Pat.P_DaCon {dacon = C.DaCon.falsee,
                                                       tyargs =[],
                                                       binds = []},
                                  body = failExp}]}
                         in
                            (AST.Var.array,
                             [a_param, i_param, v_param],
                             res_ty, bodyNode)
                         end
                   in
                      [argc, arg, fail, print, size, subscript, array]
                   end)
               val AST.Prog.Prog (decls, exp) = prog
               val (env', declsC) = convertDecls (Env.extend (env0, envP)) decls
               val expC = convertExp (Env.extend (Env.extend (env0, envP), env')) exp
               val progC = C.Prog.Prog {decls = primDeclsC @ declsC, exp = expC}
            in
               progC
            end
      in
         convertProg prog
      end

   val convert =
      Control.mkKeepCtlPass
      {keepPre = NONE,
       keepPost = SOME {output = CoreIR.Prog.output,
                        ext = "core"},
       passName = "convert-to-core",
       pass = convert}

   val convert =
      Control.mkTracePass
      {msgPre = NONE,
       msgPost = NONE,
       passName = "convert-to-core",
       pass = convert}
end
