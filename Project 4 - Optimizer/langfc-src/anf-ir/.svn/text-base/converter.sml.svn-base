(* langfc-src/anf-ir/convert.sml
 *
 * COPYRIGHT (c) 2015 Matthew Fluet (http://www.cs.rit.edu/~mtf)
 * All rights reserved.
 *
 * Rochester Institute of Technology
 * CSCI-742
 * S20135,S20145
 *
 * Conversion from core intermediate representation to A-normal form
 * intermediate representation in the LangF compiler (langfc).
 *)

structure AnfIRConverter :> ANF_IR_CONVERTER =
struct
   structure C = CoreIR
   structure A = AnfIR

   structure Env =
      struct
         structure TyVarEnv =
            struct
               type dom = C.TyVar.t
               type cod = A.TyVar.t
               type t = cod C.TyVar.Map.map
               val empty : t = C.TyVar.Map.empty
               val singleton : dom * cod -> t = C.TyVar.Map.singleton
               val lookup : t * dom -> cod option = C.TyVar.Map.find
               val extend : t * t -> t = C.TyVar.Map.unionWith #2
            end
         structure TyConEnv =
            struct
               type dom = C.TyCon.t
               type cod = A.TyCon.t
               type t = cod C.TyCon.Map.map
               val empty : t = C.TyCon.Map.empty
               val singleton : dom * cod -> t = C.TyCon.Map.singleton
               val lookup : t * dom -> cod option = C.TyCon.Map.find
               val extend : t * t -> t = C.TyCon.Map.unionWith #2
            end
         structure DaConEnv =
            struct
               type dom = C.DaCon.t
               type cod = A.DaCon.t
               type t = cod C.DaCon.Map.map
               val empty : t = C.DaCon.Map.empty
               val singleton : dom * cod -> t = C.DaCon.Map.singleton
               val lookup : t * dom -> cod option = C.DaCon.Map.find
               val extend : t * t -> t = C.DaCon.Map.unionWith #2
            end
         structure VarEnv =
            struct
               type dom = C.Var.t
               type cod = A.Var.t
               type t = cod C.Var.Map.map
               val empty : t = C.Var.Map.empty
               val singleton : dom * cod -> t = C.Var.Map.singleton
               val lookup : t * dom -> cod option = C.Var.Map.find
               val extend : t * t -> t = C.Var.Map.unionWith #2
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

         fun fromTyConEnv tyconEnv =
            Env {tyvarEnv = TyVarEnv.empty,
                 tyconEnv = tyconEnv,
                 daconEnv = DaConEnv.empty,
                 varEnv = VarEnv.empty}
         val singletonTyCon = fromTyConEnv o TyConEnv.singleton
         fun lookupTyCon (Env {tyconEnv, ...}, tycon) =
            TyConEnv.lookup (tyconEnv, tycon)

         fun fromDaConEnv daconEnv =
            Env {tyvarEnv = TyVarEnv.empty,
                 tyconEnv = TyConEnv.empty,
                 daconEnv = daconEnv,
                 varEnv = VarEnv.empty}
         val singletonDaCon = fromDaConEnv o DaConEnv.singleton
         fun lookupDaCon (Env {daconEnv, ...}, dacon) =
            DaConEnv.lookup (daconEnv, dacon)

         fun fromVarEnv varEnv =
            Env {tyvarEnv = TyVarEnv.empty,
                 tyconEnv = TyConEnv.empty,
                 daconEnv = DaConEnv.empty,
                 varEnv = varEnv}
         val singletonVar = fromVarEnv o VarEnv.singleton
         fun lookupVar (Env {varEnv, ...}, var) =
            VarEnv.lookup (varEnv, var)

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
      end

   fun toStringTyVar tyvar =
      Layout.toString (C.TyVar.layout tyvar)
   fun toStringTyCon tycon =
      Layout.toString (C.TyCon.layout tycon)
   fun toStringDaCon dacon =
      Layout.toString (C.DaCon.layout dacon)
   fun toStringVar var =
      Layout.toString (C.Var.layout var)

   fun convert (prog : C.Prog.t) : A.Prog.t =
      let
         exception ConvertError
         fun raiseConvertError msgs =
            (List.map (fn msg => (TextIO.output (TextIO.stdErr, msg))) ("Convert Error:\n" :: msgs)
             ; TextIO.output1 (TextIO.stdErr, #"\n")
             ; raise ConvertError)

         fun convertType (env, ty) =
            case ty of
               C.Type.T_TyFn {tyvar, res} =>
                  let
                     val tyvarA = A.TyVar.new (C.TyVar.name tyvar)
                     val resA = convertType (Env.extend (env, Env.singletonTyVar (tyvar, tyvarA)), res)
                  in
                     A.Type.T_TyFn {tyvar = tyvarA, res = resA}
                  end
             | C.Type.T_Fn {arg, res} =>
                  let
                     val argA = convertType (env, arg)
                     val resA = convertType (env, res)
                  in
                     A.Type.T_Fn {arg = argA, res = resA}
                  end
             | C.Type.T_TyCon {tycon, tyargs} =>
                  let
                     val tyargsA = convertTypes (env, tyargs)
                  in
                     case Env.lookupTyCon (env, tycon) of
                        NONE =>
                           raiseConvertError
                           (["Unbound type constructor: ",
                             toStringTyCon tycon, "."])
                      | SOME tyconA =>
                           A.Type.T_TyCon {tycon = tyconA, tyargs = tyargsA}
                  end
             | C.Type.T_TyVar {tyvar} =>
                  (case Env.lookupTyVar (env, tyvar) of
                      NONE =>
                         raiseConvertError
                         (["Unbound type variable: ",
                           toStringTyVar tyvar, "."])
                    | SOME tyvarA =>
                         A.Type.T_TyVar {tyvar = tyvarA})
         and convertTypes (env, tys) =
            List.map (fn ty => convertType (env, ty)) tys
         val convertType = fn (env, ty) =>
            A.Type.clone (convertType (env, ty))

         fun convertPat (env, pat) =
            case pat of
               C.Pat.P_DaCon {dacon, tyargs, binds} =>
                  let
                     val tyargsA = convertTypes (env, tyargs)
                     val (bindsA, env') =
                        ListExtra.mapAndFoldl
                        (fn ({var = var_i, var_ty = var_ty_i}, env') =>
                         let
                            val varA_i = A.Var.new (C.Var.name var_i)
                            val var_tyA_i = convertType (env, var_ty_i)
                            val env'_i = Env.singletonVar (var_i, varA_i)
                         in
                            ({var = varA_i, var_ty = var_tyA_i},
                             Env.extend (env', env'_i))
                         end)
                        Env.empty
                        binds
                  in
                     case Env.lookupDaCon (env, dacon) of
                        NONE =>
                           raiseConvertError
                           (["Unbound data constructor: ",
                             toStringDaCon dacon, ".\n"])
                      | SOME daconA =>
                           (env',
                            A.Pat.P_DaCon {dacon = daconA,
                                           tyargs = tyargsA,
                                           binds = bindsA})
                  end
             | C.Pat.P_Var {var, var_ty} =>
                  let
                     val varA = A.Var.new (C.Var.name var)
                     val var_tyA = convertType (env, var_ty)
                     val env' = Env.singletonVar (var, varA)
                  in
                     (env',
                      A.Pat.P_Var {var = varA, var_ty = var_tyA})
                  end

         fun convertDaConDecl (env, dacon_decl) =
            let
               val {dacon, arg_tys} = dacon_decl
               val daconC = A.DaCon.new (C.DaCon.name dacon)
               val arg_tysC = convertTypes (env, arg_tys)
               val env' = Env.singletonDaCon (dacon, daconC)
            in
               (env',
                {dacon = daconC, arg_tys = arg_tysC})
            end
         fun convertDaConDecls (env, dacon_decls) =
            let
               val (dacon_declsC, env') =
                  ListExtra.mapAndFoldl
                  (fn (dacon_decl_i, env') =>
                   let
                      val (env'_i, dacon_declC_i) = convertDaConDecl (env, dacon_decl_i)
                   in
                      (dacon_declC_i,
                       Env.extend (env', env'_i))
                   end)
                  Env.empty
                  dacon_decls
            in
               (env', dacon_declsC)
            end

         fun convertExp (env, exp) =
            let
               val (declsA, rhsA) = convertExpNode (env, C.Exp.node exp)
               val tyA = convertType (env, C.Exp.ty exp)
               val varA = A.Var.new "x"
            in
               (declsA @ [A.Decl.D_Val {var = varA, var_ty = tyA, rhs = rhsA}],
                varA, fn () => A.Type.clone tyA)
            end
         and convertExps (env, exps) =
            let
               val (varsA, declssA) =
                  ListExtra.mapAndFoldl
                  (fn (exp_i, declssA) =>
                   let
                      val (declsA_i, varA_i, _) = convertExp (env, exp_i)
                   in
                      (varA_i, declsA_i :: declssA)
                   end)
                  []
                  exps
            in
               (List.concat (List.rev declssA), varsA)
            end
         and convertExpNode (env, node) =
            case node of
               C.Exp.E_Fn {lam} =>
                  let
                     val lamA = convertLam (env, lam)
                  in
                     ([], A.RHS.R_Fn {lam = lamA})
                  end
             | C.Exp.E_Prim {prim, tyargs, args} =>
                  let
                     val tyargsA = convertTypes (env, tyargs)
                     val (declsA, argsA) = convertExps (env, args)
                  in
                     (declsA, A.RHS.R_Prim {prim = prim, tyargs = tyargsA, args = argsA})
                  end
             | C.Exp.E_DaCon {dacon, tyargs, args} =>
                  let
                     val tyargsA = convertTypes (env, tyargs)
                     val (declsA, argsA) = convertExps (env, args)
                  in
                     case Env.lookupDaCon (env, dacon) of
                        NONE =>
                           raiseConvertError
                           (["Unbound data constructor: ",
                             toStringDaCon dacon, "."])
                       | SOME daconA =>
                           (declsA, A.RHS.R_DaCon {dacon = daconA, tyargs = tyargsA, args = argsA})
                  end
             | C.Exp.E_Apply {func, applyarg = C.ApplyArg.A_Exp arg} =>
                  let
                     val (decls_funcA, funcA, _) = convertExp (env, func)
                     val (decls_argA, argA, _) = convertExp (env, arg)
                  in
                     (decls_funcA @ decls_argA, A.RHS.R_VApply {func = funcA, arg = argA})
                  end
             | C.Exp.E_Apply {func, applyarg = C.ApplyArg.A_Type tyarg} =>
                  let
                     val (decls_funcA, funcA, _) = convertExp (env, func)
                     val tyargA = convertType (env, tyarg)
                  in
                     (decls_funcA, A.RHS.R_TApply {func = funcA, tyarg = tyargA})
                  end
             | C.Exp.E_Var {var} =>
                  (case Env.lookupVar (env, var) of
                      NONE =>
                         raiseConvertError
                         (["Unbound variable: ",
                           toStringVar var, "."])
                    | SOME varC =>
                         ([], A.RHS.R_Var {var = varC}))
             | C.Exp.E_Integer i =>
                  ([], A.RHS.R_Integer i)
             | C.Exp.E_String s =>
                  ([], A.RHS.R_String s)
             | C.Exp.E_Let {decl, body} =>
                  let
                     val (env', decls_declA) = convertDecl (env, decl)
                     val (decls_bodyA, var_bodyA, _) = convertExp (Env.extend (env, env'), body)
                  in
                     (decls_declA @ decls_bodyA, A.RHS.R_Var {var = var_bodyA})
                  end
             | C.Exp.E_Case {arg, matchrules} =>
                  let
                     val (decls_argA, argA, _) = convertExp (env, arg)
                     val matchrulesA = List.map (fn mr => convertMatchRule (env, mr)) matchrules
                  in
                     (decls_argA, A.RHS.R_Case {arg = argA, matchrules = matchrulesA})
                  end
         and convertLam (env, lam) =
            case lam of
               C.Lam.Lam {param = C.Param.P_Var {var, var_ty}, body} =>
                  let
                     val varA = A.Var.new (C.Var.name var)
                     val var_tyA = convertType (env, var_ty)
                     val env' = Env.singletonVar (var, varA)
                     val (decls_bodyA, var_bodyA, ty_bodyA) = convertExp (Env.extend (env, env'), body)
                     val bodyA = A.Exp.Exp {decls = decls_bodyA, var = var_bodyA, ty = ty_bodyA ()}
                  in
                     A.Lam.L_VLam {var = varA, var_ty = var_tyA, body = bodyA}
                  end
             | C.Lam.Lam {param = C.Param.P_TyVar {tyvar}, body} =>
                  let
                     val tyvarA = A.TyVar.new (C.TyVar.name tyvar)
                     val env' = Env.singletonTyVar (tyvar, tyvarA)
                     val (decls_bodyA, var_bodyA, ty_bodyA) = convertExp (Env.extend (env, env'), body)
                     val bodyA = A.Exp.Exp {decls = decls_bodyA, var = var_bodyA, ty = ty_bodyA ()}
                  in
                     A.Lam.L_TLam {tyvar = tyvarA, body = bodyA}
                  end
         and convertMatchRule (env, matchrule) =
            case matchrule of
               C.MatchRule.MatchRule {pat, body} =>
                  let
                     val (env', patA) = convertPat (env, pat)
                     val (decls_bodyA, var_bodyA, ty_bodyA) = convertExp (Env.extend (env, env'), body)
                     val bodyA = A.Exp.Exp {decls = decls_bodyA, var = var_bodyA, ty = ty_bodyA ()}
                  in
                     A.MatchRule.MatchRule {pat = patA, body = bodyA}
                  end
         and convertDecl (env, decl) =
            case decl of
               C.Decl.D_Data {data_decls} =>
                  let
                     val (data_declsAux, envTC) =
                        ListExtra.mapAndFoldl
                        (fn ({tycon, tyvars, dacon_decls}, envTC) =>
                         let
                            val tyconA = A.TyCon.new (C.TyCon.name tycon)
                            val (tyvarsA, envTV) =
                               ListExtra.mapAndFoldl
                               (fn (tyvar, envTV) =>
                                let
                                   val tyvarA = A.TyVar.new (C.TyVar.name tyvar)
                                   val envTV_i = Env.singletonTyVar (tyvar, tyvarA)
                                in
                                   (tyvarA, Env.extend (envTV, envTV_i))
                                end)
                               Env.empty
                               tyvars
                            val envTC_i = Env.singletonTyCon (tycon, tyconA)
                         in
                            ((tyconA, tyvarsA, envTV, dacon_decls), Env.extend (envTC, envTC_i))
                         end)
                        Env.empty
                        data_decls
                     val (data_declsA, envDC) =
                        ListExtra.mapAndFoldl
                        (fn ((tyconA, tyvarsA, envTV, dacon_decls), envDC) =>
                         let
                            val (envDC_i, dacon_declsA) = convertDaConDecls (Env.extend (env, (Env.extend (envTC, envTV))), dacon_decls)
                         in
                            ({tycon = tyconA,
                              tyvars = tyvarsA,
                              dacon_decls = dacon_declsA},
                             (Env.extend (envDC, envDC_i)))
                         end)
                        Env.empty
                        data_declsAux
                     val env' = Env.extend (envTC, envDC)
                  in
                     (env',
                      [A.Decl.D_Data {data_decls = data_declsA}])
                  end
             | C.Decl.D_Val {var, var_ty, exp} =>
                  let
                     val varA = A.Var.new (C.Var.name var)
                     val var_tyA = convertType (env, var_ty)
                     val (declsA, rhsA) = convertExpNode (env, C.Exp.node exp)
                  in
                     (Env.singletonVar (var, varA),
                      declsA @ [A.Decl.D_Val {var = varA, var_ty = var_tyA, rhs = rhsA}])
                  end
             | C.Decl.D_Fun {fun_decls} =>
                  let
                     val (fun_declsAux, env') =
                        ListExtra.mapAndFoldl
                        (fn ({func, func_ty, lam}, env') =>
                         let
                            val funcA = A.Var.new (C.Var.name func)
                            val func_tyA = convertType (env, func_ty)
                            val envF = Env.singletonVar (func, funcA)
                         in
                            ((funcA, func_tyA, lam), Env.extend (env', envF))
                         end)
                        Env.empty
                        fun_decls
                     val fun_declsA =
                        List.map
                        (fn (funcA, func_tyA, lam) =>
                         let
                            val lamA = convertLam (Env.extend (env, env'), lam)
                         in
                            {func = funcA, func_ty = func_tyA, lam = lamA}
                         end)
                        fun_declsAux
                  in
                     (env',
                      [A.Decl.D_Fun {fun_decls = fun_declsA}])
                  end
         and convertDecls (env, decls) =
            let
               val (declssA, env') =
                  ListExtra.mapAndFoldl
                  (fn (decl_i, env') =>
                   let
                      val (env'_i, declsA_i) = convertDecl (Env.extend (env, env'), decl_i)
                   in
                      (declsA_i, Env.extend (env', env'_i))
                   end)
                  Env.empty
                  decls
            in
               (env', List.concat declssA)
            end

         fun convertProg prog =
            let
               (* The initial environment E_0. *)
               val env0 = Env.empty
               (* Pre-defined type constructors. *)
               val env0 =
                  List.foldl
                  (fn ((tyconAST, tyconC), env) =>
                   Env.extend (env, Env.singletonTyCon (tyconAST, tyconC)))
                  env0
                  [(C.TyCon.array, A.TyCon.array),
                   (C.TyCon.bool, A.TyCon.bool),
                   (C.TyCon.integer, A.TyCon.integer),
                   (C.TyCon.string, A.TyCon.string),
                   (C.TyCon.unit, A.TyCon.unit)]
               (* Pre-defined data constructors. *)
               val env0 =
                  List.foldl
                  (fn ((daconAST, daconC), env) =>
                   Env.extend (env, Env.singletonDaCon (daconAST, daconC)))
                  env0
                  [(C.DaCon.falsee, A.DaCon.falsee),
                   (C.DaCon.truee, A.DaCon.truee),
                   (C.DaCon.unit, A.DaCon.unit)]
               val C.Prog.Prog {decls, exp} = prog
               val (env', declsA) = convertDecls (env0, decls)
               val (decls_expA, var_expA, ty_expA) = convertExp (Env.extend (env0, env'), exp)
               val progA = A.Prog.Prog {decls = declsA @ decls_expA, var = var_expA, ty = ty_expA ()}
            in
               progA
            end
      in
         convertProg prog
      end

   val convert =
      Control.mkKeepCtlPass
      {keepPre = NONE,
       keepPost = SOME {output = AnfIR.Prog.output,
                        ext = "anf"},
       passName = "convert-to-anf",
       pass = convert}

   val convert =
      Control.mkTracePass
      {msgPre = NONE,
       msgPost = NONE,
       passName = "convert-to-anf",
       pass = convert}
end
