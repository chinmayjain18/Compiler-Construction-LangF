(* langfc-src/anf-ir/copy-prop.sml
 *
 * COPYRIGHT (c) 2015 Matthew Fluet (http://www.cs.rit.edu/~mtf)
 * All rights reserved.
 *
 * Rochester Institute of Technology
 * CSCI-742
 * S20135,S20145
 *
 * Copy propagation optimization for A-normal form intermediate
 * representation.
 *
 *
 * Transform
 *
 *   let
 *     ...
 *     val y = x
 *     ...
 *     val z = ... y ...
 *     ...
 *   in
 *     y
 *   end
 *
 * to
 *
 *   let
 *     ...
 *     val y = x
 *     ...
 *     val z = ... x ...
 *     ...
 *   in
 *     x
 *   end
 *
 *)

structure CopyPropTbl : ANF_IR_OPTIMIZATION =
struct
   structure A = AnfIR

   structure Tbl =
      struct
         structure VarTbl =
            struct
               type dom = A.Var.t
               type cod = A.Var.t
               type t = cod A.Var.Tbl.hash_table
               val lookup : t * dom -> cod option = fn (tbl, var) =>
                  A.Var.Tbl.find tbl var
               val insert : t * dom * cod -> unit = fn (tbl, var, var') =>
                  A.Var.Tbl.insert tbl (var, var')
               val new : unit -> t = fn () =>
                  A.Var.Tbl.mkTable (32, Fail "VarTbl")
            end

         datatype t =
            Tbl of {varTbl: VarTbl.t}

         val new = fn () =>
            Tbl {varTbl = VarTbl.new ()}

         fun lookupVar (Tbl {varTbl, ...}, var) =
            VarTbl.lookup (varTbl, var)
         fun insertVar (Tbl {varTbl, ...}, var, var') =
            VarTbl.insert (varTbl, var, var')
      end

   fun xformVar (tbl: Tbl.t, var: A.Var.t) : A.Var.t =
      case Tbl.lookupVar (tbl, var) of
         NONE => var
       | SOME var' => var'
   fun xformVars (tbl: Tbl.t, vars: A.Var.t list) : A.Var.t list =
      List.map (fn var => xformVar (tbl, var)) vars

   fun xformExp (tbl: Tbl.t, exp: A.Exp.t) : A.Exp.t =
      case exp of
         A.Exp.Exp {decls, var, ty} =>
            A.Exp.Exp {decls = xformDecls (tbl, decls),
                       var = xformVar (tbl, var),
                       ty = ty}
   and xformRHS (tbl: Tbl.t, rhs: A.RHS.t) : A.RHS.t =
      case rhs of
         A.RHS.R_Fn {lam} =>
            A.RHS.R_Fn {lam = xformLam (tbl, lam)}
       | A.RHS.R_Prim {prim, tyargs, args} =>
            A.RHS.R_Prim {prim = prim, tyargs = tyargs, args = xformVars (tbl, args)}
       | A.RHS.R_DaCon {dacon, tyargs, args} =>
            A.RHS.R_DaCon {dacon = dacon, tyargs = tyargs, args = xformVars (tbl, args)}
       | A.RHS.R_VApply {func, arg} =>
            A.RHS.R_VApply {func = xformVar (tbl, func), arg = xformVar (tbl, arg)}
       | A.RHS.R_TApply {func, tyarg} =>
            A.RHS.R_TApply {func = xformVar (tbl, func), tyarg = tyarg}
       | A.RHS.R_Var {var} =>
            A.RHS.R_Var {var = xformVar (tbl, var)}
       | A.RHS.R_Integer i =>
            A.RHS.R_Integer i
       | A.RHS.R_String s =>
            A.RHS.R_String s
       | A.RHS.R_Case {arg, matchrules} =>
            let
               val arg = xformVar (tbl, arg)
               val matchrules = List.map (fn mr => xformMatchRule (tbl, arg, mr)) matchrules
            in
               A.RHS.R_Case {arg = arg,
                             matchrules = matchrules}
            end
   and xformLam (tbl: Tbl.t, lam: A.Lam.t) : A.Lam.t =
      case lam of
         A.Lam.L_VLam {var, var_ty, body} =>
            A.Lam.L_VLam {var = var, var_ty = var_ty, body = xformExp (tbl, body)}
       | A.Lam.L_TLam {tyvar, body} =>
            A.Lam.L_TLam {tyvar = tyvar, body = xformExp (tbl, body)}
   and xformMatchRule (tbl: Tbl.t, arg: A.Var.t, matchrule: A.MatchRule.t) : A.MatchRule.t =
      case matchrule of
         A.MatchRule.MatchRule {pat, body} =>
            let
               val () =
                  case pat of
                     A.Pat.P_Var {var, ...} =>
                        Tbl.insertVar (tbl, var, arg)
                   | _ => ()
               val body = xformExp (tbl, body)
            in
               A.MatchRule.MatchRule {pat = pat, body = body}
            end
   and xformDecl (tbl: Tbl.t, decl: A.Decl.t) : A.Decl.t =
      case decl of
         A.Decl.D_Data {data_decls} =>
            A.Decl.D_Data {data_decls = data_decls}
       | A.Decl.D_Val {var, var_ty, rhs} =>
            let
               val rhs' = xformRHS (tbl, rhs)
               val () =
                  case rhs' of
                     A.RHS.R_Var {var = var'} => Tbl.insertVar (tbl, var, var')
                   | _ => ()
            in
               A.Decl.D_Val {var = var, var_ty = var_ty, rhs = rhs'}
            end
       | A.Decl.D_Fun {fun_decls} =>
            A.Decl.D_Fun {fun_decls = List.map (fn {func, func_ty, lam} =>
                                                {func = func,
                                                 func_ty = func_ty,
                                                 lam = xformLam (tbl, lam)})
                                               fun_decls}
   and xformDecls (tbl: Tbl.t, decls: A.Decl.t list) : A.Decl.t list =
      List.map (fn decl => xformDecl (tbl, decl)) decls
            
   fun xform (prog: A.Prog.t) : A.Prog.t =
      let
         val tbl = Tbl.new ()
         val A.Prog.Prog {decls, var, ty} = prog
      in
         A.Prog.Prog {decls = xformDecls (tbl, decls),
                      var = xformVar (tbl, var),
                      ty = ty}
      end
end

structure CopyPropEnv : ANF_IR_OPTIMIZATION =
struct
   structure A = AnfIR

   structure Env =
      struct
         structure VarEnv =
            struct
               type dom = A.Var.t
               type cod = A.Var.t
               type t = cod A.Var.Map.map
               val empty : t = A.Var.Map.empty
               val singleton : dom * cod -> t = A.Var.Map.singleton
               val lookup : t * dom -> cod option = A.Var.Map.find
               val extend : t * t -> t = A.Var.Map.unionWith #2
            end

         datatype t =
            Env of {varEnv: VarEnv.t}

         val empty =
            Env {varEnv = VarEnv.empty}

         fun fromVarEnv varEnv =
            Env {varEnv = varEnv}
         val singletonVar = fromVarEnv o VarEnv.singleton
         fun lookupVar (Env {varEnv, ...}, var) =
            VarEnv.lookup (varEnv, var)

         fun extend (Env {varEnv = varEnv1},
                     Env {varEnv = varEnv2}) =
            Env {varEnv = VarEnv.extend (varEnv1, varEnv2)}
      end

   fun xformVar (env: Env.t, var: A.Var.t) : A.Var.t =
      case Env.lookupVar (env, var) of
         NONE => var
       | SOME var' => var'
   fun xformVars (env: Env.t, vars: A.Var.t list) : A.Var.t list =
      List.map (fn var => xformVar (env, var)) vars

   fun xformExp (env: Env.t, exp: A.Exp.t) : A.Exp.t =
      case exp of
         A.Exp.Exp {decls, var, ty} =>
            let
               val (decls', env') = xformDecls (env, decls)
               val var' = xformVar (Env.extend (env, env'), var)
            in
               A.Exp.Exp {decls = decls',
                          var = var',
                          ty = ty}
            end
   and xformRHS (env: Env.t, rhs: A.RHS.t) : A.RHS.t =
      case rhs of
         A.RHS.R_Fn {lam} =>
            A.RHS.R_Fn {lam = xformLam (env, lam)}
       | A.RHS.R_Prim {prim, tyargs, args} =>
            A.RHS.R_Prim {prim = prim, tyargs = tyargs, args = xformVars (env, args)}
       | A.RHS.R_DaCon {dacon, tyargs, args} =>
            A.RHS.R_DaCon {dacon = dacon, tyargs = tyargs, args = xformVars (env, args)}
       | A.RHS.R_VApply {func, arg} =>
            A.RHS.R_VApply {func = xformVar (env, func), arg = xformVar (env, arg)}
       | A.RHS.R_TApply {func, tyarg} =>
            A.RHS.R_TApply {func = xformVar (env, func), tyarg = tyarg}
       | A.RHS.R_Var {var} =>
            A.RHS.R_Var {var = xformVar (env, var)}
       | A.RHS.R_Integer i =>
            A.RHS.R_Integer i
       | A.RHS.R_String s =>
            A.RHS.R_String s
       | A.RHS.R_Case {arg, matchrules} =>
            A.RHS.R_Case {arg = xformVar (env, arg),
                          matchrules = List.map (fn mr => xformMatchRule (env, arg, mr)) matchrules}
   and xformLam (env: Env.t, lam: A.Lam.t) : A.Lam.t =
      case lam of
         A.Lam.L_VLam {var, var_ty, body} =>
            A.Lam.L_VLam {var = var, var_ty = var_ty, body = xformExp (env, body)}
       | A.Lam.L_TLam {tyvar, body} =>
            A.Lam.L_TLam {tyvar = tyvar, body = xformExp (env, body)}
   and xformMatchRule (env: Env.t, arg: A.Var.t, matchrule: A.MatchRule.t) : A.MatchRule.t =
      case matchrule of
         A.MatchRule.MatchRule {pat, body} =>
            let
               val env' =
                  case pat of
                     A.Pat.P_Var {var, ...} =>
                        Env.singletonVar (var, arg)
                   | _ => Env.empty
               val body = xformExp (Env.extend (env, env'), body)
            in
               A.MatchRule.MatchRule {pat = pat, body = body}
            end
   and xformDecl (env: Env.t, decl: A.Decl.t) : A.Decl.t * Env.t =
      case decl of
         A.Decl.D_Data {data_decls} =>
            (A.Decl.D_Data {data_decls = data_decls},
             Env.empty)
       | A.Decl.D_Val {var, var_ty, rhs} =>
            let
               val rhs' = xformRHS (env, rhs)
               val env' =
                  case rhs' of
                     A.RHS.R_Var {var = var'} => Env.singletonVar (var, var')
                   | _ => Env.empty
            in
               (A.Decl.D_Val {var = var, var_ty = var_ty, rhs = rhs'},
                env')
            end
       | A.Decl.D_Fun {fun_decls} =>
            (A.Decl.D_Fun {fun_decls = List.map (fn {func, func_ty, lam} =>
                                                 {func = func,
                                                  func_ty = func_ty,
                                                  lam = xformLam (env, lam)})
                                                fun_decls},
             Env.empty)
   and xformDecls (env: Env.t, decls: A.Decl.t list) : A.Decl.t list * Env.t  =
      ListExtra.mapAndFoldl
      (fn (decl_i, env') =>
       let
          val (decl_i', env_i') = xformDecl (Env.extend (env, env'), decl_i)
       in
          (decl_i', Env.extend (env', env_i'))
       end)
      Env.empty
      decls
            
   fun xform (prog: A.Prog.t) : A.Prog.t =
      let
         val A.Prog.Prog {decls, var, ty} = prog
         val env0 = Env.empty
         val (decls', env') = xformDecls (env0, decls)
         val var' = xformVar (Env.extend (env0, env'), var)
      in
         A.Prog.Prog {decls = decls',
                      var = var',
                      ty = ty}
      end
end

structure CopyProp = CopyPropTbl
