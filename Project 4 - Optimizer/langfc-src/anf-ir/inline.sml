(* langfc-src/anf-ir/inline.sml
 *
 * COPYRIGHT (c) 2015 Matthew Fluet (http://www.cs.rit.edu/~mtf)
 * All rights reserved.
 *
 * Rochester Institute of Technology
 * CSCI-742
 * S20135,S20145
 *
 * Inlining optimization for A-normal form intermediate
 * representation.
 *)

structure Inline : ANF_IR_OPTIMIZATION =
struct
   structure A = AnfIR

   val inlineThreshCtl : int Controls.control =
      Controls.genControl
      {name = "anf-inline-thresh",
       pri = [],
       obscurity = 0,
       help = "anf inlining optimization threshold",
       default = 48}
   val () =
      ControlRegistry.register Control.topRegistry
      {ctl = Controls.stringControl ControlUtil.Cvt.int inlineThreshCtl,
       envName = NONE}

   structure Tbl =
      struct
         structure VarTbl =
            struct
               type dom = A.Var.t
               type cod = A.Lam.t
               type t = cod A.Var.Tbl.hash_table
               val lookup : t * dom -> cod option = fn (tbl, var) =>
                  A.Var.Tbl.find tbl var
               val insert : t * dom * cod -> unit = fn (tbl, var, lam) =>
                  A.Var.Tbl.insert tbl (var, lam)
               val new : unit -> t = fn () =>
                  A.Var.Tbl.mkTable (32, Fail "VarTbl")
            end

         datatype t =
            Tbl of {varTbl: VarTbl.t}

         val new = fn () =>
            Tbl {varTbl = VarTbl.new ()}

         fun lookupVar (Tbl {varTbl, ...}, var) =
            VarTbl.lookup (varTbl, var)
         fun insertVar (Tbl {varTbl, ...}, var, lam) =
            VarTbl.insert (varTbl, var, lam)
      end

   fun xformExp (tbl: Tbl.t, exp: A.Exp.t) : A.Exp.t =
      case exp of
         A.Exp.Exp {decls, var, ty} =>
            A.Exp.Exp {decls = xformDecls (tbl, decls),
                       var = var,
                       ty = ty}
   and xformRHS (tbl: Tbl.t, rhs: A.RHS.t) : A.RHS.t =
      case rhs of
         A.RHS.R_Fn {lam} =>
            A.RHS.R_Fn {lam = xformLam (tbl, lam)}
       | A.RHS.R_Prim {prim, tyargs, args} =>
            A.RHS.R_Prim {prim = prim, tyargs = tyargs, args = args}
       | A.RHS.R_DaCon {dacon, tyargs, args} =>
            A.RHS.R_DaCon {dacon = dacon, tyargs = tyargs, args = args}
       | A.RHS.R_VApply {func, arg} =>
            A.RHS.R_VApply {func = func, arg = arg}
       | A.RHS.R_TApply {func, tyarg} =>
            A.RHS.R_TApply {func = func, tyarg = tyarg}
       | A.RHS.R_Var {var} =>
            A.RHS.R_Var {var = var}
       | A.RHS.R_Integer i =>
            A.RHS.R_Integer i
       | A.RHS.R_String s =>
            A.RHS.R_String s
       | A.RHS.R_Case {arg, matchrules} =>
            A.RHS.R_Case {arg = arg,
                          matchrules = List.map (fn mr => xformMatchRule (tbl, mr)) matchrules}
   and xformLam (tbl: Tbl.t, lam: A.Lam.t) : A.Lam.t =
      case lam of
         A.Lam.L_VLam {var, var_ty, body} =>
            A.Lam.L_VLam {var = var, var_ty = var_ty, body = xformExp (tbl, body)}
       | A.Lam.L_TLam {tyvar, body} =>
            A.Lam.L_TLam {tyvar = tyvar, body = xformExp (tbl, body)}
   and xformMatchRule (tbl: Tbl.t, matchrule: A.MatchRule.t) : A.MatchRule.t =
      case matchrule of
         A.MatchRule.MatchRule {pat, body} =>
            A.MatchRule.MatchRule {pat = pat, body = xformExp (tbl, body)}
   and xformDecl (tbl: Tbl.t, decl: A.Decl.t) : A.Decl.t list =
      case decl of
         A.Decl.D_Data {data_decls} =>
            [A.Decl.D_Data {data_decls = data_decls}]
       | A.Decl.D_Val {var, var_ty, rhs} =>
            let
               val rhs' = xformRHS (tbl, rhs)
               fun keep () =
                  [A.Decl.D_Val {var = var, var_ty = var_ty, rhs = rhs'}]
            in
               case rhs' of
                  A.RHS.R_Var {var = var'} =>
                     let
                        val () =
                           case Tbl.lookupVar (tbl, var') of
                              NONE => ()
                            | SOME lam' => Tbl.insertVar (tbl, var, lam')
                     in
                        keep ()
                     end
                | A.RHS.R_Fn {lam} =>
                     let
                        val () =
                           if A.Lam.size lam <= Controls.get inlineThreshCtl
                              then Tbl.insertVar (tbl, var, lam)
                           else ()
                     in
                        keep ()
                     end
                | A.RHS.R_VApply {func, arg} =>
                     (case Option.map A.Lam.clone (Tbl.lookupVar (tbl, func)) of
                         SOME (A.Lam.L_VLam {var = formal_arg,
                                             var_ty = formal_arg_ty,
                                             body = A.Exp.Exp {decls = body_decls,
                                                               var = body_var,
                                                               ty = body_ty}}) =>
                            let
                               val decls' =
                                  [A.Decl.D_Val {var = formal_arg,
                                                 var_ty = formal_arg_ty,
                                                 rhs = A.RHS.R_Var {var = arg}}] @
                                  body_decls @
                                  [A.Decl.D_Val {var = var,
                                                 var_ty = var_ty,
                                                 rhs = A.RHS.R_Var {var = body_var}}]
                            in
                               xformDecls (tbl, decls')
                            end
                       | _ => keep ())
                | A.RHS.R_TApply {func, tyarg} =>
                     (case Option.map A.Lam.clone (Tbl.lookupVar (tbl, func)) of
                         SOME (A.Lam.L_TLam {tyvar = formal_tyarg,
                                             body = A.Exp.Exp {decls = body_decls,
                                                               var = body_var,
                                                               ty = body_ty}}) =>
                            let
                               val decls' =
                                  (AnfIR.Decl.tySubstL (body_decls, (tyarg, formal_tyarg))) @
                                  [A.Decl.D_Val {var = var,
                                                 var_ty = var_ty,
                                                 rhs = A.RHS.R_Var {var = body_var}}]
                            in
                               xformDecls (tbl, decls')
                            end
                       | _ => keep ())
                 | _ => keep ()
            end
       | A.Decl.D_Fun {fun_decls} =>
            let
               val fun_decls =
                  List.map (fn {func, func_ty, lam} =>
                            {func = func,
                             func_ty = func_ty,
                             lam = xformLam (tbl, lam)})
                           fun_decls
               val () =
                  case fun_decls of
                     [{func, func_ty, lam}] =>
                        if A.Var.Set.member (A.Lam.freeVars lam, func)
                           then ()
                        else if A.Lam.size lam <= Controls.get inlineThreshCtl
                           then Tbl.insertVar (tbl, func, lam)
                        else ()
                   | _ => ()
            in
               [A.Decl.D_Fun {fun_decls = fun_decls}]
            end

   and xformDecls (tbl: Tbl.t, decls: A.Decl.t list) : A.Decl.t list =
      List.concat (List.map (fn decl => xformDecl (tbl, decl)) decls)

   fun xform (prog: A.Prog.t) : A.Prog.t =
      let
         val tbl = Tbl.new ()
         val A.Prog.Prog {decls, var, ty} = prog
      in
         A.Prog.Prog {decls = xformDecls (tbl, decls),
                      var = var,
                      ty = ty}
      end
end
