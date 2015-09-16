(* langfc-src/anf-ir/flatten-fail.sml
 *
 * COPYRIGHT (c) 2015 Matthew Fluet (http://www.cs.rit.edu/~mtf)
 * All rights reserved.
 *
 * Rochester Institute of Technology
 * CSCI-742
 * S20135,S20145
 *
 * "Flatten Fail" optimization for A-normal form intermediate
 * representation.
 *
 *
 * Transform
 *
 *   let
 *     declsA
 *     val x : tyx = case a of
 *                       pat1 => let
 *                                 decls1
 *                               in x1 : tyx  end
 *                     | pat2  => let
 *                                  decls21
 *                                  val f2 : tyf2 = fail [tyf2] m2
 *                                  decls22
 *                                in x2 : tyx  end
 *                     ...
 *                     | patN => let
 *                                 declsN1
 *                                 val fN : tyfN = fail [tyfN] mN
 *                                 declsN2
 *                               in yN : tyx  end
 *                   end
 *     declsB
 *   in r : tyr end
 *
 * to
 *
 *   let
 *     declsA
 *     val s : tyr = case a of
 *                       pat1 => let
 *                                 decls1
 *                                 val x : tyx = x1
 *                                 declsB
 *                               in r : tyr  end
 *                     | pat2 => let
 *                                 decls21
 *                                 val f2 : tyr = fail [tyr] m2
 *                               in f2 : tyr  end
 *                     ...
 *                     | patN => let
 *                                 declsN1
 *                                 val fN : tyr = fail [tyr] mN
 *                               in fN : tyr  end
 *                   end
 *   in s : tyr end
 *
 * This transformation exposes 'a |-> pat1' and decls1 within declsB.
 * It is especially useful for exposing the bounds checking of "a ! i"
 * and "a ! i := v".
 *
 *)

structure FlattenFail : ANF_IR_OPTIMIZATION =
struct
   structure A = AnfIR

   fun xform1Exp exp =
      case exp of
         A.Exp.Exp {decls, var, ty} =>
            let
               val (decls, var) = xform1Decls (decls, var, ty)
            in
               A.Exp.Exp {decls = decls,
                          var = var,
                          ty = ty}
            end
   and xform1RHS rhs =
      case rhs of
         A.RHS.R_Fn {lam} =>
            A.RHS.R_Fn {lam = xform1Lam lam}
       | A.RHS.R_Case {arg, matchrules} =>
            A.RHS.R_Case {arg = arg, matchrules = List.map xform1MatchRule matchrules}
       | _ => rhs
   and xform1Lam lam =
      case lam of
         A.Lam.L_VLam {var, var_ty, body} =>
            A.Lam.L_VLam {var = var, var_ty = var_ty, body = xform1Exp body}
       | A.Lam.L_TLam {tyvar, body} =>
            A.Lam.L_TLam {tyvar = tyvar, body = xform1Exp body}
   and xform1MatchRule matchrule =
      case matchrule of
         A.MatchRule.MatchRule {pat, body} =>
            A.MatchRule.MatchRule {pat = pat, body = xform1Exp body}
   and xform1Decl decl =
      case decl of
         A.Decl.D_Data {data_decls} =>
            A.Decl.D_Data {data_decls = data_decls}
       | A.Decl.D_Val {var, var_ty, rhs} =>
            A.Decl.D_Val {var = var, var_ty = var_ty, rhs = xform1RHS rhs}
       | A.Decl.D_Fun {fun_decls} =>
            A.Decl.D_Fun {fun_decls = List.map (fn {func, func_ty, lam} =>
                                                {func = func,
                                                 func_ty = func_ty,
                                                 lam = xform1Lam lam})
                                               fun_decls}
   and xform1Decls (decls, res, res_ty) =
      List.foldr
      (fn (decl, (decls, res)) =>
       let
          val decl = xform1Decl decl
          fun keep () = (decl::decls, res)
       in
          case decl of
             A.Decl.D_Val {var, var_ty, rhs} =>
                (case rhs of
                    A.RHS.R_Prim {prim = A.Prim.Fail, tyargs, args} =>
                       let
                          val rhs =
                             A.RHS.R_Prim {prim = A.Prim.Fail, tyargs = [res_ty], args = args}
                       in
                          ([A.Decl.D_Val {var = var, var_ty = res_ty, rhs = rhs}], var)
                       end
                  | _ => keep ())
           | _ => keep ()
       end)
      ([],res)
      decls

   fun xform2Exp exp: A.Exp.t =
      case exp of
         A.Exp.Exp {decls, var, ty} =>
            let
               val (decls, var) = xform2Decls (decls, var, ty)
            in
               A.Exp.Exp {decls = decls,
                          var = var,
                          ty = ty}
            end
   and xform2RHS rhs =
      case rhs of
         A.RHS.R_Fn {lam} =>
            A.RHS.R_Fn {lam = xform2Lam lam}
       | A.RHS.R_Case {arg, matchrules} =>
            A.RHS.R_Case {arg = arg, matchrules = List.map xform2MatchRule matchrules}
       | _ => rhs
   and xform2Lam lam =
      case lam of
         A.Lam.L_VLam {var, var_ty, body} =>
            A.Lam.L_VLam {var = var, var_ty = var_ty, body = xform2Exp body}
       | A.Lam.L_TLam {tyvar, body} =>
            A.Lam.L_TLam {tyvar = tyvar, body = xform2Exp body}
   and xform2MatchRule matchrule =
      case matchrule of
         A.MatchRule.MatchRule {pat, body} =>
            A.MatchRule.MatchRule {pat = pat, body = xform2Exp body}
   and xform2Decls (decls, res, res_ty) =
      xform2DeclsAux (List.rev decls, [], res, res_ty)
   and xform2DeclsAux (rev_decls, acc_decls, res, res_ty) =
      case rev_decls of
         [] => (acc_decls, res)
       | decl::rev_decls =>
            let
               val keep = fn decl =>
                  xform2DeclsAux (rev_decls, decl::acc_decls, res, res_ty)
            in
               case decl of
                  A.Decl.D_Data {data_decls} =>
                     keep (A.Decl.D_Data {data_decls = data_decls})
                | A.Decl.D_Val {var, var_ty, rhs} =>
                     let
                        val keep = fn rhs =>
                           keep (A.Decl.D_Val {var = var, var_ty = var_ty, rhs = xform2RHS rhs})
                     in
                        case rhs of
                           A.RHS.R_Case {arg, matchrules} =>
                              let
                                 val (matchrules, nonFailMatchRules) =
                                    ListExtra.mapAndFoldl
                                    (fn (A.MatchRule.MatchRule
                                         {pat,
                                          body = A.Exp.Exp
                                                 {decls = body_decls,
                                                  var = body_var,
                                                  ty = body_ty}},
                                         nonFailMatchRules) =>
                                     case List.rev body_decls of
                                        (A.Decl.D_Val
                                         {var = fail_var,
                                          var_ty = fail_var_ty,
                                          rhs = A.RHS.R_Prim
                                          {prim = A.Prim.Fail,
                                           tyargs = fail_tyargs,
                                           args = fail_args}})
                                        ::
                                        body_decls =>
                                           (fn () =>
                                            let
                                               val fail_rhs =
                                                  A.RHS.R_Prim
                                                  {prim = A.Prim.Fail,
                                                   tyargs = [res_ty],
                                                   args = fail_args}
                                               val fail_decl =
                                                  A.Decl.D_Val
                                                  {var = fail_var,
                                                   var_ty = res_ty,
                                                   rhs = fail_rhs}
                                            in
                                               A.MatchRule.MatchRule
                                               {pat = pat,
                                                body = A.Exp.Exp
                                                       {decls = List.rev (fail_decl::body_decls),
                                                        var = fail_var,
                                                        ty = res_ty}}
                                            end,
                                            nonFailMatchRules)
                                      | _ =>
                                           (fn () =>
                                            let
                                               val copy_decl =
                                                  A.Decl.D_Val
                                                  {var = var,
                                                   var_ty = var_ty,
                                                   rhs = A.RHS.R_Var {var = body_var}}
                                               val (decls, res) =
                                                  xform2DeclsAux (List.rev body_decls,
                                                                  copy_decl::acc_decls,
                                                                  res,
                                                                  res_ty)
                                            in
                                               A.MatchRule.MatchRule
                                               {pat = pat,
                                                body = A.Exp.Exp
                                                       {decls = decls,
                                                        var = res,
                                                        ty = res_ty}}
                                            end,
                                         nonFailMatchRules + 1))
                                    0
                                    matchrules
                              in
                                 case nonFailMatchRules of
                                    1 => let
                                            val var = A.Var.new (A.Var.name var)
                                            val matchrules = List.map (fn mk => mk ()) matchrules
                                            val rhs =
                                               A.RHS.R_Case
                                               {arg = arg,
                                                matchrules = matchrules}
                                         in
                                            xform2DeclsAux (rev_decls,
                                                            [A.Decl.D_Val
                                                             {var = var,
                                                              var_ty = res_ty,
                                                              rhs = rhs}],
                                                            var,
                                                            res_ty)
                                         end
                                  | _ => keep rhs
                              end
                         | rhs => keep rhs
                     end
                | A.Decl.D_Fun {fun_decls} =>
                     keep (A.Decl.D_Fun {fun_decls = List.map (fn {func, func_ty, lam} =>
                                                               {func = func,
                                                                func_ty = func_ty,
                                                                lam = xform2Lam lam})
                                                              fun_decls})
            end

   fun xform (prog: A.Prog.t) : A.Prog.t =
      let
         val A.Prog.Prog {decls, var, ty} = prog
         val (decls, var) = xform1Decls (decls, var, ty)
         val (decls, var) = xform2Decls (decls, var, ty)
      in
         A.Prog.Prog {decls = decls,
                      var = var,
                      ty = ty}
      end
end
