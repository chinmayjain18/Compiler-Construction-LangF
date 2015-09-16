(* langfc-src/anf-ir/unused.sml
 *
 * COPYRIGHT (c) 2015 Matthew Fluet (http://www.cs.rit.edu/~mtf)
 * All rights reserved.
 *
 * Rochester Institute of Technology
 * CSCI-742
 * S20135,S20145
 *
 * A simple unused tycon and variable elimination optimization for
 * A-normal form intermediate representation.
 *)

structure Unused : ANF_IR_OPTIMIZATION =
struct
   structure A = AnfIR

   fun pureExp (exp: A.Exp.t) : bool =
      case exp of
         A.Exp.Exp {decls, ...} => List.all pureDecl decls
   and pureRHS (rhs: A.RHS.t) : bool =
      case rhs of
         A.RHS.R_Fn _ => true
       | A.RHS.R_Prim {prim, ...} =>
            (case prim of
                A.Prim.Div => true
              | A.Prim.Mod => true
              | A.Prim.Array => true
              | A.Prim.Len => true
              | A.Prim.Idx => true
              | A.Prim.Upd => false
              | A.Prim.Arg => true
              | A.Prim.Argc => true
              | A.Prim.Print => false
              | A.Prim.Fail => false
              | _ => true)
       | A.RHS.R_DaCon _ => true
       | A.RHS.R_VApply _ => false
       | A.RHS.R_TApply _ => false
       | A.RHS.R_Var _ => true
       | A.RHS.R_Integer _ => true
       | A.RHS.R_String _ => true
       | A.RHS.R_Case {arg, matchrules} =>
            List.all pureMatchRule matchrules
   and pureMatchRule (matchrule: A.MatchRule.t) : bool =
      case matchrule of
         A.MatchRule.MatchRule {body, ...} =>
            pureExp body
   and pureDecl (decl: A.Decl.t) : bool =
      case decl of
         A.Decl.D_Data _ => true
       | A.Decl.D_Val {rhs, ...} => pureRHS rhs
       | A.Decl.D_Fun _ => true

   fun xformExp (exp: A.Exp.t) : A.Exp.t =
      case exp of
         A.Exp.Exp {decls, var, ty} =>
            A.Exp.Exp {decls = xformDecls (decls, A.TyCon.Set.empty, A.Var.Set.singleton var),
                       var = var,
                       ty = ty}
   and xformRHS (rhs: A.RHS.t) : A.RHS.t =
      case rhs of
         A.RHS.R_Fn {lam} =>
            A.RHS.R_Fn {lam = xformLam lam}
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
                          matchrules = List.map xformMatchRule matchrules}
   and xformLam (lam: A.Lam.t) : A.Lam.t =
      case lam of
         A.Lam.L_VLam {var, var_ty, body} =>
            A.Lam.L_VLam {var = var,
                          var_ty = var_ty,
                          body = xformExp body}
       | A.Lam.L_TLam {tyvar, body} =>
            A.Lam.L_TLam {tyvar = tyvar,
                          body = xformExp body}
   and xformMatchRule (matchrule: A.MatchRule.t) : A.MatchRule.t =
      case matchrule of
         A.MatchRule.MatchRule {pat, body} =>
            A.MatchRule.MatchRule {pat = pat,
                                   body = xformExp body}
   and xformDecl (decl: A.Decl.t, usedTyCons: A.TyCon.Set.set, usedVars: A.Var.Set.set) : A.Decl.t option =
      case decl of
         A.Decl.D_Data {data_decls} =>
            let
               fun drop () = NONE
               fun keep () =
                  SOME (A.Decl.D_Data {data_decls = data_decls})
            in
               if List.exists (fn {tycon, ...} => A.TyCon.Set.member (usedTyCons, tycon)) data_decls
                  then keep ()
               else drop ()
            end
       | A.Decl.D_Val {var, var_ty, rhs} =>
            let
               fun drop () = NONE
               fun keep () =
                  (SOME o A.Decl.D_Val)
                  {var = var,
                   var_ty = var_ty,
                   rhs = xformRHS rhs}
            in
               if A.Var.Set.member (usedVars, var)
                  orelse not (pureRHS rhs)
                  then keep ()
               else drop ()
            end
       | A.Decl.D_Fun {fun_decls} =>
            let
               fun drop () = NONE
               fun keep () =
                  (SOME o A.Decl.D_Fun)
                  {fun_decls =
                   List.map (fn {func, func_ty, lam} =>
                             {func = func,
                              func_ty = func_ty,
                              lam = xformLam lam})
                   fun_decls}
            in
               if List.exists (fn {func, ...} => A.Var.Set.member (usedVars, func)) fun_decls
                  then keep ()
               else drop ()
            end
   and xformDecls (decls: A.Decl.t list, usedTyCons: A.TyCon.Set.set, usedVars: A.Var.Set.set) : A.Decl.t list =
      let
         val (decls, _, _) =
            List.foldr
            (fn (decl, (decls, usedTyCons, usedVars)) =>
             case xformDecl (decl, usedTyCons, usedVars) of
                NONE => (decls, usedTyCons, usedVars)
              | SOME decl => let
                                val (_, freeTyCons, _, freeVars) = A.Decl.freeIds decl
                             in 
                                (decl::decls,
                                 A.TyCon.Set.union (usedTyCons, freeTyCons),
                                 A.Var.Set.union (usedVars, freeVars))
                             end)
            ([], usedTyCons, usedVars)
            decls
      in
         decls
      end

   fun xform (prog: A.Prog.t) : A.Prog.t =
      let
         val A.Prog.Prog {decls, var, ty} = prog
      in
         A.Prog.Prog {decls = xformDecls (decls, A.TyCon.Set.empty, A.Var.Set.singleton var),
                      var = var,
                      ty = ty}
      end

end
