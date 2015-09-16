(* langfc-src/anf-ir/inline.sml
 *
 * COPYRIGHT (c) 2015 Matthew Fluet (http://www.cs.rit.edu/~mtf)
 * All rights reserved.
 *
 * Rochester Institute of Technology
 * CSCI-742
 * S20135,S20145
 *
 * Simplify Case optimization for A-normal form intermediate
 * representation.
 *)

structure SimplifyCase : ANF_IR_OPTIMIZATION =
struct
   structure A = AnfIR

   structure Tbl =
      struct
         structure VarTbl =
            struct
               type dom = A.Var.t
               type cod = {dacon: A.DaCon.t, args: A.Var.t list} option ref
               type t = cod A.Var.Tbl.hash_table
               val lookup : t * dom -> cod option = fn (tbl, var) =>
                  A.Var.Tbl.find tbl var
               val insert : t * dom * cod -> unit = fn (tbl, var, dacon) =>
                  A.Var.Tbl.insert tbl (var, dacon)
               val new : unit -> t = fn () =>
                  A.Var.Tbl.mkTable (32, Fail "VarTbl")
            end

         datatype t =
            Tbl of {varTbl: VarTbl.t}

         val new = fn () =>
            Tbl {varTbl = VarTbl.new ()}

         fun lookupVar (Tbl {varTbl, ...}, var) =
            VarTbl.lookup (varTbl, var)
         fun insertVar (Tbl {varTbl, ...}, var, daconOptRef) =
            VarTbl.insert (varTbl, var, daconOptRef)
      end

   fun xformExp (tbl: Tbl.t, exp: A.Exp.t) : A.Exp.t =
      case exp of
         A.Exp.Exp {decls, var, ty} =>
            A.Exp.Exp {decls = xformDecls (tbl, decls),
                       var = var,
                       ty = ty}
   and xformLam (tbl: Tbl.t, lam: A.Lam.t) : A.Lam.t =
      case lam of
         A.Lam.L_VLam {var, var_ty, body} =>
            (Tbl.insertVar (tbl, var, ref NONE);
             A.Lam.L_VLam {var = var, var_ty = var_ty, body = xformExp (tbl, body)})
       | A.Lam.L_TLam {tyvar, body} =>
            A.Lam.L_TLam {tyvar = tyvar, body = xformExp (tbl, body)}
   and xformMatchRule (tbl: Tbl.t, arg : A.Var.t, matchrule: A.MatchRule.t) : A.MatchRule.t =
      case matchrule of
         A.MatchRule.MatchRule {pat, body} =>
            let
               val argDaConOptRef = valOf (Tbl.lookupVar (tbl, arg))
               val argDaConOpt = ! argDaConOptRef
               val () =
                  case pat of
                     A.Pat.P_DaCon {dacon, binds, ...} =>
                        argDaConOptRef :=
                        SOME {dacon = dacon,
                              args = List.map (fn {var, ...} =>
                                               (Tbl.insertVar (tbl, var, ref NONE); var))
                                              binds}
                   | A.Pat.P_Var {var, ...} =>
                        Tbl.insertVar (tbl, var, argDaConOptRef)
               val body = xformExp (tbl, body)
               val () = argDaConOptRef := argDaConOpt
            in
               A.MatchRule.MatchRule {pat = pat, body = body}
            end
   and xformDecl (tbl: Tbl.t, decl: A.Decl.t) : A.Decl.t list =
      case decl of
         A.Decl.D_Data {data_decls} =>
            [A.Decl.D_Data {data_decls = data_decls}]
       | A.Decl.D_Val {var, var_ty, rhs} =>
            let
               fun bind daconOptRef =
                  Tbl.insertVar (tbl, var, daconOptRef)
               fun keep rhs =
                  [A.Decl.D_Val {var = var, var_ty = var_ty, rhs = rhs}]
            in
               case rhs of
                  A.RHS.R_Fn {lam} =>
                     (bind (ref NONE); keep (A.RHS.R_Fn {lam = xformLam (tbl, lam)}))
                | A.RHS.R_Var {var = var'} =>
                     (bind (valOf (Tbl.lookupVar (tbl, var'))); keep rhs)
                | A.RHS.R_DaCon {dacon, tyargs, args} =>
                     (bind (ref (SOME {dacon = dacon, args = args})); keep rhs)
                | A.RHS.R_Case {arg, matchrules} =>
                     let
                        fun default () =
                           (bind (ref NONE);
                            keep (A.RHS.R_Case
                                  {arg = arg,
                                   matchrules = List.map (fn mr => xformMatchRule (tbl, arg, mr)) matchrules}))
                     in
                        case ! (valOf (Tbl.lookupVar (tbl, arg))) of
                           NONE => default ()
                         | SOME {dacon, args} =>
                              let
                                 fun chk (A.MatchRule.MatchRule {pat, ...}) =
                                    case pat of
                                       A.Pat.P_DaCon {dacon = dacon', ...} =>
                                          A.DaCon.equals (dacon, dacon')
                                     | A.Pat.P_Var _ =>
                                          true
                              in
                                 case List.find chk matchrules of
                                    SOME (A.MatchRule.MatchRule
                                          {pat = A.Pat.P_Var
                                                 {var = pat_var,
                                                  var_ty = pat_var_ty},
                                           body = A.Exp.Exp
                                                  {decls = body_decls,
                                                   var = body_var, ...}}) =>
                                       xformDecls
                                       (tbl,
                                        [A.Decl.D_Val
                                         {var = pat_var,
                                          var_ty = pat_var_ty,
                                          rhs = A.RHS.R_Var {var = arg}}] @
                                        body_decls @
                                        [A.Decl.D_Val
                                         {var = var,
                                          var_ty = var_ty,
                                          rhs = A.RHS.R_Var {var = body_var}}])
                                  | SOME (A.MatchRule.MatchRule
                                          {pat = A.Pat.P_DaCon
                                                 {binds, ...},
                                           body = A.Exp.Exp
                                                  {decls = body_decls,
                                                   var = body_var, ...}}) =>
                                       xformDecls
                                       (tbl,
                                        (ListPair.map
                                         (fn ({var = pat_var, var_ty = pat_var_ty},
                                              arg) =>
                                          A.Decl.D_Val
                                          {var = pat_var,
                                           var_ty = pat_var_ty,
                                           rhs = A.RHS.R_Var {var = arg}})
                                         (binds, args)) @
                                        body_decls @
                                        [A.Decl.D_Val
                                         {var = var,
                                          var_ty = var_ty,
                                          rhs = A.RHS.R_Var {var = body_var}}])
                                  | NONE => raise Fail "xformDecl:: List.find chk matchrules"
                            end
                     end
                 | _ => (bind (ref NONE); keep rhs)
            end
       | A.Decl.D_Fun {fun_decls} =>
            let
               val () =
                  List.app (fn {func, ...} =>
                            Tbl.insertVar (tbl, func, ref NONE))
                           fun_decls
               val fun_decls =
                  List.map (fn {func, func_ty, lam} =>
                            {func = func,
                             func_ty = func_ty,
                             lam = xformLam (tbl, lam)})
                           fun_decls
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
