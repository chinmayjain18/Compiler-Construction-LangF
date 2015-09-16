(* langfc-src/core-ir/dead-code.sml
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
 * A simple dead-code elimination pass for core intermediate
 * representation.
 *)

structure DeadCode :> CORE_IR_OPTIMIZATION =
struct
   structure C = CoreIR

   fun pureExp (exp: C.Exp.t) : bool =
      case exp of
         C.Exp.Exp {node, ...} => pureExpNode node
   and pureExpNode (node: C.Exp.node) : bool =
      case node of
         C.Exp.E_Fn _ => true
       | C.Exp.E_Prim {prim, args, ...} =>
            (List.all pureExp args) andalso
            (case prim of
                C.Prim.Div => false
              | C.Prim.Mod => false
              | C.Prim.Array => false
              | C.Prim.Len => true
              | C.Prim.Idx => true
              | C.Prim.Upd => false
              | C.Prim.Print => false
              | C.Prim.Fail => false
              | _ => true)
       | C.Exp.E_DaCon {args, ...} =>
            List.all pureExp args
       | C.Exp.E_Apply _ => false
       | C.Exp.E_Var _ => true
       | C.Exp.E_Integer _ => true
       | C.Exp.E_String _ => true
       | C.Exp.E_Let {decl, body} =>
            (pureDecl decl) andalso (pureExp body)
       | C.Exp.E_Case {arg, matchrules} =>
            (pureExp arg) andalso (pureMatchRules matchrules)
   and pureMatchRule (matchrule: C.MatchRule.t) : bool =
      case matchrule of
         C.MatchRule.MatchRule {body, ...} =>
            pureExp body
   and pureMatchRules (matchrules: C.MatchRule.t list) : bool =
      List.all pureMatchRule matchrules
   and pureDecl (decl: C.Decl.t) : bool =
      case decl of
         C.Decl.D_Data _ => true
       | C.Decl.D_Val {exp, ...} => pureExp exp
       | C.Decl.D_Fun _ => true
            
   fun deadCode (prog : C.Prog.t) : C.Prog.t =
      let
         fun deadCodeExp (exp: C.Exp.t) : C.Exp.t =
            case exp of
               C.Exp.Exp {node, ty} =>
                  C.Exp.make (deadCodeExpNode node, ty)
         and deadCodeExpNode (node: C.Exp.node) : C.Exp.node =
            case node of
               C.Exp.E_Fn {lam} =>
                  C.Exp.E_Fn {lam = deadCodeLam lam}
             | C.Exp.E_Prim {prim, tyargs, args} =>
                  C.Exp.E_Prim {prim = prim,
                                tyargs = tyargs,
                                args = List.map deadCodeExp args}
             | C.Exp.E_DaCon {dacon, tyargs, args} =>
                  C.Exp.E_DaCon {dacon = dacon,
                                 tyargs = tyargs,
                                 args = List.map deadCodeExp args}
             | C.Exp.E_Apply {func, applyarg} =>
                  C.Exp.E_Apply {func = deadCodeExp func,
                                 applyarg = deadCodeApplyArg applyarg}
             | C.Exp.E_Var {var} =>
                  C.Exp.E_Var {var = var}
             | C.Exp.E_Integer i =>
                  C.Exp.E_Integer i
             | C.Exp.E_String s =>
                  C.Exp.E_String s
             | C.Exp.E_Let {decl, body} =>
                  let
                     val body = deadCodeExp body
                     val free_vars_body = C.Exp.freeVars body
                  in
                     case deadCodeDecl (free_vars_body, decl) of
                        NONE => C.Exp.node body
                      | SOME decl => C.Exp.E_Let {decl = decl,
                                                  body = body}
                  end
             | C.Exp.E_Case {arg, matchrules} =>
                  C.Exp.E_Case {arg = deadCodeExp arg,
                                matchrules = deadCodeMatchRules matchrules}
         and deadCodeLam (lam: C.Lam.t) : C.Lam.t =
            case lam of
               C.Lam.Lam {param, body} =>
                  C.Lam.Lam {param = param,
                             body = deadCodeExp body}
         and deadCodeApplyArg (applyarg: C.ApplyArg.t) : C.ApplyArg.t =
            case applyarg of
               C.ApplyArg.A_Exp exp =>
                  C.ApplyArg.A_Exp (deadCodeExp exp)
             | C.ApplyArg.A_Type ty =>
                  C.ApplyArg.A_Type ty
         and deadCodeMatchRule (matchrule: C.MatchRule.t) : C.MatchRule.t =
            case matchrule of
               C.MatchRule.MatchRule {pat, body} =>
                  C.MatchRule.MatchRule {pat = pat,
                                         body = deadCodeExp body}
         and deadCodeMatchRules (matchrules: C.MatchRule.t list) : C.MatchRule.t list =
            List.map deadCodeMatchRule matchrules
         and deadCodeDecl (fvs: C.Var.Set.set, decl: C.Decl.t) : C.Decl.t option =
            case decl of
               C.Decl.D_Data {data_decls} =>
                  SOME (C.Decl.D_Data {data_decls = data_decls})
             | C.Decl.D_Val {var, var_ty, exp} =>
                  let
                     fun drop () = NONE
                     fun keep () =
                        (SOME o C.Decl.D_Val)
                        {var = var,
                         var_ty = var_ty,
                         exp = deadCodeExp exp}
                  in
                     if C.Var.Set.member (fvs, var)
                        orelse not (pureExp exp)
                        then keep ()
                     else drop ()
                  end
             | C.Decl.D_Fun {fun_decls} =>
                  let
                     fun drop () = NONE
                     fun keep () =
                        (SOME o C.Decl.D_Fun)
                        {fun_decls =
                         List.map (fn {func, func_ty, lam} =>
                                   {func = func,
                                    func_ty = func_ty,
                                    lam = deadCodeLam lam})
                                  fun_decls}
                  in
                     keep ()
                  end
         fun deadCodeDecls (fvs: C.Var.Set.set, decls: C.Decl.t list) : C.Decl.t list =
            let 
               val (_, decls) =
                  List.foldr
                  (fn (decl, (fvs, decls)) =>
                   case deadCodeDecl (fvs, decl) of
                      NONE => (fvs, decls)
                    | SOME decl => (C.Var.Set.union (fvs, C.Decl.freeVars decl),
                                    decl::decls))
                  (fvs, [])
                  decls
            in
               decls
            end

         fun deadCodeProg (prog: C.Prog.t) : C.Prog.t =
            let
               val C.Prog.Prog {decls, exp} = prog
               val exp = deadCodeExp exp
               val decls = deadCodeDecls (C.Exp.freeVars exp, decls)
            in
               C.Prog.Prog {decls = decls, exp = exp}
            end
         val prog = deadCodeProg prog
      in
         prog
      end

   val xform = deadCode
end
