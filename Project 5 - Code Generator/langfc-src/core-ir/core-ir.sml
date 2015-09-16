(* langfc-src/core-ir/core-ir.sml
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
 * Core intermediate representation in the LangF compiler (langfc).
 *)

structure CoreIR :> CORE_IR =
struct

   structure Layout =
      struct
         open Layout
         fun prefixSpaceIfNonEmpty t =
            if isEmpty t then t else seq [space, t]
         fun optSeq (start, finish, sep) lay xs =
            if List.null xs
               then empty
            else seq [str start,
                      (mayAlign o separateRight)
                      (List.map lay xs, sep),
                      str finish]
      end

   structure TyVar = Id (val defaultName = "'a")

   structure TyCon = Id (val defaultName = "T")
   structure TyCon =
      struct
         open TyCon

         (* Pre-defined type constructors;
          * use 'newSpecial' to omit uniquifying suffix,
          * so that a pretty-printed CoreIR can be
          * scanned/parsed/type-checked as a LangF program.
          *)
         val unit = newSpecial "Unit"
         val bool = newSpecial "Bool"
         val integer = newSpecial "Integer"
         val string = newSpecial "String"
         val array = newSpecial "Array"
      end
   structure DaCon = Id (val defaultName = "D")
   structure DaCon =
      struct
         open DaCon

         (* Pre-defined data constructors;
          * use 'newSpecial' to omit uniquifying suffix,
          * so that a pretty-printed CoreIR can be
          * scanned/parsed/type-checked as a LangF program.
          *)
         val unit = newSpecial "Unit"
         val truee = newSpecial "True"
         val falsee = newSpecial "False"
      end

   structure Var = Id (val defaultName = "x")

   structure Type =
      struct
         datatype t =
            T_TyFn of {tyvar: TyVar.t, res: t}
          | T_Fn of {arg: t, res: t}
          | T_TyCon of {tycon: TyCon.t, tyargs: t list}
          | T_TyVar of {tyvar: TyVar.t}

         fun layout ty = layoutT ty
         and layoutT ty = layoutAux true ty
         and layoutF ty = layoutAux false ty
         and layoutAux isDelimited ty =
            let
               fun delimit t = if isDelimited then t else Layout.paren t
            in
               case ty of
                  T_TyFn {tyvar, res} =>
                     (delimit o Layout.mayAlign)
                     [Layout.seq [Layout.str "[",
                                  TyVar.layout tyvar,
                                  Layout.str "]",
                                  Layout.space,
                                  Layout.str "->"],
                      layoutF res]
                | T_Fn {arg, res} =>
                     (delimit o Layout.mayAlign)
                     [layoutF arg,
                      Layout.str "->",
                      layoutF res]
                | T_TyCon {tycon, tyargs} =>
                     if List.null tyargs
                        then TyCon.layout tycon
                     else
                        (delimit o Layout.seq)
                        [TyCon.layout tycon,
                         Layout.prefixSpaceIfNonEmpty (layoutArgs tyargs)]
                | T_TyVar {tyvar} => TyVar.layout tyvar
            end
         and layoutArgs tys =
            Layout.optSeq ("[", "]", ",") layoutT tys

         val bool = T_TyCon {tycon = TyCon.bool, tyargs = []}
         val unit = T_TyCon {tycon = TyCon.unit, tyargs = []}
         val integer = T_TyCon {tycon = TyCon.integer, tyargs = []}
         val string = T_TyCon {tycon = TyCon.string, tyargs = []}
         fun array ty = T_TyCon {tycon = TyCon.array, tyargs = [ty]}

         fun equals (ty1, ty2) =
            let
               fun loop tyVarEquiv (ty1, ty2) =
                  case (ty1, ty2) of
                     (T_TyFn {tyvar = tyvar1, res = res1},
                      T_TyFn {tyvar = tyvar2, res = res2}) =>
                        loop (fn (tyvar1', tyvar2') =>
                              case (TyVar.equals (tyvar1', tyvar1),
                                    TyVar.equals (tyvar2', tyvar2)) of
                                 (true, true) => true
                               | (false, false) => tyVarEquiv (tyvar1', tyvar2')
                               | _ => false)
                             (res1, res2)
                   | (T_Fn {arg = arg1, res = res1},
                      T_Fn {arg = arg2, res = res2}) =>
                        loop tyVarEquiv (arg1, arg2) andalso
                        loop tyVarEquiv (res1, res2)
                   | (T_TyCon {tycon = tycon1, tyargs = tyargs1},
                      T_TyCon {tycon = tycon2, tyargs = tyargs2}) =>
                        TyCon.equals (tycon1, tycon2) andalso
                        ListPair.allEq (loop tyVarEquiv) (tyargs1, tyargs2)
                   | (T_TyVar {tyvar = tyvar1},
                      T_TyVar {tyvar = tyvar2}) =>
                        tyVarEquiv (tyvar1, tyvar2)
                   | _ => false
            in
               loop TyVar.equals (ty1, ty2)
            end

         local
            fun singletonTyVar tyvar = (TyVar.Set.singleton tyvar, TyCon.Set.empty)
            fun singletonTyCon tycon = (TyVar.Set.empty, TyCon.Set.singleton tycon)
            fun union ((tyvar_set1, tycon_set1), (tyvar_set2, tycon_set2)) =
               (TyVar.Set.union (tyvar_set1, tyvar_set2),
                TyCon.Set.union (tycon_set1, tycon_set2))
            fun difference ((tyvar_set1, tycon_set1), (tyvar_set2, tycon_set2)) =
               (TyVar.Set.difference (tyvar_set1, tyvar_set2),
                TyCon.Set.difference (tycon_set1, tycon_set2))
         in
            fun freeIds ty =
               case ty of
                  T_TyFn {tyvar, res} =>
                     difference (freeIds res,
                                 singletonTyVar tyvar)
             | T_Fn {arg, res} =>
                     union (freeIds arg, freeIds res)
             | T_TyCon {tycon, tyargs, ...} =>
                  List.foldl (fn (ty,free_ids) =>
                              union (freeIds ty, free_ids))
                             (singletonTyCon tycon)
                             tyargs
             | T_TyVar {tyvar} => singletonTyVar tyvar
            val freeTyVars = #1 o freeIds
            val freeTyCons = #2 o freeIds
         end
         fun subst (ty, (actual, formal)) =
            let
               fun sub ty = subst (ty, (actual, formal))
            in
               case ty of
                  T_TyFn {tyvar, res} =>
                     if TyVar.equals (tyvar, formal)
                        then T_TyFn {tyvar = tyvar, res = res}
                     else if TyVar.Set.member (freeTyVars actual, tyvar)
                        then let
                                val fresh_tyvar =
                                   TyVar.new (TyVar.name tyvar)
                                val fresh_res =
                                   subst (res, (T_TyVar {tyvar = fresh_tyvar}, tyvar))
                             in
                                T_TyFn {tyvar = fresh_tyvar,
                                        res = sub fresh_res}
                             end
                     else T_TyFn {tyvar = tyvar, res = sub res}
                | T_Fn {arg, res} =>
                     T_Fn {arg = sub arg,
                           res = sub res}
                | T_TyCon {tycon, tyargs} =>
                      T_TyCon {tycon = tycon,
                               tyargs = substL (tyargs, (actual, formal))}
                | T_TyVar {tyvar} =>
                      if TyVar.equals (tyvar, formal)
                         then actual
                      else T_TyVar {tyvar = tyvar}
            end
         and substL (tys, (actual, formal)) =
            List.map (fn ty => subst (ty, (actual, formal))) tys
         fun substs (ty, actuals_formals) =
            List.foldl (fn ((actual, formal), ty) => subst (ty, (actual, formal))) ty actuals_formals
         and substsL (tys, actuals_formals) =
            List.map (fn ty => substs (ty, actuals_formals)) tys

         fun alphaRename ty =
            case ty of
               T_TyFn {tyvar, res} =>
                  let
                     val fresh_tyvar = TyVar.new (TyVar.name tyvar)
                     val res = alphaRename res
                     val fresh_res = subst (res, (T_TyVar {tyvar = fresh_tyvar}, tyvar))
                  in
                     T_TyFn {tyvar = fresh_tyvar,
                             res = fresh_res}
                  end
             | T_Fn {arg, res} =>
                  T_Fn {arg = alphaRename arg,
                        res = alphaRename res}
             | T_TyCon {tycon, tyargs} =>
                  T_TyCon {tycon = tycon,
                           tyargs = List.map alphaRename tyargs}
             | T_TyVar {tyvar} => T_TyVar {tyvar = tyvar}
      end

   structure Param =
      struct
         datatype t =
            P_Var of {var: Var.t, var_ty: Type.t}
          | P_TyVar of {tyvar: TyVar.t}

         fun layout param =
            case param of
               P_Var {var, var_ty} =>
                  Layout.seq [Layout.str "(",
                              Var.layout var,
                              Layout.space,
                              Layout.str ":",
                              Layout.space,
                              Type.layout var_ty,
                              Layout.str ")"]
             | P_TyVar {tyvar} =>
                  Layout.seq [Layout.str "[",
                              TyVar.layout tyvar,
                              Layout.str "]"]
      end

   structure Pat =
      struct
         datatype t =
            P_DaCon of {dacon: DaCon.t,
                        tyargs: Type.t list,
                        binds: {var: Var.t, var_ty: Type.t} list}
          | P_Var of {var: Var.t, var_ty: Type.t}

         fun layout pat =
            case pat of
               P_DaCon {dacon, tyargs, binds} =>
                  Layout.seq [DaCon.layout dacon,
                              Layout.prefixSpaceIfNonEmpty (Type.layoutArgs tyargs),
                              Layout.prefixSpaceIfNonEmpty (layoutBinds binds)]
             | P_Var {var, var_ty} => layoutBind {var = var, var_ty = var_ty}
         and layoutBinds binds =
            Layout.optSeq ("{", "}", ",") layoutBind binds
         and layoutBind {var, var_ty} =
            Layout.seq [Var.layout var,
                        Layout.space,
                        Layout.str "(* ",
                        Layout.str ":",
                        Layout.space,
                        Type.layout var_ty,
                        Layout.str " *)"]
      end

   structure Prim =
      struct
         datatype t =
            Add
          | Arg
          | Argc
          | Array
          | Concat
          | Div
          | Eq
          | Fail
          | Gt
          | Gte
          | Idx
          | Len
          | Lt
          | Lte
          | Mod
          | Mul
          | NEq
          | Neg
          | Print
          | Size
          | Sub
          | Subscript
          | Upd

         fun layout p =
            case p of
               Add => Layout.str "@Add"
             | Arg => Layout.str "@Arg"
             | Argc => Layout.str "@Argc"
             | Array => Layout.str "@Array"
             | Concat => Layout.str "@Concat"
             | Div => Layout.str "@Div"
             | Eq => Layout.str "@Eq"
             | Fail => Layout.str "@Fail"
             | Gt => Layout.str "@Gt"
             | Gte => Layout.str "@Gte"
             | Lt => Layout.str "@Lt"
             | Lte => Layout.str "@Lte"
             | Mod => Layout.str "@Mod"
             | Mul => Layout.str "@Mul"
             | NEq => Layout.str "@NEq"
             | Neg => Layout.str "@Neg"
             | Print => Layout.str "@Print"
             | Size => Layout.str "@Size"
             | Sub => Layout.str "@Sub"
             | Subscript => Layout.str "@Subscript"
             | Upd => Layout.str "@Upd"
             | Idx => Layout.str "@Idx"
             | Len => Layout.str "@Len"
      end

   structure Exp_Lam_ApplyArg_MatchRule_Decl =
      struct
         datatype exp = Exp of {node: exp_node, ty: Type.t}
         and exp_node =
            E_Fn of {lam: lam}
          | E_Prim of {prim: Prim.t, tyargs: Type.t list, args: exp list}
          | E_DaCon of {dacon: DaCon.t, tyargs: Type.t list, args: exp list}
          | E_Apply of {func: exp, applyarg: applyarg}
          | E_Var of {var: Var.t}
          | E_Integer of IntInf.int
          | E_String of String.string
          | E_Let of {decl: decl, body: exp}
          | E_Case of {arg: exp, matchrules: matchrule list}

         and lam = Lam of {param: Param.t, body: exp}

         and applyarg =
            A_Exp of exp
          | A_Type of Type.t

         and matchrule = MatchRule of {pat: Pat.t, body: exp}

         and decl =
            D_Data of {data_decls: {tycon: TyCon.t,
                                    tyvars: TyVar.t list,
                                    dacon_decls: {dacon: DaCon.t,
                                                  arg_tys: Type.t list} list} list}
          | D_Val of {var: Var.t, var_ty: Type.t, exp: exp}
          | D_Fun of {fun_decls: {func: Var.t, func_ty: Type.t, lam: lam} list}

         fun layoutExp e = layoutExpT e
         and layoutExpT e = layoutExpAux true e
         and layoutExpF e = layoutExpAux false e
         and layoutExpAux isDelimited e =
            let
               fun delimit t = if isDelimited then t else Layout.paren t
            in
               case e of
                  Exp {node, ty} =>
                     (delimit o Layout.seq)
                     [layoutExpNodeF node,
                      Layout.space,
                      Layout.str ":",
                      Layout.space,
                      Type.layout ty]
            end
         and layoutExpNode e = layoutExpNodeT e
         and layoutExpNodeT e = layoutExpNodeAux true e
         and layoutExpNodeF e = layoutExpNodeAux false e
         and layoutExpNodeAux isDelimited e =
            let
               fun delimit t = if isDelimited then t else Layout.paren t
            in
               case e of
                  E_Fn {lam} => (delimit o layoutLam) lam
                | E_Prim {prim, tyargs, args} =>
                     let
                        fun arg i = List.nth (args, i)
                        fun unary sym =
                           (delimit o Layout.mayAlign)
                           [Layout.str sym,
                            layoutExpF (arg 0)]
                        fun binary sym =
                           (delimit o Layout.mayAlign)
                           [layoutExpF (arg 0),
                            Layout.str sym,
                            layoutExpF (arg 1)]
                        fun ternary (syml,symr) =
                           (delimit o Layout.mayAlign)
                           [layoutExpF (arg 0),
                            Layout.str syml,
                            layoutExpF (arg 1),
                            Layout.str symr,
                            layoutExpF (arg 2)]
                        fun func name =
                           (delimit o Layout.mayAlign)
                           (List.concat
                            [[Layout.str name],
                             List.map (layoutApplyArg o A_Type) tyargs,
                             List.map (layoutApplyArg o A_Exp) args])
                     in
                        case prim of
                           Prim.Add => binary "+"
                         | Prim.Arg => func "arg"
                         | Prim.Argc => func "argc"
                         | Prim.Array => func "array"
                         | Prim.Concat => binary "^"
                         | Prim.Div => binary "/"
                         | Prim.Eq => binary "=="
                         | Prim.Fail => func "fail"
                         | Prim.Gt => binary ">"
                         | Prim.Gte => binary ">="
                         | Prim.Lt => binary "<"
                         | Prim.Lte => binary "<="
                         | Prim.Mod => binary "%"
                         | Prim.Mul => binary "*"
                         | Prim.NEq => binary "<>"
                         | Prim.Neg => unary "~"
                         | Prim.Print => func "print"
                         | Prim.Size => func "size"
                         | Prim.Sub => binary "-"
                         | Prim.Subscript => func "subscript"
                         | Prim.Upd => ternary ("!", ":=")
                         | Prim.Idx => binary "!"
                         | Prim.Len => unary "#"
                     end
                | E_DaCon {dacon, tyargs, args} =>
                     let
                        fun layoutDaConArgs args =
                           Layout.optSeq ("{", "}", ",") layoutExpT args
                     in
                        if List.null tyargs andalso List.null args
                           then DaCon.layout dacon
                        else
                           (delimit o Layout.seq)
                           [DaCon.layout dacon,
                            Layout.prefixSpaceIfNonEmpty (Type.layoutArgs tyargs),
                            Layout.prefixSpaceIfNonEmpty (layoutDaConArgs args)]
                     end
                | E_Apply {func, applyarg} =>
                     (delimit o Layout.mayAlign)
                     [layoutExpF func,
                      layoutApplyArg applyarg]
                | E_Var {var} => Var.layout var
                | E_Integer i => Layout.str (IntInf.toString i)
                | E_String s => Layout.str (concat ["\"", String.toString s, "\""])
                | E_Let {decl, body} =>
                     let
                        fun loop (exp, decls) =
                           case exp of
                              Exp {node = E_Let {decl, body}, ...} => loop (body, decl::decls)
                            | _ => (List.rev decls, exp)
                        val (decls, body) = loop (body, [decl])
                     in
                        (delimit o Layout.align)
                        [Layout.str "let",
                         Layout.indent (Layout.align (List.map layoutDecl decls), 3),
                         Layout.str "in",
                         Layout.indent (layoutExpT body, 3),
                         Layout.str "end"]
                     end
                | E_Case {arg, matchrules} =>
                     let
                        fun layoutMatchRules mrs =
                           (Layout.align o List.rev)
                           (List.foldl
                            (fn (mr,acc) =>
                             (Layout.seq [Layout.str "| ", layoutMatchRule mr])::acc)
                            []
                            mrs)
                     in
                        (delimit o Layout.align)
                        [Layout.seq
                         [Layout.str "case",
                          Layout.space,
                          layoutExpT arg,
                          Layout.space,
                          Layout.str "of"],
                         Layout.indent (layoutMatchRule (List.hd matchrules), 3),
                         Layout.indent (layoutMatchRules (List.tl matchrules), 1),
                         Layout.str "end"]
                     end
            end
         and layoutLam lam =
            case lam of
               Lam {param, body} =>
                  Layout.mayAlign
                  [Layout.seq [Layout.str "fn",
                               Layout.space,
                               Param.layout param,
                               Layout.space,
                               Layout.str "=>"],
                   Layout.indent (layoutExpF body, 3)]
         and layoutApplyArg a =
            case a of
               A_Exp e => layoutExpF e
             | A_Type ty =>
                  Layout.seq
                  [Layout.str "[",
                   Type.layout ty,
                   Layout.str "]"]
         and layoutMatchRule mr =
            case mr of
               MatchRule {pat, body} =>
                  Layout.mayAlign
                  [Layout.seq
                   [Pat.layout pat,
                    Layout.space,
                    Layout.str "=>"],
                   Layout.indent (layoutExpT body, 3)]
         and layoutDecl d =
            case d of
               D_Data {data_decls} =>
                  let
                     fun layoutDaConArgTys tys =
                        Layout.optSeq ("{", "}", ",") Type.layout tys
                     fun layoutDaConDecl {dacon, arg_tys} =
                        Layout.seq
                        [DaCon.layout dacon,
                         Layout.prefixSpaceIfNonEmpty (layoutDaConArgTys arg_tys)]
                     fun layoutDaConDecls dacon_decls =
                        (Layout.align o List.rev o #2)
                        (List.foldl
                         (fn (dacon_decl,(first,acc)) =>
                          (false,
                           (Layout.seq
                            [if first then Layout.str "=" else Layout.str "|",
                                Layout.space,
                                layoutDaConDecl dacon_decl])::acc))
                         (true,[])
                         dacon_decls)
                     fun layoutTyVars tyvars =
                        Layout.optSeq ("[", "]", ",") TyVar.layout tyvars
                     fun layoutDataDecl {tycon, tyvars, dacon_decls} =
                        Layout.seq
                        [TyCon.layout tycon,
                         Layout.prefixSpaceIfNonEmpty (layoutTyVars tyvars),
                         Layout.space,
                         layoutDaConDecls dacon_decls]
                     fun layoutDataDecls data_decls =
                        (Layout.align o List.rev o #2)
                        (List.foldl
                         (fn (data_decl,(first,acc)) =>
                          (false,
                           (Layout.seq
                            [if first then Layout.str "datatype" else Layout.str "and",
                                Layout.space,
                                layoutDataDecl data_decl])::acc))
                         (true,[])
                         data_decls)
                  in
                     layoutDataDecls data_decls
                  end
             | D_Val {var, var_ty, exp} =>
                  Layout.mayAlign
                  [Layout.seq
                   [Layout.str "val",
                    Layout.space,
                    Var.layout var,
                    Layout.space,
                    Layout.str ":",
                    Layout.space,
                    Type.layout var_ty,
                    Layout.space,
                    Layout.str "="],
                   Layout.indent (layoutExpT exp, 3)]
             | D_Fun {fun_decls} =>
                  let
                     fun layoutFunDecl kw {func, func_ty, lam} =
                        case lam of
                           Lam {param, body as Exp {ty = body_ty, ...}} =>
                              Layout.mayAlign
                              [Layout.seq
                               [Layout.str kw,
                                Layout.space,
                                Var.layout func,
                                Layout.space,
                                Layout.str "(*",
                                Layout.space,
                                Layout.str ":",
                                Layout.space,
                                Type.layout func_ty,
                                Layout.space,
                                Layout.str "*)",
                                Layout.space,
                                Param.layout param,
                                Layout.space,
                                Layout.str ":",
                                Layout.space,
                                Type.layout body_ty,
                                Layout.space,
                                Layout.str "="],
                               Layout.indent (layoutExpF body, 3)]
                     fun layoutFunDecls fun_decls =
                        (Layout.align o List.rev o #2)
                        (List.foldl
                         (fn (fun_decl,(first,acc)) =>
                          (false,
                           (layoutFunDecl (if first then "fun" else "and") fun_decl)::acc))
                         (true,[])
                         fun_decls)
                  in
                     layoutFunDecls fun_decls
                  end

         local
            val empty = (TyVar.Set.empty, TyCon.Set.empty,
                         DaCon.Set.empty, Var.Set.empty)
            fun singletonTyVar tyvar = (TyVar.Set.singleton tyvar, TyCon.Set.empty,
                                        DaCon.Set.empty, Var.Set.empty)
            fun fromListTyVars tyvars = (TyVar.Set.fromList tyvars, TyCon.Set.empty,
                                         DaCon.Set.empty, Var.Set.empty)
            fun singletonTyCon tycon = (TyVar.Set.empty, TyCon.Set.singleton tycon,
                                        DaCon.Set.empty, Var.Set.empty)
            fun singletonDaCon dacon = (TyVar.Set.empty, TyCon.Set.empty,
                                        DaCon.Set.singleton dacon, Var.Set.empty)
            fun singletonVar var = (TyVar.Set.empty, TyCon.Set.empty,
                                    DaCon.Set.empty, Var.Set.singleton var)
            fun union ((tyvar_set1, tycon_set1, dacon_set1, var_set1),
                       (tyvar_set2, tycon_set2, dacon_set2, var_set2)) =
               (TyVar.Set.union (tyvar_set1, tyvar_set2),
                TyCon.Set.union (tycon_set1, tycon_set2),
                DaCon.Set.union (dacon_set1, dacon_set2),
                Var.Set.union (var_set1, var_set2))
            fun difference ((tyvar_set1, tycon_set1, dacon_set1, var_set1),
                            (tyvar_set2, tycon_set2, dacon_set2, var_set2)) =
               (TyVar.Set.difference (tyvar_set1, tyvar_set2),
                TyCon.Set.difference (tycon_set1, tycon_set2),
                DaCon.Set.difference (dacon_set1, dacon_set2),
                Var.Set.difference (var_set1, var_set2))
         in
            fun freeIdsType ty =
               let val (tyvar_set, tycon_set) = Type.freeIds ty
               in (tyvar_set, tycon_set, DaCon.Set.empty, Var.Set.empty)
               end
            fun freeIdsTypes tys =
               List.foldl
               (fn (ty, free_ids) =>
                union (free_ids, freeIdsType ty))
               empty
               tys
            fun freeIdsParam p =
               case p of
                  Param.P_Var {var, var_ty} =>
                     (freeIdsType var_ty, singletonVar var)
                | Param.P_TyVar {tyvar} =>
                     (empty, singletonTyVar tyvar)
            fun freeIdsBind {var, var_ty} =
               (freeIdsType var_ty, singletonVar var)
            fun freeIdsBinds binds =
               List.foldl
               (fn (bind_i, (free_ids, mask_ids)) =>
                let
                   val (free_ids_i, mask_ids_i) =
                      freeIdsBind bind_i
                in
                   (union (free_ids, free_ids_i),
                    union (mask_ids, mask_ids_i))
                end)
               (empty, empty)
               binds
            fun freeIdsPat p =
               case p of
                  Pat.P_DaCon {dacon, tyargs, binds} =>
                     let
                        val (free_ids_binds, mask_ids_binds) =
                           freeIdsBinds binds
                        val free_ids_tyargs =
                           freeIdsTypes tyargs
                     in
                        (union (singletonDaCon dacon,
                                union (free_ids_tyargs, free_ids_binds)),
                         mask_ids_binds)
                     end
                | Pat.P_Var bind =>
                     freeIdsBind bind
            fun freeIdsExp exp =
               case exp of
                  Exp {node, ...} => freeIdsExpNode node
            and freeIdsExpNode node =
               case node of
                  E_Fn {lam} => freeIdsLam lam
                | E_Prim {prim, tyargs, args} =>
                     let
                        val free_ids_tyargs =
                           freeIdsTypes tyargs
                        val free_ids_args =
                           freeIdsExps args
                     in
                        union (free_ids_tyargs, free_ids_args)
                     end
                | E_DaCon {dacon, tyargs, args} =>
                     let
                        val free_ids_tyargs =
                           freeIdsTypes tyargs
                        val free_ids_args =
                           freeIdsExps args
                     in
                        union (singletonDaCon dacon,
                               union (free_ids_tyargs, free_ids_args))
                     end
                | E_Apply {func, applyarg} =>
                     union (freeIdsExp func, freeIdsApplyArg applyarg)
                | E_Var {var} =>
                     singletonVar var
                | E_Integer _ => empty
                | E_String _ => empty
                | E_Let {decl, body} =>
                     let
                        val (free_ids_decl, mask_ids_decl) =
                           freeIdsDecl decl
                        val free_ids_body =
                           freeIdsExp body
                     in
                        union (free_ids_decl,
                               difference (free_ids_body, mask_ids_decl))
                     end
                | E_Case {arg, matchrules} =>
                     union (freeIdsExp arg,
                            freeIdsMatchRules matchrules)
            and freeIdsExps exps =
               List.foldl
               (fn (exp, free_ids) =>
                union (free_ids, freeIdsExp exp))
               empty
               exps
            and freeIdsLam lam =
               case lam of
                  Lam {param, body} =>
                     let
                        val (free_ids_param, mask_ids_param) =
                           freeIdsParam param
                        val free_ids_body =
                           freeIdsExp body
                     in
                        union (free_ids_param,
                               difference (free_ids_body, mask_ids_param))
                     end
            and freeIdsApplyArg applyarg =
               case applyarg of
                  A_Exp exp => freeIdsExp exp
                | A_Type ty => freeIdsType ty
            and freeIdsMatchRule matchrule =
               case matchrule of
                  MatchRule {pat, body} =>
                     let
                        val (free_ids_pat, mask_ids_pat) =
                           freeIdsPat pat
                        val free_ids_body =
                           freeIdsExp body
                     in
                        union (free_ids_pat,
                               difference (free_ids_body, mask_ids_pat))
                     end
            and freeIdsMatchRules matchrules =
               List.foldl
               (fn (matchrule, free_ids) =>
                union (free_ids, freeIdsMatchRule matchrule))
               empty
               matchrules
            and freeIdsDecl decl =
               case decl of
                  D_Data {data_decls} =>
                     let
                        fun freeIdsDaConDecl {dacon, arg_tys} =
                           (freeIdsTypes arg_tys,
                            singletonDaCon dacon)
                        fun freeIdsDaConDecls dacon_decls =
                           List.foldl
                           (fn (dacon_decl_i, (free_ids, mask_ids)) =>
                            let
                               val (free_ids_i, mask_ids_i) =
                                  freeIdsDaConDecl dacon_decl_i
                            in
                               (union (free_ids, free_ids_i),
                                union (mask_ids, mask_ids_i))
                            end)
                           (empty, empty)
                           dacon_decls
                        fun freeIdsDataDecl {tycon, tyvars, dacon_decls} =
                           let
                              val (free_ids_dacon_decls, mask_ids_dacon_decls) =
                                 freeIdsDaConDecls dacon_decls
                              val mask_ids_tyvars =
                                 fromListTyVars tyvars
                           in
                              (difference (free_ids_dacon_decls, mask_ids_tyvars),
                               union (singletonTyCon tycon, mask_ids_dacon_decls))
                           end
                        val (free_ids, mask_ids) =
                           List.foldl
                           (fn (data_decl, (free_ids, mask_ids)) =>
                            let
                               val (free_ids_i, mask_ids_i) =
                                  freeIdsDataDecl data_decl
                            in
                               (union (free_ids, free_ids_i),
                                union (mask_ids, mask_ids_i))
                            end)
                           (empty, empty)
                           data_decls
                     in
                        (difference (free_ids, mask_ids),
                         mask_ids)
                     end
                | D_Val {var, var_ty, exp} =>
                     (union (freeIdsType var_ty,
                             freeIdsExp exp),
                      singletonVar var)
                | D_Fun {fun_decls} =>
                     let
                        fun freeIdsFunDecl {func, func_ty, lam} =
                           (union (freeIdsType func_ty,
                                   freeIdsLam lam),
                            singletonVar func)
                        val (free_ids, mask_ids) =
                           List.foldl
                           (fn (fun_decl_i, (free_ids, mask_ids)) =>
                            let
                               val (free_ids_i, mask_ids_i) =
                                  freeIdsFunDecl fun_decl_i
                            in
                               (union (free_ids, free_ids_i),
                                union (mask_ids, mask_ids_i))
                            end)
                           (empty, empty)
                           fun_decls
                     in
                        (difference (free_ids, mask_ids),
                         mask_ids)
                     end

            and freeIdsDecls decls =
               List.foldr
               (fn (decl_i, (free_ids, mask_ids)) =>
                let
                   val (free_ids_i, mask_ids_i) =
                      freeIdsDecl decl_i
                in
                   (union (difference (free_ids, mask_ids_i), free_ids_i),
                    union (mask_ids, mask_ids_i))
                end)
               (empty, empty)
               decls
         end
      end
   structure Exp =
      struct
         datatype t = datatype Exp_Lam_ApplyArg_MatchRule_Decl.exp
         datatype node = datatype Exp_Lam_ApplyArg_MatchRule_Decl.exp_node

         fun make (node, ty) = Exp {node = node, ty = ty}
         fun node (Exp {node, ...}) = node
         fun ty (Exp {ty, ...}) = ty
         val layout = Exp_Lam_ApplyArg_MatchRule_Decl.layoutExp
         val layoutNode = Exp_Lam_ApplyArg_MatchRule_Decl.layoutExpNode

         val freeIds = Exp_Lam_ApplyArg_MatchRule_Decl.freeIdsExp
         val freeTyVars = #1 o freeIds
         val freeTyCons = #2 o freeIds
         val freeDaCons = #3 o freeIds
         val freeVars = #4 o freeIds
      end
   structure Lam =
      struct
         datatype t = datatype Exp_Lam_ApplyArg_MatchRule_Decl.lam

         val layout = Exp_Lam_ApplyArg_MatchRule_Decl.layoutLam
      end
   structure ApplyArg =
      struct
         datatype t = datatype Exp_Lam_ApplyArg_MatchRule_Decl.applyarg
         val layout = Exp_Lam_ApplyArg_MatchRule_Decl.layoutApplyArg
      end
   structure MatchRule =
      struct
         datatype t = datatype Exp_Lam_ApplyArg_MatchRule_Decl.matchrule

         val layout = Exp_Lam_ApplyArg_MatchRule_Decl.layoutMatchRule
      end
   structure Decl =
      struct
         datatype t = datatype Exp_Lam_ApplyArg_MatchRule_Decl.decl

         val layout = Exp_Lam_ApplyArg_MatchRule_Decl.layoutDecl

         val freeIds = #1 o Exp_Lam_ApplyArg_MatchRule_Decl.freeIdsDecl
         val freeTyVars = #1 o freeIds
         val freeTyCons = #2 o freeIds
         val freeDaCons = #3 o freeIds
         val freeVars = #4 o freeIds
      end

   structure Prog =
      struct
         datatype t = Prog of {decls: Decl.t list, exp: Exp.t}

         fun layout p =
            case p of
               Prog {decls, exp} =>
                  Layout.align ((List.map Decl.layout decls) @
                                [Layout.str ";"] @
                                [Layout.seq [Exp.layout exp, Layout.str "\n"]])
         fun output (outStrm, prog) = Layout.output (outStrm, layout prog)
      end
end
