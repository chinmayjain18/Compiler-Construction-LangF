(* langfc-src/abs-syn-tree/abs-syn-tree.sig
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
 * Abstract syntax tree representation in the LangF compiler (langfc).
 *)

structure AbsSynTree :> ABS_SYN_TREE =
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
          * so that a pretty-printed AST can be
          * scanned/parsed/type-checked as a LangF program.
          *)
         val unit = newSpecial "Unit"
         val bool = newSpecial "Bool"
         val integer = newSpecial "Integer"
         val string = newSpecial "String"
         val array = newSpecial "Array"
      end
   structure DaCon = Id (val defaultName = "C")
   structure DaCon =
      struct
         open DaCon

         (* Pre-defined data constructors;
          * use 'newSpecial' to omit uniquifying suffix,
          * so that a pretty-printed AST can be
          * scanned/parsed/type-checked as a LangF program.
          *)
         val unit = newSpecial "Unit"
         val truee = newSpecial "True"
         val falsee = newSpecial "False"
      end

   structure Var = Id (val defaultName = "x")
   structure Var =
      struct
         open Var

         (* Pre-defined variables;
          * use 'newSpecial' to omit uniquifying suffix,
          * so that a pretty-printed AST can be
          * scanned/parsed/type-checked as a LangF program.
          *)
         val argc = newSpecial "argc"
         val arg = newSpecial "arg"
         val fail = newSpecial "fail"
         val print = newSpecial "print"
         val size = newSpecial "size"
         val subscript = newSpecial "subscript"
         val array = newSpecial "array"
      end

   structure Type =
      struct
         datatype t =
            T_TyFn of TyVar.t * t
          | T_Fn of t * t
          | T_TyCon of TyCon.t * t list
          | T_TyVar of TyVar.t

         fun layout ty = layoutT ty
         and layoutT ty = layoutAux true ty
         and layoutF ty = layoutAux false ty
         and layoutAux isDelimited ty =
            let
               fun delimit t = if isDelimited then t else Layout.paren t
            in
               case ty of
                  T_TyFn (a, ty) =>
                     (delimit o Layout.mayAlign)
                     [Layout.seq [Layout.str "[",
                                  TyVar.layout a,
                                  Layout.str "]",
                                  Layout.space,
                                  Layout.str "->"],
                      layoutF ty]
                | T_Fn (ty1, ty2) =>
                     (delimit o Layout.mayAlign)
                     [layoutF ty1,
                      Layout.str "->",
                      layoutF ty2]
                | T_TyCon (tycon, tys) =>
                     if List.null tys
                        then TyCon.layout tycon
                     else
                        (delimit o Layout.seq)
                        [TyCon.layout tycon,
                         Layout.prefixSpaceIfNonEmpty (layoutArgs tys)]
                | T_TyVar a => TyVar.layout a
            end
         and layoutArgs tys =
            Layout.optSeq ("[", "]", ",") layoutT tys

         val bool = T_TyCon (TyCon.bool, [])
         val unit = T_TyCon (TyCon.unit, [])
         val integer = T_TyCon (TyCon.integer, [])
         val string = T_TyCon (TyCon.string, [])
         fun array ty = T_TyCon (TyCon.array, [ty])

         fun equals (ty1, ty2) =
            let
               fun loop tyVarEquiv (ty1, ty2) =
                  case (ty1, ty2) of
                     (T_TyFn (tyvar1, res1),
                      T_TyFn (tyvar2, res2)) =>
                        loop (fn (tyvar1', tyvar2') =>
                              case (TyVar.equals (tyvar1', tyvar1),
                                    TyVar.equals (tyvar2', tyvar2)) of
                                 (true, true) => true
                               | (false, false) => tyVarEquiv (tyvar1', tyvar2')
                               | _ => false)
                             (res1, res2)
                   | (T_Fn (arg1, res1),
                      T_Fn (arg2, res2)) =>
                        loop tyVarEquiv (arg1, arg2) andalso
                        loop tyVarEquiv (res1, res2)
                   | (T_TyCon (tycon1, tyargs1),
                      T_TyCon (tycon2, tyargs2)) =>
                        TyCon.equals (tycon1, tycon2) andalso
                        ListPair.allEq (loop tyVarEquiv) (tyargs1, tyargs2)
                   | (T_TyVar tyvar1,
                      T_TyVar tyvar2) =>
                        tyVarEquiv (tyvar1, tyvar2)
                   | _ => false
            in
               loop TyVar.equals (ty1, ty2)
            end

         fun freeTyVars ty =
            case ty of
               T_TyFn (tyvar, res) =>
                  let
                     val tyvar_set = freeTyVars res
                  in
                     if TyVar.Set.member (tyvar_set, tyvar)
                        then TyVar.Set.delete (tyvar_set, tyvar)
                     else tyvar_set
                  end
             | T_Fn (arg, res) =>
                  TyVar.Set.union (freeTyVars arg, freeTyVars res)
             | T_TyCon (tycon, tyargs) =>
                  List.foldr (fn (ty,tyvar_set) =>
                              TyVar.Set.union (freeTyVars ty, tyvar_set))
                             TyVar.Set.empty
                             tyargs
             | T_TyVar tyvar => TyVar.Set.singleton tyvar
         fun subst (ty, (actual, formal)) =
            let
               fun sub ty = subst (ty, (actual, formal))
            in
               case ty of
                  T_TyFn (tyvar, res) =>
                     if TyVar.equals (tyvar, formal)
                        then T_TyFn (tyvar, res)
                     else if TyVar.Set.member (freeTyVars actual, tyvar)
                        then let
                                val fresh_tyvar =
                                   TyVar.new (TyVar.name tyvar)
                                val fresh_res =
                                   subst (res, (T_TyVar fresh_tyvar, tyvar))
                             in
                                T_TyFn (fresh_tyvar, sub fresh_res)
                             end
                     else T_TyFn (tyvar, sub res)
                | T_Fn (arg, res) =>
                     T_Fn (sub arg, sub res)
                | T_TyCon (tycon, tyargs) =>
                      T_TyCon (tycon,
                               substL (tyargs, (actual, formal)))
                | T_TyVar tyvar =>
                      if TyVar.equals (tyvar, formal)
                         then actual
                      else T_TyVar tyvar
            end
         and substL (tys, (actual, formal)) =
            List.map (fn ty => subst (ty, (actual, formal))) tys
         fun substs (ty, actuals_formals) =
            List.foldl (fn ((actual, formal), ty) => subst (ty, (actual, formal))) ty actuals_formals
         and substsL (tys, actuals_formals) =
            List.map (fn ty => substs (ty, actuals_formals)) tys
         fun alphaRename ty =
            case ty of
               T_TyFn (tyvar, res) =>
                  let
                     val fresh_tyvar = TyVar.new (TyVar.name tyvar)
                     val res = alphaRename res
                     val fresh_res = subst (res, (T_TyVar fresh_tyvar, tyvar))
                  in
                     T_TyFn (fresh_tyvar, fresh_res)
                  end
             | T_Fn (arg, res) =>
                  T_Fn (alphaRename arg, alphaRename res)
             | T_TyVar tyvar => T_TyVar tyvar
             | T_TyCon (tycon, tyargs) =>
                  T_TyCon (tycon, List.map alphaRename tyargs)
         fun tycons ty =
            case ty of
               T_TyFn (tyvar, res) =>
                  tycons res
             | T_Fn (arg, res) =>
                  TyCon.Set.union (tycons arg, tycons res)
             | T_TyCon (tycon, tyargs) =>
                  List.foldr (fn (ty,tycon_set) =>
                              TyCon.Set.union (tycons ty, tycon_set))
                             (TyCon.Set.singleton tycon)
                             tyargs
             | T_TyVar tyvar => TyCon.Set.empty

         val layout = fn ty =>
            if Controls.get IdControls.canonicalIdentifiersCtl
               then layout (alphaRename ty)
            else layout ty
      end

   structure Param =
      struct
         datatype t =
            P_Var of Var.t * Type.t
          | P_TyVar of TyVar.t

         fun layout p =
            case p of
               P_Var (x, ty) =>
                  Layout.seq [Layout.str "(",
                              Var.layout x,
                              Layout.space,
                              Layout.str ":",
                              Layout.space,
                              Type.layout ty,
                              Layout.str ")"]
             | P_TyVar a =>
                  Layout.seq [Layout.str "[",
                              TyVar.layout a,
                              Layout.str "]"]
         fun layouts ps =
            Layout.seq (Layout.separate (List.map layout ps, " "))
      end

   structure SimplePat =
      struct
         datatype t =
            P_Var of Var.t * Type.t
          | P_Wild of Type.t

         fun layout p =
            case p of
               P_Var (x, ty) =>
                  Layout.seq
                  [Var.layout x,
                   Layout.space,
                   Layout.str "(* ",
                   Layout.str ":",
                   Layout.space,
                   Type.layout ty,
                   Layout.str " *)"]
             | P_Wild ty =>
                  Layout.seq
                  [Layout.str "_",
                   Layout.space,
                   Layout.str "(* ",
                   Layout.str ":",
                   Layout.space,
                   Type.layout ty,
                   Layout.str " *)"]

      end
   structure Pat =
      struct
         datatype t =
            P_DaCon of DaCon.t * Type.t list * SimplePat.t list
          | P_SimplePat of SimplePat.t

         fun layout p =
            case p of
               P_DaCon (dacon, tys, ps) =>
                  Layout.seq [DaCon.layout dacon,
                              Layout.prefixSpaceIfNonEmpty (Type.layoutArgs tys),
                              Layout.prefixSpaceIfNonEmpty (layoutDaConPats ps)]
             | P_SimplePat p => SimplePat.layout p
         and layoutDaConPats ps =
            Layout.optSeq ("{", "}", ",") SimplePat.layout ps
      end

   structure TernOp =
      struct
         datatype t =
            Upd of Type.t
         fun layout t =
            case t of
               Upd ty => (Layout.str "!", Layout.seq [Layout.str ":=",
                                                      Layout.space,
                                                      Layout.str "(* [",
                                                      Type.layout ty,
                                                      Layout.str "] *)"])
      end
   structure BinOp =
      struct
         datatype t =
            Eq
          | NEq
          | Lt
          | Lte
          | Gt
          | Gte
          | Concat
          | Add
          | Sub
          | Mul
          | Div
          | Mod
          | Idx of Type.t
         fun layout b =
            case b of
               Eq => Layout.str "=="
             | NEq => Layout.str "<>"
             | Lt => Layout.str "<"
             | Lte => Layout.str "<="
             | Gt => Layout.str ">"
             | Gte => Layout.str ">="
             | Concat => Layout.str "^"
             | Add => Layout.str "+"
             | Sub => Layout.str "-"
             | Mul => Layout.str "*"
             | Div => Layout.str "/"
             | Mod => Layout.str "%"
             | Idx ty => Layout.seq [Layout.str "!",
                                     Layout.space,
                                     Layout.str "(* [",
                                     Type.layout ty,
                                     Layout.str "] *)"]
      end
   structure UnOp =
      struct
         datatype t = 
            Neg
          | Len of Type.t
         fun layout u =
            case u of
               Neg => Layout.str "~"
             | Len ty => Layout.seq [Layout.str "#",
                                     Layout.space,
                                     Layout.str "(* [",
                                     Type.layout ty,
                                     Layout.str "] *)"]
      end

   structure Exp_MatchRule_ApplyArg_Decl =
      struct
         datatype exp = Exp of {node: exp_node, ty: Type.t}
         and exp_node =
            E_Fn of Param.t list * exp
          | E_If of exp * exp * exp
          | E_Orelse of exp * exp
          | E_Andalso of exp * exp
          | E_TernOp of TernOp.t * exp * exp * exp
          | E_BinOp of BinOp.t * exp * exp
          | E_UnOp of UnOp.t * exp
          | E_DaCon of DaCon.t * Type.t list * exp list
          | E_Apply of exp * applyarg
          | E_Var of Var.t
          | E_Integer of IntInf.int
          | E_String of String.string
          | E_Seq of exp list
          | E_Let of decl list * exp list
          | E_Case of exp * matchrule list
         and matchrule = MatchRule of (Pat.t * exp)
         and applyarg =
            A_Exp of exp
          | A_Type of Type.t
         and decl =
            D_Data of (TyCon.t * TyVar.t list *
                       (DaCon.t * Type.t list) list) list
          | D_Val of SimplePat.t * exp
          | D_Fun of (Var.t * Param.t list * Type.t * exp) list

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
                  E_Fn (xs, e) =>
                     (delimit o Layout.mayAlign)
                     [Layout.seq [Layout.str "fn",
                                  Layout.prefixSpaceIfNonEmpty (Param.layouts xs),
                                  Layout.space,
                                  Layout.str "=>"],
                      Layout.indent (layoutExpT e, 3)]
                | E_If (eIf, eThen, eElse) =>
                     (delimit o Layout.mayAlign)
                     [Layout.seq [Layout.str "if",
                                  Layout.space,
                                  layoutExpT eIf],
                      Layout.indent (Layout.seq [Layout.str "then",
                                                 Layout.space,
                                                 layoutExpT eThen], 3),
                      Layout.indent (Layout.seq [Layout.str "else",
                                                 Layout.space,
                                                 layoutExpT eElse], 3)]
                | E_Orelse (e1, e2) =>
                     (delimit o Layout.mayAlign)
                     [layoutExpF e1,
                      Layout.str "orelse",
                      layoutExpF e2]
                | E_Andalso (e1, e2) =>
                     (delimit o Layout.mayAlign)
                     [layoutExpF e1,
                      Layout.str "andalso",
                      layoutExpF e2]
                | E_TernOp (t, e1, e2, e3) =>
                     (delimit o Layout.mayAlign)
                     [layoutExpF e1,
                      #1 (TernOp.layout t),
                      layoutExpF e2,
                      #2 (TernOp.layout t),
                      layoutExpF e3]
                | E_BinOp (b, e1, e2) =>
                     (delimit o Layout.mayAlign)
                     [layoutExpF e1,
                      BinOp.layout b,
                      layoutExpF e2]
                | E_UnOp (u, e) =>
                     (delimit o Layout.mayAlign)
                     [UnOp.layout u,
                      layoutExpF e]
                | E_DaCon (dacon, tys, es) =>
                     let
                        fun layoutDaConArgs es =
                           Layout.optSeq ("{", "}", ",") layoutExpT es
                     in
                        if List.null tys andalso List.null es
                           then DaCon.layout dacon
                        else
                           (delimit o Layout.seq)
                           [DaCon.layout dacon,
                            Layout.prefixSpaceIfNonEmpty (Type.layoutArgs tys),
                            Layout.prefixSpaceIfNonEmpty (layoutDaConArgs es)]
                     end
                | E_Apply (e, a) =>
                     (delimit o Layout.mayAlign)
                     [layoutExpF e,
                      layoutApplyArg a]
                | E_Var x => Var.layout x
                | E_Integer i => Layout.str (IntInf.toString i)
                | E_String s => Layout.str (concat ["\"", String.toString s, "\""])
                | E_Seq es =>
                     Layout.seq
                     [if List.length es <= 1
                         then Layout.str "(* ERROR: E_Seq(es) with List.length es <= 1 *)"
                      else Layout.empty,
                      Layout.str "(",
                      (Layout.mayAlign o Layout.separateRight)
                      (List.map layoutExpT es, ";"),
                      Layout.str ")"]
                | E_Let (ds, es) =>
                     (delimit o Layout.align)
                     [Layout.str "let",
                      Layout.indent (Layout.align (List.map layoutDecl ds), 3),
                      Layout.str "in",
                      Layout.indent ((Layout.mayAlign o Layout.separateRight)
                                     (List.map layoutExpT es, ";"), 3),
                      Layout.str "end"]
                | E_Case (e, mrs) =>
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
                          layoutExpT e,
                          Layout.space,
                          Layout.str "of"],
                         Layout.indent (layoutMatchRule (List.hd mrs), 3),
                         Layout.indent (layoutMatchRules (List.tl mrs), 1),
                         Layout.str "end"]
                     end
            end
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
               MatchRule (p, e) =>
                  Layout.mayAlign
                  [Layout.seq
                   [Pat.layout p,
                    Layout.space,
                    Layout.str "=>"],
                   Layout.indent (layoutExpT e, 3)]
         and layoutDecl d =
            case d of
               D_Data datadecls =>
                  let
                     fun layoutDaConArgTys tys =
                        Layout.optSeq ("{", "}", ",") Type.layout tys
                     fun layoutDaConDecl (dacon, tys) =
                        Layout.seq
                        [DaCon.layout dacon,
                         Layout.prefixSpaceIfNonEmpty (layoutDaConArgTys tys)]
                     fun layoutDaConDecls dacondecls =
                        (Layout.align o List.rev o #2)
                        (List.foldl
                         (fn (dacondecl,(first,acc)) =>
                          (false,
                           (Layout.seq
                            [if first then Layout.str "=" else Layout.str "|",
                                Layout.space,
                                layoutDaConDecl dacondecl])::acc))
                         (true,[])
                         dacondecls)
                     fun layoutTyVars bs =
                        Layout.optSeq ("[", "]", ",") TyVar.layout bs
                     fun layoutDataDecl (tycon, bs, dacondecls) =
                        Layout.seq
                        [TyCon.layout tycon,
                         Layout.prefixSpaceIfNonEmpty (layoutTyVars bs),
                         Layout.space,
                         layoutDaConDecls dacondecls]
                     fun layoutDataDecls datadecls =
                        (Layout.align o List.rev o #2)
                        (List.foldl
                         (fn (datadecl,(first,acc)) =>
                          (false,
                           (Layout.seq
                            [if first then Layout.str "datatype" else Layout.str "and",
                                Layout.space,
                                layoutDataDecl datadecl])::acc))
                         (true,[])
                         datadecls)
                  in
                     layoutDataDecls datadecls
                  end
             | D_Val (p, e) =>
                  Layout.mayAlign
                  [Layout.seq
                   [Layout.str "val",
                    Layout.space,
                    SimplePat.layout p,
                    Layout.space,
                    Layout.str "="],
                   Layout.indent (layoutExpT e, 3)]
             | D_Fun fundecls =>
                  let
                     fun layoutFunDecl kw (f, xs, ty, e) =
                        Layout.mayAlign
                        [Layout.seq
                         [Layout.str kw,
                          Layout.space,
                          Var.layout f,
                          Layout.prefixSpaceIfNonEmpty (Param.layouts xs),
                          Layout.space,
                          Layout.str ":",
                          Layout.space,
                          Type.layout ty,
                          Layout.space,
                          Layout.str "="],
                         Layout.indent (layoutExpT e, 3)]
                     fun layoutFunDecls fundecls =
                        (Layout.align o List.rev o #2)
                        (List.foldl
                         (fn (fundecl,(first,acc)) =>
                          (false,
                           (layoutFunDecl (if first then "fun" else "and") fundecl)::acc))
                         (true,[])
                         fundecls)
                  in
                     layoutFunDecls fundecls
                  end
      end
   structure Exp =
      struct
         datatype node = datatype Exp_MatchRule_ApplyArg_Decl.exp_node
         datatype t = datatype Exp_MatchRule_ApplyArg_Decl.exp

         fun make (node, ty) = Exp {node = node, ty = ty}
         fun node (Exp {node, ...}) = node
         fun ty (Exp {ty, ...}) = ty
         val layout = Exp_MatchRule_ApplyArg_Decl.layoutExp
         val layoutNode = Exp_MatchRule_ApplyArg_Decl.layoutExpNode

         val unit = make (E_DaCon (DaCon.unit, [], []), Type.unit)
      end
   structure MatchRule =
      struct
         datatype t = datatype Exp_MatchRule_ApplyArg_Decl.matchrule

         val layout = Exp_MatchRule_ApplyArg_Decl.layoutMatchRule
      end
   structure ApplyArg =
      struct
         datatype t = datatype Exp_MatchRule_ApplyArg_Decl.applyarg

         val layout = Exp_MatchRule_ApplyArg_Decl.layoutApplyArg
      end
   structure Decl =
      struct
         datatype t = datatype Exp_MatchRule_ApplyArg_Decl.decl

         val layout = Exp_MatchRule_ApplyArg_Decl.layoutDecl
      end

   structure Prog =
      struct
         datatype t = Prog of Decl.t list * Exp.t

         fun layout p =
            case p of
               Prog (decls, exp) =>
                  Layout.align ((List.map Decl.layout decls) @
                                [Layout.str ";"] @
                                [Layout.seq [Exp.layout exp, Layout.str "\n"]])
         fun output (outStrm, prog) = Layout.output (outStrm, layout prog)
      end

end
