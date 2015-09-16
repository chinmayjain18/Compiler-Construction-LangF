(* langfc-src/parse-tree/parse-tree.sml
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
 * Parse-tree representation in the LangF compiler (langfc).
 *)

structure ParseTree :> PARSE_TREE =
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

   structure Mark =
      struct
         type 'a t = {node: 'a, span: Source.Span.t}
      end

   structure TyVarName =
      struct
         datatype t = TyVarName of Atom.atom Mark.t

         fun make (node, span) = TyVarName {node = node, span = span}
         fun node (TyVarName {node, ...}) = node
         fun span (TyVarName {span, ...}) = span
         fun layout x = Layout.str (Atom.toString (node x))

         structure OrdKey =
            struct
               type ord_key = t
               fun compare (x, y) =
                  Atom.compare (node x, node y)
            end
         structure Set = RedBlackSetFn (OrdKey)
         structure Map = RedBlackMapFn (OrdKey)

         structure HashKey =
            struct
               type hash_key = t
               fun hashVal x = Atom.hash (node x)
               fun sameKey (x, y) = Atom.same (node x, node y)
            end
         structure Tbl = HashTableFn (HashKey)
      end
   structure TyConName =
      struct
         datatype t = TyConName of Atom.atom Mark.t

         fun make (node, span) = TyConName {node = node, span = span}
         fun node (TyConName {node, ...}) = node
         fun span (TyConName {span, ...}) = span
         fun layout x = Layout.str (Atom.toString (node x))

         local
            fun mk s = make (Atom.atom s, Source.Span.bogus)
         in
            val unit = mk "Unit"
            val bool = mk "Bool"
            val integer = mk "Integer"
            val string = mk "String"
            val array = mk "Array"
         end

         structure OrdKey =
            struct
               type ord_key = t
               fun compare (x, y) =
                  Atom.compare (node x, node y)
            end
         structure Set = RedBlackSetFn (OrdKey)
         structure Map = RedBlackMapFn (OrdKey)

         structure HashKey =
            struct
               type hash_key = t
               fun hashVal x = Atom.hash (node x)
               fun sameKey (x, y) = Atom.same (node x, node y)
            end
         structure Tbl = HashTableFn (HashKey)
      end
   structure DaConName =
      struct
         datatype t = DaConName of Atom.atom Mark.t

         fun make (node, span) = DaConName {node = node, span = span}
         fun node (DaConName {node, ...}) = node
         fun span (DaConName {span, ...}) = span
         fun layout x = Layout.str (Atom.toString (node x))

         local
            fun mk s = make (Atom.atom s, Source.Span.bogus)
         in
            val unit = mk "Unit"
            val truee = mk "True"
            val falsee = mk "False"
         end

         structure OrdKey =
            struct
               type ord_key = t
               fun compare (x, y) =
                  Atom.compare (node x, node y)
            end
         structure Set = RedBlackSetFn (OrdKey)
         structure Map = RedBlackMapFn (OrdKey)

         structure HashKey =
            struct
               type hash_key = t
               fun hashVal x = Atom.hash (node x)
               fun sameKey (x, y) = Atom.same (node x, node y)
            end
         structure Tbl = HashTableFn (HashKey)
      end
   structure VarName =
      struct
         datatype t = VarName of Atom.atom Mark.t

         fun make (node, span) = VarName {node = node, span = span}
         fun node (VarName {node, ...}) = node
         fun span (VarName {span, ...}) = span
         fun layout x = Layout.str (Atom.toString (node x))

         local
            fun mk s = make (Atom.atom s, Source.Span.bogus)
         in
            val argc = mk "argc"
            val arg = mk "arg"
            val fail = mk "fail"
            val print = mk "print"
            val size = mk "size"
            val sub = mk "sub"
            val array = mk "array"
         end

         structure OrdKey =
            struct
               type ord_key = t
               fun compare (x, y) =
                  Atom.compare (node x, node y)
            end
         structure Set = RedBlackSetFn (OrdKey)
         structure Map = RedBlackMapFn (OrdKey)

         structure HashKey =
            struct
               type hash_key = t
               fun hashVal x = Atom.hash (node x)
               fun sameKey (x, y) = Atom.same (node x, node y)
            end
         structure Tbl = HashTableFn (HashKey)
      end

   structure Type =
      struct
         datatype t = Type of node Mark.t
         and node =
            T_TyFn of TyVarName.t * t
          | T_Fn of t * t
          | T_TyCon of TyConName.t * t list
          | T_TyVar of TyVarName.t

         fun make (node, span) = Type {node = node, span = span}
         fun node (Type {node, ...}) = node
         fun span (Type {span, ...}) = span
         fun layout ty = layoutT ty
         and layoutT ty = layoutAux true ty
         and layoutF ty = layoutAux false ty
         and layoutAux isDelimited ty =
            let
               fun delimit t = if isDelimited then t else Layout.paren t
            in
               case node ty of
                  T_TyFn (a, ty) =>
                     (delimit o Layout.mayAlign)
                     [Layout.seq [Layout.str "[",
                                  TyVarName.layout a,
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
                        then TyConName.layout tycon
                     else
                        (delimit o Layout.seq)
                        [TyConName.layout tycon,
                         Layout.prefixSpaceIfNonEmpty (layoutArgs tys)]
                | T_TyVar a => TyVarName.layout a
            end
         and layoutArgs tys =
            Layout.optSeq ("[", "]", ",") layoutT tys
      end

   structure Param =
      struct
         datatype t = Param of node Mark.t
         and node =
            P_VarName of VarName.t * Type.t
          | P_TyVarName of TyVarName.t

         fun make (node, span) = Param {node = node, span = span}
         fun node (Param {node, ...}) = node
         fun span (Param {span, ...}) = span
         fun layout p =
            case node p of
               P_VarName (x, ty) =>
                  Layout.seq [Layout.str "(",
                              VarName.layout x,
                              Layout.space,
                              Layout.str ":",
                              Layout.space,
                              Type.layout ty,
                              Layout.str ")"]
             | P_TyVarName a =>
                  Layout.seq [Layout.str "[",
                              TyVarName.layout a,
                              Layout.str "]"]
         fun layouts ps =
            Layout.seq (Layout.separate (List.map layout ps, " "))
      end

   structure SimplePat =
      struct
         datatype t = SimplePat of node Mark.t
         and node =
            P_VarName of VarName.t
          | P_Wild

         fun make (node, span) = SimplePat {node = node, span = span}
         fun node (SimplePat {node, ...}) = node
         fun span (SimplePat {span, ...}) = span
         fun layout p =
            case node p of
               P_VarName x => VarName.layout x
             | P_Wild => Layout.str "_"
      end
   structure Pat =
      struct
         datatype t = Pat of node Mark.t
         and node =
            P_DaCon of DaConName.t * Type.t list * SimplePat.t list
          | P_SimplePat of SimplePat.t

         fun make (node, span) = Pat {node = node, span = span}
         fun node (Pat {node, ...}) = node
         fun span (Pat {span, ...}) = span
         fun layout p =
            case node p of
               P_DaCon (dacon, tys, ps) =>
                  Layout.seq [DaConName.layout dacon,
                              Layout.prefixSpaceIfNonEmpty (Type.layoutArgs tys),
                              Layout.prefixSpaceIfNonEmpty (layoutDaConPats ps)]
             | P_SimplePat p => SimplePat.layout p
         and layoutDaConPats ps =
            Layout.optSeq ("{", "}", ",") SimplePat.layout ps
      end

   structure TernOp =
      struct
         datatype t = Upd
         fun layout t =
            case t of
               Upd => (Layout.str "!", Layout.str ":=")
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
          | Idx
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
             | Idx => Layout.str "!"
      end
   structure UnOp =
      struct
         datatype t = Neg | Len
         fun layout u =
            case u of
               Neg => Layout.str "~"
             | Len => Layout.str "#"
      end

   structure Exp_MatchRule_ApplyArg_Decl =
      struct
         datatype exp = Exp of exp_node Mark.t
         and exp_node =
            E_Fn of Param.t list * exp
          | E_If of exp * exp * exp
          | E_Orelse of exp * exp
          | E_Andalso of exp * exp
          | E_Constraint of exp * Type.t
          | E_TernOp of TernOp.t * exp * exp * exp
          | E_BinOp of BinOp.t * exp * exp
          | E_UnOp of UnOp.t * exp
          | E_DaCon of DaConName.t * Type.t list * exp list
          | E_Apply of exp * applyarg
          | E_VarName of VarName.t
          | E_Integer of IntInf.int
          | E_String of String.string
          | E_Seq of exp list
          | E_Let of decl list * exp list
          | E_Case of exp * matchrule list
         and matchrule = MatchRule of (Pat.t * exp) Mark.t
         and applyarg = ApplyArg of applyarg_node Mark.t
         and applyarg_node =
            A_Exp of exp
          | A_Type of Type.t
         and decl = Decl of decl_node Mark.t
         and decl_node =
            D_Type of TyConName.t * TyVarName.t list * Type.t
          | D_Data of (TyConName.t * TyVarName.t list *
                       (DaConName.t * Type.t list) list) list
          | D_Val of SimplePat.t * Type.t option * exp
          | D_Fun of (VarName.t * Param.t list * Type.t * exp) list

         fun layoutExp e = layoutExpT e
         and layoutExpT e = layoutExpAux true e
         and layoutExpF e = layoutExpAux false e
         and layoutExpAux isDelimited e =
            let
               fun delimit t = if isDelimited then t else Layout.paren t
            in
               case (case e of Exp {node, ...} => node) of
                  E_Fn (xs, e) =>
                     (delimit o Layout.mayAlign)
                     [Layout.seq [Layout.str "fn",
                                  Layout.prefixSpaceIfNonEmpty (Param.layouts xs),
                                  Layout.space,
                                  Layout.str "=>"],
                      Layout.indent (layoutExpF e, 3)]
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
                                                 layoutExpF eElse], 3)]
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
                | E_Constraint (e, ty) =>
                     (delimit o Layout.seq)
                     [layoutExpF e,
                      Layout.space,
                      Layout.str ":",
                      Layout.space,
                      Type.layout ty]
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
                           then DaConName.layout dacon
                        else
                           (delimit o Layout.seq)
                           [DaConName.layout dacon,
                            Layout.prefixSpaceIfNonEmpty (Type.layoutArgs tys),
                            Layout.prefixSpaceIfNonEmpty (layoutDaConArgs es)]
                     end
                | E_Apply (e, a) =>
                     (delimit o Layout.mayAlign)
                     [layoutExpF e,
                      layoutApplyArg a]
                | E_VarName x => VarName.layout x
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
            case (case a of ApplyArg {node, ...} => node) of
               A_Exp e => layoutExpF e
             | A_Type ty =>
                  Layout.seq
                  [Layout.str "[",
                   Type.layout ty,
                   Layout.str "]"]
         and layoutMatchRule mr =
            case (case mr of MatchRule {node, ...} => node) of
               (p, e) =>
                  Layout.mayAlign
                  [Layout.seq
                   [Pat.layout p,
                    Layout.space,
                    Layout.str "=>"],
                   Layout.indent (layoutExpT e, 3)]
         and layoutDecl d =
            case (case d of Decl {node, ...} => node) of
               D_Type (tycon, bs, ty) =>
                  let
                     fun layoutTyVarNames bs =
                        Layout.optSeq ("[", "]", ",") TyVarName.layout bs
                  in
                     Layout.seq [Layout.str "type",
                                 Layout.space,
                                 TyConName.layout tycon,
                                 Layout.prefixSpaceIfNonEmpty (layoutTyVarNames bs),
                                 Layout.space,
                                 Layout.str "=",
                                 Layout.space,
                                 Type.layout ty]
                  end
             | D_Data datadecls =>
                  let
                     fun layoutDaConArgTys tys =
                        Layout.optSeq ("{", "}", ",") Type.layout tys
                     fun layoutDaConDecl (dacon, tys) =
                        Layout.seq
                        [DaConName.layout dacon,
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
                     fun layoutTyVarNames bs =
                        Layout.optSeq ("[", "]", ",") TyVarName.layout bs
                     fun layoutDataDecl (tycon, bs, dacondecls) =
                        Layout.seq
                        [TyConName.layout tycon,
                         Layout.prefixSpaceIfNonEmpty (layoutTyVarNames bs),
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
             | D_Val (p, tyOpt, e) =>
                  Layout.mayAlign
                  [Layout.seq
                   [Layout.str "val",
                    Layout.space,
                    SimplePat.layout p,
                    case tyOpt of
                       NONE => Layout.empty
                     | SOME ty =>
                          Layout.seq
                          [Layout.space,
                           Layout.str ":",
                           Layout.space,
                           Type.layout ty],
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
                          VarName.layout f,
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
         datatype t = datatype Exp_MatchRule_ApplyArg_Decl.exp
         datatype node = datatype Exp_MatchRule_ApplyArg_Decl.exp_node

         fun make (node, span) = Exp {node = node, span = span}
         fun node (Exp {node, ...}) = node
         fun span (Exp {span, ...}) = span
         val layout = Exp_MatchRule_ApplyArg_Decl.layoutExp
      end
   structure MatchRule =
      struct
         datatype t = datatype Exp_MatchRule_ApplyArg_Decl.matchrule
         type node = Pat.t * Exp.t

         fun make (node, span) = MatchRule {node = node, span = span}
         fun node (MatchRule {node, ...}) = node
         fun span (MatchRule {span, ...}) = span
         val layout = Exp_MatchRule_ApplyArg_Decl.layoutMatchRule
      end
   structure ApplyArg =
      struct
         datatype t = datatype Exp_MatchRule_ApplyArg_Decl.applyarg
         datatype node = datatype Exp_MatchRule_ApplyArg_Decl.applyarg_node

         fun make (node, span) = ApplyArg {node = node, span = span}
         fun node (ApplyArg {node, ...}) = node
         fun span (ApplyArg {span, ...}) = span
         val layout = Exp_MatchRule_ApplyArg_Decl.layoutApplyArg
      end
   structure Decl =
      struct
         datatype t = datatype Exp_MatchRule_ApplyArg_Decl.decl
         datatype node = datatype Exp_MatchRule_ApplyArg_Decl.decl_node

         fun make (node, span) = Decl {node = node, span = span}
         fun node (Decl {node, ...}) = node
         fun span (Decl {span, ...}) = span
         val layout = Exp_MatchRule_ApplyArg_Decl.layoutDecl
      end

   structure Prog =
      struct
         type node = Decl.t list * Exp.t
         datatype t = Prog of node Mark.t

         fun make (node, span) = Prog {node = node, span = span}
         fun node (Prog {node, ...}) = node
         fun span (Prog {span, ...}) = span
         fun layout p =
            case node p of
               (decls, exp) =>
                  Layout.align ((List.map Decl.layout decls) @
                                [Layout.str ";"] @
                                [Layout.seq [Exp.layout exp, Layout.str "\n"]])
         fun output (outStrm, prog) = Layout.output (outStrm, layout prog)
      end

end
