(* langfc-src/type-checker/type-checker.sml
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
 * Front-end type checker in the LangF compiler (langfc).
 *)

structure TypeChecker :> TYPE_CHECKER =
struct
   structure PT = ParseTree
   structure AST = AbsSynTree
   structure Env = Environment

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

   fun toStringTyVarName tyvar =
      Layout.toString (PT.TyVarName.layout tyvar)
   fun toStringTyConName tycon =
      Layout.toString (PT.TyConName.layout tycon)
   fun toStringDaConName dacon =
      Layout.toString (PT.DaConName.layout dacon)
   fun toStringVarName var =
      Layout.toString (PT.VarName.layout var)

   local
      fun maybeElide s =
         let
            val len_s = String.size s
         in
            if len_s < 50
               then s
            else let
                    val front =
                       (CharVectorSlice.vector o CharVectorSlice.slice)
                       (s, 0, SOME 25)
                    val back =
                       (CharVectorSlice.vector o CharVectorSlice.slice)
                       (s, len_s - 20, NONE)
                 in
                    String.concat
                    [front, " ... ", back]
                 end
         end
   in
      fun toStringType ty =
         maybeElide (Layout.toString (PT.Type.layout ty))
      fun toStringPat pat =
         maybeElide (Layout.toString (PT.Pat.layout pat))
      fun toStringExp exp =
         maybeElide (Layout.toString (PT.Exp.layout exp))
      fun toStringMatchRule matchrule =
         maybeElide (Layout.toString (PT.MatchRule.layout matchrule))
      fun toStringDecl decl =
         maybeElide (Layout.toString (PT.Decl.layout decl))
   end

   local
      fun layoutType env ty = layoutTypeT env ty
      and layoutTypeT env ty = layoutTypeAux env true ty
      and layoutTypeF env ty = layoutTypeAux env false ty
      and layoutTypeAux env isDelimited ty =
         let
            fun delimit t = if isDelimited then t else Layout.paren t
         in
            case ty of
               AST.Type.T_TyFn (a, ty) =>
                  let
                     val tyvarPT =
                        let
                           fun next cs =
                              case cs of
                                 nil => [#"a"]
                               | #"z"::cs => #"a" :: (next cs)
                               | c::cs => (Char.succ c) :: cs
                           fun search cs =
                              let
                                 val name = implode (#"'" :: (List.rev cs))
                                 val tyvarPT =
                                    PT.TyVarName.TyVarName
                                    {node = Atom.atom name,
                                     span = Source.Span.bogus}
                              in
                                 case Env.lookupTyVar (env, tyvarPT) of
                                    NONE => tyvarPT
                                  | SOME _ => search (next cs)
                              end
                        in
                           search (next [])
                        end
                     val env' =
                        Env.extend
                        (env, Env.singletonTyVarWithRev
                         (tyvarPT, {tyvar = a}))
                  in
                     (delimit o Layout.mayAlign)
                     [Layout.seq [Layout.str "[",
                                  layoutTyVar env' a,
                                  Layout.str "]",
                                  Layout.space,
                                  Layout.str "->"],
                      layoutTypeF env' ty]
                  end
             | AST.Type.T_Fn (ty1, ty2) =>
                  (delimit o Layout.mayAlign)
                  [layoutTypeF env ty1,
                   Layout.str "->",
                   layoutTypeF env ty2]
             | AST.Type.T_TyCon (tycon, tys) =>
                  if List.null tys
                     then layoutTyCon env tycon
                  else
                     (delimit o Layout.seq)
                     [layoutTyCon env tycon,
                      Layout.prefixSpaceIfNonEmpty (layoutTypeArgs env tys)]
             | AST.Type.T_TyVar a => layoutTyVar env a
         end
      and layoutTypeArgs env tys =
         Layout.optSeq ("[", "]", ",") (layoutTypeT env) tys
      and layoutTyVar env tyvar =
         (case Env.lookupTyVarRev (env, tyvar) of
             NONE => raise Fail "Env.lookupTyVarRev"
           | SOME ({tyvar = tyvarPT, ...}) =>
                let
                   val inScope =
                      PT.TyVarName.layout tyvarPT
                   val outOfScope =
                      Layout.seq [Layout.str "?", inScope, Layout.str "?"]
                in
                   case Env.lookupTyVar (env, tyvarPT) of
                      NONE => outOfScope
                    | SOME {tyvar = tyvarAST, ...} =>
                         if AST.TyVar.equals (tyvar, tyvarAST)
                            then inScope
                         else outOfScope
                end)
      and layoutTyCon env tycon =
         (case Env.lookupTyConRev (env, tycon) of
             NONE => raise Fail "Env.lookupTyConRev"
           | SOME ({tycon = tyconPT, ...}) =>
                let
                   val inScope =
                      PT.TyConName.layout tyconPT
                   val outOfScope =
                      Layout.seq [Layout.str "?", inScope, Layout.str "?"]
                in
                   case Env.lookupTyCon (env, tyconPT) of
                      NONE => outOfScope
                    | SOME (Env.TyConEnv.Type _) =>
                         outOfScope
                    | SOME (Env.TyConEnv.Data {tycon = tyconAST, ...}) =>
                         if AST.TyCon.equals (tycon, tyconAST)
                            then inScope
                         else outOfScope
                end)
      fun layoutDaCon env dacon =
         (case Env.lookupDaConRev (env, dacon) of
             NONE => raise Fail "Env.lookupDaConRev"
           | SOME ({dacon = daconPT, ...}) =>
                let
                   val inScope =
                      PT.DaConName.layout daconPT
                   val outOfScope =
                      Layout.seq [Layout.str "?", inScope, Layout.str "?"]
                in
                   case Env.lookupDaCon (env, daconPT) of
                      NONE => outOfScope
                    | SOME {dacon = daconAST, ...} =>
                         if AST.DaCon.equals (dacon, daconAST)
                            then inScope
                         else outOfScope
                end)
   in
      fun toStringTypeAST env ty =
         Layout.toString (layoutType env ty)
      fun toStringTyConAST env tycon =
         Layout.toString (layoutTyCon env tycon)
      fun toStringDaConAST env dacon =
         Layout.toString (layoutDaCon env dacon)
   end

   val minInt = ~ (IntInf.pow (2, 62))
   val maxInt = IntInf.pow (2, 62) - 1

   datatype u = Infinite
              | Finite of AST.DaCon.Set.set
              | Empty

   fun typeCheck (errStrm: ErrorStream.t,
                  prog : PT.Prog.t) : AST.Prog.t option =
      let

         exception TypeError
         fun addErrorAt (span, msgs) : unit =
            ErrorStream.addErrorAt (errStrm, span, concat ("\n" :: msgs))

         val bogusTyVar =
            PT.TyVarName.make (Atom.atom "?", Source.Span.bogus)
         val bogusTyVarAST =
            AST.TyVar.newSpecial "?"
         val bogusTyAST =
            AST.Type.T_TyVar (bogusTyVarAST)

         fun fixArity (xs, arity, mkDefault, mkError) =
            let
               val len_xs = List.length xs
               fun err () = mkError {given = Int.toString len_xs,
                                     takes = Int.toString arity}
            in
               if arity < len_xs
                  then (err ();
                        List.take (xs, arity))
               else if arity > len_xs
                   then (err ();
                         xs @ (List.tabulate
                               (arity - len_xs,
                                fn _ => mkDefault ())))
               else xs
            end

         fun checkType (env: Env.t, ty: PT.Type.t)
                       : AST.Type.t =
            case PT.Type.node ty of
               PT.Type.T_TyFn (tyvar, res) =>
                  let
                     val tyvarAST = AST.TyVar.new (toStringTyVarName tyvar)
                     val resAST =
                        checkType
                        (Env.extend
                         (env,
                          Env.singletonTyVarWithRev (tyvar, {tyvar = tyvarAST})),
                         res)
                  in
                     AST.Type.T_TyFn (tyvarAST, resAST)
                  end
             | PT.Type.T_Fn (arg, res) =>
                  let
                     val argAST = checkType (env, arg)
                     val resAST = checkType (env, res)
                  in
                     AST.Type.T_Fn (argAST, resAST)
                  end
             | PT.Type.T_TyCon (tycon, actual_tyargs) =>
                  let
                     val actual_tyargsAST = checkTypes (env, actual_tyargs)
                     fun finish_actual_tyargsAST arity =
                        fixArity
                        (actual_tyargsAST,
                         arity,
                         fn () => AST.Type.unit,
                         fn {given, takes} =>
                         addErrorAt
                         (PT.Type.span ty,
                          ["Type constructor ",
                           toStringTyConName tycon,
                           " given ", given,
                           " type arguments but takes ", takes,
                           " type arguments.\n",
                           "  in: ", toStringType ty]))
                  in
                     case Env.lookupTyCon (env, tycon) of
                        NONE =>
                           (addErrorAt
                            (PT.TyConName.span tycon,
                             ["Unbound type constructor: ",
                              toStringTyConName tycon, ".\n",
                              "  in: ", toStringType ty]);
                            AST.Type.unit)
                      | SOME (Env.TyConEnv.Type
                              {tyvars = formal_tyvarsAST, ty = tyAST}) =>
                           let
                              val actual_tyargsAST =
                                 finish_actual_tyargsAST
                                 (List.length formal_tyvarsAST)
                           in
                              AST.Type.substs (tyAST,
                                               ListPair.zip (actual_tyargsAST,
                                                             formal_tyvarsAST))
                           end
                      | SOME (Env.TyConEnv.Data
                              {tycon = tyconAST}) =>
                           let
                              val {arity, ...} =
                                 valOf (Env.lookupTyConRev (env, tyconAST))
                              val actual_tyargsAST =
                                 finish_actual_tyargsAST arity
                           in
                              AST.Type.T_TyCon (tyconAST, actual_tyargsAST)
                           end
                  end
             | PT.Type.T_TyVar tyvar =>
                  (case Env.lookupTyVar (env, tyvar) of
                      NONE =>
                         (addErrorAt
                          (PT.TyVarName.span tyvar,
                           ["Unbound type variable: ",
                            toStringTyVarName tyvar, ".\n",
                            "  in: ", toStringType ty]);
                          AST.Type.unit)
                    | SOME {tyvar = tyvarAST} =>
                         AST.Type.T_TyVar tyvarAST)
         and checkTypes (env: Env.t, tys : PT.Type.t list)
                        : AST.Type.t list =
            List.map (fn ty => checkType (env, ty)) tys

         fun checkParam (env: Env.t, param: PT.Param.t)
                        : Env.t * (AST.Type.t -> AST.Type.t) * AST.Param.t =
            case PT.Param.node param of
               PT.Param.P_VarName (var, var_ty) =>
                  let
                     val varAST = AST.Var.new (toStringVarName var)
                     val var_tyAST = checkType (env, var_ty)
                     val env' =
                        Env.singletonVar (var, {var = varAST, var_ty = var_tyAST})
                     val mkTy = fn res_tyAST =>
                        AST.Type.T_Fn (var_tyAST, res_tyAST)
                  in
                     (env', mkTy,
                      AST.Param.P_Var (varAST, var_tyAST))
                  end
             | PT.Param.P_TyVarName tyvar =>
                  let
                     val tyvarAST = AST.TyVar.new (toStringTyVarName tyvar)
                     val env' =
                        Env.singletonTyVarWithRev (tyvar, {tyvar = tyvarAST})
                     val mkTy = fn res_tyAST =>
                        AST.Type.T_TyFn (tyvarAST, res_tyAST)
                  in
                     (env', mkTy,
                      AST.Param.P_TyVar tyvarAST)
                  end

         fun checkParams (env: Env.t, params: PT.Param.t list)
                         : Env.t * (AST.Type.t -> AST.Type.t) * AST.Param.t list =
            let
               val (paramsAST, (env', mkTy)) =
                  ListExtra.mapAndFoldl
                  (fn (param_i, (env', mkTy)) =>
                   let
                      val (env'_i, mkTy_i, paramAST_i) =
                         checkParam (Env.extend (env, env'), param_i)
                   in
                      (paramAST_i,
                       (Env.extend (env', env'_i),
                        mkTy o mkTy_i))
                   end)
                  (Env.empty, fn ty => ty)
                  params
            in
               (env', mkTy, paramsAST)
            end

         fun checkSimplePat (env: Env.t, arg_tyAST: AST.Type.t, pat: PT.SimplePat.t,
                             setV: PT.VarName.Set.set, mkExtraMsgs: unit -> string list)
                            : Env.t * AST.SimplePat.t * PT.VarName.Set.set =
            case PT.SimplePat.node pat of
               PT.SimplePat.P_VarName var =>
                  let
                     val varAST = AST.Var.new (toStringVarName var)
                     val env' =
                        Env.singletonVar (var, {var = varAST, var_ty = arg_tyAST})
                     val () =
                        if PT.VarName.Set.member (setV, var)
                           then addErrorAt
                                (PT.VarName.span var,
                                 ["Variable of pattern defined multiple times.\n",
                                  "  variable: ", toStringVarName var, "\n"]
                                 @ (mkExtraMsgs ()))
                        else ()
                     val setV =
                        PT.VarName.Set.add (setV, var)
                  in
                     (env',
                      AST.SimplePat.P_Var (varAST, arg_tyAST),
                      setV)
                  end
             | PT.SimplePat.P_Wild =>
                  let
                     val env' = Env.empty
                  in
                     (env',
                      AST.SimplePat.P_Wild arg_tyAST,
                      setV)
                  end

         fun checkPat (env: Env.t, arg_tyAST: AST.Type.t, pat: PT.Pat.t,
                       setDC: u, mkExtraMsgs: unit -> string list)
                      : Env.t * AST.Pat.t * u =
            case PT.Pat.node pat of
               PT.Pat.P_DaCon (dacon, actual_tyargs, actual_pats) =>
                  let
                     val actual_tyargsAST = checkTypes (env, actual_tyargs)
                     fun finish_actual_tyargsAST arity =
                        fixArity
                        (actual_tyargsAST,
                         arity,
                         fn _ => AST.Type.unit,
                         fn {given, takes} =>
                         addErrorAt
                         (PT.Pat.span pat,
                          ["Data constructor ",
                           toStringDaConName dacon,
                           " given ", given,
                           " type arguments but takes ", takes,
                           " type arguments.\n",
                           "  in pattern: ", toStringPat pat, "\n"]
                          @ (mkExtraMsgs ())))
                     fun finish_actual_patsAST formal_arg_tysAST =
                        let
                           val (actual_patsAST, (env', setV)) =
                              ListPairExtra.mapAndFoldl
                              (fn (actual_pat_i, formal_arg_tyAST_i, (env', setV)) =>
                               let
                                  val (env'_i, actual_patAST_i, setV) =
                                     checkSimplePat (env,
                                                     formal_arg_tyAST_i,
                                                     actual_pat_i,
                                                     setV,
                                                     fn () =>
                                                     ["  in pattern: ", toStringPat pat, "\n"]
                                                     @ (mkExtraMsgs ()))
                               in
                                  (actual_patAST_i,
                                   (Env.extend (env', env'_i),
                                    setV))
                               end)
                               (Env.empty, PT.VarName.Set.empty)
                               (actual_pats, formal_arg_tysAST)
                        in
                           (env', actual_patsAST)
                        end
                  in
                     case Env.lookupDaCon (env, dacon) of
                        NONE =>
                           let
                              val _ =
                                 addErrorAt
                                 (PT.DaConName.span dacon,
                                  ["Unbound data constructor: ",
                                   toStringDaConName dacon, ".\n",
                                   "  in pattern: ", toStringPat pat, "\n"]
                                  @ (mkExtraMsgs ()))
                              val (env', actual_patsAST) =
                                 finish_actual_patsAST
                                 (List.map (fn _ => AST.Type.unit) actual_pats)
                           in
                              (env',
                               AST.Pat.P_DaCon
                               (AST.DaCon.new "Bogus",
                                actual_tyargsAST,
                                actual_patsAST),
                               setDC)
                           end
                      | SOME {dacon = daconAST, tyvars = formal_tyvarsAST,
                              arg_tys = formal_arg_tysAST, tycon = tyconAST} =>
                           let
                              val actual_tyargsAST =
                                 finish_actual_tyargsAST
                                 (List.length formal_tyvarsAST)
                              val formal_arg_tysAST =
                                 AST.Type.substsL (formal_arg_tysAST,
                                                   ListPair.zip (actual_tyargsAST, formal_tyvarsAST))
                              val formal_arg_tysAST =
                                 fixArity
                                 (formal_arg_tysAST,
                                  List.length actual_pats,
                                  fn _ => AST.Type.unit,
                                  fn {given, takes} =>
                                  addErrorAt
                                  (PT.Pat.span pat,
                                   ["Data constructor ",
                                    toStringDaConName dacon,
                                    " given ", takes,
                                    " patterns but takes ", given,
                                    " arguments.\n",
                                    "  in pattern: ", toStringPat pat, "\n"]
                                   @ (mkExtraMsgs ())))
                              val (env', actual_patsAST) =
                                 finish_actual_patsAST formal_arg_tysAST
                              val tyAST =
                                 AST.Type.T_TyCon (tyconAST,
                                                   actual_tyargsAST)
                              val () =
                                 if AST.Type.equals (arg_tyAST, tyAST)
                                    then ()
                                 else addErrorAt
                                      (PT.Pat.span pat,
                                       ["Argument and pattern in 'case' disagree.\n",
                                        "  argument: ", toStringTypeAST env arg_tyAST, "\n",
                                        "  pattern:  ", toStringTypeAST env tyAST, "\n"]
                                       @ (mkExtraMsgs ()))
                              val setDC =
                                 case setDC of
                                    Infinite => setDC
                                  | Finite setDC =>
                                       if AST.DaCon.Set.member (setDC, daconAST)
                                          then Finite (AST.DaCon.Set.delete (setDC, daconAST))
                                       else (addErrorAt
                                             (PT.Pat.span pat,
                                              ["Redundant pattern in 'case'.\n",
                                               "  pattern: ", toStringPat pat, "\n"]
                                              @ (mkExtraMsgs ()));
                                             Finite setDC)
                                  | Empty =>
                                       (addErrorAt
                                        (PT.Pat.span pat,
                                         ["Redundant pattern in 'case'.\n",
                                          "  pattern: ", toStringPat pat, "\n"]
                                         @ (mkExtraMsgs ()));
                                        setDC)
                           in
                              (env',
                               AST.Pat.P_DaCon (daconAST,
                                                actual_tyargsAST,
                                                actual_patsAST),
                               setDC)
                           end
                  end
             | PT.Pat.P_SimplePat spat =>
                  let
                     val (env', spatAST, _) =
                        checkSimplePat (env,
                                        arg_tyAST,
                                        spat,
                                        PT.VarName.Set.empty,
                                        fn () => [])
                     val () =
                        case setDC of
                           Infinite => ()
                         | Finite setDC =>
                              if AST.DaCon.Set.isEmpty setDC
                                 then addErrorAt
                                      (PT.Pat.span pat,
                                       ["Redundant pattern in 'case'.\n",
                                        "  pattern: ", toStringPat pat, "\n"]
                                       @ (mkExtraMsgs ()))
                              else ()
                         | Empty =>
                              addErrorAt
                              (PT.Pat.span pat,
                               ["Redundant pattern in 'case'.\n",
                                "  pattern: ", toStringPat pat, "\n"]
                               @ (mkExtraMsgs ()))
                  in
                     (env', AST.Pat.P_SimplePat spatAST, Empty)
                  end

         fun checkDaConDecl (env: Env.t,
                             tyvarsAST: AST.TyVar.t list,
                             tyconAST: AST.TyCon.t,
                             dacon_decl: PT.DaConName.t * PT.Type.t list,
                             setDC: PT.DaConName.Set.set,
                             mkExtraMsgs: unit -> string list)
                            : Env.t * (AST.DaCon.t * AST.Type.t list) * PT.DaConName.Set.set =
            let
               val (dacon, arg_tys) = dacon_decl
               val daconAST = AST.DaCon.new (toStringDaConName dacon)
               val arg_tysAST = checkTypes (env, arg_tys)
               val env' =
                  Env.singletonDaConWithRev (dacon, {tyvars = tyvarsAST,
                                                     arg_tys = arg_tysAST,
                                                     tycon = tyconAST,
                                                     dacon = daconAST})
               val () =
                  if PT.DaConName.Set.member (setDC, dacon)
                     then addErrorAt
                          (PT.DaConName.span dacon,
                           ["Data constructor of 'datatype' defined multiple times.\n",
                            "  data constructor: ", toStringDaConName dacon, "\n"]
                           @ (mkExtraMsgs ()))
                  else ()
               val setDC = PT.DaConName.Set.add (setDC, dacon)
            in
               (env',
                (daconAST, arg_tysAST),
                setDC)
            end
         fun checkDaConDecls (env: Env.t,
                              tyvarsAST: AST.TyVar.t list,
                              tyconAST: AST.TyCon.t,
                              dacon_decls: (PT.DaConName.t * PT.Type.t list) list,
                              setDC: PT.DaConName.Set.set,
                              mkExtraMsgs: unit -> string list)
                             : Env.t * (AST.DaCon.t * AST.Type.t list) list * PT.DaConName.Set.set =
            let
               val (dacon_declsAST, (env', setDC)) =
                  ListExtra.mapAndFoldl
                  (fn (dacon_decl_i, (env', setDC)) =>
                   let
                      val (env'_i, dacon_declAST_i, setDC) =
                         checkDaConDecl (env, tyvarsAST, tyconAST,
                                         dacon_decl_i,
                                         setDC, mkExtraMsgs)
                   in
                      (dacon_declAST_i,
                       (Env.extend (env', env'_i),
                        setDC))
                   end)
                  (Env.empty, setDC)
                  dacon_decls
            in
               (env',
                dacon_declsAST,
                setDC)
            end

         fun checkExp (env: Env.t, exp: PT.Exp.t)
                      : AST.Exp.t =
            case PT.Exp.node exp of
               PT.Exp.E_Fn (params, body) =>
                  let
                     val (env', mkTy, paramsAST) =
                        checkParams (env, params)
                     val bodyAST = checkExp (Env.extend (env, env'), body)
                     val body_tyAST = AST.Exp.ty bodyAST
                  in
                     AST.Exp.make
                     (AST.Exp.E_Fn (paramsAST, bodyAST),
                      mkTy body_tyAST)
                  end
             | PT.Exp.E_If (expIf, expThen, expElse) =>
                  let
                     val expIfAST = checkExp (env, expIf)
                     val expIf_tyAST = AST.Exp.ty expIfAST
                     val () =
                        if AST.Type.equals (expIf_tyAST, AST.Type.bool)
                           then ()
                        else addErrorAt
                             (PT.Exp.span expIf,
                              ["Test of 'if' not of type ",
                               toStringTypeAST env AST.Type.bool, ".\n",
                               "  test: ", toStringTypeAST env expIf_tyAST, "\n",
                               "  in: ", toStringExp exp])
                     val expThenAST = checkExp (env, expThen)
                     val expThen_tyAST = AST.Exp.ty expThenAST
                     val expElseAST = checkExp (env, expElse)
                     val expElse_tyAST = AST.Exp.ty expElseAST
                     val () =
                        if AST.Type.equals (expThen_tyAST, expElse_tyAST)
                           then ()
                        else addErrorAt
                             (PT.Exp.span exp,
                              ["Then and else branches of 'if' disagree.\n",
                               "  then: ", toStringTypeAST env expThen_tyAST, "\n",
                               "  else: ", toStringTypeAST env expElse_tyAST, "\n",
                               "  in: ", toStringExp exp])
                  in
                     AST.Exp.make
                     (AST.Exp.E_If (expIfAST, expThenAST, expElseAST),
                      expElse_tyAST)
                  end
             | PT.Exp.E_Orelse (expl, expr) =>
                  let
                     val explAST = checkExp (env, expl)
                     val expl_tyAST = AST.Exp.ty explAST
                     val () =
                        if AST.Type.equals (expl_tyAST, AST.Type.bool)
                           then ()
                        else addErrorAt
                             (PT.Exp.span expl,
                              ["Left operand of 'orelse' not of type ",
                               toStringTypeAST env AST.Type.bool, ".\n",
                               "  operand: ", toStringTypeAST env expl_tyAST, "\n",
                               "  in: ", toStringExp exp])
                     val exprAST = checkExp (env, expr)
                     val expr_tyAST = AST.Exp.ty exprAST
                     val () =
                        if AST.Type.equals (expr_tyAST, AST.Type.bool)
                           then ()
                        else addErrorAt
                             (PT.Exp.span expr,
                              ["Right operand of 'orelse' not of type ",
                               toStringTypeAST env AST.Type.bool, ".\n",
                               "  operand: ", toStringTypeAST env expr_tyAST, "\n",
                               "  in: ", toStringExp exp])
                  in
                     AST.Exp.make
                     (AST.Exp.E_Orelse (explAST, exprAST),
                      AST.Type.bool)
                  end
             | PT.Exp.E_Andalso (expl, expr) =>
                  let
                     val explAST = checkExp (env, expl)
                     val expl_tyAST = AST.Exp.ty explAST
                     val () =
                        if AST.Type.equals (expl_tyAST, AST.Type.bool)
                           then ()
                        else addErrorAt
                             (PT.Exp.span expl,
                              ["Left operand of 'andalso' not of type ",
                               toStringTypeAST env AST.Type.bool, ".\n",
                               "  operand: ", toStringTypeAST env expl_tyAST, "\n",
                               "  in: ", toStringExp exp])
                     val exprAST = checkExp (env, expr)
                     val expr_tyAST = AST.Exp.ty exprAST
                     val () =
                        if AST.Type.equals (expr_tyAST, AST.Type.bool)
                           then ()
                        else addErrorAt
                             (PT.Exp.span expr,
                              ["Right operand of 'andalso' not of type ",
                               toStringTypeAST env AST.Type.bool, ".\n",
                               "  operand: ", toStringTypeAST env expr_tyAST, "\n",
                               "  in: ", toStringExp exp])
                  in
                     AST.Exp.make
                     (AST.Exp.E_Andalso (explAST, exprAST),
                      AST.Type.bool)
                  end
             | PT.Exp.E_Constraint (body, ty) =>
                  let
                     val bodyAST = checkExp (env, body)
                     val body_tyAST = AST.Exp.ty bodyAST
                     val tyAST = checkType (env, ty)
                     val () =
                        if AST.Type.equals (body_tyAST, tyAST)
                           then ()
                        else addErrorAt
                             (PT.Exp.span exp,
                              ["Expression and constraint in ':' disagree.\n",
                               "  expression: ", toStringTypeAST env body_tyAST, "\n",
                               "  constraint: ", toStringTypeAST env tyAST, "\n",
                               "  in: ", toStringExp exp])
                  in
                     bodyAST
                  end
             | PT.Exp.E_TernOp (ternop, expl, expm, expr) =>
                  let
                     val chk_ternop_left_tyAST =
                        let
                           val arrayUpdTernOp =
                              fn expl_tyAST =>
                              let
                                 fun mkRes tyargAST =
                                    fn expm_tyAST =>
                                    (if AST.Type.equals (expm_tyAST, AST.Type.integer)
                                        then NONE
                                     else SOME AST.Type.integer,
                                     fn expr_tyAST =>
                                     (if AST.Type.equals (expr_tyAST, tyargAST)
                                         then NONE
                                      else SOME tyargAST,
                                      (AST.TernOp.Upd tyargAST, tyargAST)))
                                 fun mkResErr tyargAST =
                                    (SOME (AST.Type.array tyargAST),
                                     mkRes tyargAST)
                              in
                                 case expl_tyAST of
                                    AST.Type.T_TyCon (tyconAST, actual_tyargsAST) =>
                                       if AST.TyCon.equals (tyconAST, AST.TyCon.array)
                                          then 
                                             case actual_tyargsAST of
                                                [] => mkResErr bogusTyAST
                                              | [tyargAST] => (NONE, mkRes tyargAST)
                                              | tyargAST::_ => mkResErr tyargAST
                                       else mkResErr bogusTyAST
                                  | _ => mkResErr bogusTyAST
                              end
                        in
                           case ternop of
                              PT.TernOp.Upd => arrayUpdTernOp
                        end
                     val explAST = checkExp (env, expl)
                     val expl_tyAST = AST.Exp.ty explAST
                     val (chk_ternop_left_tyAST_res, chk_ternop_mid_tyAST) =
                        chk_ternop_left_tyAST expl_tyAST
                     val () =
                        case chk_ternop_left_tyAST_res of
                           NONE => ()
                         | SOME ternop_left_tyAST =>
                              addErrorAt
                              (PT.Exp.span expl,
                               ["Left operand of '",
                                Layout.toString (Layout.seq [#1 (PT.TernOp.layout ternop),
                                                             Layout.str " ",
                                                             #2 (PT.TernOp.layout ternop)]),
                                "' not of type ",
                                toStringTypeAST env ternop_left_tyAST, ".\n",
                                "  operand: ", toStringTypeAST env expl_tyAST, "\n",
                                "  in: ", toStringExp exp])
                     val expmAST = checkExp (env, expm)
                     val expm_tyAST = AST.Exp.ty expmAST
                     val (chk_ternop_mid_tyAST_res, chk_ternop_right_tyAST) =
                        chk_ternop_mid_tyAST expm_tyAST
                     val () =
                        case chk_ternop_mid_tyAST_res of
                           NONE => ()
                         | SOME ternop_mid_tyAST =>
                              addErrorAt
                              (PT.Exp.span expm,
                               ["Mid operand of '",
                                Layout.toString (Layout.seq [#1 (PT.TernOp.layout ternop),
                                                             Layout.str " ",
                                                             #2 (PT.TernOp.layout ternop)]),
                                "' not of type ",
                                toStringTypeAST env ternop_mid_tyAST, ".\n",
                                "  operand: ", toStringTypeAST env expm_tyAST, "\n",
                                "  in: ", toStringExp exp])
                     val exprAST = checkExp (env, expr)
                     val expr_tyAST = AST.Exp.ty exprAST
                     val (chk_ternop_right_tyAST_res, (ternopAST, ternop_res_tyAST)) =
                        chk_ternop_right_tyAST expr_tyAST
                     val () =
                        case chk_ternop_right_tyAST_res of
                           NONE => ()
                         | SOME ternop_right_tyAST =>
                              addErrorAt
                              (PT.Exp.span expr,
                               ["Right operand of '",
                                Layout.toString (Layout.seq [#1 (PT.TernOp.layout ternop),
                                                             Layout.str " ",
                                                             #2 (PT.TernOp.layout ternop)]),
                                "' not of type ",
                                toStringTypeAST env ternop_right_tyAST, ".\n",
                                "  operand: ", toStringTypeAST env expr_tyAST, "\n",
                                "  in: ", toStringExp exp])
                  in
                     AST.Exp.make
                     (AST.Exp.E_TernOp (ternopAST, explAST, expmAST, exprAST),
                      ternop_res_tyAST)
                  end
             | PT.Exp.E_BinOp (binop, expl, expr) =>
                  let
                     val chk_binop_left_tyAST =
                        let
                           fun simpleBinOp (binop, tyAST_left, tyAST_right, tyAST_res) =
                              fn expl_tyAST =>
                              (if AST.Type.equals (expl_tyAST, tyAST_left)
                                  then NONE
                               else SOME tyAST_left,
                               fn expr_tyAST =>
                               (if AST.Type.equals (expr_tyAST, tyAST_right)
                                   then NONE
                                else SOME tyAST_right,
                                (binop, tyAST_res)))
                           fun integerArithBinOp binop =
                              simpleBinOp (binop, 
                                           AST.Type.integer, AST.Type.integer, 
                                           AST.Type.integer)
                           fun integerCmpBinOp binop =
                              simpleBinOp (binop, 
                                           AST.Type.integer, AST.Type.integer, 
                                           AST.Type.bool)
                           val arrayIdxBinOp =
                              fn expl_tyAST =>
                              let
                                 fun mkRes tyargAST =
                                    fn expr_tyAST =>
                                    (if AST.Type.equals (expr_tyAST, AST.Type.integer)
                                        then NONE
                                     else SOME AST.Type.integer,
                                     (AST.BinOp.Idx tyargAST, tyargAST))
                                 fun mkResErr tyargAST =
                                    (SOME (AST.Type.array tyargAST),
                                     mkRes tyargAST)
                              in
                                 case expl_tyAST of
                                    AST.Type.T_TyCon (tyconAST, actual_tyargsAST) =>
                                       if AST.TyCon.equals (tyconAST, AST.TyCon.array)
                                          then 
                                             case actual_tyargsAST of
                                                [] => mkResErr bogusTyAST
                                              | [tyargAST] => (NONE, mkRes tyargAST)
                                              | tyargAST::_ => mkResErr tyargAST
                                       else mkResErr bogusTyAST
                                  | _ => mkResErr bogusTyAST
                              end
                        in
                           case binop of
                              PT.BinOp.Eq => integerCmpBinOp AST.BinOp.Eq
                            | PT.BinOp.NEq => integerCmpBinOp AST.BinOp.NEq
                            | PT.BinOp.Lt => integerCmpBinOp AST.BinOp.Lt
                            | PT.BinOp.Lte => integerCmpBinOp AST.BinOp.Lte
                            | PT.BinOp.Gt => integerCmpBinOp AST.BinOp.Gt
                            | PT.BinOp.Gte => integerCmpBinOp AST.BinOp.Gte
                            | PT.BinOp.Concat =>
                                 simpleBinOp
                                 (AST.BinOp.Concat,
                                  AST.Type.string, AST.Type.string,
                                  AST.Type.string)
                            | PT.BinOp.Add => integerArithBinOp AST.BinOp.Add
                            | PT.BinOp.Sub => integerArithBinOp AST.BinOp.Sub
                            | PT.BinOp.Mul => integerArithBinOp AST.BinOp.Mul
                            | PT.BinOp.Div => integerArithBinOp AST.BinOp.Div
                            | PT.BinOp.Mod => integerArithBinOp AST.BinOp.Mod
                            | PT.BinOp.Idx => arrayIdxBinOp
                        end
                     val explAST = checkExp (env, expl)
                     val expl_tyAST = AST.Exp.ty explAST
                     val (chk_binop_left_tyAST_res, chk_binop_right_tyAST) =
                        chk_binop_left_tyAST expl_tyAST
                     val () =
                        case chk_binop_left_tyAST_res of
                           NONE => ()
                         | SOME binop_left_tyAST =>
                              addErrorAt
                              (PT.Exp.span expl,
                               ["Left operand of '",
                                Layout.toString (PT.BinOp.layout binop),
                                "' not of type ",
                                toStringTypeAST env binop_left_tyAST, ".\n",
                                "  operand: ", toStringTypeAST env expl_tyAST, "\n",
                                "  in: ", toStringExp exp])
                     val exprAST = checkExp (env, expr)
                     val expr_tyAST = AST.Exp.ty exprAST
                     val (chk_binop_right_tyAST_res, (binopAST, binop_res_tyAST)) =
                        chk_binop_right_tyAST expr_tyAST
                     val () =
                        case chk_binop_right_tyAST_res of
                           NONE => ()
                         | SOME binop_right_tyAST =>
                              addErrorAt
                              (PT.Exp.span expr,
                               ["Right operand of '",
                                Layout.toString (PT.BinOp.layout binop),
                                "' not of type ",
                                toStringTypeAST env binop_right_tyAST, ".\n",
                                "  operand: ", toStringTypeAST env expr_tyAST, "\n",
                                "  in: ", toStringExp exp])
                  in
                     AST.Exp.make
                     (AST.Exp.E_BinOp (binopAST, explAST, exprAST),
                      binop_res_tyAST)
                  end
             | PT.Exp.E_UnOp (unop, expo) =>
                  let
                     val chk_unop_oper_tyAST =
                        let
                           fun integerArithUnOp unop =
                              fn expo_tyAST =>
                              (if AST.Type.equals (expo_tyAST, AST.Type.integer)
                                  then NONE
                               else SOME AST.Type.integer,
                               (unop, AST.Type.integer))
                           val arrayLenUnOp =
                              fn expo_tyAST =>
                              let
                                 fun mkRes tyargAST =
                                    (AST.UnOp.Len tyargAST, AST.Type.integer)
                                 fun mkResErr tyargAST =
                                    (SOME (AST.Type.array tyargAST),
                                     mkRes tyargAST)
                              in
                                 case expo_tyAST of
                                    AST.Type.T_TyCon (tyconAST, actual_tyargsAST) =>
                                       if AST.TyCon.equals (tyconAST, AST.TyCon.array)
                                          then 
                                             case actual_tyargsAST of
                                                [] => mkResErr bogusTyAST
                                              | [tyargAST] => (NONE, mkRes tyargAST)
                                              | tyargAST::_ => mkResErr tyargAST
                                       else mkResErr bogusTyAST
                                  | _ => mkResErr bogusTyAST
                              end
                        in
                           case unop of
                              PT.UnOp.Neg => integerArithUnOp AST.UnOp.Neg
                            | PT.UnOp.Len => arrayLenUnOp
                        end
                     val expoAST = checkExp (env, expo)
                     val expo_tyAST = AST.Exp.ty expoAST
                     val (chk_unop_oper_tyAST_res, (unopAST, unop_res_tyAST)) =
                        chk_unop_oper_tyAST expo_tyAST
                     val () =
                        case chk_unop_oper_tyAST_res of
                           NONE => ()
                         | SOME unop_oper_tyAST =>
                              addErrorAt
                              (PT.Exp.span expo,
                               ["Operand of '",
                                Layout.toString (PT.UnOp.layout unop),
                                "' not of type ",
                                toStringTypeAST env unop_oper_tyAST, ".\n",
                                "  operand: ", toStringTypeAST env expo_tyAST, "\n",
                                "  in: ", toStringExp exp])
                  in
                     AST.Exp.make
                     (AST.Exp.E_UnOp (unopAST, expoAST),
                      unop_res_tyAST)
                  end
             | PT.Exp.E_DaCon (dacon, actual_tyargs, actual_args) =>
                  let
                     val actual_tyargsAST = checkTypes (env, actual_tyargs)
                     fun finish_actual_tyargsAST arity =
                        fixArity
                        (actual_tyargsAST,
                         arity,
                         fn _ => AST.Type.unit,
                         fn {given, takes} =>
                         addErrorAt
                         (PT.DaConName.span dacon,
                          ["Data constructor ",
                           toStringDaConName dacon,
                           " given ", given,
                           " type arguments but takes ", takes,
                           " type arguments.\n",
                           "  in: ", toStringExp exp]))
                     val actual_argsAST = checkExps (env, actual_args)
                     fun finish_actual_argsAST formal_arg_tysAST =
                        ListPair.app
                        (fn ((actual_arg,actual_argAST), formal_arg_tyAST_opt) =>
                         let
                            val actual_arg_tyAST = AST.Exp.ty actual_argAST
                         in
                            case formal_arg_tyAST_opt of
                               NONE => ()
                             | SOME formal_arg_tyAST =>
                                  if AST.Type.equals (actual_arg_tyAST, formal_arg_tyAST)
                                     then ()
                                  else addErrorAt
                                       (PT.Exp.span actual_arg,
                                        ["Argument type and expression of data constructor ", toStringDaConName dacon, " disagree.\n",
                                         "  arg type:   ",
                                         toStringTypeAST env formal_arg_tyAST, "\n",
                                         "  expression: ",
                                         toStringTypeAST env actual_arg_tyAST, "\n",
                                         "  in arg: ", toStringExp actual_arg, "\n",
                                         "  in: ", toStringExp exp])
                         end)
                        (ListPair.zip (actual_args, actual_argsAST),
                         formal_arg_tysAST)
                  in
                     case Env.lookupDaCon (env, dacon) of
                        NONE =>
                           (addErrorAt
                            (PT.DaConName.span dacon,
                             ["Unbound data constructor: ",
                              toStringDaConName dacon, ".\n",
                              "  in: ", toStringExp exp]);
                            AST.Exp.unit)
                       | SOME {dacon = daconAST, tyvars = formal_tyvarsAST,
                               arg_tys = formal_arg_tysAST, tycon = tyconAST} =>
                           let
                              val actual_tyargsAST =
                                 finish_actual_tyargsAST (List.length formal_tyvarsAST)
                              val actual_tyargsAST__formal_tyvarsAST =
                                 ListPair.zip (actual_tyargsAST, formal_tyvarsAST)
                              val formal_arg_tysAST =
                                 List.map (fn formal_arg_tyAST =>
                                           SOME (AST.Type.substs (formal_arg_tyAST,
                                                                  actual_tyargsAST__formal_tyvarsAST)))
                                          formal_arg_tysAST
                              val formal_arg_tysAST =
                                 fixArity
                                 (formal_arg_tysAST,
                                  List.length actual_argsAST,
                                  fn _ => NONE,
                                  fn {given, takes} =>
                                  addErrorAt
                                  (PT.Exp.span exp,
                                   ["Data constructor ",
                                    toStringDaConName dacon,
                                    " given ", takes,
                                    " arguments but takes ", given,
                                    " arguments.\n",
                                    "  in: ", toStringExp exp]))
                              val () =
                                 finish_actual_argsAST formal_arg_tysAST
                              val res_tyAST =
                                 AST.Type.T_TyCon (tyconAST, actual_tyargsAST)
                           in
                              AST.Exp.make
                              (AST.Exp.E_DaCon (daconAST,
                                                actual_tyargsAST,
                                                actual_argsAST),
                               res_tyAST)
                           end
                  end
             | PT.Exp.E_Apply (func, applyarg) =>
                  let
                     val funcAST = checkExp (env, func)
                     val func_tyAST = AST.Exp.ty funcAST
                     val applyargAST = checkApplyArg (env, applyarg)
                  in
                     case func_tyAST of
                        AST.Type.T_TyFn (formal_tyargAST, res_tyAST) =>
                           let
                              val actual_tyAST =
                                 case applyargAST of
                                    AST.ApplyArg.A_Type actual_tyAST => actual_tyAST
                                  | AST.ApplyArg.A_Exp argAST =>
                                       (addErrorAt
                                        (PT.Exp.span exp,
                                         ["Argument of apply not a type.\n",
                                          "  function: ", toStringTypeAST env func_tyAST, "\n",
                                          "  in: ", toStringExp exp]);
                                        AST.Type.unit)
                           in
                              AST.Exp.make
                              (AST.Exp.E_Apply (funcAST, applyargAST),
                               AST.Type.subst (res_tyAST, (actual_tyAST, formal_tyargAST)))
                           end
                      | AST.Type.T_Fn (formal_arg_tyAST, res_tyAST) =>
                           let
                              val () =
                                 case applyargAST of
                                    AST.ApplyArg.A_Exp actual_argAST =>
                                       let
                                          val actual_arg_tyAST = AST.Exp.ty actual_argAST
                                       in
                                          if AST.Type.equals (actual_arg_tyAST, formal_arg_tyAST)
                                             then ()
                                          else addErrorAt
                                               (PT.Exp.span exp,
                                                ["Function and argument of apply disagree.\n",
                                                 "  function: ", toStringTypeAST env func_tyAST, "\n",
                                                 "  argument: ", toStringTypeAST env actual_arg_tyAST, "\n",
                                                 "  in: ", toStringExp exp])
                                       end
                                  | AST.ApplyArg.A_Type tyAST =>
                                       addErrorAt
                                       (PT.Exp.span exp,
                                        ["Argument of apply not an expression.\n",
                                         "  function: ", toStringTypeAST env func_tyAST, "\n",
                                         "  in: ", toStringExp exp])
                           in
                              AST.Exp.make
                              (AST.Exp.E_Apply (funcAST, applyargAST),
                               res_tyAST)
                           end
                      | _ =>
                           (addErrorAt
                            (PT.Exp.span exp,
                             ["Function of apply not of function type.\n",
                              "  function: ", toStringTypeAST env func_tyAST, "\n",
                              "  in: ", toStringExp exp]);
                            AST.Exp.make
                            (AST.Exp.E_Apply (funcAST, applyargAST),
                             AST.Type.unit))
                  end
             | PT.Exp.E_VarName var =>
                  (case Env.lookupVar (env, var) of
                      NONE =>
                         (addErrorAt
                          (PT.VarName.span var,
                           ["Unbound variable: ",
                            toStringVarName var, ".\n",
                            "  in: ", toStringExp exp]);
                          AST.Exp.unit)
                    | SOME {var = varAST, var_ty = var_tyAST} =>
                         AST.Exp.make
                         (AST.Exp.E_Var varAST,
                          var_tyAST))
             | PT.Exp.E_Integer i =>
                  let
                     val () =
                        if i < minInt
                           then addErrorAt
                                (PT.Exp.span exp,
                                 ["Integer constant too small: ",
                                  IntInf.toString i, ".\n",
                                  "  in: ", toStringExp exp])
                        else if i > maxInt
                           then addErrorAt
                                (PT.Exp.span exp,
                                 ["Integer constant too big: ",
                                  IntInf.toString i, ".\n",
                                  "  in: ", toStringExp exp])
                        else ()
                  in
                     AST.Exp.make
                     (AST.Exp.E_Integer i,
                      AST.Type.integer)
                  end
             | PT.Exp.E_String s =>
                  AST.Exp.make
                  (AST.Exp.E_String s,
                   AST.Type.string)
             | PT.Exp.E_Seq exps =>
                  let
                     val expsAST = checkExps (env, exps)
                  in
                     AST.Exp.make
                     (AST.Exp.E_Seq expsAST,
                      case List.rev expsAST of
                         nil => AST.Type.unit
                       | expAST::_ => AST.Exp.ty expAST)
                  end
             | PT.Exp.E_Let (decls, bodyExps) =>
                  let
                     val (env', declsAST) =
                        checkDecls (env, decls)
                     val bodyExpsAST = checkExps (Env.extend (env, env'), bodyExps)
                     val bodyExps_tyAST =
                        case List.rev bodyExpsAST of
                           nil => AST.Type.unit
                         | bodyExpAST::_ => AST.Exp.ty bodyExpAST
                     val () =
                        AST.TyCon.Set.app
                        (fn tyconAST =>
                         addErrorAt
                         (PT.Exp.span exp,
                          ["Type constructor escapes the scope of its definition.\n",
                           "  type constructor: ", toStringTyConAST (Env.extend (env, env')) tyconAST, "\n",
                           "  in: ", toStringExp exp]))
                        (AST.TyCon.Set.difference
                         (AST.Type.tycons bodyExps_tyAST,
                          Env.tycons env))
                  in
                     AST.Exp.make
                     (AST.Exp.E_Let (declsAST, bodyExpsAST),
                      bodyExps_tyAST)
                  end
             | PT.Exp.E_Case (arg, matchrules) =>
                  let
                     val argAST = checkExp (env, arg)
                     val arg_tyAST = AST.Exp.ty argAST
                     val setDC =
                        case arg_tyAST of
                           AST.Type.T_TyCon (tyconAST, _) =>
                              (case Env.lookupTyConRev (env, tyconAST) of
                                  NONE =>
                                     (addErrorAt
                                      (PT.Exp.span exp,
                                       ["Unbound type constructor: ",
                                        toStringTyConAST env tyconAST, ".\n",
                                        "  in: ", toStringTypeAST env arg_tyAST, "\n",
                                        "  in: ", toStringExp exp]);
                                      Infinite)
                                | SOME {dacons, ...} =>
                                     if List.null dacons
                                        then Infinite
                                     else Finite (AST.DaCon.Set.fromList dacons))
                         | _ => Infinite
                     val (res_tyAST, matchrulesAST, setDC) =
                        checkMatchRules (env, arg_tyAST,
                                         matchrules,
                                         setDC,
                                         fn () => ["  in: ", toStringExp exp])
                     val () =
                        case setDC of
                           Infinite =>
                              addErrorAt
                              (PT.Exp.span exp,
                               ["Non-exhaustive match rules in 'case'.\n",
                                "  in: ", toStringExp exp])
                         | Finite setDC =>
                              if AST.DaCon.Set.isEmpty setDC
                                 then ()
                              else let
                                      val missing =
                                         String.concatWith ", "
                                                           (List.map (toStringDaConAST env)
                                                                     (AST.DaCon.Set.listItems setDC))
                                   in
                                      addErrorAt
                                      (PT.Exp.span exp,
                                       ["Non-exhaustive match rules in 'case'.\n",
                                        "  missing: ", missing, "\n",
                                        "  in: ", toStringExp exp])
                                   end
                         | Empty => ()

                  in
                     AST.Exp.make
                     (AST.Exp.E_Case (argAST, matchrulesAST),
                      res_tyAST)
                  end
         and checkExps (env: Env.t, exps: PT.Exp.t list)
                       : AST.Exp.t list =
            List.map (fn exp => checkExp (env, exp)) exps
         and checkApplyArg (env: Env.t, applyarg: PT.ApplyArg.t)
                           : AST.ApplyArg.t =
            case PT.ApplyArg.node applyarg of
               PT.ApplyArg.A_Exp exp =>
                  AST.ApplyArg.A_Exp (checkExp (env, exp))
             | PT.ApplyArg.A_Type ty =>
                  AST.ApplyArg.A_Type (checkType (env, ty))
         and checkMatchRule (env: Env.t, arg_tyAST: AST.Type.t,
                             matchrule: PT.MatchRule.t,
                             setDC: u,
                             mkExtraMsgs: unit -> string list)
                            : AST.Type.t * AST.MatchRule.t * u =
            case PT.MatchRule.node matchrule of
               (pat, body) =>
                  let
                     val (env', patAST, setDC) =
                        checkPat (env, arg_tyAST, pat,
                                  setDC,
                                  fn () =>
                                  ["  in rule: ", toStringMatchRule matchrule, "\n"]
                                  @ (mkExtraMsgs ()))
                     val bodyAST = checkExp (Env.extend (env, env'), body)
                     val body_tyAST = AST.Exp.ty bodyAST
                  in
                     (body_tyAST,
                      AST.MatchRule.MatchRule (patAST, bodyAST),
                      setDC)
                  end
         and checkMatchRules (env: Env.t,
                              arg_tyAST: AST.Type.t,
                              matchrules: PT.MatchRule.t list,
                              setDC: u,
                              mkExtraMsgs: unit -> string list)
                             : AST.Type.t * AST.MatchRule.t list * u =
            let
               val (matchrulesAST, (prev_res_tyAST, setDC)) =
                  ListExtra.mapAndFoldl
                  (fn (matchrule, (prev_res_tyAST, setDC)) =>
                   let
                      val (res_tyAST, matchruleAST, setDC) =
                         checkMatchRule (env, arg_tyAST,
                                         matchrule,
                                         setDC,
                                         mkExtraMsgs)
                      val () =
                         case prev_res_tyAST of
                            NONE => ()
                          | SOME prev_res_tyAST =>
                               if AST.Type.equals (res_tyAST, prev_res_tyAST)
                                  then ()
                               else addErrorAt
                                    (PT.MatchRule.span matchrule,
                                     ["Result types of match rules in 'case' disagree.\n",
                                      "  previous result type: ",
                                      toStringTypeAST env prev_res_tyAST, "\n",
                                      "  this result type:     ",
                                      toStringTypeAST env res_tyAST, "\n",
                                      "  in rule: ", toStringMatchRule matchrule, "\n"]
                                     @ (mkExtraMsgs ()))
                   in
                      (matchruleAST,
                       (SOME res_tyAST, setDC))
                   end)
                   (NONE, setDC)
                   matchrules
               val res_tyAST =
                  case prev_res_tyAST of
                     NONE => AST.Type.unit
                   | SOME res_tyAST => res_tyAST
            in
               (res_tyAST, matchrulesAST, setDC)
            end
         and checkDecl (env: Env.t, decl: PT.Decl.t) :
                       Env.t * AST.Decl.t option =
            case PT.Decl.node decl of
               PT.Decl.D_Type (tycon, tyvars, ty) =>
                  let
                     val (tyvarsAST, envTV) =
                        ListExtra.mapAndFoldl
                        (fn (tyvar, envTV) =>
                         let
                            val tyvarAST =
                               AST.TyVar.new (toStringTyVarName tyvar)
                            val envTV_i =
                               Env.singletonTyVarWithRev (tyvar, {tyvar = tyvarAST})
                            val () =
                               case Env.lookupTyVar (envTV, tyvar) of
                                  NONE => ()
                                | SOME _ =>
                                     addErrorAt
                                     (PT.TyVarName.span tyvar,
                                      ["Type parameter of 'type' defined multiple times.\n",
                                       "  type parameter: ", toStringTyVarName tyvar, "\n",
                                       "  type constructor: ", toStringTyConName tycon, "\n",
                                       "  in: ", toStringDecl decl])
                         in
                            (tyvarAST,
                             Env.extend (envTV, envTV_i))
                         end)
                        Env.empty
                        tyvars
                     val tyAST = checkType (Env.extend (env, envTV), ty)
                     val env' =
                        Env.singletonTyCon
                        (tycon,
                         Env.TyConEnv.Type {tyvars = tyvarsAST,
                                            ty = tyAST})
                  in
                     (env', NONE)
                  end
             | PT.Decl.D_Data data_decls =>
                  let
                     val (data_declsAux, envTC) =
                        ListExtra.mapAndFoldl
                        (fn ((tycon, tyvars, dacon_decls), envTC) =>
                         let
                            val tyconAST = AST.TyCon.new (toStringTyConName tycon)
                            val arity = List.length tyvars
                            val (tyvarsAST, envTV) =
                               ListExtra.mapAndFoldl
                               (fn (tyvar, envTV) =>
                                let
                                   val tyvarAST = AST.TyVar.new (toStringTyVarName tyvar)
                                   val envTV_i =
                                      Env.singletonTyVarWithRev (tyvar, {tyvar = tyvarAST})
                                   val () =
                                      case Env.lookupTyVar (envTV, tyvar) of
                                         NONE => ()
                                       | SOME _ =>
                                            addErrorAt
                                            (PT.TyVarName.span tyvar,
                                             ["Type parameter of 'datatype' defined multiple times.\n",
                                              "  type parameter: ", toStringTyVarName tyvar, "\n",
                                              "  type constructor: ", toStringTyConName tycon, "\n",
                                              "  in: ", toStringDecl decl])
                                in
                                   (tyvarAST,
                                    Env.extend (envTV, envTV_i))
                                end)
                               Env.empty
                               tyvars
                            val envTC_i =
                               Env.singletonTyConDataWithRev
                               (tycon, {tycon = tyconAST}, {arity = arity, dacons = []})
                            val () =
                               case Env.lookupTyCon (envTC, tycon) of
                                  NONE => ()
                                | SOME _ =>
                                     addErrorAt
                                     (PT.TyConName.span tycon,
                                      ["Type constructor of 'datatype' defined multiple times.\n",
                                       "  type constructor: ", toStringTyConName tycon, "\n",
                                       "  in: ", toStringDecl decl])
                         in
                            ((tycon, tyconAST, tyvarsAST, envTV, dacon_decls),
                             Env.extend (envTC, envTC_i))
                         end)
                        Env.empty
                        data_decls
                     val (data_declsAST, (env', setDC)) =
                        ListExtra.mapAndFoldl
                        (fn ((tycon, tyconAST, tyvarsAST, envTV, dacon_decls), (env', setDC)) =>
                         let
                            val arity = List.length tyvarsAST
                            val (envDC_i, dacon_declsAST, setDC) =
                               checkDaConDecls (Env.extend (env, (Env.extend (envTC, envTV))),
                                                tyvarsAST, tyconAST,
                                                dacon_decls,
                                                setDC,
                                                fn () =>
                                                ["  in: ", toStringDecl decl])
                            val daconsAST =
                               List.map (fn (daconAST, _) => daconAST) dacon_declsAST
                            val envTC_i =
                               Env.singletonTyConDataWithRev
                               (tycon, {tycon = tyconAST}, {arity = arity, dacons = daconsAST})
                         in
                            ((tyconAST, tyvarsAST, dacon_declsAST),
                             (Env.extend (env', Env.extend (envTC_i, envDC_i)),
                              setDC))
                         end)
                        (Env.empty, PT.DaConName.Set.empty)
                        data_declsAux
                  in
                     (env',
                      SOME (AST.Decl.D_Data data_declsAST))
                  end
             | PT.Decl.D_Val (pat, constr_tyOpt, exp) =>
                  let
                     val expAST = checkExp (env, exp)
                     val exp_tyAST = AST.Exp.ty expAST
                     val arg_tyAST =
                        case constr_tyOpt of
                           NONE => exp_tyAST
                         | SOME constr_ty =>
                              let
                                 val constr_tyAST = checkType (env, constr_ty)
                                 val () =
                                    if AST.Type.equals (exp_tyAST, constr_tyAST)
                                       then ()
                                    else addErrorAt
                                         (PT.Decl.span decl,
                                          ["Constraint and expression of 'val' disagree.\n",
                                           "  constraint: ", toStringTypeAST env constr_tyAST, "\n",
                                           "  expression: ", toStringTypeAST env exp_tyAST, "\n",
                                           "  in: ", toStringDecl decl])
                              in
                                 constr_tyAST
                              end
                     val (env', patAST, _) =
                        checkSimplePat (env, arg_tyAST, pat,
                                        PT.VarName.Set.empty,
                                        fn () => [])
                  in
                     (env',
                      SOME (AST.Decl.D_Val (patAST, expAST)))
                  end
             | PT.Decl.D_Fun fun_decls =>
                  let
                     val (fun_declsAux, env') =
                        ListExtra.mapAndFoldl
                        (fn ((var, params, res_ty, body), env') =>
                         let
                            val varAST = AST.Var.new (toStringVarName var)
                            val (envP, mkTy, paramsAST) =
                               checkParams (env, params)
                            val res_tyAST =
                               checkType (Env.extend (env, envP), res_ty)
                            val fun_tyAST = mkTy res_tyAST
                            val envF = Env.singletonVar (var, {var_ty = fun_tyAST,
                                                               var = varAST})
                            val () =
                               case Env.lookupVar (env', var) of
                                  NONE => ()
                                | SOME _ =>
                                     addErrorAt
                                     (PT.VarName.span var,
                                      ["Function variable of 'fun' defined multiple times.\n",
                                       "  variable: ", toStringVarName var, "\n",
                                       "  in: ", toStringDecl decl])
                         in
                            ((varAST, envP, paramsAST, res_tyAST, body),
                             Env.extend (env', envF))
                         end)
                        Env.empty
                        fun_decls
                     val fun_declsAST =
                        List.map
                        (fn (varAST, envP, paramsAST, res_tyAST, body) =>
                         let
                            val body_env =
                               Env.extend (env, Env.extend (env', envP))
                            val bodyAST = checkExp (body_env, body)
                            val body_tyAST = AST.Exp.ty bodyAST
                            val () =
                               if AST.Type.equals (body_tyAST, res_tyAST)
                                  then ()
                               else addErrorAt
                                    (PT.Decl.span decl,
                                     ["Result type and expression of 'fun' disagree.\n",
                                      "  result type: ", toStringTypeAST body_env res_tyAST, "\n",
                                      "  expression:  ", toStringTypeAST body_env body_tyAST, "\n",
                                      "  in: ", toStringDecl decl])
                         in
                            (varAST, paramsAST, res_tyAST, bodyAST)
                         end)
                        fun_declsAux
                  in
                     (env', SOME (AST.Decl.D_Fun fun_declsAST))
                  end
         and checkDecls (env: Env.t, decls: PT.Decl.t list)
                        : Env.t * AST.Decl.t list =
            let
               val (declsAST, env') =
                  ListExtra.mapAndFoldl
                  (fn (decl_i, env') =>
                   let
                      val (env'_i, declAST_i) =
                         checkDecl (Env.extend (env, env'), decl_i)
                   in
                      (declAST_i,
                       Env.extend (env', env'_i))
                   end)
                  Env.empty
                  decls
            in
               (env',
                List.mapPartial (fn declAST => declAST) declsAST)
            end

         fun checkProg (prog: PT.Prog.t) : AST.Prog.t option =
            let
               val env0 = Env.initial
               val env0 = 
                  Env.extend 
                  (env0, 
                   Env.singletonTyVarWithRev
                   (bogusTyVar, {tyvar = bogusTyVarAST}))
               val (decls, exp) = PT.Prog.node prog
               val (env', declsAST) = checkDecls (env0, decls)
               val expAST = checkExp (Env.extend (env0, env'), exp)
               val progAST = AST.Prog.Prog (declsAST, expAST)
            in
               SOME progAST
            end
            handle TypeError => NONE
      in
         checkProg prog
      end

end
