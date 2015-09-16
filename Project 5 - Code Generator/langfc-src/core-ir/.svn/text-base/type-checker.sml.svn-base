(* langfc-src/core-ir/type-checker.sml
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
 * Type-checker for core intermediate representation.
 *)

structure CoreIRTypeChecker :> CORE_IR_TYPE_CHECKER =
struct
   structure C = CoreIR

   structure Env =
      struct
         structure TyVarEnv =
            struct
               type dom = C.TyVar.t
               type cod = unit
               type t = cod C.TyVar.Map.map
               val empty : t = C.TyVar.Map.empty
               val singleton : dom * cod -> t = C.TyVar.Map.singleton
               val lookup : t * dom -> cod option = C.TyVar.Map.find
               val extend : t * t -> t = C.TyVar.Map.unionWith #2
               val domain : t -> C.TyVar.Set.set =
                  C.TyVar.Set.fromList o C.TyVar.Map.listKeys
            end
         structure TyConEnv =
            struct
               type dom = C.TyCon.t
               type cod = {arity: int,
                           dacons: C.DaCon.t list}
               type t = cod C.TyCon.Map.map
               val empty : t = C.TyCon.Map.empty
               val singleton : dom * cod -> t = C.TyCon.Map.singleton
               val lookup : t * dom -> cod option = C.TyCon.Map.find
               val extend : t * t -> t = C.TyCon.Map.unionWith #2
               val domain : t -> C.TyCon.Set.set =
                  C.TyCon.Set.fromList o C.TyCon.Map.listKeys
            end
         structure DaConEnv =
            struct
               type dom = C.DaCon.t
               type cod = {tyvars: C.TyVar.t list,
                           arg_tys: C.Type.t list,
                           tycon: C.TyCon.t}
               type t = cod C.DaCon.Map.map
               val empty : t = C.DaCon.Map.empty
               val singleton : dom * cod -> t = C.DaCon.Map.singleton
               val lookup : t * dom -> cod option = C.DaCon.Map.find
               val extend : t * t -> t = C.DaCon.Map.unionWith #2
               val domain : t -> C.DaCon.Set.set =
                  C.DaCon.Set.fromList o C.DaCon.Map.listKeys
            end
         structure VarEnv =
            struct
               type dom = C.Var.t
               type cod = {var_ty: C.Type.t}
               type t = cod C.Var.Map.map
               val empty : t = C.Var.Map.empty
               val singleton : dom * cod -> t = C.Var.Map.singleton
               val lookup : t * dom -> cod option = C.Var.Map.find
               val extend : t * t -> t = C.Var.Map.unionWith #2
               val domain : t -> C.Var.Set.set =
                  C.Var.Set.fromList o C.Var.Map.listKeys
            end

         datatype t =
            Env of {tyvarEnv: TyVarEnv.t,
                    tyconEnv: TyConEnv.t,
                    daconEnv: DaConEnv.t,
                    varEnv: VarEnv.t}

         val empty =
            Env {tyvarEnv = TyVarEnv.empty,
                 tyconEnv = TyConEnv.empty,
                 daconEnv = DaConEnv.empty,
                 varEnv = VarEnv.empty}

         fun fromTyVarEnv tyvarEnv =
            Env {tyvarEnv = tyvarEnv,
                 tyconEnv = TyConEnv.empty,
                 daconEnv = DaConEnv.empty,
                 varEnv = VarEnv.empty}
         val singletonTyVar = fromTyVarEnv o TyVarEnv.singleton
         fun lookupTyVar (Env {tyvarEnv, ...}, tyvar) =
            TyVarEnv.lookup (tyvarEnv, tyvar)
         fun domainTyVar (Env {tyvarEnv, ...}) =
            TyVarEnv.domain tyvarEnv

         fun fromTyConEnv tyconEnv =
            Env {tyvarEnv = TyVarEnv.empty,
                 tyconEnv = tyconEnv,
                 daconEnv = DaConEnv.empty,
                 varEnv = VarEnv.empty}
         val singletonTyCon = fromTyConEnv o TyConEnv.singleton
         fun lookupTyCon (Env {tyconEnv, ...}, tycon) =
            TyConEnv.lookup (tyconEnv, tycon)
         fun domainTyCon (Env {tyconEnv, ...}) =
            TyConEnv.domain tyconEnv

         fun fromDaConEnv daconEnv =
            Env {tyvarEnv = TyVarEnv.empty,
                 tyconEnv = TyConEnv.empty,
                 daconEnv = daconEnv,
                 varEnv = VarEnv.empty}
         val singletonDaCon = fromDaConEnv o DaConEnv.singleton
         fun lookupDaCon (Env {daconEnv, ...}, dacon) =
            DaConEnv.lookup (daconEnv, dacon)
         fun domainDaCon (Env {daconEnv, ...}) =
            DaConEnv.domain daconEnv

         fun fromVarEnv varEnv =
            Env {tyvarEnv = TyVarEnv.empty,
                 tyconEnv = TyConEnv.empty,
                 daconEnv = DaConEnv.empty,
                 varEnv = varEnv}
         val singletonVar = fromVarEnv o VarEnv.singleton
         fun lookupVar (Env {varEnv, ...}, var) =
            VarEnv.lookup (varEnv, var)
         fun domainVar (Env {varEnv, ...}) =
            VarEnv.domain varEnv

         fun extend (Env {tyvarEnv = tyvarEnv1,
                          tyconEnv = tyconEnv1,
                          daconEnv = daconEnv1,
                          varEnv = varEnv1},
                     Env {tyvarEnv = tyvarEnv2,
                          tyconEnv = tyconEnv2,
                          daconEnv = daconEnv2,
                          varEnv = varEnv2}) =
            Env {tyvarEnv = TyVarEnv.extend (tyvarEnv1, tyvarEnv2),
                 tyconEnv = TyConEnv.extend (tyconEnv1, tyconEnv2),
                 daconEnv = DaConEnv.extend (daconEnv1, daconEnv2),
                 varEnv = VarEnv.extend (varEnv1, varEnv2)}
      end

   fun toStringTyVar tyvar =
      Layout.toString (C.TyVar.layout tyvar)
   fun toStringTyCon tycon =
      Layout.toString (C.TyCon.layout tycon)
   fun toStringDaCon dacon =
      Layout.toString (C.DaCon.layout dacon)
   fun toStringVar var =
      Layout.toString (C.Var.layout var)

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
         maybeElide (Layout.toString (C.Type.layout ty))
      fun toStringPat pat =
         maybeElide (Layout.toString (C.Pat.layout pat))
      fun toStringExp exp =
         maybeElide (Layout.toString (C.Exp.layout exp))
      fun toStringExpNode node =
         maybeElide (Layout.toString (C.Exp.layoutNode node))
      fun toStringLam lam =
         maybeElide (Layout.toString (C.Lam.layout lam))
      fun toStringMatchRule matchrule =
         maybeElide (Layout.toString (C.MatchRule.layout matchrule))
      fun toStringDecl decl =
         maybeElide (Layout.toString (C.Decl.layout decl))
      fun toStringProg prog =
         maybeElide (Layout.toString (C.Prog.layout prog))
   end

   fun typeCheck (prog : C.Prog.t) : unit =
      let
         exception TypeError
         fun raiseTypeError msgs =
            (List.map (fn msg => (TextIO.output (TextIO.stdErr, msg))) ("Type Error:\n" :: msgs)
             ; TextIO.output1 (TextIO.stdErr, #"\n")
             ; raise TypeError)

         fun chkArity (xs, arity, mkError) =
            let
               val len_xs = List.length xs
               fun err () = mkError {given = Int.toString len_xs,
                                     takes = Int.toString arity}
            in
               if arity < len_xs
                  then err ()
               else if arity > len_xs
                   then err ()
               else ()
            end

         fun checkType (env : Env.t, ty : C.Type.t)
                       : unit =
            case ty of
               C.Type.T_TyFn {tyvar, res} =>
                  let
                     val () =
                        checkType
                        (Env.extend
                         (env, Env.singletonTyVar (tyvar, ())),
                         res)
                  in
                     ()
                  end
             | C.Type.T_Fn {arg, res} =>
                  let
                     val () = checkType (env, arg)
                     val () = checkType (env, res)
                  in
                     ()
                  end
             | C.Type.T_TyCon {tycon, tyargs = actual_tyargs} =>
                  let
                     val () = List.app (fn actual_tyarg => checkType (env, actual_tyarg)) actual_tyargs
                     fun finish arity =
                        chkArity
                        (actual_tyargs,
                         arity,
                         fn {given, takes} =>
                         raiseTypeError
                         (["Type constructor ",
                           toStringTyCon tycon,
                           " given ", given,
                           " type arguments but takes ", takes,
                           " type arguments.\n",
                           "  in: ", toStringType ty]))
                  in
                     case Env.lookupTyCon (env, tycon) of
                        NONE =>
                           raiseTypeError
                           (["Unbound type constructor: ",
                             toStringTyCon tycon, ".\n",
                             "  in: ", toStringType ty])
                      | SOME {arity, ...} =>
                           let
                              val () = finish arity
                           in
                              ()
                           end
                  end
             | C.Type.T_TyVar {tyvar} =>
                  (case Env.lookupTyVar (env, tyvar) of
                      NONE =>
                         raiseTypeError
                         (["Unbound type variable: ",
                           toStringTyVar tyvar, ".\n",
                           "  in: ", toStringType ty])
                    | SOME () => ())

         fun checkParam (env: Env.t, param: C.Param.t)
                        : Env.t * (C.Type.t -> C.Type.t) =
            case param of
               C.Param.P_Var {var, var_ty} =>
                  let
                     val () = checkType (env, var_ty)
                     val env' =
                        Env.singletonVar (var, {var_ty = var_ty})
                     val mkTy = fn res_ty =>
                        C.Type.T_Fn {arg = var_ty, res = res_ty}
                  in
                     (env', mkTy)
                  end
             | C.Param.P_TyVar {tyvar} =>
                  let
                     val env' =
                        Env.singletonTyVar (tyvar, ())
                     val mkTy = fn res_ty =>
                        C.Type.T_TyFn {tyvar = tyvar, res = res_ty}
                  in
                     (env', mkTy)
                  end

         fun checkBind (env: Env.t, arg_ty: C.Type.t,
                        bind: {var: C.Var.t, var_ty: C.Type.t},
                        mkExtraMsgs: unit -> string list)
                       : Env.t =
            case bind of
               {var, var_ty} =>
                  let
                     val () = checkType (env, var_ty)
                     val () =
                        if C.Type.equals (arg_ty, var_ty)
                           then ()
                        else raiseTypeError
                             (["Variable type and argument type in binding disagree.\n",
                               "  variable: ", toStringVar var, "\n",
                               "  var type: ", toStringType var_ty, "\n",
                               "  arg type: ", toStringType arg_ty, "\n"]
                              @ (mkExtraMsgs ()))
                     val env' =
                        Env.singletonVar (var, {var_ty = var_ty})
                  in
                     env'
                  end

         fun checkPat (env: Env.t, arg_ty: C.Type.t, pat: C.Pat.t,
                       setDC: C.DaCon.Set.set,
                       mkExtraMsgs: unit -> string list)
                      : Env.t * C.DaCon.Set.set =
            case pat of
               C.Pat.P_DaCon {dacon,
                              tyargs = actual_tyargs,
                              binds = actual_binds} =>
                  let
                     val () = List.app (fn actual_tyarg => checkType (env, actual_tyarg)) actual_tyargs
                     fun finish_actual_tyargs arity =
                        chkArity
                        (actual_tyargs,
                         arity,
                         fn {given, takes} =>
                         raiseTypeError
                         (["Data constructor ",
                           toStringDaCon dacon,
                           " given ", given,
                           " type arguments but takes ", takes,
                           " type arguments.\n",
                           "  in pattern: ", toStringPat pat, "\n"]
                          @ (mkExtraMsgs ())))
                     fun finish_actual_binds formal_arg_tys =
                        let
                           val env' =
                              ListPair.foldl
                              (fn (actual_bind_i, formal_arg_ty_i, env') =>
                               let
                                  val env'_i =
                                     checkBind (env, formal_arg_ty_i,
                                                actual_bind_i,
                                                fn () =>
                                                ["  in pattern: ", toStringPat pat, "\n"]
                                                @ (mkExtraMsgs ()))
                               in
                                  Env.extend (env', env'_i)
                               end)
                               Env.empty
                               (actual_binds, formal_arg_tys)
                        in
                           env'
                        end
                  in
                     case Env.lookupDaCon (env, dacon) of
                        NONE =>
                           raiseTypeError
                           (["Unbound data constructor: ",
                             toStringDaCon dacon, ".\n",
                             "  in pattern: ", toStringPat pat, "\n"]
                            @ (mkExtraMsgs ()))
                      | SOME {tyvars = formal_tyvars,
                              arg_tys = formal_arg_tys,
                              tycon = tycon} =>
                           let
                              val () =
                                 finish_actual_tyargs
                                 (List.length formal_tyvars)
                              val formal_arg_tys =
                                 C.Type.substsL (formal_arg_tys,
                                                 ListPair.zip (actual_tyargs,
                                                               formal_tyvars))
                              val () =
                                 chkArity
                                 (formal_arg_tys,
                                  List.length actual_binds,
                                  fn {given, takes} =>
                                  raiseTypeError
                                  (["Data constructor ",
                                    toStringDaCon dacon,
                                    " given ", takes,
                                    " binds but takes ", given,
                                    " arguments.\n",
                                    "  in pattern: ", toStringPat pat, "\n"]
                                   @ (mkExtraMsgs ())))
                              val env' =
                                 finish_actual_binds formal_arg_tys
                              val ty =
                                 C.Type.T_TyCon
                                 {tycon = tycon,
                                  tyargs = actual_tyargs}
                              val () =
                                 if C.Type.equals (arg_ty, ty)
                                    then ()
                                 else raiseTypeError
                                      (["Argument and pattern in 'case' disagree.\n",
                                        "  argument: ", toStringType arg_ty, "\n",
                                        "  pattern:  ", toStringType ty, "\n"]
                                       @ (mkExtraMsgs ()))
                              val setDC =
                                 if C.DaCon.Set.member (setDC, dacon)
                                    then C.DaCon.Set.delete (setDC, dacon)
                                 else raiseTypeError
                                      (["Redundant pattern in 'case'.\n",
                                        "  pattern: ", toStringPat pat, "\n"]
                                       @ (mkExtraMsgs ()))
                           in
                              (env', setDC)
                           end
                  end
             | C.Pat.P_Var bind =>
                  let
                     val env' =
                        checkBind (env, arg_ty,
                                   bind,
                                   fn () => [])
                     val () =
                        if C.DaCon.Set.isEmpty setDC
                           then raiseTypeError
                                (["Redundant pattern in 'case'.\n",
                                  "  pattern: ", toStringPat pat, "\n"]
                                 @ (mkExtraMsgs ()))
                        else ()
                  in
                     (env', C.DaCon.Set.empty)
                  end

         fun checkDaConDecl (env: Env.t, tyvars: C.TyVar.t list, tycon: C.TyCon.t,
                             dacon_decl: {dacon: C.DaCon.t,
                                          arg_tys: C.Type.t list})
                            : Env.t =
            let
               val {dacon, arg_tys} = dacon_decl
               val () = List.app (fn arg_ty => checkType (env, arg_ty)) arg_tys
               val env' =
                  Env.singletonDaCon (dacon, {tyvars = tyvars,
                                              arg_tys = arg_tys,
                                              tycon = tycon})
            in
               env'
            end
         fun checkDaConDecls (env: Env.t, tyvars: C.TyVar.t list, tycon: C.TyCon.t,
                              dacon_decls: {dacon: C.DaCon.t,
                                            arg_tys: C.Type.t list} list)
                             : Env.t =
            let
               val env' =
                  List.foldl
                  (fn (dacon_decl_i, env') =>
                   let
                      val env'_i =
                         checkDaConDecl (env, tyvars, tycon,
                                         dacon_decl_i)
                   in
                      Env.extend (env', env'_i)
                   end)
                  Env.empty
                  dacon_decls
            in
               env'
            end

         fun checkExp (env: Env.t, exp: C.Exp.t)
                      : C.Type.t =
            case exp of
               C.Exp.Exp {node, ty} =>
                  let
                     val () = checkType (env, ty)
                     val node_ty = checkExpNode (env, node)
                     val () =
                        if C.Type.equals (node_ty, ty)
                           then ()
                        else raiseTypeError
                             (["Expression type and expression node disagree.\n",
                               "  expression type: ", toStringType node_ty, "\n",
                               "  expression node: ", toStringType ty, "\n",
                               "  in: ", toStringExp exp])
                  in
                     node_ty
                  end
         and checkExpNode (env: Env.t, node: C.Exp.node)
                          : C.Type.t =
            case node of
               C.Exp.E_Fn {lam} => checkLam (env, lam)
             | C.Exp.E_Prim {prim, tyargs = actual_tyargs, args = actual_args} =>
                  let
                     val {tyvars = formal_tyvars, arg_tys, res_ty} =
                        let
                           val integerBinOp =
                              {tyvars = [],
                               arg_tys = [C.Type.integer, C.Type.integer],
                               res_ty = C.Type.integer}
                           val integerUnOp =
                              {tyvars = [],
                               arg_tys = [C.Type.integer],
                               res_ty = C.Type.integer}
                           val integerCmp =
                              {tyvars = [],
                               arg_tys = [C.Type.integer, C.Type.integer],
                               res_ty = C.Type.bool}
                        in
                           case prim of
                              (* integer primitives *)
                              (* binary operations *)
                              C.Prim.Add => integerBinOp
                            | C.Prim.Sub => integerBinOp
                            | C.Prim.Mul => integerBinOp
                            | C.Prim.Div => integerBinOp
                            | C.Prim.Mod => integerBinOp
                            (* unary operations *)
                            | C.Prim.Neg => integerUnOp
                            (* comparision operations *)
                            | C.Prim.Eq => integerCmp
                            | C.Prim.NEq => integerCmp
                            | C.Prim.Lt => integerCmp
                            | C.Prim.Lte => integerCmp
                            | C.Prim.Gt => integerCmp
                            | C.Prim.Gte => integerCmp
                            (* string primitives *)
                            | C.Prim.Concat =>
                                 {tyvars = [],
                                  arg_tys = [C.Type.string, C.Type.string],
                                  res_ty = C.Type.string}
                            | C.Prim.Print =>
                                 {tyvars = [],
                                  arg_tys = [C.Type.string],
                                  res_ty = C.Type.unit}
                            | C.Prim.Size =>
                                 {tyvars = [],
                                  arg_tys = [C.Type.string],
                                  res_ty = C.Type.integer}
                            | C.Prim.Subscript =>
                                 {tyvars = [],
                                  arg_tys = [C.Type.string, C.Type.integer],
                                  res_ty = C.Type.integer}
                            (* array primitives *)
                            | C.Prim.Array =>
                                 let
                                    val tyvar = C.TyVar.new "'zzz"
                                    val ty = C.Type.T_TyVar {tyvar = tyvar}
                                 in
                                    {tyvars = [tyvar],
                                     arg_tys = [C.Type.integer, ty],
                                     res_ty = C.Type.array ty}
                                 end
                            | C.Prim.Len =>
                                 let
                                    val tyvar = C.TyVar.new "'zzz"
                                    val ty = C.Type.T_TyVar {tyvar = tyvar}
                                 in
                                    {tyvars = [tyvar],
                                     arg_tys = [C.Type.array ty],
                                     res_ty = C.Type.integer}
                                 end
                            | C.Prim.Idx =>
                                 let
                                    val tyvar = C.TyVar.new "'zzz"
                                    val ty = C.Type.T_TyVar {tyvar = tyvar}
                                 in
                                    {tyvars = [tyvar],
                                     arg_tys = [C.Type.array ty, C.Type.integer],
                                     res_ty = ty}
                                 end
                            | C.Prim.Upd =>
                                 let
                                    val tyvar = C.TyVar.new "'zzz"
                                    val ty = C.Type.T_TyVar {tyvar = tyvar}
                                 in
                                    {tyvars = [tyvar],
                                     arg_tys = [C.Type.array ty, C.Type.integer, ty],
                                     res_ty = ty}
                                 end
                            (* misc primitives *)
                            | C.Prim.Argc =>
                                 {tyvars = [],
                                  arg_tys = [C.Type.unit],
                                  res_ty = C.Type.integer}
                            | C.Prim.Arg =>
                                 {tyvars = [],
                                  arg_tys = [C.Type.integer],
                                  res_ty = C.Type.string}
                            | C.Prim.Fail =>
                                 let
                                    val tyvar = C.TyVar.new "'zzz"
                                 in
                                    {tyvars = [tyvar],
                                     arg_tys = [C.Type.string],
                                     res_ty = C.Type.T_TyVar {tyvar = tyvar}}
                                 end
                        end
                     val () = List.app (fn actual_tyarg => checkType (env, actual_tyarg)) actual_tyargs
                     val () =
                        chkArity
                        (actual_tyargs,
                         List.length formal_tyvars,
                         fn {given, takes} =>
                         raiseTypeError
                         (["Primitive ",
                           Layout.toString (C.Prim.layout prim),
                           " given ", given,
                           " type arguments but takes ", takes,
                           " type arguments.\n",
                           "  in: ", toStringExpNode node]))
                     val arg_tys =
                        C.Type.substsL (arg_tys,
                                        ListPair.zip (actual_tyargs,
                                                      formal_tyvars))
                     val () =
                        chkArity
                        (actual_args,
                         List.length arg_tys,
                         fn {given, takes} =>
                         raiseTypeError
                         (["Primitive ",
                           Layout.toString (C.Prim.layout prim),
                           " given ", given,
                           " arguments but takes ", takes,
                           " arguments.\n",
                           "  in: ", toStringExpNode node]))
                     val () =
                        ListPair.app
                        (fn (actual_arg, arg_ty) =>
                         let
                            val actual_arg_ty = checkExp (env, actual_arg)
                         in
                            if C.Type.equals (actual_arg_ty, arg_ty)
                               then ()
                            else raiseTypeError
                                 (["Argument type and expression of primitive ",
                                   Layout.toString (C.Prim.layout prim), " disagree.\n",
                                   "  arg type:   ",
                                   toStringType arg_ty, "\n",
                                   "  expression: ",
                                   toStringType actual_arg_ty, "\n",
                                   "  in arg: ", toStringExp actual_arg, "\n",
                                   "  in: ", toStringExpNode node])
                         end)
                        (actual_args, arg_tys)
                  in
                     C.Type.substs (res_ty,
                                    ListPair.zip (actual_tyargs,
                                                  formal_tyvars))
                  end
             | C.Exp.E_DaCon {dacon,
                              tyargs = actual_tyargs,
                              args = actual_args} =>
                  let
                     val () = List.app (fn actual_tyarg => checkType (env, actual_tyarg)) actual_tyargs
                     fun finish_actual_tyargs arity =
                        chkArity
                        (actual_tyargs,
                         arity,
                         fn {given, takes} =>
                         raiseTypeError
                         (["Data constructor ",
                           toStringDaCon dacon,
                           " given ", given,
                           " type arguments but takes ", takes,
                           " type arguments.\n",
                           "  in: ", toStringExpNode node]))
                     fun finish_actual_args formal_arg_tys =
                        ListPair.app
                        (fn (actual_arg, formal_arg_ty) =>
                         let
                            val actual_arg_ty = checkExp (env, actual_arg)
                         in
                            if C.Type.equals (actual_arg_ty, formal_arg_ty)
                               then ()
                            else raiseTypeError
                                 (["Argument type and expression of data constructor ",
                                   toStringDaCon dacon, " disagree.\n",
                                   "  arg type:   ",
                                   toStringType formal_arg_ty, "\n",
                                   "  expression: ",
                                   toStringType actual_arg_ty, "\n",
                                   "  in arg: ", toStringExp actual_arg, "\n",
                                   "  in: ", toStringExpNode node])
                         end)
                        (actual_args, formal_arg_tys)
                  in
                     case Env.lookupDaCon (env, dacon) of
                        NONE =>
                           raiseTypeError
                           (["Unbound data constructor: ",
                             toStringDaCon dacon, ".\n",
                             "  in: ", toStringExpNode node])
                       | SOME {tyvars = formal_tyvars,
                               arg_tys = formal_arg_tys,
                               tycon = tycon} =>
                           let
                              val () =
                                 finish_actual_tyargs
                                 (List.length formal_tyvars)
                              val formal_arg_tys =
                                 C.Type.substsL (formal_arg_tys,
                                                 ListPair.zip (actual_tyargs,
                                                               formal_tyvars))
                              val () =
                                 chkArity
                                 (formal_arg_tys,
                                  List.length actual_args,
                                  fn {given, takes} =>
                                  raiseTypeError
                                  (["Data constructor ",
                                    toStringDaCon dacon,
                                    " given ", takes,
                                    " arguments but takes ", given,
                                    " arguments.\n",
                                    "  in: ", toStringExpNode node]))
                              val () =
                                 finish_actual_args formal_arg_tys
                           in
                              C.Type.T_TyCon {tycon = tycon,
                                              tyargs = actual_tyargs}
                           end
                  end
             | C.Exp.E_Apply {func, applyarg} =>
                  let
                     val func_ty = checkExp (env, func)
                  in
                     case func_ty of
                        C.Type.T_TyFn {tyvar = formal_tyarg, res = res_ty} =>
                           (case applyarg of
                               C.ApplyArg.A_Type actual_ty =>
                                  let
                                     val () = checkType (env, actual_ty)
                                  in
                                     C.Type.subst (res_ty, (actual_ty, formal_tyarg))
                                  end
                             | _ =>
                                  raiseTypeError
                                  (["Argument of apply not a type.\n",
                                    "  function: ", toStringType func_ty, "\n",
                                    "  in: ", toStringExpNode node]))
                      | C.Type.T_Fn {arg = formal_arg_ty, res = res_ty} =>
                           (case applyarg of
                               C.ApplyArg.A_Exp actual_arg =>
                                  let
                                     val actual_arg_ty =
                                        checkExp (env, actual_arg)
                                     val () =
                                        if C.Type.equals (actual_arg_ty, formal_arg_ty)
                                           then ()
                                        else raiseTypeError
                                             (["Function and argument of apply disagree.\n",
                                               "  function: ", toStringType func_ty, "\n",
                                               "  argument: ", toStringType actual_arg_ty, "\n",
                                               "  in: ", toStringExpNode node])
                                  in
                                     res_ty
                                  end
                             | _ =>
                                  raiseTypeError
                                  (["Argument of apply not an expression.\n",
                                    "  function: ", toStringType func_ty, "\n",
                                    "  in: ", toStringExpNode node]))
                      | _ =>
                           raiseTypeError
                           (["Function of apply not of function type.\n",
                             "  function: ", toStringType func_ty, "\n",
                             "  in: ", toStringExpNode node])
                  end
             | C.Exp.E_Var {var} =>
                  (case Env.lookupVar (env, var) of
                      NONE =>
                         raiseTypeError
                         (["Unbound variable: ",
                           toStringVar var, ".\n",
                           "  in: ", toStringExpNode node])
                    | SOME {var_ty} => var_ty)
             | C.Exp.E_Integer i => C.Type.integer
             | C.Exp.E_String s => C.Type.string
             | C.Exp.E_Let {decl, body} =>
                  let
                     val env' = checkDecl (env, decl)
                     val body_ty = checkExp (Env.extend (env, env'), body)
                  in
                     body_ty
                  end
             | C.Exp.E_Case {arg, matchrules} =>
                  let
                     val arg_ty = checkExp (env, arg)
                     val setDC =
                        case arg_ty of
                           C.Type.T_TyCon {tycon, ...} =>
                              (case Env.lookupTyCon (env, tycon) of
                                  NONE =>
                                     raiseTypeError
                                     (["Unbound type constructor: ",
                                       toStringTyCon tycon, ".\n",
                                       "  in: ", toStringType arg_ty, "\n",
                                       "  in: ", toStringExpNode node])
                                | SOME {dacons, ...} =>
                                     C.DaCon.Set.fromList dacons)
                         | _ =>
                              raiseTypeError
                              (["Argument type not type constructor in 'case'.\n",
                                "  arg type: ", toStringType arg_ty, "\n",
                                "  in: ", toStringExpNode node])
                     val res_ty =
                        checkMatchRules (env, arg_ty,
                                         matchrules,
                                         setDC,
                                         fn () => ["  in: ", toStringExpNode node])
                  in
                     res_ty
                  end
         and checkLam (env: Env.t, lam: C.Lam.t)
                      : C.Type.t =
            case lam of
               C.Lam.Lam {param, body} =>
                  let
                     val (env', mkTy) = checkParam (env, param)
                     val body_ty = checkExp (Env.extend (env, env'), body)
                  in
                     mkTy body_ty
                  end
         and checkMatchRule (env: Env.t, arg_ty: C.Type.t,
                             matchrule: C.MatchRule.t,
                             setDC: C.DaCon.Set.set,
                             mkExtraMsgs: unit -> string list)
                            : C.Type.t * C.DaCon.Set.set =
            case matchrule of
               C.MatchRule.MatchRule {pat, body} =>
                  let
                     val (env', setDC) =
                        checkPat (env, arg_ty, pat,
                                  setDC,
                                  fn () =>
                                  ["  in rule: ", toStringMatchRule matchrule, "\n"]
                                  @ (mkExtraMsgs ()))
                     val body_ty = checkExp (Env.extend (env, env'), body)
                  in
                     (body_ty, setDC)
                  end
         and checkMatchRules (env: Env.t, arg_ty: C.Type.t,
                              matchrules: C.MatchRule.t list,
                              setDC: C.DaCon.Set.set,
                              mkExtraMsgs: unit -> string list)
                             : C.Type.t =
            let
               val (prev_res_ty, setDC) =
                  List.foldl
                  (fn (matchrule, (prev_res_ty, setDC)) =>
                   let
                      val (res_ty, setDC) =
                         checkMatchRule (env, arg_ty,
                                         matchrule,
                                         setDC,
                                         mkExtraMsgs)
                      val () =
                         case prev_res_ty of
                            NONE => ()
                          | SOME prev_res_ty =>
                               if C.Type.equals (res_ty, prev_res_ty)
                                  then ()
                               else raiseTypeError
                                    (["Result types of match rules in 'case' disagree.\n",
                                      "  previous result type: ",
                                      toStringType prev_res_ty, "\n",
                                      "  this result type:     ",
                                      toStringType res_ty, "\n",
                                      "  in rule: ", toStringMatchRule matchrule, "\n"]
                                     @ (mkExtraMsgs ()))
                   in
                      (SOME res_ty, setDC)
                   end)
                   (NONE, setDC)
                   matchrules
               val res_ty =
                  case prev_res_ty of
                     NONE =>
                        raiseTypeError
                        (["No match rules in 'case' expression.\n"]
                         @ (mkExtraMsgs ()))
                   | SOME res_ty => res_ty
               val () =
                  if C.DaCon.Set.isEmpty setDC
                     then ()
                  else let
                          val missing =
                             String.concatWith ", "
                                               (List.map toStringDaCon
                                                         (C.DaCon.Set.listItems setDC))
                       in
                          raiseTypeError
                          (["Non-exhaustive match rules in 'case'.\n",
                            "  missing: ", missing, "\n"]
                           @ (mkExtraMsgs ()))
                       end
            in
               res_ty
            end
         and checkDecl (env: Env.t, decl: C.Decl.t) :
                       Env.t =
            case decl of
               C.Decl.D_Data {data_decls} =>
                  let
                     val (data_declsAux, envTC) =
                        ListExtra.mapAndFoldl
                        (fn ({tycon, tyvars, dacon_decls}, envTC) =>
                         let
                            val arity = List.length tyvars
                            val envTV =
                               List.foldl
                               (fn (tyvar, envTV) =>
                                let
                                   val envTV_i =
                                      Env.singletonTyVar (tyvar, ())
                                in
                                   Env.extend (envTV, envTV_i)
                                end)
                               Env.empty
                               tyvars
                            val envTC_i =
                               Env.singletonTyCon
                               (tycon, {arity = arity,
                                        dacons = List.map (fn {dacon, ...} => dacon)
                                                          dacon_decls})
                         in
                            ({tycon = tycon,
                              tyvars = tyvars,
                              envTV = envTV,
                              dacon_decls = dacon_decls},
                             Env.extend (envTC, envTC_i))
                         end)
                        Env.empty
                        data_decls
                     val envDC =
                        List.foldl
                        (fn ({tycon, tyvars, envTV, dacon_decls}, envDC) =>
                         let
                            val envDC_i =
                               checkDaConDecls (Env.extend (env, (Env.extend (envTC, envTV))),
                                                tyvars, tycon,
                                                dacon_decls)
                         in
                            Env.extend (envDC, envDC_i)
                         end)
                        Env.empty
                        data_declsAux
                     val env' = Env.extend (envTC, envDC)
                  in
                     env'
                  end
             | C.Decl.D_Val {var, var_ty, exp} =>
                  let
                     val exp_ty = checkExp (env, exp)
                     val env' =
                        checkBind (env, exp_ty,
                                   {var = var, var_ty = var_ty},
                                   fn () =>
                                   ["  in: ", toStringDecl decl])
                  in
                     env'
                  end
             | C.Decl.D_Fun {fun_decls} =>
                  let
                     val env' =
                        List.foldl
                        (fn ({func, func_ty, ...}, env') =>
                         let
                            val envF =
                               checkBind (env, func_ty,
                                          {var = func, var_ty = func_ty},
                                          fn () =>
                                          ["  in: ", toStringDecl decl])
                         in
                            Env.extend (env', envF)
                         end)
                        Env.empty
                        fun_decls
                     val () =
                        List.app
                        (fn {func, func_ty, lam, ...} =>
                         let
                            val lam_ty = checkLam (Env.extend (env, env'), lam)
                            val _ =
                               checkBind (env, lam_ty,
                                          {var = func, var_ty = func_ty},
                                          fn () =>
                                          ["  in: ", toStringDecl decl])
                         in
                            ()
                         end)
                        fun_decls
                  in
                     env'
                  end
         fun checkDecls (env: Env.t, decls: C.Decl.t list)
                        : Env.t =
            let
               val env' =
                  List.foldl
                  (fn (decl_i, env') =>
                   let
                      val env'_i =
                         checkDecl (Env.extend (env, env'), decl_i)
                   in
                      Env.extend (env', env'_i)
                   end)
                  Env.empty
                  decls
            in
               env'
            end

         fun checkProg (prog: C.Prog.t) : unit =
            let
               (* The initial environment E_0. *)
               val env0 = Env.empty
               (* Pre-defined type constructors. *)
               val env0 =
                  List.foldl
                  (fn ((tycon, arity, dacons), env) =>
                   Env.extend (env, Env.singletonTyCon (tycon, {arity = arity, dacons = dacons})))
                  env0
                  [(C.TyCon.array, 1, []),
                   (C.TyCon.bool, 0, [C.DaCon.falsee, C.DaCon.truee]),
                   (C.TyCon.integer, 0, []),
                   (C.TyCon.string, 0, []),
                   (C.TyCon.unit, 0, [C.DaCon.unit])]
               (* Pre-defined data constructors. *)
               val env0 =
                  List.foldl
                  (fn ((dacon, item), env) =>
                   Env.extend (env, Env.singletonDaCon (dacon, item)))
                  env0
                  [(C.DaCon.falsee, {tyvars = [], arg_tys = [],
                                     tycon = C.TyCon.bool}),
                   (C.DaCon.truee, {tyvars = [], arg_tys = [],
                                    tycon = C.TyCon.bool}),
                   (C.DaCon.unit, {tyvars = [], arg_tys = [],
                                   tycon = C.TyCon.unit})]
               val C.Prog.Prog {decls, exp} = prog
               val env' = checkDecls (env0, decls)
               val _ = checkExp (Env.extend (env0, env'), exp)
            in
               ()
            end
      in
         checkProg prog
      end

   val typeCheck =
      Control.mkTracePass
      {msgPre = NONE,
       msgPost = NONE,
       passName = "type-check-core",
       pass = typeCheck}

end
