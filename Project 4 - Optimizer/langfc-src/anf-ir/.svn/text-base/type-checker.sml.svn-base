(* langfc-src/anf-ir/type-checker.sml
 *
 * COPYRIGHT (c) 2015 Matthew Fluet (http://www.cs.rit.edu/~mtf)
 * All rights reserved.
 *
 * Rochester Institute of Technology
 * CSCI-742
 * S20135,S20145
 *
 * Type-checker for A-normal form intermediate representation.
 *)

structure AnfIRTypeChecker :> ANF_IR_TYPE_CHECKER =
struct
   structure A = AnfIR

   structure Tbl =
      struct
         structure TyVarTbl =
            struct
               type dom = A.TyVar.t
               type cod = unit
               type t = cod A.TyVar.Tbl.hash_table
               val lookup : t * dom -> cod option = fn (tbl, tyvar) =>
                  A.TyVar.Tbl.find tbl tyvar
               val insert : t * dom -> unit = fn (tbl, tyvar) =>
                  A.TyVar.Tbl.insert tbl (tyvar, ())
               val new : unit -> t = fn () =>
                  A.TyVar.Tbl.mkTable (32, Fail "TyVarTbl")
            end
         structure TyConTbl =
            struct
               type dom = A.TyCon.t
               type cod = unit
               type t = cod A.TyCon.Tbl.hash_table
               val lookup : t * dom -> cod option = fn (tbl, tycon) =>
                  A.TyCon.Tbl.find tbl tycon
               val insert : t * dom -> unit = fn (tbl, tycon) =>
                  A.TyCon.Tbl.insert tbl (tycon, ())
               val new : unit -> t = fn () =>
                  A.TyCon.Tbl.mkTable (32, Fail "TyConTbl")
            end
         structure DaConTbl =
            struct
               type dom = A.DaCon.t
               type cod = unit
               type t = cod A.DaCon.Tbl.hash_table
               val lookup : t * dom -> cod option = fn (tbl, dacon) =>
                  A.DaCon.Tbl.find tbl dacon
               val insert : t * dom -> unit = fn (tbl, dacon) =>
                  A.DaCon.Tbl.insert tbl (dacon, ())
               val new : unit -> t = fn () =>
                  A.DaCon.Tbl.mkTable (32, Fail "DaConTbl")
            end
         structure VarTbl =
            struct
               type dom = A.Var.t
               type cod = unit
               type t = cod A.Var.Tbl.hash_table
               val lookup : t * dom -> cod option = fn (tbl, var) =>
                  A.Var.Tbl.find tbl var
               val insert : t * dom -> unit = fn (tbl, var) =>
                  A.Var.Tbl.insert tbl (var, ())
               val new : unit -> t = fn () =>
                  A.Var.Tbl.mkTable (32, Fail "VarTbl")
            end

         datatype t =
            Tbl of {tyvarTbl: TyVarTbl.t,
                    tyconTbl: TyConTbl.t,
                    daconTbl: DaConTbl.t,
                    varTbl: VarTbl.t}

         val new = fn () =>
            Tbl {tyvarTbl = TyVarTbl.new (),
                 tyconTbl = TyConTbl.new (),
                 daconTbl = DaConTbl.new (),
                 varTbl = VarTbl.new ()}

         fun lookupTyVar (Tbl {tyvarTbl, ...}, tyvar) =
            TyVarTbl.lookup (tyvarTbl, tyvar)
         fun insertTyVar (Tbl {tyvarTbl, ...}, tyvar) =
            TyVarTbl.insert (tyvarTbl, tyvar)

         fun lookupTyCon (Tbl {tyconTbl, ...}, tycon) =
            TyConTbl.lookup (tyconTbl, tycon)
         fun insertTyCon (Tbl {tyconTbl, ...}, tycon) =
            TyConTbl.insert (tyconTbl, tycon)

         fun lookupDaCon (Tbl {daconTbl, ...}, dacon) =
            DaConTbl.lookup (daconTbl, dacon)
         fun insertDaCon (Tbl {daconTbl, ...}, dacon) =
            DaConTbl.insert (daconTbl, dacon)

         fun lookupVar (Tbl {varTbl, ...}, var) =
            VarTbl.lookup (varTbl, var)
         fun insertVar (Tbl {varTbl, ...}, var) =
            VarTbl.insert (varTbl, var)
      end

   structure Env =
      struct
         structure TyVarEnv =
            struct
               type dom = A.TyVar.t
               type cod = unit
               type t = cod A.TyVar.Map.map
               val empty : t = A.TyVar.Map.empty
               val singleton : dom * cod -> t = A.TyVar.Map.singleton
               val lookup : t * dom -> cod option = A.TyVar.Map.find
               val extend : t * t -> t = A.TyVar.Map.unionWith #2
               val domain : t -> A.TyVar.Set.set =
                  A.TyVar.Set.fromList o A.TyVar.Map.listKeys
            end
         structure TyConEnv =
            struct
               type dom = A.TyCon.t
               type cod = {arity: int,
                           dacons: A.DaCon.t list}
               type t = cod A.TyCon.Map.map
               val empty : t = A.TyCon.Map.empty
               val singleton : dom * cod -> t = A.TyCon.Map.singleton
               val lookup : t * dom -> cod option = A.TyCon.Map.find
               val extend : t * t -> t = A.TyCon.Map.unionWith #2
               val domain : t -> A.TyCon.Set.set =
                  A.TyCon.Set.fromList o A.TyCon.Map.listKeys
            end
         structure DaConEnv =
            struct
               type dom = A.DaCon.t
               type cod = {tyvars: A.TyVar.t list,
                           arg_tys: A.Type.t list,
                           tycon: A.TyCon.t}
               type t = cod A.DaCon.Map.map
               val empty : t = A.DaCon.Map.empty
               val singleton : dom * cod -> t = A.DaCon.Map.singleton
               val lookup : t * dom -> cod option = A.DaCon.Map.find
               val extend : t * t -> t = A.DaCon.Map.unionWith #2
               val domain : t -> A.DaCon.Set.set =
                  A.DaCon.Set.fromList o A.DaCon.Map.listKeys
            end
         structure VarEnv =
            struct
               type dom = A.Var.t
               type cod = {var_ty: A.Type.t}
               type t = cod A.Var.Map.map
               val empty : t = A.Var.Map.empty
               val singleton : dom * cod -> t = A.Var.Map.singleton
               val lookup : t * dom -> cod option = A.Var.Map.find
               val extend : t * t -> t = A.Var.Map.unionWith #2
               val domain : t -> A.Var.Set.set =
                  A.Var.Set.fromList o A.Var.Map.listKeys
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
   structure TblEnv =
      struct
         fun lookupTyVar (tbl, env, tyvar) =
            case Tbl.lookupTyVar (tbl, tyvar) of
               NONE => NONE
             | SOME _ => Env.lookupTyVar (env, tyvar)
         fun lookupTyCon (tbl, env, tycon) =
            case Tbl.lookupTyCon (tbl, tycon) of
               NONE => NONE
             | SOME _ => Env.lookupTyCon (env, tycon)
         fun lookupDaCon (tbl, env, dacon) =
            case Tbl.lookupDaCon (tbl, dacon) of
               NONE => NONE
             | SOME _ => Env.lookupDaCon (env, dacon)
         fun lookupVar (tbl, env, var) =
            case Tbl.lookupVar (tbl, var) of
               NONE => NONE
             | SOME _ => Env.lookupVar (env, var)
      end

   fun toStringTyVar tyvar =
      Layout.toString (A.TyVar.layout tyvar)
   fun toStringTyCon tycon =
      Layout.toString (A.TyCon.layout tycon)
   fun toStringDaCon dacon =
      Layout.toString (A.DaCon.layout dacon)
   fun toStringVar var =
      Layout.toString (A.Var.layout var)

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
         maybeElide (Layout.toString (A.Type.layout ty))
      fun toStringPat pat =
         maybeElide (Layout.toString (A.Pat.layout pat))
      fun toStringExp exp =
         maybeElide (Layout.toString (A.Exp.layout exp))
      fun toStringRHS rhs =
         maybeElide (Layout.toString (A.RHS.layout rhs))
      fun toStringLam lam =
         maybeElide (Layout.toString (A.Lam.layout lam))
      fun toStringMatchRule matchrule =
         maybeElide (Layout.toString (A.MatchRule.layout matchrule))
      fun toStringDecl decl =
         maybeElide (Layout.toString (A.Decl.layout decl))
      fun toStringProg prog =
         maybeElide (Layout.toString (A.Prog.layout prog))
   end

   fun typeCheck (prog: A.Prog.t) : unit =
      let
         exception TypeError
         fun raiseTypeError msgs =
            (List.map (fn msg => (TextIO.output (TextIO.stdErr, msg))) ("Type Error:\n" :: msgs)
             ; TextIO.output1 (TextIO.stdErr, #"\n")
             ; raise TypeError)

         val tbl = Tbl.new ()

         fun bindTyVar tyvar : unit =
            case Tbl.lookupTyVar (tbl, tyvar) of
               NONE => Tbl.insertTyVar (tbl, tyvar)
             | SOME () =>
                  raiseTypeError
                  ["Rebound type variable: ",
                   toStringTyVar tyvar, "."]
         fun bindTyCon tycon : unit =
            case Tbl.lookupTyCon (tbl, tycon) of
               NONE => Tbl.insertTyCon (tbl, tycon)
             | SOME () =>
                  raiseTypeError
                  ["Rebound type constructor: ",
                   toStringTyCon tycon, "."]
         fun bindDaCon dacon : unit =
            case Tbl.lookupDaCon (tbl, dacon) of
               NONE => Tbl.insertDaCon (tbl, dacon)
             | SOME () =>
                  raiseTypeError
                  ["Rebound data constructor: ",
                   toStringDaCon dacon, "."]
         fun bindVar var : unit =
            case Tbl.lookupVar (tbl, var) of
               NONE => Tbl.insertVar (tbl, var)
             | SOME () =>
                  raiseTypeError
                  ["Rebound variable: ",
                   toStringVar var, "."]

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

         fun checkType (env: Env.t, ty: A.Type.t)
                       : unit =
            case ty of
               A.Type.T_TyFn {tyvar, res} =>
                  let
                     val () = bindTyVar tyvar
                     val () =
                        checkType
                        (Env.extend
                         (env, Env.singletonTyVar (tyvar, ())),
                         res)
                  in
                     ()
                  end
             | A.Type.T_Fn {arg, res} =>
                  let
                     val () = checkType (env, arg)
                     val () = checkType (env, res)
                  in
                     ()
                  end
             | A.Type.T_TyCon {tycon, tyargs = actual_tyargs} =>
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
                     case TblEnv.lookupTyCon (tbl, env, tycon) of
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
             | A.Type.T_TyVar {tyvar} =>
                  (case TblEnv.lookupTyVar (tbl, env, tyvar) of
                      NONE =>
                         raiseTypeError
                         (["Unbound type variable: ",
                           toStringTyVar tyvar, ".\n",
                           "  in: ", toStringType ty])
                    | SOME () => ())

         fun checkVar (env: Env.t, var: A.Var.t,
                       mkExtraMsgs: unit -> string list) =
            case TblEnv.lookupVar (tbl, env, var) of
               NONE =>
                  raiseTypeError
                  (["Unbound variable: ",
                    toStringVar var, ".\n"]
                   @ (mkExtraMsgs ()))
             | SOME {var_ty} => var_ty

         fun checkBind (env: Env.t, arg_ty: A.Type.t, bind: {var: A.Var.t, var_ty: A.Type.t},
                        mkExtraMsgs: unit -> string list)
                       : Env.t =
            case bind of
               {var, var_ty} =>
                  let
                     val () = checkType (env, var_ty)
                     val () =
                        if A.Type.equals (arg_ty, var_ty)
                           then ()
                        else raiseTypeError
                             (["Variable type and argument type in binding disagree.\n",
                               "  variable: ", toStringVar var, "\n",
                               "  var type: ", toStringType var_ty, "\n",
                               "  arg type: ", toStringType arg_ty, "\n"]
                              @ (mkExtraMsgs ()))
                     val () = bindVar var
                     val env' = Env.singletonVar (var, {var_ty = var_ty})
                  in
                     env'
                  end

         fun checkPat (env: Env.t, arg_ty: A.Type.t, pat: A.Pat.t,
                       setDC: A.DaCon.Set.set,
                       mkExtraMsgs: unit -> string list)
                      : Env.t * A.DaCon.Set.set =
            case pat of
               A.Pat.P_DaCon {dacon,
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
                     case TblEnv.lookupDaCon (tbl, env, dacon) of
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
                                 A.Type.tySubstsL (formal_arg_tys,
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
                                 A.Type.T_TyCon
                                 {tycon = tycon,
                                  tyargs = actual_tyargs}
                              val () =
                                 if A.Type.equals (arg_ty, ty)
                                    then ()
                                 else raiseTypeError
                                      (["Argument and pattern in 'case' disagree.\n",
                                        "  argument: ", toStringType arg_ty, "\n",
                                        "  pattern:  ", toStringType ty, "\n"]
                                       @ (mkExtraMsgs ()))
                              val setDC =
                                 if A.DaCon.Set.member (setDC, dacon)
                                    then A.DaCon.Set.delete (setDC, dacon)
                                 else raiseTypeError
                                      (["Redundant pattern in 'case'.\n",
                                        "  pattern: ", toStringPat pat, "\n"]
                                       @ (mkExtraMsgs ()))
                           in
                              (env', setDC)
                           end
                  end
             | A.Pat.P_Var bind =>
                  let
                     val env' =
                        checkBind (env, arg_ty,
                                   bind,
                                   fn () => [])
                     val () =
                        if A.DaCon.Set.isEmpty setDC
                           then raiseTypeError
                                (["Redundant pattern in 'case'.\n",
                                  "  pattern: ", toStringPat pat, "\n"]
                                 @ (mkExtraMsgs ()))
                        else ()
                  in
                     (env', A.DaCon.Set.empty)
                  end

         fun checkDaConDecl (env: Env.t, tyvars: A.TyVar.t list, tycon: A.TyCon.t,
                             dacon_decl: {dacon: A.DaCon.t,
                                          arg_tys: A.Type.t list})
                            : Env.t =
            let
               val {dacon, arg_tys} = dacon_decl
               val () = List.app (fn arg_ty => checkType (env, arg_ty)) arg_tys
               val () = bindDaCon dacon
               val env' =
                  Env.singletonDaCon (dacon, {tyvars = tyvars,
                                              arg_tys = arg_tys,
                                              tycon = tycon})
            in
               env'
            end
         fun checkDaConDecls (env: Env.t, tyvars: A.TyVar.t list, tycon: A.TyCon.t,
                              dacon_decls: {dacon: A.DaCon.t,
                                            arg_tys: A.Type.t list} list)
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

         fun checkExp (env: Env.t, exp: A.Exp.t)
                      : A.Type.t = 
            case exp of
               A.Exp.Exp {decls, var, ty} =>
                  let
                     val env' = checkDecls (env, decls)
                     val var_ty = checkVar (Env.extend (env, env'), var, fn () =>
                                            ["  in: ", toStringExp exp])
                     val () = checkType (env, ty)
                     val () =
                        if A.Type.equals (var_ty, ty)
                           then ()
                        else raiseTypeError
                             (["Expression type and expression var disagree.\n",
                               "  expression type: ", toStringType ty, "\n",
                               "  expression var: ", toStringType var_ty, "\n",
                               "  in: ", toStringExp exp])
                  in
                     ty
                  end
         and checkRHS (env: Env.t, rhs: A.RHS.t)
                      : A.Type.t = 
            case rhs of
               A.RHS.R_Fn {lam} => checkLam (env, lam)
             | A.RHS.R_Prim {prim, tyargs = actual_tyargs, args = actual_args} =>
                  let
                     val {tyvars = formal_tyvars, arg_tys, res_ty} =
                        let
                           val integerBinOp =
                              {tyvars = [],
                               arg_tys = [A.Type.integer, A.Type.integer],
                               res_ty = A.Type.integer}
                           val integerUnOp =
                              {tyvars = [],
                               arg_tys = [A.Type.integer],
                               res_ty = A.Type.integer}
                           val integerCmp =
                              {tyvars = [],
                               arg_tys = [A.Type.integer, A.Type.integer],
                               res_ty = A.Type.bool}
                        in
                           case prim of
                              (* integer primitives *)
                              (* binary operations *)
                              A.Prim.Add => integerBinOp
                            | A.Prim.Sub => integerBinOp
                            | A.Prim.Mul => integerBinOp
                            | A.Prim.Div => integerBinOp
                            | A.Prim.Mod => integerBinOp
                            (* unary operations *)
                            | A.Prim.Neg => integerUnOp
                            (* comparision operations *)
                            | A.Prim.Eq => integerCmp
                            | A.Prim.NEq => integerCmp
                            | A.Prim.Lt => integerCmp
                            | A.Prim.Lte => integerCmp
                            | A.Prim.Gt => integerCmp
                            | A.Prim.Gte => integerCmp
                            (* string primitives *)
                            | A.Prim.Concat =>
                                 {tyvars = [],
                                  arg_tys = [A.Type.string, A.Type.string],
                                  res_ty = A.Type.string}
                            | A.Prim.Print =>
                                 {tyvars = [],
                                  arg_tys = [A.Type.string],
                                  res_ty = A.Type.unit}
                            | A.Prim.Size =>
                                 {tyvars = [],
                                  arg_tys = [A.Type.string],
                                  res_ty = A.Type.integer}
                            | A.Prim.Subscript =>
                                 {tyvars = [],
                                  arg_tys = [A.Type.string, A.Type.integer],
                                  res_ty = A.Type.integer}
                            (* array primitives *)
                            | A.Prim.Array =>
                                 let
                                    val tyvar = A.TyVar.new "'zzz"
                                    val ty = A.Type.T_TyVar {tyvar = tyvar}
                                 in
                                    {tyvars = [tyvar],
                                     arg_tys = [A.Type.integer, ty],
                                     res_ty = A.Type.array ty}
                                 end
                            | A.Prim.Len =>
                                 let
                                    val tyvar = A.TyVar.new "'zzz"
                                    val ty = A.Type.T_TyVar {tyvar = tyvar}
                                 in
                                    {tyvars = [tyvar],
                                     arg_tys = [A.Type.array ty],
                                     res_ty = A.Type.integer}
                                 end
                            | A.Prim.Idx =>
                                 let
                                    val tyvar = A.TyVar.new "'zzz"
                                    val ty = A.Type.T_TyVar {tyvar = tyvar}
                                 in
                                    {tyvars = [tyvar],
                                     arg_tys = [A.Type.array ty, A.Type.integer],
                                     res_ty = ty}
                                 end
                            | A.Prim.Upd =>
                                 let
                                    val tyvar = A.TyVar.new "'zzz"
                                    val ty = A.Type.T_TyVar {tyvar = tyvar}
                                 in
                                    {tyvars = [tyvar],
                                     arg_tys = [A.Type.array ty, A.Type.integer, ty],
                                     res_ty = ty}
                                 end
                            (* misc primitives *)
                            | A.Prim.Argc =>
                                 {tyvars = [],
                                  arg_tys = [A.Type.unit],
                                  res_ty = A.Type.integer}
                            | A.Prim.Arg =>
                                 {tyvars = [],
                                  arg_tys = [A.Type.integer],
                                  res_ty = A.Type.string}
                            | A.Prim.Fail =>
                                 let
                                    val tyvar = A.TyVar.new "'zzz"
                                 in
                                    {tyvars = [tyvar],
                                     arg_tys = [A.Type.string],
                                     res_ty = A.Type.T_TyVar {tyvar = tyvar}}
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
                           Layout.toString (A.Prim.layout prim),
                           " given ", given,
                           " type arguments but takes ", takes,
                           " type arguments.\n",
                           "  in: ", toStringRHS rhs]))
                     val arg_tys =
                        A.Type.tySubstsL (arg_tys,
                                          ListPair.zip (actual_tyargs,
                                                        formal_tyvars))
                     val () =
                        chkArity
                        (actual_args,
                         List.length arg_tys,
                         fn {given, takes} =>
                         raiseTypeError
                         (["Primitive ",
                           Layout.toString (A.Prim.layout prim),
                           " given ", given,
                           " arguments but takes ", takes,
                           " arguments.\n",
                           "  in: ", toStringRHS rhs]))
                     val () =
                        ListPair.app
                        (fn (actual_arg, arg_ty) =>
                         let
                            val actual_arg_ty = checkVar (env, actual_arg, fn () =>
                                                          ["  in: ", toStringRHS rhs])
                         in
                            if A.Type.equals (actual_arg_ty, arg_ty)
                               then ()
                            else raiseTypeError
                                 (["Argument type and var of primitive ",
                                   Layout.toString (A.Prim.layout prim), " disagree.\n",
                                   "  arg type:   ",
                                   toStringType arg_ty, "\n",
                                   "  var: ",
                                   toStringType actual_arg_ty, "\n",
                                   "  in var: ", toStringVar actual_arg, "\n",
                                   "  in: ", toStringRHS rhs])
                         end)
                        (actual_args, arg_tys)
                  in
                     A.Type.tySubsts (res_ty,
                                      ListPair.zip (actual_tyargs,
                                                    formal_tyvars))
                  end
             | A.RHS.R_DaCon {dacon,
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
                           "  in: ", toStringRHS rhs]))
                     fun finish_actual_args formal_arg_tys =
                        ListPair.app
                        (fn (actual_arg, formal_arg_ty) =>
                         let
                            val actual_arg_ty = checkVar (env, actual_arg, fn () =>
                                                          ["  in: ", toStringRHS rhs])
                         in
                            if A.Type.equals (actual_arg_ty, formal_arg_ty)
                               then ()
                            else raiseTypeError
                                 (["Argument type and expression of data constructor ",
                                   toStringDaCon dacon, " disagree.\n",
                                   "  arg type:   ",
                                   toStringType formal_arg_ty, "\n",
                                   "  var: ",
                                   toStringType actual_arg_ty, "\n",
                                   "  in arg: ", toStringVar actual_arg, "\n",
                                   "  in: ", toStringRHS rhs])
                         end)
                        (actual_args, formal_arg_tys)
                  in
                     case TblEnv.lookupDaCon (tbl, env, dacon) of
                        NONE =>
                           raiseTypeError
                           (["Unbound data constructor: ",
                             toStringDaCon dacon, ".\n",
                             "  in: ", toStringRHS rhs])
                       | SOME {tyvars = formal_tyvars,
                               arg_tys = formal_arg_tys,
                               tycon = tycon} =>
                           let
                              val () =
                                 finish_actual_tyargs
                                 (List.length formal_tyvars)
                              val formal_arg_tys =
                                 A.Type.tySubstsL (formal_arg_tys,
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
                                    "  in: ", toStringRHS rhs]))
                              val () =
                                 finish_actual_args formal_arg_tys
                           in
                              A.Type.T_TyCon {tycon = tycon,
                                              tyargs = actual_tyargs}
                           end
                  end
             | A.RHS.R_VApply {func, arg} =>
                  let
                     val func_ty = checkVar (env, func, fn () =>
                                             ["  in: ", toStringRHS rhs])
                  in
                     case func_ty of
                        A.Type.T_Fn {arg = formal_arg_ty, res = res_ty} =>
                           let
                              val actual_arg_ty = checkVar (env, arg, fn () =>
                                                            ["  in: ", toStringRHS rhs])
                              val () =
                                 if A.Type.equals (actual_arg_ty, formal_arg_ty)
                                    then ()
                                 else raiseTypeError
                                      (["Function and argument of apply disagree.\n",
                                        "  function: ", toStringType func_ty, "\n",
                                        "  argument: ", toStringType actual_arg_ty, "\n",
                                        "  in: ", toStringRHS rhs])
                           in
                              res_ty
                           end
                      | _ =>
                           raiseTypeError
                           (["Function of apply not of function type.\n",
                             "  function: ", toStringType func_ty, "\n",
                             "  in: ", toStringRHS rhs])
                  end
             | A.RHS.R_TApply {func, tyarg = actual_ty} =>
                  let
                     val func_ty = checkVar (env, func, fn () =>
                                             ["  in: ", toStringRHS rhs])
                  in
                     case func_ty of
                        A.Type.T_TyFn {tyvar = formal_tyarg, res = res_ty} =>
                           let
                              val () = checkType (env, actual_ty)
                           in
                              A.Type.tySubst (res_ty, (actual_ty, formal_tyarg))
                           end
                      | _ =>
                           raiseTypeError
                           (["Function of apply not of function type.\n",
                             "  function: ", toStringType func_ty, "\n",
                             "  in: ", toStringRHS rhs])
                  end
             | A.RHS.R_Var {var} => checkVar (env, var, fn () =>
                                              ["  in: ", toStringRHS rhs])
             | A.RHS.R_Integer _ => A.Type.integer
             | A.RHS.R_String _ => A.Type.string
             | A.RHS.R_Case {arg, matchrules} =>
                  let
                     val arg_ty = checkVar (env, arg, fn () =>
                                            ["  in: ", toStringRHS rhs])
                     val setDC =
                        case arg_ty of
                           A.Type.T_TyCon {tycon, ...} =>
                              (case TblEnv.lookupTyCon (tbl, env, tycon) of
                                  NONE =>
                                     raiseTypeError
                                     (["Unbound type constructor: ",
                                       toStringTyCon tycon, ".\n",
                                       "  in: ", toStringType arg_ty, "\n",
                                       "  in: ", toStringRHS rhs])
                                | SOME {dacons, ...} =>
                                     A.DaCon.Set.fromList dacons)
                         | _ =>
                              raiseTypeError
                              (["Argument type not type constructor in 'case'.\n",
                                "  arg type: ", toStringType arg_ty, "\n",
                                "  in: ", toStringRHS rhs])
                     val res_ty =
                        checkMatchRules (env, arg_ty,
                                         matchrules,
                                         setDC,
                                         fn () => ["  in: ", toStringRHS rhs])
                  in
                     res_ty
                  end
         and checkLam (env: Env.t, lam: A.Lam.t)
                      : A.Type.t =
            case lam of
               A.Lam.L_VLam {var, var_ty, body} =>
                  let
                     val env' = checkBind (env, var_ty, {var = var, var_ty = var_ty}, fn () =>
                                           ["  in: ", toStringLam lam])
                     val body_ty = checkExp (Env.extend (env, env'), body)
                  in
                     A.Type.T_Fn {arg = var_ty, res = body_ty}
                  end
             | A.Lam.L_TLam {tyvar, body} =>
                  let
                     val () = bindTyVar tyvar
                     val env' = Env.singletonTyVar (tyvar, ())
                     val body_ty = checkExp (Env.extend (env, env'), body)
                  in
                     A.Type.T_TyFn {tyvar = tyvar, res = body_ty}
                  end
         and checkMatchRule (env: Env.t, arg_ty: A.Type.t,
                             matchrule: A.MatchRule.t,
                             setDC: A.DaCon.Set.set,
                             mkExtraMsgs: unit -> string list)
                            : A.Type.t * A.DaCon.Set.set =
            case matchrule of
               A.MatchRule.MatchRule {pat, body} =>
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
         and checkMatchRules (env: Env.t, arg_ty: A.Type.t,
                              matchrules: A.MatchRule.t list,
                              setDC: A.DaCon.Set.set,
                              mkExtraMsgs: unit -> string list)
                             : A.Type.t =
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
                               if A.Type.equals (res_ty, prev_res_ty)
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
                  if A.DaCon.Set.isEmpty setDC
                     then ()
                  else let
                          val missing =
                             String.concatWith ", "
                                               (List.map toStringDaCon
                                                         (A.DaCon.Set.listItems setDC))
                       in
                          raiseTypeError
                          (["Non-exhaustive match rules in 'case'.\n",
                            "  missing: ", missing, "\n"]
                           @ (mkExtraMsgs ()))
                       end
            in
               res_ty
            end
         and checkDecl (env: Env.t, decl: A.Decl.t) :
                       Env.t =
            case decl of
               A.Decl.D_Data {data_decls} =>
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
                                   val () = bindTyVar tyvar
                                   val envTV_i =
                                      Env.singletonTyVar (tyvar, ())
                                in
                                   Env.extend (envTV, envTV_i)
                                end)
                               Env.empty
                               tyvars
                            val () = bindTyCon tycon
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
             | A.Decl.D_Val {var, var_ty, rhs} =>
                  let
                     val rhs_ty = checkRHS (env, rhs)
                     val env' =
                        checkBind (env, rhs_ty,
                                   {var = var, var_ty = var_ty},
                                   fn () =>
                                   ["  in: ", toStringDecl decl])
                  in
                     env'
                  end
             | A.Decl.D_Fun {fun_decls} =>
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
                        (fn {func_ty, lam, ...} =>
                         let
                            val lam_ty = checkLam (Env.extend (env, env'), lam)
                            val () =
                               if A.Type.equals (func_ty, lam_ty)
                                  then ()
                               else raiseTypeError
                                    (["Function type and lambda type disagree.\n",
                                      "  function type: ", toStringType func_ty, "\n",
                                      "  lambda type: ", toStringType lam_ty, "\n",
                                      "  in: ", toStringDecl decl])
                         in
                            ()
                         end)
                        fun_decls
                  in
                     env'
                  end
         and checkDecls (env: Env.t, decls: A.Decl.t list)
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

         fun checkProg (prog: A.Prog.t) : unit =
            let
               (* The initial environment E_0. *)
               val env0 = Env.empty
               (* Pre-defined type constructors. *)
               val env0 =
                  List.foldl
                  (fn ((tycon, item), env) =>
                   (bindTyCon tycon; Env.extend (env, Env.singletonTyCon (tycon, item))))
                  env0
                  [(A.TyCon.array, {arity = 1, dacons = []}),
                   (A.TyCon.bool, {arity = 0, dacons = [A.DaCon.falsee, A.DaCon.truee]}),
                   (A.TyCon.integer, {arity = 0, dacons = []}),
                   (A.TyCon.string, {arity = 0, dacons = []}),
                   (A.TyCon.unit, {arity = 0, dacons = [A.DaCon.unit]})]
               (* Pre-defined data constructors. *)
               val env0 =
                  List.foldl
                  (fn ((dacon, item), env) =>
                   (bindDaCon dacon; Env.extend (env, Env.singletonDaCon (dacon, item))))
                  env0
                  [(A.DaCon.falsee, {tyvars = [], arg_tys = [],
                                     tycon = A.TyCon.bool}),
                   (A.DaCon.truee, {tyvars = [], arg_tys = [],
                                    tycon = A.TyCon.bool}),
                   (A.DaCon.unit, {tyvars = [], arg_tys = [],
                                   tycon = A.TyCon.unit})]
               val A.Prog.Prog {decls, var, ty} = prog
               val env' = checkDecls (env0, decls)
               val var_ty = checkVar (Env.extend (env0, env'), var, fn () =>
                                      ["  in: ", toStringProg prog])
               val () = checkType (Env.extend (env0, env'), ty)
               val () =
                  if A.Type.equals (var_ty, ty)
                     then ()
                  else raiseTypeError
                       (["Program type and program var disagree.\n",
                         "  program type: ", toStringType ty, "\n",
                         "  program var: ", toStringType var_ty, "\n",
                         "  in: ", toStringProg prog])
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
       passName = "type-check-anf",
       pass = typeCheck}

end
