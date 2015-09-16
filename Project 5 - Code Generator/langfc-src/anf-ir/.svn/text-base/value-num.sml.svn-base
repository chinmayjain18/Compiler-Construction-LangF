(* langfc-src/anf-ir/value-num.sml
 *
 * COPYRIGHT (c) 2015 Matthew Fluet (http://www.cs.rit.edu/~mtf)
 * All rights reserved.
 *
 * Rochester Institute of Technology
 * CSCI-742
 * S20135,S20145
 *
 * Superlocal value numbering optimization for A-normal form
 * intermediate representation.
 *)

structure IntSet : ORD_SET where type Key.ord_key = int =
   RedBlackSetFn (struct
                     type ord_key = Int.int
                     val compare = Int.compare
                  end)
structure IntSet = IntListSet
structure IntSet = IntRedBlackSet
structure IntSet = IntBinarySet
structure IntMap : ORD_MAP where type Key.ord_key = int =
   RedBlackMapFn (struct
                     type ord_key = Int.int
                     val compare = Int.compare
                  end)
structure IntMap = IntListMap
structure IntMap = IntRedBlackMap
structure IntMap = IntBinaryMap
structure IntTbl : MONO_HASH_TABLE where type Key.hash_key = int =
   HashTableFn (struct
                   type hash_key = Int.int
                   val hashVal = Word.fromInt
                   val sameKey = fn (i, j) => i = j
                end)
structure IntTbl = IntHashTable

structure ValueNumBasicEnv : ANF_IR_OPTIMIZATION =
struct
   structure A = AnfIR

   structure SymbolicRHS =
      struct
         datatype t =
            S_Prim of {prim: A.Prim.t, tyargs: A.Type.t list, args: int list}
          | S_Integer of IntInf.int

         fun layout srhs =
            case srhs of
               S_Prim {prim, tyargs, args} =>
                  let
                     fun arg i = List.nth (args, i)
                     fun unary sym =
                        Layout.mayAlign
                        [Layout.str sym,
                         Layout.str ("$" ^ (Int.toString (arg 0)))]
                     fun binary sym =
                        Layout.mayAlign
                        [Layout.str ("$" ^ (Int.toString (arg 0))),
                         Layout.str sym,
                         Layout.str ("$" ^ (Int.toString (arg 1)))]
                     fun ternary (syml,symr) =
                        Layout.mayAlign
                        [Layout.str ("$" ^ (Int.toString (arg 0))),
                         Layout.str syml,
                         Layout.str ("$" ^ (Int.toString (arg 1))),
                         Layout.str symr,
                         Layout.str ("$" ^ (Int.toString (arg 2)))]
                     fun func name =
                        Layout.mayAlign
                        (List.concat
                         [[Layout.str name],
                          List.map (fn arg => Layout.seq [Layout.str "[", A.Type.layout arg, Layout.str "]"]) tyargs,
                          List.map (fn n => Layout.str ("$" ^ (Int.toString n))) args])
                  in
                     case prim of
                        A.Prim.Add => binary "+"
                      | A.Prim.Arg => func "arg"
                      | A.Prim.Argc => func "argc"
                      | A.Prim.Array => func "array"
                      | A.Prim.Concat => binary "^"
                      | A.Prim.Div => binary "/"
                      | A.Prim.Eq => binary "=="
                      | A.Prim.Fail => func "fail"
                      | A.Prim.Gt => binary ">"
                      | A.Prim.Gte => binary ">="
                      | A.Prim.Lt => binary "<"
                      | A.Prim.Lte => binary "<="
                      | A.Prim.Mod => binary "%"
                      | A.Prim.Mul => binary "*"
                      | A.Prim.NEq => binary "<>"
                      | A.Prim.Neg => unary "~"
                      | A.Prim.Print => func "print"
                      | A.Prim.Size => func "size"
                      | A.Prim.Sub => binary "-"
                      | A.Prim.Subscript => func "subscript"
                      | A.Prim.Upd => ternary ("!", ":=")
                      | A.Prim.Idx => binary "!"
                      | A.Prim.Len => unary "#"
                  end
             | S_Integer i => Layout.str (IntInf.toString i)

         fun compare (srhs1: t, srhs2: t) : order =
            case (srhs1, srhs2) of
               (S_Prim {prim = prim1, tyargs = tyargs1, args = args1},
                S_Prim {prim = prim2, tyargs = tyargs2, args = args2}) =>
                  (case A.Prim.compare (prim1, prim2) of
                      LESS => LESS
                    | EQUAL => (case List.collate A.Type.compare (tyargs1, tyargs2) of
                                   LESS => LESS
                                 | EQUAL => List.collate Int.compare (args1, args2)
                                 | GREATER => GREATER)
                    | GREATER => GREATER)
             | (S_Prim _, _) => LESS
             | (_, S_Prim _) => GREATER
             | (S_Integer i1, S_Integer i2) => IntInf.compare (i1, i2)
         fun equals (srhs1: t, srhs2: t) : bool =
            compare (srhs1, srhs2) = EQUAL
         fun hash (srhs: t) : word =
            case srhs of
               S_Prim {prim, tyargs, args} =>
                  List.foldl (fn (arg, w) => Word.xorb (Word.fromInt arg, w))
                             (List.foldl (fn (tyarg, w) => Word.xorb (A.Type.hash tyarg, w))
                                         (A.Prim.hash prim)
                                         tyargs)
                             args
             | S_Integer i => Word.fromLargeInt (IntInf.toLarge i)

         structure OrdKey =
            struct
               type ord_key = t
               val compare = compare
            end
         structure Set : ORD_SET where type Key.ord_key = t = RedBlackSetFn (OrdKey)
         structure Map : ORD_MAP where type Key.ord_key = t = RedBlackMapFn (OrdKey)

         structure HashKey =
            struct
               type hash_key = t
               val hashVal = hash
               val sameKey = equals
            end
         structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t = HashTableFn (HashKey)
      end
   structure SRHS = SymbolicRHS

   structure Env =
      struct
         structure VarEnv =
            struct
               type dom = A.Var.t
               type cod = int
               type t = cod A.Var.Map.map
               val empty : t = A.Var.Map.empty
               val singleton : dom * cod -> t = A.Var.Map.singleton
               val lookup : t * dom -> cod option = A.Var.Map.find
               val extend : t * t -> t = A.Var.Map.unionWith #2
            end
         structure ValNumEnv =
            struct
               type dom = int
               type cod = A.Var.t
               type t = cod IntMap.map
               val empty : t = IntMap.empty
               val singleton : dom * cod -> t = IntMap.singleton
               val lookup : t * dom -> cod option = IntMap.find
               val extend : t * t -> t = IntMap.unionWith #2
            end
         structure SRHSEnv =
            struct
               type dom = SRHS.t
               type cod = int
               type t = cod SRHS.Map.map
               val empty : t = SRHS.Map.empty
               val singleton : dom * cod -> t = SRHS.Map.singleton
               val lookup : t * dom -> cod option = SRHS.Map.find
               val extend : t * t -> t = SRHS.Map.unionWith #2
            end

         datatype t =
            Env of {varEnv: VarEnv.t,
                    valNumEnv: ValNumEnv.t,
                    srhsEnv: SRHSEnv.t}

         val empty =
            Env {varEnv = VarEnv.empty,
                 valNumEnv = ValNumEnv.empty,
                 srhsEnv = SRHSEnv.empty}

         fun fromVarEnv varEnv =
            Env {varEnv = varEnv,
                 valNumEnv = ValNumEnv.empty,
                 srhsEnv = SRHSEnv.empty}
         val singletonVar = fromVarEnv o VarEnv.singleton
         fun lookupVar (Env {varEnv, ...}, var) =
            VarEnv.lookup (varEnv, var)

         fun fromValNumEnv valNumEnv =
            Env {varEnv = VarEnv.empty,
                 valNumEnv = valNumEnv,
                 srhsEnv = SRHSEnv.empty}
         val singletonValNum = fromValNumEnv o ValNumEnv.singleton
         fun lookupValNum (Env {valNumEnv, ...}, n) =
            ValNumEnv.lookup (valNumEnv, n)

         fun fromSRHSEnv srhsEnv =
            Env {varEnv = VarEnv.empty,
                 valNumEnv = ValNumEnv.empty,
                 srhsEnv = srhsEnv}
         val singletonSRHS = fromSRHSEnv o SRHSEnv.singleton
         fun lookupSRHS (Env {srhsEnv, ...}, srhs) =
            SRHSEnv.lookup (srhsEnv, srhs)

         fun extend (Env {varEnv = varEnv1, valNumEnv = valNumEnv1, srhsEnv = srhsEnv1},
                     Env {varEnv = varEnv2, valNumEnv = valNumEnv2, srhsEnv = srhsEnv2}) =
            Env {varEnv = VarEnv.extend (varEnv1, varEnv2),
                 valNumEnv = ValNumEnv.extend (valNumEnv1, valNumEnv2),
                 srhsEnv = SRHSEnv.extend (srhsEnv1, srhsEnv2)}

         val singletonVarWithValNum = fn (var, n) =>
            let
               val env1 = singletonVar (var, n)
               val env2 = singletonValNum (n, var)
            in
               extend (env1, env2)
            end
      end

   val nextValNum =
      let val ctr = ref 0
      in fn () => (ctr := !ctr + 1; !ctr)
      end

   fun lookupVar (env: Env.t, var: A.Var.t) : int =
      case Env.lookupVar (env, var) of
         NONE => raise Fail (concat ["ValueNumBasicEnv.lookupVar: ", A.Var.toString var])
       | SOME valNum => valNum
   fun lookupValNum (env: Env.t, n: int) : A.Var.t =
      case Env.lookupValNum (env, n) of
         NONE => raise Fail (concat ["ValueNumBasicEnv.lookupValNum: ", Int.toString n])
       | SOME var => var

   fun xformPat (env: Env.t, pat: A.Pat.t) : Env.t =
      case pat of
         A.Pat.P_DaCon {dacon, tyargs, binds} =>
            List.foldl
            (fn ({var, ...}, env') =>
             Env.extend (env', Env.singletonVarWithValNum (var, nextValNum ())))
            Env.empty
            binds
       | A.Pat.P_Var {var, var_ty} =>
            Env.singletonVarWithValNum (var, nextValNum ())

   fun xformExp (env: Env.t, exp: A.Exp.t) : A.Exp.t =
      case exp of
         A.Exp.Exp {decls, var, ty} =>
            let
               val (env', decls) = xformDecls (env, decls)
            in
               A.Exp.Exp {decls = decls, var = var, ty = ty}
            end
   and xformLam (env: Env.t, lam: A.Lam.t) : A.Lam.t =
      case lam of
         A.Lam.L_VLam {var, var_ty, body} =>
            A.Lam.L_VLam
            {var = var,
             var_ty = var_ty,
             body = xformExp (Env.extend (env, Env.singletonVarWithValNum (var, nextValNum ())), body)}
       | A.Lam.L_TLam {tyvar, body} =>
            A.Lam.L_TLam
            {tyvar = tyvar,
             body = xformExp (env, body)}
   and xformMatchRule (env: Env.t, matchrule: A.MatchRule.t) : A.MatchRule.t =
      case matchrule of
         A.MatchRule.MatchRule {pat, body} =>
            let
               val env' = xformPat (env, pat)
            in
               A.MatchRule.MatchRule
               {pat = pat,
                body = xformExp (Env.extend (env, env'), body)}
            end
   and xformDecl (env: Env.t, decl: A.Decl.t) : Env.t * A.Decl.t =
      case decl of
         A.Decl.D_Data {data_decls} =>
            (Env.empty,
             A.Decl.D_Data {data_decls = data_decls})
       | A.Decl.D_Val {var, var_ty, rhs} =>
            let
               fun keep rhs = (Env.singletonVarWithValNum (var, nextValNum ()), rhs)
               fun try srhs =
                  case Env.lookupSRHS (env, srhs) of
                     NONE =>
                        let
                           val valNum = nextValNum ()
                        in
                           (Env.extend (Env.singletonVarWithValNum (var, valNum),
                                        Env.singletonSRHS (srhs, valNum)),
                            rhs)
                        end
                   | SOME valNum' =>
                        let
                           val var' = lookupValNum (env, valNum')
                        in
                           (Env.singletonVar (var, valNum'),
                            A.RHS.R_Var {var = var'})
                        end
               val (env', rhs) =
                  case rhs of
                     A.RHS.R_Fn {lam} =>
                        keep (A.RHS.R_Fn
                              {lam = xformLam (env, lam)})
                   | A.RHS.R_Prim {prim, tyargs, args} =>
                        let
                           val args = List.map (fn arg => lookupVar (env, arg)) args
                           val try = fn () =>
                              try (SRHS.S_Prim {prim = prim, tyargs = tyargs, args = args})
                        in
                           case prim of
                              A.Prim.Add => try ()
                            | A.Prim.Sub => try ()
                            | A.Prim.Mul => try ()
                            | A.Prim.Div => try ()
                            | A.Prim.Mod => try ()
                            | A.Prim.Neg => try ()
                            | A.Prim.Eq => try ()
                            | A.Prim.NEq => try ()
                            | A.Prim.Lt => try ()
                            | A.Prim.Lte => try ()
                            | A.Prim.Gt => try ()
                            | A.Prim.Gte => try ()
                            | _ => keep rhs
                        end
                   | A.RHS.R_DaCon _ => keep rhs
                   | A.RHS.R_VApply _ => keep rhs
                   | A.RHS.R_TApply _ => keep rhs
                   | A.RHS.R_Var {var = var'} => (Env.singletonVar (var, lookupVar (env, var')), rhs)
                   | A.RHS.R_Integer i => try (SRHS.S_Integer i)
                   | A.RHS.R_String s => keep rhs
                   | A.RHS.R_Case {arg, matchrules} =>
                        keep (A.RHS.R_Case
                              {arg = arg,
                               matchrules = List.map (fn mr => xformMatchRule (env, mr)) matchrules})
            in
               (env',
                A.Decl.D_Val {var = var, var_ty = var_ty, rhs = rhs})
            end
       | A.Decl.D_Fun {fun_decls} =>
            let
               val env' =
                  List.foldl
                  (fn ({func, ...}, env') =>
                   let
                      val envF = Env.singletonVarWithValNum (func, nextValNum ())
                   in
                      Env.extend (env', envF)
                   end)
                  Env.empty
                  fun_decls
            in
               (env',
                A.Decl.D_Fun {fun_decls = List.map (fn {func, func_ty, lam} =>
                                                    {func = func, func_ty = func_ty,
                                                     lam = xformLam (Env.extend (env, env'), lam)})
                                                   fun_decls})
            end
   and xformDecls (env: Env.t, decls: A.Decl.t list) : Env.t * A.Decl.t list =
      let
         val (decls, env') =
            ListExtra.mapAndFoldl
            (fn (decl_i, env') =>
             let
                val (env'_i, decl_i) =
                   xformDecl (Env.extend (env, env'), decl_i)
             in
                (decl_i,
                 Env.extend (env', env'_i))
             end)
            Env.empty
            decls
      in
         (env', decls)
      end

   fun xform (prog: A.Prog.t) : A.Prog.t =
      let
         val A.Prog.Prog {decls, var, ty} = prog
         val (env', decls) = xformDecls (Env.empty, decls)
      in
         A.Prog.Prog {decls = decls, var = var, ty = ty}
      end
end

structure ValueNumBasicTbl : ANF_IR_OPTIMIZATION =
struct
   structure A = AnfIR

   structure SymbolicRHS =
      struct
         datatype t =
            S_Prim of {prim: A.Prim.t, tyargs: A.Type.t list, args: int list}
          | S_Integer of IntInf.int

         fun layout srhs =
            case srhs of
               S_Prim {prim, tyargs, args} =>
                  let
                     fun arg i = List.nth (args, i)
                     fun unary sym =
                        Layout.mayAlign
                        [Layout.str sym,
                         Layout.str ("$" ^ (Int.toString (arg 0)))]
                     fun binary sym =
                        Layout.mayAlign
                        [Layout.str ("$" ^ (Int.toString (arg 0))),
                         Layout.str sym,
                         Layout.str ("$" ^ (Int.toString (arg 1)))]
                     fun ternary (syml,symr) =
                        Layout.mayAlign
                        [Layout.str ("$" ^ (Int.toString (arg 0))),
                         Layout.str syml,
                         Layout.str ("$" ^ (Int.toString (arg 1))),
                         Layout.str symr,
                         Layout.str ("$" ^ (Int.toString (arg 2)))]
                     fun func name =
                        Layout.mayAlign
                        (List.concat
                         [[Layout.str name],
                          List.map (fn arg => Layout.seq [Layout.str "[", A.Type.layout arg, Layout.str "]"]) tyargs,
                          List.map (fn n => Layout.str ("$" ^ (Int.toString n))) args])
                  in
                     case prim of
                        A.Prim.Add => binary "+"
                      | A.Prim.Arg => func "arg"
                      | A.Prim.Argc => func "argc"
                      | A.Prim.Array => func "array"
                      | A.Prim.Concat => binary "^"
                      | A.Prim.Div => binary "/"
                      | A.Prim.Eq => binary "=="
                      | A.Prim.Fail => func "fail"
                      | A.Prim.Gt => binary ">"
                      | A.Prim.Gte => binary ">="
                      | A.Prim.Lt => binary "<"
                      | A.Prim.Lte => binary "<="
                      | A.Prim.Mod => binary "%"
                      | A.Prim.Mul => binary "*"
                      | A.Prim.NEq => binary "<>"
                      | A.Prim.Neg => unary "~"
                      | A.Prim.Print => func "print"
                      | A.Prim.Size => func "size"
                      | A.Prim.Sub => binary "-"
                      | A.Prim.Subscript => func "subscript"
                      | A.Prim.Upd => ternary ("!", ":=")
                      | A.Prim.Idx => binary "!"
                      | A.Prim.Len => unary "#"
                  end
             | S_Integer i => Layout.str (IntInf.toString i)

         fun compare (srhs1: t, srhs2: t) : order =
            case (srhs1, srhs2) of
               (S_Prim {prim = prim1, tyargs = tyargs1, args = args1},
                S_Prim {prim = prim2, tyargs = tyargs2, args = args2}) =>
                  (case A.Prim.compare (prim1, prim2) of
                      LESS => LESS
                    | EQUAL => (case List.collate A.Type.compare (tyargs1, tyargs2) of
                                   LESS => LESS
                                 | EQUAL => List.collate Int.compare (args1, args2)
                                 | GREATER => GREATER)
                    | GREATER => GREATER)
             | (S_Prim _, _) => LESS
             | (_, S_Prim _) => GREATER
             | (S_Integer i1, S_Integer i2) => IntInf.compare (i1, i2)
         fun equals (srhs1: t, srhs2: t) : bool =
            compare (srhs1, srhs2) = EQUAL
         fun hash (srhs: t) : word =
            case srhs of
               S_Prim {prim, tyargs, args} =>
                  List.foldl (fn (arg, w) => Word.xorb (Word.fromInt arg, w))
                             (List.foldl (fn (tyarg, w) => Word.xorb (A.Type.hash tyarg, w))
                                         (A.Prim.hash prim)
                                         tyargs)
                             args
             | S_Integer i => Word.fromLargeInt (IntInf.toLarge i)

         structure OrdKey =
            struct
               type ord_key = t
               val compare = compare
            end
         structure Set : ORD_SET where type Key.ord_key = t = RedBlackSetFn (OrdKey)
         structure Map : ORD_MAP where type Key.ord_key = t = RedBlackMapFn (OrdKey)

         structure HashKey =
            struct
               type hash_key = t
               val hashVal = hash
               val sameKey = equals
            end
         structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t = HashTableFn (HashKey)
      end
   structure SRHS = SymbolicRHS

   structure Tbl =
      struct
         structure VarTbl =
            struct
               type dom = A.Var.t
               type cod = int
               type t = cod A.Var.Tbl.hash_table
               val lookup : t * dom -> cod option = fn (tbl, var) =>
                  A.Var.Tbl.find tbl var
               val insert : t * dom * cod -> unit = fn (tbl, var, valNum) =>
                  A.Var.Tbl.insert tbl (var, valNum)
               val new : unit -> t = fn () =>
                  A.Var.Tbl.mkTable (32, Fail "VarTbl")
            end
         structure ValNumTbl =
            struct
               type dom = int
               type cod = A.Var.t
               type t = cod IntTbl.hash_table
               val lookup : t * dom -> cod option = fn (tbl, valNum) =>
                  IntTbl.find tbl valNum
               val insert : t * dom * cod -> unit = fn (tbl, valNum, var) =>
                  IntTbl.insert tbl (valNum, var)
               val new : unit -> t = fn () =>
                  IntTbl.mkTable (32, Fail "ValNumTbl")
            end
         structure SRHSTbl =
            struct
               type dom = SRHS.t
               type cod = int
               type t = cod SRHS.Tbl.hash_table
               val lookup : t * dom -> cod option = fn (tbl, srhs) =>
                  SRHS.Tbl.find tbl srhs
               val insert : t * dom * cod -> unit = fn (tbl, srhs, valNum) =>
                  SRHS.Tbl.insert tbl (srhs, valNum)
               val new : unit -> t = fn () =>
                  SRHS.Tbl.mkTable (32, Fail "SRHSTbl")
               val filter : t * (cod -> bool) -> unit = fn (tbl, p) =>
                  SRHS.Tbl.filter p tbl
            end

         datatype t =
            Tbl of {varTbl: VarTbl.t,
                    valNumTbl: ValNumTbl.t,
                    srhsTbl: SRHSTbl.t}

         val new = fn () =>
            Tbl {varTbl = VarTbl.new (),
                 valNumTbl = ValNumTbl.new (),
                 srhsTbl = SRHSTbl.new ()}

         fun lookupVar (Tbl {varTbl, ...}, var) =
            VarTbl.lookup (varTbl, var)
         fun insertVar (Tbl {varTbl, ...}, var, valNum) =
            VarTbl.insert (varTbl, var, valNum)

         fun lookupValNum (Tbl {valNumTbl, ...}, valNum) =
            ValNumTbl.lookup (valNumTbl, valNum)
         fun insertValNum (Tbl {valNumTbl, ...}, valNum, var) =
            ValNumTbl.insert (valNumTbl, valNum, var)

         fun lookupSRHS (Tbl {srhsTbl, ...}, srhs) =
            SRHSTbl.lookup (srhsTbl, srhs)
         fun insertSRHS (Tbl {srhsTbl, ...}, srhs, valNum) =
            SRHSTbl.insert (srhsTbl, srhs, valNum)

         fun insertVarWithValNum (tbl, var, valNum) =
            (insertVar (tbl, var, valNum); insertValNum (tbl, valNum, var))

         fun filterSRHS (Tbl {srhsTbl, ...}, p) =
            SRHSTbl.filter (srhsTbl, p)
      end

   val nextValNum =
      let val ctr = ref 0
      in fn () => (ctr := !ctr + 1; !ctr)
      end

   fun lookupVar (tbl: Tbl.t, var: A.Var.t) : int =
      case Tbl.lookupVar (tbl, var) of
         NONE =>
            let
               val valNum = nextValNum ()
               val () = Tbl.insertVarWithValNum (tbl, var, valNum)
            in
               valNum
            end
       | SOME valNum => valNum
   fun lookupValNum (tbl: Tbl.t, n: int) : A.Var.t =
      case Tbl.lookupValNum (tbl, n) of
         NONE => raise Fail (concat ["ValueNumBasicTbl.lookupValNum: ", Int.toString n])
       | SOME var => var

   fun xformExp (tbl: Tbl.t, exp: A.Exp.t) : A.Exp.t =
      case exp of
         A.Exp.Exp {decls, var, ty} =>
            let
               val mark = nextValNum ()
               val decls = xformDecls (tbl, decls)
               val () = Tbl.filterSRHS (tbl, fn valNum => valNum <= mark)
            in
               A.Exp.Exp {decls = decls, var = var, ty = ty}
            end
   and xformLam (tbl: Tbl.t, lam: A.Lam.t) : A.Lam.t =
      case lam of
         A.Lam.L_VLam {var, var_ty, body} =>
            A.Lam.L_VLam
            {var = var,
             var_ty = var_ty,
             body = xformExp (tbl, body)}
       | A.Lam.L_TLam {tyvar, body} =>
            A.Lam.L_TLam
            {tyvar = tyvar,
             body = xformExp (tbl, body)}
   and xformMatchRule (tbl: Tbl.t, matchrule: A.MatchRule.t) : A.MatchRule.t =
      case matchrule of
         A.MatchRule.MatchRule {pat, body} =>
            A.MatchRule.MatchRule
            {pat = pat,
             body = xformExp (tbl, body)}
   and xformDecl (tbl: Tbl.t, decl: A.Decl.t) : A.Decl.t =
      case decl of
         A.Decl.D_Data {data_decls} =>
            A.Decl.D_Data {data_decls = data_decls}
       | A.Decl.D_Val {var, var_ty, rhs} =>
            let
               fun keep rhs = rhs
               fun try srhs =
                  case Tbl.lookupSRHS (tbl, srhs) of
                     NONE =>
                        let
                           val valNum = nextValNum ()
                           val () = Tbl.insertVarWithValNum (tbl, var, valNum)
                           val () = Tbl.insertSRHS (tbl, srhs, valNum)
                        in
                           rhs
                        end
                   | SOME valNum' =>
                        let
                           val var' = lookupValNum (tbl, valNum')
                           val () = Tbl.insertVar (tbl, var, valNum')
                        in
                           A.RHS.R_Var {var = var'}
                        end
               val rhs =
                  case rhs of
                     A.RHS.R_Fn {lam} =>
                        keep (A.RHS.R_Fn
                              {lam = xformLam (tbl, lam)})
                   | A.RHS.R_Prim {prim, tyargs, args} =>
                        let
                           val args = List.map (fn arg => lookupVar (tbl, arg)) args
                           val try = fn () =>
                              try (SRHS.S_Prim {prim = prim, tyargs = tyargs, args = args})
                        in
                           case prim of
                              A.Prim.Add => try ()
                            | A.Prim.Sub => try ()
                            | A.Prim.Mul => try ()
                            | A.Prim.Div => try ()
                            | A.Prim.Mod => try ()
                            | A.Prim.Neg => try ()
                            | A.Prim.Eq => try ()
                            | A.Prim.NEq => try ()
                            | A.Prim.Lt => try ()
                            | A.Prim.Lte => try ()
                            | A.Prim.Gt => try ()
                            | A.Prim.Gte => try ()
                            | _ => keep rhs
                        end
                   | A.RHS.R_DaCon _ => keep rhs
                   | A.RHS.R_VApply _ => keep rhs
                   | A.RHS.R_TApply _ => keep rhs
                   | A.RHS.R_Var {var = var'} => (Tbl.insertVar (tbl, var, lookupVar (tbl, var')); rhs)
                   | A.RHS.R_Integer i => try (SRHS.S_Integer i)
                   | A.RHS.R_String s => keep rhs
                   | A.RHS.R_Case {arg, matchrules} =>
                        keep (A.RHS.R_Case
                              {arg = arg,
                               matchrules = List.map (fn mr => xformMatchRule (tbl, mr)) matchrules})
            in
               A.Decl.D_Val {var = var, var_ty = var_ty, rhs = rhs}
            end
       | A.Decl.D_Fun {fun_decls} =>
            A.Decl.D_Fun {fun_decls = List.map (fn {func, func_ty, lam} =>
                                                {func = func, func_ty = func_ty,
                                                 lam = xformLam (tbl, lam)})
                                                fun_decls}
   and xformDecls (tbl: Tbl.t, decls: A.Decl.t list) : A.Decl.t list =
      List.map (fn decl => xformDecl (tbl, decl)) decls

   fun xform (prog: A.Prog.t) : A.Prog.t =
      let
         val A.Prog.Prog {decls, var, ty} = prog
         val decls = xformDecls (Tbl.new (), decls)
      in
         A.Prog.Prog {decls = decls, var = var, ty = ty}
      end
end

structure ValueNumBasic = ValueNumBasicEnv

structure ValueNumExtd : ANF_IR_OPTIMIZATION =
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

   structure A = AnfIR

   structure SymbolicRHS =
      struct
         datatype t =
            S_Prim of {prim: A.Prim.t, tyargs: A.Type.t list, args: int list}
          | S_DaCon of {dacon: A.DaCon.t, tyargs: A.Type.t list, args: int list}
          | S_Integer of IntInf.int
          | S_String of string

         fun layout srhs =
            case srhs of
               S_Prim {prim, tyargs, args} =>
                  let
                     fun arg i = List.nth (args, i)
                     fun unary sym =
                        Layout.mayAlign
                        [Layout.str sym,
                         Layout.str ("$" ^ (Int.toString (arg 0)))]
                     fun binary sym =
                        Layout.mayAlign
                        [Layout.str ("$" ^ (Int.toString (arg 0))),
                         Layout.str sym,
                         Layout.str ("$" ^ (Int.toString (arg 1)))]
                     fun ternary (syml,symr) =
                        Layout.mayAlign
                        [Layout.str ("$" ^ (Int.toString (arg 0))),
                         Layout.str syml,
                         Layout.str ("$" ^ (Int.toString (arg 1))),
                         Layout.str symr,
                         Layout.str ("$" ^ (Int.toString (arg 2)))]
                     fun func name =
                        Layout.mayAlign
                        (List.concat
                         [[Layout.str name],
                          List.map (fn arg => Layout.seq [Layout.str "[", A.Type.layout arg, Layout.str "]"]) tyargs,
                          List.map (fn n => Layout.str ("$" ^ (Int.toString n))) args])
                  in
                     case prim of
                        A.Prim.Add => binary "+"
                      | A.Prim.Arg => func "arg"
                      | A.Prim.Argc => func "argc"
                      | A.Prim.Array => func "array"
                      | A.Prim.Concat => binary "^"
                      | A.Prim.Div => binary "/"
                      | A.Prim.Eq => binary "=="
                      | A.Prim.Fail => func "fail"
                      | A.Prim.Gt => binary ">"
                      | A.Prim.Gte => binary ">="
                      | A.Prim.Lt => binary "<"
                      | A.Prim.Lte => binary "<="
                      | A.Prim.Mod => binary "%"
                      | A.Prim.Mul => binary "*"
                      | A.Prim.NEq => binary "<>"
                      | A.Prim.Neg => unary "~"
                      | A.Prim.Print => func "print"
                      | A.Prim.Size => func "size"
                      | A.Prim.Sub => binary "-"
                      | A.Prim.Subscript => func "subscript"
                      | A.Prim.Upd => ternary ("!", ":=")
                      | A.Prim.Idx => binary "!"
                      | A.Prim.Len => unary "#"
                  end
             | S_DaCon {dacon, tyargs, args} =>
                  let
                     fun layoutDaConArgs args =
                        Layout.optSeq ("{", "}", ",") (fn n => Layout.str ("$" ^ (Int.toString n))) args
                  in
                     if List.null tyargs andalso List.null args
                        then A.DaCon.layout dacon
                     else
                        Layout.seq
                        [A.DaCon.layout dacon,
                         Layout.prefixSpaceIfNonEmpty (Layout.optSeq ("[", "]", ",") A.Type.layout tyargs),
                         Layout.prefixSpaceIfNonEmpty (layoutDaConArgs args)]
                  end
             | S_Integer i => Layout.str (IntInf.toString i)
             | S_String s => Layout.str (concat ["\"", String.toString s, "\""])
 
         fun compare (srhs1: t, srhs2: t) : order =
            case (srhs1, srhs2) of
               (S_Prim {prim = prim1, tyargs = tyargs1, args = args1},
                S_Prim {prim = prim2, tyargs = tyargs2, args = args2}) =>
                  (case A.Prim.compare (prim1, prim2) of
                      LESS => LESS
                    | EQUAL => (case List.collate A.Type.compare (tyargs1, tyargs2) of
                                   LESS => LESS
                                 | EQUAL => List.collate Int.compare (args1, args2)
                                 | GREATER => GREATER)
                    | GREATER => GREATER)
             | (S_Prim _, _) => LESS
             | (_, S_Prim _) => GREATER
             | (S_DaCon {dacon = dacon1, tyargs = tyargs1, args = args1},
                S_DaCon {dacon = dacon2, tyargs = tyargs2, args = args2}) =>
                  (case A.DaCon.compare (dacon1, dacon2) of
                      LESS => LESS
                    | EQUAL => (case List.collate A.Type.compare (tyargs1, tyargs2) of
                                   LESS => LESS
                                 | EQUAL => List.collate Int.compare (args1, args2)
                                 | GREATER => GREATER)
                    | GREATER => GREATER)
             | (S_DaCon _, _) => LESS
             | (_, S_DaCon _) => GREATER
             | (S_Integer i1, S_Integer i2) => IntInf.compare (i1, i2)
             | (S_Integer _, _) => LESS
             | (_, S_Integer _) => GREATER
             | (S_String s1, S_String s2) => String.compare (s1, s2)
         fun equals (srhs1: t, srhs2: t) : bool =
            compare (srhs1, srhs2) = EQUAL
         fun hash (srhs: t) : word =
            case srhs of
               S_Prim {prim, tyargs, args} =>
                  List.foldl (fn (arg, w) => Word.xorb (Word.fromInt arg, w))
                             (List.foldl (fn (tyarg, w) => Word.xorb (A.Type.hash tyarg, w))
                                         (A.Prim.hash prim)
                                         tyargs)
                             args
             | S_DaCon {dacon, tyargs, args} =>
                  List.foldl (fn (arg, w) => Word.xorb (Word.fromInt arg, w))
                             (List.foldl (fn (tyarg, w) => Word.xorb (A.Type.hash tyarg, w))
                                         (A.DaCon.hash dacon)
                                         tyargs)
                             args
             | S_Integer i => Word.fromLargeInt (IntInf.toLarge i)
             | S_String s => HashString.hashString s

         structure OrdKey =
            struct
               type ord_key = t
               val compare = compare
            end
         structure Set : ORD_SET where type Key.ord_key = t = RedBlackSetFn (OrdKey)
         structure Map : ORD_MAP where type Key.ord_key = t = RedBlackMapFn (OrdKey)

         structure HashKey =
            struct
               type hash_key = t
               val hashVal = hash
               val sameKey = equals
            end
         structure Tbl : MONO_HASH_TABLE where type Key.hash_key = t = HashTableFn (HashKey)
      end
   structure SRHS = SymbolicRHS

   structure Env =
      struct
         structure VarEnv =
            struct
               type dom = A.Var.t
               type cod = {valNum: int,
                           srhs: SRHS.t option}
               type t = cod A.Var.Map.map
               val empty : t = A.Var.Map.empty
               val singleton : dom * cod -> t = A.Var.Map.singleton
               val lookup : t * dom -> cod option = A.Var.Map.find
               val extend : t * t -> t = A.Var.Map.unionWith #2
               fun layout env =
                  Layout.seq
                  [Layout.str "{",
                   (Layout.mayAlign o Layout.separateRight)
                   (List.map (fn (var, {valNum, srhs}) =>
                              Layout.seq
                              [A.Var.layout var,
                               Layout.str " |-> ",
                               Layout.str ("$" ^ (Int.toString valNum)),
                               Layout.str " / ",
                               case srhs of
                                  NONE => Layout.str "?"
                                | SOME srhs => SRHS.layout srhs])
                             (A.Var.Map.listItemsi env),
                    ","),
                   Layout.str "}"]
            end
         structure ValNumEnv =
            struct
               type dom = int
               type cod = A.Var.t
               type t = cod IntMap.map
               val empty : t = IntMap.empty
               val singleton : dom * cod -> t = IntMap.singleton
               val lookup : t * dom -> cod option = IntMap.find
               val extend : t * t -> t = IntMap.unionWith #2
               fun layout env =
                  Layout.seq
                  [Layout.str "{",
                   (Layout.mayAlign o Layout.separateRight)
                   (List.map (fn (valNum, var) =>
                              Layout.seq
                              [Layout.str ("$" ^ (Int.toString valNum)),
                               Layout.str " |-> ",
                               A.Var.layout var])
                             (IntMap.listItemsi env),
                    ","),
                   Layout.str "}"]
            end
         structure SRHSEnv =
            struct
               type dom = SRHS.t
               type cod = int
               type t = cod SRHS.Map.map
               val empty : t = SRHS.Map.empty
               val singleton : dom * cod -> t = SRHS.Map.singleton
               val lookup : t * dom -> cod option = SRHS.Map.find
               val extend : t * t -> t = SRHS.Map.unionWith #2
               fun layout env =
                  Layout.seq
                  [Layout.str "{",
                   (Layout.mayAlign o Layout.separateRight)
                   (List.map (fn (srhs, valNum) =>
                              Layout.seq
                              [SRHS.layout srhs,
                               Layout.str " |-> ",
                               Layout.str ("$" ^ (Int.toString valNum))])
                             (SRHS.Map.listItemsi env),
                    ","),
                   Layout.str "}"]
            end

         datatype t =
            Env of {varEnv: VarEnv.t,
                    valNumEnv: ValNumEnv.t,
                    srhsEnv: SRHSEnv.t}

         val empty =
            Env {varEnv = VarEnv.empty,
                 valNumEnv = ValNumEnv.empty,
                 srhsEnv = SRHSEnv.empty}

         fun fromVarEnv varEnv =
            Env {varEnv = varEnv,
                 valNumEnv = ValNumEnv.empty,
                 srhsEnv = SRHSEnv.empty}
         val singletonVar' = fromVarEnv o VarEnv.singleton
         val singletonVar = fn (var, n) =>
            singletonVar' (var, {valNum = n, srhs = NONE})
         fun lookupVar (Env {varEnv, ...}, var) =
            VarEnv.lookup (varEnv, var)

         fun fromValNumEnv valNumEnv =
            Env {varEnv = VarEnv.empty,
                 valNumEnv = valNumEnv,
                 srhsEnv = SRHSEnv.empty}
         val singletonValNum = fromValNumEnv o ValNumEnv.singleton
         fun lookupValNum (Env {valNumEnv, ...}, n) =
            ValNumEnv.lookup (valNumEnv, n)

         fun fromSRHSEnv srhsEnv =
            Env {varEnv = VarEnv.empty,
                 valNumEnv = ValNumEnv.empty,
                 srhsEnv = srhsEnv}
         val singletonSRHS = fromSRHSEnv o SRHSEnv.singleton
         fun lookupSRHS (Env {srhsEnv, ...}, srhs) =
            SRHSEnv.lookup (srhsEnv, srhs)

         fun extend (Env {varEnv = varEnv1, valNumEnv = valNumEnv1, srhsEnv = srhsEnv1},
                     Env {varEnv = varEnv2, valNumEnv = valNumEnv2, srhsEnv = srhsEnv2}) =
            Env {varEnv = VarEnv.extend (varEnv1, varEnv2),
                 valNumEnv = ValNumEnv.extend (valNumEnv1, valNumEnv2),
                 srhsEnv = SRHSEnv.extend (srhsEnv1, srhsEnv2)}

         val singletonVarWithValNum' = fn (var, {valNum, srhs}) =>
            let
               val env1 = singletonVar' (var, {valNum = valNum, srhs = srhs})
               val env2 = singletonValNum (valNum, var)
            in
               extend (env1, env2)
            end
         val singletonVarWithValNum = fn (var, n) =>
            let
               val env1 = singletonVar (var, n)
               val env2 = singletonValNum (n, var)
            in
               extend (env1, env2)
            end

         fun layout (Env {varEnv, valNumEnv, srhsEnv}) =
            Layout.align
            [VarEnv.layout varEnv,
             ValNumEnv.layout valNumEnv,
             SRHSEnv.layout srhsEnv]
      end

   val nextValNum =
      let val ctr = ref 0
      in fn () => (ctr := !ctr + 1; !ctr)
      end

   fun lookupVar' (env: Env.t, var: A.Var.t) : {valNum: int, srhs: SRHS.t option} =
      case Env.lookupVar (env, var) of
         NONE => raise Fail (concat ["ValueNumExtd.lookupVar': ", A.Var.toString var])
       | SOME {valNum, srhs} => {valNum = valNum, srhs = srhs}
   fun lookupVar (env: Env.t, var: A.Var.t) : int =
      case Env.lookupVar (env, var) of
         NONE => raise Fail (concat ["ValueNumExtd.lookupVar: ", A.Var.toString var])
       | SOME {valNum, ...} => valNum
   fun lookupValNum (env: Env.t, n: int) : A.Var.t =
      case Env.lookupValNum (env, n) of
         NONE => raise Fail (concat ["ValueNumExtd.lookupValNum: ", Int.toString n])
       | SOME var => var

   fun xformPat (env: Env.t, arg: A.Var.t, pat: A.Pat.t) : Env.t =
      case pat of
         A.Pat.P_DaCon {dacon = pat_dacon, tyargs = pat_tyargs, binds} =>
            let
               fun default () =
                  let
                     val (args, env') =
                        ListExtra.mapAndFoldl
                        (fn ({var, ...}, env') =>
                         let
                            val valNum = nextValNum ()
                         in
                            (valNum, Env.extend (env', Env.singletonVarWithValNum (var, valNum)))
                         end)
                        Env.empty
                        binds
                     val srhs = SRHS.S_DaCon {dacon = pat_dacon, tyargs = pat_tyargs, args = args}
                     val env'' =
                        let
                           val valNum =
                              case Env.lookupSRHS (env, srhs) of
                                 NONE => lookupVar (env, arg)
                               | SOME valNum => valNum
                        in
                           Env.extend
                           (Env.singletonVar' (arg, {valNum = valNum, srhs = SOME srhs}),
                            Env.singletonSRHS (srhs, valNum))
                        end
                  in
                     Env.extend (env', env'')
                  end
            in
               case #srhs (lookupVar' (env, arg)) of
                  SOME (SRHS.S_DaCon {dacon = arg_dacon, tyargs = arg_tyargs, args = args}) =>
                     ListPair.foldl
                     (fn (arg, {var, ...}, env') =>
                      Env.extend (env',
                                  Env.singletonVarWithValNum'
                                  (var, lookupVar' (env, lookupValNum (env, arg)))))
                     Env.empty
                     (args, binds)
              | _ => default ()
            end
       | A.Pat.P_Var {var, var_ty} =>
            let
               val env' = Env.singletonVarWithValNum' (var, lookupVar' (env, arg))
            in
               env'
            end

   fun xformExp (env: Env.t, exp: A.Exp.t) : A.Exp.t =
      case exp of
         A.Exp.Exp {decls, var, ty} =>
            let
               val (env', decls) = xformDecls (env, decls)
            in
               A.Exp.Exp {decls = decls, var = var, ty = ty}
            end
   and xformLam (env: Env.t, lam: A.Lam.t) : A.Lam.t =
      case lam of
         A.Lam.L_VLam {var, var_ty, body} =>
            A.Lam.L_VLam
            {var = var,
             var_ty = var_ty,
             body = xformExp (Env.extend (env, Env.singletonVarWithValNum (var, nextValNum ())), body)}
       | A.Lam.L_TLam {tyvar, body} =>
            A.Lam.L_TLam
            {tyvar = tyvar,
             body = xformExp (env, body)}
   and xformMatchRule (env: Env.t, arg: A.Var.t, matchrule: A.MatchRule.t) : A.MatchRule.t =
      case matchrule of
         A.MatchRule.MatchRule {pat, body} =>
            let
               val env' = xformPat (env, arg, pat)
            in
               A.MatchRule.MatchRule
               {pat = pat,
                body = xformExp (Env.extend (env, env'), body)}
            end
   and xformDecl (env: Env.t, decl: A.Decl.t) : Env.t * A.Decl.t =
      case decl of
         A.Decl.D_Data {data_decls} =>
            (Env.empty,
             A.Decl.D_Data {data_decls = data_decls})
       | A.Decl.D_Val {var, var_ty, rhs} =>
            let
               val rhs =
                  case rhs of
                     A.RHS.R_Prim {prim, tyargs, args} =>
                        let
                           local
                              fun wrap (i: IntInf.int) =
                                 let
                                    val bits = IntInf.andb (i, IntInf.<< (1, 0w62) - 1)
                                    val res =
                                       if IntInf.andb (i, IntInf.<< (1, 0w62)) = 0
                                          then bits
                                       else IntInf.orb (bits, IntInf.<< (~1, 0w62))
                                 in
                                    A.RHS.R_Integer res
                                 end
                           in
                              fun doBinArith (oper, i1: IntInf.int, i2: IntInf.int) =
                                 wrap (oper (i1, i2))
                              fun doUnArith (oper, i: IntInf.int) =
                                 wrap (oper i)
                           end
                           local
                              fun wrap dacon =
                                 A.RHS.R_DaCon {dacon = dacon, tyargs = [], args = []}
                           in
                              fun doCmp (oper, i1: IntInf.int, i2: IntInf.int) =
                                 if oper (i1, i2)
                                    then wrap A.DaCon.truee
                                 else wrap A.DaCon.falsee
                           end
                           val arg = fn i => List.nth (args, i)
                           val args = List.map (fn arg => #srhs (lookupVar' (env, arg))) args
                        in
                           case (prim, args) of
                              (A.Prim.Add, [SOME (SRHS.S_Integer i1), SOME (SRHS.S_Integer i2)]) =>
                                 doBinArith (IntInf.+, i1, i2)
                            | (A.Prim.Add, [SOME (SRHS.S_Integer 0), _]) =>
                                 A.RHS.R_Var {var = arg 1}
                            | (A.Prim.Add, [_, SOME (SRHS.S_Integer 0)]) =>
                                 A.RHS.R_Var {var = arg 0}
                            | (A.Prim.Sub, [SOME (SRHS.S_Integer i1), SOME (SRHS.S_Integer i2)]) =>
                                 doBinArith (IntInf.-, i1, i2)
                            | (A.Prim.Sub, [SOME (SRHS.S_Integer 0), _]) =>
                                 A.RHS.R_Prim {prim = A.Prim.Neg, tyargs = [], args = [arg 1]}
                            | (A.Prim.Sub, [_, SOME (SRHS.S_Integer 0)]) =>
                                 A.RHS.R_Var {var = arg 0}
                            | (A.Prim.Sub, [_, _]) =>
                                 if A.Var.equals (arg 0, arg 1) then A.RHS.R_Integer 0 else rhs
                            | (A.Prim.Mul, [SOME (SRHS.S_Integer i1), SOME (SRHS.S_Integer i2)]) =>
                                 doBinArith (IntInf.*, i1, i2)
                            | (A.Prim.Mul, [SOME (SRHS.S_Integer 0), _]) =>
                                 A.RHS.R_Integer 0
                            | (A.Prim.Mul, [_, SOME (SRHS.S_Integer 0)]) =>
                                 A.RHS.R_Integer 0
                            | (A.Prim.Mul, [SOME (SRHS.S_Integer 1), _]) =>
                                 A.RHS.R_Var {var = arg 1}
                            | (A.Prim.Mul, [_, SOME (SRHS.S_Integer 1)]) =>
                                 A.RHS.R_Var {var = arg 0}
                            | (A.Prim.Mul, [SOME (SRHS.S_Integer ~1), _]) =>
                                 A.RHS.R_Prim {prim = A.Prim.Neg, tyargs = [], args = [arg 1]}
                            | (A.Prim.Mul, [_, SOME (SRHS.S_Integer ~1)]) =>
                                 A.RHS.R_Prim {prim = A.Prim.Neg, tyargs = [], args = [arg 0]}
                            | (A.Prim.Div, [SOME (SRHS.S_Integer i1), SOME (SRHS.S_Integer i2)]) =>
                                 if i2 = 0 then rhs else doBinArith (IntInf.quot, i1, i2)
                            | (A.Prim.Div, [SOME (SRHS.S_Integer 0), _]) =>
                                 A.RHS.R_Integer 0
                            | (A.Prim.Div, [_, SOME (SRHS.S_Integer 1)]) =>
                                 A.RHS.R_Var {var = arg 0}
                            | (A.Prim.Div, [_, SOME (SRHS.S_Integer ~1)]) =>
                                 A.RHS.R_Prim {prim = A.Prim.Neg, tyargs = [], args = [arg 0]}
                            | (A.Prim.Div, [_, _]) =>
                                 if A.Var.equals (arg 0, arg 1) then A.RHS.R_Integer 1 else rhs
                            | (A.Prim.Mod, [SOME (SRHS.S_Integer i1), SOME (SRHS.S_Integer i2)]) =>
                                 if i2 = 0 then rhs else doBinArith (IntInf.rem, i1, i2)
                            | (A.Prim.Mod, [SOME (SRHS.S_Integer 0), _]) =>
                                 A.RHS.R_Integer 0
                            | (A.Prim.Mod, [_, SOME (SRHS.S_Integer 1)]) =>
                                 A.RHS.R_Integer 0
                            | (A.Prim.Mod, [_, SOME (SRHS.S_Integer ~1)]) =>
                                 A.RHS.R_Integer 0
                            | (A.Prim.Mod, [_, _]) =>
                                 if A.Var.equals (arg 0, arg 1) then A.RHS.R_Integer 0 else rhs
                            | (A.Prim.Neg, [SOME (SRHS.S_Integer i)]) =>
                                 doUnArith (IntInf.~, i)
                            | (A.Prim.Eq, [SOME (SRHS.S_Integer i1), SOME (SRHS.S_Integer i2)]) =>
                                 doCmp (op =, i1, i2)
                            | (A.Prim.Eq, [_, _]) =>
                                 if A.Var.equals (arg 0, arg 1) then A.RHS.R_DaCon {dacon = A.DaCon.truee, tyargs = [], args = []} else rhs
                            | (A.Prim.NEq, [SOME (SRHS.S_Integer i1), SOME (SRHS.S_Integer i2)]) =>
                                 doCmp (op <>, i1, i2)
                            | (A.Prim.Lt, [SOME (SRHS.S_Integer i1), SOME (SRHS.S_Integer i2)]) =>
                                 doCmp (IntInf.<, i1, i2)
                            | (A.Prim.Lt, [_, _]) =>
                                 if A.Var.equals (arg 0, arg 1) then A.RHS.R_DaCon {dacon = A.DaCon.falsee, tyargs = [], args = []} else rhs
                            | (A.Prim.Lte, [SOME (SRHS.S_Integer i1), SOME (SRHS.S_Integer i2)]) =>
                                 doCmp (IntInf.<=, i1, i2)
                            | (A.Prim.Lte, [_, _]) =>
                                 if A.Var.equals (arg 0, arg 1) then A.RHS.R_DaCon {dacon = A.DaCon.truee, tyargs = [], args = []} else rhs
                            | (A.Prim.Gt, [SOME (SRHS.S_Integer i1), SOME (SRHS.S_Integer i2)]) =>
                                 doCmp (IntInf.>, i1, i2)
                            | (A.Prim.Gt, [_, _]) =>
                                 if A.Var.equals (arg 0, arg 1) then A.RHS.R_DaCon {dacon = A.DaCon.falsee, tyargs = [], args = []} else rhs
                            | (A.Prim.Gte, [SOME (SRHS.S_Integer i1), SOME (SRHS.S_Integer i2)]) =>
                                 doCmp (IntInf.>=, i1, i2)
                            | (A.Prim.Gte, [_, _]) =>
                                 if A.Var.equals (arg 0, arg 1) then A.RHS.R_DaCon {dacon = A.DaCon.truee, tyargs = [], args = []} else rhs
                            | (A.Prim.Concat, [SOME (SRHS.S_String s1), SOME (SRHS.S_String s2)]) =>
                                 A.RHS.R_String (s1 ^ s2)
                            | (A.Prim.Concat, [SOME (SRHS.S_String ""), _]) =>
                                 A.RHS.R_Var {var = arg 1}
                            | (A.Prim.Concat, [_, SOME (SRHS.S_String "")]) =>
                                 A.RHS.R_Var {var = arg 0}
                            | (A.Prim.Size, [SOME (SRHS.S_String s)]) =>
                                 A.RHS.R_Integer (IntInf.fromInt (String.size s))
                            | (A.Prim.Subscript, [SOME (SRHS.S_String s), SOME (SRHS.S_Integer i)]) =>
                                 if 0 <= i andalso i < IntInf.fromInt (String.size s)
                                    then let val c = String.sub (s, IntInf.toInt i)
                                         in A.RHS.R_Integer (IntInf.fromInt (ord c))
                                         end
                                 else A.RHS.R_Integer 0
                            | _ => rhs
                        end
                   | _ => rhs
               fun keep rhs = (Env.singletonVarWithValNum (var, nextValNum ()), rhs)
               fun try (srhs, srhsExtras) =
                  case Env.lookupSRHS (env, srhs) of
                     NONE =>
                        let
                           val valNum = nextValNum ()
                        in
                           (List.foldl Env.extend
                                       (Env.singletonVarWithValNum' (var, {valNum = valNum, srhs = SOME srhs}))
                                       (List.map (fn (srhs, valNum) => Env.singletonSRHS (srhs, valNum))
                                                 ((srhs, valNum) :: (srhsExtras valNum))),
                            rhs)
                        end
                   | SOME valNum' =>
                        let
                           val var' = lookupValNum (env, valNum')
                        in
                           (Env.singletonVarWithValNum' (var, {valNum = valNum', srhs = SOME srhs}),
                            A.RHS.R_Var {var = var'})
                        end
               val (env', rhs) =
                  case rhs of
                     A.RHS.R_Fn {lam} => keep (A.RHS.R_Fn {lam = xformLam (env, lam)})
                   | A.RHS.R_Prim {prim, tyargs, args} =>
                        let
                           val args = List.map (fn arg => lookupVar (env, arg)) args
                           val arg = fn i => List.nth (args, i)
                           val try = fn srhsExtras =>
                              try (SRHS.S_Prim {prim = prim, tyargs = tyargs, args = args},
                                   srhsExtras)
                        in
                           case prim of
                              A.Prim.Add => try (fn var =>
                                                 [(SRHS.S_Prim {prim = A.Prim.Add,
                                                                tyargs = [],
                                                                args = [arg 1, arg 0]}, var),
                                                  (SRHS.S_Prim {prim = A.Prim.Sub,
                                                                tyargs = [],
                                                                args = [var, arg 0]}, arg 1),
                                                  (SRHS.S_Prim {prim = A.Prim.Sub,
                                                                tyargs = [],
                                                                args = [var, arg 1]}, arg 0)])
                            | A.Prim.Sub => try (fn var => 
                                                 [(SRHS.S_Prim {prim = A.Prim.Add,
                                                                tyargs = [],
                                                                args = [var, arg 1]}, arg 0),
                                                  (SRHS.S_Prim {prim = A.Prim.Add,
                                                                tyargs = [],
                                                                args = [arg 1, var]}, arg 0),
                                                  (SRHS.S_Prim {prim = A.Prim.Sub,
                                                                tyargs = [],
                                                                args = [arg 0, var]}, arg 1)])
                            | A.Prim.Mul => try (fn var =>
                                                 [(SRHS.S_Prim {prim = A.Prim.Mul,
                                                                tyargs = [],
                                                                args = [arg 1, arg 0]}, var),
                                                  (SRHS.S_Prim {prim = A.Prim.Div,
                                                                tyargs = [],
                                                                args = [var, arg 0]}, arg 1),
                                                  (SRHS.S_Prim {prim = A.Prim.Div,
                                                                tyargs = [],
                                                                args = [var, arg 1]}, arg 0)])
                            | A.Prim.Div => try (fn var => [])
                            | A.Prim.Mod => try (fn var => [])
                            | A.Prim.Neg => try (fn var => 
                                                 [(SRHS.S_Prim {prim = A.Prim.Neg,
                                                                tyargs = [],
                                                                args = [var]}, arg 0)])
                            | A.Prim.Eq => try (fn var =>
                                                [(SRHS.S_Prim {prim = A.Prim.Eq,
                                                               tyargs = [],
                                                               args = [arg 1, arg 0]}, var)])
                            | A.Prim.NEq => try (fn var =>
                                                 [(SRHS.S_Prim {prim = A.Prim.NEq,
                                                                tyargs = [],
                                                                args = [arg 1, arg 0]}, var)])
                            | A.Prim.Lt => try (fn var =>
                                                [(SRHS.S_Prim {prim = A.Prim.Gt,
                                                               tyargs = [],
                                                               args = [arg 1, arg 0]}, var)])
                            | A.Prim.Lte => try (fn var =>
                                                 [(SRHS.S_Prim {prim = A.Prim.Gte,
                                                                tyargs = [],
                                                                args = [arg 1, arg 0]}, var)])
                            | A.Prim.Gt => try (fn var =>
                                                [(SRHS.S_Prim {prim = A.Prim.Lt,
                                                               tyargs = [],
                                                               args = [arg 1, arg 0]}, var)])
                            | A.Prim.Gte => try (fn var =>
                                                 [(SRHS.S_Prim {prim = A.Prim.Lte,
                                                                tyargs = [],
                                                                args = [arg 1, arg 0]}, var)])
                            | A.Prim.Concat => try (fn var => [])
                            | A.Prim.Print => keep rhs
                            | A.Prim.Size => try (fn var => [])
                            | A.Prim.Subscript => try (fn var => [])
                            | A.Prim.Array => let
                                                 val valNum = nextValNum ()
                                                 val env =
                                                    Env.extend
                                                    (Env.singletonVarWithValNum (var, valNum),
                                                     Env.singletonSRHS (SRHS.S_Prim {prim = A.Prim.Len,
                                                                                     tyargs = tyargs,
                                                                                     args = [valNum]},
                                                                        arg 0))
                                              in
                                                 (env, rhs)
                                              end
                            | A.Prim.Len => try (fn var => [])
                            | A.Prim.Idx => keep rhs
                            | A.Prim.Upd => (Env.singletonVar (var, arg 2), rhs)
                            | A.Prim.Argc => try (fn var => [])
                            | A.Prim.Arg => try (fn var => [])
                            | A.Prim.Fail => keep rhs
                        end
                   | A.RHS.R_DaCon {dacon, tyargs, args} =>
                        try (SRHS.S_DaCon
                             {dacon = dacon,
                              tyargs = tyargs,
                              args = List.map (fn arg => lookupVar (env, arg)) args},
                             fn var => [])
                   | A.RHS.R_VApply _ => keep rhs
                   | A.RHS.R_TApply _ => keep rhs
                   | A.RHS.R_Var {var = var'} => (Env.singletonVar' (var, lookupVar' (env, var')), rhs)
                   | A.RHS.R_Integer i => try (SRHS.S_Integer i, fn var => [])
                   | A.RHS.R_String s => try (SRHS.S_String s, fn var => [])
                   | A.RHS.R_Case {arg, matchrules} =>
                        keep (A.RHS.R_Case
                              {arg = arg,
                               matchrules = List.map (fn mr => xformMatchRule (env, arg, mr)) matchrules})
            in
               (env',
                A.Decl.D_Val {var = var, var_ty = var_ty, rhs = rhs})
            end
       | A.Decl.D_Fun {fun_decls} =>
            let
               val env' =
                  List.foldl
                  (fn ({func, ...}, env') =>
                   let
                      val envF = Env.singletonVarWithValNum (func, nextValNum ())
                   in
                      Env.extend (env', envF)
                   end)
                  Env.empty
                  fun_decls
            in
               (env',
                A.Decl.D_Fun {fun_decls = List.map (fn {func, func_ty, lam} =>
                                                    {func = func, func_ty = func_ty,
                                                     lam = xformLam (Env.extend (env, env'), lam)})
                                                   fun_decls})
            end
   and xformDecls (env: Env.t, decls: A.Decl.t list) : Env.t * A.Decl.t list =
      let
         val (decls, env') =
            ListExtra.mapAndFoldl
            (fn (decl_i, env') =>
             let
                val (env'_i, decl_i) =
                   xformDecl (Env.extend (env, env'), decl_i)
             in
                (decl_i,
                 Env.extend (env', env'_i))
             end)
            Env.empty
            decls
      in
         (env', decls)
      end

   fun xform (prog: A.Prog.t) : A.Prog.t =
      let
         val A.Prog.Prog {decls, var, ty} = prog
         val (env', decls) = xformDecls (Env.empty, decls)
      in
         A.Prog.Prog {decls = decls, var = var, ty = ty}
      end
end

structure ValueNum = 
struct
   structure ValueNumOptCtl =
      struct
         datatype t = None | Basic | BasicTbl | BasicEnv | Extd
         val fromString =
            fn "none" => SOME None
             | "basic" => SOME Basic
             | "basic-env" => SOME BasicEnv
             | "basic-tbl" => SOME BasicTbl
             | "extd" => SOME Extd
             | _ => NONE
         val toString =
            fn None => "none"
             | Basic => "basic"
             | BasicEnv => "basic-env"
             | BasicTbl => "basic-tbl"
             | Extd => "extd"
         val value_cvt =
            {tyName = "value-num-ctl",
             fromString = fromString,
             toString = toString}
      end

   val valueNumOptCtl : ValueNumOptCtl.t Controls.control =
      Controls.genControl
      {name = "anf-value-num",
       pri = [],
       obscurity = 1,
       help = "select between <none>, <basic>, and <extd> value numbering optimization",
       default = ValueNumOptCtl.Basic}
   val () =
      ControlRegistry.register Control.topRegistry
      {ctl = Controls.stringControl ValueNumOptCtl.value_cvt valueNumOptCtl,
       envName = NONE}

   fun xform (prog: AnfIR.Prog.t) : AnfIR.Prog.t =
      case Controls.get valueNumOptCtl of
         ValueNumOptCtl.None => prog
       | ValueNumOptCtl.Basic => ValueNumBasic.xform prog
       | ValueNumOptCtl.BasicEnv => ValueNumBasicEnv.xform prog
       | ValueNumOptCtl.BasicTbl => ValueNumBasicTbl.xform prog
       | ValueNumOptCtl.Extd => ValueNumExtd.xform prog
end
