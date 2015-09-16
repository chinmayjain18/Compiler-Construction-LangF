(* langfc-src/anf-ir/interpreter.sml
 *
 * COPYRIGHT (c) 2011-2015 Matthew Fluet (http://www.cs.rit.edu/~mtf)
 * All rights reserved.
 *
 * Rochester Institute of Technology
 * CSCI-742
 * Q20122,S20135,S20145
 *
 * Interpreter for A-normal form intermediate representation.
 *)

structure AnfIRInterpreter :> ANF_IR_INTERPRETER =
struct

   structure A = AnfIR

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

   structure Addr = Id (val defaultName = "&&")

   structure Value_Clos_Env =
      struct
         datatype value =
            V_Integer of IntInf.int
          | V_String of String.string
          | V_DaCon of {dacon: A.DaCon.t, args: value list}
          | V_Clos of clos
          | V_Addr of Addr.t

         and clos =
            C_VClos of {env: env ref, var: A.Var.t, body: A.Exp.t}
          | C_TClos of {env: env ref, body: A.Exp.t}

         and env = Env of value A.Var.Map.map

         fun layoutValue v =
            case v of
               V_Integer i => Layout.str (IntInf.toString i)
             | V_String s => Layout.str (concat ["\"", String.toString s, "\""])
             | V_DaCon {dacon, args} =>
                  let
                     fun layoutDaConArgs args =
                        Layout.optSeq ("{", "}", ",") layoutValue args
                  in
                     Layout.seq [A.DaCon.layout dacon,
                                 Layout.prefixSpaceIfNonEmpty (layoutDaConArgs args)]
                  end
             | V_Clos c => layoutClos c
             | V_Addr a => Addr.layout a
         and layoutClos c =
            case c of
               C_VClos {env, var, body} =>
                  Layout.seq
                  [Layout.str "<<",
                   Layout.str "E",
                   Layout.space,
                   Layout.str ",",
                   Layout.space,
                   Layout.str "fn",
                   Layout.space,
                   A.Var.layout var,
                   Layout.space,
                   Layout.str "=>",
                   Layout.space,
                   Layout.str "...",
                   Layout.str ">>"]
             | C_TClos {env, body} =>
                  Layout.seq
                  [Layout.str "<<",
                   Layout.str "E",
                   Layout.space,
                   Layout.str ",",
                   Layout.space,
                   Layout.str "fn",
                   Layout.space,
                   Layout.str "[",
                   Layout.str "]",
                   Layout.space,
                   Layout.str "=>",
                   Layout.space,
                   Layout.str "...",
                   Layout.str ">>"]
      end

   structure Value =
      struct
         datatype t = datatype Value_Clos_Env.value
         val layout = Value_Clos_Env.layoutValue
      end

   structure Result =
      struct
         datatype t =
            R_Value of Value.t
          | R_Fail of String.string
         fun layout res =
            case res of
               R_Value value =>
                  Value.layout value
             | R_Fail s =>
                  Layout.seq
                  [Layout.str "fail ",
                   Layout.str ("\"" ^ String.toString s ^ "\"")]
         val toString = Layout.toString o layout
         fun output (outStrm, res) = Layout.output (outStrm, Layout.seq [layout res, Layout.str "\n"])
      end

   structure Clos =
      struct
         datatype t = datatype Value_Clos_Env.clos
         val layout = Value_Clos_Env.layoutClos
      end

   structure Env =
      struct
         datatype t = datatype Value_Clos_Env.env

         val empty =
            Env (A.Var.Map.empty)

         fun singletonVar (var, value) =
            Env (A.Var.Map.singleton (var, value))
         fun lookupVar (Env env, var) =
            A.Var.Map.find (env, var)
         fun filterVar (Env env, pred) =
            Env (A.Var.Map.filteri pred env)
         fun domainVar (Env env) =
            A.Var.Set.fromList (A.Var.Map.listKeys env)

         fun extend (Env env1, Env env2) =
            Env (A.Var.Map.unionWith #2 (env1, env2))
      end

   structure Heap =
      struct
         datatype t = Heap of (Value.t Vector.vector) Addr.Map.map

         val empty =
            Heap (Addr.Map.empty)

         fun singletonAddr (addr, vector) =
            Heap (Addr.Map.singleton (addr, vector))
         fun lookupAddr (Heap heap, addr) =
            Addr.Map.find (heap, addr)
         fun domainAddr (Heap heap) =
            Addr.Set.fromList (Addr.Map.listKeys heap)

         fun extend (Heap heap1, Heap heap2) =
            Heap (Addr.Map.unionWith #2 (heap1, heap2))
      end

   fun interpret (prog : A.Prog.t, args : string list) : Result.t =
      let
         val cmdLineArgs = args

         exception LangFFail of string
         exception InterpError

         fun interpLam (env: Env.t, lam: A.Lam.t)
                       : Clos.t =
            let
               val lam_env = ref Env.empty
               val fvs = A.Lam.freeVars lam
               val clos = interpLamAux (lam_env, lam)
               val () = lam_env :=
                  Env.filterVar
                  (env, fn (var, _) =>
                   A.Var.Set.member (fvs, var))
            in
               clos
            end
         and interpLamAux (env: Env.t ref, lam: A.Lam.t)
                          : Clos.t =
            case lam of
               A.Lam.L_VLam {var, var_ty, body} =>
                  Clos.C_VClos {env = env, var = var, body = body}
             | A.Lam.L_TLam {tyvar, body} =>
                  Clos.C_TClos {env = env, body = body}

         fun interpPat (arg: Value.t, pat: A.Pat.t)
                       : Env.t option =
           case pat of
              A.Pat.P_DaCon {dacon = formal_dacon, binds = formal_binds, ...} =>
                 (case arg of
                     Value.V_DaCon {dacon = actual_dacon, args = actual_args, ...} =>
                        if A.DaCon.equals (formal_dacon, actual_dacon)
                           andalso List.length formal_binds = List.length actual_args
                           then let
                                   val env' =
                                      ListPair.foldl
                                      (fn (formal_bind_i, actual_arg_i, env') =>
                                       let
                                          val env'_i =
                                             Env.singletonVar (#var formal_bind_i, actual_arg_i)
                                       in
                                          Env.extend (env', env'_i)
                                       end)
                                      Env.empty
                                      (formal_binds, actual_args)
                                in
                                   SOME env'
                                end
                        else NONE
                   | _ => NONE)
            | A.Pat.P_Var bind =>
                 let
                    val env' =
                       Env.singletonVar (#var bind, arg)
                 in
                    SOME env'
                 end

         fun interpPrim (heap: Heap.t, prim: A.Prim.t, args: Value.t list)
                        : Value.t * Heap.t =
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
                        Value.V_Integer res
                     end
               in
                  fun doBinArith (oper, i1: IntInf.int, i2: IntInf.int) =
                     wrap (oper (i1, i2))
                  fun doUnArith (oper, i: IntInf.int) =
                     wrap (oper i)
               end
               local
                  fun wrap dacon =
                     Value.V_DaCon {dacon = dacon, args = []}
               in
                  fun doCmp (oper, i1: IntInf.int, i2: IntInf.int) =
                     if oper (i1, i2)
                        then wrap A.DaCon.truee
                     else wrap A.DaCon.falsee
               end
            in
               case (prim, args) of
                (* misc primitives *)
                  (A.Prim.Print, [Value.V_String s]) =>
                     let
                        val () = TextIO.print s
                     in
                        (Value.V_DaCon {dacon = A.DaCon.unit, args = []}, heap)
                     end
                | (A.Prim.Fail, [Value.V_String s]) =>
                     let
                        val () = TextIO.print (s ^ "\n")
                     in
                        raise LangFFail s
                     end
                | (A.Prim.Argc, [Value.V_DaCon {dacon, args = []}]) =>
                     if A.DaCon.equals (dacon, A.DaCon.unit)
                        then let
                                val args = cmdLineArgs
                                val argc = IntInf.fromInt (List.length args)
                             in
                                (Value.V_Integer argc, heap)
                             end
                     else raise InterpError
                | (A.Prim.Arg, [Value.V_Integer i]) =>
                     let
                        val args = cmdLineArgs
                        val argc = IntInf.fromInt (List.length args)
                     in
                        if 0 <= i andalso i < argc
                           then let val i = IntInf.toInt i
                                in (Value.V_String (List.nth (args, i)), heap)
                                end
                        else (Value.V_String "", heap)
                     end
                (* array primitives *)
                | (A.Prim.Array, [Value.V_Integer i, v]) =>
                     if i < 0
                        then raise InterpError
                     else let
                             fun mkAddr () =
                                let
                                   val addr = Addr.new "&&"
                                in
                                   case Heap.lookupAddr (heap, addr) of
                                      NONE => addr
                                    | SOME _ => mkAddr ()
                                end
                             val addr = mkAddr ()
                             val vector = Vector.tabulate (IntInf.toInt i, fn _ => v)
                             val heap' = Heap.singletonAddr (addr, vector)
                          in
                             (Value.V_Addr addr, Heap.extend (heap, heap'))
                          end
                | (A.Prim.Len, [Value.V_Addr addr]) =>
                     (case Heap.lookupAddr (heap, addr) of
                         SOME vector => 
                            let
                               val len = Vector.length vector
                            in
                               (Value.V_Integer (IntInf.fromInt len), heap)
                            end
                       | NONE => raise InterpError)
                | (A.Prim.Idx, [Value.V_Addr addr, Value.V_Integer i]) =>
                     (case Heap.lookupAddr (heap, addr) of
                         SOME vector => 
                            if 0 <= i andalso i < IntInf.fromInt (Vector.length vector)
                               then (Vector.sub (vector, IntInf.toInt i), heap)
                            else raise InterpError
                       | NONE => raise InterpError)
                | (A.Prim.Upd, [Value.V_Addr addr, Value.V_Integer i, v]) =>
                     (case Heap.lookupAddr (heap, addr) of
                         SOME vector => 
                            if 0 <= i andalso i < IntInf.fromInt (Vector.length vector)
                               then let
                                       val vector'  =
                                          Vector.update (vector, IntInf.toInt i, v)
                                       val heap' =
                                          Heap.singletonAddr (addr, vector')
                                    in
                                       (v, Heap.extend (heap, heap'))
                                    end
                            else raise InterpError
                       | NONE => raise InterpError)
                (* integer arithmetic primitives *)
                | (A.Prim.Add, [Value.V_Integer i1, Value.V_Integer i2]) =>
                     (doBinArith (IntInf.+, i1, i2), heap)
                | (A.Prim.Sub, [Value.V_Integer i1, Value.V_Integer i2]) =>
                     (doBinArith (IntInf.-, i1, i2), heap)
                | (A.Prim.Mul, [Value.V_Integer i1, Value.V_Integer i2]) =>
                     (doBinArith (IntInf.*, i1, i2), heap)
                | (A.Prim.Div, [Value.V_Integer i1, Value.V_Integer i2]) =>
                     (doBinArith (IntInf.quot, i1, i2), heap)
                | (A.Prim.Mod, [Value.V_Integer i1, Value.V_Integer i2]) =>
                     (doBinArith (IntInf.rem, i1, i2), heap)
                | (A.Prim.Neg, [Value.V_Integer i]) =>
                     (doUnArith (IntInf.~, i), heap)
                (* integer comparision primitives *)
                | (A.Prim.Eq, [Value.V_Integer i1, Value.V_Integer i2]) =>
                     (doCmp (op =, i1, i2), heap)
                | (A.Prim.NEq, [Value.V_Integer i1, Value.V_Integer i2]) =>
                     (doCmp (op <>, i1, i2), heap)
                | (A.Prim.Lt, [Value.V_Integer i1, Value.V_Integer i2]) =>
                     (doCmp (IntInf.<, i1, i2), heap)
                | (A.Prim.Lte, [Value.V_Integer i1, Value.V_Integer i2]) =>
                     (doCmp (IntInf.<=, i1, i2), heap)
                | (A.Prim.Gt, [Value.V_Integer i1, Value.V_Integer i2]) =>
                     (doCmp (IntInf.>, i1, i2), heap)
                | (A.Prim.Gte, [Value.V_Integer i1, Value.V_Integer i2]) =>
                     (doCmp (IntInf.>=, i1, i2), heap)
                (* string primitives *)
                | (A.Prim.Concat, [Value.V_String s1, Value.V_String s2]) =>
                     (Value.V_String (s1 ^ s2), heap)
                | (A.Prim.Size, [Value.V_String s]) =>
                     let val sz = String.size s
                     in (Value.V_Integer (IntInf.fromInt sz), heap)
                     end
                | (A.Prim.Subscript, [Value.V_String s, Value.V_Integer i]) =>
                     if 0 <= i andalso i < IntInf.fromInt (String.size s)
                        then let val c = String.sub (s, IntInf.toInt i)
                             in (Value.V_Integer (IntInf.fromInt (ord c)), heap)
                             end
                     else (Value.V_Integer 0, heap)
                | _ => raise InterpError
            end

         fun interpVar (env: Env.t, var: A.Var.t)
                       : Value.t =
           case Env.lookupVar (env, var) of
              NONE => raise InterpError
            | SOME value => value

         fun interpExp (env: Env.t, heap: Heap.t, exp: A.Exp.t)
                       : Value.t * Heap.t =
           let
              val A.Exp.Exp {decls, var, ...} = exp
              val (env', heap') = interpDecls (env, heap, decls)
           in
              (interpVar (Env.extend (env, env'), var), heap')
           end

         and interpRHS (env: Env.t, heap: Heap.t, rhs: A.RHS.t)
                       : Value.t * Heap.t =
            case rhs of
               A.RHS.R_Fn {lam} => (Value.V_Clos (interpLam (env, lam)), heap)
             | A.RHS.R_Prim {prim, args, ...} =>
                  interpPrim (heap, prim, List.map (fn arg => interpVar (env, arg)) args)
             | A.RHS.R_DaCon {dacon, tyargs, args} =>
                  (Value.V_DaCon {dacon = dacon, args = List.map (fn arg => interpVar (env, arg)) args}, heap)
             | A.RHS.R_VApply {func, arg} =>
                  (case interpVar (env, func) of
                      Value.V_Clos (Clos.C_VClos {env = env_c, var, body = body_c, ...}) =>
                         interpExp (Env.extend (!env_c, Env.singletonVar (var, interpVar (env, arg))), heap, body_c)
                    | _ => raise InterpError)
             | A.RHS.R_TApply {func, tyarg} =>
                  (case interpVar (env, func) of
                      Value.V_Clos (Clos.C_TClos {env = env_c, body = body_c}) =>
                         interpExp (!env_c, heap, body_c)
                    | _ => raise InterpError)
             | A.RHS.R_Var {var} =>
                  (interpVar (env, var), heap)
             | A.RHS.R_Integer i => (Value.V_Integer i, heap)
             | A.RHS.R_String s => (Value.V_String s, heap)
             | A.RHS.R_Case {arg, matchrules} =>
                  interpMatchRules (env, heap, interpVar (env, arg), matchrules)

         and interpMatchRule (env: Env.t, heap: Heap.t, arg: Value.t,
                              matchrule: A.MatchRule.t)
                             : (unit -> Value.t * Heap.t) option =
            case matchrule of
               A.MatchRule.MatchRule {pat, body} =>
                  (case interpPat (arg, pat) of
                      NONE => NONE
                    | SOME env' => SOME (fn () => interpExp (Env.extend (env, env'), heap, body)))
         and interpMatchRules (env: Env.t, heap: Heap.t, arg: Value.t,
                               matchrules: A.MatchRule.t list)
                              : Value.t * Heap.t =
            case matchrules of
               [] => raise Match
             | matchrule::matchrules =>
                  (case interpMatchRule (env, heap, arg, matchrule) of
                      NONE => interpMatchRules (env, heap, arg, matchrules)
                    | SOME th => th ())

         and interpDecl (env: Env.t, heap: Heap.t, decl: A.Decl.t) :
                        Env.t * Heap.t =
            case decl of
               A.Decl.D_Data _ => (Env.empty, heap)
             | A.Decl.D_Val {var, var_ty, rhs} =>
                  let
                     val (value', heap') = interpRHS (env, heap, rhs)
                     val env' = Env.singletonVar (var, value')
                  in
                     (env', heap')
                  end
             | A.Decl.D_Fun {fun_decls} =>
                  let
                     val common_env = ref Env.empty
                     val fvs = A.Decl.freeVars decl
                     val env' =
                        List.foldl
                        (fn ({func, lam, ...}, env') =>
                         let
                            val clos = interpLamAux (common_env, lam)
                            val envF = Env.singletonVar (func, Value.V_Clos clos)
                         in
                            Env.extend (env', envF)
                         end)
                        Env.empty
                        fun_decls
                     val () = common_env :=
                        Env.extend (Env.filterVar
                                    (env,
                                     fn (var, _) =>
                                     A.Var.Set.member (fvs, var)),
                                    env')
                  in
                     (env', heap)
                  end
         and interpDecls (env: Env.t, heap: Heap.t, decls: A.Decl.t list) :
                         Env.t * Heap.t =
           let
               val (env', heap') =
                  List.foldl
                  (fn (decl_i, (env', heap')) =>
                   let
                      val (env'_i, heap'_i) =
                         interpDecl (Env.extend (env, env'), heap', decl_i)
                   in
                      (Env.extend (env', env'_i), heap'_i)
                   end)
                  (Env.empty, heap)
                  decls
            in
               (env', heap')
            end

         fun interpProg (prog: A.Prog.t) : Value.t =
            let
               val A.Prog.Prog {decls, var, ...} = prog
               val (env_d, heap_d) = interpDecls (Env.empty, Heap.empty, decls)
               val value' = interpVar (env_d, var)
            in
               value'
            end

         val res =
            Result.R_Value (interpProg prog)
            handle LangFFail s => Result.R_Fail s
      in
         res
      end

   val interpret =
      Control.mkKeepCtlPass
      {keepPre = NONE,
       keepPost = SOME {output = Result.output,
                        ext = "anf-res"},
       passName = "interpret-anf",
       pass = interpret}

   val interpret =
      Control.mkTracePass
      {msgPre = NONE,
       msgPost = SOME Result.toString,
       passName = "interpret-anf",
       pass = interpret}
end
