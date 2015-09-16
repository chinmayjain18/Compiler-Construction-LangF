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

structure ValueNum : ANF_IR_OPTIMIZATION =
struct
   structure A = AnfIR
   val nextNum = let val ctr = ref 0
		 in fn () => (ctr := !ctr + 1 ; !ctr)
		 end
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
	 structure SrhsEnv =
	    struct
	       type dom = t
               type cod = Int.int
               type t = cod Map.map
               val empty : t = Map.empty
               val singleton : dom * cod -> t = Map.singleton
               val lookup : t * dom -> cod option = Map.find
               val extend : t * t -> t = Map.unionWith #2
	    end
	    datatype t =
	       Env of {srhsEnv: SrhsEnv.t}

            val empty =
               Env {srhsEnv = SrhsEnv.empty}

            fun fromSRhsEnv srhsEnv =
               Env {srhsEnv = srhsEnv}
            val singletonSrhs = fromSRhsEnv o SrhsEnv.singleton
            fun lookupVar (Env {srhsEnv, ...}, var) =
               SrhsEnv.lookup (srhsEnv, var)

            fun extend (Env {srhsEnv = srhsEnv1},
               		Env {srhsEnv = srhsEnv2}) =
                  Env {srhsEnv = SrhsEnv.extend (srhsEnv1, srhsEnv2)}
      end

   structure Env =
      struct
	 
	 structure OrdKey =
            struct
               type ord_key = A.Var.t
               val compare = A.Var.compare
            end
         structure Map : ORD_MAP where type Key.ord_key = A.Var.t = RedBlackMapFn (OrdKey)

	 structure RevOrdKey =
	    struct
	       type ord_key = Int.int
	       val compare = Int.compare
	    end
	 structure RevMap : ORD_MAP where type Key.ord_key = Int.int = RedBlackMapFn (RevOrdKey)

         structure RhsVarEnv =
            struct
               type dom = A.Var.t
               type cod = Int.int
               type t = cod Map.map
               val empty : t = Map.empty
               val singleton : dom * cod -> t = Map.singleton
               val lookup : t * dom -> cod option = Map.find
               val extend : t * t -> t = Map.unionWith #2
            end

	 structure RevRhsVarEnv =
            struct
               type dom = Int.int
               type cod = A.Var.t
               type t = cod RevMap.map
               val empty : t = RevMap.empty
               val singleton : dom * cod -> t = RevMap.singleton
               val lookup : t * dom -> cod option = RevMap.find
               val extend : t * t -> t = RevMap.unionWith #2
            end

         datatype t =
            Env of {rhsVarEnv: RhsVarEnv.t, revRhsVarEnv: RevRhsVarEnv.t}

         val empty =
            Env {rhsVarEnv = RhsVarEnv.empty, revRhsVarEnv = RevRhsVarEnv.empty}
	 
	 fun extend (Env {rhsVarEnv = rhsVarEnv1, revRhsVarEnv = revRhsVarEnv1},
                     Env {rhsVarEnv = rhsVarEnv2, revRhsVarEnv = revRhsVarEnv2}) =
            Env {rhsVarEnv = RhsVarEnv.extend (rhsVarEnv1, rhsVarEnv2),
		 revRhsVarEnv = RevRhsVarEnv.extend (revRhsVarEnv1, revRhsVarEnv2)}
         
	 fun fromRhsVarEnv rhsVarEnv =
            Env {rhsVarEnv = rhsVarEnv, revRhsVarEnv = RevRhsVarEnv.empty}
         val singletonVar = fromRhsVarEnv o RhsVarEnv.singleton
         fun lookupVar (Env {rhsVarEnv, ...}, var) =
            RhsVarEnv.lookup (rhsVarEnv, var)

         fun fromRevRhsVarEnv revRhsVarEnv =
            Env {rhsVarEnv = RhsVarEnv.empty, revRhsVarEnv = revRhsVarEnv}
         val singletonRevVar = fromRevRhsVarEnv o RevRhsVarEnv.singleton
         fun lookupRevVar (Env {revRhsVarEnv, ...}, var) =
            RevRhsVarEnv.lookup (revRhsVarEnv, var)

      end

   structure SRHS = SymbolicRHS

   fun xformExp (env: Env.t, srhsenv: SRHS.SrhsEnv.t, exp: A.Exp.t) : A.Exp.t =
      case exp of
         A.Exp.Exp {decls, var, ty} =>
            let
               val (decls', (env', srhsenv')) = xformDecls (env, srhsenv, decls)
            in
               A.Exp.Exp {decls = decls',
                          var = var,
                          ty = ty}
            end

   and xformPat(env: Env.t, srhsenv:SRHS.SrhsEnv.t, pat: A.Pat.t) : Env.t =
      case pat of
	  A.Pat.P_DaCon {dacon, tyargs, binds} => (let
					       	    fun buildNewEnv (env', binds') : Env.t =
						      (case binds' of
						        {var=var,var_ty=var_ty}::tl => (let
							                       		  val varNum = nextNum ()
									    		  val envNew = Env.singletonVar(var, varNum)
											  val revEnvNew = Env.singletonRevVar(varNum, var)
											  val envNew' = Env.extend(envNew, revEnvNew)
		     									  val envNewR = Env.extend(env', envNew') 
						             		 		in
									    		  buildNewEnv(envNewR, tl)
									 		end )
							  | nil => env' )
					      	  in
					            buildNewEnv(env, binds)
					          end )
	| A.Pat.P_Var {var,var_ty} => let 
					val nextVarNum = nextNum ()
					val envNew = Env.singletonVar(var, nextVarNum)
					val revEnvNew = Env.singletonRevVar(nextVarNum, var)
					val envNew' = Env.extend(envNew, revEnvNew)
				      in Env.extend(env, envNew')
				      end

   and xformMatchRule (env: Env.t, srhsenv:SRHS.SrhsEnv.t, matchrule: A.MatchRule.t) : A.MatchRule.t =
      case matchrule of
         A.MatchRule.MatchRule {pat, body} =>(let
						val newEnv = xformPat(env, srhsenv, pat)
					      in	
						A.MatchRule.MatchRule{pat=pat, body = xformExp(newEnv, srhsenv, body)}
					      end )	

   and xformLam (env: Env.t, srhsenv: SRHS.SrhsEnv.t, lam: A.Lam.t) : A.Lam.t =
      case lam of
         A.Lam.L_VLam {var, var_ty, body} => let
					       	val nextVarNum = nextNum ()
						val envNew = Env.singletonVar(var, nextVarNum)
						val revEnvNew = Env.singletonRevVar(nextVarNum, var)
						val envNew' = Env.extend(envNew, revEnvNew)
						val env' = Env.extend(env, envNew')
					     in
            A.Lam.L_VLam {var = var, var_ty = var_ty, body = xformExp(env', srhsenv, body)}
					     end
       | A.Lam.L_TLam {tyvar, body} =>
            A.Lam.L_TLam {tyvar = tyvar, body = xformExp (env, srhsenv, body)}
	
   and xformDecl(env: Env.t, srhsenv: SRHS.SrhsEnv.t, decl: A.Decl.t) : A.Decl.t * Env.t * SRHS.SrhsEnv.t =
      case decl of
	A.Decl.D_Val {var, var_ty, rhs} =>
	     (let
		val (rhs', env', srhsenv') = (case rhs of
		   	     A.RHS.R_Integer i => (let
						     val srhs = SRHS.S_Integer(i)
						     val rhsVarNum = SRHS.SrhsEnv.lookup(srhsenv, srhs)
						     val (rhs', env', srhsenv') = case rhsVarNum of
								    SOME num => let
										  val revVar' = Env.lookupRevVar(env, num)
										  val srhsNew = case revVar' of
												  SOME var' => var'
												 | NONE => raise Fail ("SRHS lookup failed")
										  val rhs' = A.RHS.R_Var {var=srhsNew}
										  val envNew = Env.singletonVar(var, num)
										  val revEnvNew = Env.singletonRevVar(num, var)
										  val env' = Env.extend(envNew, revEnvNew)
										in
										  (rhs', env', srhsenv)
										end
 								   | NONE => let
										val newNum' = nextNum()
										val envNew = Env.singletonVar(var, newNum')
										val revEnvNew = Env.singletonRevVar(newNum', var)
										val env' = Env.extend(envNew, revEnvNew)
										val srhsenv' = SRHS.SrhsEnv.singleton(srhs, newNum')
									     in
									       (rhs, env', srhsenv')
									     end
					 	   in
					             (rhs', env', srhsenv')
					           end )
		  	  | A.RHS.R_Var {var = varP} => (let
						    val rhsVarNum = Env.lookupVar(env, varP)
						    val (envNew,rhsVarNum') = (case rhsVarNum of
							   	      SOME num => let
										    val envNew = Env.singletonVar(var, num)
										    val revEnvNew = Env.singletonRevVar(num, var)
										    val env' = Env.extend(envNew, revEnvNew)
										  in
										    (env', num)
										  end
					
								    | NONE => raise Fail (("ICE: Var not found "^Layout.toString(A.Decl.layout decl)^Layout.toString(A.Var.layout var))))				
					          in
						    (rhs, envNew, srhsenv)
					          end ) 
			  | A.RHS.R_Prim {prim, tyargs, args} => (let
								    val rhsArgList = List.map(fn (var) => let
													    val var' = Env.lookupVar(env, var)
													  in
													    case var' of
													      SOME num => num
													     | NONE => raise Fail "Arg Lookup Failed"
													  end
											      ) args
								    fun xformPrim(rhs:A.RHS.t) = (let 
								    val srhs = SRHS.S_Prim({prim=prim, tyargs=tyargs, args=rhsArgList})
								    val srhsNum = SRHS.SrhsEnv.lookup(srhsenv, srhs)
								    val (srhsenvNew,num,rhsNew) = (case srhsNum of
										      SOME num => let
												    val srhsNew = Env.lookupRevVar(env, num)
												    val rhs' = case srhsNew of
												  NONE => raise Fail "SRHS look up fail"
												| SOME var' => A.RHS.R_Var{var=var'}
												  in
												    (srhsenv,num, rhs')
												  end
										      | NONE =>(let
											 val srhsNum' = nextNum()
												in 
							(SRHS.SrhsEnv.singleton(srhs, srhsNum'),srhsNum',rhs)
												end) )
								    val envNew = Env.singletonVar(var, num)
							            val revEnvNew = Env.singletonRevVar(num, var)
								    val env' = Env.extend(envNew, revEnvNew)
								  in
								    (rhsNew, env', srhsenvNew)
								  end)
								  val (rhs', envNew', srhsEnvNew') = (case prim of
										  A.Prim.Add => xformPrim(rhs)
										| A.Prim.Div => xformPrim(rhs)
										| A.Prim.Eq => xformPrim(rhs)
										| A.Prim.Gt => xformPrim(rhs)
										| A.Prim.Gte => xformPrim(rhs)
										| A.Prim.Lt => xformPrim(rhs)
										| A.Prim.Lte => xformPrim(rhs)
										| A.Prim.Mod => xformPrim(rhs)
										| A.Prim.Mul => xformPrim(rhs)
										| A.Prim.NEq => xformPrim(rhs)
										| A.Prim.Neg => xformPrim(rhs)
										| A.Prim.Sub => xformPrim(rhs)
										| _ =>	let
											  val newNum = nextNum()
											  val env1 = Env.singletonVar(var, newNum)
											  val env2 = Env.singletonRevVar(newNum, var)
											  val envR = Env.extend(env1, env2)
											in
											  (rhs, envR, srhsenv)
											end)
						in (rhs', envNew',srhsEnvNew')
					end )
			  | A.RHS.R_Fn {lam} =>let 
						 val nextVarNum = nextNum ()
						 val envNew = Env.singletonVar(var, nextVarNum)
						 val revEnvNew = Env.singletonRevVar(nextVarNum, var)
						 val envNew' = Env.extend(envNew, revEnvNew)
					       in (A.RHS.R_Fn {lam = xformLam (env, srhsenv, lam)}, envNew', srhsenv)
					       end
			  | A.RHS.R_VApply {func, arg} =>let
							   val num = nextNum()
							   val envNew = Env.singletonVar(var, num)
						           val revEnvNew = Env.singletonRevVar(num, var)
							   val envR = Env.extend(envNew, revEnvNew)
							   val env' = Env.extend(env, envR)
							 in
							   (A.RHS.R_VApply{func = func, arg = arg}, env', srhsenv)
							 end
			  | A.RHS.R_TApply {func, tyarg} =>let
							     val num = nextNum()
							     val envNew = Env.singletonVar(var, num)
						             val revEnvNew = Env.singletonRevVar(num, var)
							     val envR = Env.extend(envNew, revEnvNew)
							     val env' = Env.extend(env, envR)
							   in
				       			     (A.RHS.R_TApply{func = func, tyarg = tyarg}, env, srhsenv)
							   end
			  | A.RHS.R_Case {arg, matchrules} => let 
								val nextVarNum = nextNum ()
								val envNew = Env.singletonVar(var, nextVarNum)
								val revEnvNew = Env.singletonRevVar(nextVarNum, var)
								val envNew' = Env.extend(envNew, revEnvNew)
								val env' = Env.extend(env, envNew')					
								val num = nextNum()
								val envNew = Env.singletonVar(var, num)
						                val revEnvNew = Env.singletonRevVar(num, var)
							        val envR = Env.extend(envNew, revEnvNew)
							      in
           			(A.RHS.R_Case {arg = arg, matchrules = List.map (fn mr => xformMatchRule (env', srhsenv, mr)) matchrules}, envR, srhsenv)
							      end
			  | _ => let
				   val nextVarNum = nextNum ()
				   val envNew = Env.singletonVar(var, nextVarNum)
	  		 	   val revEnvNew = Env.singletonRevVar(nextVarNum, var)
				   val envNew' = Env.extend(envNew, revEnvNew)
				 in (rhs, envNew', srhsenv)
				 end
			)
	      in
		(A.Decl.D_Val {var = var, var_ty = var_ty, rhs = rhs'}, env', srhsenv')
	      end)
	| A.Decl.D_Data {data_decls} =>
            (A.Decl.D_Data {data_decls = data_decls},
             Env.empty, SRHS.SrhsEnv.empty)
	| A.Decl.D_Fun {fun_decls} => let
					val newEnv = (let
						       fun buildNewEnv (env, fun_decls') : Env.t =
							(case fun_decls' of
							   {func,func_ty,lam}::tl => (let
											val varNum = nextNum ()
											val envNew = Env.singletonVar(func, varNum)
										        val revEnvNew = Env.singletonRevVar(varNum, func)
										        val env' = Env.extend(envNew, revEnvNew)
											val envNew' = Env.extend(env, env') 
										      in
											buildNewEnv(envNew', tl)
										      end )
							  | nil => env )
						      in
						       buildNewEnv(env, fun_decls)
						      end )
				      in
					((A.Decl.D_Fun {fun_decls = List.map (fn {func, func_ty, lam} =>
                                                 {func = func,
                                                  func_ty = func_ty,
                                                  lam = xformLam (newEnv, srhsenv, lam)})
                                                fun_decls}), newEnv, srhsenv)
				      end

   and xformDecls(env: Env.t, srhsenv: SRHS.SrhsEnv.t, decls: A.Decl.t list) : A.Decl.t list * (Env.t * SRHS.SrhsEnv.t)  =
      ListExtra.mapAndFoldl
      (fn (decl_i, (env', srhsenv')) =>
       let
          val (decl_i', env_i' ,srhsenv_i') = xformDecl (Env.extend (env, env'), SRHS.SrhsEnv.extend(srhsenv, srhsenv'), decl_i)
       in 
          (decl_i', (Env.extend (env', env_i'), SRHS.SrhsEnv.extend(srhsenv',srhsenv_i')))
       end)
      (Env.empty, SRHS.SrhsEnv.empty)
      decls

   and xform (prog: A.Prog.t) : A.Prog.t =
      let
         val A.Prog.Prog {decls, var, ty} = prog
         val env0 = Env.empty
	 val srhs0 = SRHS.SrhsEnv.empty
         val (decls', (env', srhs')) = xformDecls (env0, srhs0, decls)
      in
         A.Prog.Prog {decls = decls',
                      var = var,
                      ty = ty}
      end
end
