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
   fun toStringPatName pat =
      Layout.toString (PT.SimplePat.layout pat)

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
      fun toStringASTType ty =
         maybeElide (Layout.toString (AST.Type.layout ty))
      fun toStringPat pat =
         maybeElide (Layout.toString (PT.Pat.layout pat))
      fun toStringExp exp =
         maybeElide (Layout.toString (PT.Exp.layout exp))
      fun toStringMatchRule matchrule =
         maybeElide (Layout.toString (PT.MatchRule.layout matchrule))
      fun toStringDecl decl =
         maybeElide (Layout.toString (PT.Decl.layout decl))
   end

   fun toStringTypeAST tyAST =
      Layout.toString (AST.Type.layout tyAST)
   fun toStringExpPT tyAST =
      Layout.toString (PT.Exp.layout tyAST)

   fun typeCheck (errStrm: ErrorStream.t,
                  prog : PT.Prog.t) : AST.Prog.t option =
      let

         (* By raising an exception upon reporting an error, we have a
          * type checker that stops after reporting the first error.
          * This makes the type checker easier to write, but doesn't
          * provide much feedback to the user.  After you have your
          * complete type checker working, consider improving your
          * type checker to continue after detecting the first error
          * and report multiple errors.
          *)
         exception TypeError
         fun addErrorAt (span, msgs) =
            (ErrorStream.addErrorAt (errStrm, span, concat ("\n" :: msgs))
             ; raise TypeError)

         (*
          * 5.1 Types
          *) 
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
                          Env.singletonTyVar (tyvar, {tyvar = tyvarAST})),
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
                     fun checkArity takes =
                        let
                           val given = List.length actual_tyargs
                        in
                           if given = takes
                              then ()
                           else
                              addErrorAt
                              (PT.Type.span ty,
                               ["Type constructor ",
                                toStringTyConName tycon,
                                " given ",
                                Int.toString given,
                                " type arguments but takes ",
                                Int.toString takes,
                                " type arguments.\n",
                                "  in: ", toStringType ty])
                        end
                  in
                     case Env.lookupTyCon (env, tycon) of
                        NONE =>
                           addErrorAt
                           (PT.TyConName.span tycon,
                            ["Unbound type constructor: ",
                             toStringTyConName tycon, "."])
                      | SOME (Env.TyConEnv.Type 
                              {tyvars = formal_tyvarsAST, ty = tyAST}) =>
                           let
                              val arity = List.length formal_tyvarsAST
                              val () = checkArity arity
                              val actual_tyargsAST = checkTypes (env, actual_tyargs)
                           in
                              AST.Type.substs (tyAST,
                                               ListPair.zip (actual_tyargsAST,
                                                             formal_tyvarsAST))
                           end
                      | SOME (Env.TyConEnv.Data {arity, tycon = tyconAST}) =>
                           let
                              val () = checkArity arity
                              val actual_tyargsAST = checkTypes (env, actual_tyargs)
                           in
                              AST.Type.T_TyCon (tyconAST, actual_tyargsAST)
                           end
                        end
             | PT.Type.T_TyVar tyvar =>
                  (case Env.lookupTyVar (env, tyvar) of
                      NONE =>
                         addErrorAt
                         (PT.TyVarName.span tyvar,
                          ["Unbound type variable: ",
                           toStringTyVarName tyvar, "."])
                    | SOME {tyvar = tyvarAST} =>
                         AST.Type.T_TyVar tyvarAST)
         and checkTypes (env: Env.t, tys: PT.Type.t list)
                        : AST.Type.t list =
            List.map (fn ty => checkType (env, ty)) tys

         (*
          * 5.2 Parameters
          *
          *
          * For simple binding checking:
          *  checkParam : Env.t * PT.Param.t -> Env.t
          *
          * For type checking, without producing an abstract syntax tree:
          *  checkParam : Env.t * PT.Param.t -> Env.t * (AST.Type.t -> AST.Type.t)
          *
          * For type checking and producing an abstract syntax tree:
          *  checkParam : Env.t * PT.Param.t -> Env.t * (AST.Type.t -> AST.Type.t) * AST.Param.t
          *)
         fun checkParam (env: Env.t, param: PT.Param.t)
		: Env.t * (AST.Type.t -> AST.Type.t) * AST.Param.t =
            case PT.Param.node param of
               PT.Param.P_VarName (var, var_ty) =>
                  let
                     val var_tyAST = checkType (env, var_ty)
		     val varAST = AST.Var.new (toStringVarName var)
                     val env' =
                        Env.singletonVar (var, {var_ty = var_tyAST, var = varAST})
                     val mkTy = fn res_tyAST =>
                        AST.Type.T_Fn (var_tyAST, res_tyAST)
                  in
                     (env', mkTy, AST.Param.P_Var(varAST, var_tyAST))
                  end
             | PT.Param.P_TyVarName tyvar =>
                  let
                     val tyvarAST = AST.TyVar.new (toStringTyVarName tyvar)
                     val env' =
                        Env.singletonTyVar (tyvar, {tyvar = tyvarAST})
                     val mkTy = fn res_tyAST =>
                        AST.Type.T_TyFn (tyvarAST, res_tyAST)
                  in
                     (env', mkTy, AST.Param.P_TyVar(tyvarAST))
                  end

         (*
          * 5.2.1 Multiple Parameters
          *
          *
          * For simple binding checking:
          *  checkParams : Env.t * PT.Param.t list -> Env.t
          *
          * For type checking, without producing an abstract syntax tree:
          *  checkParams : Env.t * PT.Param.t list -> Env.t * (AST.Type.t -> AST.Type.t)
          *
          * For type checking and producing an abstract syntax tree:
          *  checkParams : Env.t * PT.Param.t list -> Env.t * (AST.Type.t -> AST.Type.t) * AST.Param.t list
          *)
         fun checkParams (env: Env.t, params: PT.Param.t list)
		: Env.t * (AST.Type.t -> AST.Type.t) * AST.Param.t list =
            let
               val (env', mkTy, paramListAST) =
                  List.foldl
                  (fn (param_i, (env', mkTy, paramAST)) =>
                   let
                      val (env'_i, mkTy_i, paramAST_i) =
                         checkParam (Env.extend (env, env'), param_i)
                   in
                      (Env.extend (env', env'_i),
                       mkTy o mkTy_i, paramAST@[paramAST_i])
                   end)
                  (Env.empty, fn ty => ty, [])
                  params
            in
               (env', mkTy, paramListAST)
            end

         (*
          * 5.3.1 Simple Patterns
          *
          *
          * For simple binding checking:
          *  checkSimplePat : Env.t * PT.SimplePat.t -> Env.t
          *
          * For type checking, without producing an abstract syntax tree:
          *  checkSimplePat : Env.t * AST.Type.t * PT.SimplePat.t -> Env.t
          *
          * For type checking and producing an abstract syntax tree:
          *  checkSimplePat : Env.t * AST.Type.t * PT.SimplePat.t -> Env.t * AST.SimplePat.t
          *)
         fun checkSimplePat (env: Env.t, astType:AST.Type.t, pat: PT.SimplePat.t)
		: Env.t * AST.SimplePat.t =
            case PT.SimplePat.node pat of
               PT.SimplePat.P_VarName var =>
                  let
                    val varAST = AST.Var.new(toStringVarName var) 
		    val env' =
                        Env.singletonVar (var, {var_ty = astType, var = varAST})
                  in
                     (env', AST.SimplePat.P_Var(varAST, astType))
                  end
             | PT.SimplePat.P_Wild =>
                  let
                     val env' = Env.empty
                  in
                     (env', AST.SimplePat.P_Wild(astType))
                  end

         (*
          * 5.3.2 Patterns
          *
          *
          * For simple binding checking:
          *  checkPat : Env.t * PT.Pat.t -> Env.t
          *
          * For type checking, without producing an abstract syntax tree:
          *  checkPat : Env.t * AST.Type.t * PT.Pat.t -> Env.t
          *
          * For type checking and producing an abstract syntax tree:
          *  checkPat : Env.t * AST.Type.t * PT.Pat.t -> Env.t * AST.Pat.t
          *)
         fun checkPat (env: Env.t, astType: AST.Type.t, pat: PT.Pat.t) 
		: Env.t * AST.Pat.t =
            case PT.Pat.node pat of
               PT.Pat.P_DaCon (dacon, actual_tyargs, actual_pats) =>
                  (case Env.lookupDaCon (env, dacon) of
                      NONE =>
                         addErrorAt
                         (PT.DaConName.span dacon,
                          ["Unbound data constructor: ",
                           toStringDaConName dacon, "."])
                    | SOME {dacon = daconAST, tyvars = formal_tyvarsAST,
                            arg_tys = formal_arg_tysAST, tycon = tyconAST} =>
                         let
			    val () = if (List.length actual_pats) = (List.length formal_arg_tysAST)
				     then ()
				     else (addErrorAt(PT.DaConName.span dacon, ["Number of arguments used do not match"]))
			    val actual_tyargsAST = checkTypes (env, actual_tyargs)
			    val (a,b) = case astType of
					AST.Type.T_TyCon(a,b) => (a,b)
					| _ => (addErrorAt(PT.DaConName.span dacon, ["Data declaration not defined correctly"]))
			    val () = if AST.TyCon.equals (a, tyconAST)
				     then ()
				     else (addErrorAt(PT.DaConName.span dacon, ["Data contructor types mismatch"]))
			    val () = if ListPair.all AST.Type.equals (actual_tyargsAST, b)
				     then ()
				     else (addErrorAt(PT.DaConName.span dacon, ["Data contructor types mismatch"]))
			    val zipList = ListPair.zip(actual_tyargsAST, formal_tyvarsAST)
			    val subList = AST.Type.substsL(formal_arg_tysAST, zipList)
			    val foldLZipList = ListPair.zip(actual_pats, subList)
                            val (env', patList') =
                               List.foldl
                               (fn ((actual_pat_i, subType_i), (env',patASTList)) =>
                                let
                                   val (env'_i,patAST_i) = checkSimplePat (env, subType_i, actual_pat_i)
                                in
                                   (Env.extend (env', env'_i), patASTList@[patAST_i])
                                end)
                               (Env.empty, [])
                               foldLZipList
			    val boolVal = let
				fun check (patList: PT.SimplePat.t list) = (case patList of
									     (hd::tl) => if (List.exists (fn (name) => 
									                 ((toStringPatName name) = (toStringPatName hd) andalso (toStringPatName hd) <> ("_"))) tl) orelse check tl
			      						  	      then (addErrorAt(PT.DaConName.span dacon, ["Pattern Names are not unique"]))
										      else false 

									      | nil => false )
			       in
				 check actual_pats
			       end
			    val patAST = AST.Pat.P_DaCon(daconAST, actual_tyargsAST, patList')
                         in
                            (env', patAST)
                         end )
             | PT.Pat.P_SimplePat spat =>
                  let
                     val (env', simplePatAST) = checkSimplePat (env, astType, spat)
		     val patAST = AST.Pat.P_SimplePat(simplePatAST)
                  in
                     (env', patAST)
                  end


         (*
          * 5.6.3 Data Constructor Declarations
          *)
         fun checkDaConDecl (env: Env.t,
                             tyvarsAST: AST.TyVar.t list,
                             tyconAST: AST.TyCon.t,
                             dacon_decl: PT.DaConName.t * PT.Type.t list)
                            : Env.t * (AST.DaCon.t * AST.Type.t list) =
            let
               val (dacon, arg_tys) =  dacon_decl
               val daconAST = AST.DaCon.new (toStringDaConName dacon)
               val arg_tysAST = checkTypes (env, arg_tys)
               val env' =
                  Env.singletonDaCon (dacon, {tyvars = tyvarsAST,
                                              arg_tys = arg_tysAST,
                                              tycon = tyconAST,
                                              dacon = daconAST})
            in
               (env',
                (daconAST, arg_tysAST))
            end
         fun checkDaConDecls (env: Env.t,
                              tyvarsAST: AST.TyVar.t list,
                              tyconAST: AST.TyCon.t,
                              dacon_decls: (PT.DaConName.t * PT.Type.t list) list)
                             : Env.t * (AST.DaCon.t * AST.Type.t list) list =
            let
               val (dacon_declsAST, env') =
                  ListExtra.mapAndFoldl
                  (fn (dacon_decl_i, env') =>
                   let
                      val (env'_i, dacon_declAST_i) =
                         checkDaConDecl (env,
                                         tyvarsAST,
                                         tyconAST,
                                         dacon_decl_i)
                   in
                      (dacon_declAST_i,
                       (Env.extend (env', env'_i)))
                   end)
                  Env.empty
                  dacon_decls
            in
               (env',
                dacon_declsAST)
            end

         (*
          * 5.4 Expressions
          *
          *
          * For simple binding checking:
          *  checkExp : Env.t * PT.Exp.t -> unit
          *
          * For type checking, without producing an abstract syntax tree:
          *  checkExp : Env.t * PT.Exp.t -> AST.Type.t
          *
          * For type checking and producing an abstract syntax tree:
          *  checkExp : Env.t * PT.Exp.t -> AST.Type.t * AST.Exp.t
          *)
         fun checkExp (env: Env.t, exp: PT.Exp.t)
		: AST.Type.t * AST.Exp.t =
            (case PT.Exp.node exp of
               PT.Exp.E_Fn (params, body) =>
                  let
                     val (env', mkTy, paramASTList) = checkParams (env, params)
                     val (fnType, funcExp) = checkExp (Env.extend (env, env'), body)
		     val funcExpAST = AST.Exp.make(AST.Exp.E_Fn(paramASTList, funcExp), mkTy fnType)
                  in
                     (mkTy fnType, funcExpAST)
                  end
             | PT.Exp.E_If (expIf, expThen, expElse) =>
                  let
                     val (ifTypeAST,ifExp) = checkExp (env, expIf)
                     val (thenTypeAST,thenExp) = checkExp (env, expThen)
                     val (elseTypeAST,elseExp) = checkExp (env, expElse)
		     val ifAST = AST.Exp.make(AST.Exp.E_If(ifExp,thenExp,elseExp), thenTypeAST)
                  in
		     if AST.Type.equals (AST.Type.bool, ifTypeAST)
		     then ( if AST.Type.equals (thenTypeAST, elseTypeAST)
		            then (thenTypeAST,ifAST)
		     	    else (addErrorAt (PT.Exp.span expThen, ["Return type of the 'then' and 'else' expression do not match"]))
			  )
		     else (addErrorAt (PT.Exp.span expIf, ["IF expression should return a boolean type"]))
                  end
             | PT.Exp.E_Orelse (expl, expr) =>
                  let
                     val (lTypeAST,lExpAST) = checkExp (env, expl)
                     val (rTypeAST,rExpAST) = checkExp (env, expr)
		     val orAST = AST.Exp.make(AST.Exp.E_Orelse(lExpAST,rExpAST), AST.Type.bool)
                  in
                     if AST.Type.equals (AST.Type.bool, lTypeAST)
		     then ( if AST.Type.equals (AST.Type.bool, rTypeAST)
		     	    then (rTypeAST, orAST) 
		     	    else (addErrorAt (PT.Exp.span expr, ["Return type of the right expression should be boolean"]))
			  )
		     else (addErrorAt (PT.Exp.span expl, ["Return type of the left expression should be boolean"]))
                  end
             | PT.Exp.E_Andalso (expl, expr) =>
                  let
                     val (lTypeAST,lExpAST) = checkExp (env, expl)
                     val (rTypeAST,rExpAST) = checkExp (env, expr)
		     val andAST = AST.Exp.make(AST.Exp.E_Andalso(lExpAST,rExpAST), AST.Type.bool)
                  in
                     if AST.Type.equals (AST.Type.bool, lTypeAST)
		     then ( if AST.Type.equals (AST.Type.bool, rTypeAST)
		     	    then (rTypeAST, andAST) 
		     	    else (addErrorAt (PT.Exp.span expr, ["Return type of the right expression should be boolean"]))
			  )
		     else (addErrorAt (PT.Exp.span expl, ["Return type of the left expression should be boolean"]))
                  end
             | PT.Exp.E_Constraint (body, ty) =>
                  let
                     val (expTypeAST,bodyAST) = checkExp (env, body)
                     val tyAST = checkType (env, ty)
                  in
                     if AST.Type.equals (expTypeAST, tyAST)
		     then (expTypeAST, bodyAST)
		     else (addErrorAt (PT.Exp.span body, ["Type mismatch in the constraint expression"]))
                  end
             | PT.Exp.E_TernOp (ternop, expl, expm, expr) =>
                  let
                     val (lTypeAST,lExpAST) = checkExp (env, expl)
                     val (mTypeAST,mExpAST) = checkExp (env, expm)
                     val (rTypeAST,rExpAST) = checkExp (env, expr)
		     val ternAST = AST.Exp.make(AST.Exp.E_TernOp(AST.TernOp.Upd(rTypeAST),lExpAST,mExpAST,rExpAST), rTypeAST)
                  in
                     case lTypeAST of
			AST.Type.T_TyCon(a,hd::tl) => if AST.Type.equals (AST.Type.integer, mTypeAST)
			    			      then( if AST.Type.equals (hd, rTypeAST)
							    then (rTypeAST, ternAST)
							    else(addErrorAt(PT.Exp.span expr, ["Type mismatch"]))
							   )
			    			      else(addErrorAt(PT.Exp.span expm, ["Type mismatch. Integer expected"]))
			| _ => (addErrorAt(PT.Exp.span expl, ["Type mismatch. Array expected "]))
                  end
             | PT.Exp.E_BinOp (binop, expl, expr) =>
                  let
                     val (lTypeAST,lExpAST) = checkExp (env, expl)
                     val (rTypeAST,rExpAST) = checkExp (env, expr)
                  in
		     (case binop of
			PT.BinOp.Eq => let
					val typeAST = logicalOpHelper (lTypeAST, rTypeAST, expl, expr)
					val binExpAST = AST.Exp.make(AST.Exp.E_BinOp(AST.BinOp.Eq,lExpAST,rExpAST), typeAST)
				       in
					(typeAST, binExpAST)
				       end
			| PT.BinOp.NEq => let
					   val typeAST = logicalOpHelper (lTypeAST, rTypeAST, expl, expr)
					   val binExpAST = AST.Exp.make(AST.Exp.E_BinOp(AST.BinOp.NEq, lExpAST, rExpAST), typeAST)
					  in
					   (typeAST, binExpAST)
					  end
			| PT.BinOp.Lt => let
					  val typeAST = logicalOpHelper (lTypeAST, rTypeAST, expl, expr)
					  val binExpAST = AST.Exp.make(AST.Exp.E_BinOp(AST.BinOp.Lt, lExpAST, rExpAST), typeAST)
					 in
					  (typeAST, binExpAST)
					 end
			| PT.BinOp.Lte => let
					   val typeAST = logicalOpHelper (lTypeAST, rTypeAST, expl, expr)
					   val binExpAST = AST.Exp.make(AST.Exp.E_BinOp(AST.BinOp.Lte, lExpAST, rExpAST), typeAST)
					  in
					   (typeAST, binExpAST)
					  end
			| PT.BinOp.Gt => let
					  val typeAST = logicalOpHelper (lTypeAST, rTypeAST, expl, expr)
					  val binExpAST = AST.Exp.make(AST.Exp.E_BinOp(AST.BinOp.Gt, lExpAST, rExpAST), typeAST)
					 in
					  (typeAST, binExpAST)
					 end
			| PT.BinOp.Gte => let
					   val typeAST = logicalOpHelper (lTypeAST, rTypeAST, expl, expr)
					   val binExpAST = AST.Exp.make(AST.Exp.E_BinOp(AST.BinOp.Gte, lExpAST, rExpAST), typeAST)
					  in
					   (typeAST, binExpAST)
					  end
			| PT.BinOp.Add => let
					   val typeAST = arithmaticOpHelper (lTypeAST, rTypeAST, expl, expr)
					   val binExpAST = AST.Exp.make(AST.Exp.E_BinOp(AST.BinOp.Add, lExpAST, rExpAST), typeAST)
					  in
					   (typeAST, binExpAST)
					  end
			| PT.BinOp.Sub => let 
					   val typeAST = arithmaticOpHelper (lTypeAST, rTypeAST, expl, expr)
					   val binExpAST = AST.Exp.make(AST.Exp.E_BinOp(AST.BinOp.Sub, lExpAST, rExpAST), typeAST)
					  in
					   (typeAST, binExpAST)
					  end
			| PT.BinOp.Mul => let
					   val typeAST = arithmaticOpHelper (lTypeAST, rTypeAST, expl, expr)
					   val binExpAST = AST.Exp.make(AST.Exp.E_BinOp(AST.BinOp.Mul, lExpAST, rExpAST), typeAST)
					  in
					   (typeAST, binExpAST)
					  end
			| PT.BinOp.Div => let
					   val typeAST = arithmaticOpHelper (lTypeAST, rTypeAST, expl, expr)
					   val binExpAST = AST.Exp.make(AST.Exp.E_BinOp(AST.BinOp.Div, lExpAST, rExpAST), typeAST)
					  in
					   (typeAST, binExpAST)
					  end
			| PT.BinOp.Mod => let
					   val typeAST = arithmaticOpHelper (lTypeAST, rTypeAST, expl, expr)
					   val binExpAST = AST.Exp.make(AST.Exp.E_BinOp(AST.BinOp.Mod, lExpAST, rExpAST), typeAST)
					  in
					   (typeAST, binExpAST)
					  end
			| PT.BinOp.Concat => let
					      val typeAST = concatOpHelper (lTypeAST, rTypeAST, expl, expr)
					      val binExpAST = AST.Exp.make(AST.Exp.E_BinOp(AST.BinOp.Concat, lExpAST, rExpAST), typeAST)
					     in
					      (typeAST, binExpAST)
					     end
			| PT.BinOp.Idx => let
					   val typeAST = idxOpHelper (lTypeAST, rTypeAST, expl, expr)
					   val binExpAST = AST.Exp.make(AST.Exp.E_BinOp(AST.BinOp.Idx(typeAST), lExpAST, rExpAST), typeAST)
					  in
					   (typeAST, binExpAST)
					  end)
		end
             | PT.Exp.E_UnOp (unop, expo) =>
                  let
                     val (eTypeAST,expoAST) = checkExp (env, expo)
                  in
		     (case unop of
			  PT.UnOp.Neg => if AST.Type.equals (AST.Type.integer, eTypeAST)
				  	 then (eTypeAST, AST.Exp.make(AST.Exp.E_UnOp(AST.UnOp.Neg,expoAST), eTypeAST))
				  	 else (addErrorAt (PT.Exp.span expo, ["Return type of the expression should be integer"]))
			| PT.UnOp.Len => case eTypeAST of
					    AST.Type.T_TyCon(a,hd::tl) => 
						(AST.Type.integer, AST.Exp.make(AST.Exp.E_UnOp(AST.UnOp.Len(hd),expoAST), AST.Type.integer))
					    | _ => (addErrorAt(PT.Exp.span expo, ["Return type of the expression should be an array"])) )
                  end
             | PT.Exp.E_DaCon (dacon, actual_tyargs, actual_args) =>
                  (case Env.lookupDaCon (env, dacon) of
                      NONE =>
                         addErrorAt
                         (PT.DaConName.span dacon,
                          ["Unbound data constructor: ",
                           toStringDaConName dacon, "."])
                    | SOME {dacon = daconAST, tyvars = formal_tyvarsAST,
                            arg_tys = formal_arg_tysAST, tycon = tyconAST} =>
                         let
                            val actual_tyargsAST = checkTypes (env, actual_tyargs)
                            val tempList = checkExps (env, actual_args)
			    val (expTypeList, expAST) = ListPair.unzip(tempList)
			    val zipList = ListPair.zip(actual_tyargsAST, formal_tyvarsAST)
			    val subList = AST.Type.substsL(formal_arg_tysAST, zipList)
			    val () = if (List.length actual_args) = (List.length formal_arg_tysAST)
				     then ()
				     else (addErrorAt(PT.DaConName.span dacon, ["Number of arguments used do not match"]))
			    val () = if ListPair.all AST.Type.equals (expTypeList, subList)
				     then ()
				     else (addErrorAt(PT.DaConName.span dacon, ["Types in expression list do not match"]))
			    val daConTypeAST = AST.Type.T_TyCon(tyconAST, actual_tyargsAST)
			    val daConExp = AST.Exp.make(AST.Exp.E_DaCon(daconAST, actual_tyargsAST, expAST), daConTypeAST)
                         in
                            (daConTypeAST, daConExp)
                         end)
             | PT.Exp.E_Apply (func, applyarg) =>
                  let
                     val (funcType, funcExpAST) = checkExp (env, func)
                  in
                     case PT.ApplyArg.node applyarg of
                        PT.ApplyArg.A_Exp exp =>
                           let
                              val (expTypeAST, expAST) = checkExp (env, exp)
                           in
			     case funcType of
				AST.Type.T_Fn(a,b) => if AST.Type.equals (a, expTypeAST)
			      			      then (b, AST.Exp.make(AST.Exp.E_Apply(funcExpAST, AST.ApplyArg.A_Exp(expAST)), b))
			         		      else (addErrorAt(PT.Exp.span func, ["Type mismatch. Function types do not match"]))
				| _ => (addErrorAt(PT.Exp.span func, ["Function type not found in Apply Function Expression ",toStringASTType funcType]))
                           end
                      | PT.ApplyArg.A_Type ty =>
                           let
                              val tyAST = checkType (env, ty)
                           in
			      case funcType of
				AST.Type.T_TyFn(a,b) => (AST.Type.subst (b, (tyAST,a)), 
							  AST.Exp.make(AST.Exp.E_Apply(funcExpAST, AST.ApplyArg.A_Type(tyAST)), AST.Type.subst (b, (tyAST,a))))
				| _ => (addErrorAt(PT.Exp.span func, ["Function type not found in Apply Type Expression AST = ",toStringASTType funcType," Expected = ",toStringExpPT func]))
                           end
                  end
             | PT.Exp.E_VarName var =>
                  (case Env.lookupVar (env, var) of
                      NONE => addErrorAt(PT.VarName.span var,["Unbound variable: ",toStringVarName var, "."])
                    | SOME { var_ty = varTyAST, var = var_ty } => (varTyAST, AST.Exp.make(AST.Exp.E_Var(var_ty), varTyAST)) )
             | PT.Exp.E_Integer i => if ( (i >= ~(IntInf.pow(2,62))) andalso (i < (IntInf.pow(2,62))) )
				     then (AST.Type.integer, AST.Exp.make(AST.Exp.E_Integer(i), AST.Type.integer))
				     else (addErrorAt(PT.Exp.span exp, ["Integer value out of bounds."]))
             | PT.Exp.E_String s => (AST.Type.string, AST.Exp.make(AST.Exp.E_String(s), AST.Type.string))
             | PT.Exp.E_Seq exps =>
                  let
		     val tempList = checkExps (env, exps)
                     val (expNTypeAST, expListAST) = ListPair.unzip(tempList)
     		     val lastType = List.last(expNTypeAST)
		     val seqAST = AST.Exp.make(AST.Exp.E_Seq(expListAST), lastType)
                  in
                     (lastType, seqAST)
                  end
             | PT.Exp.E_Let (decls, bodyExps) => 
                  let
                     val (env', declsAST) = checkDecls (env, decls)
                     val (bodyexpTypeASTList,bodyExpListAST) = ListPair.unzip(checkExps (Env.extend (env, env'), bodyExps))
		     val lastTypeAST = List.last(bodyexpTypeASTList)
		     val firstBodyExp = List.nth(bodyExps,0)
		     val () = if AST.TyCon.Set.isSubset(AST.Type.tycons (lastTypeAST), Env.tycons(env))
			      then ()
			      else (addErrorAt(PT.Exp.span firstBodyExp, ["Type not defined in the environment"]))
		     val letExpAST = AST.Exp.make(AST.Exp.E_Let(declsAST, bodyExpListAST), lastTypeAST)
                  in
		    (lastTypeAST , letExpAST)
                  end
             | PT.Exp.E_Case (arg, matchrules) => 
                  let
                     val (argTypeAST, argExpAST) = checkExp (env, arg)
                     val (rulesTypeAST, ruleList) = checkMatchRules (env, argTypeAST, matchrules)
		     val caseExpAST = AST.Exp.make(AST.Exp.E_Case(argExpAST, ruleList), rulesTypeAST)
                  in
                     (rulesTypeAST, caseExpAST)
                  end )
	 and logicalOpHelper (lType: AST.Type.t, rType: AST.Type.t, expl: PT.Exp.t, expr: PT.Exp.t) =
	   if AST.Type.equals (AST.Type.integer, lType)
	   then ( if AST.Type.equals (AST.Type.integer, rType)
		  then (AST.Type.bool)
		  else (addErrorAt(PT.Exp.span expr, ["Type mismatch in Logical Expression(R). Integer expected "]))
		)
	   else (addErrorAt(PT.Exp.span expl, ["Type mismatch in Logical Expression(L). Integer expected "]))

	 and arithmaticOpHelper (lType: AST.Type.t, rType: AST.Type.t, expl: PT.Exp.t, expr: PT.Exp.t) =
	   if AST.Type.equals (AST.Type.integer, lType)
	   then ( if AST.Type.equals (AST.Type.integer, rType)
		  then (AST.Type.integer)
		  else (addErrorAt(PT.Exp.span expr, ["Type mismatch in Arithmatic Expression(R). Integer expected "]))
		)
	   else (addErrorAt(PT.Exp.span expl, ["Type mismatch in Arithmatic Expression(L). Integer expected "]))

	 and idxOpHelper (lType: AST.Type.t, rType: AST.Type.t, expl: PT.Exp.t, expr: PT.Exp.t) =
	   case lType of
	     AST.Type.T_TyCon(a,hd::tl) => ( if AST.Type.equals (AST.Type.integer, rType)
		  			     then (hd)
		  			     else (addErrorAt(PT.Exp.span expr, ["Type mismatch in IDX Expression(R). Integer expected "]))
				      	    )
	   | _ => (addErrorAt(PT.Exp.span expl, ["Type mismatch in IDX Expression(L)."]))

	 and concatOpHelper (lType: AST.Type.t, rType: AST.Type.t, expl: PT.Exp.t, expr: PT.Exp.t) =
	   if AST.Type.equals (AST.Type.string, lType)
	   then ( if AST.Type.equals (AST.Type.string, rType)
		  then (AST.Type.string)
		  else (addErrorAt(PT.Exp.span expr, ["Type mismatch in Concat Expression(R). Integer expected "]))
		)
	   else (addErrorAt(PT.Exp.span expl, ["Type mismatch in Concat Expression(L). Integer expected "]))
         (*
          * For simple binding checking:
          *  checkExps : Env.t list * PT.Exp.t -> unit list
          *
          * For type checking, without producing an abstract syntax tree:
          *  checkExps : Env.t * PT.Exp.t list -> AST.Type.t list
          *
          * For type checking and producing an abstract syntax tree:
          *  checkExps : Env.t * PT.Exp.t list -> (AST.Type.t * AST.Exp.t) list
          *)
         and checkExps (env: Env.t, exps: PT.Exp.t list) : (AST.Type.t * AST.Exp.t) list =
            List.map (fn exp => checkExp (env, exp)) exps
         (*
          * 5.5 Match Rules
          *
          *
          * For simple binding checking:
          *  checkMatchRule : Env.t * PT.MatchRule.t -> unit
          *
          * For type checking, without producing an abstract syntax tree:
          *  checkMatchRule : Env.t * AST.Type.t * PT.MatchRule.t -> AST.Type.t
          *
          * For type checking and producing an abstract syntax tree:
          *  checkMatchRule : Env.t * AST.Type.t * PT.MatchRule.t -> AST.Type.t * AST.MatchRule.t
          *)
         and checkMatchRule (env: Env.t, ty:AST.Type.t, matchrule: PT.MatchRule.t)
		: AST.Type.t * AST.MatchRule.t =
            case PT.MatchRule.node matchrule of
               (pat, body) =>
                  let
                     val (env',patAST) = checkPat (env, ty, pat)
                     val (bodyTypeAST,bodyExpAST) = checkExp (Env.extend (env, env'), body)
		     val matchRuleAST = (AST.MatchRule.MatchRule(patAST, bodyExpAST))
                  in
                     (bodyTypeAST, matchRuleAST)
                  end
         (*
          * 5.5.1 Multiple Match Rules
          *
          *
          * For simple binding checking:
          *  checkMatchRules : Env.t * PT.MatchRule.t list -> unit
          *
          * For type checking, without producing an abstract syntax tree:
          *  checkMatchRules : Env.t * AST.Type.t -> PT.MatchRule.t list -> AST.Type.t
          *
          * For type checking and producing an abstract syntax tree:
          *  checkMatchRules : Env.t * AST.Type.t -> PT.MatchRule.t list -> AST.Type.t * AST.MatchRule.t list
          *)
         and checkMatchRules (env: Env.t, ty:AST.Type.t, matchrules: PT.MatchRule.t list) =
            let
               val tempList = List.map (fn matchrule => checkMatchRule (env, ty, matchrule)) matchrules
	       val (typeList, ruleASTList) = ListPair.unzip (tempList)
	       val matchrule = List.nth(matchrules,0)
	       val typeSingle = List.nth(typeList,0)
            in
               (matchRuleTypeHelper (typeList, matchrule, typeSingle), ruleASTList)
            end
	 and matchRuleTypeHelper (typeList : AST.Type.t list, matchrule: PT.MatchRule.t, astType: AST.Type.t) =
	    case typeList of
		hd :: tl => (if AST.Type.equals ((matchRuleTypeHelper (tl,matchrule,astType)), hd)
			     then hd
			     else (addErrorAt(PT.MatchRule.span matchrule, ["Match Rule Types mismatch"])) )
		| nil => astType
         (*
          * 5.6 Declarations
          *)
         and checkDecl (env: Env.t, decl: PT.Decl.t)
                       : Env.t * AST.Decl.t option =
            case PT.Decl.node decl of
               (*
                * 5.6.1 Type Declarations
                *)
               PT.Decl.D_Type (tycon, tyvars, ty) =>
                  let
                     val tyvarSet =
                        List.foldl
                        (fn (tyvar, tyvarSet) =>
                         let
                            val () =
                               if PT.TyVarName.Set.member (tyvarSet, tyvar)
                                  then
                                     addErrorAt
                                     (PT.TyVarName.span tyvar,
                                      ["Type parameter of 'type' defined multiple times.\n",
                                       "  type parameter: ", toStringTyVarName tyvar, "\n",
                                       "  type constructor: ", toStringTyConName tycon, "\n",
                                       "  in: ", toStringDecl decl])
                               else ()
                            val tyvarSet = PT.TyVarName.Set.add (tyvarSet, tyvar)
                         in
                            tyvarSet
                         end)
                         PT.TyVarName.Set.empty
                         tyvars
                     val (tyvarsAST, envTV) =
                        ListExtra.mapAndFoldl
                        (fn (tyvar, envTV) =>
                         let
                            val tyvarAST =
                               AST.TyVar.new (toStringTyVarName tyvar)
                            val envTV_i =
                               Env.singletonTyVar (tyvar, {tyvar = tyvarAST})
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
                     val (tyconSet, daconSet) =
                        List.foldl (fn (data_decl, (tyconSet, daconSet)) =>
                                    let
                                       val (tycon, tyvars, dacon_decls) = data_decl
                                       val () =
                                          if PT.TyConName.Set.member (tyconSet, tycon)
                                             then
                                                addErrorAt
                                                (PT.TyConName.span tycon,
                                                 ["Type constructor of 'datatype' defined multiple times.\n",
                                                  "  type constructor: ", toStringTyConName tycon, "\n",
                                                  "  in: ", toStringDecl decl])
                                          else ()
                                       val tyconSet = PT.TyConName.Set.add (tyconSet, tycon)
                                       val tyvarSet =
                                          List.foldl
                                          (fn (tyvar, tyvarSet) =>
                                           let
                                              val () =
                                                 if PT.TyVarName.Set.member (tyvarSet, tyvar)
                                                    then
                                                       addErrorAt
                                                       (PT.TyVarName.span tyvar,
                                                        ["Type parameter of 'datatype' defined multiple times.\n",
                                                         "  type parameter: ", toStringTyVarName tyvar, "\n",
                                                         "  type constructor: ", toStringTyConName tycon, "\n",
                                                         "  in: ", toStringDecl decl])
                                                 else ()
                                              val tyvarSet = PT.TyVarName.Set.add (tyvarSet, tyvar)
                                           in
                                              tyvarSet
                                           end)
                                          PT.TyVarName.Set.empty
                                          tyvars
                                       val daconSet =
                                          List.foldl
                                          (fn (dacon_decl, daconSet) =>
                                           let
                                              val (dacon, arg_tys) = dacon_decl
                                              val () =
                                                 if PT.DaConName.Set.member (daconSet, dacon)
                                                    then
                                                       addErrorAt
                                                       (PT.DaConName.span dacon,
                                                        ["Data constructor of 'datatype' defined multiple times.\n",
                                                         "  data constructor: ", toStringDaConName dacon, "\n",
                                                         "  type constructor: ", toStringTyConName tycon, "\n",
                                                         "  in: ", toStringDecl decl])
                                                 else ()
                                              val daconSet = PT.DaConName.Set.add (daconSet, dacon)
                                           in
                                              daconSet
                                           end)
                                          daconSet
                                          dacon_decls
                                    in
                                       (tyconSet, daconSet)
                                    end)
                                  (PT.TyConName.Set.empty, PT.DaConName.Set.empty)
                                  data_decls
                     val (data_declsAux, envTC) =
                        ListExtra.mapAndFoldl
                        (fn ((tycon, tyvars, dacon_decls), envTC) =>
                         let
                            val tyconAST = AST.TyCon.new (toStringTyConName tycon)
                            val arity = List.length tyvars
                            val envTC_i =
                               Env.singletonTyConData (tycon, {arity = arity, tycon = tyconAST})
                         in
                            ((tyconAST, tyvars, dacon_decls),
                             Env.extend (envTC, envTC_i))
                         end)
                        Env.empty
                        data_decls
                     val (data_declsAST, env') =
                        ListExtra.mapAndFoldl
                        (fn ((tyconAST, tyvars, dacon_decls), env') =>
                         let
                            val (tyvarsAST, envTV_i) =
                               ListExtra.mapAndFoldl
                               (fn (tyvar, envTV) =>
                                let
                                   val tyvarAST =
                                      AST.TyVar.new (toStringTyVarName tyvar)
                                   val envTV_i =
                                      Env.singletonTyVar (tyvar, {tyvar = tyvarAST})
                                in
                                   (tyvarAST,
                                    Env.extend (envTV, envTV_i))
                                end)
                               Env.empty
                               tyvars
                            val (envDC_i, dacon_declsAST) =
                               checkDaConDecls (Env.extend (env, (Env.extend (envTC, envTV_i))),
                                                tyvarsAST,
                                                tyconAST,
                                                dacon_decls)
                         in
                            ((tyconAST, tyvarsAST, dacon_declsAST),
                             Env.extend (env', envDC_i))
                         end)
                        envTC
                        data_declsAux
                  in
                     (env',
                      SOME (AST.Decl.D_Data data_declsAST))
                  end
               (*
                * 5.6.4 Value Declarations
                *)
             | PT.Decl.D_Val (pat, constr_tyOpt, exp) =>
                  let
                     val (expTypeAST, expAST) = checkExp (env, exp)
                     val constr_tyAST =
                        case constr_tyOpt of
                           NONE => ()
                         | SOME constr_ty => 
                              let
                                 val constr_tyAST = checkType (env, constr_ty)
                              in
                                 if AST.Type.equals (constr_tyAST, expTypeAST)
				 then ()
				 else (addErrorAt(PT.Exp.span exp, ["Type mismatch in val declarations"]))
                              end
                     val (env', patAST) = checkSimplePat (env, expTypeAST, pat)
                  in
                     (env', SOME (AST.Decl.D_Val(patAST, expAST)))
                  end
               (*
                * 5.6.5 Function Declarations
                *)
             | PT.Decl.D_Fun fun_decls => 
                  let
                     val (fun_declsAux, (env', varASTList, paramList, resTyList, varList)) =
                        ListExtra.mapAndFoldl
                        (fn ((var, params, res_ty, body), (env', varAST', paramAST, resAST, var')) =>
                         let
                            val (envP, mkTy, paramASTList) = checkParams (env, params)
                            val res_tyAST = checkType (Env.extend (env, envP), res_ty)
			    val varAST = AST.Var.new (toStringVarName var)
                            val env'_i = Env.singletonVar (var, {var_ty = mkTy res_tyAST, var = varAST})
                         in
                            ((envP, body),
                             (Env.extend (env', env'_i), varAST'@[varAST], paramAST@[paramASTList], resAST@[res_tyAST], var'@[var]))
                         end)
                        (Env.empty, [], [], [], [])
                        fun_decls
		     val (w,x,y,z) = List.nth(fun_decls,0)
		     val boolVal = let
				fun check (varList:PT.VarName.t list) = (case varList of
									  (hd::tl) => if (List.exists (fn (name) => ((toStringVarName name) = (toStringVarName hd))) tl) andalso check tl
			      						  	      then (addErrorAt(PT.Exp.span z, ["Function Names are not unique"]))
										      else true 

									| nil => true )
			       in
				 check varList
			       end
                     val eBodyASTList =
                        List.map
                        (fn (envP, body) => checkExp (Env.extend (env, Env.extend (env', envP)),
                                         body)) fun_declsAux
		     val tempList = ListPair.zip(ListPair.zip(varASTList, paramList), eBodyASTList)
		     val funDeclsASTList = List.map (fn ((a,b),(c,d)) => (a,b,c,d)) tempList
		     val (a,b) = ListPair.unzip(eBodyASTList)
		     val () = if ListPair.all AST.Type.equals(resTyList,a)
			      then ()
			      else (addErrorAt(PT.Exp.span z, ["Return Type and Expression Type do not match"]))
                  in
                     (env',
                      SOME (AST.Decl.D_Fun funDeclsASTList))
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
               val (decls, exp) = PT.Prog.node prog
               val (env', declsAST) = checkDecls (env0, decls)
               (*val expTypeAST = checkExp (Env.extend (env0, env'), exp)
               val progAST = AST.Prog.Prog (declsAST, AST.Exp.unit)*)
               val (tyAST, expAST) = checkExp (Env.extend (env0, env'), exp)
               val progAST = AST.Prog.Prog (declsAST, expAST)
            in
               SOME progAST
            end
            handle TypeError => NONE
      in
         checkProg prog
      end

end
