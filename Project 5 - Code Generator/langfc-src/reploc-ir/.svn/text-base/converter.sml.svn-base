(* langfc-src/reploc-ir/converter.sml
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
 * Conversion from core intermediate representation to
 * representation&location intermediate representation in the LangF
 * compiler (langfc).
 *)

structure RepLocIRConverter :> REPLOC_IR_CONVERTER =
struct
   structure C = CoreIR
   structure RL = RepLocIR
   structure RL =
      struct
         open RL
         structure TyRep =
            struct
               datatype t = Boxed | Mixed | Unboxed
            end
         structure Slot =
            struct
               open Slot
               fun max (slot1, slot2) = Int.max (slot1, slot2)
               fun next (slots: Set.set) : t =
                  let
                     fun loop i =
                        if Set.member (slots, i)
                           then loop (i + 1)
                        else i
                  in
                     loop 0
                  end
            end
         structure Exp =
            struct
               open Exp
               val unit = E_DaCon {dacon = DaCon.unit, args = []}
            end
      end

   structure Env =
      struct
         structure TyConEnv =
            struct
               type dom = C.TyCon.t
               type cod = {rep: RL.TyRep.t}
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
               type cod = {dacon: RL.DaCon.t}
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
               type cod = {loc: RL.VarLoc.t}
               type t = cod C.Var.Map.map
               val empty : t = C.Var.Map.empty
               val singleton : dom * cod -> t = C.Var.Map.singleton
               val lookup : t * dom -> cod option = C.Var.Map.find
               val extend : t * t -> t = C.Var.Map.unionWith #2
               val domain : t -> C.Var.Set.set =
                  C.Var.Set.fromList o C.Var.Map.listKeys
               val map : (dom * cod -> cod) -> t -> t =
                  C.Var.Map.mapi
               val foldl : (dom * cod * 'b -> 'b) -> 'b -> t -> 'b =
                  C.Var.Map.foldli
               val foldr : (dom * cod * 'b -> 'b) -> 'b -> t -> 'b =
                  C.Var.Map.foldri
               val filter : (dom * cod -> bool) -> t -> t =
                  C.Var.Map.filteri
            end

         datatype t =
            Env of {tyconEnv: TyConEnv.t,
                    daconEnv: DaConEnv.t,
                    varEnv: VarEnv.t}

         val empty =
            Env {tyconEnv = TyConEnv.empty,
                 daconEnv = DaConEnv.empty,
                 varEnv = VarEnv.empty}

         fun fromTyConEnv tyconEnv =
            Env {tyconEnv = tyconEnv,
                 daconEnv = DaConEnv.empty,
                 varEnv = VarEnv.empty}
         val singletonTyCon = fromTyConEnv o TyConEnv.singleton
         fun lookupTyCon (Env {tyconEnv, ...}, tycon) =
            TyConEnv.lookup (tyconEnv, tycon)
         fun domainTyCon (Env {tyconEnv, ...}) =
            TyConEnv.domain tyconEnv

         fun fromDaConEnv daconEnv =
            Env {tyconEnv = TyConEnv.empty,
                 daconEnv = daconEnv,
                 varEnv = VarEnv.empty}
         val singletonDaCon = fromDaConEnv o DaConEnv.singleton
         fun lookupDaCon (Env {daconEnv, ...}, dacon) =
            DaConEnv.lookup (daconEnv, dacon)
         fun domainDaCon (Env {daconEnv, ...}) =
            DaConEnv.domain daconEnv

         fun fromVarEnv varEnv =
            Env {tyconEnv = TyConEnv.empty,
                 daconEnv = DaConEnv.empty,
                 varEnv = varEnv}
         val singletonVar = fromVarEnv o VarEnv.singleton
         fun lookupVar (Env {varEnv, ...}, var) =
            VarEnv.lookup (varEnv, var)
         fun domainVar (Env {varEnv, ...}) =
            VarEnv.domain varEnv
         fun mapVar f (Env {tyconEnv, daconEnv, varEnv}) =
            Env {tyconEnv = tyconEnv,
                 daconEnv = daconEnv,
                 varEnv = VarEnv.map f varEnv}
         fun foldlVar f b (Env {varEnv, ...}) =
            VarEnv.foldl f b varEnv
         fun foldrVar f b (Env {varEnv, ...}) =
            VarEnv.foldr f b varEnv
         fun filterVar f (Env {tyconEnv, daconEnv, varEnv}) =
            Env {tyconEnv = tyconEnv,
                 daconEnv = daconEnv,
                 varEnv = VarEnv.filter f varEnv}

         fun extend (Env {tyconEnv = tyconEnv1,
                          daconEnv = daconEnv1,
                          varEnv = varEnv1},
                     Env {tyconEnv = tyconEnv2,
                          daconEnv = daconEnv2,
                          varEnv = varEnv2}) =
            Env {tyconEnv = TyConEnv.extend (tyconEnv1, tyconEnv2),
                 daconEnv = DaConEnv.extend (daconEnv1, daconEnv2),
                 varEnv = VarEnv.extend (varEnv1, varEnv2)}

         (* The initial environment E_0. *)
         val initial = empty
         (* Pre-defined type constructors. *)
         val initial =
            List.foldl
            (fn ((tycon, item), env) =>
             extend (env, singletonTyCon (tycon, item)))
            initial
            [(C.TyCon.array, {rep = RL.TyRep.Boxed}),
             (C.TyCon.bool, {rep = RL.TyRep.Unboxed}),
             (C.TyCon.integer, {rep = RL.TyRep.Unboxed}),
             (C.TyCon.string, {rep = RL.TyRep.Boxed}),
             (C.TyCon.unit, {rep = RL.TyRep.Unboxed})]
         (* Pre-defined data constructors. *)
         val initial =
            List.foldl
            (fn ((dacon, item), env) => extend (env, singletonDaCon (dacon, item)))
            initial
            [(C.DaCon.falsee, {dacon = RL.DaCon.falsee}),
             (C.DaCon.truee, {dacon = RL.DaCon.truee}),
             (C.DaCon.unit, {dacon = RL.DaCon.unit})]
      end

   fun toStringTyVar tyvar =
      Layout.toString (C.TyVar.layout tyvar)
   fun toStringTyCon tycon =
      Layout.toString (C.TyCon.layout tycon)
   fun toStringDaCon dacon =
      Layout.toString (C.DaCon.layout dacon)
   fun toStringVar var =
      Layout.toString (C.Var.layout var)

   fun convert (prog : C.Prog.t) : RL.Prog.t =
      let
         exception ConvertError
         fun raiseConvertError msgs =
            (List.map (fn msg => (TextIO.output (TextIO.stdErr, msg))) ("Convert Error:\n" :: msgs)
             ; TextIO.output1 (TextIO.stdErr, #"\n")
             ; raise ConvertError)

         val functions : RL.Function.t list ref = ref []

         fun convertType (env : Env.t)
                         (ty : C.Type.t)
                         : RL.TyRep.t =
            case ty of
               C.Type.T_TyFn {tyvar, res} => RL.TyRep.Boxed
             | C.Type.T_Fn {arg, res} => RL.TyRep.Boxed
             | C.Type.T_TyCon {tycon, tyargs} =>
                  (case Env.lookupTyCon (env, tycon) of
                      NONE =>
                         raiseConvertError
                         (["Unbound type constructor: ",
                           toStringTyCon tycon, "."])
                    | SOME {rep, ...} => rep)
             | C.Type.T_TyVar {tyvar} =>
                      RL.TyRep.Mixed

         fun convertParam (env: Env.t)
                          (param: C.Param.t)
                          : Env.t =
            case param of
               C.Param.P_Var {var, var_ty} =>
                  Env.singletonVar (var, {loc = RL.VarLoc.Param})
             | C.Param.P_TyVar {tyvar} =>
                  Env.empty

         fun convertBind (env: Env.t)
                         (bind: {var: C.Var.t, var_ty: C.Type.t})
                         : Env.t * RL.Slot.t =
            case bind of
               {var, ...} =>
                  let
                     val slot =
                        RL.Slot.next
                        (Env.foldlVar
                         (fn (var,{loc},slots) =>
                          case loc of
                             RL.VarLoc.Local slot =>
                                RL.Slot.Set.add (slots, slot)
                           | _ => slots)
                         RL.Slot.Set.empty
                         env)
                     val env' =
                        Env.singletonVar
                        (var, {loc = RL.VarLoc.Local slot})
                  in
                     (env', slot)
                  end

         fun convertPat (env: Env.t)
                        (pat: C.Pat.t)
                        : Env.t * RL.Pat.t * RL.Slot.t =
            case pat of
               C.Pat.P_DaCon {dacon, binds, ...} =>
                  let
                     val dacon =
                        case Env.lookupDaCon (env, dacon) of
                           NONE =>
                              raiseConvertError
                              (["Unbound data constructor: ",
                                toStringDaCon dacon, "."])
                         | SOME {dacon} => dacon
                     val (slots, env') =
                        ListExtra.mapAndFoldl
                        (fn (bind_i, env') =>
                         let
                            val (env'_i, slot_i) =
                               convertBind (Env.extend (env, env')) bind_i
                         in
                            (slot_i, Env.extend (env', env'_i))
                         end)
                        Env.empty
                        binds
                     val maxSlot = List.foldl RL.Slot.max ~1 slots
                  in
                     (env', RL.Pat.P_DaCon {dacon = dacon, slots = slots}, maxSlot)
                  end
             | C.Pat.P_Var bind =>
                  let
                     val (env', slot) =
                        convertBind env bind
                  in
                     (env', RL.Pat.P_Var {slot = slot}, slot)
                  end

         fun convertExp (env: Env.t)
                        (exp: C.Exp.t)
                        : RL.Exp.t * RL.Slot.t =
           case exp of
              C.Exp.Exp {node, ty} => convertExpNode env node
         and convertExpNode (env: Env.t)
                            (node: C.Exp.node)
                            : RL.Exp.t * RL.Slot.t =
            case node of
               C.Exp.E_Fn {lam} =>
                  let
                     (* Turn an anonymous function into a named function. *)
                     val func = C.Var.new "anon"
                     val func_ty =
                        case lam of
                           C.Lam.Lam {param, body} =>
                              let
                                 val res_ty = C.Exp.ty body
                              in
                                 case param of
                                    C.Param.P_Var {var_ty, ...} =>
                                       C.Type.T_Fn {arg = var_ty, res = res_ty}
                                  | C.Param.P_TyVar {tyvar} =>
                                       C.Type.T_TyFn {tyvar = tyvar, res = res_ty}
                              end
                     val fun_decl =
                        {func = func,
                         func_ty = func_ty,
                         lam = lam}
                     val node' =
                        C.Exp.E_Let
                        {decl = C.Decl.D_Fun {fun_decls = [fun_decl]},
                         body = C.Exp.make (C.Exp.E_Var {var = func},
                                            func_ty)}
                  in
                     convertExpNode env node'
                  end
             | C.Exp.E_Prim {prim, args, ...} =>
                  let
                     val prim = prim
                     val (args, args_maxSlot) =
                        ListExtra.mapAndFoldl
                        (fn (arg, maxSlot) =>
                         let
                            val (arg, arg_maxSlot) =
                               convertExp env arg
                         in
                            (arg, RL.Slot.max (maxSlot, arg_maxSlot))
                         end)
                        ~1
                        args
                     val maxSlot = args_maxSlot
                  in
                     (RL.Exp.E_Prim {prim = prim, args = args}, maxSlot)
                  end
             | C.Exp.E_DaCon {dacon, args, ...} =>
                  let
                     val dacon =
                        case Env.lookupDaCon (env, dacon) of
                           NONE =>
                              raiseConvertError
                              (["Unbound data constructor: ",
                                toStringDaCon dacon, "."])
                         | SOME {dacon} => dacon
                     val (args, args_maxSlot) =
                        ListExtra.mapAndFoldl
                        (fn (arg, maxSlot) =>
                         let
                            val (arg, arg_maxSlot) =
                               convertExp env arg
                         in
                            (arg, RL.Slot.max (maxSlot, arg_maxSlot))
                         end)
                        ~1
                        args
                     val maxSlot = args_maxSlot
                  in
                     (RL.Exp.E_DaCon {dacon = dacon, args = args}, maxSlot)
                  end
             | C.Exp.E_Apply {func, applyarg} =>
                  let
                     val (func, func_maxSlot) = convertExp env func
                     val (arg, arg_maxSlot) = convertApplyArg env applyarg
                     val maxSlot = RL.Slot.max (func_maxSlot, arg_maxSlot)
                  in
                     (RL.Exp.E_Apply {func = func, arg = arg}, maxSlot)
                  end
             | C.Exp.E_Var {var} =>
                  (case Env.lookupVar (env, var) of
                      NONE =>
                         raiseConvertError
                         (["Unbound variable: ",
                           toStringVar var, "."])
                    | SOME {loc} => (RL.Exp.E_Var {loc = loc}, ~1))
             | C.Exp.E_Integer i => (RL.Exp.E_Integer i, ~1)
             | C.Exp.E_String s => (RL.Exp.E_String s, ~1)
             | C.Exp.E_Let {decl, body} =>
                  let
                     val (env', decl, decl_maxSlot) = convertDecl env decl
                     val (body, body_maxSlot) = convertExp (Env.extend (env, env')) body
                     val maxSlot = RL.Slot.max (decl_maxSlot, body_maxSlot)
                  in
                     case decl of
                        NONE => (body, maxSlot)
                      | SOME decl => (RL.Exp.E_Let {decl = decl, body = body}, maxSlot)
                  end
             | C.Exp.E_Case {arg, matchrules} =>
                  let
                     val (arg, arg_maxSlot) = convertExp env arg
                     val (matchrules, matchrules_maxSlot) =
                        ListExtra.mapAndFoldl
                        (fn (matchrule, maxSlot) =>
                         let
                            val (matchrule, matchrule_maxSlot) =
                               convertMatchRule env matchrule
                         in
                            (matchrule, RL.Slot.max (maxSlot, matchrule_maxSlot))
                         end)
                        ~1
                        matchrules
                     val maxSlot = RL.Slot.max (arg_maxSlot, matchrules_maxSlot)
                  in
                     (RL.Exp.E_Case {arg = arg, matchrules = matchrules}, maxSlot)
                  end
         and convertLam (env: Env.t)
                        (func: RL.Func.t)
                        (lam: C.Lam.t)
                        : unit =
            case lam of
               C.Lam.Lam {param, body} =>
                  let
                     val env' =
                        convertParam env param
                     val (body, maxSlot) =
                        convertExp (Env.extend (env, env')) body
                     val function =
                        RL.Function.Function
                        {func = func,
                         nLocals = maxSlot + 1,
                         body = body}
                  in
                     functions := function :: !functions
                  end
         and convertApplyArg (env: Env.t)
                             (applyarg: C.ApplyArg.t)
                             : RL.Exp.t * RL.Slot.t =
            case applyarg of
               C.ApplyArg.A_Exp exp => convertExp env exp
             | C.ApplyArg.A_Type _ => (RL.Exp.unit, ~1)
         and convertMatchRule (env: Env.t)
                              (matchrule: C.MatchRule.t)
                              : RL.MatchRule.t * RL.Slot.t =
            case matchrule of
               C.MatchRule.MatchRule {pat, body} =>
                  let
                     val (env', pat, pat_maxSlot) = convertPat env pat
                     val (body, body_maxSlot) = convertExp (Env.extend (env, env')) body
                     val maxSlot = RL.Slot.max (pat_maxSlot, body_maxSlot)
                  in
                     (RL.MatchRule.MatchRule {pat = pat, body = body}, maxSlot)
                  end
         and convertDecl (env: Env.t)
                         (decl: C.Decl.t)
                         : Env.t * RL.Decl.t option * RL.Slot.t =
            case decl of
               C.Decl.D_Data {data_decls} =>
                  let
                     val env' =
                        List.foldl
                        (fn ({tycon, tyvars, dacon_decls}, env') =>
                         let
                            val (env', tag) =
                               List.foldl
                               (fn ({dacon, arg_tys}, (env', tag)) =>
                                let
                                   val dacon_name =
                                      RL.DaConName.new (C.DaCon.name dacon)
                                   val dacon_rep =
                                      case arg_tys of
                                         [] => RL.DaConRep.UnboxedTag tag
                                       | _ => RL.DaConRep.TaggedBox tag
                                   val env'' =
                                      Env.singletonDaCon
                                      (dacon, {dacon = RL.DaCon.DaCon {name = dacon_name,
                                                                      rep = dacon_rep}})
                                in
                                   (Env.extend (env', env''),
                                    tag + 1)
                                end)
                               (env', 0)
                               dacon_decls
                            val tycon_rep = RL.TyRep.Mixed
                            val env'' =
                               Env.singletonTyCon
                               (tycon, {rep = tycon_rep})
                         in
                            Env.extend (env', env'')
                         end)
                        Env.empty
                        data_decls
                  in
                     (env', NONE, ~1)
                  end
             | C.Decl.D_Val {var, var_ty, exp} =>
                  let
                     val (exp, exp_maxSlot) = convertExp env exp
                     val (env', slot) =
                        convertBind env {var = var, var_ty = var_ty}
                     val maxSlot = RL.Slot.max (exp_maxSlot, slot)
                  in
                     (env', SOME (RL.Decl.D_Val {slot = slot, exp = exp}), maxSlot)
                  end
             | C.Decl.D_Fun {fun_decls} =>
                  let
                     val free_vars =
                        C.Decl.freeVars decl
                     val (inner_env, _) =
                        C.Var.Set.foldl
                        (fn (var,(inner_env,i)) =>
                         let
                            val env' =
                               Env.singletonVar (var, {loc = RL.VarLoc.Global i})
                         in
                            (Env.extend (inner_env, env'),
                             i + 1)
                         end)
                        (Env.filterVar (fn _ => false) env, 0)
                        free_vars
                     val outer_env =
                        C.Var.Set.foldr
                        (fn (var,outer_env) =>
                         let
                            val env' =
                               #loc (valOf (Env.lookupVar (env, var)))
                               handle _ => raise Fail (toStringVar var)
                         in
                            env'::outer_env
                         end)
                        []
                        free_vars
                     val (fun_decls, (env', inner_env', maxSlot)) =
                        ListExtra.mapAndFoldl
                        (fn ({func, func_ty, lam}, (env', inner_env', maxSlot)) =>
                         let
                            val func' = RL.Func.new (C.Var.name func)
                            val (envF,slotF) =
                               convertBind (Env.extend (env, env'))
                                           {var = func, var_ty = func_ty}
                            val inner_envF =
                               Env.singletonVar (func, {loc = RL.VarLoc.Self func'})
                         in
                            ({slot = slotF, func = func', lam = lam},
                             (Env.extend (env', envF),
                              Env.extend (inner_env', inner_envF),
                              RL.Slot.max (maxSlot, slotF)))
                         end)
                        (Env.empty,Env.empty,~1)
                        fun_decls
                     val inner_env = Env.extend (inner_env, inner_env')
                     val fun_decls =
                        List.map
                        (fn {slot, func, lam} =>
                         let
                            val () =
                               convertLam inner_env func lam
                         in
                            {slot = slot,
                             func = func}
                         end)
                        fun_decls
                  in
                     (env',
                      SOME (RL.Decl.D_Fun {env = outer_env, fun_decls = fun_decls}),
                      maxSlot)
                  end

         fun convertProg (prog: C.Prog.t) : RL.Prog.t =
            let
               val env0 = Env.initial
               val C.Prog.Prog {decls, exp} = prog
(*
               val exp =
                  C.Exp.make
                  (C.Exp.E_Let
                   {decl =
                    C.Decl.D_Val
                    {var = C.Var.new "z", 
                     var_ty = C.Exp.ty
                     exp, exp = exp},
                    body =
                    C.Exp.make
                    (C.Exp.E_DaCon
                     {dacon = C.DaCon.unit,
                      tyargs = [],
                      args = []},
                     C.Type.unit)},
                   C.Type.unit)
*)
               val exp =
                  List.foldr
                  (fn (decl, exp) =>
                   C.Exp.make 
                   (C.Exp.E_Let {decl = decl, body = exp},
                    C.Exp.ty exp))
                  exp
                  decls
(*
               val prog = C.Prog.Prog {decls = [], exp = exp}
               val _ = CoreIRTypeChecker.typeCheck (errStrm, prog)
*)
               val (exp, maxSlot) = convertExp env0 exp
               val mainFunc = RL.Func.new "main"
               val main =
                  RL.Function.Function
                  {func = mainFunc,
                   nLocals = maxSlot + 1,
                   body = exp}
            in
               RL.Prog.Prog {main = mainFunc, functions = main::(List.rev (!functions))}
            end
      in
         convertProg prog
      end


   val convert =
      Control.mkKeepCtlPass
      {keepPre = NONE,
       keepPost = SOME {output = RepLocIR.Prog.output,
                        ext = "reploc"},
       passName = "convert-to-reploc",
       pass = convert}

   val convert =
      Control.mkTracePass
      {msgPre = NONE,
       msgPost = NONE,
       passName = "convert-to-reploc",
       pass = convert}
end
