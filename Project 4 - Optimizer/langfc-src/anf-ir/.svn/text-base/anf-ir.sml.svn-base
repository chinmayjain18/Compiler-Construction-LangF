(* langfc-src/anf-ir/core-ir.sml
 *
 * COPYRIGHT (c) 2015 Matthew Fluet (http://www.cs.rit.edu/~mtf)
 * All rights reserved.
 *
 * Rochester Institute of Technology
 * CSCI-742
 * S20135,S20145
 *
 * A-normal form intermediate representation in the LangF compiler (langfc).
 *)

structure AnfIR :> ANF_IR where type Prim.t = CoreIR.Prim.t =
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
          * so that a pretty-printed AnfIR can be
          * scanned/parsed/type-checked as a LangF program.
          *)
         val unit = newSpecial "Unit"
         val bool = newSpecial "Bool"
         val integer = newSpecial "Integer"
         val string = newSpecial "String"
         val array = newSpecial "Array"
      end
   structure DaCon = Id (val defaultName = "D")
   structure DaCon =
      struct
         open DaCon

         (* Pre-defined data constructors;
          * use 'newSpecial' to omit uniquifying suffix,
          * so that a pretty-printed AnfIR can be
          * scanned/parsed/type-checked as a LangF program.
          *)
         val unit = newSpecial "Unit"
         val truee = newSpecial "True"
         val falsee = newSpecial "False"
      end

   structure Var = Id (val defaultName = "x")

   structure FreeIds =
      struct
         type t = (TyVar.Set.set * TyCon.Set.set * DaCon.Set.set * Var.Set.set)

         val empty = (TyVar.Set.empty, TyCon.Set.empty, DaCon.Set.empty, Var.Set.empty)

         fun singletonTyVar tyvar = (TyVar.Set.singleton tyvar, TyCon.Set.empty, DaCon.Set.empty, Var.Set.empty)
         fun singletonTyCon tycon = (TyVar.Set.empty, TyCon.Set.singleton tycon, DaCon.Set.empty, Var.Set.empty)
         fun singletonDaCon dacon = (TyVar.Set.empty, TyCon.Set.empty, DaCon.Set.singleton dacon, Var.Set.empty)
         fun singletonVar var = (TyVar.Set.empty, TyCon.Set.empty, DaCon.Set.empty, Var.Set.singleton var)

         fun fromListTyVars tyvars = (TyVar.Set.fromList tyvars, TyCon.Set.empty, DaCon.Set.empty, Var.Set.empty)
         fun fromListVars vars = (TyVar.Set.empty, TyCon.Set.empty, DaCon.Set.empty, Var.Set.fromList vars)

         fun union ((freeTyVars1, freeTyCons1, freeDaCons1, freeVars1),
                    (freeTyVars2, freeTyCons2, freeDaCons2, freeVars2)) =
            (TyVar.Set.union (freeTyVars1, freeTyVars2),
             TyCon.Set.union (freeTyCons1, freeTyCons2),
             DaCon.Set.union (freeDaCons1, freeDaCons2),
             Var.Set.union (freeVars1, freeVars2))

         fun difference ((freeTyVars1, freeTyCons1, freeDaCons1, freeVars1),
                         (freeTyVars2, freeTyCons2, freeDaCons2, freeVars2)) =
            (TyVar.Set.difference (freeTyVars1, freeTyVars2),
             TyCon.Set.difference (freeTyCons1, freeTyCons2),
             DaCon.Set.difference (freeDaCons1, freeDaCons2),
             Var.Set.difference (freeVars1, freeVars2))
      end

   structure CloneTbl =
      struct
         structure TyVarTbl =
            struct
               type dom = TyVar.t
               type cod = TyVar.t
               type t = cod TyVar.Tbl.hash_table
               val lookup : t * dom -> cod option = fn (tbl, tyvar) =>
                  TyVar.Tbl.find tbl tyvar
               val insert : t * dom * cod -> unit = fn (tbl, tyvar, tyvar') =>
                  TyVar.Tbl.insert tbl (tyvar, tyvar')
               val new : unit -> t = fn () =>
                  TyVar.Tbl.mkTable (32, Fail "TyVarTbl")
            end
         structure TyConTbl =
            struct
               type dom = TyCon.t
               type cod = TyCon.t
               type t = cod TyCon.Tbl.hash_table
               val lookup : t * dom -> cod option = fn (tbl, tycon) =>
                  TyCon.Tbl.find tbl tycon
               val insert : t * dom * cod -> unit = fn (tbl, tycon, tycon') =>
                  TyCon.Tbl.insert tbl (tycon, tycon')
               val new : unit -> t = fn () =>
                  TyCon.Tbl.mkTable (32, Fail "TyConTbl")
            end
         structure DaConTbl =
            struct
               type dom = DaCon.t
               type cod = DaCon.t
               type t = cod DaCon.Tbl.hash_table
               val lookup : t * dom -> cod option = fn (tbl, dacon) =>
                  DaCon.Tbl.find tbl dacon
               val insert : t * dom * cod -> unit = fn (tbl, dacon, dacon') =>
                  DaCon.Tbl.insert tbl (dacon, dacon')
               val new : unit -> t = fn () =>
                  DaCon.Tbl.mkTable (32, Fail "DaConTbl")
            end
         structure VarTbl =
            struct
               type dom = Var.t
               type cod = Var.t
               type t = cod Var.Tbl.hash_table
               val lookup : t * dom -> cod option = fn (tbl, var) =>
                  Var.Tbl.find tbl var
               val insert : t * dom * cod -> unit = fn (tbl, var, var') =>
                  Var.Tbl.insert tbl (var, var')
               val new : unit -> t = fn () =>
                  Var.Tbl.mkTable (32, Fail "VarTbl")
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
         fun insertTyVar (Tbl {tyvarTbl, ...}, tyvar, tyvar') =
            TyVarTbl.insert (tyvarTbl, tyvar, tyvar')

         fun lookupTyCon (Tbl {tyconTbl, ...}, tycon) =
            TyConTbl.lookup (tyconTbl, tycon)
         fun insertTyCon (Tbl {tyconTbl, ...}, tycon, tycon') =
            TyConTbl.insert (tyconTbl, tycon, tycon')

         fun lookupDaCon (Tbl {daconTbl, ...}, dacon) =
            DaConTbl.lookup (daconTbl, dacon)
         fun insertDaCon (Tbl {daconTbl, ...}, dacon, dacon') =
            DaConTbl.insert (daconTbl, dacon, dacon')

         fun lookupVar (Tbl {varTbl, ...}, var) =
            VarTbl.lookup (varTbl, var)
         fun insertVar (Tbl {varTbl, ...}, var, var') =
            VarTbl.insert (varTbl, var, var')
      end

   structure CloneEnv =
      struct
         structure TyVarEnv =
            struct
               type dom = TyVar.t
               type cod = TyVar.t
               type t = cod TyVar.Map.map
               val empty : t = TyVar.Map.empty
               val singleton : dom * cod -> t = TyVar.Map.singleton
               val lookup : t * dom -> cod option = TyVar.Map.find
               val extend : t * t -> t = TyVar.Map.unionWith #2
            end
         structure TyConEnv =
            struct
               type dom = TyCon.t
               type cod = TyCon.t
               type t = cod TyCon.Map.map
               val empty : t = TyCon.Map.empty
               val singleton : dom * cod -> t = TyCon.Map.singleton
               val lookup : t * dom -> cod option = TyCon.Map.find
               val extend : t * t -> t = TyCon.Map.unionWith #2
            end
         structure DaConEnv =
            struct
               type dom = DaCon.t
               type cod = DaCon.t
               type t = cod DaCon.Map.map
               val empty : t = DaCon.Map.empty
               val singleton : dom * cod -> t = DaCon.Map.singleton
               val lookup : t * dom -> cod option = DaCon.Map.find
               val extend : t * t -> t = DaCon.Map.unionWith #2
            end
         structure VarEnv =
            struct
               type dom = Var.t
               type cod = Var.t
               type t = cod Var.Map.map
               val empty : t = Var.Map.empty
               val singleton : dom * cod -> t = Var.Map.singleton
               val lookup : t * dom -> cod option = Var.Map.find
               val extend : t * t -> t = Var.Map.unionWith #2
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

         fun fromTyConEnv tyconEnv =
            Env {tyvarEnv = TyVarEnv.empty,
                 tyconEnv = tyconEnv,
                 daconEnv = DaConEnv.empty,
                 varEnv = VarEnv.empty}
         val singletonTyCon = fromTyConEnv o TyConEnv.singleton
         fun lookupTyCon (Env {tyconEnv, ...}, tycon) =
            TyConEnv.lookup (tyconEnv, tycon)

         fun fromDaConEnv daconEnv =
            Env {tyvarEnv = TyVarEnv.empty,
                 tyconEnv = TyConEnv.empty,
                 daconEnv = daconEnv,
                 varEnv = VarEnv.empty}
         val singletonDaCon = fromDaConEnv o DaConEnv.singleton
         fun lookupDaCon (Env {daconEnv, ...}, dacon) =
            DaConEnv.lookup (daconEnv, dacon)

         fun fromVarEnv varEnv =
            Env {tyvarEnv = TyVarEnv.empty,
                 tyconEnv = TyConEnv.empty,
                 daconEnv = DaConEnv.empty,
                 varEnv = varEnv}
         val singletonVar = fromVarEnv o VarEnv.singleton
         fun lookupVar (Env {varEnv, ...}, var) =
            VarEnv.lookup (varEnv, var)

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

   structure TyVar =
      struct
         open TyVar
         fun cloneAux (tbl, tyvar) =
            case CloneTbl.lookupTyVar (tbl, tyvar) of
               SOME tyvar' => tyvar'
             | NONE => tyvar
         fun cloneAuxL (tbl, tyvars) = List.map (fn var => cloneAux (tbl, var)) tyvars
      end

   structure TyCon =
      struct
         open TyCon
         fun cloneAux (tbl, tycon) =
            case CloneTbl.lookupTyCon (tbl, tycon) of
               SOME tycon' => tycon'
             | NONE => tycon
      end

   structure DaCon =
      struct
         open DaCon
         fun cloneAux (tbl, dacon) =
            case CloneTbl.lookupDaCon (tbl, dacon) of
               SOME dacon' => dacon'
             | NONE => dacon
      end

   structure Var =
      struct
         open Var
         fun cloneAux (tbl, var) =
            case CloneTbl.lookupVar (tbl, var) of
               SOME var' => var'
             | NONE => var
         fun cloneAuxL (tbl, vars) = List.map (fn var => cloneAux (tbl, var)) vars
      end

   structure Type =
      struct
         datatype t =
            T_TyFn of {tyvar: TyVar.t, res: t}
          | T_Fn of {arg: t, res: t}
          | T_TyCon of {tycon: TyCon.t, tyargs: t list}
          | T_TyVar of {tyvar: TyVar.t}

         val bool = T_TyCon {tycon = TyCon.bool, tyargs = []}
         val unit = T_TyCon {tycon = TyCon.unit, tyargs = []}
         val integer = T_TyCon {tycon = TyCon.integer, tyargs = []}
         val string = T_TyCon {tycon = TyCon.string, tyargs = []}
         fun array ty = T_TyCon {tycon = TyCon.array, tyargs = [ty]}

         fun compare (ty1, ty2) : order =
            let
               fun loop (bnds, isBndTyVar1, isBndTyVar2) (ty1, ty2) =
                  case (ty1, ty2) of
                     (T_TyFn {tyvar = tyvar1, res = res1},
                      T_TyFn {tyvar = tyvar2, res = res2}) =>
                        loop (bnds + 1,
                              fn tyvar1' => if TyVar.equals (tyvar1', tyvar1)
                                               then SOME bnds
                                            else isBndTyVar1 tyvar1',
                              fn tyvar2' => if TyVar.equals (tyvar2', tyvar2)
                                               then SOME bnds
                                            else isBndTyVar2 tyvar2')
                             (res1, res2)
                   | (T_TyFn _, _) => LESS
                   | (_, T_TyFn _) => GREATER
                   | (T_Fn {arg = arg1, res = res1},
                      T_Fn {arg = arg2, res = res2}) =>
                        (case loop (bnds, isBndTyVar1, isBndTyVar2) (arg1, arg2) of
                            LESS => LESS
                          | EQUAL => loop (bnds, isBndTyVar1, isBndTyVar2) (res1, res2)
                          | GREATER => GREATER)
                   | (T_Fn _, _) => LESS
                   | (_, T_Fn _) => GREATER
                   | (T_TyCon {tycon = tycon1, tyargs = tyargs1},
                      T_TyCon {tycon = tycon2, tyargs = tyargs2}) =>
                        (case TyCon.compare (tycon1, tycon2) of
                            LESS => LESS
                          | EQUAL => List.collate (loop (bnds, isBndTyVar1, isBndTyVar2)) (tyargs1, tyargs2)
                          | GREATER => GREATER)
                   | (T_TyCon _, _) => LESS
                   | (_, T_TyCon _) => GREATER
                   | (T_TyVar {tyvar = tyvar1},
                      T_TyVar {tyvar = tyvar2}) =>
                        (case (isBndTyVar1 tyvar1, isBndTyVar2 tyvar2) of
                            (SOME i1, SOME i2) => Int.compare (i1, i2)
                          | (SOME _, NONE) => GREATER
                          | (NONE, SOME _) => LESS
                          | (NONE, NONE) => TyVar.compare (tyvar1, tyvar2))
            in
               loop (0, fn _ => NONE, fn _ => NONE) (ty1, ty2)
            end
         fun equals (ty1, ty2) = compare (ty1, ty2) = EQUAL

         fun hash ty : word =
            let
               fun loop (bnds, isBndTyVar) ty =
                  case ty of
                     T_TyFn {tyvar, res} =>
                        loop (bnds + 1,
                              fn tyvar' => if TyVar.equals (tyvar', tyvar)
                                              then SOME bnds
                                           else NONE)
                             res
                   | T_Fn {arg, res} =>
                        Word.xorb (loop (bnds, isBndTyVar) arg, loop (bnds, isBndTyVar) res)
                   | T_TyCon {tycon, tyargs} =>
                        List.foldl (fn (tyarg, w) => Word.xorb (loop (bnds, isBndTyVar) tyarg, w)) (TyCon.hash tycon) tyargs
                   | T_TyVar {tyvar} =>
                        (case isBndTyVar tyvar of
                            SOME i => Word.fromInt i
                          | NONE => TyVar.hash tyvar)
            in
               loop (0, fn _ => NONE) ty
            end

         structure OrdKey =
            struct
               type ord_key = t
               val compare = compare
            end
         structure Map = RedBlackMapFn (OrdKey)
         structure Set = RedBlackSetFn (OrdKey)

         structure HashKey =
            struct
               type hash_key = t
               val hashVal = hash
               val sameKey = equals
            end
         structure Tbl = HashTableFn (HashKey)

         fun size ty =
            case ty of
               T_TyFn {tyvar, res} =>
                  1 + 1 + size res
             | T_Fn {arg, res} =>
                  size arg + 1 + size res
             | T_TyCon {tycon, tyargs} =>
                  1 + sizeL tyargs
             | T_TyVar {tyvar} =>
                  1
         and sizeL tys =
            List.foldl (fn (ty, s) => size ty + s) 0 tys

         fun layout ty = layoutT ty
         and layoutT ty = layoutAux true ty
         and layoutF ty = layoutAux false ty
         and layoutAux isDelimited ty =
            let
               fun delimit t = if isDelimited then t else Layout.paren t
            in
               case ty of
                  T_TyFn {tyvar, res} =>
                     (delimit o Layout.mayAlign)
                     [Layout.seq [Layout.str "[",
                                  TyVar.layout tyvar,
                                  Layout.str "]",
                                  Layout.space,
                                  Layout.str "->"],
                      layoutF res]
                | T_Fn {arg, res} =>
                     (delimit o Layout.mayAlign)
                     [layoutF arg,
                      Layout.str "->",
                      layoutF res]
                | T_TyCon {tycon, tyargs} =>
                     if List.null tyargs
                        then TyCon.layout tycon
                     else
                        (delimit o Layout.seq)
                        [TyCon.layout tycon,
                         Layout.prefixSpaceIfNonEmpty (layoutArgs tyargs)]
                | T_TyVar {tyvar} => TyVar.layout tyvar
            end
         and layoutArgs tys =
            Layout.optSeq ("[", "]", ",") layoutT tys

         fun freeIds ty =
            case ty of
               T_TyFn {tyvar, res} =>
                  FreeIds.difference (freeIds res,
                                      FreeIds.singletonTyVar tyvar)
             | T_Fn {arg, res} =>
                  FreeIds.union (freeIds arg, freeIds res)
             | T_TyCon {tycon, tyargs, ...} =>
                  List.foldl (fn (ty,free_ids) =>
                              FreeIds.union (freeIds ty, free_ids))
                             (FreeIds.singletonTyCon tycon)
                             tyargs
                | T_TyVar {tyvar} => FreeIds.singletonTyVar tyvar
         val freeTyVars = #1 o freeIds
         val freeTyCons = #2 o freeIds
         val freeDaCons = #3 o freeIds
         val freeVars = #4 o freeIds
         fun freeIdsL tys =
            List.foldl
            (fn (ty, free_ids) =>
             FreeIds.union (free_ids, freeIds ty))
            FreeIds.empty
            tys

         fun cloneAux (tbl, ty) =
            case ty of
               T_TyFn {tyvar, res} =>
                  let
                     val tyvar' = TyVar.new (TyVar.name tyvar)
                     val () = CloneTbl.insertTyVar (tbl, tyvar, tyvar')
                  in
                     T_TyFn {tyvar = tyvar',
                             res = cloneAux (tbl, res)}
                  end
             | T_Fn {arg, res} =>
                  T_Fn {arg = cloneAux (tbl, arg),
                        res = cloneAux (tbl, res)}
             | T_TyCon {tycon, tyargs} =>
                  T_TyCon {tycon = TyCon.cloneAux (tbl, tycon),
                           tyargs = cloneAuxL (tbl, tyargs)}
             | T_TyVar {tyvar} =>
                  T_TyVar {tyvar = TyVar.cloneAux (tbl, tyvar)}
         and cloneAuxL (tbl, tys) = List.map (fn ty => cloneAux (tbl, ty)) tys
         fun clone ty = cloneAux (CloneTbl.new (), ty)
         fun cloneL tys = List.map clone tys

         fun tySubst (ty, (actual, formal)) =
            let
               fun sub ty = tySubst (ty, (actual, formal))
            in
               case ty of
                  T_TyFn {tyvar, res} =>
                     if TyVar.equals (tyvar, formal)
                        then T_TyFn {tyvar = tyvar, res = res}
                     else if TyVar.Set.member (freeTyVars actual, tyvar)
                        then let
                                val tyvar' = TyVar.new (TyVar.name tyvar)
                                val tbl = CloneTbl.new ()
                                val () = CloneTbl.insertTyVar (tbl, tyvar, tyvar')
                                val res' = cloneAux (tbl, res)
                             in
                                T_TyFn {tyvar = tyvar',
                                        res = sub res'}
                             end
                     else T_TyFn {tyvar = tyvar, res = sub res}
                | T_Fn {arg, res} =>
                     T_Fn {arg = sub arg,
                           res = sub res}
                | T_TyCon {tycon, tyargs} =>
                     T_TyCon {tycon = tycon,
                              tyargs = tySubstL (tyargs, (actual, formal))}
                | T_TyVar {tyvar} =>
                     if TyVar.equals (tyvar, formal)
                        then clone actual
                     else T_TyVar {tyvar = tyvar}
            end
         and tySubstL (tys, (actual, formal)) =
            List.map (fn ty => tySubst (ty, (actual, formal))) tys
         fun tySubsts (ty, actuals_formals) =
            List.foldl (fn ((actual, formal), ty) => tySubst (ty, (actual, formal))) ty actuals_formals
         and tySubstsL (tys, actuals_formals) =
            List.map (fn ty => tySubsts (ty, actuals_formals)) tys
      end

   structure Bind =
      struct
         type t = {var: Var.t, var_ty: Type.t}

         fun size {var, var_ty} =
            1 + Type.size var_ty

         fun layout ({var, var_ty}, com) =
            Layout.seq [Var.layout var,
                        Layout.space,
                        if com then Layout.str "(* " else Layout.empty,
                        Layout.str ":",
                        Layout.space,
                        Type.layout var_ty,
                        if com then Layout.str " *)" else Layout.empty]

         fun freeIds {var, var_ty} =
            (Type.freeIds var_ty, FreeIds.singletonVar var)
            
         fun cloneAux (tbl, {var, var_ty}) =
            let
               val var' = Var.new (Var.name var)
               val () = CloneTbl.insertVar (tbl, var, var')
            in
               {var = var', var_ty = Type.cloneAux (tbl, var_ty)}
            end

         fun tySubst ({var, var_ty}, (actual, formal)) =
            {var = var, var_ty = Type.tySubst (var_ty, (actual, formal))}
      end
   structure Binds =
      struct
         type t = Bind.t list

         fun size binds =
            List.foldl (fn (bind, s) => Bind.size bind + s) 0 binds

         fun freeIds binds =
            List.foldl
            (fn (bind_i, (free_ids, mask_ids)) =>
             let
                val (free_ids_i, mask_ids_i) = Bind.freeIds bind_i
             in
                   (FreeIds.union (free_ids, free_ids_i),
                    FreeIds.union (mask_ids, mask_ids_i))
             end)
            (FreeIds.empty, FreeIds.empty)
            binds

         fun cloneAux (tbl, binds) =
            List.map (fn bind => Bind.cloneAux (tbl, bind)) binds

         fun tySubst (binds, (actual, formal)) =
            List.map (fn bind => Bind.tySubst (bind, (actual, formal))) binds
      end

   structure Pat =
      struct
         datatype t =
            P_DaCon of {dacon: DaCon.t,
                        tyargs: Type.t list,
                        binds: {var: Var.t, var_ty: Type.t} list}
          | P_Var of {var: Var.t, var_ty: Type.t}

         fun size pat =
            case pat of
               P_DaCon {dacon, tyargs, binds} =>
                  1 + Type.sizeL tyargs + Binds.size binds
             | P_Var bind =>
                  Bind.size bind

         fun layout pat =
            case pat of
               P_DaCon {dacon, tyargs, binds} =>
                  Layout.seq [DaCon.layout dacon,
                              Layout.prefixSpaceIfNonEmpty (Type.layoutArgs tyargs),
                              Layout.prefixSpaceIfNonEmpty (layoutBinds binds)]
             | P_Var {var, var_ty} => Bind.layout ({var = var, var_ty = var_ty}, true)
         and layoutBinds binds =
            Layout.optSeq ("{", "}", ",") (fn bind => Bind.layout (bind, true)) binds

         fun freeIds p =
            case p of
               P_DaCon {dacon, tyargs, binds} =>
                  let
                     val (free_ids_binds, mask_ids_binds) = Binds.freeIds binds
                        val free_ids_tyargs = Type.freeIdsL tyargs
                  in
                     (FreeIds.union (FreeIds.singletonDaCon dacon,
                                     FreeIds.union (free_ids_tyargs, free_ids_binds)),
                      mask_ids_binds)
                  end
             | P_Var bind =>
                  Bind.freeIds bind

         fun cloneAux (tbl, p) =
            case p of
               P_DaCon {dacon, tyargs, binds} =>
                  P_DaCon {dacon = DaCon.cloneAux (tbl, dacon),
                           tyargs = Type.cloneAuxL (tbl, tyargs),
                           binds = Binds.cloneAux (tbl, binds)}
             | P_Var bind =>
                  P_Var (Bind.cloneAux (tbl, bind))

         fun tySubst (p, (actual, formal)) =
            case p of
               P_DaCon {dacon, tyargs, binds} =>
                  P_DaCon {dacon = dacon,
                           tyargs = Type.tySubstL (tyargs, (actual, formal)),
                           binds = Binds.tySubst (binds, (actual, formal))}
             | P_Var bind =>
                  P_Var (Bind.tySubst (bind, (actual, formal)))
      end

   structure Prim =
      struct
         datatype t = datatype CoreIR.Prim.t
            
         fun toInt p =
            case p of
               Add => 1
             | Arg => 3
             | Argc => 5
             | Array => 7
             | Concat => 9
             | Div => 11
             | Eq => 13
             | Fail => 15
             | Gt => 17
             | Gte => 19
             | Lt => 21
             | Lte => 23
             | Mod => 25
             | Mul => 27
             | NEq => 29
             | Neg => 31
             | Print => 33
             | Size => 35
             | Sub => 37
             | Subscript => 39
             | Upd => 41
             | Idx => 43
             | Len => 45

         fun compare (prim1, prim2) =
            Int.compare (toInt prim1, toInt prim2)
         fun equals (prim1, prim2) =
            compare (prim1, prim2) = EQUAL
         fun hash prim =
            Word.fromInt (toInt prim)

         val layout = CoreIR.Prim.layout
      end

   structure Exp_RHS_Lam_MatchRule_Decl =
      struct
         datatype exp = Exp of {decls: decl list,
                                var: Var.t,
                                ty: Type.t}

         and rhs =
            R_Fn of {lam: lam}
          | R_Prim of {prim: Prim.t, tyargs: Type.t list, args: Var.t list}
          | R_DaCon of {dacon: DaCon.t, tyargs: Type.t list, args: Var.t list}
          | R_VApply of {func: Var.t, arg: Var.t}
          | R_TApply of {func: Var.t, tyarg: Type.t}
          | R_Var of {var: Var.t}
          | R_Integer of IntInf.int
          | R_String of String.string
          | R_Case of {arg: Var.t, matchrules: matchrule list}

         and lam =
            L_VLam of {var: Var.t, var_ty: Type.t, body: exp}
          | L_TLam of {tyvar: TyVar.t, body: exp}

         and matchrule = MatchRule of {pat: Pat.t, body: exp}

         and decl =
            D_Data of {data_decls: {tycon: TyCon.t,
                                    tyvars: TyVar.t list,
                                    dacon_decls: {dacon: DaCon.t,
                                                  arg_tys: Type.t list} list} list}
          | D_Val of {var: Var.t, var_ty: Type.t, rhs: rhs}
          | D_Fun of {fun_decls: {func: Var.t, func_ty: Type.t, lam: lam} list}

         fun sizeExp e =
            case e of
               Exp {decls, var, ty} =>
                  sizeDecls decls + 1 (* + Type.size ty *)
         and sizeRHS r =
            case r of
               R_Fn {lam} => sizeLam lam
             | R_Prim {prim, tyargs, args} =>
                  1 (* + Type.sizeL tyargs *) + List.length args
             | R_DaCon {dacon, tyargs, args} =>
                  1 (* + Type.sizeL tyargs *) + List.length args
             | R_VApply {func, arg} =>
                  2
             | R_TApply {func, tyarg} =>
                  1 (* + Type.size tyarg *)
             | R_Var {var} => 0 (* 1 *)
             | R_Integer i => let
                                 val n = 1 + IntInf.log2 (IntInf.abs i + 1)
                                 val q = Int.quot (n, 8)
                                 val r = Int.rem (n, 8)
                              in
                                 if r > 0 then q + 1 else q
                              end
             | R_String s => let
                                val n = String.size s
                                val q = Int.quot (n, 8)
                                val r = Int.rem (n, 8)
                             in 
                                if r > 0 then q + 1 else q
                             end
             | R_Case {arg, matchrules} =>
                  1 + List.foldl (fn (mr, s) => sizeMatchRule mr + s) 0 matchrules
         and sizeLam lam =
            case lam of
               L_VLam {var, var_ty, body} =>
                  1 (* + Type.size var_ty  *) + sizeExp body
             | L_TLam {tyvar, body} =>
                  (* 1 + *) sizeExp body
         and sizeMatchRule mr =
            case mr of
               MatchRule {pat, body} =>
                  Pat.size pat + sizeExp body
         and sizeDecl d =
            case d of
               D_Data {data_decls} =>
                  let
                     fun sizeDaConDecl {dacon, arg_tys} =
                        1 + Type.sizeL arg_tys
                     fun sizeDaConDecls dacon_decls =
                        List.foldl (fn (dacon_decl, s) => sizeDaConDecl dacon_decl + s) 0 dacon_decls
                     fun sizeDataDecl {tycon, tyvars, dacon_decls} =
                        1 + List.length tyvars + sizeDaConDecls dacon_decls
                  in
                     (* List.foldl (fn (data_decl, s) => sizeDataDecl data_decl + s) 0 data_decls *)
                     0
                  end
             | D_Val {var, var_ty, rhs} =>
                  1 (* + Type.size var_ty *) + sizeRHS rhs
             | D_Fun {fun_decls} =>
                  List.foldl (fn ({func, func_ty, lam}, s) =>
                              1 (* + Type.size func_ty *) + sizeLam lam + s)
                             0
                             fun_decls
         and sizeDecls ds =
            List.foldl (fn (d, s) => sizeDecl d + s) 0 ds

         fun layoutExp e = layoutExpT e
         and layoutExpT e = layoutExpAux true e
         and layoutExpF e = layoutExpAux false e
         and layoutExpAux isDelimited e =
            let
               fun delimit t = if isDelimited then t else Layout.paren t
            in
               case e of
                  Exp {decls, var, ty} =>
                     if List.null decls
                        then Var.layout var
                        else (delimit o Layout.align)
                             [Layout.str "let",
                              Layout.indent (Layout.align (List.map layoutDecl decls), 3),
                              Layout.str "in",
                              Layout.indent (Layout.seq [Var.layout var,
                                                         Layout.space,
                                                         Layout.str ":",
                                                         Layout.space,
                                                         Type.layout ty], 3), 
                              Layout.str "end"]
            end
         and layoutRHS r = layoutRHST r
         and layoutRHST r = layoutRHSAux true r
         and layoutRHSF r = layoutRHSAux false r
         and layoutRHSAux isDelimited r =
            let
               fun delimit t = if isDelimited then t else Layout.paren t
            in
               case r of
                  R_Fn {lam} => (delimit o layoutLam) lam
                | R_Prim {prim, tyargs, args} =>
                     let
                        fun arg i = List.nth (args, i)
                        fun unary sym =
                           (delimit o Layout.mayAlign)
                           [Layout.str sym,
                            Var.layout (arg 0)]
                        fun binary sym =
                           (delimit o Layout.mayAlign)
                           [Var.layout (arg 0),
                            Layout.str sym,
                            Var.layout (arg 1)]
                        fun ternary (syml,symr) =
                           (delimit o Layout.mayAlign)
                           [Var.layout (arg 0),
                            Layout.str syml,
                            Var.layout (arg 1),
                            Layout.str symr,
                            Var.layout (arg 2)]
                        fun func name =
                           (delimit o Layout.mayAlign)
                           (List.concat
                            [[Layout.str name],
                             List.map (fn arg => Layout.seq [Layout.str "[", Type.layout arg, Layout.str "]"]) tyargs,
                             List.map Var.layout args])
                     in
                        case prim of
                           Prim.Add => binary "+"
                         | Prim.Arg => func "arg"
                         | Prim.Argc => func "argc"
                         | Prim.Array => func "array"
                         | Prim.Concat => binary "^"
                         | Prim.Div => binary "/"
                         | Prim.Eq => binary "=="
                         | Prim.Fail => func "fail"
                         | Prim.Gt => binary ">"
                         | Prim.Gte => binary ">="
                         | Prim.Lt => binary "<"
                         | Prim.Lte => binary "<="
                         | Prim.Mod => binary "%"
                         | Prim.Mul => binary "*"
                         | Prim.NEq => binary "<>"
                         | Prim.Neg => unary "~"
                         | Prim.Print => func "print"
                         | Prim.Size => func "size"
                         | Prim.Sub => binary "-"
                         | Prim.Subscript => func "subscript"
                         | Prim.Upd => ternary ("!", ":=")
                         | Prim.Idx => binary "!"
                         | Prim.Len => unary "#"
                     end
                | R_DaCon {dacon, tyargs, args} =>
                     let
                        fun layoutDaConArgs args =
                           Layout.optSeq ("{", "}", ",") Var.layout args
                     in
                        if List.null tyargs andalso List.null args
                           then DaCon.layout dacon
                        else
                           (delimit o Layout.seq)
                           [DaCon.layout dacon,
                            Layout.prefixSpaceIfNonEmpty (Type.layoutArgs tyargs),
                            Layout.prefixSpaceIfNonEmpty (layoutDaConArgs args)]
                     end
                | R_VApply {func, arg} =>
                     (delimit o Layout.mayAlign)
                     [Var.layout func,
                      Var.layout arg]
                | R_TApply {func, tyarg} =>
                     (delimit o Layout.mayAlign)
                     [Var.layout func,
                      Layout.seq
                      [Layout.str "[",
                       Type.layout tyarg,
                       Layout.str "]"]]
                | R_Var {var} => Var.layout var
                | R_Integer i => Layout.str (IntInf.toString i)
                | R_String s => Layout.str (concat ["\"", String.toString s, "\""])
                | R_Case {arg, matchrules} =>
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
                          Var.layout arg,
                          Layout.space,
                          Layout.str "of"],
                         Layout.indent (layoutMatchRule (List.hd matchrules), 3),
                         Layout.indent (layoutMatchRules (List.tl matchrules), 1),
                         Layout.str "end"]
                     end
            end
         and layoutLam lam =
            case lam of
               L_VLam {var, var_ty, body} =>
                  Layout.mayAlign
                  [Layout.str (concat ["(* lam size: ", Int.toString (sizeLam lam), " *)"]),
                   Layout.seq [Layout.str "fn",
                               Layout.space,
                               Layout.str "(",
                               Var.layout var,
                               Layout.space,
                               Layout.str ":",
                               Layout.space,
                               Type.layout var_ty,
                               Layout.str ")",
                               Layout.space,
                               Layout.str "=>"],
                   Layout.indent (layoutExpF body, 3)]
             | L_TLam {tyvar, body} =>
                  Layout.mayAlign
                  [Layout.str (concat ["(* lam size: ", Int.toString (sizeLam lam), " *)"]),
                   Layout.seq [Layout.str "fn",
                               Layout.space,
                               Layout.str "[",
                               TyVar.layout tyvar,
                               Layout.str "]",
                               Layout.space,
                               Layout.str "=>"],
                   Layout.indent (layoutExpF body, 3)]
         and layoutMatchRule mr =
            case mr of
               MatchRule {pat, body} =>
                  Layout.mayAlign
                  [Layout.seq
                   [Pat.layout pat,
                    Layout.space,
                    Layout.str "=>"],
                   Layout.indent (layoutExpT body, 3)]
         and layoutDecl d =
            case d of
               D_Data {data_decls} =>
                  let
                     fun layoutDaConArgTys tys =
                        Layout.optSeq ("{", "}", ",") Type.layout tys
                     fun layoutDaConDecl {dacon, arg_tys} =
                        Layout.seq
                        [DaCon.layout dacon,
                         Layout.prefixSpaceIfNonEmpty (layoutDaConArgTys arg_tys)]
                     fun layoutDaConDecls dacon_decls =
                        (Layout.align o List.rev o #2)
                        (List.foldl
                         (fn (dacon_decl,(first,acc)) =>
                          (false,
                           (Layout.seq
                            [if first then Layout.str "=" else Layout.str "|",
                                Layout.space,
                                layoutDaConDecl dacon_decl])::acc))
                         (true,[])
                         dacon_decls)
                     fun layoutTyVars tyvars =
                        Layout.optSeq ("[", "]", ",") TyVar.layout tyvars
                     fun layoutDataDecl {tycon, tyvars, dacon_decls} =
                        Layout.seq
                        [TyCon.layout tycon,
                         Layout.prefixSpaceIfNonEmpty (layoutTyVars tyvars),
                         Layout.space,
                         layoutDaConDecls dacon_decls]
                     fun layoutDataDecls data_decls =
                        (Layout.align o List.rev o #2)
                        (List.foldl
                         (fn (data_decl,(first,acc)) =>
                          (false,
                           (Layout.seq
                            [if first then Layout.str "datatype" else Layout.str "and",
                                Layout.space,
                                layoutDataDecl data_decl])::acc))
                         (true,[])
                         data_decls)
                  in
                     layoutDataDecls data_decls
                  end
             | D_Val {var, var_ty, rhs} =>
                  Layout.mayAlign
                  [Layout.seq
                   [Layout.str "val",
                    Layout.space,
                    Var.layout var,
                    Layout.space,
                    Layout.str ":",
                    Layout.space,
                    Type.layout var_ty,
                    Layout.space,
                    Layout.str "="],
                   Layout.indent (layoutRHST rhs, 3)]
             | D_Fun {fun_decls} =>
                  let
                     fun layoutFunDecl kw {func, func_ty, lam} =
                        case lam of
                           L_VLam {var, var_ty, body as Exp {ty = body_ty, ...}} =>
                              Layout.mayAlign
                              [Layout.seq
                               [Layout.str kw,
                                Layout.space,
                                Var.layout func,
                                Layout.space,
                                Layout.str "(*",
                                Layout.space,
                                Layout.str ":",
                                Layout.space,
                                Type.layout func_ty,
                                Layout.space,
                                Layout.str "*)",
                                Layout.space,
                                Layout.str "(",
                                Var.layout var,
                                Layout.space,
                                Layout.str ":",
                                Layout.space,
                                Type.layout var_ty,
                                Layout.str ")",
                                Layout.space,
                                Layout.str ":",
                                Layout.space,
                                Type.layout body_ty,
                                Layout.space,
                                Layout.str "="],
                               Layout.indent (layoutExpF body, 3)]
                         | L_TLam {tyvar, body as Exp {ty = body_ty, ...}} =>
                              Layout.mayAlign
                              [Layout.seq
                               [Layout.str kw,
                                Layout.space,
                                Var.layout func,
                                Layout.space,
                                Layout.str "(*",
                                Layout.space,
                                Layout.str ":",
                                Layout.space,
                                Type.layout func_ty,
                                Layout.space,
                                Layout.str "*)",
                                Layout.space,
                                Layout.str "[",
                                TyVar.layout tyvar,
                                Layout.str "]",
                                Layout.space,
                                Layout.str ":",
                                Layout.space,
                                Type.layout body_ty,
                                Layout.space,
                                Layout.str "="],
                               Layout.indent (layoutExpF body, 3)]
                     fun layoutFunDecls fun_decls =
                        (Layout.align o List.rev o #2)
                        (List.foldl
                         (fn (fun_decl,(first,acc)) =>
                          (false,
                           (layoutFunDecl (if first then "fun" else "and") fun_decl)::acc))
                         (true,[])
                         fun_decls)
                  in
                     layoutFunDecls fun_decls
                  end

         fun freeIdsExp exp =
            case exp of
               Exp {decls, var, ...} =>
                  let
                     val (free_ids_decls, mask_ids_decls) = freeIdsDecls decls
                  in
                     FreeIds.union (free_ids_decls,
                                    FreeIds.difference (FreeIds.singletonVar var, mask_ids_decls))
                  end
         and freeIdsRHS rhs =
            case rhs of
               R_Fn {lam} => freeIdsLam lam
             | R_Prim {prim, tyargs, args} =>
                  let
                     val free_ids_tyargs = Type.freeIdsL tyargs
                     val free_ids_args = FreeIds.fromListVars args
                  in
                     FreeIds.union (free_ids_tyargs, free_ids_args)
                  end
             | R_DaCon {dacon, tyargs, args} =>
                  let
                     val free_ids_tyargs = Type.freeIdsL tyargs
                     val free_ids_args = FreeIds.fromListVars args
                  in
                     FreeIds.union (FreeIds.singletonDaCon dacon,
                                    FreeIds.union (free_ids_tyargs, free_ids_args))
                  end
             | R_VApply {func, arg} =>
                  FreeIds.union (FreeIds.singletonVar func, FreeIds.singletonVar arg)
             | R_TApply {func, tyarg} =>
                  FreeIds.union (FreeIds.singletonVar func, Type.freeIds tyarg)
             | R_Var {var} =>
                  FreeIds.singletonVar var
             | R_Integer _ => FreeIds.empty
             | R_String _ => FreeIds.empty
             | R_Case {arg, matchrules} =>
                  FreeIds.union (FreeIds.singletonVar arg,
                                 freeIdsMatchRules matchrules)
         and freeIdsLam lam =
            case lam of
               L_VLam {var, var_ty, body} =>
                  let
                     val free_ids_var_ty = Type.freeIds var_ty
                     val free_ids_body = freeIdsExp body
                  in
                     FreeIds.union (free_ids_var_ty,
                                    FreeIds.difference (free_ids_body, FreeIds.singletonVar var))
                  end
             | L_TLam {tyvar, body} =>
                  let
                     val free_ids_body = freeIdsExp body
                  in
                     FreeIds.difference (free_ids_body, FreeIds.singletonTyVar tyvar)
                  end
         and freeIdsMatchRule matchrule =
            case matchrule of
               MatchRule {pat, body} =>
                  let
                     val (free_ids_pat, mask_ids_pat) = Pat.freeIds pat
                     val free_ids_body = freeIdsExp body
                  in
                     FreeIds.union (free_ids_pat,
                                    FreeIds.difference (free_ids_body, mask_ids_pat))
                  end
         and freeIdsMatchRules matchrules =
            List.foldl
            (fn (matchrule, free_ids) =>
             FreeIds.union (free_ids, freeIdsMatchRule matchrule))
            FreeIds.empty
            matchrules
         and freeIdsDecl decl =
            case decl of
               D_Data {data_decls} =>
                  let
                     fun freeIdsDaConDecl {dacon, arg_tys} = (Type.freeIdsL arg_tys, FreeIds.singletonDaCon dacon)
                     fun freeIdsDaConDecls dacon_decls =
                        List.foldl
                        (fn (dacon_decl_i, (free_ids, mask_ids)) =>
                         let
                            val (free_ids_i, mask_ids_i) = freeIdsDaConDecl dacon_decl_i
                         in
                            (FreeIds.union (free_ids, free_ids_i),
                             FreeIds.union (mask_ids, mask_ids_i))
                         end)
                        (FreeIds.empty, FreeIds.empty)
                        dacon_decls
                     fun freeIdsDataDecl {tycon, tyvars, dacon_decls} =
                        let
                           val (free_ids_dacon_decls, mask_ids_dacon_decls) = freeIdsDaConDecls dacon_decls
                           val mask_ids_tyvars = FreeIds.fromListTyVars tyvars
                        in
                           (FreeIds.difference (free_ids_dacon_decls, mask_ids_tyvars),
                            FreeIds.union (FreeIds.singletonTyCon tycon, mask_ids_dacon_decls))
                        end
                     val (free_ids, mask_ids) =
                        List.foldl
                        (fn (data_decl, (free_ids, mask_ids)) =>
                         let
                            val (free_ids_i, mask_ids_i) = freeIdsDataDecl data_decl
                         in
                            (FreeIds.union (free_ids, free_ids_i),
                             FreeIds.union (mask_ids, mask_ids_i))
                         end)
                        (FreeIds.empty, FreeIds.empty)
                        data_decls
                  in
                     (FreeIds.difference (free_ids, mask_ids),
                      mask_ids)
                  end
             | D_Val {var, var_ty, rhs} =>
                  (FreeIds.union (Type.freeIds var_ty,
                                  freeIdsRHS rhs),
                   FreeIds.singletonVar var)
             | D_Fun {fun_decls} =>
                  let
                     fun freeIdsFunDecl {func, func_ty, lam} =
                        (FreeIds.union (Type.freeIds func_ty,
                                        freeIdsLam lam),
                         FreeIds.singletonVar func)
                     val (free_ids, mask_ids) =
                        List.foldl
                        (fn (fun_decl_i, (free_ids, mask_ids)) =>
                         let
                            val (free_ids_i, mask_ids_i) = freeIdsFunDecl fun_decl_i
                         in
                            (FreeIds.union (free_ids, free_ids_i),
                             FreeIds.union (mask_ids, mask_ids_i))
                         end)
                        (FreeIds.empty, FreeIds.empty)
                        fun_decls
                  in
                     (FreeIds.difference (free_ids, mask_ids),
                      mask_ids)
                  end
         and freeIdsDecls decls =
            List.foldr
            (fn (decl_i, (free_ids, mask_ids)) =>
             let
                val (free_ids_i, mask_ids_i) = freeIdsDecl decl_i
             in
                (FreeIds.union (FreeIds.difference (free_ids, mask_ids_i), free_ids_i),
                 FreeIds.union (mask_ids, mask_ids_i))
             end)
            (FreeIds.empty, FreeIds.empty)
            decls

         fun cloneAuxExp (tbl, e) =
            case e of
               Exp {decls, var, ty} =>
                  Exp {decls = cloneAuxDecls (tbl, decls),
                       var = Var.cloneAux (tbl, var),
                       ty = Type.cloneAux (tbl, ty)}
         and cloneAuxRHS (tbl, r) =
            case r of
               R_Fn {lam} => 
                  R_Fn {lam = cloneAuxLam (tbl, lam)}
             | R_Prim {prim, tyargs, args} =>
                  R_Prim {prim = prim,
                          tyargs = Type.cloneAuxL (tbl, tyargs),
                          args = Var.cloneAuxL (tbl, args)}
             | R_DaCon {dacon, tyargs, args} =>
                  R_DaCon {dacon = dacon,
                           tyargs = Type.cloneAuxL (tbl, tyargs),
                           args = Var.cloneAuxL (tbl, args)}
             | R_VApply {func, arg} =>
                  R_VApply {func = Var.cloneAux (tbl, func),
                            arg = Var.cloneAux (tbl, arg)}
             | R_TApply {func, tyarg} =>
                  R_TApply {func = Var.cloneAux (tbl, func),
                            tyarg = Type.cloneAux (tbl, tyarg)}
             | R_Var {var} =>
                  R_Var {var = Var.cloneAux (tbl, var)}
             | R_Integer i =>
                  R_Integer i
             | R_String s =>
                  R_String s
             | R_Case {arg, matchrules} =>
                  R_Case {arg = Var.cloneAux (tbl, arg),
                          matchrules = List.map (fn mr => cloneAuxMatchRule (tbl, mr)) matchrules}
         and cloneAuxLam (tbl, lam) =
            case lam of
               L_VLam {var, var_ty, body} =>
                  let
                     val var' = Var.new (Var.name var)
                     val () = CloneTbl.insertVar (tbl, var, var')
                  in
                     L_VLam {var = var',
                             var_ty = Type.cloneAux (tbl, var_ty),
                             body = cloneAuxExp (tbl, body)}
                  end
             | L_TLam {tyvar, body} =>
                  let
                     val tyvar' = TyVar.new (TyVar.name tyvar)
                     val () = CloneTbl.insertTyVar (tbl, tyvar, tyvar')
                  in
                     L_TLam {tyvar = tyvar',
                             body = cloneAuxExp (tbl, body)}
                  end
         and cloneAuxMatchRule (tbl, mr) =
            case mr of
               MatchRule {pat, body} =>
                  MatchRule {pat = Pat.cloneAux (tbl, pat),
                             body = cloneAuxExp (tbl, body)}
         and cloneAuxDecl (tbl, d) =
            case d of
               D_Data {data_decls} =>
                  let
                     val () =
                        List.app (fn {tycon, tyvars, dacon_decls} =>
                                  (CloneTbl.insertTyCon (tbl, tycon, TyCon.new (TyCon.name tycon));
                                   List.app (fn tyvar => CloneTbl.insertTyVar (tbl, tyvar, TyVar.new (TyVar.name tyvar))) tyvars;
                                   List.app (fn {dacon, arg_tys} =>
                                             CloneTbl.insertDaCon (tbl, dacon, DaCon.new (DaCon.name dacon)))
                                            dacon_decls))
                                 data_decls
                     val data_decls =
                        List.map (fn {tycon, tyvars, dacon_decls} =>
                                  let
                                     val dacon_decls =
                                        List.map (fn {dacon, arg_tys} =>
                                                  {dacon = DaCon.cloneAux (tbl, dacon),
                                                   arg_tys = Type.cloneAuxL (tbl, arg_tys)})
                                                 dacon_decls
                                  in
                                     {tycon = TyCon.cloneAux (tbl, tycon),
                                      tyvars = TyVar.cloneAuxL (tbl, tyvars),
                                      dacon_decls = dacon_decls}
                                  end)
                                 data_decls
                  in
                     D_Data {data_decls = data_decls}
                  end
             | D_Val {var, var_ty, rhs} =>
                  let
                     val var' = Var.new (Var.name var)
                     val () = CloneTbl.insertVar (tbl, var, var')
                  in
                     D_Val {var = var',
                            var_ty = Type.cloneAux (tbl, var_ty),
                            rhs = cloneAuxRHS (tbl, rhs)}
                  end
             | D_Fun {fun_decls} =>
                  let
                     val () =
                        List.app (fn {func, func_ty, lam} =>
                                  CloneTbl.insertVar (tbl, func, Var.new (Var.name func)))
                                 fun_decls
                     val fun_decls =
                        List.map (fn {func, func_ty, lam} =>
                                  {func = Var.cloneAux (tbl, func),
                                   func_ty = Type.cloneAux (tbl, func_ty),
                                   lam = cloneAuxLam (tbl, lam)})
                                 fun_decls
                  in
                     D_Fun {fun_decls = fun_decls}
                  end
         and cloneAuxDecls (tbl, ds) =
            List.map (fn d => cloneAuxDecl (tbl, d)) ds

         fun tySubstExp (e, (actual, formal)) =
            case e of
               Exp {decls, var, ty} =>
                  Exp {decls = tySubstDecls (decls, (actual, formal)),
                       var = var,
                       ty = Type.tySubst (ty, (actual, formal))}
         and tySubstRHS (r, (actual, formal)) =
            case r of
               R_Fn {lam} => 
                  R_Fn {lam = tySubstLam (lam, (actual, formal))}
             | R_Prim {prim, tyargs, args} =>
                  R_Prim {prim = prim,
                          tyargs = Type.tySubstL (tyargs, (actual, formal)),
                          args = args}
             | R_DaCon {dacon, tyargs, args} =>
                  R_DaCon {dacon = dacon,
                           tyargs = Type.tySubstL (tyargs, (actual, formal)),
                           args = args}
             | R_VApply {func, arg} =>
                  R_VApply {func = func,
                            arg = arg}
             | R_TApply {func, tyarg} =>
                  R_TApply {func = func,
                            tyarg = Type.tySubst (tyarg, (actual, formal))}
             | R_Var {var} =>
                  R_Var {var = var}
             | R_Integer i =>
                  R_Integer i
             | R_String s =>
                  R_String s
             | R_Case {arg, matchrules} =>
                  R_Case {arg = arg,
                          matchrules = List.map (fn mr => tySubstMatchRule (mr, (actual, formal))) matchrules}
         and tySubstLam (lam, (actual, formal)) =
            case lam of
               L_VLam {var, var_ty, body} =>
                  L_VLam {var = var,
                          var_ty = Type.tySubst (var_ty, (actual, formal)),
                          body = tySubstExp (body, (actual, formal))}
             | L_TLam {tyvar, body} =>
                  L_TLam {tyvar = tyvar,
                          body = tySubstExp (body, (actual, formal))}
         and tySubstMatchRule (mr, (actual, formal)) =
            case mr of
               MatchRule {pat, body} =>
                  MatchRule {pat = Pat.tySubst (pat, (actual, formal)),
                             body = tySubstExp (body, (actual, formal))}
         and tySubstDecl (d, (actual, formal)) =
            case d of
               D_Data {data_decls} =>
                  let
                     val data_decls =
                        List.map (fn {tycon, tyvars, dacon_decls} =>
                                  let
                                     val dacon_decls =
                                        List.map (fn {dacon, arg_tys} =>
                                                  {dacon = dacon,
                                                   arg_tys = Type.tySubstL (arg_tys, (actual, formal))})
                                                 dacon_decls
                                  in
                                     {tycon = tycon,
                                      tyvars = tyvars,
                                      dacon_decls = dacon_decls}
                                  end)
                                 data_decls
                  in
                     D_Data {data_decls = data_decls}
                  end
             | D_Val {var, var_ty, rhs} =>
                  D_Val {var = var,
                         var_ty = Type.tySubst (var_ty, (actual, formal)),
                         rhs = tySubstRHS (rhs, (actual, formal))}
             | D_Fun {fun_decls} =>
                  let
                     val fun_decls =
                        List.map (fn {func, func_ty, lam} =>
                                  {func = func,
                                   func_ty = Type.tySubst (func_ty, (actual, formal)),
                                   lam = tySubstLam (lam, (actual, formal))})
                                 fun_decls
                  in
                     D_Fun {fun_decls = fun_decls}
                  end
         and tySubstDecls (ds, (actual, formal)) =
            List.map (fn d => tySubstDecl (d, (actual, formal))) ds
      end

   structure Exp =
      struct
         datatype t = datatype Exp_RHS_Lam_MatchRule_Decl.exp

         val size = Exp_RHS_Lam_MatchRule_Decl.sizeExp
         val layout = Exp_RHS_Lam_MatchRule_Decl.layoutExp

         val freeIds = Exp_RHS_Lam_MatchRule_Decl.freeIdsExp
         val freeTyVars = #1 o freeIds
         val freeTyCons = #2 o freeIds
         val freeDaCons = #3 o freeIds
         val freeVars = #4 o freeIds

         fun clone e = Exp_RHS_Lam_MatchRule_Decl.cloneAuxExp (CloneTbl.new (), e)
         val tySubst = Exp_RHS_Lam_MatchRule_Decl.tySubstExp
      end
   structure RHS =
      struct
         datatype t = datatype Exp_RHS_Lam_MatchRule_Decl.rhs

         val size = Exp_RHS_Lam_MatchRule_Decl.sizeRHS
         val layout = Exp_RHS_Lam_MatchRule_Decl.layoutRHS

         val freeIds = Exp_RHS_Lam_MatchRule_Decl.freeIdsRHS
         val freeTyVars = #1 o freeIds
         val freeTyCons = #2 o freeIds
         val freeDaCons = #3 o freeIds
         val freeVars = #4 o freeIds

         fun clone r = Exp_RHS_Lam_MatchRule_Decl.cloneAuxRHS (CloneTbl.new (), r)
         val tySubst = Exp_RHS_Lam_MatchRule_Decl.tySubstRHS
      end
   structure Lam =
      struct
         datatype t = datatype Exp_RHS_Lam_MatchRule_Decl.lam

         val size = Exp_RHS_Lam_MatchRule_Decl.sizeLam
         val layout = Exp_RHS_Lam_MatchRule_Decl.layoutLam

         val freeIds = Exp_RHS_Lam_MatchRule_Decl.freeIdsLam
         val freeTyVars = #1 o freeIds
         val freeTyCons = #2 o freeIds
         val freeDaCons = #3 o freeIds
         val freeVars = #4 o freeIds

         fun clone lam = Exp_RHS_Lam_MatchRule_Decl.cloneAuxLam (CloneTbl.new (), lam)
         val tySubst = Exp_RHS_Lam_MatchRule_Decl.tySubstLam
      end
   structure MatchRule =
      struct
         datatype t = datatype Exp_RHS_Lam_MatchRule_Decl.matchrule

         val size = Exp_RHS_Lam_MatchRule_Decl.sizeMatchRule
         val layout = Exp_RHS_Lam_MatchRule_Decl.layoutMatchRule
      end
   structure Decl =
      struct
         datatype t = datatype Exp_RHS_Lam_MatchRule_Decl.decl

         val size = Exp_RHS_Lam_MatchRule_Decl.sizeDecl
         val sizeL = Exp_RHS_Lam_MatchRule_Decl.sizeDecls
         val layout = Exp_RHS_Lam_MatchRule_Decl.layoutDecl

         val freeIds = #1 o Exp_RHS_Lam_MatchRule_Decl.freeIdsDecl
         val freeTyVars = #1 o freeIds
         val freeTyCons = #2 o freeIds
         val freeDaCons = #3 o freeIds
         val freeVars = #4 o freeIds

         fun clone d = Exp_RHS_Lam_MatchRule_Decl.cloneAuxDecl (CloneTbl.new (), d)
         val tySubst = Exp_RHS_Lam_MatchRule_Decl.tySubstDecl
         fun tySubstL (ds, (actual, formal)) = List.map (fn d => tySubst (d, (actual, formal))) ds
      end

   structure Prog =
      struct
         datatype t = Prog of {decls: Decl.t list, var: Var.t, ty: Type.t}

         fun size p =
            case p of
               Prog {decls, var, ty} =>
                  Decl.sizeL decls + 1 (* + Type.size ty *)

         fun layout p =
            case p of
               Prog {decls, var, ty} =>
                  Layout.align ([Layout.str (concat ["(* program size: ", Int.toString (size p), " *)"])] @
                                (List.map Decl.layout decls) @
                                [Layout.str ";"] @
                                [Layout.seq [Var.layout var,
                                             Layout.space, Layout.str ":", Layout.space, Type.layout ty,
                                             Layout.str "\n"]])
         fun output (outStrm, prog) = Layout.output (outStrm, layout prog)
      end
end
