(* langfc-src/reploc-ir/reploc-ir.sml
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
 * Location/Representation core intermediate representation in the
 * LangF compiler (langfc).
 *)

structure RepLocIR :> REPLOC_IR where type Prim.t = CoreIR.Prim.t =
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

   structure DaConRep =
      struct
         datatype t =
            UnboxedTag of int  (* integer tag for nullary *)
          | TaggedBox of int   (* heap-allocated args (with tag) *)
(*
          | Boxed              (* heap-allocated args (no tag) *)
          | Transparent        (* arg *)
*)

         fun layout r =
            case r of
               UnboxedTag i =>
                  Layout.seq [Layout.str "UnboxedTag(",
                              Layout.str (Int.toString i),
                              Layout.str ")"]
             | TaggedBox i =>
                  Layout.seq [Layout.str "TaggedBox(",
                              Layout.str (Int.toString i),
                              Layout.str ")"]
(*
             | Boxed =>
                  Layout.str "Boxed"
             | Transparent =>
                  Layout.str "Transparent"
*)
      end
   structure DaConName = Id (val defaultName = "C")
   structure DaConName =
      struct
         open DaConName

         (* Pre-defined data constructors;
          * use 'newSpecial' to omit uniquifying suffix.
          *)
         val unit = newSpecial "Unit"
         val truee = newSpecial "True"
         val falsee = newSpecial "False"
      end
   structure DaCon =
      struct
         datatype t = DaCon of {name: DaConName.t, rep: DaConRep.t}

         fun layout (DaCon {name, rep}) =
            Layout.seq [DaConName.layout name,
                        Layout.str "@",
                        DaConRep.layout rep]

         (* Pre-defined data constructors.
          *)
         val unit = DaCon {name = DaConName.unit,
                           rep = DaConRep.UnboxedTag 0}
         val falsee = DaCon {name = DaConName.falsee,
                             rep = DaConRep.UnboxedTag 0}
         val truee = DaCon {name = DaConName.truee,
                            rep = DaConRep.UnboxedTag 1}
      end

   structure Func = Id (val defaultName = "f")

   structure Slot =
      struct
         type t = int
         val layout = Layout.str o Int.toString
         val toString = Layout.toString o layout

         structure OrdKey =
            struct
               type ord_key = t
               val compare = Int.compare
            end
         structure Map = RedBlackMapFn (OrdKey)
         structure Set = RedBlackSetFn (OrdKey)

         structure HashKey =
            struct
               type hash_key = t
               val hashVal = Word.fromInt
               val sameKey = fn (i: int, j) => i = j
            end
         structure Tbl = HashTableFn (HashKey)
      end

   structure VarLoc =
      struct
         datatype t =
            Self of Func.t  (* variable is a function
                                * from current function's recursive group *)
          | Param              (* variable is current function's parameter *)
          | Local of Slot.t    (* variable is from current function's frame *)
          | Global of Slot.t   (* variable is from current function's environment *)

         fun layout a =
            case a of
               Self func =>
                  Layout.seq [Layout.str "Self(",
                              Func.layout func,
                              Layout.str ")"]
             | Param =>
                  Layout.str "Param"
             | Local s =>
                  Layout.seq [Layout.str "Local(",
                              Slot.layout s,
                              Layout.str ")"]
             | Global s =>
                  Layout.seq [Layout.str "Global(",
                              Slot.layout s,
                              Layout.str ")"]
      end

   structure Pat =
      struct
         datatype t =
            P_DaCon of {dacon: DaCon.t,
                        slots: Slot.t list  (* store matched arguments in local slots *)
                       }
          | P_Var of {slot: Slot.t          (* store matched argument in local slot *)
                     }

         fun layout pat =
            case pat of
               P_DaCon {dacon, slots} =>
                  Layout.seq [DaCon.layout dacon,
                              Layout.prefixSpaceIfNonEmpty (layoutSlots slots)]
             | P_Var {slot} => layoutSlot slot
         and layoutSlots slots =
            Layout.optSeq ("{", "}", ",") layoutSlot slots
         and layoutSlot slot =
            Layout.seq [Layout.str "Local(",
                        Slot.layout slot,
                        Layout.str ")"]
      end

   structure Prim =
      struct
         datatype t = datatype CoreIR.Prim.t
         val layout = CoreIR.Prim.layout
       end

   structure Exp_MatchRule_Decl =
      struct
         datatype exp =
            E_Prim of {prim: Prim.t, args: exp list}
          | E_DaCon of {dacon: DaCon.t, args: exp list}
          | E_Apply of {func: exp, arg: exp}
          | E_Var of {loc: VarLoc.t}
          | E_Integer of IntInf.int
          | E_String of String.string
          | E_Let of {decl: decl, body: exp}
          | E_Case of {arg: exp, matchrules: matchrule list}

         and matchrule = MatchRule of {pat: Pat.t, body: exp}

         and decl =
            D_Val of {slot: Slot.t,  (* store evaluated expression in local slot *)
                      exp: exp
                     }
          | D_Fun of {env: VarLoc.t list,        (* shared environment *)
                      fun_decls: {slot: Slot.t,  (* store function closure in local slot *)
                                  func: Func.t
                                 } list
                     }

         fun layoutExp e = layoutExpT e
         and layoutExpT e = layoutExpAux true e
         and layoutExpF e = layoutExpAux false e
         and layoutExpAux isDelimited e =
            let
               fun delimit t = if isDelimited then t else Layout.paren t
            in
               case e of
                  E_Prim {prim, args} =>
                     let
                        fun arg i = List.nth (args, i)
                        fun unary sym =
                           (delimit o Layout.mayAlign)
                           [Layout.str sym,
                            layoutExpF (arg 0)]
                        fun binary sym =
                           (delimit o Layout.mayAlign)
                           [layoutExpF (arg 0),
                            Layout.str sym,
                            layoutExpF (arg 1)]
                        fun ternary (syml,symr) =
                           (delimit o Layout.mayAlign)
                           [layoutExpF (arg 0),
                            Layout.str syml,
                            layoutExpF (arg 1),
                            Layout.str symr,
                            layoutExpF (arg 2)]
                        fun func name =
                           (delimit o Layout.mayAlign)
                           (List.concat
                            [[Layout.str name],
                             List.map layoutExpF args])
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
                | E_DaCon {dacon, args} =>
                     let
                        fun layoutDaConArgs args =
                           Layout.optSeq ("{", "}", ",") layoutExpT args
                     in
                        (delimit o Layout.seq)
                        [DaCon.layout dacon,
                         Layout.prefixSpaceIfNonEmpty (layoutDaConArgs args)]
                     end
                | E_Apply {func, arg} =>
                     (delimit o Layout.mayAlign)
                     [layoutExpF func,
                      layoutExpF arg]
                | E_Var {loc} => VarLoc.layout loc
                | E_Integer i => Layout.str (IntInf.toString i)
                | E_String s => Layout.str (concat ["\"", String.toString s, "\""])
                | E_Let {decl, body} =>
                     let
                        fun loop (exp, decls) =
                           case exp of
                              E_Let {decl, body} => loop (body, decl::decls)
                            | _ => (List.rev decls, exp)
                        val (decls, body) = loop (body, [decl])
                     in
                        (delimit o Layout.mayAlign)
                        [Layout.str "let",
                         Layout.indent (Layout.align (List.map layoutDecl decls), 3),
                         Layout.str "in",
                         Layout.indent (layoutExpT body, 3),
                         Layout.str "end"]
                     end
                | E_Case {arg, matchrules} =>
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
                          layoutExpT arg,
                          Layout.space,
                          Layout.str "of"],
                         Layout.indent (layoutMatchRule (List.hd matchrules), 3),
                         Layout.indent (layoutMatchRules (List.tl matchrules), 1),
                         Layout.str "end"]
                     end
            end
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
               D_Val {slot, exp} =>
                  Layout.mayAlign
                  [Layout.seq
                   [Layout.str "val",
                    Layout.space,
                    Layout.str "Local(",
                    Slot.layout slot,
                    Layout.str ")",
                    Layout.space,
                    Layout.str "="],
                   layoutExpT exp]
             | D_Fun {env, fun_decls} =>
                  Layout.align
                  ((Layout.seq
                    [Layout.str "fun",
                     Layout.space,
                     Layout.str "$",
                     Layout.space,
                     Layout.str "=",
                     Layout.space,
                     Layout.tuple (List.map VarLoc.layout env)])::
                   (List.map
                    (fn {slot, func} =>
                     Layout.seq
                     [Layout.str "and",
                      Layout.space,
                      Layout.str "Local(",
                      Slot.layout slot,
                      Layout.str ")",
                      Layout.space,
                      Layout.str "=",
                      Layout.space,
                      Layout.str "{",
                      Func.layout func,
                      Layout.str ",",
                      Layout.space,
                      Layout.str "$",
                      Layout.str "}"])
                    fun_decls))
      end
   structure Exp =
      struct
         datatype t = datatype Exp_MatchRule_Decl.exp

         val layout = Exp_MatchRule_Decl.layoutExp
      end
   structure MatchRule =
      struct
         datatype t = datatype Exp_MatchRule_Decl.matchrule

         val layout = Exp_MatchRule_Decl.layoutMatchRule
      end
   structure Decl =
      struct
         datatype t = datatype Exp_MatchRule_Decl.decl

         val layout = Exp_MatchRule_Decl.layoutDecl
      end

   structure Function =
      struct
         datatype t = Function of {func: Func.t,
                                   nLocals: int,
                                   body: Exp.t}
            
         fun layout (Function {func, nLocals, body}) =
            Layout.mayAlign
            [Layout.seq [Layout.str "function ",
                         Func.layout func,
                         Layout.space,
                         Layout.str "#(",
                         Layout.str (Int.toString nLocals),
                         Layout.str ")",
                         Layout.space,
                         Layout.str "="],
             Layout.indent (Exp.layout body, 3)]
      end

   structure Prog =
      struct
         datatype t = Prog of {main: Func.t, functions: Function.t list}

         fun layout (Prog {main, functions}) =
            Layout.align
            ((Layout.seq
              [Layout.str "main: ", Func.layout main])::
             (List.map Function.layout functions))
         fun output (outStrm, prog) = Layout.output (outStrm, layout prog)
      end
end
