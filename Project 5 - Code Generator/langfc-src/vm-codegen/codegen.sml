(* langfc-src/vm-codegen/codegen.sml
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
 * VM code generator in the LangF compiler (langfc).
 *)

structure VMCodeGen :> VM_CODE_GEN =
struct
   structure RL = RepLocIR
   structure I = Instruction

   fun codeGen (prog : RL.Prog.t) : CodeStream.t =
      let
         (* create (mutable) codeStrm to collect instructions/labels *)
         val codeStrm = CodeStream.new ()
         (* convenience functions, since all instructions/labels will be collected in codeStrm *)
         val emitInstr = fn instr => CodeStream.emit codeStrm instr
         val defineLabel = fn label => CodeStream.defineLabel codeStrm label


         (* choose a unique vmcode label for each func (function name) *)
         val funcToLabel =
            let
               val funcTbl : Label.t RL.Func.Tbl.hash_table =
                  RL.Func.Tbl.mkTable (32, Fail "funcTbl")
            in
               fn func =>
               case RL.Func.Tbl.find funcTbl func of
                  NONE =>
                     let
                        val label = Label.new (RL.Func.name func)
                        val () = RL.Func.Tbl.insert funcTbl (func, label)
                     in
                        label
                     end
                | SOME label => label
            end


         (* emit code for a varloc::
          *   stack on entry:  ...
          *   stack at exit:   ... v
          * where v is the value loaded/allocated for the varloc.
          * (Note: with the exception of the varloc value v, the stack
          * at exit is the same as the stack on entry; however, the
          * stack may have grown and shrunk between entry and exit.
          *)
         fun genVarLoc (loc: RL.VarLoc.t) : unit =
            case loc of
              RL.VarLoc.Self func => let
                                        val () = emitInstr (I.label (funcToLabel func))
                                        val () = emitInstr I.pushep
                                        val () = emitInstr (I.alloc 2)
                                     in
                                        ()
                                     end
            | RL.VarLoc.Param => emitInstr (I.loadlocal 2)
            | RL.VarLoc.Local s => emitInstr (I.loadlocal (~(s+1)))
            | RL.VarLoc.Global s => emitInstr (I.loadlocal s)
              

         (* emit code for an expression::
          *   stack on entry:  ...
          *   stack at exit:   ... res
          * where res is the value to which the expression evaluates.
          * (Note: with the exception of the expression value res, the
          * stack at exit is the same as the stack on entry above the
          * function's local variables; however, the stack may have
          * grown and shrunk between entry and exit.  Furthermore,
          * evaluation of an expression may change one or more of the
          * function's local variables on the stack (due to nested
          * case- and let-expressions).  
          *)
         fun genExp (exp: RL.Exp.t) : unit =
            (case exp of
              RL.Exp.E_Prim {prim, args} =>
                (case prim of
                   RL.Prim.Add => let
                                     val () = genExp (List.nth (args, 0)) 
                                     val () = genExp (List.nth (args, 1))
                                     val () = emitInstr I.add
                                  in
                                     ()
                                  end
                 | RL.Prim.Arg => let
                                     val () = genExp (List.nth (args, 0))
                                     val n = CodeStream.c_function (codeStrm, "VM_Arg")
                                  in
                                     emitInstr (I.ccall n)
                                  end 
                 | RL.Prim.Argc => let
                                      val () = genExp (List.nth (args, 0))
                                      val v = CodeStream.c_function (codeStrm, "VM_Argc")
                                   in
                                      emitInstr (I.ccall v)
                                   end
                 | RL.Prim.Array => let
                                       val () = genExp (List.nth (args, 0))
                                       val () = genExp (List.nth (args, 1))
                                    in
                                       emitInstr I.repalloc
                                    end
                 | RL.Prim.Concat => let
                                        val () = genExp (List.nth (args, 0))
                                        val () = genExp (List.nth (args, 1))
                                        val n = CodeStream.c_function (codeStrm, "VM_Concat")
                                     in
                                        emitInstr (I.ccall n)
                                     end
                 | RL.Prim.Div => let 
                                     val () = genExp (List.nth (args, 0))
                                     val () = genExp (List.nth (args, 1))
                                     val () = emitInstr I.div
                                  in
                                     ()
                                  end 
                 | RL.Prim.Eq => let
                                    val () = genExp (List.nth (args, 0))
                                    val () = genExp (List.nth (args, 1))
                                    val () = emitInstr I.equ
                                 in
                                    ()
                                 end
                 | RL.Prim.Fail => let
                                      val () = emitInstr (I.int 0)
                                      val () = genExp (List.nth (args, 0))
                                      val n  = CodeStream.c_function (codeStrm, "VM_Print")
                                      val () = emitInstr (I.ccall n)
                                      val () = emitInstr (I.int 1)
                                      val () = emitInstr I.neg
                                      val () = emitInstr I.halt
                                   in
                                      ()
                                   end
                 | RL.Prim.Gt => let
                                    val () = genExp (List.nth (args, 0))
                                    val () = genExp (List.nth (args, 1))
                                    val () = emitInstr I.lesseq
                                    val () = emitInstr I.not
                                 in
                                    ()
                                 end
                 | RL.Prim.Gte => let
                                     val () = genExp (List.nth (args, 0))
                                     val () = genExp (List.nth (args, 1))
                                     val () = emitInstr I.less
                                     val () = emitInstr I.not
                                  in
                                     ()
                                  end
                 | RL.Prim.Idx => let
                                     val () = genExp (List.nth (args, 0))
                                     val () = genExp (List.nth (args, 1))
                                     val () = emitInstr I.index
                                  in
                                     ()
                                  end
                 | RL.Prim.Len => let
                                     val () = genExp (List.nth (args, 0))
                                     val () = emitInstr I.length
                                  in
                                     ()
                                  end
                 | RL.Prim.Lt => let
                                    val () = genExp (List.nth (args, 0))
                                    val () = genExp (List.nth (args, 1))
                                    val () = emitInstr I.less
                                 in
                                    ()
                                 end
                 | RL.Prim.Lte => let
                                     val () = genExp (List.nth (args, 0))
                                     val () = genExp (List.nth (args, 1))
                                     val () = emitInstr I.lesseq
                                  in
                                     ()
                                  end
                 | RL.Prim.Mod => let
                                     val () = genExp (List.nth (args, 0))
                                     val () = genExp (List.nth (args, 1))
                                     val () = emitInstr I.mod
                                  in
                                     ()
                                  end
                 | RL.Prim.Mul => let
                                     val () = genExp (List.nth (args, 0))
                                     val () = genExp (List.nth (args, 1))
                                     val () = emitInstr I.mul
                                  in
                                     ()
                                  end
                 | RL.Prim.NEq => let
                                     val () = genExp (List.nth (args, 0))
                                     val () = genExp (List.nth (args, 1))
                                     val () = emitInstr I.equ
                                     val () = emitInstr I.not
                                  in
                                     ()
                                  end
                 | RL.Prim.Neg => let
                                     val () = genExp (List.nth (args, 0))
                                     val () = emitInstr I.neg
                                  in
                                     ()
                                  end
                 | RL.Prim.Print => let
                                       val () = emitInstr (I.int 0)
                                       val () = genExp (List.nth (args, 0))
                                       val n = CodeStream.c_function (codeStrm, "VM_Print")
                                    in
                                       emitInstr (I.ccall n)
                                    end
                 | RL.Prim.Size => let
                                      val () = genExp (List.nth (args, 0))
                                      val n = CodeStream.c_function (codeStrm, "VM_Size")
                                   in
                                      emitInstr (I.ccall n)
                                   end
                 | RL.Prim.Sub => let 
                                      val () = genExp (List.nth (args, 0))
                                      val () = genExp (List.nth (args, 1))
                                      val () = emitInstr I.sub
                                  in
                                     ()
                                  end
                 | RL.Prim.Subscript => let
                                           val () = genExp (List.nth (args, 0))
                                           val () = genExp (List.nth (args, 1))
                                           val n = CodeStream.c_function (codeStrm, "VM_Subscript")
                                        in
                                           emitInstr (I.ccall n)
                                        end
                 | RL.Prim.Upd => let
                                     val () = genExp (List.nth (args, 0))
                                     val () = genExp (List.nth (args, 1))
                                     val () = genExp (List.nth (args, 2))
                                     val () = emitInstr I.update
                                  in
                                     ()
                                  end)
            | RL.Exp.E_DaCon {dacon, args} => (case dacon of
                                                RL.DaCon.DaCon {name, rep} => (case rep of 
                                                                                RL.DaConRep.UnboxedTag i => emitInstr (I.int i)
                                                                              | RL.DaConRep.TaggedBox i => let
                                                                                                           val () = emitInstr (I.int i)
                                                                                                              val () = genDaconArgs args
                                                                                                              val () = emitInstr (I.alloc ((List.length args) + 1))
                                                                                                           in
                                                                                                              ()
                                                                                                           end))
            | RL.Exp.E_Apply {func, arg} => let
                                               val () = emitInstr I.pushep
                                               val () = genExp func
                                               val () = genExp arg 
                                               val () = emitInstr I.swap
                                               val () = emitInstr I.explode
                                               val () = emitInstr I.popep
                                               val () = emitInstr I.call
                                               val () = emitInstr I.swap
                                               val () = emitInstr I.popep
                                            in
                                               ()
                                            end
            | RL.Exp.E_Var {loc} => genVarLoc loc
            | RL.Exp.E_Integer n => emitInstr (I.integer n)
            | RL.Exp.E_String s => let
                                      val n = CodeStream.string (codeStrm, s)
                                      val () = emitInstr (I.literal n)
                                   in
                                      ()
                                   end
            | RL.Exp.E_Let {decl, body} => let
                                              val () = genDecl decl
                                              val () = genExp body
                                           in
                                              ()
                                           end
            | RL.Exp.E_Case {arg, matchrules} => let
                                                    val () = genExp arg
                                                    val l1 = Label.new "startLabel"
                                                    val l2 = Label.new "endLabel"
                                                    val () = genMatchRules (matchrules, l1, l2)
                                                    val () = defineLabel l2 
                                                 in
                                                    ()
                                                 end)
         and genMatchRules (matchRules: RL.MatchRule.t list, l1: Label.t, l2: Label.t) =
            let
               val () = (case matchRules of
                            RL.MatchRule.MatchRule {pat, body}::rest => 
                              (let
                                 val () = defineLabel l1
                                 val start = (case rest of
                                               nil => l2
                                             | _   => Label.new "startLabel")
                                 val () = (case pat of
                                            RL.Pat.P_Var {slot} => 
                                              (let
                                                 val () = emitInstr (I.storelocal(~(slot+1)))
                                                 val () = genExp body
                                                 val () = emitInstr (I.jmp l2)
                                               in
                                                 ()
                                               end)
                                          | RL.Pat.P_DaCon {dacon, slots} => 
                                              (let
                                                 val () = (case dacon of
                                                             RL.DaCon.DaCon {name, rep} =>
                                                               (case rep of
                                                                 RL.DaConRep.UnboxedTag i =>
                                                                   (let
                                                                      val () = emitInstr I.dup
                                                                      val () = emitInstr I.boxed
                                                                      val () = emitInstr (I.jmpif start)
                                                                      val () = emitInstr I.dup
                                                                      val () = emitInstr (I.int i)
                                                                      val () = emitInstr I.equ
                                                                      val () = emitInstr I.not
                                                                      val () = emitInstr (I.jmpif start)
                                                                      val () = emitInstr I.pop
                                                                      val () = genExp body
                                                                      val () = emitInstr (I.jmp l2)
                                                                   in
                                                                      ()
                                                                   end)
                                                               | RL.DaConRep.TaggedBox i =>
                                                                   (let
                                                                      val () = emitInstr I.dup
                                                                      val () = emitInstr I.boxed
                                                                      val () = emitInstr I.not
                                                                      val () = emitInstr (I.jmpif start)
                                                                      val () = emitInstr I.dup
                                                                      val () = emitInstr (I.select 0)
                                                                      val () = emitInstr (I.int i)
                                                                      val () = emitInstr I.equ
                                                                      val () = emitInstr I.not
                                                                      val () = emitInstr (I.jmpif start)
                                                                      val () = emitInstr I.explode
                                                                      val () = genSlots slots
                                                                      val () = emitInstr I.pop
                                                                      val () = genExp body
                                                                      val () = emitInstr (I.jmp l2)
                                                                   in
                                                                      ()
                                                                   end)))
                                              in
                                                 ()
                                              end))
                              in
                                 genMatchRules (rest, start, l2)
                              end)
                          | nil => ())
            in 
               ()
            end
         and genSlots (slots: RL.Slot.t list) : unit =
            case slots of
              [] => ()
            | (x::xs) => let
                            val () = emitInstr (I.storelocal(~(x+1)))
                            val () = genSlots xs
                         in
                            ()
                         end
                                                
         and genDaconArgs (args: RL.Exp.t list) : unit =
            case args of
              [] => ()
            | (x::xs) => let
                            val () = genExp x
                            val () = genDaconArgs xs
                         in
                            ()  
                         end 
         (* emit code for a declaration::
          *   stack on entry:  ...
          *   stack at exit:   ...
          * Note: the stack at exit is the same as the stack on entry

          * above the function's local variables; however, the stack
          * may have grown and shrunk between entry and exit.
          * Furthermore, a declaration will change one or more of the
          * function's local variables on the stack (and may further
          * change the function's local variables on the stack (due to
          * nested case- and let-expressions)).
          *)
         and genDecl (decl: RL.Decl.t) : unit =
            case decl of
              RL.Decl.D_Val {slot, exp} => let
                                              val () = genExp exp
                                              val () = emitInstr (I.storelocal (~(slot+1)))
                                           in
                                              ()
                                           end
            | RL.Decl.D_Fun {env, fun_decls} => let
                                                   val () = List.app (fn (env') => genVarLoc env') env
                                                   val () = emitInstr (I.alloc (List.length env))
                                                   val () = List.app (fn (f) => case f of
                                                                                  {slot, func} => let
                                                                                                     val () = emitInstr I.dup
                                                                                                     val () = emitInstr (I.label (funcToLabel func))
                                                                                                     val () = emitInstr I.swap
                                                                                                     val () = emitInstr (I.alloc 2)
                                                                                                     val () = emitInstr (I.storelocal (~(slot+1)))
                                                                                                  in
                                                                                                     ()
                                                                                                  end) fun_decls
                                                                                                                
                                                   val () = emitInstr I.pop
                                                in
                                                   ()
                                                end

         (* emit code for a RepLocIR function
          *   stack on entry:   ... arg raddr_caller
          *   stack at return:  ... res
          * Note: the stack at exit is the same as the stack on entry;
          * however, the stack may have grown and shrunk between entry
          * and exit.
          *)
         fun genFunction (function: RL.Function.t) : unit =
            case function of
               RL.Function.Function {func, nLocals, body} =>
                  let
                     val lab = funcToLabel func

                     val () = defineLabel lab
                     (* stk: ... arg raddr_caller *)
                     val () = emitInstr (I.entry nLocals)
                     (* stk: ... arg raddr_caller fp_caller w_0 ... w_{n-1} *)
                     val () = genExp body
                     (* stk: ... arg raddr_caller fp_caller v'_0 ... v'_{n-1} res *)
                     val () = emitInstr I.ret
                     (* stk: ... res *)
                  in
                     ()
                  end

         (* emit code for startup
          *   stack on entry:  
          *   stack at halt:   res
          *)
         fun genStart main =
            let
               (* stack: *)
               val () = emitInstr (I.int 0)
               (* stack: 0 *)
               val () = emitInstr (I.label (funcToLabel main))
               (* stack: 0 mainLab *)
               val () = emitInstr (I.alloc 0)
               (* stack: 0 mainLab <> *)
               val () = emitInstr I.popep
               (* stack: 0 mainLab *)
               val () = emitInstr I.call
               (* stack: res *)
               val () = emitInstr I.halt
            in
               ()
            end

         val RL.Prog.Prog {main, functions} = prog
         (* emit code for startup *)
         val () = genStart main
         (* emit code for all RepLocIR functions *)
         val () = List.app genFunction functions
      in
         codeStrm
      end

   val codeGen =
      Control.mkKeepCtlPass
      {keepPre = NONE,
       keepPost = SOME {output = CodeStream.output,
                        ext = "vmcode"},
       passName = "vm-codegen",
       pass = codeGen}

   val codeGen =
      Control.mkTracePass
      {msgPre = NONE,
       msgPost = NONE,
       passName = "vm-codegen",
       pass = codeGen}

end
