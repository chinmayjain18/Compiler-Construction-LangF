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
	       RL.VarLoc.Self (func) => let
					  val () = emitInstr(I.label (funcToLabel func))
					  val () = emitInstr(I.pushep)
					  val () = emitInstr(I.alloc(2))
					in
					  ()
					end
	     | RL.VarLoc.Param => emitInstr(I.loadlocal(2))
	     | RL.VarLoc.Local (slot) => emitInstr(I.loadlocal(~(slot+1)))
	     | RL.VarLoc.Global (slot)=> emitInstr(I.loadglobal(slot))

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
	 fun genPrim(prim: RL.Prim.t, args: RL.Exp.t list) =
	    case prim of
	       RL.Prim.Add => let
				val () = genExp(List.nth(args,0))
				val () = genExp(List.nth(args,1))
			      in
				emitInstr(I.add)
			      end
	     | RL.Prim.Sub => let
				val () = genExp(List.nth(args,0))
				val () = genExp(List.nth(args,1))
			      in
				emitInstr(I.sub)
			      end
	     | RL.Prim.Mul => let
				val () = genExp(List.nth(args,0))
				val () = genExp(List.nth(args,1))
			      in
				emitInstr(I.mul)
			      end
	     | RL.Prim.Div => let
				val () = genExp(List.nth(args,0))
				val () = genExp(List.nth(args,1))
			      in
				emitInstr(I.div)
			      end
	     | RL.Prim.Mod => let
				val () = genExp(List.nth(args,0))
				val () = genExp(List.nth(args,1))
			      in
				emitInstr(I.mod)
			      end
	     | RL.Prim.Eq => let
				val () = genExp(List.nth(args,0))
				val () = genExp(List.nth(args,1))
			     in
				emitInstr(I.equ)
			     end
             | RL.Prim.Gt => let
				val () = genExp(List.nth(args,0))
				val () = genExp(List.nth(args,1))
				val () = emitInstr(I.lesseq)
			     in
				emitInstr(I.not)
			     end
             | RL.Prim.Gte => let
				val () = genExp(List.nth(args,0))
				val () = genExp(List.nth(args,1))
				val () = emitInstr(I.less)
			      in
				emitInstr(I.not)
			      end
             | RL.Prim.Lt => let
				val () = genExp(List.nth(args,0))
				val () = genExp(List.nth(args,1))
			     in
				emitInstr(I.less)
			     end
             | RL.Prim.Lte => let
				val () = genExp(List.nth(args,0))
				val () = genExp(List.nth(args,1))
			      in
				emitInstr(I.lesseq)
			      end
	     | RL.Prim.NEq => let
				val () = genExp(List.nth(args,0))
				val () = genExp(List.nth(args,1))
				val () = emitInstr(I.equ)
			      in
				emitInstr(I.not)
			      end
             | RL.Prim.Neg => let
				val () = genExp(List.nth(args,0))
			      in
				emitInstr(I.neg)
			      end
	     | RL.Prim.Idx => let
				val () = genExp(List.nth(args,0))
				val () = genExp(List.nth(args,1))
			      in
				emitInstr(I.index)
			      end
             | RL.Prim.Len => let
				val () = genExp(List.nth(args,0))
			      in
				emitInstr(I.length)
			      end
	     | RL.Prim.Upd => let
				val () = genExp(List.nth(args,0))
				val () = genExp(List.nth(args,1))
				val () = genExp(List.nth(args,2))
			      in
				emitInstr(I.update)
			      end
	     | RL.Prim.Arg => let
				val () = genExp(List.nth(args,0))
				val arg = CodeStream.c_function(codeStrm,"VM_Arg")
			      in
				emitInstr(I.ccall arg)
			      end
             | RL.Prim.Argc => let
				 val () = genExp(List.nth(args,0))
				 val arg = CodeStream.c_function(codeStrm,"VM_Argc")
			       in
				 emitInstr(I.ccall arg)
			       end
             | RL.Prim.Array => let
				  val n = genExp(List.nth(args,0))
				  val d = genExp(List.nth(args,1))
				in
				  emitInstr(I.repalloc)
				end
             | RL.Prim.Concat => let
				   val () = genExp(List.nth(args,0))
				   val () = genExp(List.nth(args,1))
				   val arg = CodeStream.c_function(codeStrm,"VM_Concat")
				 in
				   emitInstr(I.ccall arg)
				 end
             | RL.Prim.Fail => let
				 val () = emitInstr(I.integer 0)
				 val () = genExp(List.nth(args,0))
				 val arg' = CodeStream.c_function(codeStrm,"VM_Print")
				 val () = emitInstr(I.ccall arg')
				 val () = emitInstr(I.integer 0)
				 val n = CodeStream.string(codeStrm, "\n")
				 val () = emitInstr(I.literal n)
				 val () = emitInstr(I.ccall arg')
				 val () = emitInstr(I.integer (~1))
			       in
				 emitInstr(I.halt)
			       end
             | RL.Prim.Print => let
				  val () = emitInstr(I.integer 0)
				  val () = genExp(List.nth(args,0))
				  val arg = CodeStream.c_function(codeStrm,"VM_Print")
				in
				  emitInstr(I.ccall arg)
				end
             | RL.Prim.Size => let
				 val () = genExp(List.nth(args,0))
				 val arg = CodeStream.c_function(codeStrm, "VM_Size")
			       in
				 emitInstr(I.ccall arg)
			       end
	     | RL.Prim.Subscript => let
				      val () = genExp(List.nth(args,0))
				      val () = genExp(List.nth(args,1))
				      val arg = CodeStream.c_function(codeStrm, "VM_Subscript")
				    in
				      emitInstr(I.ccall arg)
				    end

	 and genMatchRules(matchrules: RL.MatchRule.t list) =
	    let
	       	val exitLabel = Label.new "Label"
	       	val firstLabel = Label.new "Label"
	       	val () = genMatchRule(matchrules, firstLabel, exitLabel)		
		val () = defineLabel exitLabel
	    in
	       ()
	    end

	 and genMatchRule(matchrules: RL.MatchRule.t list, current: Label.t, exit: Label.t) =
	    let
	       val () = (case matchrules of
		    RL.MatchRule.MatchRule{pat=pat, body=exp}::tl => (let
						 val () = defineLabel current
				    	         val current' = case tl of
								   nil => exit
								 | _ => Label.new "Label"
						 val () = case pat of
							     RL.Pat.P_Var{slot=slot} => (let
											    val () = emitInstr(I.storelocal(~(slot+1)))
											    val () = genExp(exp)
											    val () = emitInstr(I.jmp exit)
											 in
											    ()
											 end)
							   | RL.Pat.P_DaCon {dacon=dacon, slots=slots} => let
													     val () = emitInstr(I.dup)
													     val () = case dacon of
												RL.DaCon.DaCon{name=x,rep=y} => case y of
														RL.DaConRep.UnboxedTag i =>
														     (let
														val () = emitInstr(I.boxed)
													     	val () = emitInstr(I.jmpif (current'))
														val () = emitInstr(I.dup)
														val () = emitInstr(I.int i)
														val () = emitInstr(I.equ)
														val () = emitInstr(I.not)
														val () = emitInstr(I.jmpif current')
														val () = emitInstr(I.pop)
														val () = genExp(exp)
														val () = emitInstr(I.jmp exit)
														      in
														()
														      end)
													      | RL.DaConRep.TaggedBox i =>
														     (let
														val () = emitInstr(I.boxed)
													     	val () = emitInstr(I.not)
													     	val () = emitInstr(I.jmpif (current'))
														val () = emitInstr(I.dup)
														val () = emitInstr(I.select 0)
														val () = emitInstr(I.int i)
														val () = emitInstr(I.equ)
														val () = emitInstr(I.not)
														val () = emitInstr(I.jmpif (current'))
														val () = emitInstr(I.explode)
														val () = genSlots(slots)
														val () = emitInstr(I.pop)
														val () = genExp(exp)
														val () = emitInstr(I.jmp exit)
														      in
														()
														      end)
													  in
													    ()
													  end
			      in
				 genMatchRule(tl, current', exit)
			      end )
		  | nil => () )
	    in
	       ()
	    end

	 and genSlots(slots: RL.Slot.t list) = 
	    case slots of
		hd::tl => let
			     val () = genSlots(tl)
			     val () = emitInstr(I.storelocal(~(hd+1)))
			  in
			     ()
			  end
	      | nil => ()

         and genExp (exp: RL.Exp.t) : unit =
	    case exp of
	       RL.Exp.E_Prim {prim=prim, args=args} => genPrim(prim, args)
             | RL.Exp.E_Integer i => emitInstr(I.integer i)
	     | RL.Exp.E_Let {decl=decl, body=body} => let
							 val () = genDecl(decl)
						      in
							 genExp(body)
						      end
             | RL.Exp.E_Case {arg=arg, matchrules=matchruleList} => let
								       val () = genExp(arg)
								       val () = genMatchRules(matchruleList)
								    in
								       ()
								    end
             | RL.Exp.E_DaCon {dacon=dacon, args=al} => let
							   val () = case dacon of
							    		 RL.DaCon.DaCon{name=x,rep=y} => (case y of
											RL.DaConRep.UnboxedTag i =>
										  	      let
												val () = emitInstr(I.int i)
											      in
												()
											      end
										      | RL.DaConRep.TaggedBox i =>
											      let
												val () = emitInstr(I.int i)
												val () = genArgs(al)
												val () = emitInstr(I.alloc(List.length(al)+1))
											      in
												()
											      end )
							     in
								()
							     end
	     | RL.Exp.E_String s => let
				       val n = CodeStream.string(codeStrm, s)
				       val () = emitInstr(I.literal n)
				    in
				       ()
				    end
             | RL.Exp.E_Var {loc=loc} => genVarLoc(loc)
             | RL.Exp.E_Apply {func=func, arg=arg} => let
							val () = emitInstr(I.pushep)
							val () = genExp(func)
							val () = genExp(arg)
							val () = emitInstr(I.swap)
							val () = emitInstr(I.explode)
							val () = emitInstr(I.popep)
							val () = emitInstr(I.call)
							val () = emitInstr(I.swap)
							val () = emitInstr(I.popep)
						      in
							()
						      end
	 
	 and genArgs(al: RL.Exp.t list) =
		let
		   val () = case al of
				hd::tl => let
					     val () = genExp(hd)
					  in
					     genArgs(tl)
					  end
				| nil => ()
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
               RL.Decl.D_Val {slot=slot, exp= exp} => let
							 val () = genExp(exp)
						      in
							 emitInstr(I.storelocal(~(slot+1)))
						      end
	     | RL.Decl.D_Fun {env=env, fun_decls=fun_decls} =>
						      let
							val () = List.app(fn (env') => genVarLoc env') env
							val () = emitInstr(I.alloc (List.length(env)))
							val () = List.app(fn (fun_decl) => case fun_decl of
											       {slot=slot, func=func} => let
													val () = emitInstr(I.dup)
													val funcString = funcToLabel func
													val () = emitInstr(I.label funcString)
													val () = emitInstr(I.swap)
													val () = emitInstr(I.alloc(2))
													val () = emitInstr(I.storelocal(~(slot+1)))
															     in
													()
															     end ) fun_decls
							val () = emitInstr(I.pop)
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
