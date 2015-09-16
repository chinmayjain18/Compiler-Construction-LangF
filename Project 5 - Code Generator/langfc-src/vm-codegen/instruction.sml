(* langfc-src/vm-codegen/instructions.sig
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
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * VM code instructions in the LangF compiler (langfc).
 *)

structure Instruction :> INSTRUCTION =
struct

   structure F = Format

   structure Info =
      struct
         type t = {opc: word,
                   name: string,
                   minSz: int,
                   maxSz: int}
      end
   structure Param =
      struct
         datatype t =
            NoParam
          | Int of {signed: bool} * IntInf.int
          | Label of Label.t
      end

   datatype t = T of {info: Info.t, param: Param.t}

   fun dest (T instr) = instr

   fun toString (T {info= {name, ...}, param}) =
      case param of
         Param.NoParam => name
       | Param.Int ({signed}, n) => F.format "%s(%d)"
            [F.STR name, F.LINT n]
       | Param.Label l => F.format "%s(%s)"
            [F.STR name, F.STR(Label.toString l)]
   val layout = Layout.str o toString

   (* make an instruction that takes no arguments *)
   fun mkInstr0 info = T {info = info, param = Param.NoParam}

   (* make an instruction that takes a label argument *)
   fun mkInstrL info label = T {info = info, param = Param.Label label}

   (* make an instruction that takes an unsigned integer argument *)
   fun mkInstrU info n =
      let
         val _ =
            if (n < 0)
               then raise Fail (concat ["unexpected negative argument to ", #name info, " instruction"])
            else ()
      in
         T {info = info, param = Param.Int ({signed=false}, IntInf.fromInt n)}
      end

   (* make an instruction that takes a signed integer argument *)
   fun mkInstrS info i = T {info = info, param = Param.Int ({signed=true}, IntInf.fromInt i)}

   nonfix div mod

   (* instruction info; note that the opcodes must agree with the VM *)
   val haltInfo        = {opc = 0w00, name = "halt", minSz = 1, maxSz = 1}
   val addInfo         = {opc = 0w01, name = "add", minSz = 1, maxSz = 1}
   val subInfo         = {opc = 0w02, name = "sub", minSz = 1, maxSz = 1}
   val mulInfo         = {opc = 0w03, name = "mul", minSz = 1, maxSz = 1}
   val divInfo         = {opc = 0w04, name = "div", minSz = 1, maxSz = 1}
   val modInfo         = {opc = 0w05, name = "mod", minSz = 1, maxSz = 1}
   val negInfo         = {opc = 0w06, name = "neg", minSz = 1, maxSz = 1}
   val equInfo         = {opc = 0w07, name = "equ", minSz = 1, maxSz = 1}
   val lessInfo        = {opc = 0w08, name = "less", minSz = 1, maxSz = 1}
   val lesseqInfo      = {opc = 0w09, name = "lesseq", minSz = 1, maxSz = 1}
   val notInfo         = {opc = 0w10, name = "not", minSz = 1, maxSz = 1}
   val boxedInfo       = {opc = 0w11, name = "boxed", minSz = 1, maxSz = 1}
   val allocInfo       = {opc = 0w12, name = "alloc", minSz = 2, maxSz = 3}
   val repallocInfo    = {opc = 0w13, name = "repalloc", minSz = 1, maxSz = 1}
   val explodeInfo     = {opc = 0w14, name = "explode", minSz = 1, maxSz = 1}
   val lengthInfo      = {opc = 0w15, name = "length", minSz = 1, maxSz = 1}
   val selectInfo      = {opc = 0w16, name = "select", minSz = 2, maxSz = 3}
   val indexInfo       = {opc = 0w17, name = "index", minSz = 1, maxSz = 1}
   val updateInfo      = {opc = 0w18, name = "update", minSz = 1, maxSz = 1}
   val integerInfo     = {opc = 0w19, name = "integer", minSz = 2, maxSz = 9}
   val literalInfo     = {opc = 0w20, name = "literal", minSz = 2, maxSz = 3}
   val labelInfo       = {opc = 0w21, name = "label", minSz = 2, maxSz = 3}
   val swapInfo        = {opc = 0w22, name = "swap", minSz = 1, maxSz = 3}
   val popInfo         = {opc = 0w23, name = "pop", minSz = 1, maxSz = 3}
   val pushInfo        = {opc = 0w24, name = "push", minSz = 1, maxSz = 3}
   val loadlocalInfo   = {opc = 0w25, name = "loadlocal", minSz = 2, maxSz = 3}
   val storelocalInfo  = {opc = 0w26, name = "storelocal", minSz = 2, maxSz = 3}
   val loadglobalInfo  = {opc = 0w27, name = "loadglobal", minSz = 2, maxSz = 3}
   val pushepInfo      = {opc = 0w28, name = "pushep", minSz = 1, maxSz = 1}
   val popepInfo       = {opc = 0w29, name = "popep", minSz = 1, maxSz = 1}
   val jmpInfo         = {opc = 0w30, name = "jmp", minSz = 2, maxSz = 3}
   val jmpifInfo       = {opc = 0w31, name = "jmpif", minSz = 2, maxSz = 3}
   val callInfo        = {opc = 0w32, name = "call", minSz = 1, maxSz = 1}
   val tailcallInfo    = {opc = 0w33, name = "tailcall", minSz = 1, maxSz = 1}
   val entryInfo       = {opc = 0w34, name = "entry", minSz = 2, maxSz = 3}
   val retInfo         = {opc = 0w35, name = "ret", minSz = 1, maxSz = 1}
   val ccallInfo       = {opc = 0w36, name = "ccall", minSz = 2, maxSz = 3}
   val nopInfo         = {opc = 0w37, name = "nop", minSz = 1, maxSz = 1}

   val halt        = mkInstr0 haltInfo
   val add         = mkInstr0 addInfo
   val sub         = mkInstr0 subInfo
   val mul         = mkInstr0 mulInfo
   val div         = mkInstr0 divInfo
   val mod         = mkInstr0 modInfo
   val neg         = mkInstr0 negInfo
   val equ         = mkInstr0 equInfo
   val less        = mkInstr0 lessInfo
   val lesseq      = mkInstr0 lesseqInfo
   val not         = mkInstr0 notInfo
   val boxed       = mkInstr0 boxedInfo
   val alloc       = mkInstrU allocInfo
   val repalloc    = mkInstr0 repallocInfo
   val explode     = mkInstr0 explodeInfo
   val length      = mkInstr0 lengthInfo
   val select      = mkInstrU selectInfo
   val index       = mkInstr0 indexInfo
   val update      = mkInstr0 updateInfo
   val int         = mkInstrS integerInfo
   val integer     = fn ii => T {info = integerInfo, param = Param.Int ({signed=true}, ii)}
   val literal     = mkInstrU literalInfo
   val label       = mkInstrL labelInfo
   val swap        = mkInstr0 swapInfo
   val swapn       = mkInstrU swapInfo
   val pop         = mkInstr0 popInfo
   val popn        = mkInstrU popInfo
   val dup         = mkInstr0 pushInfo
   val pushn       = mkInstrU pushInfo
   val loadlocal   = mkInstrS loadlocalInfo
   val storelocal  = mkInstrS storelocalInfo
   val loadglobal  = mkInstrU loadglobalInfo
   val pushep      = mkInstr0 pushepInfo
   val popep       = mkInstr0 popepInfo
   val jmp         = mkInstrL jmpInfo
   val jmpif       = mkInstrL jmpifInfo
   val call        = mkInstr0 callInfo
   val tailcall    = mkInstr0 tailcallInfo
   val entry       = mkInstrU entryInfo
   val ret         = mkInstr0 retInfo
   val ccall       = mkInstrU ccallInfo
   val nop         = mkInstr0 nopInfo

end
