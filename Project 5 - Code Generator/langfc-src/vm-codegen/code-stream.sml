(* langfc-src/vm-codegen/code-stream.sml
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
 * VM code streams in the LangF compiler (langfc).
 *)

structure CodeStream :> CODE_STREAM =
struct

   structure Label =
      struct
         open Label

         local
            exception LabelNotFound
            val locTbl : int Tbl.hash_table =
               Tbl.mkTable (16, LabelNotFound)
         in
            fun setLoc (lab, i) =
               Tbl.insert locTbl (lab, i)
            fun getLoc lab =
               (Tbl.lookup locTbl lab)
               handle LabelNotFound => (setLoc (lab, ~1); ~1)
         end
      end
   structure Instr = Instruction
   structure F = Format
   structure II = IntInf

   structure StringTbl =
      HashTableFn (struct
                      type hash_key = string
                      val hashVal = HashString.hashString
                      val sameKey : (string * string) -> bool = (op =)
                   end)

   structure Code =
      struct
         datatype t =
            Loc of Label.t
          | Instr of {i : Instr.t, sz : int}
          | VarInstr of {i : Instr.t, sz : int}
      end

    fun sizeOf (addr, instr) = (case #param (Instr.dest instr)
           of Instr.Param.NoParam => 1
            | Instr.Param.Int ({signed=true}, i) =>
                if (~128 <= i) andalso (i < 128) then 2
                else if (~32768 <= i) andalso (i < 32768) then 3
                else if (~4611686018427387904 <= i) andalso (i < 4611686018427387904) then 9
                else raise Fail "parameter too large"
            | Instr.Param.Int ({signed=false}, n) =>
                if (n < 256) then 2
                else if (n < 65536) then 3
                else raise Fail "parameter too large"
            | Instr.Param.Label lab => if ((Label.getLoc lab) < 0)
                then 2 (* minimum size of instruction+label *)
                else let
                  val offset = (Label.getLoc lab) - addr
                  in
                    if (~128 <= offset) andalso (offset < 128) then 2
                    else if (~32768 <= offset) andalso (offset < 32768) then 3
                    else raise Fail "offset too large"
                  end
          (* end case *))


   datatype t = CS of {
	addr : int ref,
	code : Code.t list ref,
	nLits : int ref,
	lits : int StringTbl.hash_table,
	nCfuns : int ref,
	cfuns : int StringTbl.hash_table
      }

    fun tableToArray (n : int, tbl : int StringTbl.hash_table) = let
	  val arr = Array.array(n, "")
	  fun ins (s, i) = Array.update(arr, i, s)
	  in
	    StringTbl.appi ins tbl;
	    arr
	  end

  (* create a new code stream *)
    fun new () = CS{
	    addr = ref 0,
	    code = ref [],
	    nLits = ref 0,
	    lits = StringTbl.mkTable (32, Fail "lits"),
	    nCfuns = ref 0,
	    cfuns = StringTbl.mkTable (16, Fail "cfuns")
	  }

  (* dump the contents of the code stream to the specified output stream *)
    fun output (outS, CS{code, nLits, lits, nCfuns, cfuns, ...}) = let
	  fun pr s = TextIO.output(outS, s)
	  fun prf (fmt, items) = pr(F.format fmt items)
	  fun prStr (id, s) =
		prf("  [%2d] \"%s\"\n", [F.INT id, F.STR(String.toString s)])
	  fun prIns (Code.Loc lab, adr) = (
		prf("[%04d]  %s:\n", [F.INT adr, F.STR(Label.toString lab)]);
		adr)
	    | prIns (Code.Instr{i, sz}, adr) = (
		prf("[%04d]    %s\n", [F.INT adr, F.STR(Instruction.toString i)]);
		adr+sz)
	    | prIns (Code.VarInstr{i, sz}, adr) = (
		prf("[%04d]    %s  /* sz = %d */\n",
		  [F.INT adr, F.STR(Instruction.toString i), F.INT sz]);
		adr+sz)
	  in
	    prf ("%d Literals\n", [F.INT(!nLits)]);
	    Array.appi prStr (tableToArray(!nLits, lits));
	    prf ("%d C functions\n", [F.INT(!nCfuns)]);
	    Array.appi prStr (tableToArray(!nCfuns, cfuns));
	    pr "Code\n";
	    ignore (List.foldr prIns 0 (!code))
	  end

  (* emit an instruction to the stream *)
    fun emit (CS{addr, code, ...}) instr =
       let
          val {info, param} = Instr.dest instr
       in
          case param of
             Instr.Param.Label _ =>
                let
                   val sz = sizeOf(!addr + #minSz info, instr)
                   val () = addr := !addr + sz
		in
                   if (sz = #maxSz info)
                      then code := Code.Instr{i=instr, sz=sz} :: !code
                      else code := Code.VarInstr{i=instr, sz=sz} :: !code
		end
           | _ =>
                let
                   val sz = sizeOf(!addr, instr)
                   val () = addr := !addr + sz;
		in
                   code := Code.Instr{i=instr, sz=sz} :: !code
		end
       end

  (* bind the label to the current location *)
    fun defineLabel (CS{addr, code, ...}) lab = (
	  if (Label.getLoc lab >= 0)
	    then raise Fail((Label.name lab) ^ " already defined")
	    else ();
	  Label.setLoc (lab, !addr);
	  code := Code.Loc lab :: !code)

  (* map a string literal to an index in the literal table *)
    fun string (CS{nLits, lits, ...}, name) = (case StringTbl.find lits name
	   of NONE => let
		val id = !nLits
		in
		  StringTbl.insert lits (name, id);
		  nLits := id+1;
		  id
		end
	    | SOME id => id
	  (* end case *))

  (* map a C function's name to an index in the C-function table *)
    fun c_function (CS{nCfuns, cfuns, ...}, name) = (case StringTbl.find cfuns name
	   of NONE => let
		val id = !nCfuns
		in
		  StringTbl.insert cfuns (name, id);
		  nCfuns := id+1;
		  id
		end
	    | SOME id => id
	  (* end case *))

  (* resolve span-dependent instructions in the code stream *)
    fun assemble instrs = let
	(* update the addresses of locations, while reversing the list *)
	  fun updateLocs (adr, [], instrs) = instrs
	    | updateLocs (adr, i::r, instrs) = (case i
		 of Code.Loc lab => (Label.setLoc (lab, adr);
                                     updateLocs(adr, r, i::instrs))
		  | Code.Instr{sz, ...} => updateLocs(adr-sz, r, i::instrs)
		  | Code.VarInstr{sz, ...} => updateLocs(adr-sz, r, i::instrs)
		(* end case *))
	(* fix the sizes of VarInstrs for when we have reached a fixed point *)
	  fun fixSizes ([], instrs) = instrs
	    | fixSizes (i::r, instrs) = (case i
		 of Code.VarInstr{sz, i} => fixSizes (r, Code.Instr{sz=sz, i=i}::instrs)
		  | _ => fixSizes (r, i::instrs)
		(* end case *))
	  fun resolve (false, true, adr, [], instrs) =
		{sz = adr, code = updateLocs(adr, instrs, [])}
	    | resolve (true, true, adr, [], instrs) =
		resolve (false, false, 0, updateLocs(adr, instrs, []), [])
	    | resolve (false, false, adr, [], instrs) =
		{sz = adr, code = updateLocs(adr, instrs, [])}
	    | resolve (true, false, adr, [], instrs) =
		{sz = adr, code = fixSizes (instrs, [])}
	    | resolve (anyVIs, changed, adr, instr::r, instrs) = (
		case instr
		 of Code.Loc _ => resolve(anyVIs, changed, adr, r, instr::instrs)
		  | Code.Instr{sz, ...} => resolve(anyVIs, changed, adr+sz, r, instr::instrs)
		  | Code.VarInstr{sz, i} => let
                      val {info, ...} = Instr.dest i
		      val sz' = sizeOf(adr+sz, i)
		      val changed = changed orelse (sz <> sz')
		      in
			if (sz' <> sz)
			  then if sz' = #maxSz info
			    then resolve(anyVIs, true, adr+sz', r, Code.Instr{sz=sz', i=i}::instrs)
			    else resolve(true, true, adr+sz', r, Code.VarInstr{sz=sz', i=i}::instrs)
			  else resolve(true, changed, adr+sz, r, instr::instrs)
		      end
		(* end case *))
	  in
	    resolve (false, false, 0, instrs, [])
	  end

  (* assemble the code and write the object file *)
    fun finish (outS, CS{addr, code, nLits, lits, nCfuns, cfuns}) = let
	  val {sz, code} = assemble (List.rev (!code)) before code := []
	  val litTbl = tableToArray (!nLits, lits)
	  val cfunTbl = tableToArray (!nCfuns, cfuns)
	(* emit an unsigned byte to the object file *)
          fun emitByte b = BinIO.output1(outS, Word8.fromLargeInt b)
	(* emit a unsigned 8-bit quantity *)
	  fun emitU8 w = emitByte w
	(* emit a signed 8-bit quantity *)
	  fun emitS8 i = emitByte i
	(* emit an unsigned 16-bit quantity *)
	  fun emitU16 w = (emitByte (II.~>>(w, 0w8)); emitByte w)
	(* emit a signed 16-bit quantity *)
	  fun emitS16 i = (emitByte (II.~>>(i, 0w8)); emitByte i)
	(* emit an unsigned 32-bit quantity *)
	  fun emitU32 i = (emitU16 (II.~>>(i, 0w16)); emitU16 i)
	(* emit a signed 32-bit quantity *)
	  fun emitS32 i = (emitS16 (II.~>>(i, 0w16)); emitU16 i)
	(* emit a signed 64-bit quantity *)
	  fun emitS64 i = (emitS32 (II.~>>(i, 0w32)); emitU32 i)
	(* emit a string with its length *)
	  fun emitS s = let
		val n = size s
		in
		  if (65535 < n)
		    then raise Fail "string constant too large" else ();
		  emitU16(II.fromInt n);
		  BinIO.output (outS, Byte.stringToBytes s)
		end
	(* emit an instruction *)
	  fun emitI (Code.Loc _, adr) = adr
	    | emitI (Code.Instr{i, sz}, adr) = let
                val {info as {opc, ...}, param} = Instr.dest i
		val len = if (sz = 9) then 3 else II.fromInt(sz-1)
		fun emitParam (1, _) = ()
		  | emitParam (2, Instr.Param.Int ({signed=true}, i)) = emitS8 i
		  | emitParam (3, Instr.Param.Int ({signed=true}, i)) = emitS16 i
		  | emitParam (9, Instr.Param.Int ({signed=true}, i)) = emitS64 i
		  | emitParam (2, Instr.Param.Int ({signed=false}, n)) = emitU8 n
		  | emitParam (3, Instr.Param.Int ({signed=false}, n)) = emitU16 n
		  | emitParam (sz, Instr.Param.Label lab) = (
		      if (Label.getLoc lab < 0)
			then raise Fail (concat
                                         ["undefined label: ", #name info,
                                          "(", Label.name lab, ")"])
			else ();
		      emitParam (sz, Instr.Param.Int ({signed=true}, II.fromInt ((Label.getLoc lab) - (adr+sz)))))
		  | emitParam _ = raise Fail "bogus instruction format"
		in
		  emitByte (II.orb(II.<<(len, 0w6), Word.toLargeInt opc));
		  emitParam (sz, param);
		  adr + sz
		end
	    | emitI _ = raise Fail "unexpected VarInstr"
	(* object file magic header *)
	  val magic = "vm 1.0      "
	  in
	  (* output header *)
	    BinIO.output (outS, Byte.stringToBytes magic);
	    emitU16 (II.fromInt (!nLits));
	    emitU16 (II.fromInt (!nCfuns));
	    emitU16 (II.fromInt sz);
	  (* output literals *)
	    Array.app emitS litTbl;
	  (* output C functions *)
	    Array.app emitS cfunTbl;
	  (* output code *)
	    List.foldl emitI 0 code;
	    BinIO.closeOut outS
	  end

    val new = fn () =>
       let
          val cs = new ()
          val start = Label.newSpecial "_start"
          val () = defineLabel cs start
       in
          cs
       end
  end
