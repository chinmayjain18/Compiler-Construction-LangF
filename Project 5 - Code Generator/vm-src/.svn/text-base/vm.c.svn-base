/* vm.c
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
 * COPYRIGHT (c) 2005 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 */

#include "defs.h"
#include "vm-internal.h"

/* test if an address is in range */
static inline bool inAddrRange (void *adr, int sz, void *base)
{
  return ((base <= adr) && ((char *)adr < ((char *)base + sz)));
}

/* test if an index is in the range 0..sz */
static inline bool inRange (Int_t i, Int_t sz)
{
  return ((Word_t)sz >= (Word_t)i);
}

/* NewVM:
 */
VM_t *NewVM ()
{
  VM_t        *vm;

  vm = NEW(VM_t);
  vm->prog.codeSz = 0;
  vm->prog.code = 0;
  vm->prog.nLits = 0;
  vm->prog.lits = 0;
  vm->prog.nCFuns = 0;
  vm->prog.cfuns = 0;
  vm->stkSz = STACK_SZB;
  vm->stk = (Word_t *)NEWVEC(Byte_t, vm->stkSz);
  vm->heapSz = HEAP_SZB;
  vm->heap = (Word_t *)NEWVEC(Byte_t, vm->heapSz);

  /* initialize VM registers */
  vm->pc = &(vm->prog.code[0]);
  vm->sp = (Word_t *)((Byte_t *)(vm->stk) + vm->stkSz);
  vm->fp = 0;
  vm->ep = 0;
  vm->ap = vm->heap;

  /* initialize flags */
  vm->halted = false;
  vm->error = false;
  vm->trace = false;
  vm->result = false;

  /* initialize args */
  vm->argc = 0;
  vm->argv = 0;

  return vm;
}

/* VM_Init:
 */
void VM_Init (VM_t *vm, int argc, const char **argv)
{
  VM_InitIO ();

  /* save command-line arguments */
  vm->argc = argc;
  vm->argv = argv;

  /* push a dummy return address to make things look right */
  *--(vm->sp) = 0;

  /* set the PC to the beginning of the program */
  vm->pc = vm->prog.code;
}

/* VM_Interp:
 */
int VM_Interp (VM_t *vm)
{
  Byte_t *pc;
  Word_t *sp;

#ifndef NDEBUG
  if (vm->trace) {
    fprintf(stderr, "stack: "FMTPTR" .. "FMTPTR"\n",
            (uintptr_t)(vm->stk), (uintptr_t)((Byte_t *)(vm->stk) + vm->stkSz));
    fprintf(stderr, "heap:  "FMTPTR" .. "FMTPTR"\n", 
            (uintptr_t)(vm->heap), (uintptr_t)((Byte_t *)(vm->heap) + vm->heapSz));
    fprintf(stderr, "code:  "FMTPTR" .. "FMTPTR"\n", 
            (uintptr_t)(vm->prog.code), (uintptr_t)(vm->prog.code+vm->prog.codeSz));
  }
#endif

#define SAVE_STATE      do { vm->pc = pc; vm->sp = sp; } while (0)
#define LOAD_STATE      do { pc = vm->pc; sp = vm->sp; } while (0)
#define POP()           *sp++
#define POPI()          (Int_t)*sp++
#define PUSH(W)         do { *--sp = (W); } while (0)
#define PUSHI(I)        do { *--sp = (Word_t)(I); } while (0)
#define FETCH_OPND      do {                    \
    if (ILENGTH(instr) == 1) {                  \
      opnd = *pc++;                             \
    }                                           \
    else {                                      \
      assert (ILENGTH(instr) == 2);             \
      Int_t hi = *pc++;                         \
      Int_t lo = *pc++;                         \
      opnd = ((hi << 8) | lo);                  \
    }                                           \
  } while (0)
#define FETCH_OPND0     do {                    \
    if (ILENGTH(instr) == 0)                    \
      opnd = 1;                                 \
    else FETCH_OPND;                            \
  } while (0)
#define FETCH_IOPND     do {                    \
    if (ILENGTH(instr) == 1) {                  \
      opnd = *((signed char *)pc);              \
      pc++;                                     \
    }                                           \
    else {                                      \
      assert (ILENGTH(instr) == 2);             \
      Int_t hi = *((signed char *)pc);          \
      Int_t lo = pc[1];                         \
      pc += 2;                                  \
      opnd = ((hi << 8) | lo);                  \
    }                                           \
  } while (0)

  LOAD_STATE;
  while (! vm->halted) {
    if (! inAddrRange(pc, vm->prog.codeSz, vm->prog.code)) {
      SAVE_STATE;
      VM_Die(vm, "bad PC");
      return 1;
    }
#ifndef NDEBUG
    if (vm->trace) {
      fprintf (stderr, ""FMTPTR": ", (uintptr_t)pc);
      VM_PrintInstr (stderr, vm, pc);
      if (inAddrRange(sp, vm->stkSz, vm->stk)) {
        fprintf (stderr, "; tos@"FMTPTR" = ", (uintptr_t)sp);
        VM_PrintWord (stderr, vm, *sp, 0);
        fprintf (stderr, "\n");
      }
      else
        fprintf (stderr, "\n");
    }
#endif
    Opcode_t instr = *pc++;

    switch (OPCODE(instr)) {
    case OP_HALT:
      if (vm->result) {
        VM_PrintWord (stderr, vm, *sp, 1);
        fprintf (stderr, "\n");
      }
      vm->halted = true;
      SAVE_STATE;
      break;
    case OP_ADD: {
      Int_t a = POPI()-1;
      *sp = (Word_t)((Int_t)*sp + a);
    } break;
    case OP_SUB: {
      Int_t a = POPI()-1;
      *sp = (Word_t)((Int_t)*sp - a);
    } break;
    case OP_MUL: {
      Int_t a = UntagInt(vm, POPI());
      Int_t b = UntagInt(vm, POPI());
      PUSHI (TagInt(b*a));
    } break;
    case OP_DIV: {
      Int_t a = UntagInt(vm, POPI());
      Int_t b = UntagInt(vm, POPI());
      PUSHI (TagInt(b / a));
    } break;
    case OP_MOD: {
      Int_t a = UntagInt(vm, POPI());
      Int_t b = UntagInt(vm, POPI());
      PUSHI (TagInt(b % a));
    } break;
    case OP_NEG: {
      Int_t a = UntagInt(vm, POPI());
      PUSHI (TagInt(-a));
    } break;
    case OP_EQU: {
      Word_t a = *sp++;
      *sp = (a == *sp) ? VM_TRUE : VM_FALSE;
    } break;
    case OP_LESS: {
      Int_t a = POPI();
      *sp = (*(Int_t *)sp < a) ? VM_TRUE : VM_FALSE;
    } break;
    case OP_LESSEQ: {
      Int_t a = POPI();
      *sp = (*(Int_t *)sp <= a) ? VM_TRUE : VM_FALSE;
    } break;
    case OP_NOT:
      if (*sp == VM_FALSE) *sp = VM_TRUE; else *sp = VM_FALSE;
      break;
    case OP_BOXED:
      if (isPtr(*sp) && inAddrRange((Word_t *)(*sp), vm->heapSz, vm->heap))
        *sp = VM_TRUE;
      else
        *sp = VM_FALSE;
      break;
    case OP_ALLOC: {
      Int_t opnd;
      FETCH_OPND;
      SAVE_STATE;
      Word_t *obj = VM_Alloc(vm, opnd);
      for (Int_t i = opnd-1;  i >= 0;  i--)
        obj[i] = POP();
      PUSH((Word_t)obj);
    } break;
    case OP_REPALLOC: {
      Word_t v = POP();
      Int_t n = UntagInt(vm, POPI());
      PUSH(v);
      SAVE_STATE;
      Word_t *obj = VM_Alloc(vm, n);
      v = POP();
      for (Int_t i = 0;  i < n;  i++)
        obj[i] = v;
      PUSH((Word_t)obj);
    } break;
    case OP_EXPLODE: {
      Word_t *obj = (Word_t *)POP();
      if (!isPtr((Word_t)obj) || !isRecordHdr(obj[-1])) {
        SAVE_STATE;
        VM_Die(vm, "OP_EXPLODE: expected record");
      }
      Int_t n = GetLength(obj[-1]);
      for (Int_t i = 0;  i < n;  i++)
        PUSH(obj[i]);
    } break;
    case OP_LENGTH: {
      Word_t *obj = (Word_t *)POP();
      if (!isPtr((Word_t)obj) || !isRecordHdr(obj[-1])) {
        SAVE_STATE;
        VM_Die(vm, "OP_LENGTH: expected record");
      }
      Int_t n = GetLength(obj[-1]);
      PUSHI(TagInt(n));
    } break;
    case OP_SELECT: {
      Int_t opnd;
      FETCH_OPND;
      Word_t *obj = (Word_t *)POP();
      if (!isPtr((Word_t)obj) || !isRecordHdr(obj[-1])) {
        SAVE_STATE;
        VM_Die(vm, "OP_SELECT: expected record");
      } else if (! inRange(opnd, GetLength(obj[-1]))) {
        SAVE_STATE;
        VM_Die (vm, "OP_SELECT: subscript out of bounds");
      } else
        PUSH(obj[opnd]);
    } break;
    case OP_INDEX: {
      Int_t i = UntagInt(vm, POPI());
      Word_t *obj = (Word_t *)POP();
      if (!isPtr((Word_t)obj) || !isRecordHdr(obj[-1])) {
        SAVE_STATE;
        VM_Die(vm, "OP_INDEX: expected record");
      } else if (! inRange(i, GetLength(obj[-1]))) {
        SAVE_STATE;
        VM_Die (vm, "OP_INDEX: subscript out of bounds");
      } else
        PUSH(obj[i]);
    } break;
    case OP_UPDATE: {
      Word_t v = POP();
      Int_t i = UntagInt(vm, POPI());
      Word_t *obj = (Word_t *)POP();
      if (!isPtr((Word_t)obj) || !isRecordHdr(obj[-1])) {
        SAVE_STATE;
        VM_Die(vm, "OP_UPDATE: expected record");
      } else if (! inRange(i, GetLength(obj[-1]))) {
        SAVE_STATE;
        VM_Die (vm, "OP_UPDATE: subscript out of bounds");
      } else
        obj[i] = v;
      PUSH(v);
    } break;
    case OP_INTEGER:
      if (ILENGTH(instr) == 1) {
        Int_t a = *((signed char *)pc);
        pc++;
        PUSHI(TagInt(a));
      }
      else if (ILENGTH(instr) == 2) {
        Int_t hi = *((signed char *)pc);
        Int_t lo = pc[1];
        pc += 2;
        PUSHI(TagInt((hi << 8) | lo));
      }
      else {
        assert (ILENGTH(instr) == 3);
        Int_t b7 = *((signed char *)pc);
        Int_t b6 = pc[1];
        Int_t b5 = pc[2];
        Int_t b4 = pc[3];
        Int_t b3 = pc[4];
        Int_t b2 = pc[5];
        Int_t b1 = pc[6];
        Int_t b0 = pc[7];
        pc += 8;
        PUSHI(TagInt((b7 << 56) | (b6 << 48) | (b5 << 40) | (b4 << 32) | (b3 << 24) | (b2 << 16) | (b1 << 8) | b0));
      }
      break;
    case OP_LITERAL: {
      Int_t opnd;
      FETCH_OPND;
      if (! inRange(opnd, vm->prog.nLits)) {
        SAVE_STATE;
        VM_Die(vm, "bad literal index");
      }
      SAVE_STATE;
      Int_t len = StringLen(vm, vm->prog.lits[opnd]);
      VM_Object_t res = VM_AllocString (vm, len);
      strncpy ((char *)res, (char *)(vm->prog.lits[opnd]), len);
      PUSH((Word_t)res);
    } break;
    case OP_LABEL: {
      Int_t opnd;
      FETCH_IOPND;
      PUSH((Word_t)(pc + opnd));
    } break;
    case OP_SWAP: {
      Int_t opnd;
      FETCH_OPND0;
      Word_t tmp = *sp;
      *sp = sp[opnd];
      sp[opnd] = tmp;
    } break;
    case OP_POP: {
      Int_t opnd;
      FETCH_OPND0;
      sp += opnd;
    } break;
    case OP_PUSH: {
      Int_t opnd;
      if (ILENGTH(instr) == 0)
        opnd = 0;
      else
        FETCH_OPND;
      Word_t v = sp[opnd];
      PUSH(v);
    } break;
    case OP_LOADLOCAL: {
      Int_t opnd;
      FETCH_IOPND;
      PUSH(vm->fp[opnd]);
    } break;
    case OP_STORELOCAL: {
      Int_t opnd;
      FETCH_IOPND;
      vm->fp[opnd] = POP();
    } break;
    case OP_LOADGLOBAL: {
      Int_t opnd;
      FETCH_OPND;
      PUSH(vm->ep[opnd]);
    } break;
    case OP_PUSHEP:
      PUSH((Word_t)(vm->ep));
      break;
    case OP_POPEP:
      vm->ep = (Word_t *)POP();
      break;
    case OP_JMP: {
      Int_t opnd;
      FETCH_IOPND;
      pc += opnd;
    } break;
    case OP_JMPIF: {
      Int_t opnd;
      FETCH_IOPND;
      if (POP() == VM_TRUE)
        pc += opnd;
    } break;
    case OP_CALL: {
      Byte_t *dst = (Byte_t *)POP();  /* pop the destination PC */
      PUSH((Word_t)pc);
      if (! inAddrRange(dst, vm->prog.codeSz, vm->prog.code)) {
        SAVE_STATE;
        VM_Die(vm, "OP_CALL: bogus target PC");
      } else
        pc = dst;
    } break;
    case OP_TAILCALL: {
      Byte_t *dst = (Byte_t *)POP();  /* pop the destination PC */
      Word_t arg = POP();             /* pop argument into arg */
      sp = vm->fp;                    /* cutback stack frame */
      vm->fp = (Word_t *)POP();       /* restore saved FP */
      sp[1] = arg;                    /* overwrite old argument with arg */
      if (! inAddrRange(dst, vm->prog.codeSz, vm->prog.code)) {
        SAVE_STATE;
        VM_Die(vm, "OP_TAILCALL: bogus target PC");
      } else
        pc = dst;
    } break;
    case OP_ENTRY: {
      Int_t opnd;
      FETCH_OPND;
      PUSH((Word_t)(vm->fp));
      vm->fp = sp;
      for (Int_t i = 0;  i < opnd;  i++)
         PUSHI(TagInt(0));
    } break;
    case OP_RET: {
      Word_t res = POP();             /* pop result into res */
      sp = vm->fp;                    /* cutback stack frame */
      vm->fp = (Word_t *)POP();       /* restore saved FP */
      pc = (Byte_t *)POP();           /* restore return PC */
      sp++;                           /* discard argument */
      PUSH(res);                      /* push result */
    } break;
    case OP_CCALL: {
      Int_t opnd;
      FETCH_OPND;
      SAVE_STATE;
      if (! inRange(opnd, vm->prog.nCFuns)) {
        SAVE_STATE;
        VM_Die(vm, "OP_CCALL: bad C function index");
      }
      else
        (vm->prog.cfuns[opnd])(vm);
      LOAD_STATE;
    } break;
    case OP_NOP:
      break;
    default:
      SAVE_STATE;
      VM_Die (vm, "bogus opcode");
      break;
    }
  } /* end of while */
  
  if (vm->error || (! inAddrRange(sp, vm->stkSz, vm->stk)))
    return 1;
  else
    return 0; 
}

static const char *VM_OpcodeName[NUM_OPCODES] = {
  [OP_HALT] =       "halt",
  [OP_ADD] =        "add",
  [OP_SUB] =        "sub",
  [OP_MUL] =        "mul",
  [OP_DIV] =        "div",
  [OP_MOD] =        "mod",
  [OP_NEG] =        "neg",
  [OP_EQU] =        "equ",
  [OP_LESS] =       "less",
  [OP_LESSEQ] =     "lesseq",
  [OP_NOT] =        "not",
  [OP_BOXED] =      "boxed",
  [OP_ALLOC] =      "alloc",
  [OP_REPALLOC] =   "repalloc",
  [OP_EXPLODE] =    "explode",
  [OP_LENGTH] =     "length",
  [OP_SELECT] =     "select",
  [OP_INDEX] =      "index",
  [OP_UPDATE] =     "update",
  [OP_INTEGER] =    "integer",
  [OP_LITERAL] =    "literal",
  [OP_LABEL] =      "label",
  [OP_SWAP] =       "swap",
  [OP_POP] =        "pop",
  [OP_PUSH] =       "push",
  [OP_LOADLOCAL] =  "loadlocal",
  [OP_STORELOCAL] = "storelocal",
  [OP_LOADGLOBAL] = "loadglobal",
  [OP_PUSHEP] =     "pushep",
  [OP_POPEP] =      "popep",
  [OP_JMP] =        "jmp",
  [OP_JMPIF] =      "jmpif",
  [OP_CALL] =       "call",
  [OP_TAILCALL] =   "tailcall",
  [OP_ENTRY] =      "entry",
  [OP_RET] =        "ret",
  [OP_CCALL] =      "ccall",
  [OP_NOP] =        "nop",
};

/* VM_PrintInstr:
 *
 * Print the instruction at location pc to the file out.  This function returns the address
 * of the next instruction.
 */
Byte_t *VM_PrintInstr (FILE *out, VM_t *vm, Byte_t *pc)
{
  Byte_t instr = *pc++;
  int opcode = OPCODE(instr);
  
  if (opcode > OP_NOP) {
    fprintf(out, "bogus opcode = %d", opcode);
    return pc;
  }

  switch (ILENGTH(instr)) {
  case 0:
    fprintf(out, "%s", VM_OpcodeName[opcode]);
    break;
  case 1:
    switch (opcode) {
    case OP_LABEL:
    case OP_JMP:
    case OP_JMPIF: {
      Int_t offset = *((signed char *)pc);
      pc++;
      fprintf(out, "%s("FMTINT":s) ["FMTPTR"]", VM_OpcodeName[opcode], offset, (uintptr_t)(pc+offset));
    } break;
    case OP_INTEGER:
    case OP_LOADLOCAL:
    case OP_STORELOCAL:
      fprintf(out, "%s(%d:b)", VM_OpcodeName[opcode], *((signed char *)pc));
      pc++;
      break;
    default:
      fprintf(out, "%s(%d:b)", VM_OpcodeName[opcode], *pc++);
      break;
    }
    break;
  case 2: {
    Int_t hi = *((signed char *)pc);
    Int_t lo = pc[1];
    pc += 2;
    Int_t offset = (hi << 8) | lo;
    switch (opcode) {
    case OP_LABEL:
    case OP_JMP:
    case OP_JMPIF:
      fprintf(out, "%s("FMTINT":s) ["FMTPTR"]", VM_OpcodeName[opcode], offset, (uintptr_t)(pc+offset));
      break;
    case OP_INTEGER:
    case OP_LOADLOCAL:
    case OP_STORELOCAL:
      fprintf(out, "%s("FMTINT":s)", VM_OpcodeName[opcode], offset);
      break;
    default:
      fprintf(out, "%s("FMTINT":s)", VM_OpcodeName[opcode], offset & 0xffff);
      break;
    }
  } break;
  case 3: {
    Int_t b7 = *((signed char *)pc);
    Int_t b6 = pc[1];
    Int_t b5 = pc[2];
    Int_t b4 = pc[3];
    Int_t b3 = pc[4];
    Int_t b2 = pc[5];
    Int_t b1 = pc[6];
    Int_t b0 = pc[7];
    fprintf(out, "%s("FMTINT":l)", VM_OpcodeName[opcode],
            (b7 << 56) | (b6 << 48) | (b5 << 40) | (b4 << 32) |
            (b3 << 24) | (b2 << 16) | (b1 << 8) | b0);
  } break;
  default:
    fprintf(stderr, "Fatal VM error: bogus instruction length(%d) at "FMTPTR"\n", ILENGTH(instr), (uintptr_t)(pc-1));
    exit(1);
  }
  
  return pc; 
}

/* VM_PrintWord:
 */
void VM_PrintWord (FILE *f, VM_t *vm, Word_t w, int depth)
{
  if (inAddrRange((Word_t *)w, vm->prog.codeSz, vm->prog.code))
    fprintf(f, FMTPTR":code", (uintptr_t)w);
  else if (isInt(w))
    fprintf(f, FMTINT":int", UntagInt(vm, w));
  else if (! isPtr(w))
    fprintf(f, FMTWORD":hdr", w);
  else if (inAddrRange((Word_t *)w, vm->heapSz, vm->heap)) {
    if (isString((Word_t *)w)) {
      int len = StringLen(vm, (Word_t *)w);
      fprintf(f, FMTPTR":heap -> \"", (uintptr_t)w);
      for (int i = 0; i < len; i++) {
        char ch = ((char *)w)[i];
        switch (ch) {
        case '\\':
          fputs("\\\\", f);
          break;
        case '\"':
          fputs("\\\"", f);
          break;
        case '\'':
          fputs("\\\'", f);
          break;
        case '\n':
          fputs("\\n", f);
          break;
        case '\t':
          fputs("\\t", f);
          break;
        default:
          if (iscntrl(ch)) {
            fprintf(f, "\\x%02x", ch);
          } else {
            fputc(ch, f);
          }
        }
      }
      fprintf(f, "\"");
    } else {
      if (depth <= 0) {
        fprintf(f, FMTPTR":heap", (uintptr_t)w);
      } else {
        fprintf(f, FMTPTR":heap -> <", (uintptr_t)w);
        Int_t len = GetLength(((Word_t *)w)[-1]);
        for (Int_t i = 0; i < len; i++) {
          if (i > 0) fprintf(f, ",");
          VM_PrintWord(f, vm, ((Word_t *)w)[i], depth - 1);
        }
        fprintf(f, ">");
      }
    }
  } else if (inAddrRange((Word_t *)w, vm->stkSz, vm->stk))
    fprintf(f, FMTPTR":stk", (uintptr_t)w);
  else if (inAddrRange((Word_t *)w, vm->prog.codeSz, vm->prog.code))
    fprintf(f, FMTPTR":code", (uintptr_t)w);
  else
    fprintf(f, FMTWORD":ptr", w);
}

/* VM_Die:
 */
void VM_Die (VM_t *vm, const char *msg)
{
  fprintf(stderr, "Fatal VM error: %s\n", msg);
  if (vm != 0) {
    fprintf(stderr, "  stack: "FMTPTR" .. "FMTPTR"\n",
            (uintptr_t)(vm->stk), (uintptr_t)((Byte_t *)(vm->stk) + vm->stkSz));
    fprintf(stderr, "  heap:  "FMTPTR" .. "FMTPTR"\n", 
            (uintptr_t)(vm->heap), (uintptr_t)((Byte_t *)(vm->heap) + vm->heapSz));
    fprintf(stderr, "  code:  "FMTPTR" .. "FMTPTR"\n", 
            (uintptr_t)(vm->prog.code), (uintptr_t)(vm->prog.code+vm->prog.codeSz));
    fprintf(stderr, "  registers: pc = "FMTPTR", sp = "FMTPTR", fp = "FMTPTR", ep = "FMTPTR", ap = "FMTPTR"\n",
            (uintptr_t)(vm->pc), (uintptr_t)(vm->sp), (uintptr_t)(vm->fp),
            (uintptr_t)(vm->ep), (uintptr_t)(vm->ap));
    vm->halted = true;
    vm->error = true;
  }
  exit (1);
}
