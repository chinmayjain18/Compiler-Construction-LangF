/* opcode.h
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
 */

#ifndef _OPCODE_H_
#define _OPCODE_H_

#include <stdio.h>

typedef enum { /* VM instruction opcodes (w/o length field) */
  OP_HALT = 0,
  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
  OP_MOD,
  OP_NEG,
  OP_EQU,
  OP_LESS,
  OP_LESSEQ,
  OP_NOT,
  OP_BOXED,
  OP_ALLOC,
  OP_REPALLOC,
  OP_EXPLODE,
  OP_LENGTH,
  OP_SELECT,
  OP_INDEX,
  OP_UPDATE,
  OP_INTEGER,
  OP_LITERAL,
  OP_LABEL,
  OP_SWAP,
  OP_POP,
  OP_PUSH,
  OP_LOADLOCAL,
  OP_STORELOCAL,
  OP_LOADGLOBAL,
  OP_PUSHEP,
  OP_POPEP,
  OP_JMP,
  OP_JMPIF,
  OP_CALL,
  OP_TAILCALL,
  OP_ENTRY,
  OP_RET,
  OP_CCALL,
  OP_NOP,
  NUM_OPCODES
} Opcode_t;

/* macros for factoring opcodes */
#define ILENGTH(OP)     ((unsigned)((OP)>>6))
#define OPCODE(OP)      (0x3f & (OP))   /* mask out length part of opcode */

#endif /* !_OPCODE_H_ */
