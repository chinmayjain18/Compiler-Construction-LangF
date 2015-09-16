/* vm-internal.h
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
 * Internal representation of VM state.
 */

#ifndef _VM_INTERNAL_H_
#define _VM_INTERNAL_H_

#include "defs.h"
#include "opcode.h"
#include "vm.h"

/* default stack and heap sizes */
#ifndef STACK_SZB
#  define STACK_SZB     256*1024*4
#endif
#ifndef HEAP_SZB
#  define HEAP_SZB      1024*1024*4
#endif

struct struct_vm {
  /* VM registers */
  Byte_t          *pc;  /* program counter */
  Word_t          *sp;  /* stack pointer */
  Word_t          *fp;  /* frame pointer */
  Word_t          *ep;  /* environment pointer */
  Word_t          *ap;  /* allocation pointer */
  /* VM memory */
  Program_t     prog;   /* the program */
  size_t        stkSz;  /* stack size in bytes */
  Word_t        *stk;   /* pointer to base of stack */
  size_t        heapSz; /* heap size in bytes */
  Word_t        *heap;  /* pointer to base of heap */
  /* control flags */
  bool          halted;
  bool          error;  /* true, if halted with an error */
  bool          trace;
  bool          gcmsg;
  bool          result;
  /* args */
  int           argc;
  const char    **argv;
};

/* A VM integer n is represented as 2n+1 */
static inline bool isInt (Word_t v) { return ((v & 0x1) == 1); }
static inline bool isPtr (Word_t v) { return ((v & 0x3) == 0); }
static inline Word_t TagInt (Int_t n) { return (Word_t)((n << 1) + 1); }
static inline Int_t UntagInt (VM_t *vm, Word_t n) {
#ifndef NDEBUG
  if (!isInt(n)) VM_Die(vm, "UntagInt: expected integer");
#endif
  return ((Int_t)n >> 1);
}

#define VM_UNIT         1
#define VM_FALSE        1
#define VM_TRUE         3

/* heap object headers encode the object type (string or record) and length;
 * The low four bits are used for type information and the upper 28 hold the
 * length.
 */
#define HDR_TAG                 0x2
#define FORWARD_TAG             0x3
#define STRING_HDR(n)           (((n)<<4) | 0x4 | HDR_TAG)
#define RECORD_HDR(n)           (((n)<<4) | HDR_TAG)

static inline bool isRecordHdr (Word_t hdr) { return ((hdr & 0xF) == RECORD_HDR(0)); }
static inline bool isStringHdr (Word_t hdr) { return ((hdr & 0xF) == STRING_HDR(0)); }

/* extract the length field from a header */
static inline Word_t GetLength (Word_t w) { return w >> 4; }

/* predicate to test if an object is a string */
static inline bool isString (VM_Object_t s)
{
  return isPtr((Word_t)s) && isStringHdr(s[-1]);
}

/* extract the string length of a string */
static inline Int_t StringLen (VM_t *vm, VM_Object_t s)
{
#ifndef NDEBUG
  if (!isString(s)) VM_Die(vm, "StringLen: expected string");
#endif
  return (Int_t)(GetLength(s[-1]));
}

#endif /* !_VM_INTERNAL_H_ */
