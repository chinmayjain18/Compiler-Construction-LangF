/* vm.h
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
 *
 * Public interface to the VM.
 */

#ifndef _VM_H_
#define _VM_H_

#include "defs.h"
#include <stdio.h>

typedef unsigned char Byte_t;

typedef uint64_t Word_t;
#define FMTWORD "0x%016"PRIx64

typedef int64_t Int_t;
#define FMTINT "%"PRId64

typedef Word_t *VM_Object_t;

typedef struct struct_vm VM_t;

typedef void (*RuntimeFun_t)(VM_t *);

typedef struct { /* a VM program produced by the compiler */
  size_t        codeSz; /* the program size in bytes */
  Byte_t        *code;  /* the program data */
  size_t        nLits;  /* size of literal table */
  VM_Object_t   *lits;
  size_t        nCFuns; /* size of C function table */
  RuntimeFun_t  *cfuns;
} Program_t;

extern VM_t *NewVM ();
extern bool VM_LoadProgram (VM_t *vm, const char *name);
extern int VM_Interp (VM_t *vm);
extern void VM_Init (VM_t *vm, int arg, const char **argv);
extern void VM_Die (VM_t *vm, const char *);
extern VM_Object_t VM_Alloc (VM_t *vm, size_t n);
extern VM_Object_t VM_AllocString (VM_t *vm, size_t n);
extern void VM_InitIO();
extern void VM_FinishIO();

extern Byte_t *VM_PrintInstr (FILE *out, VM_t *vm, Byte_t *pc);
extern void VM_PrintWord (FILE *out, VM_t *vm, Word_t w, int depth);

#endif /* !_VM_H_ */
