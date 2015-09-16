/* runtime-funs.h
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

#ifndef _RUNTIME_FUNS_H_
#define _RUNTIME_FUNS_H_

extern void VM_Argc (VM_t *);
extern void VM_Arg (VM_t *);
extern void VM_Print (VM_t *);
/* extern void VM_PrintLn (VM_t *); */
/* extern void VM_ReadLn (VM_t *); */
/* extern void VM_OpenIn (VM_t *); */
/* extern void VM_OpenOut (VM_t *); */
extern void VM_Size (VM_t *);
extern void VM_Concat (VM_t *);
extern void VM_Subscript (VM_t *);
/* extern void VM_Substring (VM_t *); */
/* extern void VM_IntToString (VM_t *); */
/* extern void VM_StringCmp (VM_t *); */

#define RT_FUN(F)       { .name = #F, .fn = F }

typedef struct {
    const char          *name;
    RuntimeFun_t        fn;
} RTFunInfo_t;

static RTFunInfo_t RTFunTbl[] = {
        RT_FUN(VM_Argc),
        RT_FUN(VM_Arg),
        RT_FUN(VM_Print),
        RT_FUN(VM_Size),
        RT_FUN(VM_Concat),
        RT_FUN(VM_Subscript),
        { 0, 0 }
    };

#endif /* !_RUNTIME_FUNS_H_ */
