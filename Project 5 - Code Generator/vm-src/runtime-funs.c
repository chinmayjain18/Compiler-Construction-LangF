/* runtime-funs.c
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

#include <stdio.h>
#include <string.h>
#include "vm-internal.h"

#define POP()           (*(vm->sp)++)
#define POPI()          ((Int_t)*(vm->sp++))
#define POPP()          ((VM_Object_t)*(vm->sp++))
#define PEEK(N)         (*(vm->sp+(N)))
#define PEEKI(N)        ((Int_t)*(vm->sp+(N)))
#define PEEKP(N)        ((VM_Object_t)*(vm->sp+(N)))
#define PUSH(W)         do { *--(vm->sp) = (W); } while (0)
#define PUSHI(I)        do { *--(vm->sp) = (Word_t)(I); } while (0)

#define MAX_OPEN        32
static FILE     *OpenIn[MAX_OPEN];
static int      NOpenIn = 0;
static FILE     *OpenOut[MAX_OPEN];
static int      NOpenOut = 0;


/* VM_InitIO:
 */
void VM_InitIO ()
{
  OpenIn[0] = stdin;
  NOpenIn++;
  OpenOut[0] = stdout;
  NOpenOut++;
}

/* VM_FinishIO:
 */
void VM_FinishIO ()
{
  int i;

  for (i = 1;  i < NOpenIn;  i++)
    fclose (OpenIn[i]);
  fflush (stdout);
  for (i = 1;  i < NOpenOut;  i++)
    fclose (OpenOut[i]); 
}

/* VM_Argc:
 *
 *      unit  <argc>  ==>  n
 */
void VM_Argc (VM_t *vm)
{
  (void)POP();
  PUSHI(TagInt((Int_t)(vm->argc)));
}

/* VM_Arg:
 *
 *      n  <arg>  ==>  str
 */
void VM_Arg (VM_t *vm)
{
  Int_t i = POPI();

  assert (isInt(i));

  i = UntagInt(vm, i);

  VM_Object_t res;
  if (0 <= i && i < vm->argc) {
    Int_t len = strlen(vm->argv[i]);
    res = VM_AllocString (vm, len);
    strncpy ((char *)res, vm->argv[i], len);
  } else {
    res = VM_AllocString (vm, 0);
  }
  PUSH((Word_t)res);
}

/* VM_Print:
 *
 *      fid str  <print>  ==>  u
 */
void VM_Print (VM_t *vm)
{
  VM_Object_t s = POPP();
  Int_t fid = POPI();
  assert (isString(s));
  assert (isInt(fid));

  fid = UntagInt(vm, fid);
  if ((0 <= fid) && (fid < NOpenOut)) {
    Int_t len = StringLen(vm, s);
    if (len > 0) {
      if (fwrite (s, len, 1, OpenOut[fid]) != 1)
        VM_Die(vm, "VM_Print: I/O error");
    }
  }
  else
    VM_Die(vm, "VM_Print: bogus fid");;
  PUSHI(VM_UNIT);
}

/* VM_Size:
 *
 *      str  <size>  ==> n
 */
void VM_Size (VM_t *vm)
{
  VM_Object_t s = POPP();
  assert (isString(s));
  PUSHI(TagInt(StringLen(vm, s))); 
}

/* VM_Concat:
 *
 *      str1 str2  <concat>  ==> str
 */
void VM_Concat (VM_t *vm)
{
  /* do not pop; need to preserve root over GC */
  VM_Object_t s2 = PEEKP(0);
  assert (isString(s2));
  VM_Object_t s1 = PEEKP(1);
  assert (isString(s1));

  /* total length */
  Int_t len = 0;
  len += StringLen(vm, s1);
  len += StringLen(vm, s2);

  /* allocate storage for the result */
  VM_Object_t res = VM_AllocString (vm, len);

  /* copy strings into result */
  s2 = POPP();
  assert (isString(s2));
  s1 = POPP();
  assert (isString(s1));

  char *p = (char *)res;
  len = StringLen(vm, s1);
  strncpy (p, (char *)s1, len);
  p += len;
  len = StringLen(vm, s2);
  strncpy (p, (char *)s2, len);
  p += len;

  PUSH ((Word_t)res);
}

/* VM_Subscript:
 *
 *      str i  <sub>  ==>  chr
 */
void VM_Subscript (VM_t *vm)
{
  Int_t i = POPI();
  VM_Object_t s = POPP();

  assert (isInt(i));
  assert (isString(s));

  i = UntagInt(vm, i);
  if ((0 <= i) && (i < StringLen(vm, s))) {
    PUSHI(TagInt((Int_t)(((unsigned char *)s)[i])));
  } else {
    PUSHI(TagInt(0));
  }
}
