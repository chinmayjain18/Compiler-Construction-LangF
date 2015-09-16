/* heap.c
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

#include <string.h>
#include "vm-internal.h"

static inline Word_t TagFP (Word_t *p) { return (Word_t)p | FORWARD_TAG; }
static inline Word_t UntagFP (Word_t w) { return (w & ~FORWARD_TAG); }
static inline Word_t isFP (Word_t w) { return (w & FORWARD_TAG) == FORWARD_TAG; }

static void VM_GC (VM_t *vm, size_t n);
static Word_t ForwardPtr (VM_t *vm, Word_t *obj);


/* VM_Alloc:
 *
 * Allocate an n-word heap object (but do not initialize it).
 */
VM_Object_t VM_Alloc (VM_t *vm, size_t n)
{
  Word_t *next = vm->ap + n + 1;
  VM_Object_t obj;

  if ((Word_t)(vm->heap) + vm->heapSz <= (Word_t)next) {
    VM_GC (vm, n);
    next = vm->ap + n + 1;
  }

  *vm->ap++ = RECORD_HDR(n);
  obj = vm->ap;
  vm->ap = next;

  return obj;
}

/* VM_AllocString:
 */
VM_Object_t VM_AllocString (VM_t *vm, size_t len)
{
  size_t n = (len + (sizeof(Word_t)-1)) >> 2;
  Word_t *next = vm->ap + n + 1;
  VM_Object_t obj;

  if ((Word_t)(vm->heap) + vm->heapSz <= (Word_t)next) {
    VM_GC (vm, n);
    next = vm->ap + n + 1;
  }

  *vm->ap++ = STRING_HDR(len);
  obj = vm->ap;
  vm->ap = next;

  return obj;
}

/* VM_GC:
 *
 * A very simple semi-space copying GC.
 */
static void VM_GC (VM_t *vm, size_t n)
{
  Word_t *toSpace = (Word_t *)NEWVEC(Byte_t, vm->heapSz);
  Word_t *sp, *spTop, *nextScan, w;
  Word_t *fromSpace = vm->heap;
  Word_t *fromSpaceTop = (Word_t *)((Word_t)fromSpace + vm->heapSz);

#ifndef NDEBUG
  if (vm->trace || vm->gcmsg) {
    fprintf(stderr, "GC starting:\n");
    fprintf(stderr, "  registers: pc = "FMTPTR", sp = "FMTPTR", fp = "FMTPTR", ep = "FMTPTR", ap = "FMTPTR"\n",
            (uintptr_t)(vm->pc), (uintptr_t)(vm->sp), (uintptr_t)(vm->fp),
            (uintptr_t)(vm->ep), (uintptr_t)(vm->ap));
    fprintf(stderr, "  from-space: "FMTPTR" .. "FMTPTR"\n",
            (uintptr_t)(vm->heap), (uintptr_t)((Byte_t *)(vm->heap) + vm->heapSz));
    fprintf(stderr, "  to-space:   "FMTPTR" .. "FMTPTR"\n",
            (uintptr_t)(toSpace), (uintptr_t)((Byte_t *)(toSpace) + vm->heapSz));
  }
#endif

  /* initialize to-space */
  nextScan = toSpace;
  vm->ap = toSpace;
  vm->heap = toSpace;

  /* scan stack for roots */
  sp = vm->sp;
  spTop = (Word_t *)((Word_t)vm->stk + vm->stkSz);
  while (sp < spTop) {
    w = *sp;
    if (isPtr(w)) {
      Word_t *wp = (Word_t *)w;
      if ((fromSpace <= wp) && (wp < fromSpaceTop))
        *sp = ForwardPtr(vm, wp);
    }
    sp++;
  }

  /* scan registers for roots */
  w = (Word_t)(vm->ep);
  if (isPtr(w)) {
    Word_t *wp = (Word_t *)w;
    if ((fromSpace <= wp) && (wp < fromSpaceTop))
      vm->ep = (Word_t *)(ForwardPtr(vm, wp));
  }

  /* do the copy collection */
  while (nextScan < vm->ap) {
    assert (vm->ap <= (Word_t *)((Word_t)toSpace + vm->heapSz));
    Word_t hdr = *nextScan;
    nextScan++;
    switch (hdr & 0xf) {
    case HDR_TAG: { /* record object */
      int len = GetLength(hdr);
      while (len > 0) {
        w = *nextScan;
        if (isPtr(w)) {
          Word_t *wp = (Word_t *)w;
          if ((fromSpace <= wp) && (wp < fromSpaceTop))
            *nextScan = ForwardPtr(vm, wp);
        }
        len--;
        nextScan++;
      }
    } break;
    case 0x4 | HDR_TAG: { /* string object */
      size_t len = GetLength(hdr);
      /* convert byte length to number of words */
      len = (len + (sizeof(Word_t)-1)) >> 2;
      nextScan += len;
    } break;
    default:
      fprintf(stderr, "Fatal VM error: bogus object header "FMTWORD" at "FMTPTR"\n", hdr, (uintptr_t)nextScan);
      exit (1);
    }
  }

  /* free from-space */
  free (fromSpace);

  if (((Word_t)vm->heap + vm->heapSz) < (Word_t)(vm->ap + n)) {
    fprintf(stderr, "Fatal VM error: memory exhausted\n");
    exit (1);
  }

#ifndef NDEBUG
  if (vm->trace || vm->gcmsg) {
    fprintf(stderr, "GC finished:\n");
    fprintf(stderr, "  registers: pc = "FMTPTR", sp = "FMTPTR", fp = "FMTPTR", ep = "FMTPTR", ap = "FMTPTR"\n",
            (uintptr_t)(vm->pc), (uintptr_t)(vm->sp), (uintptr_t)(vm->fp),
            (uintptr_t)(vm->ep), (uintptr_t)(vm->ap));
  }
#endif
}

/* ForwardPtr:
 * Forward the object
 */
Word_t ForwardPtr (VM_t *vm, Word_t *obj)
{
  Word_t hdr = obj[-1];
  Word_t *new;

  if (isFP(hdr))
    return UntagFP(hdr);
  else switch (hdr & 0xf) {
    case HDR_TAG: { /* record object */
      size_t len = GetLength(hdr);
      memcpy (vm->ap, obj-1, sizeof(Word_t) * (len+1));
      new = vm->ap+1;
      vm->ap = new+len;
      obj[-1] = TagFP(new);
      return (Word_t)new;
    } break;
    case 0x4 | HDR_TAG: { /* string object */
      size_t len = GetLength(hdr);
      /* convert byte length to number of words */
      len = (len + (sizeof(Word_t)-1)) >> 2;
      memcpy (vm->ap, obj-1, sizeof(Word_t) * (len+1));
      new = vm->ap+1;
      vm->ap = new+len;
      obj[-1] = TagFP(new);
      return (Word_t)new;
    } break;
    default:
      fprintf(stderr, "Fatal VM error: bogus object header "FMTWORD" at "FMTPTR"\n", hdr, (uintptr_t)obj);
      exit (1);
    }
}
