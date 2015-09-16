/* defs.h
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

#ifndef _DEFS_H_
#define _DEFS_H_

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <inttypes.h>
#include <ctype.h>
#include <string.h>

#define FMTPTR "0x%016"PRIxPTR

/* checked allocation */
static inline void *Malloc (size_t sz)
{
  void *nd = malloc(sz);
  if (nd == 0) {
    fprintf(stderr, "Fatal VM Error: unable to allocate memory\n");
    exit (1);
  }
  return nd;
}

static inline void Free (void *p)
{
  if (p != 0) free(p);
}

#define NEW(ty)         (ty *)Malloc(sizeof(ty))
#define NEWVEC(ty, n)   (ty *)Malloc(sizeof(ty)*n)

#endif /* !_DEFS_H_ */
