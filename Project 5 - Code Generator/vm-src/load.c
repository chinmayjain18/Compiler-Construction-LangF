/* load.c
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
 * Object-file loader.
 */

#include <string.h>
#include "vm-internal.h"
#include "runtime-funs.h"

static inline unsigned short tohost16 (unsigned char s[2])
{
    return ((s[0] << 8) | s[1]);
}

#define MAGIC           "vm 1.0"
#define MAGIC_SZ        (sizeof(MAGIC)-1)

/* VM_LoadProgram:
 */
bool VM_LoadProgram (VM_t *vm, const char *name)
{
    struct {
        char            magic[12];
        unsigned char   csNLits[2];
        unsigned char   csNCFuns[2];
        unsigned char   csCodeSz[2];
    }           objFileHdr;
    Program_t   *prog = &(vm->prog);
    FILE        *objFile;
    int         i, j;

    if (vm->prog.codeSz > 0) {
        Free (prog->code);      prog->code = 0;
        Free (prog->lits);      prog->lits = 0;
        Free (prog->cfuns);     prog->cfuns = 0;
    }

    objFile = fopen (name, "rb");
    if (objFile == NULL) {
        fprintf(stderr, "Fatal VM error: unable to open \"%s\"\n", name);
        return false;
    }

    if (fread(&objFileHdr, sizeof(objFileHdr), 1, objFile) != 1) {
        fprintf(stderr, "Fatal VM error: error reading object file header\n");
        fclose(objFile);
        return false;
    }
    if (strncmp(MAGIC, objFileHdr.magic, MAGIC_SZ) != 0) {
        fprintf(stderr, "Fatal VM error: bogus object file header:\n");
        fclose(objFile);
        return false;
    }

    prog->nLits = tohost16(objFileHdr.csNLits);
    prog->nCFuns = tohost16(objFileHdr.csNCFuns);
    prog->codeSz = tohost16(objFileHdr.csCodeSz);

  /* load the literal table */
    if (prog->nLits > 0) {
        prog->lits = NEWVEC(VM_Object_t, prog->nLits);
        for (i = 0;  i < prog->nLits;  i++) {
            unsigned char csSZ[2];
            unsigned short sz;
            if (fread(&csSZ, sizeof(sz), 1, objFile) != 1) goto error;
            sz = tohost16(csSZ);
            Word_t *obj = (Word_t *)Malloc(sz + sizeof(Word_t));
            *obj++ = STRING_HDR(sz);
            prog->lits[i] = (VM_Object_t)obj;
            if (sz > 0) {
                if (fread(obj, sz, 1, objFile) != 1) goto error;
            }
        }
    }
    else prog->lits = 0;

  /* load the C function table */
    if (prog->nCFuns > 0) {
        prog->cfuns = NEWVEC(RuntimeFun_t, prog->nCFuns);
        for (i = 0;  i < prog->nCFuns;  i++) {
            unsigned char csSZ[2];
            unsigned short sz;
            char buf[64];
            if (fread(&csSZ, sizeof(sz), 1, objFile) != 1) goto error;
            sz = tohost16(csSZ);
            assert (sz < sizeof(buf));
            if (fread(buf, sz, 1, objFile) != 1) goto error;
          /* search the C function table for the function */
            for (j = 0;  RTFunTbl[j].name != 0;  j++) {
                if (sz == strlen(RTFunTbl[j].name)
                    && strncmp(buf, RTFunTbl[j].name, sz) == 0)
                    break;
            }
            if (RTFunTbl[j].name == 0) {
                fprintf(stderr, "Fatal VM error: unrecognized cfun: \"%s\"\n", buf);
                goto error;
            }
            prog->cfuns[i] = RTFunTbl[j].fn;
        }
    }
    else prog->cfuns = 0;

  /* load the code */
    prog->code = NEWVEC(Byte_t, prog->codeSz);
    if (fread(prog->code, prog->codeSz, 1, objFile) != 1) goto error;

    return true;

  error: /* jump here for cleanup on I/O error */
    fprintf(stderr, "Fatal VM error: error reading object file\n");
    fclose (objFile);
    Free (prog->lits);
    Free (prog->cfuns);
    Free (prog->code);

    return false;

} /* end of VM_LoadProgram */
