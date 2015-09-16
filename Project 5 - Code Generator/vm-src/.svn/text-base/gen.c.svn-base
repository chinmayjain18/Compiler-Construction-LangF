/* gen.c
 *
 * COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Generate opcode information from C header file.
 */

#include "opcode.h"

typedef struct {
    char        *name;
    int         minSz, maxSz;
} Info_t;

Info_t Info[NUM_OPCODES] = {
        [OP_HALT] =             { .name = "halt",       .minSz = 1, .maxSz = 1 },
        [OP_ADD] =              { .name = "add",        .minSz = 1, .maxSz = 1 },
        [OP_SUB] =              { .name = "sub",        .minSz = 1, .maxSz = 1 },
        [OP_MUL] =              { .name = "mul",        .minSz = 1, .maxSz = 1 },
        [OP_DIV] =              { .name = "div",        .minSz = 1, .maxSz = 1 },
        [OP_MOD] =              { .name = "mod",        .minSz = 1, .maxSz = 1 },
        [OP_NEG] =              { .name = "neg",        .minSz = 1, .maxSz = 1 },
        [OP_EQU] =              { .name = "equ",        .minSz = 1, .maxSz = 1 },
        [OP_LESS] =             { .name = "less",       .minSz = 1, .maxSz = 1 },
        [OP_LESSEQ] =           { .name = "lesseq",     .minSz = 1, .maxSz = 1 },
        [OP_NOT] =              { .name = "not",        .minSz = 1, .maxSz = 1 },
        [OP_BOXED] =            { .name = "boxed",      .minSz = 1, .maxSz = 1 },
        [OP_ALLOC] =            { .name = "alloc",      .minSz = 2, .maxSz = 3 },
        [OP_REPALLOC] =         { .name = "repalloc",   .minSz = 1, .maxSz = 1 },
        [OP_EXPLODE] =          { .name = "explode",    .minSz = 1, .maxSz = 1 },
        [OP_LENGTH] =           { .name = "length",     .minSz = 1, .maxSz = 1 },
        [OP_SELECT] =           { .name = "select",     .minSz = 2, .maxSz = 3 },
        [OP_INDEX] =            { .name = "index",      .minSz = 1, .maxSz = 1 },
        [OP_UPDATE] =           { .name = "update",     .minSz = 1, .maxSz = 1 },
        [OP_INT] =              { .name = "int",        .minSz = 2, .maxSz = 5 },
        [OP_LITERAL] =          { .name = "literal",    .minSz = 2, .maxSz = 3 },
        [OP_LABEL] =            { .name = "label",      .minSz = 2, .maxSz = 3 },
        [OP_SWAP] =             { .name = "swap",       .minSz = 1, .maxSz = 3 },
        [OP_POP] =              { .name = "pop",        .minSz = 1, .maxSz = 3 },
        [OP_PUSH] =             { .name = "push",       .minSz = 2, .maxSz = 3 },
        [OP_LOADLOCAL] =        { .name = "loadlocal",  .minSz = 2, .maxSz = 3 },
        [OP_STORELOCAL] =       { .name = "storelocal", .minSz = 2, .maxSz = 3 },
        [OP_LOADGLOBAL] =       { .name = "loadglobal", .minSz = 2, .maxSz = 3 },
        [OP_PUSHEP] =           { .name = "pushep",     .minSz = 1, .maxSz = 1 },
        [OP_POPEP] =            { .name = "popep",      .minSz = 1, .maxSz = 1 },
        [OP_JMP] =              { .name = "jmp",        .minSz = 2, .maxSz = 3 },
        [OP_JMPIF] =            { .name = "jmpif",      .minSz = 2, .maxSz = 3 },
        [OP_CALL] =             { .name = "call",       .minSz = 1, .maxSz = 1 },
        [OP_TAILCALL] =         { .name = "tailcall",   .minSz = 1, .maxSz = 1 },
        [OP_ENTRY] =            { .name = "entry",      .minSz = 2, .maxSz = 3 },
        [OP_RET] =              { .name = "ret",        .minSz = 1, .maxSz = 1 },
        [OP_CCALL] =            { .name = "ccall",      .minSz = 2, .maxSz = 3 },
        [OP_NOP] =              { .name = "nop",        .minSz = 1, .maxSz = 1 },
    };

int main (int argc, char **argv)
{
    int         i;

    for (i = 0;  i < NUM_OPCODES;  i++) {
        printf("    val %sInfo\t= {opc = 0w%02d, name = \"%s\", minSz = %d, maxSz = %d}\n",
            Info[i].name, i, Info[i].name, Info[i].minSz, Info[i].maxSz);
    }
    return 0;
}
