/* main.c
 *
 * COPYRIGHT (c) 2005 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 */

#include "vm-internal.h"

static void usage ();

int main (int argc, const char **argv)
{
    VM_t        *vm;
    const char  *file = 0;
    int         i, sts;

    vm = NewVM();

  /* Process command-line args */
    for (i = 1;  i < argc;  ) {
        const char *arg = argv[i++];
        if (arg[0] == '-') {
            switch (arg[1]) {
              case 'h':
                usage ();
                return 0;
              case 't':
                vm->trace = true;
                break;
              case 'r':
                vm->result = true;
                break;
              case 'g':
                vm->gcmsg = true;
                break;
              default:
                usage ();
                return 1;
            }
        }
        else {
          /* the rest of the arguments are passed to the program */
            file = arg;
            break;
        }
    }

    if (! VM_LoadProgram(vm, file))
        return 1;

    VM_Init (vm, argc-(i-1), argv+(i-1));
    sts = VM_Interp (vm);
    VM_FinishIO ();

    return sts;

}

static void usage ()
{
    fprintf (stderr, "vm (CSCI-742: S20145)\n");
    fprintf (stderr, "usage: vm [options] file [program-args]\n");
    fprintf (stderr, "  options:\n");
    fprintf (stderr, "    -t    trace execution\n");
    fprintf (stderr, "    -r    display result (top-of-stack)\n");
    fprintf (stderr, "    -g    display gc messages\n");
    fprintf (stderr, "    -h    display help message\n");
}
