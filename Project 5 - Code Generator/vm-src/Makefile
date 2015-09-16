# vm-src/Makefile
#
# COPYRIGHT (c) 2011-2015 Matthew Fluet (http://www.cs.rit.edu/~mtf)
# All rights reserved.
#
# Rochester Institute of Technology
# 4005-711,CSCI-742
# Q20112,Q20122,S20135,S20145
#
# COPYRIGHT (c) 2009 Matthew Fluet (http://tti-c.org/fluet)
# All rights reserved.
#
# University of Chicago
# CMSC 22610
# Winter 2009
#
# COPYRIGHT (c) 2007 John Reppy (http://www.cs.uchicago.edu/~jhr)
# All rights reserved.
#
# University of Chicago
# CMSC 22610
# Winter 2007
#
# Makefile for VM
#

#
# c compiler
#
CC := gcc
CFLAGS := -std=gnu99 -m64 -Wall -g -O
CPPFLAGS :=

#
# vm executable
#
VM := ../bin/vm

all: $(VM)

SOURCES =	main.c vm.c heap.c load.c runtime-funs.c
OBJS =		$(SOURCES:.c=.o)

$(VM): $(OBJS) .depend
	$(CC) $(CFLAGS) -o $(VM) $(OBJS) $(LIBS)

#
# gcc dependency analysis
#
.depend: $(SOURCES)
	$(CC) $(CFLAGS) $(CPPFLAGS) -MM $^ > .depend

sinclude .depend

.PHONY: clean
clean:
	rm -f $(VM) $(OBJS) .depend
