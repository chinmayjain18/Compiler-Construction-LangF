# langfc-src/Makefile
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
# Makefile for LangF compiler (langfc);
# target of the build is either a MLton generated executable or an
# SML/NJ heap image.
#

#
# MLton tools
#
LANGFC_MLB		:= langfc.mlb
LANGFC_MLTON_EXEC	:= ../bin/.mlton-exec/langfc.mlton
MLTON			:= mlton
MLTON_FLAGS		:= -const 'Exn.keepHistory true'

#
# SML/NJ tools and heap suffix
#
SMLNJ_BUILD             := ml-build
SMLNJ_BUILD_FLAGS       := -Ctdp.instrument=true \$$smlnj-tdp/back-trace.cm
SMLNJ_MAKEDEPEND        := ml-makedepend
SMLNJ_MAKEDEPEND_FLAGS  :=
SMLNJ_HEAP_SUFFIX       := $(shell echo 'TextIO.output (TextIO.stdErr, SMLofNJ.SysInfo.getHeapSuffix ());' | sml 2>&1 1> /dev/null)
LANGFC_CM 		:= langfc.cm
LANGFC_SMLNJ_HEAP 	:= ../bin/.smlnj-heap/langfc.$(SMLNJ_HEAP_SUFFIX)

#
# Choose SML compiler
#
SMLC			:=
ifeq ($(SMLC),)
ifeq (mlton, $(shell if mlton >/dev/null 2>&1 || [ $$? != 127 ] ; then echo mlton; fi))
SMLC			:= mlton
else ifeq (sml, $(shell if sml </dev/null >/dev/null 2>&1 || [ $$? != 127 ] ; then echo sml; fi))
SMLC			:= smlnj
else
$(error "No SML compiler found.")
endif
endif

ifeq ($(SMLC),mlton)
DEFAULT			:= $(LANGFC_MLTON_EXEC)
else ifeq ($(SMLC),smlnj)
DEFAULT			:= $(LANGFC_SMLNJ_HEAP)
else
$(error "Invalid SML compiler specified: SMLC=$(SMLC)")
endif

all: $(DEFAULT)

ifeq ($(SMLC),mlton)
$(LANGFC_MLTON_EXEC): $(shell mlton -stop f $(LANGFC_MLB))
	$(MLTON) $(MLTON_FLAGS) -output $(LANGFC_MLTON_EXEC) $(LANGFC_MLB)
	@touch $(LANGFC_MLTON_EXEC)
endif

ifeq ($(SMLC),smlnj)
$(LANGFC_SMLNJ_HEAP): .depend_langfc
	$(SMLNJ_BUILD) $(SMLNJ_BUILD_FLAGS) $(LANGFC_CM) LangFComp.main $(LANGFC_SMLNJ_HEAP)
	@touch $(LANGFC_SMLNJ_HEAP)

# SML/NJ dependency analysis
CM_FILES := $(shell find . -type f -name '*.cm')

.depend_langfc: $(CM_FILES)
	@touch .depend_langfc
	$(SMLNJ_MAKEDEPEND) $(SMLNJ_MAKEDEPEND_FLAGS) -n -f .depend_langfc $(LANGFC_CM) $(LANGFC_SMLNJ_HEAP) || (rm -f .depend_langfc ; exit 1)
sinclude .depend_langfc
endif

.PHONY: clean
clean:
	rm -f $(LANGFC_MLTON_EXEC)
	rm -f $(LANGFC_SMLNJ_HEAP)
	rm -f .depend_langfc
	find . -type d -name '.cm' | xargs rm -rf
