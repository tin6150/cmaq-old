#
#.........................................................................
# Version "@(#)$Header$"
# Models-3 *not*CDF interface, to satisfy linker references to netCDF
# in environments for which calls to "libnetcdf.a" are forbidden.
# Copyright (C) 2003 Baron Advanced Meteorological Systems, LLC.
# Distributed under the GNU GENERAL LESSER PUBLIC LICENSE version 2.1
# See file "LGPL.txt" for conditions of use.
#.........................................................................
#
#       -------------------     Definitions:   -------------------------
#
.SUFFIXES: .m4 .c .F .f
#
BASEDIR = ${HOME}/web31
SRCDIR  = $(BASEDIR)/notcdf
IODIR   = $(BASEDIR)/ioapi
OBJDIR  = $(BASEDIR)/$(BIN)
INSTDIR = $(INSTALL)/$(BIN)

# Architecture dependent stuff
# Assumes FC is an f90 (otherwise, "make f77").

include $(IODIR)/Makeinclude.$(BIN)

VPATH = ${OBJDIR}

FFLAGS = -I$(IODIR) $(DEFINEFLAGS) $(FOPTFLAGS) $(ARCHFLAGS)

fSRC = notcdf.f

OBJ = $(fSRC:.f=.o)

LIB = libnotcdf.a

MESG1 = 'To use the "notCDF" interface, you must *MANUALLY* copy'
MESG2 = '"libnotcdf.a" to "libnetcdf.a" in directory '$(BASEDIR)/$(BIN)

#       -------------------     Targets:   -----------------------------

all: $(LIB)

clean:  $(OBJDIR)
	cd $(SRCDIR); rm *.o *.a core* ; cd $(OBJDIR); rm $(OBJ) $(LIB)

install: $(INSTDIR)
	echo "Installing in $(INSTDIR)" ; cd $(OBJDIR); cp $(LIB) $(INSTDIR)

bins:
	make BIN=Linux2_x86_64
	make BIN=Linux2_x86_64g95
	make BIN=Linux2_x86_64sun
	make BIN=Linux2_x86_64ifort
	make BIN=Linux2_x86_64g95dbg
	make BIN=Linux2_x86_64sundbg
	make BIN=Linux2_x86_64ifortdbg

binclean:
	make BIN=Linux2_x86_64          clean
	make BIN=Linux2_x86_64g95       clean
	make BIN=Linux2_x86_64sun       clean
	make BIN=Linux2_x86_64ifort     clean
	make BIN=Linux2_x86_64g95dbg    clean
	make BIN=Linux2_x86_64sundbg    clean
	make BIN=Linux2_x86_64ifortdbg  clean


#  ---------------------------  Dependencies:  --------------------------

.f.o:
	cd $(OBJDIR); $(FC) -c $(FFLAGS) $(SRCDIR)/$<

$(LIB): $(OBJ)
	cd $(OBJDIR); $(AR) $(ARFLAGS) $(LIB) $(OBJ); ranlib ${LIB} ; echo $(MESG1); echo $(MESG2); echo ''



