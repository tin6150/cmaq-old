#
#.........................................................................
# Version "@(#)$Header$"
# EDSS/Models-3 IOTESTS
# Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
# (C) 2003-2004 Baron Advanced Meteorological Systems, LLC.
# Distributed under the GNU GENERAL PUBLIC LICENSE version 2
# See file "GPL.txt" for conditions of use.
#.........................................................................
#
#    ASSUMPTION:  $(FC) is an F90.
#
#       ---------------     Definitions:   -------------------------

.SUFFIXES: .m4 .c .F .f
#
BASEDIR = ${HOME}/web31
SRCDIR  = $(BASEDIR)/iotests
IODIR   = $(BASEDIR)/ioapi
OBJDIR  = $(BASEDIR)/$(BIN)

# Architecture dependent stuff

include ${IODIR}/Makeinclude.$(BIN)

include $(PVM_ROOT)/conf/$(PVM_ARCH).def

CFLAGS = -I${IODIR} -I$(PVM_ROOT)/include $(ARCHFLAGS) $(PARFLAGS) $(COPTFLAGS)

FFLAGS = -I${IODIR} -I. -DIOAPICPL $(FOPTFLAGS) $(ARCHFLAGS) $(PARFLAGS)

LFLAGS = -I${IODIR} -DIOAPICPL $(ARCHFLAGS) $(PARFLAGS)

#  Incompatibility between netCDF versions before / after v4.1.1:
#  For netCDF v4 and later, you may also need the extra libraries
#  given by netCDF command
#
#          nc-config --libs
#
#LIBS = -L${OBJDIR} -lioapi -lnetcdf  $(PVMLIBS) $(OMPLIBS) $(ARCHLIB) $(ARCHLIBS)

LIBS =  -L${OBJDIR} -lioapi -lnetcdff- -lnetcdf $(PVMLIBS) $(OMPLIBS) $(ARCHLIB) $(ARCHLIBS)

VPATH = ${OBJDIR}


FSRC = \
cnttest.f     cpltest.f     initqux.f     interp_test.f \
inqtest.f     m3interp.f    readprofile.f setenv_test.f \
sortictest.f  testqux.f     writeprofile.f

CSRC = int4.c testread.c

OBJ = $(FSRC:.f=.o)$(CSRC:.c=.o)

ESRC= ${IODIR}/CONST3.EXT  ${IODIR}/FDESC3.EXT  \
      ${IODIR}/IODECL3.EXT ${IODIR}/NETCDF.EXT  \
      ${IODIR}/PARMS3.EXT

QLIB = libqux.a

EXE = \
cnttest       cpltest       int4          interp_test \
readprofile   setenv_test   sortictest \
testqux       writeprofile  testread

all:  $(EXE)

clean:
	rm $(OBJ) $(QLIB) $(EXE)

rmexe:
	rm $(EXE)


#  ---------------------------  RULES:  --------------------------

%.o : %.mod        #  Disable "gmake"s obnoxious implicit Modula-2 rule !!
%.f : %.F          #  Hack for some versions of  "gmake" + "gfortran"

.f.o:  $(ESRC)
	$(FC) $(FFLAGS) -c $<

.F.o:  $(ESRC)
	$(FC) $(FFLAGS) -c $<

.c.o:
	$(CC) $(CFLAGS) -c $<


#  ---------------------------  $(EXE) Program builds:  -----------------

cnttest:  cnttest.o
	$(FC) ${LFLAGS} $^ ${LIBS} -o $@

cpltest:  cpltest.o
	$(FC) ${LFLAGS} $^ ${LIBS} -o $@

int4:  int4.o
	$(CC) ${LFLAGS} $^ ${LIBS} -o $@

interp_test:  interp_test.o
	$(FC) ${LFLAGS} $^ ${LIBS} -o $@

inqtest:  inqtest.o
	$(FC) ${LFLAGS} $^ ${LIBS} -o $@

readprofile:  readprofile.o
	$(FC) ${LFLAGS} $^ ${LIBS} -o $@

setenv_test:  setenv_test.o
	$(FC) ${LFLAGS} $^ ${LIBS} -o $@

sortictest:  sortictest.o
	$(FC) ${LFLAGS} $^ ${LIBS} -o $@

testqux:  testqux.o $(QLIB)
	$(FC) ${LFLAGS} testqux.o -L. -lqux -o $@

testread:  testread.o
	$(FC) ${LFLAGS} $^ ${LIBS} -o $@

writeprofile:  writeprofile.o
	$(FC) ${LFLAGS} $^ ${LIBS} -o $@

$(QLIB):  initqux.o
	ar -cr $(QLIB) $^

