
        SUBROUTINE  WNDWSTEP( NCOLS, NROWS, NLAYS, NVARS,
     &                        LOCOL, HICOL, LOROW, HIROW,
     &                        JDATE, JTIME,
     &                        INAME, WNAME, LOGDEV )

C***********************************************************************
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi_tools/./ioapi_tools/src/wndwstep.F,v 1.2 2000/12/14 22:55:34 smith_w Exp $"
C EDSS/Models-3 M3TOOLS.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  94
C
C  FUNCTION:
C       Copy this time step of variable VNAME from INAME to WNAME.
C
C  PRECONDITIONS REQUIRED:
C       Valid dates and times JDATE:JTIME
C       f77 MALLOC()-allocation operating environment (such as Sun, SGI)
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       Models-3 I/O:  M3EXIT(), READ3(), WRITE3()
C
C  REVISION  HISTORY:
C       Prototype 1/95 by CJC
C       Modified  9/99 by CJC for enhanced portability
C
C***********************************************************************

#ifndef AUTO_ARRAYS
#if __sgi  || __sun || __osf__ || __mips__
#define AUTO_ARRAYS 0
#endif
#if _CRAY || _AIX
#define AUTO_ARRAYS 1
#endif
#endif

#ifndef AUTO_ARRAYS
#include   "--Error compiling:  unsupported architecture---"
#endif

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'  !  I/O parameter definitions
        INCLUDE 'IODECL3.EXT' !  I/O definitions and declarations


C...........   ARGUMENTS and their descriptions:

        INTEGER         NCOLS   ! input horiz grid dimension
        INTEGER         NROWS   ! input horiz grid dimension 
        INTEGER         NLAYS   ! vertical dimension 
        INTEGER         NVARS   ! vbles    dimension 
        INTEGER         LOCOL   !  left   window boundary 
        INTEGER         HICOL   !  right  window boundary 
        INTEGER         LOROW   !  bottom window boundary 
        INTEGER         HIROW   !  top    window boundary 
        INTEGER         JDATE   ! current model date
        INTEGER         JTIME   ! current model time
        CHARACTER*16    INAME   !  logical name of the  input file
        CHARACTER*16    WNAME   !  logical name of the output file
        INTEGER         LOGDEV  ! unit number for output


C...........   EXTERNAL FUNCTIONS:

#if ! AUTO_ARRAYS
        INTEGER         MALLOC
#endif    /* ! AUTO_ARRAYS */


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         SIZE   ! horiz input  grid dimension total
        REAL            GRID( HICOL - LOCOL + 1 ,
     &                        HIROW - LOROW + 1 , NLAYS , NVARS )

#if ! AUTO_ARRAYS
	INTEGER         LSIZE  / -1 /
	POINTER         ( P, GRID )

	SAVE            P, LSIZE
#endif    /* ! AUTO_ARRAYS */


C***********************************************************************
C   begin body of subroutine  WNDWSTEP

#if ! AUTO_ARRAYS
	SIZE = 4 * NLAYS * NVARS * 
     &          ( HICOL - LOCOL + 1 ) * ( HIROW - LOROW + 1 )

        IF ( SIZE .GT. LSIZE ) THEN        

	    IF ( LSIZE .GT. 0 ) CALL FREE( P )

	    P = MALLOC( SIZE )
	    IF ( P .EQ. 0 )  THEN
                 CALL M3EXIT( 'M3WNDW:WNDWSTEP', JDATE, JTIME,
     &                        'Memory allocation error', 3 )
            END IF

	    LSIZE = SIZE

	END IF		!  if isize > lsize
#endif    /* ! AUTO_ARRAYS */

        IF ( .NOT. XTRACT3( INAME, ALLVAR3, 1, NLAYS, 
     &                      LOROW, HIROW, LOCOL, HICOL,
     &                      JDATE, JTIME, GRID ) ) THEN

            CALL M3EXIT( 'M3WNDW:WNDWSTEP', JDATE, JTIME,
     &                   'Read failure:  file ' // INAME, 2 )

        END IF              !  if read3() worked, or not

        IF ( .NOT. WRITE3( WNAME, ALLVAR3, 
     &                     JDATE, JTIME, GRID ) ) THEN

            CALL M3EXIT( 'M3WNDW:WNDWSTEP', JDATE, JTIME,
     &                   'Write failure:  file ' // WNAME, 2 )

        END IF              !  if read3() worked, or not

        RETURN

        END

