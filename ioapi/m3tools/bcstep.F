
        SUBROUTINE  BCSTEP( NCOLS, NROWS, NLAYS, NVARS,
     &                      LOCOL, HICOL, LOROW, HIROW, NTHIK,
     &                      JDATE, JTIME, INAME, WNAME, LOGDEV )

C***********************************************************************
C Version "@(#)$Header$"
C EDSS/Models-3 M3TOOLS.  Copyright (C) 1992-2002 MCNC
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  102
C
C  FUNCTION:
C       Copy this time step of variable VNAME from INAME to WNAME.
C
C  PRECONDITIONS REQUIRED:
C       Valid dates and times JDATE:JTIME
C       f77 MALLOC()-allocation operating environment (such as Sun, SGI)
C	FDESC3 correctly set-up with VNAME3D
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       Models-3 I/O:  M3EXIT(), READ3(), WRITE3()
C
C  REVISION  HISTORY:
C       Prototype 1/1995 by CJC
C       Modified  9/1999 by CJC for enhanced portability
C       Modified  7/2002 by CJC:  standard-conforming DATA statement fix
C***********************************************************************

      IMPLICIT NONE

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

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'  !  I/O parameter definitions
        INCLUDE 'FDESC3.EXT'  !  file header contents
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
        INTEGER         NTHIK   !  boundary thickness
        INTEGER         JDATE   ! current model date
        INTEGER         JTIME   ! current model time
        CHARACTER*16    INAME   !  logical name of the  input file
        CHARACTER*16    WNAME   !  logical name of the output file
        INTEGER         LOGDEV  ! unit number for output


C...........   EXTERNAL FUNCTIONS:

#if ! AUTO_ARRAYS
        INTEGER         MALLOC
#endif    /* AUTO_ARRAYS */


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         I, C, R, L, V, T
        REAL            GRID( NCOLS, NROWS, NLAYS )
        REAL            BNDY( 2*NTHIK*( NCOLS3D + NROWS3D + 2*NTHIK ) ,
     &                        NLAYS )

        CHARACTER*256   MESG

#if ! AUTO_ARRAYS
        INTEGER         SIZE   ! horiz input  grid dimension total
	INTEGER         LSIZE
	DATA            LSIZE  / -1 /
	POINTER         ( P, GRID )
	POINTER         ( Q, BNDY )

	SAVE            P, Q, LSIZE
#endif    /* AUTO_ARRAYS */


C***********************************************************************
C   begin body of subroutine  BCSTEP

#if ! AUTO_ARRAYS
	SIZE = 4 * NLAYS * NCOLS * NROWS

        IF ( SIZE .GT. LSIZE ) THEN        

	    IF ( LSIZE .GT. 0 ) THEN
                CALL FREE( P )
                CALL FREE( Q )
            END IF

	    P = MALLOC( SIZE )
	    IF ( P .EQ. 0 )  THEN
                CALL M3EXIT( 'BCWNDW:BCSTEP', JDATE, JTIME,
     &                       'Memory allocation error', 3 )
            END IF

	    T = 8 * NLAYS * NTHIK * ( NCOLS3D + NROWS3D + 2 * NTHIK )
            Q = MALLOC( T )
	    IF ( Q .EQ. 0 )  THEN
                CALL M3EXIT( 'BCWNDW:BCSTEP', JDATE, JTIME,
     &                       'Memory allocation error', 3 )
            END IF

	    LSIZE = SIZE

	END IF					!  if isize > lsize
#endif    /* ! AUTO_ARRAYS */

        DO  101  V = 1, NVARS
        
            IF ( .NOT. READ3( INAME, VNAME3D( V ), ALLAYS3,
     &                        JDATE, JTIME, GRID ) ) THEN
                MESG = 'Read failure:  file ' // INAME
                CALL M3EXIT( 'BCWNDW:BCSTEP', JDATE, JTIME, MESG, 2 )
            END IF              !  if read3() worked, or not


C.......   Extract the boundary from this subgrid:

            DO  99  L = 1, NLAYS

               I = 0				!  perimeter subscript for this layer

                DO  22  R = LOROW - NTHIK, LOROW - 1		!  south bdy component
                DO  11  C = LOCOL, HICOL + NTHIK
                    I = I + 1
                    BNDY( I,L ) = GRID( C,R,L )
11              CONTINUE
22              CONTINUE

                DO  44  R = LOROW, HIROW + NTHIK		!  east bdy
                DO  33  C = HICOL + 1, HICOL + NTHIK
                    I = I + 1
                    BNDY( I,L ) = GRID( C,R,L )
33              CONTINUE
44              CONTINUE

                DO  66  R = HIROW + 1, HIROW + NTHIK		!  north
                DO  55  C = LOCOL - NTHIK, HICOL
                    I = I + 1
                    BNDY( I,L ) = GRID( C,R,L )
55              CONTINUE
66              CONTINUE

                DO  88  R = LOROW - NTHIK, HIROW		!  west
                DO  77  C = LOCOL - NTHIK, LOCOL - 1
                    I = I + 1
                    BNDY( I,L ) = GRID( C,R,L )
77              CONTINUE
88              CONTINUE

99          CONTINUE	!  end loop on layers

            IF ( .NOT. WRITE3( WNAME, VNAME3D( V ), 
     &                         JDATE, JTIME, BNDY ) ) THEN
                MESG = 'Write failure:  file ' // WNAME
                CALL M3EXIT( 'BCWNDW:BCSTEP', JDATE, JTIME, MESG, 2 )
            END IF              !  if read3() worked, or not

101     CONTINUE	!  end loop on variables

        RETURN

        END

