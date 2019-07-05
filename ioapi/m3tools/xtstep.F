
        SUBROUTINE  XTSTEPL( SIZE,  LAYER, LAY1, NLAYS, JDATE, JTIME,
     &                       INAME, VNAMEI, ONAME, VNAMEO )

C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi_tools/./ioapi_tools/src/xtstep.F,v 1.4 2001/01/03 19:58:02 coats Exp $"
C EDSS/Models-3 M3TOOLS.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C***********************************************************************
C  subroutine XTSTEPL body starts at line   94
C  subroutine XTSTEPS head starts at line  130
C  subroutine XTSTEPS body starts at line  173
C
C  FUNCTION:
C       Copy this time step of variable VNAME from INAME to ONAME.
C       
C       XTSTEPL:  file types GRDDED3, BNDARY3, CUSTOM3
C       XTSTEPS:  file type  SMATRX3
C
C  PRECONDITIONS REQUIRED:
C       Valid dates and times JDATE:JTIME
C       f77 MALLOC()-allocation operating environment (such as Sun, SGI)
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       Models-3 I/O:  M3ERR(), READ3(), WRITE3()
C
C  REVISION  HISTORY:
C       Prototype 1/95 by CJC
C       Modified  9/99 by CJC for enhanced portability
C       Modified  1/2000 by CJC:  support for layer sub-ranges
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

        INTEGER         SIZE    ! horiozontal array dimension for this variable
        INTEGER         LAYER   ! layer number to read
        INTEGER         LAY1    ! lowest layer number to write
        INTEGER         NLAYS   ! vertical array dimension
        INTEGER         JDATE   ! current model date
        INTEGER         JTIME   ! current model time
        CHARACTER*16    INAME   !  logical name of the  input file
        CHARACTER*16    ONAME   !  logical name of the output file
        CHARACTER*16    VNAMEI  !  vble name (input)
        CHARACTER*16    VNAMEO  !  vble name (output)


C...........   EXTERNAL FUNCTIONS:

#if ! AUTO_ARRAYS
        INTEGER         MALLOC
#endif    /* ! AUTO_ARRAYS */


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        REAL            GRID( SIZE, NLAYS )

#if ! AUTO_ARRAYS
	INTEGER         LSIZE  / -1 /
	POINTER         ( P, GRID )

	SAVE            P, LSIZE
#endif    /* ! AUTO_ARRAYS */

        CHARACTER*120   MESG

C***********************************************************************
C   begin body of subroutine  XTSTEPL

#if ! AUTO_ARRAYS
	IF ( SIZE .GT. LSIZE ) THEN
	    IF ( LSIZE .GT. 0 )  CALL FREE( P )
	    P = MALLOC( 8 * SIZE * NLAYS )
	    IF ( P .EQ. 0 )
     &           CALL M3EXIT( 'XTSTEPL', JDATE, JTIME,
     &                        'Memory allocation error', 2 )
	    LSIZE = SIZE
	END IF
#endif    /* ! AUTO_ARRAYS */
	
        IF ( READ3( INAME, VNAMEI, LAYER,
     &              JDATE, JTIME, GRID ) ) THEN
           
            IF ( .NOT. WRITE3( ONAME, VNAMEO, 
     &                         JDATE, JTIME, GRID( 1, LAY1 ) ) ) THEN
               
                MESG = 'Write failure:  file ' // ONAME //
     &                 ' variable ' // VNAMEO
                CALL M3EXIT( 'M3XTRACT:XTSTEPL', JDATE,JTIME, MESG, 2 )
               
            END IF              !  if read3() worked, or not
                   
        ELSE                !  read3() failed:
           
            MESG = 'Read failure:  file ' // INAME //
     &                   ' variable ' // VNAMEI
            CALL M3EXIT( 'M3XTRACT:XTSTEPL', JDATE, JTIME, MESG, 2 )
           
        END IF              !  if read3() worked, or not

        RETURN

        END

C***********************************************************************

        SUBROUTINE  XTSTEPS( SIZE, JDATE, JTIME, INAME, ONAME )

        IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'  !  I/O parameter definitions
        INCLUDE 'IODECL3.EXT' !  I/O definitions and declarations


C...........   ARGUMENTS and their descriptions:

        INTEGER         SIZE    ! horiozontal array dimension for this variable
        INTEGER         JDATE   ! current model date
        INTEGER         JTIME   ! current model time
        CHARACTER*16    INAME   !  logical name of the  input file
        CHARACTER*16    ONAME   !  logical name of the output file


C...........   EXTERNAL FUNCTIONS:

#if ! AUTO_ARRAYS
        INTEGER         MALLOC
#endif    /* ! AUTO_ARRAYS */


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         BUFFER( 2*SIZE )

#if ! AUTO_ARRAYS
	INTEGER         LSIZE  / -1 /
	POINTER       ( P, BUFFER )

	SAVE            P, LSIZE
#endif    /* ! AUTO_ARRAYS */

        CHARACTER*120   MESG

C***********************************************************************
C   begin body of subroutine  XTSTEPS

#if ! AUTO_ARRAYS
	IF ( SIZE .GT. LSIZE ) THEN
	    IF ( LSIZE .GT. 0 )  CALL FREE( P )
	    P = MALLOC( 8 * SIZE )
	    IF ( P .EQ. 0 )
     &           CALL M3EXIT( 'XTSTEPS', JDATE, JTIME,
     &                        'Memory allocation error', 2 )
	    LSIZE = SIZE
	END IF
#endif    /* ! AUTO_ARRAYS */
	
        IF ( READ3( INAME, ALLVAR3, ALLAYS3,
     &              JDATE, JTIME, BUFFER ) ) THEN
           
            IF ( .NOT. WRITE3( ONAME, ALLVAR3, 
     &                         JDATE, JTIME, BUFFER ) ) THEN
               
                MESG = 'Write failure:  file ' // ONAME
                CALL M3EXIT( 'M3XTRACT:XTSTEPS', JDATE,JTIME, MESG, 2 )
               
            END IF              !  if read3() worked, or not
                   
        ELSE                !  read3() failed:
           
            MESG = 'Read failure:  file ' // INAME
            CALL M3EXIT( 'M3XTRACT:XTSTEPS', JDATE, JTIME, MESG, 2 )
           
        END IF              !  if read3() worked, or not

        RETURN

        END



