
        SUBROUTINE  TSHIFT ( SIZE,  JDATE, JTIME, KDATE, KTIME,
     &                       INAME, ONAME, LOGDEV )

C***********************************************************************
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi_tools/./ioapi_tools/src/tshift.F,v 1.2 2000/12/14 22:55:33 smith_w Exp $"
C EDSS/Models-3 M3TOOLS.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  88
C
C  FUNCTION:
C       Copy this JDATE:JTIME from INAME to KDATE:KTIME in ONAME.
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

        INTEGER         SIZE    ! total array dimensions for this timestep
        INTEGER         JDATE   ! source date
        INTEGER         JTIME   ! source time
        INTEGER         KDATE   ! target date
        INTEGER         KTIME   ! target time
        CHARACTER*16    INAME   ! logical name of the  input file
        CHARACTER*16    ONAME   ! logical name of the output file
        INTEGER         LOGDEV  ! unit number for output


C...........   EXTERNAL FUNCTIONS:

        INTEGER         TRIMLEN
        EXTERNAL        TRIMLEN

#if ! AUTO_ARRAYS || __hpux
        INTEGER         MALLOC
#endif    /* ! AUTO_ARRAYS || __hpux*/


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        REAL            GRID( SIZE )

#if ! AUTO_ARRAYS || __hpux
	INTEGER         LSIZE  / -1 /
	POINTER         ( P, GRID )

	SAVE            P, LSIZE
#endif    /* ! AUTO_ARRAYS */

        CHARACTER*256   MESG    ! buffer for M3EXIT()

C***********************************************************************
C   begin body of subroutine  TSHIFT

#if ! AUTO_ARRAYS || __hpux
	IF ( SIZE .GT. LSIZE ) THEN
	    IF ( LSIZE .GT. 0 )  CALL FREE( P )
	    P = MALLOC( 8 * SIZE )
	    IF ( P .EQ. 0 )
     &           CALL M3EXIT( 'TSHIFT', JDATE, JTIME,
     &                       'Memory allocation error', 2 )
	    LSIZE = SIZE
	END IF                                                
#endif    /* ! AUTO_ARRAYS || __hpux */
	
        IF ( READ3( INAME, ALLVAR3, ALLAYS3,
     &              JDATE, JTIME, GRID ) ) THEN
           
            IF ( .NOT. WRITE3( ONAME, ALLVAR3, 
     &                         KDATE, KTIME, GRID ) ) THEN
               
                MESG = 'Write failure:  file "' //
     &                 ONAME( 1:TRIMLEN( ONAME ) )
                CALL M3WARN( 'M3TSHIFT:TSHIFT', KDATE, KTIME, MESG )
               
            END IF              !  if read3() worked, or not
                   
        ELSE                !  read3() failed:
           
            MESG = 'Read failure:  file "' //
     &             INAME( 1:TRIMLEN( INAME ) ) // '"'
            CALL M3WARN( 'M3TSHIFT:TSHIFT', JDATE, JTIME, MESG )
           
        END IF              !  if read3() worked, or not

        RETURN

        END

