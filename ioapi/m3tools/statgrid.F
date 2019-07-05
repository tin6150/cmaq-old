
        SUBROUTINE  STATGRID ( NCOLS, NROWS, NLAYS, NVARS,
     &                         JDATE, JTIME, NTHRES, THRES,
     &                         INNAME, VNAMES, VTYPES, LOGDEV )

C***********************************************************************
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi_tools/./ioapi_tools/src/statgrid.F,v 1.2 2000/12/14 22:55:32 smith_w Exp $"
C EDSS/Models-3 M3TOOLS.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  102
C
C  FUNCTION:
C       Statistics report to LOGDEV on variables VNAMES  from file
C       INNAME.
C       and on the results of using GRIDOPS to apply the operations
C       OPNAME( * ) to them.
C
C  PRECONDITIONS REQUIRED:
C       Valid dates and times JDATE:JTIME
C       Stack-allocation operating environment (such as CRAY)
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       Models-3 I/O:  M3ERR(), READ3(), WRITE3()
C
C  REVISION  HISTORY:
C       Prototype 3/93 by CJC
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

        INTEGER         NCOLS   ! grid dimensions, from INNAME header
        INTEGER         NROWS   ! grid dimensions, from INNAME header
        INTEGER         NLAYS   ! grid dimensions, from INNAME header
        INTEGER         NVARS   !  number of vbles to be totaled
        INTEGER         JDATE   ! current model date
        INTEGER         JTIME   ! current model time
        INTEGER         NTHRES( NVARS )		! number of tests per vble
        REAL		THRES ( 10,NVARS )	! thresholds for counting
        CHARACTER*16    INNAME                  !  input file logical name
        CHARACTER*16    VNAMES( NVARS )         !  list of vble names
        INTEGER         VTYPES( NVARS )		!  list of vble types
        INTEGER         LOGDEV  ! unit number for output


C...........   EXTERNAL FUNCTIONS:

        CHARACTER*24    DT2STR  !  formats JDATE:JTIME as Mmm DD, YYYY etc.
        INTEGER         TRIMLEN
        EXTERNAL        DT2STR, TRIMLEN

#if ! AUTO_ARRAYS
        INTEGER         MALLOC
#endif    /* ! AUTO_ARRAYS */

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        REAL             GRID( NCOLS, NROWS, NLAYS )
        INTEGER          IGRD( NCOLS, NROWS, NLAYS )
        DOUBLE PRECISION DGRD( NCOLS, NROWS, NLAYS )
        INTEGER          V, SIZE
        CHARACTER*24     DTBUF
        CHARACTER*120    MESG

#if ! AUTO_ARRAYS
	INTEGER          LSIZE  / -1 /
	POINTER          ( P, GRID )
	POINTER          ( Q, IGRD )
	POINTER          ( R, DGRD )

	SAVE     P, Q, R, LSIZE
#endif    /* ! AUTO_ARRAYS */


C***********************************************************************
C   begin body of subroutine  STATGRID

#if ! AUTO_ARRAYS
	SIZE   = NCOLS * NROWS * NLAYS
	IF ( SIZE .GT. LSIZE ) THEN
	    IF ( LSIZE .GT. 0 )  CALL FREE( P )
	    P = MALLOC( 16 * SIZE )
	    IF ( P .EQ. 0 )
     &           CALL M3EXIT( 'STATGRID', JDATE, JTIME,
     &                        'Memory allocation error', 2 )
	    Q = P + 4 * SIZE
            R = P + 8 * SIZE
            LSIZE = SIZE
	END IF
#endif    /* ! AUTO_ARRAYS */

        IF ( JDATE .NE. 0 .OR. JTIME .NE. 0 ) THEN
            DTBUF = DT2STR( JDATE, JTIME )
            WRITE( LOGDEV,92010 )
     &          INNAME, JDATE, JTIME, DTBUF( 1:TRIMLEN( DTBUF ) )
        ELSE
            WRITE( LOGDEV,92010 ) INNAME
        END IF

        DO  111  V = 1, NVARS

            IF ( VTYPES( V ) .EQ. M3REAL ) THEN

                IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3,
     &                            JDATE, JTIME, GRID ) ) THEN

                    MESG = 'Read failure:  file ' // INNAME //
     &                     ' variable ' // VNAMES( V )
                    CALL M3EXIT( 'M3STAT:STATGRID', JDATE, JTIME, 
     &                           MESG, 2 )
                    GO TO 111

                END IF              !  if read3() worked, or not

            ELSE IF ( VTYPES( V ) .EQ. M3INT ) THEN

                IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3,
     &                            JDATE, JTIME, IGRD ) ) THEN

                    MESG = 'Read failure:  file ' // INNAME //
     &                     ' variable ' // VNAMES( V )
                    CALL M3EXIT( 'M3STAT:STATGRID', JDATE, JTIME, 
     &                           MESG, 2 )
                    GO TO 111

                END IF              !  if read3() worked, or not

                CALL INTG2REAL( SIZE, IGRD, GRID )

            ELSE IF ( VTYPES( V ) .EQ. M3DBLE ) THEN

                IF ( .NOT. READ3( INNAME, VNAMES( V ), ALLAYS3,
     &                            JDATE, JTIME, DGRD ) ) THEN

                    MESG = 'Read failure:  file ' // INNAME //
     &                     ' variable ' // VNAMES( V )
                    CALL M3EXIT( 'M3STAT:STATGRID', JDATE, JTIME, 
     &                           MESG, 2 )
                    GO TO 111

                END IF              !  if read3() worked, or not

                CALL DBLE2REAL( SIZE, DGRD, GRID )

            ELSE

                MESG = 'Unknown data type for variable ' // VNAMES( V )
                CALL M3EXIT( 'M3STAT:STATGRID', JDATE, JTIME, MESG, 2 )
                GO TO 111

            END IF

            CALL STATG( NCOLS, NROWS, NLAYS, GRID,
     &                  NTHRES( V ), THRES( 1,V ),
     &                  VNAMES( V ), LOGDEV )

111     CONTINUE        !  end loop on variables

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Informational (LOG) message formats... 92xxx

92010   FORMAT ( //5X, 'File:  ', A, :,
     &            /5X, 'Date and time:', I9.7, ':', I6.6, 
     &             2X, '(', A, ')' )

        END

