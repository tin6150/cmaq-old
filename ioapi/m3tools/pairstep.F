
        SUBROUTINE  PAIRSTEP ( NCOLS,  NROWS,  NLAYS,
     &                         CLO, CHI, RLO, RHI, LLO, LHI,
     &                         JDATEA, JTIMEA, JDATEB, JTIMEB,
     &                         NAMEA,  NAMEB,  
     &                         WNAMES, WTYPES, RDEV )

C***********************************************************************
C EDSS/Models-3 M3TOOLS.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  123
C
C  FUNCTION:
C       Statistics report to RDEV on variables WNAMES( 1,* ) and
C       WNAMES( 2,* ) from files NAMEA and NAMEB, respectively,
C       and on the results of using GRIDOPS to apply the operations
C       OPNAME( * ) to them.
C
C  PRECONDITIONS REQUIRED:
C       Valid dates and times JDATE:JTIME
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       Models-3 I/O:  M3ERR(), READ3()
C       Utility routines:  DT2STR(), TRIMLEN()
C
C  REVISION  HISTORY:
C       Prototype 3/93 by CJC
C	Version  11/94 by CJC for new version of I/O API
C       Modified  9/99 by CJC for enhanced portability
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
        INCLUDE 'IODECL3.EXT' !  I/O definitions and declarations


C...........   ARGUMENTS and their descriptions:

        INTEGER         NCOLS   ! grid dimensions, from file header
        INTEGER         NROWS   ! grid dimensions, from file header
        INTEGER         NLAYS   ! grid dimensions, from file header
        INTEGER         CLO     ! bottom of col-range
        INTEGER         CHI     ! top of    col-range
        INTEGER         RLO     ! bottom of row-range
        INTEGER         RHI     ! top of    row-range
        INTEGER         LLO     ! bottom of layer-range
        INTEGER         LHI     ! top of    layer-range
        INTEGER         JDATEA  ! current model date, file A
        INTEGER         JTIMEA  ! current model time, file A
        INTEGER         JDATEB  ! current model date, file B
        INTEGER         JTIMEB  ! current model time, file B
        CHARACTER*16    NAMEA   !  logical name of the input file
        CHARACTER*16    NAMEB   !  logical name of the input file
        CHARACTER*16    WNAMES( 2 ) !  list of vble names
        INTEGER         WTYPES( 2 ) !  list of vble types
        INTEGER         RDEV    ! unit number for output


C...........   EXTERNAL FUNCTIONS:

        CHARACTER*24    DT2STR  !  formats JDATE:JTIME as Mmm DD, YYYY etc.
        INTEGER         TRIMLEN
#if ! AUTO_ARRAYS
        INTEGER         MALLOC
#endif    /* ! AUTO_ARRAYS */

        EXTERNAL        DT2STR, TRIMLEN


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        REAL             GRID1( NCOLS, NROWS, NLAYS )
        REAL             GRID2( NCOLS, NROWS, NLAYS )
        DOUBLE PRECISION DBLE1( NCOLS, NROWS, NLAYS )
        DOUBLE PRECISION DBLE2( NCOLS, NROWS, NLAYS )
        INTEGER          INTG1( NCOLS, NROWS, NLAYS )
        INTEGER          INTG2( NCOLS, NROWS, NLAYS )

#if ! AUTO_ARRAYS
	POINTER       ( P1, GRID1 )
	POINTER       ( P2, GRID2 )
	POINTER       ( P4, DBLE1 )
	POINTER       ( P5, DBLE2 )
	POINTER       ( P6, INTG1 )
	POINTER       ( P7, INTG2 )
#endif    /* ! AUTO_ARRAYS */

        LOGICAL         FLAG1, FLAG2

        INTEGER         C, R, L      !  col, row, level, variable, counters
	CHARACTER*256   MESG

	INTEGER         SIZE

#if ! AUTO_ARRAYS
	INTEGER         LSIZE / -1 /

	SAVE    P1, P2, P4, P5, P6, P7, LSIZE
#endif    /* ! AUTO_ARRAYS */


C***********************************************************************
C   begin body of subroutine  PAIRSTEP

	SIZE = NCOLS * NROWS * NLAYS

#if ! AUTO_ARRAYS
	IF ( SIZE .GT. LSIZE ) THEN
	    IF ( SIZE .GT. 0 )  CALL FREE( P1 )
	    P1 = MALLOC( 36 * SIZE )
	    IF( P1 .EQ. 0 ) 
     &          CALL M3EXIT( 'PAIRSTEP', JDATEA, JTIMEA,
     &                       'Memory allocation error.', 2 )
	    P2 = P1  +   4 * SIZE
	    P4 = P1  +  12 * SIZE
	    P5 = P1  +  20 * SIZE
	    P6 = P1  +  28 * SIZE
	    P7 = P1  +  32 * SIZE
	    LSIZE = SIZE
	END IF
#endif    /* ! AUTO_ARRAYS */
        

        IF ( WTYPES( 1 ) .EQ. M3REAL ) THEN

            FLAG1 = READ3( NAMEA,  WNAMES( 1 ), ALLAYS3,
     &                     JDATEA, JTIMEA, GRID1 )

        ELSE IF ( WTYPES( 1 ) .EQ. M3DBLE ) THEN

            FLAG1 = READ3( NAMEA,  WNAMES( 1 ), ALLAYS3,
     &                     JDATEA, JTIMEA, DBLE1 )
            CALL DBLE2REAL( SIZE, DBLE1, GRID1 )

        ELSE IF ( WTYPES( 1 ) .EQ. M3INT ) THEN

            FLAG1 = READ3( NAMEA,  WNAMES( 1 ), ALLAYS3,
     &                     JDATEA, JTIMEA, INTG1 )
            CALL INTG2REAL( SIZE, INTG1, GRID1 )

        ELSE

            FLAG1 = .FALSE.

        END IF

        IF ( WTYPES( 2 ) .EQ. M3REAL ) THEN

            FLAG2 = READ3( NAMEB,  WNAMES( 2 ), ALLAYS3,
     &                     JDATEA, JTIMEA, GRID2 )

        ELSE IF ( WTYPES( 2 ) .EQ. M3DBLE ) THEN

            FLAG2 = READ3( NAMEB,  WNAMES( 2 ), ALLAYS3,
     &                     JDATEA, JTIMEA, DBLE2 )
            CALL DBLE2REAL( SIZE, DBLE2, GRID2 )

        ELSE IF ( WTYPES( 2 ) .EQ. M3INT ) THEN

            FLAG2 = READ3( NAMEB,  WNAMES( 2 ), ALLAYS3,
     &                     JDATEA, JTIMEA, INTG2 )
            CALL INTG2REAL( SIZE, INTG2, GRID2 )

        ELSE

            FLAG2 = .FALSE.

        END IF

        IF ( .NOT. FLAG1 ) THEN

            MESG = 'Read failure:  file ' // NAMEA //
     &             ' variable ' // WNAMES( 1 )
            CALL M3WARN( 'M3PAIR:PAIRSTEP', JDATEA, JTIMEA, MESG )

        ELSE IF ( .NOT. FLAG2 ) THEN

            MESG = 'Read failure:  file ' // NAMEB //
     &             ' variable ' // WNAMES( 2 )
            CALL M3WARN( 'M3PAIR:PAIRSTEP', JDATEB, JTIMEB, MESG )

        ELSE            !  both reads OK

            DO  L = LLO, LHI   !  3-D traversal:  selected layers
            DO  R = RLO, RHI
            DO  C = CLO, CHI
                WRITE( RDEV, 93010 ) GRID1( C,R,L ), GRID2( C,R,L )
            END DO
            END DO
            END DO

        END IF          !  if !flag1, else if !flag2, else...

        RETURN

C******************  FORMAT  STATEMENTS   ******************************
C...........   Formatted-file formats... 93xxx

93010   FORMAT ( 1X, 1PE15.7, ',', 1X, 1Pe15.7 )

        END

