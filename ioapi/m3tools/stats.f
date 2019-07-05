
        SUBROUTINE  STATS( NCOLS, NROWS, NLAYS, GRID,
     &                     NEPS, EPS, LABEL, LOGDEV )

C***********************************************************************
C Version "@(#)$Header$"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2002-2007 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  65
C
C  FUNCTION:
C       Produce statistics report to LOGDEV
C
C  PRECONDITIONS REQUIRED:
C	Stack-allocation operating environment (such as CRAY)
C	number of columns, rows, and levels at most 99
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       Models-3 I/O:  M3ERR(), READ3(), WRITE3()
C
C  REVISION  HISTORY:
C       Prototype 3/1993 by CJC
C
C       Version 3/2007 by CJC: REAL*8 accumulators
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER         NCOLS   !  grid dimensions, from INNAME header
        INTEGER         NROWS   !  grid dimensions, from INNAME header
        INTEGER         NLAYS   !  grid dimensions, from INNAME header
        REAL		GRID( NCOLS, NROWS, NLAYS )	!  the grid.
        INTEGER         LOGDEV  !  unit number for stats report
        INTEGER		NEPS	!  number of thresholds
        REAL		EPS( * )!  thresholds for threshold-fraction reports
        CHARACTER*(*)   LABEL   !  legend text


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         C, R, L, V      !  col, row, level, variable, counters
        INTEGER         MC, MR, ML      !  indexes for maximum
        INTEGER         NC, NR, NL      !  indexes for minimum
        REAL            T
        REAL            AMAX
        REAL            AMIN
        REAL*8          ASUM
        REAL*8          ASSQ
        REAL*8          DNOM
        INTEGER		ECNT


C***********************************************************************
C   begin body of subroutine  STATS

C...........   Construct 3-D GRID stats: max, min and their locations,
C...........   mean, and sigma

        MC   = 1
        MR   = 1
        ML   = 1
        NC   = 1
        NR   = 1
        NL   = 1
        T    = GRID( 1,1,1 )
        AMAX = T
        AMIN = T
        ASUM = 0.0
        ASSQ = 0.0

        DO  133  L = 1, NLAYS   !  3-D traversal:  all other layers
        DO  122  R = 1, NROWS
        DO  111  C = 1, NCOLS
            T    = GRID( C,R,L )
            ASUM = ASUM + T
            ASSQ = ASSQ + T*T
            IF ( T .GT. AMAX ) THEN
                AMAX = T
                MC   = C
                MR   = R
                ML   = L
            ELSE IF ( T .LT. AMIN ) THEN
                AMIN = T
                NC   = C
                NR   = R
                NL   = L
            END IF
111     CONTINUE
122     CONTINUE
133     CONTINUE

        DNOM = 1.0 / DBLE( NCOLS * NROWS * NLAYS )
        ASUM = DNOM * ASUM
        ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0d0 ) )
        WRITE( LOGDEV,92010 )
     &      LABEL, ' 3-D grid statistics' ,
     &      'Max   ', AMAX, ' @(c,r,l)=(', MC, MR, ML, ')',
     &      'Min   ', AMIN, ' @(c,r,l)=(', NC, NR, NL, ')',
     &      'Mean  ', ASUM,
     &      'Sigma ', ASSQ


C...........   For each threshold level, count the number of times the
C...........   grid value exceeds the threshold, and report it:

        DO  199  V = 1, NEPS	!  count threshold excesses:
            ECNT = 0
            T    = EPS( V )
            DO  188  L = 1, NLAYS
            DO  177  R = 1, NROWS
            DO  166  C = 1, NCOLS
                IF ( GRID( C,R,L ) .GE. T )  ECNT = ECNT + 1
166         CONTINUE
177         CONTINUE
188         CONTINUE
            WRITE( LOGDEV,92020 ) T, ECNT, DNOM * FLOAT( ECNT )
199     CONTINUE

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Informational (LOG) message formats... 92xxx

92010   FORMAT ( /5X , 'Variable:  ', A, A,
     &           2( /9X, A, 1PE12.5, A, I2, ',', I2, ',', I2, A ),
     &           2( /9X, A, 1PE12.5 ) )

92020   FORMAT ( 9X , 'Number of times ', 1PE12.5,
     &           2X, 'exceeded:', I8,
     &           2X, 'fraction:', F10.8 )
        END

