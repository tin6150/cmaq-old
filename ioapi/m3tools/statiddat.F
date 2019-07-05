
        SUBROUTINE  STATIDDAT ( NROWS, NLAYS, NVARS,
     &                         JDATE, JTIME, NTHRES, THRES,
     &                         INNAME, VNAMES, VTYPES, LOGDEV )

C***********************************************************************
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi_tools/./ioapi_tools/src/statiddat.F,v 1.3 2001/01/03 19:57:59 coats Exp $"
C EDSS/Models-3 M3TOOLS.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  100
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

        INTEGER         NROWS   ! grid dimensions, from INNAME header
        INTEGER         NLAYS   ! grid dimensions, from INNAME header
        INTEGER         NVARS   !  number of vbles to be totaled
        INTEGER         JDATE   ! current model date
        INTEGER         JTIME   ! current model time
        INTEGER         NTHRES( NVARS )         ! number of tests per vble
        REAL            THRES ( 10,NVARS )      ! thresholds for counting
        CHARACTER*16    INNAME                  !  input file logical name
        CHARACTER*16    VNAMES( NVARS )         !  list of vble names
        INTEGER         VTYPES( NVARS )		! number of tests per vble
        INTEGER         LOGDEV  ! unit number for output


C...........   EXTERNAL FUNCTIONS:

        CHARACTER*24    DT2STR  !  formats JDATE:JTIME as Mmm DD, YYYY etc.
        EXTERNAL        DT2STR

#if ! AUTO_ARRAYS
        INTEGER         MALLOC
#endif    /* ! AUTO_ARRAYS */

C...........   LOCAL VARIABLES and their descriptions:

        INTEGER          N
        INTEGER          ID( NROWS )
        REAL             VV( NROWS, NLAYS, NVARS )
        REAL             GG( NROWS, NLAYS )
        INTEGER          V, W, SIZE, GSIZ
        CHARACTER*120    MESG

#if ! AUTO_ARRAYS
        INTEGER          LSIZE  / -1 /
        POINTER          ( P, N )
        POINTER          ( Q, ID )
        POINTER          ( R, VV )
        POINTER          ( S, GG )

        SAVE     P, Q, R, S, LSIZE
#endif    /* ! AUTO_ARRAYS */

C***********************************************************************
C   begin body of subroutine  STATIDDAT

        GSIZ = NROWS * NVARS * NLAYS
        SIZE = 1 + 2 * ( NROWS + GSIZ )

#if ! AUTO_ARRAYS
        IF ( SIZE .GT. LSIZE ) THEN
            IF ( LSIZE .GT. 0 )  CALL FREE( P )
            P = MALLOC( 4 * SIZE )
            IF ( P .EQ. 0 ) THEN
                MESG = 'Memory allocation error'
                CALL M3EXIT( 'STATIDDAT', JDATE, JTIME, MESG, 2 )
            END IF
            Q = P + 4    	!  points to ID's
            R = Q + 4 * NROWS   !  points to variables
            S = R + 8 * GSIZ	!  points to scratch-buffer
            
            LSIZE = SIZE
        END IF
#endif    /* ! AUTO_ARRAYS */
        
        IF ( JDATE .NE. 0 .OR. JTIME .NE. 0 ) THEN	
            WRITE( LOGDEV,92010 )
     &          INNAME, JDATE, JTIME, DT2STR( JDATE, JTIME )
        ELSE
            WRITE( LOGDEV,92010 ) INNAME
        END IF

        IF ( READ3( INNAME, ALLVAR3, ALLAYS3,
     &                 JDATE, JTIME, N ) ) THEN
           
            W = 1

            DO  111  V = 1, NVARS

                IF ( VTYPES( V ) .EQ. M3REAL ) THEN

                    CALL STATI( NROWS, NLAYS, N, ID, VV( 1,1,W ),
     &                          NTHRES( V ), THRES( 1,V ),
     &                          VNAMES( V ), LOGDEV )
                    W = W + GSIZ

                ELSE IF ( VTYPES( V ) .EQ. M3INT ) THEN

                    CALL INTG2REAL( GSIZ, VV( 1,1,W ), GG )
                    CALL STATI( NROWS, NLAYS, N, ID, GG,
     &                          NTHRES( V ), THRES( 1,V ),
     &                          VNAMES( V ), LOGDEV )
                    W = W + GSIZ

                ELSE IF ( VTYPES( V ) .EQ. M3DBLE ) THEN

                    CALL DBLE2REAL( GSIZ, VV( 1,1,W ), GG )
                    CALL STATI( NROWS, NLAYS, N, ID, GG,
     &                          NTHRES( V ), THRES( 1,V ),
     &                          VNAMES( V ), LOGDEV )
                    W = W + 2 * GSIZ

                ELSE

                    MESG = 'Bad type for variable ' // VNAMES( V )
                    CALL M3EXIT( 'STATIDDAT', JDATE, JTIME, MESG, 2 )

                END IF

111         CONTINUE        !  end loop on variables
           
        ELSE                !  read3() failed:
           
            MESG = 'Read failure:  file ' // INNAME
            CALL M3EXIT( 'STATIDDAT', JDATE, JTIME, MESG, 2 )

        END IF              !  if read3() worked, or not


        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Informational (LOG) message formats... 92xxx

92010   FORMAT ( //5X, 'File:  ', A, :,
     &            /5X, 'Date and time:', I7.7, ':', I6.6, 2X, A )

        END

