
        SUBROUTINE  TAGGREG

C***********************************************************************
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi_tools/./ioapi_tools/src/taggreg.F,v 1.2 2000/12/14 22:55:33 smith_w Exp $"
C EDSS/Models-3 M3TOOLS.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  entry INITTAG body starts at line  118
C  entry AGGREG  body starts at line  164
C  entry OUTAGG  body starts at line  209
C
C  FUNCTION:
C       Aggregate time steps as either sum, average, or max.
C       INITAGG: allocates memory and initializes array with first time
C                step of data
C       AGGREG:  Modifies gridded storage array based on current time step
C                of data
C       OUTAGG:  Completes gridded array values and writes to output file
C
C  PRECONDITIONS REQUIRED:
C       Valid dates and times JDATE:JTIME
C       f77 MALLOC()-allocation operating environment (such as Sun, SGI)
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       Models-3 I/O:  M3ERR(), READ3(), WRITE3()
C
C  REVISION  HISTORY:
C       Prototype 5/97 by M Houyoux
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

C...........   PARAMETERS
        INTEGER         M3SUM
        INTEGER         M3AVE
        INTEGER         M3MAX

        PARAMETER(      M3SUM = 1,
     &                  M3AVE = 2,
     &                  M3MAX = 3  )

C...........   ARGUMENTS and their descriptions:

        INTEGER         SIZE    ! total array dimensions for this variable
        INTEGER         LAYER   ! layer number to read
        INTEGER         JDATE   ! current model date
        INTEGER         JTIME   ! current model time
        CHARACTER*16    INAME   !  logical name of the  input file
        CHARACTER*16    ONAME   !  logical name of the output file
        CHARACTER*16    VNAMEI  !  vble name (input)
        CHARACTER*16    VNAMEO  !  vble name (output)
        INTEGER         TYPE    ! type of aggregation to perform
        INTEGER         NSTEPS  ! number of time steps - used for average only
        INTEGER         LOGDEV  ! unit number for output


C...........   EXTERNAL FUNCTIONS:

#if ! AUTO_ARRAYS
        INTEGER         MALLOC
#endif    /* ! AUTO_ARRAYS */


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         I

        CHARACTER*8     BUFFER

#if AUTO_ARRAYS
!  f90 treatment
        REAL, ALLOCATABLE, SAVE::    GRID   ( : )
        REAL, ALLOCATABLE, SAVE::    SCRATCH( : )
        INTEGER, SAVE::              LSIZE = -1
#endif    /* AUTO_ARRAYS */


#if ! AUTO_ARRAYS
        REAL            GRID   ( SIZE )
        REAL            SCRATCH( SIZE )
        INTEGER         LSIZE
        DATA            LSIZE  / -1 /
        POINTER         ( P, GRID )
        POINTER         ( Q, SCRATCH )

        SAVE            LSIZE, P, Q
#endif    /* ! AUTO_ARRAYS */


C***********************************************************************
C   begin body of subroutine  TAGGREG

C***********************************************************************
C   begin body of entry point INITAGG

        ENTRY INITAGG( SIZE,   LAYER, JDATE, JTIME, INAME, 
     &                 VNAMEI, LOGDEV )


#if AUTO_ARRAYS
        IF ( SIZE .GT. LSIZE ) THEN
            ALLOCATE( GRID( 2*SIZE ), SCRATCH( 2*SIZE ), STAT=I )
            IF ( I .NE. 0 ) THEN
                 CALL M3EXIT( 'XTSTEP', JDATE, JTIME,
     &                        'Memory allocation error', 2 )
            END IF
            LSIZE = SIZE
        END IF
#endif    /* AUTO_ARRAYS */

#if ! AUTO_ARRAYS
        IF ( SIZE .GT. LSIZE ) THEN
            IF ( LSIZE .GT. 0 )  CALL FREE( P )
            IF ( LSIZE .GT. 0 )  CALL FREE( Q )

            P = MALLOC( 8 * SIZE )
            Q = MALLOC( 8 * SIZE )

            IF ( P .EQ. 0  .OR. Q .EQ. 0 ) THEN
                 CALL M3EXIT( 'XTSTEP', JDATE, JTIME,
     &                        'Memory allocation error', 2 )
            END IF
            LSIZE = SIZE
        END IF
#endif    /* ! AUTO_ARRAYS */

        IF ( .NOT. READ3( INAME, VNAMEI, LAYER,
     &                    JDATE, JTIME, GRID ) ) THEN

            CALL M3EXIT ( 'M3TPROC:TAGGREG', JDATE, JTIME,
     &                    'Read failure:  file "' // INAME //
     &                    '" variable "' // VNAMEI // '"', 2 )

        END IF              !  if read3() worked, or not

        RETURN


C***********************************************************************
C   begin body of entry point AGGREG

        ENTRY AGGREG( SIZE, LAYER , JDATE, JTIME, INAME,
     &                VNAMEI, TYPE , LOGDEV   )

        IF ( SIZE .EQ. LSIZE ) THEN

            IF ( .NOT. READ3( INAME, VNAMEI, LAYER,
     &                        JDATE, JTIME, SCRATCH ) ) THEN
 
                CALL M3EXIT ( 'M3TPROC:TAGGREG', JDATE, JTIME,
     &                        'Read failure:  file "' // INAME //
     &                        '" variable "' // VNAMEI // '"', 2 )

            ELSE IF( TYPE .EQ. M3SUM .OR. TYPE .EQ. M3AVE ) THEN

                DO 111 I = 1, SIZE
                    GRID( I ) = GRID( I ) + SCRATCH( I )
111             CONTINUE

            ELSE IF( TYPE .EQ. M3MAX ) THEN

                DO 122 I = 1, SIZE
                    GRID( I ) = MAX( GRID( I ), SCRATCH( I ) )
122             CONTINUE

            ELSE

                WRITE( BUFFER, '(I8)' ) TYPE   
                CALL M3EXIT ( 'M3TPROC:TAGGREG', JDATE, JTIME,
     &                        'Aggregation type "' // BUFFER // 
     &                        '" not supported' , 2 )

            END IF              !  if read3() worked, or not
        ELSE

            CALL M3EXIT ( 'M3TPROC:TAGGREG', JDATE, JTIME,
     &                    'Illegal attempt to change SIZE argument', 2 )

        END IF

        RETURN


C***********************************************************************
C   begin body of entry point OUTAGG

        ENTRY OUTAGG( SIZE, LAYER , JDATE, JTIME, ONAME,
     &                VNAMEO, TYPE , NSTEPS, LOGDEV )

        IF ( SIZE .EQ. LSIZE ) THEN

            IF( TYPE .EQ. M3AVE ) THEN

                DO 211 I = 1, SIZE
                    GRID( I ) = GRID( I ) / REAL( NSTEPS ) 
211             CONTINUE

            END IF

            IF ( .NOT. WRITE3( ONAME, VNAMEO,
     &                         JDATE, JTIME, GRID ) ) THEN
 
                CALL M3EXIT ( 'M3TPROC:TAGGREG', JDATE, JTIME,
     &                        'Write failure:  file "' // ONAME //
     &                        '" variable "' // VNAMEO // '"', 2 )

            END IF

        ELSE 

            CALL M3EXIT ( 'M3TPROC:TAGGREG', JDATE, JTIME,
     &                    'Illegal attempt to change SIZE argument', 2 )

        END IF

        RETURN

        END

