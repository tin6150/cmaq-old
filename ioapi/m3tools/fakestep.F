
        SUBROUTINE  FAKESTEP( FNAME, JDATE, JTIME, OPS, VAL )

C***********************************************************************
C Version "@(#)$Header$"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2002-2005 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  99
C
C  FUNCTION:  Perform memory allocation and work for program FAKEFILE
C
C
C  PRECONDITIONS REQUIRED:
C	file FNAME already open; 
C	description of FNAME in FDESC3.EXT valid.        
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C	IFILL, RFILL, DFILL
C
C  REVISION  HISTORY:
C	prototype 8/1995 by CJC
C       Modified  9/1999 by CJC for enhanced portability
C       Modified 11/2005 by CJC:  removed unused vbles
C***********************************************************************

#ifndef AUTO_ARRAYS
#if __sgi  || __sun || __osf__ || __mips__
#define AUTO_ARRAYS 0
#endif
#ifdef _CRAY
#define AUTO_ARRAYS 1
#endif
#ifdef _AIX
#define AUTO_ARRAYS 1
#endif
#endif

#ifndef AUTO_ARRAYS
#include   "--Error compiling:  unsupported architecture---"
#endif

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'  !  I/O parameter definitions
        INCLUDE 'FDESC3.EXT'  !  file description
        INCLUDE 'IODECL3.EXT' !  I/O definitions and declarations


C...........   ARGUMENTS and their descriptions:

        CHARACTER*16	FNAME
        INTEGER		JDATE
        INTEGER		JTIME
        INTEGER		OPS( NVARS3D )
        REAL    	VAL( NVARS3D )


C...........   LOCAL VARIABLES and their descriptions:
C...........   NOTE:  the ANSI standard requires the use of SAVE statements
C...........   for variables which must retain their values from call to call.

        INTEGER		NCOLS, NROWS, NLAYS
        REAL            GRID( NCOLS3D, NROWS3D, NLAYS3D )
	INTEGER         STEP
	DATA            STEP   / 0 /
	SAVE            NCOLS, NROWS, NLAYS, STEP
        
#if ( ! AUTO_ARRAYS )
        INTEGER         MALLOC  !  external function
        INTEGER         SIZE
	INTEGER         LSIZE
	DATA            LSIZE  / -1 /
	POINTER         ( P, GRID )

	SAVE            P, LSIZE
#endif    /* ! AUTO_ARRAYS */


C...........   SCRATCH LOCAL VARIABLES and their descriptions:


        CHARACTER*80	MESG
        INTEGER		C, R, L, V
        INTEGER		TYPE, OP, IDEV
        INTEGER		LTYPE, LOP


C***********************************************************************
C   begin body of subroutine  FAKESTEP

#if ( ! AUTO_ARRAYS )
        IF ( LSIZE .LT. 0 ) THEN

            IF( FTYPE3D .EQ. CUSTOM3 ) THEN
                NCOLS = NCOLS3D
                NROWS = 1
                NLAYS = NLAYS3D
                SIZE  = NCOLS3D
            ELSE IF ( FTYPE3D .EQ. GRDDED3 ) THEN
                NCOLS = NCOLS3D
                NROWS = NROWS3D
                NLAYS = NLAYS3D
                SIZE  = NCOLS3D * NROWS3D
            ELSE IF ( FTYPE3D .EQ. BNDARY3 ) THEN
                NCOLS = NTHIK3D
                NROWS = 2 * ( NCOLS3D + NROWS3D + 2 * NTHIK3D )
                NLAYS = NLAYS3D
                SIZE  = ABS( NTHIK3D ) * NCOLS
            ELSE
                WRITE( MESG,94010 ) 
     &          'File type', FTYPE3D, 'not yet supported'
                CALL M3EXIT( 'FAKESTEP', JDATE, JTIME, MESG, 2 )
            END IF

            LSIZE = SIZE
            DO  V = 1, NVARS3D
                IF( VTYPE3D( V ) .EQ. M3DBLE ) THEN
                    LSIZE = 2 * SIZE
                ELSE IF( ( VTYPE3D( V ) .NE. M3INT  ) .AND.
     &                   ( VTYPE3D( V ) .NE. M3REAL ) ) THEN
                    WRITE( MESG,94010 ) 
     &              'Vble type', VTYPE3D( V ), 'not yet supported'
                    CALL M3EXIT( 'FAKESTEP', JDATE, JTIME, MESG, 2 )
                END IF
            END DO

            SIZE = LSIZE
        
            P = MALLOC( 4 * SIZE * NLAYS )
            IF ( P .EQ. 0 ) THEN
                CALL M3EXIT( 'FAKESTEP', JDATE, JTIME, 
     &                       'MALLOC failure', 2 )
            END IF

        END IF		!  if lsize < 0
#endif    /* ! AUTO_ARRAYS */

#if  ( AUTO_ARRAYS )
            IF( FTYPE3D .EQ. CUSTOM3 ) THEN
                NCOLS = NCOLS3D
                NROWS = 1
                NLAYS = NLAYS3D
            ELSE IF ( FTYPE3D .EQ. GRDDED3 ) THEN
                NCOLS = NCOLS3D
                NROWS = NROWS3D
                NLAYS = NLAYS3D
            ELSE IF ( FTYPE3D .EQ. BNDARY3 ) THEN
                NCOLS = NTHIK3D
                NROWS = 2 * ( NCOLS3D + NROWS3D + 2 * NTHIK3D )
                NLAYS = NLAYS3D
            ELSE
                WRITE( MESG,94010 ) 
     &          'File type', FTYPE3D, 'not yet supported'
                CALL M3EXIT( 'FAKESTEP', JDATE, JTIME, MESG, 2 )
            END IF

            DO  V = 1, NVARS3D
                IF( ( VTYPE3D( V ) .NE. M3INT  ) .AND.
     &              ( VTYPE3D( V ) .NE. M3REAL ) ) THEN
                    WRITE( MESG,94010 ) 
     &              'Vble type', VTYPE3D( V ), 'not yet supported'
                    CALL M3EXIT( 'FAKESTEP', JDATE, JTIME, MESG, 2 )
                END IF
            END DO
#endif    /*  AUTO_ARRAYS */


        STEP  = STEP + 1
        LTYPE = -9999
        LOP   = -9999

        DO  99  V = 1, NVARS3D

            TYPE = VTYPE3D( V )
            OP   = OPS( V )

            IF ( OP .LT. 0 ) THEN

                IDEV = -OP
                DO  L = 1, NLAYS3D
                DO  R = 1, NROWS3D
                    READ( IDEV,* ) (GRID( C,R,L ), C = 1, NCOLS3D )
                END DO
                END DO

            ELSE IF ( TYPE .NE. LTYPE  .OR.  
     &                LOP  .NE. OP     .OR.
     &                TYPE .EQ. 5         ) THEN

                IF ( OP .EQ. 4 ) THEN
                    VAL( V ) = FLOAT( STEP )
                END IF
                IF ( TYPE .EQ. M3INT ) THEN
                    CALL IFILL( GRID, NCOLS, NROWS, NLAYS, 
     &                          OPS( V ), VAL( V ) )
                ELSE IF ( TYPE .EQ. M3REAL ) THEN
                    CALL RFILL( GRID, NCOLS, NROWS, NLAYS,
     &                          OPS( V ), VAL( V ) )
                ELSE IF ( TYPE .EQ. M3DBLE ) THEN
                    CALL DFILL( GRID, NCOLS, NROWS, NLAYS,
     &                          OPS( V ), DBLE( VAL( V ) ) )
                END IF
                LTYPE = TYPE
                LOP   = OP

            END IF

            IF( .NOT. WRITE3( FNAME, VNAME3D( V ), 
     &                        JDATE, JTIME, GRID ) ) THEN
                MESG = 'Error writing ' // VNAME3D( V ) //
     &                 ' to ' // FNAME
                CALL M3EXIT( 'FAKESTEP', JDATE, JTIME, MESG, 2 )
            END IF
            
99      CONTINUE

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91000   FORMAT ( //5X , '*** ERROR ABORT in subroutine FAKESTEP ***',
     &            /5X , A ,
     &           // )        !  generic error message format


C...........   Informational (LOG) message formats... 92xxx


C...........   Formatted file I/O formats............ 93xxx


C...........   Internal buffering formats............ 94xxx

94010	FORMAT( A, I4, 2X, A )


C...........   Miscellaneous formats................. 95xxx

95000   FORMAT ( /5X , A , $ )          !  generic prompt format.


        END

