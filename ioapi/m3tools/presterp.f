
        PROGRAM PRESTERP

C***********************************************************************
C Version "@(#)$Header$"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2002-2005 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  123
C
C  DESCRIPTION:
C       Interpolate specified file from sigma levels to pressure levels,
C       using coefficient and subscript tables from PRES_*_3D
C
C  PRECONDITIONS REQUIRED:
C       3D input file having same input grid as PRES_*_3D
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       GETNUM,   GETREAL,  GETYN,    INDEX1, PROMPTMFILE,
C       SECSDIFF, SEC2TIME, TIME2SEC, TRIMLEN
C
C  REVISION  HISTORY:
C       Prototype 7/4/99 by CJC
C       Version  11/2001 by CJC for I/O API Version 2.1
C       Version  11/2005 by CJC:  eliminate unused vbles
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

      INCLUDE 'PARMS3.EXT'      ! I/O API constants
      INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure
      INCLUDE 'IODECL3.EXT'     ! I/O API function declarations


C...........   PARAMETERS and their descriptions:

        CHARACTER*16	BLANK16
        PARAMETER     ( BLANK16 = ' ' )

        CHARACTER*80    PROGVER
        DATA PROGVER /
     &'$Id:: presterp.f 49 2007-07-06 16:20:50Z coats@borel          $'
     &  /


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL       GETYN
        CHARACTER*16  PROMPTMFILE
        INTEGER       GETNUM, IARGC, INDEX1, SECSDIFF, SEC2TIME,
     &                TIME2SEC, TRIMLEN
        REAL          GETREAL

        EXTERNAL  GETNUM,   GETREAL,  GETYN,    INDEX1, PROMPTMFILE,
     &            SECSDIFF, SEC2TIME, TIME2SEC, TRIMLEN


C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         LOGDEV  !  unit number for log file
        INTEGER         ARGCNT  !  number of command-line args, from IARGC()
        CHARACTER*256   ENVBUF  !  value from command line arguments

        INTEGER         STATUS
        LOGICAL         EFLAG, AFLAG
        LOGICAL         PFLAG( 3 )
        DATA            PFLAG / 3 * .FALSE. /

        CHARACTER*16    INAME   !  logical name of the input data file
        CHARACTER*16    PNAME   !  logical name of the input pressure file
        CHARACTER*16    FNAME   !  logical name of the output file

        INTEGER         NCOLS   !  grid dimensions, from file headers
        INTEGER         NROWS   !  grid dimensions, from file headers
        INTEGER         NLAYS   !  grid dimensions, from file headers
        INTEGER         NSTEPS  !  number of vbles in INAME
        INTEGER         VGTYP   !  vertical coord type
        REAL            VGTOP

        INTEGER         PLAYS   !  number of    pressure levels from PNAME
        REAL            PRESF( MXLAYS3 + 1 ) !  pressure levels from PNAME

        REAL*8          P_ALP      ! first, second, third map
        REAL*8          P_BET      ! projection descriptive
        REAL*8          P_GAM      ! parameters.

        REAL*8          XCENT      ! lon for coord-system X=0
        REAL*8          YCENT      ! lat for coord-system Y=0
        REAL*8          XORIG      ! X-coordinate origin of grid (map units)
        REAL*8          YORIG      ! Y-coordinate origin of grid
        REAL*8          XCELL      ! X-coordinate cell dimension
        REAL*8          YCELL      ! Y-coordinate cell dimension

        INTEGER         JDATE, JTIME, TSTEP
        INTEGER         L, V, T

        CHARACTER*256   MESG    !  buffer for messages

        REAL,    ALLOCATABLE::    VSIG( :, :, : )
        INTEGER, ALLOCATABLE::    PDEX( :, :, : )
        REAL,    ALLOCATABLE::    PLIN( :, :, : )
        REAL,    ALLOCATABLE::    PLOG( :, :, : )
        REAL,    ALLOCATABLE::    PPOW( :, :, : )
        REAL,    ALLOCATABLE::    VPRS( :, :, : )

        INTEGER         ITYPE( MXVARS3 )        !  interpolation type
        REAL            BADV( MXVARS3 ), VV     !  bad-value token

C...........   STATEMENT FUNCTION:  Floating point "unequal"

        REAL*8          XX, YY
        LOGICAL         DBLERR
        DBLERR( XX, YY ) =
     &      ( (XX - YY)**2 .GT. 1.0E-10*( XX**2 + YY**2 + 1.0E-5 ) )


C***********************************************************************
C   begin body of program PRESTERP

        LOGDEV = INIT3()

        WRITE( *,92000 )
     &  ' ',
     &  'Program PRESTERP to interpolate from sigma level input file',
     &  'to pressure levels, using coefficients and indices obtained',
     &  'from PRES_CRO_3D or PRES_DOT_3D.  Both files must have the',
     &  'same horizontal grid, and must be of type GRIDDED.',
     &  ' ',
     &  'You need to have assigned logical names to the physical file',
     &  'names of both files, according to Models-3 conventions, ',
     &  'using the operation ',
     &  ' ',
     &  '     setenv <lname> <pname>',
     &  ' ',
     &  'USAGE:',
     &  '     presterp [INFILE PRESFILE OUTFILE [<interp-type>]]',
     &  ' ',
     &  'where INFILE, PRESFILE, and OUTFILE are optional arguments ',
     &  'for the logical names of the input data, the pressure-level,',
     &  'and the output files, and  <interp-type> is the type of ',
     &  'interpolation desired:',
     &  ' ',
     &  '     LIN   for linear-in-pressure interpolation',
     &  '     LOG   for log(pressure)      interpolation',
     &  '     POW   for pressure^alpha     interpolation',
     &  ' ',
     &  '(and then answer the prompts).',
     &  'NOTE:  If you specify the interpolation-type interactively ',
     &  '(at the prompt), you will specify it separately for each ',
     &  'variable, rather than for all of them at once.',
     &  ' ',
     &'Program copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.',
     &'and (C) 2002-2007 Baron Advanced Meteorological Systems, LLC',
     &'Released under Version 2 of the GNU General Public License.',
     &'See enclosed GPL.txt, or URL',
     &'http://www.gnu.org/copyleft/gpl.html  Comments and',
     &'questions are welcome and can be sent to',
     &' ',
     &'    coats@baronams.com',
     &' ',
     &'    Carlie J. Coats, Jr.',
     &'    920 Main Campus Drive, Suite 101',
     &'    Raleigh, NC 27606',
     &' ',
     &'See URL  http://www.baronams.com/products/ioapi/AA.html#tools',
     &' ',
     &'Program version: ',
     &PROGVER,
     &'Program release tag: $Name$',
     &' '

        IF ( .NOT.GETYN( 'Continue with program?', .TRUE. ) ) THEN
            CALL M3EXIT( 'PRESTERP', 0, 0,
     &                   'Terminated at user request', 2 )
        END IF

        ARGCNT = IARGC()
        IF ( ARGCNT .GE. 3 ) THEN

            CALL GETARG( 1, ENVBUF )
            INAME = ENVBUF( 1:16 )
            IF ( .NOT. OPEN3( INAME, FSREAD3, 'PRESTERP' ) ) THEN
                MESG = 'Could not open input file "'
     &                    // INAME( 1:TRIMLEN( INAME ) ) // '"'
                CALL M3EXIT( 'PRESTERP', 0, 0, MESG, 2 )
            END IF

            CALL GETARG( 2, ENVBUF )
            PNAME = ENVBUF( 1:16 )
            IF ( .NOT. OPEN3( PNAME, FSREAD3, 'PRESTERP' ) ) THEN
                MESG = 'Could not open input file "'
     &                    // PNAME( 1:TRIMLEN( PNAME ) ) // '"'
                CALL M3EXIT( 'PRESTERP', 0, 0, MESG, 2 )
            END IF

            IF ( ARGCNT .EQ. 4 ) THEN
                CALL GETARG( 4, ENVBUF )
                CALL UPCASE( ENVBUF )
                IF ( ENVBUF( 1:3 ) .EQ. 'LIN' ) THEN
                    T = 1
                    PFLAG( 1 ) = .TRUE.
                ELSE IF ( ENVBUF( 1:3 ) .EQ. 'LOG' ) THEN
                    T = 2
                    PFLAG( 2 ) = .TRUE.
                ELSE IF ( ENVBUF( 1:3 ) .EQ. 'POW' ) THEN
                    T = 3
                    PFLAG( 3 ) = .TRUE.
                ELSE
                    MESG = 'Unknown interpolation type "'
     &                    // ENVBUF( 1:TRIMLEN( ENVBUF ) ) // '"'
                    CALL M3EXIT( 'PRESTERP', 0, 0, MESG, 2 )
                END IF
                DO  V = 1, MXVARS3
                    ITYPE( V ) = T
                END DO
            END IF

        ELSE IF ( ARGCNT .EQ. 0 ) THEN

            INAME = PROMPTMFILE( 'Enter logical name for INPUT FILE',
     &                           FSREAD3, 'AFILE', 'PRESTERP' )

            PNAME = PROMPTMFILE( '            ... for PRESSURE FILE',
     &                           FSREAD3, 'PFILE', 'PRESTERP' )

        ELSE
            CALL M3EXIT( 'PRESTERP', 0, 0,
     &      'USAGE:  presterp [INFILE PRESFILE OUTFILE]', 2 )
        END IF


        !!  Get input file descriptions and check file headers.

        IF ( .NOT. DESC3( PNAME ) ) THEN
            MESG = 'Could not get description for "' // PNAME
            CALL M3EXIT( 'PRESTERP', 0, 0,MESG, 2 )
        END IF
        NCOLS = NCOLS3D
        NROWS = NROWS3D
        PLAYS = NLAYS3D
        P_ALP = P_ALP3D
        P_BET = P_BET3D
        P_GAM = P_GAM3D
        XCENT = XCENT3D
        YCENT = YCENT3D
        XORIG = XORIG3D
        YORIG = YORIG3D
        XCELL = XCELL3D
        YCELL = YCELL3D
        VGTYP = VGTYP3D
        VGTOP = VGTOP3D
        DO  L = 1, PLAYS+1
            PRESF( L ) = VGLVS3D( L )
        END DO

        IF ( .NOT. DESC3( INAME ) ) THEN
            MESG = 'Could not get description for "' // INAME
            CALL M3EXIT( 'PRESTERP', 0, 0,MESG, 2 )
        END IF
        JDATE  = SDATE3D
        JTIME  = STIME3D
        TSTEP  = TSTEP3D
        NSTEPS = MXREC3D
        NLAYS  = NLAYS3D

        IF ( NCOLS .NE. NCOLS3D ) THEN
            CALL M3EXIT( 'PRESTERP', 0, 0, 'NCOLS mismatch', 2 )
        END IF

        IF ( NROWS .NE. NROWS3D ) THEN
            CALL M3EXIT( 'PRESTERP', 0, 0, 'NROWS mismatch', 2 )
        END IF

        IF ( DBLERR( P_ALP, P_ALP3D ) ) THEN
            CALL M3EXIT( 'PRESTERP', 0, 0, 'P_ALP mismatch', 2 )
        END IF

        IF ( DBLERR( P_BET, P_BET3D ) ) THEN
            CALL M3EXIT( 'PRESTERP', 0, 0, 'P_BET mismatch', 2 )
        END IF

        IF ( DBLERR( P_GAM, P_GAM3D ) ) THEN
            CALL M3EXIT( 'PRESTERP', 0, 0, 'P_GAM mismatch', 2 )
        END IF

        IF ( DBLERR( XCENT, XCENT3D ) ) THEN
            CALL M3EXIT( 'PRESTERP', 0, 0, 'XCENT mismatch', 2 )
        END IF

        IF ( DBLERR( YCENT, YCENT3D ) ) THEN
            CALL M3EXIT( 'PRESTERP', 0, 0, 'YCENT mismatch', 2 )
        END IF

        IF ( DBLERR( XORIG, XORIG3D ) ) THEN
            CALL M3EXIT( 'PRESTERP', 0, 0, 'XORIG mismatch', 2 )
        END IF

        IF ( DBLERR( YORIG, YORIG3D ) ) THEN
            CALL M3EXIT( 'PRESTERP', 0, 0, 'YORIG mismatch', 2 )
        END IF

        IF ( DBLERR( XCELL, XCELL3D ) ) THEN
            CALL M3EXIT( 'PRESTERP', 0, 0, 'XCELL mismatch', 2 )
        END IF

        IF ( DBLERR( YCELL, YCELL3D ) ) THEN
            CALL M3EXIT( 'PRESTERP', 0, 0, 'YCELL mismatch', 2 )
        END IF


        !!  Create output file  Reuse description for INAME, except for
        !!  pressure levels obtained from PNAME.

        VGTYP3D = VGTYP
        VGTOP3D = VGTOP
        DO  L = 1, PLAYS+1
            VGLVS3D( L ) = PRESF( L )
        END DO
        NLAYS3D = PLAYS

        IF ( ARGCNT .GE. 3 ) THEN

            CALL GETARG( 3, ENVBUF )
            FNAME = ENVBUF( 1:16 )
            IF ( .NOT. OPEN3( FNAME, FSUNKN3, 'PRESTERP' ) ) THEN
                MESG = 'Could not open input file "'
     &                    // FNAME( 1:TRIMLEN( FNAME ) ) // '"'
                CALL M3EXIT( 'PRESTERP', 0, 0, MESG, 2 )
            END IF

        ELSE

            FNAME = PROMPTMFILE( 'Enter logical name for OUTPUT FILE',
     &                           FSUNKN3, 'OFILE', 'PRESTERP' )

        END IF


        !!  Get pressure-interpolation flags for each variable, and
        !!  allocate working buffers.

        IF ( ARGCNT .LT. 4 ) THEN

            WRITE( *,92000 )
     &  ' ',
     &  'Three types of pressure-level interpolation are supported:',
     &  ' ',
     &  '    1:  Linear-in-pressure interpolation',
     &  '    2:  Log-interpolation    (linear in log(P) )',
     &  '    3:  Power-interpolation  (linear in P**GAMMA)',
     &  ' ',
     &  'For each variable you need to specify the type., as well as',
     &  'the value used to indicate BAD (i.e. below-terrain) 3-D grid',
     &  'locations.',
     &  ' '
            VV = BADVAL3
            DO  V = 1, NVARS3D
                MESG = 'Enter interpolation type for "' //
     &                  VNAME3D(V)( 1:TRIMLEN( VNAME3D(V) ) ) // '"'
                T = GETNUM( 1, 3, 1, MESG )
                ITYPE( V ) = T
                PFLAG( T ) = .TRUE.
                MESG = 'Enter BELOW-GROUND value for "' //
     &                  VNAME3D(V)( 1:TRIMLEN( VNAME3D(V) ) ) // '"'
                VV = GETREAL( BADVAL3, -BADVAL3, 0.0, MESG )
                BADV( V ) = VV
            END DO

        ELSE

            DO  V = 1, NVARS3D
                BADV( V ) = BADVAL3
            END DO

        END IF

        ALLOCATE ( PDEX( NCOLS, NROWS, PLAYS ),
     &             PLIN( NCOLS, NROWS, PLAYS ),
     &             PLOG( NCOLS, NROWS, PLAYS ),
     &             PPOW( NCOLS, NROWS, PLAYS ),
     &             VPRS( NCOLS, NROWS, PLAYS ),
     &             VSIG( NCOLS, NROWS, NLAYS ), STAT = STATUS )

        IF ( STATUS .NE. 0 ) THEN
            WRITE( MESG, '( A, I10 )' )
     &      'Allocation failed:  STAT=', STATUS
            CALL M3EXIT( 'PRESTERP', 0, 0, MESG, 2 )
        END IF


        !!  Main processing loops:

        AFLAG = .FALSE.
        DO  T = 1, NSTEPS

            EFLAG = .FALSE.

            IF ( .NOT. READ3( PNAME, 'PDEX', ALLAYS3, JDATE, JTIME,
     &                        PDEX ) ) THEN
                MESG = 'Could not read PDEX from ' // PNAME
                CALL M3WARN( 'PRESTERP', 0, 0, MESG )
                EFLAG = .TRUE.
                AFLAG = .TRUE.
            END IF

            IF ( PFLAG( 1 ) ) THEN
                IF ( .NOT. READ3( PNAME, 'CLIN', ALLAYS3,
     &                            JDATE, JTIME, PLIN ) ) THEN
                    MESG = 'Could not read PLIN from ' // PNAME
                    CALL M3WARN( 'PRESTERP', 0, 0, MESG )
                    EFLAG = .TRUE.
                    AFLAG = .TRUE.
                END IF
            END IF

            IF ( PFLAG( 2 ) ) THEN
                IF ( .NOT. READ3( PNAME, 'CLOG', ALLAYS3,
     &                            JDATE, JTIME, PLOG ) ) THEN
                    MESG = 'Could not read PLOG from ' // PNAME
                    CALL M3WARN( 'PRESTERP', 0, 0, MESG )
                    EFLAG = .TRUE.
                    AFLAG = .TRUE.
                END IF
            END IF

            IF ( PFLAG( 3 ) ) THEN
                IF ( .NOT. READ3( PNAME, 'CPOW', ALLAYS3,
     &                            JDATE, JTIME, PPOW ) ) THEN
                    MESG = 'Could not read PPOW from ' // PNAME
                    CALL M3WARN( 'PRESTERP', 0, 0, MESG )
                    EFLAG = .TRUE.
                    AFLAG = .TRUE.
                END IF
            END IF

            IF ( EFLAG ) CYCLE

            DO  V = 1, NVARS3D

                IF ( .NOT. READ3( INAME, VNAME3D( V ), ALLAYS3,
     &                            JDATE, JTIME, VSIG ) ) THEN
                    MESG = 'Could not read "' // VNAME3D( V ) //
     &                     '" from ' // PNAME
                    CALL M3WARN( 'PRESTERP', 0, 0, MESG )
                    AFLAG = .TRUE.
                    CYCLE
                END IF

                IF      ( ITYPE( V ) .EQ. 1 ) THEN
                    CALL PTERP( NCOLS, NROWS, NLAYS, PLAYS, BADV( V ),
     &                          PDEX,  PLIN,  VSIG,  VPRS )
                ELSE IF ( ITYPE( V ) .EQ. 2 ) THEN
                    CALL PTERP( NCOLS, NROWS, NLAYS, PLAYS, BADV( V ),
     &                          PDEX,  PLOG,  VSIG,  VPRS )
                ELSE IF ( ITYPE( V ) .EQ. 3 ) THEN
                    CALL PTERP( NCOLS, NROWS, NLAYS, PLAYS, BADV( V ),
     &                          PDEX,  PPOW,  VSIG,  VPRS )
                END IF

                IF ( .NOT.WRITE3( FNAME, VNAME3D( V ),
     &                            JDATE, JTIME, VPRS ) ) THEN
                    MESG = 'Could not write "' // VNAME3D( V ) //
     &                     '" to ' // FNAME
                    CALL M3WARN( 'PRESTERP', 0, 0, MESG )
                    AFLAG = .TRUE.
                END IF

            END DO              !  end loop on variables V

            CALL NEXTIME( JDATE, JTIME, TSTEP )

        END DO          !  end loop on timesteps T

        CALL M3EXIT( 'PRESTERP', 0, 0,
     &               'Successful completion of program PRESTERP', 0 )

!..........................   FORMAT STATEMENTS  ....................
!...........   Informational (LOG) message formats... 92xxx

92000 FORMAT( 5X, A )

!.........................................................................

      CONTAINS

            SUBROUTINE  PTERP( NCOLS, NROWS, NLAYS, PLAYS, BADVAL,
     &                         NDEX,  COEF,  VSIG,  VPRS )
            INTEGER         NCOLS, NROWS, NLAYS, PLAYS
            REAL            BADVAL
            INTEGER         NDEX( NCOLS, NROWS, PLAYS )
            REAL            COEF( NCOLS, NROWS, PLAYS )
            REAL            VPRS( NCOLS, NROWS, PLAYS )
            REAL            VSIG( NCOLS, NROWS, NLAYS )

            INTEGER         C, R, L, K
            REAL            P, Q

            !!..........................................................

            DO  L = 1, PLAYS
            DO  R = 1, NROWS
            DO  C = 1, NCOLS
                P = COEF( C,R,L )
                IF ( P .LE. 1.0 ) THEN
                    K = NDEX( C,R,L )
                    Q = 1.0 - P
                    VPRS( C,R,L ) = P*VSIG( C,R,K ) + Q*VSIG( C,R,K+1 )
                ELSE
                    VPRS( C,R,L ) = BADVAL
                END IF
            END DO
            END DO
            END DO

            RETURN

            END SUBROUTINE  PTERP

        END PROGRAM PRESTERP

