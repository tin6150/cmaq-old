
        PROGRAM GINTERP

C***********************************************************************
C Version "@(#)$Header $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2005 Baron Advanced Meteorological Systems,LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  143
C
C  DESCRIPTION:
C       For each time step in the specified time step sequence,
C       reads all variables from the specified input file, optionally
C       under the control of the specified synchronization file,
C       interpolate it to the specified output grid, and write them
C       to the specified output file.
C
C  PRECONDITIONS REQUIRED:
C       setenv <logical name> <physical (path) name> for the input,
C       output, and GRIDDESC files.
C       Input file and output grid use the same coordinate system.
C       Specified time step sequence is valid for both the input and
C       synch files.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       I/O API
C
C  REVISION  HISTORY:
C       Prototype 8/99 by Carlie J. Coats, Jr., NCSC
C       Version  11/2001 by CJC for I/O API Version 2.1
C       Version 6/2005 by CJC:  correct LO-value for STIME=GETNUM(...
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

      INCLUDE 'PARMS3.EXT'      ! I/O API constants
      INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure
      INCLUDE 'IODECL3.EXT'     ! I/O API function declarations


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL         DSCGRID
        INTEGER         GETNUM
        LOGICAL         GETYN
        CHARACTER*16    PROMPTMFILE

        EXTERNAL        DSCGRID, GETNUM, GETYN, PROMPTMFILE

C...........   PARAMETERS and their descriptions:

        CHARACTER*80    PROGVER
        DATA PROGVER /
     &'$Id:: ginterp.f 49 2007-07-06 16:20:50Z coats@borel           $'
     &  /


C...........   LOCAL VARIABLES and their descriptions:

        CHARACTER*16    FNAME   !  input data  file logical name
        CHARACTER*16    SNAME   !  input synch file logical name
        CHARACTER*16    SVBLE   !  input   synch variable   name
        CHARACTER*16    ONAME   !  output data file logical name
        CHARACTER*16    CNAME   !  output coordinate system name
        CHARACTER*16    GNAME   !  output grid name

        LOGICAL         IFLAG	!  true iff interp (instead of copy)
        LOGICAL         SFLAG	!  true iff controlled by synch file

        LOGICAL         EFLAG
        CHARACTER*256   MESG

        INTEGER         LDEV        !  log-device
        INTEGER         STATUS      !  allocation-status

        INTEGER         C, R, L, V, N  !  loop counters

        CHARACTER*16    GDNAM1      ! grid name
        INTEGER         NCOLS1      ! number of grid columns
        INTEGER         NROWS1      ! number of grid rows
        INTEGER         NLAYS1      ! number of layers
        INTEGER         GDTYP1      ! grid type:  1=LAT-LON, 2=UTM, ...
        REAL*8          P_ALP1      ! first, second, third map
        REAL*8          P_BET1      ! projection descriptive
        REAL*8          P_GAM1      ! parameters.
        REAL*8          XCENT1      ! lon for coord-system X=0
        REAL*8          YCENT1      ! lat for coord-system Y=0
        REAL*8          XORIG1      ! X-coordinate origin of grid (map units)
        REAL*8          YORIG1      ! Y-coordinate origin of grid
        REAL*8          XCELL1      ! X-coordinate cell dimension
        REAL*8          YCELL1      ! Y-coordinate cell dimension
        REAL*8          XFINL1      ! X-coordinate origin of grid (map units)
        REAL*8          YFINL1      ! Y-coordinate origin of grid

        REAL*8          X0, Y0, X, Y

        INTEGER         NCOLS2      ! number of grid columns
        INTEGER         NROWS2      ! number of grid rows

        INTEGER         SIZE        ! grid volume, for copy

        INTEGER         JDATE
        INTEGER         JTIME
        INTEGER         TSTEP
        INTEGER         NRECS

        REAL,    ALLOCATABLE::   SBUF( :, : )
        REAL,    ALLOCATABLE::   INBUF( :, :, : )
        REAL,    ALLOCATABLE::   OUTBUF( :, :, : )
        REAL,    ALLOCATABLE::   XBUF( :, : )
        REAL,    ALLOCATABLE::   YBUF( :, : )
        REAL,    ALLOCATABLE::   CBUF( :, : )
        REAL,    ALLOCATABLE::   COPYBUF( : )
        INTEGER, ALLOCATABLE::   IBUF( :, : )



C...........   STATEMENT FUNCTIONS:  REAL, REAL*8 "definitely unequal"

        LOGICAL         FLTERR
        REAL            PP, QQ

        FLTERR( PP, QQ ) =
     &      ( (PP - QQ)**2  .GT.  1.0E-10*( PP*PP + QQ*QQ + 1.0E-5 ) )

        LOGICAL         DBLERR
        REAL*8          P, Q

        DBLERR( P, Q ) =
     &      ( (P - Q)**2  .GT.  1.0E-10*( P*P + Q*Q + 1.0E-5 ) )


C***********************************************************************
C   begin body of program GINTERP

        LDEV = INIT3()

        WRITE( *, '( 5X, A )' )
     & ' ',
     & 'Program GINTERP to read all variables in each time step in ',
     & 'the specified time step sequence from the specified input',
     & 'file, optionally under the control of the specified',
     & 'synchronization file, copy or interpolate it to the specified',
     & 'output grid, and write them to the specified output file.',
     & ' ',
     & 'THE PROGRAM WILL PROMPT YOU for the logical names of the',
     & 'input data file, the input synch file, and the output file,',
     & 'the time step sequence, and the GRIDDESC name of the output',
     & 'grid.',
     & ' ',
     & 'If you wish to copy time steps, instead of interpolate them,',
     & 'respond "SAME" to the prompt for output grid name.',
     & ' ',
     & 'PRECONDITIONS REQUIRED:',
     & ' ',
     & '    setenv <input data  file>    <path-name>',
     & '    setenv <input synch file>    <path-name, or "NONE">',
     & '    setenv GRIDDESC              <path-name>',
     & '    time step sequence is valid for both input files',
     & '    Input file and output grid use the same coord system.',
     & '    For interpolation, file type must be GRIDDED.',
     & '    For copy, file type must be GRIDDED, BOUNDARY, or CUSTOM.',
     &  ' ',
     &'See URL  http://www.baronams.com/products/ioapi/AA.html#tools',
     &' ',
     &'Program copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.',
     &'and (C) 2002-2007 Baron Advanced Meteorological Systems, LLC',
     &'Released under Version 2 of the GNU General Public License.',
     &'See enclosed GPL.txt, or URL',
     &'http://www.gnu.org/copyleft/gpl.html',
     &' ',
     &'Comments and questions are welcome and can be sent to',
     &' ',
     &'    Carlie J. Coats, Jr.    coats@baronams.com',
     &'    Baron Advanced Meteorological Systems, LLC.',
     &'    1009  Capability Drive, Suite 312, Box # 4',
     &'    Raleigh, NC 27606',
     &' ',
     &'Program version: ',
     &PROGVER,
     &'Program release tag: $Name$',
     &' '

        IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
            CALL M3EXIT( 'GINTERP', 0, 0,
     &                   'Program terminated at user request', 2 )
        END IF


C...............  Open and get description for optional synch file

        CALL GETSTR( 'Enter name for input synch file, or "NONE"',
     &               'MCPL_SYNCH_G1', SNAME )

        IF ( SNAME .EQ. 'NONE ' ) THEN

            SFLAG = .FALSE.

        ELSE

            IF ( .NOT. OPEN3( SNAME, FSREAD3, 'GINTERP' ) ) THEN
                MESG = 'Could not open ' // SNAME
                CALL M3EXIT( 'GINTERP', 0, 0, MESG, 2 )
            END IF

            IF ( .NOT. DESC3( SNAME ) ) THEN
                MESG = 'Could not get file description for ' // SNAME
                CALL M3EXIT( 'GINTERP', 0, 0, MESG, 2 )
            END IF

            NCOLS2 = NCOLS3D
            NROWS2 = NROWS3D
            SVBLE  = VNAME3D( 1 )
            SFLAG  = .TRUE.

        END IF


C...............  Open and get description for input data file

        CALL GETSTR( 'Enter name for input data file',
     &               'CHEM_CONC_3D_G1', FNAME )

        IF ( .NOT. OPEN3( FNAME, FSREAD3, 'GINTERP' ) ) THEN
            MESG = 'Could not open ' // FNAME
            CALL M3EXIT( 'GINTERP', 0, 0, MESG, 2 )
        END IF

        IF ( .NOT. DESC3( FNAME ) ) THEN
            MESG = 'Could not get file description for ' // FNAME
            CALL M3EXIT( 'GINTERP', 0, 0, MESG, 2 )
        END IF

        NCOLS1 = NCOLS3D
        NROWS1 = NROWS3D
        NLAYS1 = NLAYS3D
        GDTYP1 = GDTYP3D
        P_ALP1 = P_ALP3D
        P_BET1 = P_BET3D
        P_GAM1 = P_GAM3D
        XCENT1 = XCENT3D
        YCENT1 = YCENT3D
        XORIG1 = XORIG3D
        YORIG1 = YORIG3D
        XCELL1 = XCELL3D
        YCELL1 = YCELL3D
        XFINL1 = XORIG1 + DBLE( NCOLS1 ) * XCELL1
        YFINL1 = YORIG1 + DBLE( NROWS1 ) * YCELL1


C...............  Get output grid description, time step sequence

        CALL GETSTR( 'Enter GRIDDESC name for output grid, or "SAME"',
     &               'RTIME02T15_CRO', GNAME )

        JDATE = GETNUM( SDATE3D, 9999999, SDATE3D,
     &                  'Enter STARTING DATE for time step sequence' )

        JTIME = GETNUM( 0, 9999999, STIME3D,
     &                  'Enter STARTING TIME for time step sequence' )

        TSTEP = GETNUM( TSTEP3D, 9999999, TSTEP3D,
     &                  'Enter   TIME STEP   for time step sequence' )

        NRECS = GETNUM( 1, 9999999, MXREC3D,
     &                  'Enter     NRECS     for time step sequence' )

        SDATE3D = JDATE
        STIME3D = JTIME
        TSTEP3D = TSTEP

C...............  Setup for mode of operation:  copy or interpolate:

        IF ( ( GNAME .EQ. 'SAME' ) .OR.
     &       ( GNAME .EQ. 'same' ) ) THEN	!  set up for copy
	
	    IFLAG = .FALSE.
	
            IF ( FTYPE3D .EQ. GRDDED3 ) THEN
        	SIZE = NCOLS3D * NROWS3D * NLAYS3D
            ELSE IF ( FTYPE3D .EQ. BNDARY3 ) THEN
        	SIZE = NCOLS3D * NROWS3D * NLAYS3D
            ELSE IF ( FTYPE3D .EQ. CUSTOM3 ) THEN
        	SIZE = NCOLS3D * NROWS3D * NLAYS3D
            ELSE
        	MESG = 'Cannot copy--' //
     &                 'file type not GRIDDED, BOUNDARY, or CUSTOM'
        	CALL M3EXIT( 'MERGEGRID', 0, 0, MESG, 2 )
        	EFLAG = .TRUE.
            END IF

            ALLOCATE( COPYBUF( SIZE ), STAT = STATUS )

            IF ( STATUS .NE. 0 ) THEN
        	WRITE( MESG, '( A, I10)' )
     &               'Buffer allocation failed:  STAT=', STATUS
        	CALL M3EXIT( 'MERGEGRID', 0, 0, MESG, 2 )
            END IF

	ELSE	!  set up for interpolate:
	
	    IFLAG = .TRUE.
	
	    IF ( .NOT. DSCGRID( GNAME, CNAME, GDTYP3D,
     &                  P_ALP3D, P_BET3D,P_GAM3D, XCENT3D, YCENT3D,
     &                  XORIG3D, YORIG3D, XCELL3D, YCELL3D,
     &                  NCOLS3D, NROWS3D, NTHIK3D ) ) THEN

        	MESG   = '"' // TRIM( GNAME ) //
     &                   '" not found in GRIDDESC file'
        	CALL M3EXIT( 'UTMMERGE', 0, 0, MESG, 2 )

            END IF

            GDNAM3D = GNAME
            EFLAG   = .FALSE.

            IF ( FTYPE3D .NE. GRDDED3 ) THEN
        	MESG = 'File type not GRIDDED--cannot interpolate'
        	CALL M3MSG2( MESG )
        	EFLAG = .TRUE.
            END IF

            IF ( GDTYP1 .NE. GDTYP3D ) THEN
        	MESG = 'Coordinate system type mismatch'
        	CALL M3MSG2( MESG )
        	EFLAG = .TRUE.
            END IF

            IF ( DBLERR( P_ALP1, P_ALP3D ) ) THEN
        	MESG = 'P_ALP mismatch'
        	CALL M3MSG2( MESG )
        	EFLAG = .TRUE.
            END IF

            IF ( DBLERR( P_BET1, P_BET3D ) ) THEN
        	MESG = 'P_BET mismatch'
        	CALL M3MSG2( MESG )
        	EFLAG = .TRUE.
            END IF

            IF ( DBLERR( P_GAM1, P_GAM3D ) ) THEN
        	MESG = 'P_GAM mismatch'
        	CALL M3MSG2( MESG )
        	EFLAG = .TRUE.
            END IF

            IF ( DBLERR( XCENT1, XCENT3D ) ) THEN
        	MESG = 'XCENT mismatch'
        	CALL M3MSG2( MESG )
        	EFLAG = .TRUE.
            END IF

            IF ( DBLERR( YCENT1, YCENT3D ) ) THEN
        	MESG = 'YCENT mismatch'
        	CALL M3MSG2( MESG )
        	EFLAG = .TRUE.
            END IF

            IF ( EFLAG ) THEN
        	MESG = 'Bad setup for this run'
        	CALL M3EXIT( 'MERGEGRID', 0, 0, MESG, 2 )
            END IF


C...............  Allocate buffers; compute re-gridding matrix

            ALLOCATE( INBUF ( NCOLS1,  NROWS1,  NLAYS3D ),
     &                OUTBUF( NCOLS3D, NROWS3D, NLAYS3D ),
     &                SBUF  ( NCOLS2,  NROWS2 ),
     &                XBUF  ( NCOLS3D, NROWS3D ),
     &                YBUF  ( NCOLS3D, NROWS3D ),
     &                IBUF  ( 4, NCOLS3D*NROWS3D ),
     &                CBUF  ( 4, NCOLS3D*NROWS3D ),
     &                STAT = STATUS )

            IF ( STATUS .NE. 0 ) THEN
        	WRITE( MESG, '( A, I10)' )
     &               'Buffer allocation failed:  STAT=', STATUS
        	CALL M3EXIT( 'MERGEGRID', 0, 0, MESG, 2 )
            END IF

            X0 = XORIG3D - 0.5D0 * XCELL3D
            Y0 = YORIG3D - 0.5D0 * YCELL3D

            DO  R = 1, NROWS3D
            DO  C = 1, NCOLS3D
        	XBUF( C,R ) = X0 + DBLE( C ) * XCELL3D
        	YBUF( C,R ) = Y0 + DBLE( R ) * YCELL3D
            END DO
            END DO

            CALL UNGRIDB( NCOLS1, NROWS1, XORIG1, YORIG1,
     &                    XCELL1, YCELL1,
     &                    NCOLS3D*NROWS3D, XBUF, YBUF, IBUF, CBUF )

        END IF	!  if gname = "SAME", or not


C...............  Open output file

        CALL GETSTR( 'Enter name for output data file',
     &               'CHEM_INIT_3D_G4', ONAME )

        IF ( .NOT. OPEN3( ONAME, FSUNKN3, 'GINTERP' ) ) THEN
            MESG = 'Could not open ' // ONAME
            CALL M3EXIT( 'GINTERP', 0, 0, MESG, 2 )
        END IF


C...............  Process output time step sequence

        DO  N = 1, NRECS

            IF ( SFLAG ) THEN
                IF ( .NOT. CHECK3( SNAME, SVBLE, JDATE, JTIME ) ) THEN
                    MESG = 'Failure checking variable "' //
     &                     TRIM( SVBLE ) // '" from synch file "' //
     &                     TRIM( SNAME ) // '"'
                    CALL M3EXIT( 'GINTERP', 0, 0, MESG, 2 )
                END IF
            END IF

            WRITE( MESG, '( A, I7.7, A, I6.6 )' )
     &          'Processing  ', JDATE, ':', JTIME

            CALL M3MSG2( ' ' )
            CALL M3MSG2( MESG )

            IF ( IFLAG ) THEN	!  bilin-interpolate vs. copy

		DO  V = 1, NVARS3D

                    IF ( .NOT. READ3( FNAME, VNAME3D( V ), ALLAYS3,
     &                                JDATE, JTIME, INBUF ) ) THEN
                	MESG = 'Failure reading variable "' //
     &                         TRIM( VNAME3D( V ) )
     &                         // '" from file "' //
     &                         TRIM( FNAME ) // '"'
                	CALL M3EXIT( 'GINTERP', 0, 0, MESG, 2 )
                    END IF

                    CALL BILIN( NCOLS1  * NROWS1,
     &                          NCOLS3D * NROWS3D, NLAYS3D,
     &                          IBUF, CBUF, INBUF, OUTBUF )

                    IF ( .NOT.WRITE3( ONAME, VNAME3D( V ),
     &                                JDATE, JTIME, OUTBUF ) ) THEN
                	MESG = 'Failure writing variable "' //
     &                         TRIM( VNAME3D( V ) ) // '" to file "' //
     &                         TRIM( ONAME ) // '"'
                	CALL M3EXIT( 'GINTERP', 0, 0, MESG, 2 )
                    END IF

        	END DO      !  end loop on variables

	    ELSE	!  else no interpolation:  copy only.

		DO  V = 1, NVARS3D

                    IF ( .NOT. READ3( FNAME, VNAME3D( V ), ALLAYS3,
     &                                JDATE, JTIME, COPYBUF ) ) THEN
                	MESG = 'Failure reading variable "' //
     &                         TRIM( VNAME3D( V ) ) //
     &                         '" from file "' //
     &                         TRIM( FNAME ) // '"'
                	CALL M3EXIT( 'GINTERP', 0, 0, MESG, 2 )
                    END IF

                    IF ( .NOT.WRITE3( ONAME, VNAME3D( V ),
     &                                JDATE, JTIME, COPYBUF ) ) THEN
                	MESG = 'Failure writing variable "' //
     &                         TRIM( VNAME3D( V ) ) // '" to file "' //
     &                         TRIM( ONAME ) // '"'
                	CALL M3EXIT( 'GINTERP', 0, 0, MESG, 2 )
                    END IF

        	END DO      !  end loop on variables

	    END IF	!  if iflag, or not

            CALL NEXTIME( JDATE, JTIME, TSTEP )

        END DO          !  end loop on output time steps


C...............  Successful completion

        CALL M3EXIT( 'GINTERP', 0, 0,
     &               'Successful completion of program GINTERP', 0 )

        END

