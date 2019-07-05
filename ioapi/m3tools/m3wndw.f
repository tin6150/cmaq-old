
        PROGRAM  M3WNDW

C***********************************************************************
C Version "@(#)$Header$"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2001 MCNC and Carlie J. Coats, Jr., and
C (C) 2002-2006 Baron Advanced Meteorological Systems, LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  117
C
C  FUNCTION:
C       Window a subrectangle of the grid from gridded input file
C       specified time period, and write it to the output file.
C
C  PRECONDITIONS REQUIRED:
C       Machine with stack-allocated AUTO local variables (e.g., CRAY)
C       consistency with FORIO:PARMS3.EXT for name and description lengths.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       GETNUM, NEXTIME, STATSTEP, Models-3 I/O.
C
C  REVISION  HISTORY:
C       Prototype 5/1996 by CJC
C       Modified 10/1999 by CJC -- Fortran standards conformance
C       Version  11/2001 by CJC for I/O API Version 2.1
C       Version  10/2006 by CJC -- remove unused FLTERR()
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'  !  I/O parameter definitions
        INCLUDE 'FDESC3.EXT'  !  file header data structures
        INCLUDE 'IODECL3.EXT' !  I/O definitions and declarations


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL       GETYN, DSCGRID
        CHARACTER*16  PROMPTMFILE
        INTEGER       GETNUM, IARGC, INDEX1, SECSDIFF, SEC2TIME, 
     &                TIME2SEC, TRIMLEN
        REAL          GETREAL

        EXTERNAL  DSCGRID, GETNUM,   GETREAL,  GETYN,    INDEX1, 
     &            PROMPTMFILE, SECSDIFF, SEC2TIME, TIME2SEC, TRIMLEN


C...........   PARAMETERS and their descriptions:

        CHARACTER*16	BLANK16
        PARAMETER     ( BLANK16 = ' ' )
        CHARACTER*80 PROGVER
        DATA PROGVER /
     &'$Id:: m3wndw.f 49 2007-07-06 16:20:50Z coats@borel            $'
     &  /


C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         LOGDEV  !  unit number for log file
        INTEGER         ARGCNT  !  number of command-line args, from IARGC()
        CHARACTER*256   ENVBUF  !  value from command line arguments
        CHARACTER*256   MESG    !  message buffer for M3EXIT(), etc.

        CHARACTER*16    INAME   !  logical name of the  input file
        CHARACTER*16    WNAME   !  logical name of the output file
        CHARACTER*16    CNAME   !  output coordinate system name

        INTEGER         NCOLS   ! grid dimensions, from INAME header
        INTEGER         NROWS   ! grid dimensions, from INAME header
        INTEGER         NLAYS   ! grid dimensions, from INAME header
        INTEGER         NVARS   !  number of vbles in WNAME
        INTEGER         SDATE   !  starting date, from user
        INTEGER         STIME   !  starting time, from user
        INTEGER         JDATE   !  current date
        INTEGER         JTIME   !  current time
        INTEGER         TSTEP   !  time step, from INAME header
        INTEGER         RUNLEN  !  duration, HHMMSS from user
        INTEGER         NSTEPS  !  duration in TSTEPs
        INTEGER         I       !  scratch variables
        INTEGER         LOCOL   !  window boundary 
        INTEGER         HICOL   !  window boundary 
        INTEGER         LOROW   !  window boundary 
        INTEGER         HIROW   !  window boundary 

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

        LOGICAL         EFLAG

C...........   STATEMENT FUNCTIONS:  REAL, REAL*8 "definitely unequal"
        
        LOGICAL         DBLERR
        REAL*8          P, Q
        REAL            PP, QQ

        DBLERR( P, Q ) = 
     &      ( (P - Q)**2  .GT.  1.0E-10*( P*P + Q*Q + 1.0E-5 ) )


C.........................................................................
C   begin body of program  M3WNDW

        LOGDEV = INIT3()
        WRITE ( *,92000 )
     &  ' ',
     &  'Program M3WNDW to window variables to a specified subgrid',
     &  'from a GRIDDED, Models-3 file for a specified time period',
     &  'write them to another such file.', ' ',
     &  'You need to have set environment variables for the input',
     &  'and output file logical names.  You will be asked to select',
     &  'the window into the grid and time period to be windowed.',
     &  ' ',
     &  'USAGE:  m3wndw [INFILE OUTFILE]',
     &  '(and then answer the prompts). ',
     &  ' ',
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
     &'See URL  http://www.baronams.com/products/ioapi/AA.html#tools',
     &' ',
     &'Program version: ',
     &PROGVER, 
     &'Program release tag: $Name$', 
     &' '

        ARGCNT = IARGC()
 
        IF ( ARGCNT .EQ. 0 ) THEN       !  get names from user

            INAME = PROMPTMFILE( 'Enter logical name for  INPUT FILE',
     &                           FSREAD3, 'INFILE', 'M3WNDW' )

        ELSE IF ( ARGCNT .EQ. 2 ) THEN

            CALL GETARG( 1, ENVBUF )
            INAME = ENVBUF( 1:16 )
            IF ( .NOT. OPEN3( INAME, FSREAD3, 'M3WNDW' ) ) THEN
                MESG = 'Could not open input file "' 
     &                 // INAME( 1:TRIMLEN( INAME ) ) // '"'
                CALL M3EXIT( 'M3WNDW', 0, 0, MESG, 3 )
            END IF

            CALL GETARG( 2, ENVBUF )
            WNAME = ENVBUF( 1:16 )

        ELSE

            CALL M3EXIT( 'M3WNDW', 0, 0, 
     &                   'usage:  m3wndw [INFILE OUTFILE]', 2 )

        END IF

        IF ( .NOT. DESC3( INAME ) ) THEN
            MESG = 'Could not get description of input file ' // INAME
            CALL M3EXIT( 'M3WNDW', 0, 0, MESG, 2 )
        END IF

        IF ( FTYPE3D .NE. GRDDED3 ) THEN
            
            WRITE( MESG, 94010 )
     &          'Input file "' //
     &          INAME( 1:TRIMLEN( INAME ) ) //
     &          '" has type', FTYPE3D, 
     &          '(type GRDDED3==1 required)'
            CALL M3EXIT( 'M3WNDW', 0, 0, MESG, 3 )

        END IF

        NCOLS  = NCOLS3D
        NROWS  = NROWS3D
        NLAYS  = NLAYS3D
        NVARS  = NVARS3D
        SDATE  = SDATE3D
        STIME  = STIME3D
        TSTEP  = TSTEP3D
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


C.......   Get starting date and time, and duration:

        IF ( TSTEP .EQ. 0 ) THEN        !  time-independent file

            SDATE  = 0
            STIME  = 0
            NSTEPS = 1

        ELSE                            !  time-dependent file

            SDATE  = GETNUM( SDATE3D, 9999999, SDATE3D,
     &                  'Enter starting date (YYYYDDD) for run' )
            STIME  = GETNUM( 0, 239999, STIME3D,
     &                  'Enter starting time (HHMMSS) for run' )
            RUNLEN = SEC2TIME( MXREC3D * TIME2SEC( TSTEP3D ) )
            RUNLEN = GETNUM( 0, 999999999, RUNLEN,
     &                  'Enter duration (HHMMSS) for run' )
            NSTEPS = TIME2SEC( TSTEP )
            NSTEPS = ( TIME2SEC( RUNLEN ) + NSTEPS - 1 ) / NSTEPS

        END IF          !  time-independent file, or not


C.......   Build description for the output file, and create accordingly:
C.......   Re-use most of the input-file description.

        SDATE3D = SDATE
        STIME3D = STIME

        WRITE ( MESG,94010 )
     &  'Input file "' // INAME( 1 : TRIMLEN( INAME ) ) // 
     &  '" has grid "' // GDNAM3D( 1 : TRIMLEN( GDNAM3D ) ) // 
     &  '" with', NCOLS3D, 'cols and', NROWS3D, 'rows.'
        WRITE ( *,92000 )
     &  ' ',
     &  MESG( 1 : TRIMLEN( MESG ) ),
     &  'Now enter the window specifications.  These will be of the',
     &  'form GRID-NAME, and (if the grid is not in the current ',
     &  'GRIDDESC, the LOCOL, HICOL, LOROW, HIROW relative to the',
     &  'input file grid, where',
     &  ' ',
     &  '        LOCOL <= col <= HICOL',
     &  '        LOROW <= row <= HIROW',
     &  ' '
        MESG = 'WNDW_' // GDNAM3D
        CALL GETSTR( 'Enter name for windowed grid', 
     &               MESG( 1:16 ),  GDNAM3D )

	IF ( DSCGRID( GDNAM3D, CNAME, GDTYP3D, 
     &                P_ALP3D, P_BET3D,P_GAM3D, XCENT3D, YCENT3D,
     &                XORIG3D, YORIG3D, XCELL3D, YCELL3D, 
     &                NCOLS3D, NROWS3D, NTHIK3D ) ) THEN

            EFLAG = .FALSE.

            IF ( FTYPE3D .NE. GRDDED3 ) THEN
        	MESG = 'File type not GRIDDED--cannot window'
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

            IF ( DBLERR( XCELL1, XCELL3D ) ) THEN
        	MESG = 'XCELL mismatch'
        	CALL M3MSG2( MESG )
        	EFLAG = .TRUE.
            END IF

            IF ( DBLERR( YCELL1, YCELL3D ) ) THEN
        	MESG = 'YCELL mismatch'
        	CALL M3MSG2( MESG )
        	EFLAG = .TRUE.
            END IF

            IF ( EFLAG ) THEN
        	MESG = 'Bad setup for this run'
        	CALL M3EXIT( 'M3WNDW', 0, 0, MESG, 2 )
            END IF

            LOCOL   = 1 + NINT( ( XORIG3D - XORIG1 ) / XCELL3D )
            HICOL   = LOCOL + NCOLS3D - 1
            LOROW   = 1 + NINT( ( YORIG3D - YORIG1 ) / YCELL3D )
            HIROW   = LOROW + NROWS3D - 1

        ELSE

            LOCOL   = GETNUM( 1,     NCOLS3D, 1, 'Enter LOCOL' )
            HICOL   = GETNUM( LOCOL, NCOLS3D, 1, 'Enter HICOL' )
            LOROW   = GETNUM( 1,     NROWS3D, 1, 'Enter LOROW' )
            HIROW   = GETNUM( LOROW, NROWS3D, 1, 'Enter HIROW' )

            XORIG3D = XORIG3D + DBLE( LOCOL - 1 ) * XCELL3D
            YORIG3D = YORIG3D + DBLE( LOROW - 1 ) * YCELL3D
            NCOLS3D = HICOL - LOCOL + 1
            NROWS3D = HIROW - LOROW + 1

        END IF

        IF ( ARGCNT .EQ. 0 ) THEN
            WNAME = PROMPTMFILE( 'Enter logical name for OUTPUT FILE',
     &                           FSUNKN3, 'OUTFILE', 'M3WNDW' )
        ELSE    !  argcnt = 2:
            IF ( .NOT. OPEN3( WNAME, FSUNKN3, 'M3WNDW' ) ) THEN
                MESG = 'Could not open output file ' // WNAME
                CALL M3EXIT( 'M3WNDW', SDATE, STIME, MESG, 2 )
            END IF
        END IF          !  if argcnt zero, or 2


C.......   Process this period in the input file:

        JDATE = SDATE
        JTIME = STIME

        DO  322  I = 1, NSTEPS

            CALL WNDWSTEP( NCOLS, NROWS, NLAYS, NVARS,
     &                     LOCOL, HICOL, LOROW, HIROW,
     &                     JDATE, JTIME,
     &                     INAME, WNAME, LOGDEV )

            CALL NEXTIME( JDATE, JTIME, TSTEP )

322     CONTINUE        !  end loop on time steps


        CALL M3EXIT( 'M3WNDW', 0, 0,
     &               'Program  M3WNDW  completed successfully', 0 )


C..........................   FORMAT STATEMENTS  ....................
C...........   Informational (LOG) message formats... 92xxx

92000   FORMAT ( 5X , A )


C...........   Internal bufferring formats  ......... 94xxx 


94010	FORMAT ( 10 ( A, :, I7, :, 2X ) )

        END

