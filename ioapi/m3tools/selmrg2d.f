
        PROGRAM SELMRG2D

C***********************************************************************
C Version "@(#)$Header$"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2002-2008 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  119
C
C  DESCRIPTION:
C       Merges selected layers of selected variables from a set of
C       gridded files over a common time period, with optional renaming.
C       Horizontal grid must tbe the same grid for all files.
C
C  PRECONDITIONS REQUIRED:
C       setenv <logical names>  <path-names>
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       I/O API
C
C  REVISION  HISTORY:
C       Prototype 2/2000 by Carlie J. Coats, Jr., NCSC
C       Version  11/2001 by CJC for I/O API Version 2.1
C       Version  11/2005 by CJC:  eliminate unused vbles and functions
C       Version   9/2008 by CJC:  VDESC should be CHARACTER*80 instead of *16
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

      INCLUDE 'PARMS3.EXT'      ! I/O API constants
      INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure
      INCLUDE 'IODECL3.EXT'     ! I/O API function declarations


C...........   PARAMETERS and their descriptions:

        CHARACTER*16, PARAMETER:: FLAGVAR = 'FLAG'

        CHARACTER*80    PROGVER
        DATA PROGVER /
     &  '$Id: selmrg2d.f 334 2008-09-25 14:55:15Z coats@bdsl $'
     &  /

C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER      ENVINT, GETNUM, INDEX1, SEC2TIME, TIME2SEC
        LOGICAL      GETYN
        CHARACTER*16 PROMPTMFILE
        REAL         GETREAL

        EXTERNAL     ENVINT, GETNUM, GETREAL, GETYN, INDEX1,
     &               PROMPTMFILE, SEC2TIME, TIME2SEC


C...........   LOCAL VARIABLES and their descriptions:

        CHARACTER*16    INNAMES( MXFILE3 )
        CHARACTER*16    ANAME, FNAME
        INTEGER         VTYPE( MXVARS3 ) ! variable type:  M3(INT|REAL|DBLE)
        INTEGER         VLAYR( MXVARS3 )
        CHARACTER*16    VFILE( MXVARS3 )
        CHARACTER*16    VNAMI( MXVARS3 )
        CHARACTER*16    VNAMO( MXVARS3 )
        CHARACTER*16    UNITS( MXVARS3 )
        CHARACTER*80    VDESC( MXVARS3 )

        INTEGER         NCOLS
        INTEGER         NROWS
        INTEGER         NLAYS
        INTEGER         NVARS
        REAL*8          P_ALP      ! first, second, third map
        REAL*8          P_BET      ! projection descriptive
        REAL*8          P_GAM      ! parameters.

        REAL*8          XCENT      ! lon for coord-system X=0
        REAL*8          YCENT      ! lat for coord-system Y=0
        REAL*8          XORIG      ! X-coordinate origin of grid (map units)
        REAL*8          YORIG      ! Y-coordinate origin of grid
        REAL*8          XCELL      ! X-coordinate cell dimension
        REAL*8          YCELL      ! Y-coordinate cell dimension

        INTEGER         VGTYP      !  vertical coordinate type (VGSIGP3, ...)
        REAL            VGTOP      !  model-top, for sigma coord types.

        CHARACTER*16    GDNAM      ! grid name             (length NAMLEN3=16)

        INTEGER         FLAYS( MXFILE3 )

        INTEGER         SDATE, STIME, TSTEP, DURATN, NSTEPS
        INTEGER         JDATE, JTIME

        INTEGER         I, N, L, V, F, STEP
        INTEGER         STATUS

        REAL,    ALLOCATABLE::   INBUF( :, : )

        LOGICAL         EFLAG
        CHARACTER*256   MESG
        CHARACTER*256   CBUF

C...........   STATEMENT FUNCTION:  REAL*8 "definitely unequal"

        LOGICAL         DBLERR
        REAL*8          P, Q

        DBLERR( P, Q ) =
     &      ( (P - Q)**2  .GT.  1.0E-10*( P*P + Q*Q + 1.0E-5 ) )


C***********************************************************************
C   begin body of program SELMRG2D

        I = INIT3()
        WRITE( *,92000 )
     &' ',
     &'Program SELMRG2D to merge selected layers of selected ',
     &'variables from a set of gridded files over a commmon grid',
     &'and time period.',
     &' ',
     &'THE PROGRAM WILL PROMPT YOU for the logical names of the input',
     &'input files and the output file, the variables and layers to',
     &'extract, the names by which they should be called in the ',
     &'output file, and the time step sequence to be processed.',
     &'Default responses are indicated in square brackets',
     &'[LIKE THIS], and may be accepted by hitting the RETURN key.',
     &' ',
     &'PRECONDITIONS REQUIRED:',
     &' ',
     &'    setenv <first input name>    <path-names>',
     &'    ...',
     &'    setenv <last  input name>    <path-names>',
     &'    setenv <output name>         <path-names>',
     &' ',
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

        IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
            CALL M3EXIT( 'SELMRG2D', 0, 0,
     &                   'Program terminated at user request', 2 )
        END IF

        DO  F = 1, 9
            WRITE( INNAMES( F ), '( A, I1 )' ) 'INFILE', F
        END DO

        DO  F = 10, MIN( MXFILE3, 99 )
            WRITE( INNAMES( F ), '( A, I2 )' ) 'INFILE', F
        END DO

        DO  F = 100, MXFILE3
            WRITE( INNAMES( F ), '( A, I3 )' ) 'INFILE', F
        END DO

C...............  Open/Process the first input file

        INNAMES( 1 ) = PROMPTMFILE(  'Enter first input file', FSREAD3,
     &                                INNAMES( 1 ), 'SELMRG2D' )

        IF ( .NOT. DESC3( INNAMES( 1 ) ) ) THEN
            MESG = 'Could not get file description for ' // INNAMES(1)
            CALL M3EXIT( 'SELMRG2D', 0, 0, MESG, 2 )
        END IF

        IF ( FTYPE3D .NE. GRDDED3 ) THEN
            MESG = 'File "' // TRIM( INNAMES(1) ) //
     &             '" not a gridded file'
            CALL M3EXIT( 'SELMRG2D', 0, 0, MESG, 2 )
        END IF

        FLAYS( 1 ) = NLAYS3D
        NCOLS = NCOLS3D
        NROWS = NROWS3D
        NLAYS = NLAYS3D
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
        GDNAM = GDNAM3D

        SDATE  = SDATE3D
        STIME  = STIME3D
        TSTEP  = TSTEP3D
        NSTEPS = MXREC3D

        WRITE( *, '( /5X, A, 120( /5X, I2, 6A, : ) )' )
     &      'Variables in this file are:',
     &      (  V, ':  ',
     &         VNAME3D(V), ' (',
     &         TRIM( UNITS3D(V) ), '): ',
     &         TRIM( VDESC3D(V) ), V=1, NVARS3D )
        WRITE( *,* )
        F = 1
        N = 0
        I = 1
11      CONTINUE

            I = GETNUM( 0, NVARS3D, I,
     &          'Enter # for next variable to extract (0 to quit)' )

            IF ( I .EQ. 0 ) GO TO 12

            N = N + 1
            IF ( NLAYS3D .EQ. 1 ) THEN
                VNAMI(N) = VNAME3D(I)
                ANAME    = VNAME3D(I)
                VLAYR(N) = 1
                VDESC(N) = VDESC3D(I)
            ELSE
                MESG = 'Enter layer to extract from ' // VNAME3D(I)
                VLAYR(N) = GETNUM( 1, NLAYS3D, 1, MESG )
                VNAMI(N) = VNAME3D(I)
                WRITE( CBUF, '( A, I4, A, 2X, A )' )
     &                   'Layer ', VLAYR(N), ':', TRIM( VDESC3D(I) )
                ANAME = CBUF
                VDESC(N) = CBUF
            END IF
            VTYPE(N) = VTYPE3D( I )
            UNITS(N) = UNITS3D( I )
            VFILE(N) = INNAMES( 1 )
            WRITE( CBUF, '( A, I3.3 )' ) TRIM( VNAME3D(I) ), VLAYR(N)
            CALL GETSTR( 'Enter output name for this variable/layer',
     &                   ANAME, VNAMO(N) )

            IF ( N .LT. MXVARS3 ) GO TO 11
            MESG = 'I/O API max number of variables now selected'
            CALL M3MSG2( MESG )
            GO TO 44

12      CONTINUE        !  exit from get-variables loop


C...............  Open/Process the rest of the input data files

22      CONTINUE        !  get rest of the input files

            F = F + 1
            INNAMES( F ) = PROMPTMFILE(
     &                        'Enter next input file, or "NONE"',
     &                        FSREAD3, INNAMES( F ), 'SELMRG2D' )

            IF ( INNAMES( F ) .EQ. 'NONE' ) GO TO 44

            IF ( .NOT. DESC3( INNAMES( F ) ) ) THEN
                MESG = 'Could not get file description for ' //
     &                 INNAMES(F)
                CALL M3EXIT( 'SELMRG2D', 0, 0, MESG, 2 )
            END IF

            IF ( FTYPE3D .NE. GRDDED3 ) THEN
                MESG = 'File "' // TRIM( INNAMES(F) ) //
     &                 '" not a gridded file'
                CALL M3EXIT( 'SELMRG2D', 0, 0, MESG, 2 )
            END IF

            FLAYS( F ) = NLAYS3D

            IF ( NCOLS .NE. NCOLS3D ) THEN
                MESG = 'NCOLS mismatch, file ' // INNAMES(F)
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( NROWS .NE. NROWS3D ) THEN
                MESG = 'NROWS mismatch, file ' // INNAMES(F)
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( P_ALP, P_ALP3D ) ) THEN
                MESG = 'P_ALP mismatch, file ' // INNAMES(F)
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( P_BET, P_BET3D ) ) THEN
                MESG = 'P_BET mismatch, file ' // INNAMES(F)
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( P_GAM, P_GAM3D ) ) THEN
                MESG = 'P_GAM mismatch, file ' // INNAMES(F)
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( XCENT, XCENT3D ) ) THEN
                MESG = 'XCENT mismatch, file ' // INNAMES(F)
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( YCENT, YCENT3D ) ) THEN
                MESG = 'YCENT mismatch, file ' // INNAMES(F)
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( XORIG, XORIG3D ) ) THEN
                MESG = 'XORIG mismatch, file ' // INNAMES(F)
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( YORIG, YORIG3D ) ) THEN
                MESG = 'YORIG mismatch, file ' // INNAMES(F)
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( XCELL, XCELL3D ) ) THEN
                MESG = 'XCELL mismatch, file ' // INNAMES(F)
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( DBLERR( YCELL, YCELL3D ) ) THEN
                MESG = 'YCELL mismatch, file ' // INNAMES(F)
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            WRITE( *, '( /5X, A, 120( /5X, I2, 6A, : ) )' )
     &          'Variables in this file are:',
     &          (  V, ':  ',
     &             VNAME3D(V), ' (',
     &             TRIM( UNITS3D(V) ), '): ',
     &             TRIM( VDESC3D(V) ), V=1, NVARS3D )
            WRITE( *,* )
            I = 1
            L = 1
33          CONTINUE

                I = GETNUM( 0, NVARS3D, I,
     &          'Enter # for next variable to extract (0 to quit)' )

                IF ( I .EQ. 0 ) GO TO 34

                N = N + 1
                IF ( NLAYS3D .EQ. 1 ) THEN
                    VNAMI(N) = VNAME3D(I)
                    ANAME    = VNAME3D(I)
                    VLAYR(N) = 1
                    VDESC(N) = VDESC3D(I)
                ELSE
                    MESG = 'Enter layer to extract from ' // VNAME3D(I)
                    VLAYR(N) = GETNUM( 1, NLAYS3D, 1, MESG )
                    VNAMI(N) = VNAME3D(I)
                    WRITE( CBUF, '( A, I4, A, 2X, A )' )
     &                   'Layer ', VLAYR(N), ':', TRIM( VDESC3D(I) )
                    ANAME = CBUF
                    VDESC(N) = CBUF
                END IF
                VTYPE(N) = VTYPE3D( I )
                UNITS(N) = UNITS3D( I )
                VFILE(N) = INNAMES( F )
                CALL GETSTR(
     &              'Enter output name for this variable/layer',
     &               ANAME, VNAMO(N) )

                IF ( N .LT. MXVARS3 ) GO TO 33
                MESG = 'I/O API max number of variables now selected'
                CALL M3MSG2( MESG )
                GO TO 44

34          CONTINUE        !  exit from get-variables loop

            GO TO  22

44      CONTINUE        !  exit from get-files loop

        NVARS = N

        SDATE = GETNUM( 0, 9999999, SDATE,
     &                  'Enter starting DATE for the run (HHMMSS)' )

        STIME = GETNUM( 0, 235959, STIME,
     &                  'Enter starting TIME (HHMMSS)' )

        TSTEP = GETNUM( 0, 235959, TSTEP,
     &                  'Enter OUTPUT TIME STEP (HHMMSS)' )

        I = SEC2TIME( NSTEPS * TIME2SEC( TSTEP ) )
        DURATN = GETNUM( 0,99999999, I, 'Enter RUN DURATION (HHMMSS)' )


C...............  Build the output file:

        NCOLS3D = NCOLS
        NROWS3D = NROWS
        NLAYS3D = 1
        P_ALP3D = P_ALP
        P_BET3D = P_BET
        P_GAM3D = P_GAM
        XCENT3D = XCENT
        YCENT3D = YCENT
        XORIG3D = XORIG
        YORIG3D = YORIG
        XCELL3D = XCELL
        YCELL3D = YCELL
        VGTYP3D = IMISS3
        VGTOP3D = BADVAL3
        GDNAM3D = GDNAM
        VGLVS3D( 1 ) = BADVAL3
        VGLVS3D( 2 ) = BADVAL3

        NVARS3D = NVARS
        DO  V = 1, NVARS3D
            VNAME3D( V ) = VNAMO( V )
            VTYPE3D( V ) = VTYPE( V )
            UNITS3D( V ) = UNITS( V )
            VDESC3D( V ) = VDESC( V )
        END DO

        FNAME = PROMPTMFILE(  'Enter output file', FSUNKN3,
     &                        'OUTFILE', 'SELMRG2D' )

        ALLOCATE( INBUF ( NCOLS, NROWS ), STAT = STATUS )

        IF ( STATUS .NE. 0 ) THEN
            WRITE( MESG, '( A, I10)' )
     &           'Buffer allocation failed:  STAT=', STATUS
            CALL M3EXIT( 'SELMRG2D', 0, 0, MESG, 2 )
        END IF


C...............  Perform the merge

        NSTEPS = TIME2SEC( DURATN ) / TIME2SEC( TSTEP )
        JDATE  = SDATE
        JTIME  = STIME

        DO  STEP = 1, NSTEPS

            DO  V = 1, NVARS

               IF ( .NOT. READ3( VFILE( V ), VNAMI( V ), VLAYR( V ),
     &                            JDATE, JTIME, INBUF ) ) THEN
                   MESG = 'Could not read "' // TRIM( VNAMI(V) ) //
     &                    '" from "' // TRIM( VFILE( V ) ) // '"'
                   CALL M3EXIT( 'SELMRG2D', JDATE, JTIME, MESG, 2 )
               END IF  !  if read failed

               IF ( .NOT. WRITE3( FNAME, VNAMO( V ),
     &                            JDATE, JTIME, INBUF ) ) THEN
                    MESG = 'Could not write "' // TRIM( VNAMO(V) ) //
     &                     '" to "' // TRIM( FNAME ) // '"'
                    CALL M3EXIT( 'SELMRG2D', JDATE, JTIME, MESG, 2 )
                END IF  !  if write failed

            END DO              !  end loop on variables V for this time step

            CALL NEXTIME( JDATE, JTIME, TSTEP )

        END DO          !  end loop on output time steps


        CALL M3EXIT( 'SELMRG2D', 0, 0,
     &               'Successful completion of program SELMRG2D', 0 )
C      STOP

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx
C...........   Informational (LOG) message formats... 92xxx

92000   FORMAT ( 5X, A )


C...........   Formatted file I/O formats............ 93xxx
C...........   Internal buffering formats............ 94xxx
C...........   Miscellaneous formats................. 95xxx


        END

