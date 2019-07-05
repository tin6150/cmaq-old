
        PROGRAM M3COMBO

C***********************************************************************
C Version "@(#)$Header$"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2002-2005 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  111
C
C  DESCRIPTION:
C       For a given input file, define a new output file with variables
C	that are linear combinations of the variables in the input file,
C	with specified coefficients.
C       For each time step in the specified time step sequence,
C       reads all variables from the specified input file, computes
C       the specified linear combinations, and writes them to the
C       specified output file.
C
C  PRECONDITIONS REQUIRED:
C       setenv  <logical name> <physical (path) name> for the input and
C                                                     output files.
C	setenv  COMBO_VBLES <comma-delimited list of variable-names>
C	For each combo-variable,
C		setenv  <name>_VBLES  <comma-delimited list of input vbles>
C		setenv  <name>_COEFS  <comma-delimited list of coefficients>
C       Specified time step sequence is valid for the input file.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       I/O API
C
C  REVISION  HISTORY:
C       Prototype 8/2001 by Carlie J. Coats, Jr., MCNC Environmental Programs
C       Version  11/2001 by CJC for I/O API Version 2
C       Version   7/2004 by CJC:  add offsets B_i as option.
C       Version   7/2004 bug-fix from M. Talat Odman, Ga. Tech.
C       Version   6/2005 by CJC:  improved default for NRECS
C***********************************************************************

      IMPLICIT NONE


C...........   INCLUDES:

      INCLUDE 'PARMS3.EXT'      ! I/O API constants
      INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure
      INCLUDE 'IODECL3.EXT'     ! I/O API function declarations


C...........   EXTERNAL FUNCTIONS and their descriptions:

        CHARACTER*24    DT2STR
        INTEGER         CURREC, GETNUM, INDEX1, SEC2TIME, TIME2SEC
        LOGICAL         GETYN, STRLIST, REALIST
        CHARACTER*16    PROMPTMFILE
        REAL            ENVREAL

        EXTERNAL    CURREC,DT2STR, ENVREAL, GETNUM, GETYN, INDEX1,
     &              PROMPTMFILE, REALIST, STRLIST,SEC2TIME, TIME2SEC

     
C...........   PARAMETERS and their descriptions:

        CHARACTER*80    PROGVER
        DATA PROGVER /
     &  '$Id$'
     &  /


C...........   LOCAL VARIABLES and their descriptions:

        CHARACTER*16    FNAME   !  input data  file logical name
        CHARACTER*16    ONAME   !  output data file logical name

        INTEGER         SIZE    ! grid volume, for copy
        INTEGER		NVARS	! number of output variables
        INTEGER		NINP	! number of input variables
        CHARACTER*16    VNAME( MXVARS3 )
        CHARACTER*16    UNITS( MXVARS3 )
        INTEGER		NCOEF( MXVARS3 )
        CHARACTER*16    INAME( MXVARS3, MXVARS3 )
        CHARACTER*16    CNAME( MXVARS3 )
        INTEGER		INDX( MXVARS3, MXVARS3 )
        REAL		COEF( MXVARS3, MXVARS3 )
        REAL		OFFS( MXVARS3 )

        LOGICAL         EFLAG

        INTEGER         JDATE, JTIME, TSTEP
        INTEGER         EDATE, ETIME, TSECS, NRECS

        INTEGER         K, M, N, I, J, V
        REAL		C

        CHARACTER*24    NAMBUF, CHRBUF, OFFBUF, STRBUF
        CHARACTER*256   MESG, SCRBUF

        INTEGER         LDEV        !  log-device
        INTEGER         STATUS      !  allocation-status

        REAL,    ALLOCATABLE::   INBUF( :, : )
        REAL,    ALLOCATABLE::   OUTBUF( : )


C***********************************************************************
C   begin body of program M3COMBO

        LDEV = INIT3()

        WRITE( *, '( 5X, A )' )
     &' ',
     &'Program M3COMBO to compute linear combinations',
     &' ',
     &'      Y_i = SUM A_ij*Xj + B_i',
     &' ',
     &'of variables, for specified coefficients {A_ij} and offsets',
     &'{B_i}, from a specified input file, for a specified timestep',
     &'sequence, and then write these to a specified output file.',
     &' ',
     &'THE PROGRAM WILL PROMPT YOU for the logical names of the input',
     &'and output files, and for the time step sequence.Default ',
     &'responses are indicated in square brackets [LIKE THIS], and may',
     &'be accepted by hitting the RETURN key.',
     &' ',
     &'PRECONDITIONS REQUIRED:',
     &' ',
     &'    Time step sequence is valid for the input file.',
     &'    File type must be GRIDDED, BOUNDARY, or CUSTOM',
     &'    Input variables are all of type REAL',
     &' ',
     &'    setenv <input  file>    <path-name>',
     &'    setenv <output file>    <path-name>',
     &'    setenv COMBO_VBLES      <list of names for the ',
     &'                             output variables>',
     &'    setenv COMBO_UNITS      <units for the ',
     &'                              output variables>',
     &'    For each output variable <name>,',
     &'		setenv  <name>_VBLES  <list of input variable names>',
     &'		setenv  <name>_COEFS  <list of REAL coefficients>',
     &'		setenv  <name>_OFFSET <optional REAL offsets>',
     &' ',
     &'See URL  http://www.baronams.com/products/ioapi/AA.html#tools',
     &' ',
     &'Program copyright (C) 2001-2002 MCNC and Carlie J. Coats, Jr.,',
     &'and (C) 2002-2005 Baron Advanced Meteorological Systems, LLC.',
     &'Released under Version 2 of the GNU General Public License.',
     &'See URL http://www.gnu.org/copyleft/gpl.html or enclosed ',
     &'GPL.txt.',
     &'Comments and questions are welcome and can be sent to',
     &' ',
     &'    coats@baronams.com',
     &' ',
     &'    Carlie J. Coats, Jr.',
     &'    Baron Advanced Meteorological Systems, LLC.',
     &'    920 Main Campus Drive, Suite 101',
     &'    Raleigh, NC 27606',
     &' ',
     &' ',
     &'Program version: ' // PROGVER, 
     &' ',
     &'Program release tag: $Name$', 
     &' '

        IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
            MESG = 'Program terminated at user request'
            CALL M3EXIT( 'M3COMBO', 0, 0, MESG, 2 )
        END IF


C...............  Open and get description for input data file

        FNAME =  PROMPTMFILE( 'Enter name for input data file',
     &                        FSREAD3, 'CHEM_CONC_3D', 'M3COMBO' )

        IF ( .NOT. DESC3( FNAME ) ) THEN
            MESG = 'Could not get file description for ' // FNAME
            CALL M3EXIT( 'M3COMBO', 0, 0, MESG, 2 )
        ELSE IF ( FTYPE3D .EQ. GRDDED3 ) THEN
            SIZE = NCOLS3D * NROWS3D * NLAYS3D
        ELSE IF ( FTYPE3D .EQ. BNDARY3 ) THEN
            SIZE = 2 * NLAYS3D * ABS( NTHIK3D )
     &               * ( NCOLS3D + NROWS3D + 2 * NTHIK3D )
        ELSE IF ( FTYPE3D .EQ. CUSTOM3 ) THEN
            SIZE = NCOLS3D * NLAYS3D
        ELSE
            WRITE( MESG, '( A, I10 )' ) 
     &            'Unsupported input file type', FTYPE3D
            CALL M3EXIT( 'M3COMBO', 0, 0, MESG, 2 )
        END IF
        
        JDATE = GETNUM( SDATE3D, 9999999, SDATE3D, 
     &                  'Enter STARTING DATE for time step sequence' )

        JTIME = GETNUM( 0, 9999999, STIME3D, 
     &                  'Enter STARTING TIME for time step sequence' )

        TSTEP = GETNUM( TSTEP3D, 9999999, TSTEP3D, 
     &                  'Enter   TIME STEP   for time step sequence' )

        EDATE = SDATE3D
        ETIME = STIME3D
        TSECS = ( MXREC3D - 1 ) * TIME2SEC( TSTEP3D )
        CALL NEXTIME( EDATE, ETIME, SEC2TIME( TSECS ) )
        N  =  CURREC( EDATE, ETIME, JDATE, JTIME, TSTEP, I, J )
        
        NRECS = GETNUM( 1, 9999999, N, 
     &                  'Enter     NRECS     for time step sequence' )

        EFLAG = .FALSE.

        IF ( .NOT. STRLIST( 'COMBO_VBLES', 
     &                      'list of output-variable names',
     &                       MXVARS3, NVARS, VNAME ) ) THEN
            MESG = 'Bad list of output variable names'
            CALL M3EXIT( 'M3COMBO', 0, 0, MESG, 2 )
        ELSE IF ( .NOT. STRLIST( 'COMBO_UNITS', 
     &                      'list of output-variable units',
     &                       MXVARS3, N, UNITS ) ) THEN
            MESG = 'Bad list of output variable units'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        ELSE IF ( N .NE. NVARS ) THEN
            MESG = 'Mismatched count:  vbles and units'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        NINP  = 0

	DO  N = 1, NVARS

            NAMBUF = TRIM( VNAME( N ) ) // '_VBLES'
            CHRBUF = TRIM( VNAME( N ) ) // '_COEFS'
            OFFBUF = TRIM( VNAME( N ) ) // '_OFFSET'

            IF ( .NOT. STRLIST( NAMBUF, 
     &           'input-variables to construct ' // VNAME( N ),
     &           MXVARS3, NCOEF( N ), INAME( 1,N ) ) ) THEN

                MESG = 'Bad list of input names for '// VNAME( N )
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.

            ELSE IF ( .NOT. REALIST( CHRBUF, 
     &           'coefficients to construct ' // VNAME( N ),
     &           MXVARS3, M, COEF( 1,N ) ) ) THEN

                MESG = 'Bad list of coefficients for '// VNAME( N )
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.

            ELSE IF ( M .NE.  NCOEF( N ) ) THEN

                MESG = 'Mismatched count:  vbles and coeffs for '// 
     &                 VNAME( N )
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.

            ELSE

                DO  M = 1, NCOEF( N )

                    I = INDEX1( INAME( M,N ), NVARS3D,  VNAME3D )

                    IF ( I .LE. 0 ) THEN
                        MESG = 'Variable "' // TRIM( INAME( M,N ) ) //
     &                         '" not found in file "' //
     &                         TRIM( FNAME ) // '"'
                       CALL M3MSG2( MESG )
                       EFLAG = .TRUE.
                       CYCLE
                   ELSE IF ( VTYPE3D( I ) .NE. M3REAL ) THEN
                        MESG = 'Variable "' // TRIM( INAME( M,N ) ) //
     &                         '" not of type REAL'
                       CALL M3MSG2( MESG )
                       EFLAG = .TRUE.
                       CYCLE
                   END IF

                    I = INDEX1( INAME( M,N ), NINP,  CNAME )
                    IF ( I .GT. 0 ) THEN
                        INDX( M,N ) = I
                    ELSE
                        NINP = NINP + 1
                        CNAME( NINP ) = INAME( M,N )
                        INDX( M,N ) = NINP
                    END IF

                END DO		!  end loop on inputs for this output vble

            END IF	!  if bad strlist, bad realist, etc., or not

            OFFS( N ) = ENVREAL( OFFBUF, 'Offset for linear combo',
     &                           0.0, I )
            IF ( I .GT. 0 ) THEN
                MESG = 'Bad offset for '// VNAME( N )
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF
            
           
        END DO		!  end loop on output variables
        
        IF ( EFLAG ) THEN
            MESG = 'Bad setup for program'
            CALL M3EXIT( 'M3COMBO', 0, 0, MESG, 2 )
        END IF


C...............  Create output file, borrowing most of file
C...............  description from FNAME:

        SDATE3D = JDATE
        STIME3D = JTIME
        TSTEP3D = TSTEP

	NVARS3D = NVARS

        DO  V = 1, NVARS
            VNAME3D( V ) = VNAME( V )
            UNITS3D( V ) = UNITS( V )
            VTYPE3D( V ) = M3REAL
            SCRBUF = 'Linear combination of '
            DO  M = 1, NCOEF( V )
                MESG = TRIM( SCRBUF )//' "'//TRIM( INAME( M,V ) )//'"'
                SCRBUF = MESG
            END DO
            VDESC3D( V ) = SCRBUF
        END DO	! end loop on output variables

        ONAME =  PROMPTMFILE( 'Enter name for output combo file',
     &                        FSUNKN3, 'CHEM_COMBO_3D', 'M3COMBO' )


C...............  Allocate buffers:

            ALLOCATE( INBUF ( SIZE, NINP ),  
     &                OUTBUF( SIZE )      ,  STAT = STATUS )

            IF ( STATUS .NE. 0 ) THEN
        	WRITE( MESG, '( A, I10)' )  
     &               'Buffer allocation failed:  STAT=', STATUS
        	CALL M3EXIT( 'M3COMBO', 0, 0, MESG, 2 )
            END IF


C...............  Process the output time step sequence

        DO  N = 1, NRECS

            DO V = 1, NINP

                IF ( .NOT.READ3( FNAME, CNAME( V ), ALLAYS3, 
     &                           JDATE, JTIME, INBUF( 1,V ) ) ) THEN
                    MESG = 'Failure reading variable "' // 
     &                         TRIM( CNAME( V ) ) //
     &                         '" from file "' //
     &                         TRIM( FNAME ) // '"'
                    CALL M3MSG2( MESG )
                    EFLAG = .TRUE.
                END IF

            END DO	!  end loop on input variables

            IF ( EFLAG ) THEN
        	CALL M3EXIT( 'M3COMBO', 0, 0, 'Input error', 2 )
            END IF

            DO  V = 1, NVARS

                I = INDX( 1,V )
                C = COEF( 1,V )
                DO  K = 1, SIZE
                    OUTBUF( K ) = C * INBUF( K,I ) + OFFS( V )
                END DO

                DO  M = 2, NCOEF( V )
                    I = INDX( M,V )
                    C = COEF( M,V )
                    DO  K = 1, SIZE
                        OUTBUF( K ) = OUTBUF( K ) + C * INBUF( K,I )
                    END DO
                END DO

                IF ( .NOT.WRITE3( ONAME, VNAME( V ),
     &                           JDATE, JTIME, OUTBUF ) ) THEN
                    MESG = 'Failure writing variable "' // 
     &                         TRIM( VNAME( V ) ) //
     &                         '" to file "' //
     &                         TRIM( ONAME ) // '"'
                    CALL M3MSG2( MESG )
                    EFLAG = .TRUE.
                END IF

            END DO	!  end loop on output variables

            IF ( EFLAG ) THEN
        	CALL M3EXIT( 'M3COMBO', 0, 0, 'Output error', 2 )
            END IF

            CALL NEXTIME( JDATE, JTIME, TSTEP )

        END DO          !  end loop on output time steps


C...............  Successful completion

        CALL M3EXIT( 'M3COMBO', 0, 0, 
     &               'Successful completion of program M3COMBO', 0 )

        END


  
        
