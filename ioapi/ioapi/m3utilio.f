
        MODULE M3UTILIO

        !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        !! Version "%W% %P% %G% %U% $Id$"
        !! Copyright (c) 2003-2005 Baron Advanced Meteorological Systems and
        !! (c) 2005-2011 Carlie J. Coats, Jr.
        !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
        !! See file "GPL.txt" for conditions of use.
        !!...................................................................
        !!  DESCRIPTION:
        !!      Models-3 I/O API declarations and INTERFACE blocks.
        !!      Additional utility routines:
        !!          SPLITLINE: Split LINE into fields FIELD( N )
        !!          FIXFIELD:  Convert "missing" = "-9" fields and 
        !!                     leading blanks in FIELD to all-zeros
        !!          KEYVAL:    retrieve value of REAL KEY from FDESC3D fields
        !!          KEYSTR:    retrieve value of char-string KEY...
        !!          GRDCHK3:   Checks FDESC3 coordinate and grid description
        !!                     variables against description arguments
        !!          INDEXINT1: Look up integer key in unsorted list
        !!
        !!  PRECONDITIONS:
        !!      Consistency of INTERFACE blocks with I/O API sources.
        !!
        !!  REVISION  HISTORY:
        !!      Prototype 11/2004 by Carlie J. Coats, Jr., BAMS,
        !!      for WRF/sub-grid SMOKE development.
        !!      Version 2/2011 by CJC:  fix mis-spelled CURRSTEP() declaration
        !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

            IMPLICIT NONE

            INCLUDE 'PARMS3.EXT'        !  I/O API PARAMETERs
            INCLUDE 'FDESC3.EXT'        !  I/O API file headers
            INCLUDE 'IODECL3.EXT'       !  I/O API function declarations


            !!........  PUBLIC Routines:

            PUBLIC  SPLITLINE, FIXFIELD,
     &              KEYVAL, KEYSTR, INDEXINT1, LASTTIME, LCM


            !!........  INTERFACE Blocks:
            !!
            !!    BILIN, BMATVEC, CURREC, DAYMON, DMATVEC, DT2STR, ENVDBLE,
            !!    ENVINT, ENVREAL, ENVSTR, ENVYN, FINDC, FIND1,
            !!    FIND2, FIND3, FIND4, FINDR1, FINDR2, FINDR3, FINDR4,
            !!    GCD, GETDATE, GETDBLE, GETDFILE, GETEFILE, GETFFILE, 
            !!    GETMENU, GETNUM, GETREAL, GETSTR, GETYN, HHMMSS,
            !!    INDEX1, IINDEXINT1, NTLIST, ISDST, JSTEP3, JULIAN, LBLANK,
            !!    LOCAT1, LOCAT2, LOCAT3, LOCAT4, LOCATC, LOCATR1, LOCATR2,
            !!    LOCATR3, LOCATR4, M3EXIT, M3FLUSH, M3MESG, M3MSG2, M3PARAG,
            !!    M3PROMPT, M3WARN,
            !!    MMDDYY, NEXTIME, PMATVEC, POLY, PROMPTDFILE, PROMPTFFILE,
            !!    PROMPTMFILE, REALIST, SETENVVAR, SORTIC, SORTI1, SORTI2,
            !!    SORTI3, SORTI4, SORTR1, SORTR2, SORTR3, SORTR4, STR2DBLE,
            !!    STR2INT, STR2REAL, STRLIST, SEC2TIME, SECSDIFF,
            !!    TIME2SEC, UNGRIDB, UNGRIDI, WKDAY, YEAR4, YR2DAY

            INTERFACE
                SUBROUTINE  BILIN( M, N, P, IX, AX, V, C )
                INTEGER         M               ! length of input  vector
                INTEGER         N               ! length of output vector
                INTEGER         P               ! number of layers
                INTEGER         IX( 4,N )       ! index array
                REAL            AX( 4,N )       ! 4-band coeff matrix
                REAL            V( M,P )        ! P-layered input  vector
                REAL            C( N,P )        ! P-layered output vector
                END SUBROUTINE  BILIN
            END INTERFACE

            INTERFACE
                SUBROUTINE  BMATVEC( M, N, P, IX, AX, V, C )
                INTEGER         M               ! length of input  vector
                INTEGER         N               ! length of output vector
                INTEGER         P               ! number of layers
                INTEGER         IX( 4,N )       ! index array
                REAL            AX( 4,N )       ! 4-band coeff matrix
                REAL            V( M,P )        ! P-layered input  vector
                REAL            C( P,N )        ! P-layered output vector
                END SUBROUTINE  BMATVEC
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION CURREC ( JDATE, JTIME, 
     &                                    SDATE, STIME, TSTEP, 
     &                                    CDATE, CTIME )
                INTEGER       SDATE, STIME    !  starting d&t for the sequence
                INTEGER       TSTEP           !  time step for the sequence
                INTEGER       JDATE, JTIME    !  d&t requested
                INTEGER       CDATE, CTIME    !  d&t for timestep of JDATE:JTIME
                END FUNCTION CURREC
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION CURRSTEP ( JDATE, JTIME, 
     &                                      SDATE, STIME, TSTEP, 
     &                                      CDATE, CTIME )
                INTEGER       SDATE, STIME    !  starting d&t for the sequence
                INTEGER       TSTEP           !  time step for the sequence
                INTEGER       JDATE, JTIME    !  d&t requested
                INTEGER       CDATE, CTIME    !  d&t for timestep of JDATE:JTIME
                END FUNCTION CURRSTEP
            END INTERFACE

            INTERFACE
                SUBROUTINE DAYMON( JDATE, MNTH, MDAY )
                INTEGER	JDATE	!  Julian date, format YYYYDDD = 1000*Year + Day
                INTEGER MNTH    !  month (1...12)
                INTEGER MDAY    !  day-of-month (1...28,29,30,31)
                END SUBROUTINE  DAYMON
            END INTERFACE

            INTERFACE
                SUBROUTINE  DMATVEC( N, A, V, C )
                INTEGER		N		! length of input vector
                REAL 		A( N )		! diagonal coeff matrix
                REAL		V( N )		! input  vector
                REAL		C( N )		! output vector
                END SUBROUTINE  DMATVEC
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION  DSCGRID( GNAME, CNAME,
     &              CTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT,
     &              XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, NTHIK )
                CHARACTER*(*) GNAME	!  grid  sys name
                CHARACTER*(*) CNAME	!  coord sys name
                INTEGER       CTYPE	!  coord sys type
                REAL*8        P_ALP	!  first, second, third map
                REAL*8        P_BET	!  projection descriptive
                REAL*8        P_GAM	!  parameters
                REAL*8        XCENT	!  lon for coord-system X=0
                REAL*8        YCENT	!  lat for coord-system Y=0
                REAL*8        XORIG	!  X-coordinate origin of grid (map units)
                REAL*8        YORIG	!  Y-coordinate origin of grid
                REAL*8        XCELL	!  X-coordinate cell dimension
                REAL*8        YCELL	!  Y-coordinate cell dimension
                INTEGER       NCOLS	!  number of grid columns
                INTEGER       NROWS	!  number of grid rows
                INTEGER       NTHIK	!  BOUNDARY:  perimeter thickness (cells)
                END FUNCTION  DSCGRID
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION DSCOORD( CNAME, CTYPE,
     &                                    P_ALP, P_BET, P_GAM,
     &                                    XCENT, YCENT )
                CHARACTER*(*) CNAME	!  coord sys name
                INTEGER       CTYPE	!  coord sys type
                REAL*8        P_ALP	!  first, second, third map
                REAL*8        P_BET	!  projection descriptive
                REAL*8        P_GAM	!  parameters
                REAL*8        XCENT	!  lon for coord-system X=0
                REAL*8        YCENT	!  lat for coord-system Y=0
                END FUNCTIONDSCOORD
            END INTERFACE

            INTERFACE
                CHARACTER(LEN=24) FUNCTION  DT2STR( JDATE , JTIME )
                INTEGER         JDATE   !  Julian date, coded YYYYDDD
                INTEGER         JTIME   !  time, coded HHMMSS
                END FUNCTION  DT2STR
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION ENVINT( LNAME, DESC, DEFAULT, STAT )
                CHARACTER*(*)   LNAME
                CHARACTER*(*)   DESC
                INTEGER         DEFAULT
                INTEGER         STAT
                END FUNCTION ENVINT
            END INTERFACE

            INTERFACE
                REAL FUNCTION ENVREAL( LNAME, DESC, DEFAULT, STAT )
                CHARACTER*(*)   LNAME
                CHARACTER*(*)   DESC
                REAL            DEFAULT
                INTEGER         STAT
                END FUNCTION ENVREAL
            END INTERFACE

            INTERFACE
                REAL*8 FUNCTION ENVDBLE( LNAME, DESC, DEFAULT, STAT )
                CHARACTER*(*)   LNAME
                CHARACTER*(*)   DESC
                REAL*8          DEFAULT
                INTEGER         STAT
                END FUNCTION ENVDBLE
            END INTERFACE                       !  ENVDBLE

            INTERFACE
                SUBROUTINE ENVSTR( LNAME, DESC, DEFAULT, EQNAME, STAT )
                CHARACTER*(*)   LNAME
                CHARACTER*(*)   DESC
                CHARACTER*(*)   DEFAULT
                CHARACTER*(*)   EQNAME
                INTEGER         STAT
                END SUBROUTINE ENVSTR
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION ENVYN( LNAME, DESC, DEFAULT, STAT )
                CHARACTER*(*)   LNAME
                CHARACTER*(*)   DESC
                LOGICAL         DEFAULT
                INTEGER         STAT
                END FUNCTION ENVYN
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION FINDC( KEY, N, LIST )
                CHARACTER*(*)  KEY           !  key
                INTEGER        N             !  table size
                CHARACTER*(*)  LIST( N )     !  table to search for KEY
                END FUNCTION FINDC
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION FIND1( K, N, LIST )
                INTEGER  K             !  first  key
                INTEGER  N             !  table size
                INTEGER  LIST( N )     !  table to search for K
                END FUNCTION FIND1
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION FIND2( K1, K2, N, LIST1, LIST2 )
                INTEGER  K1             !  first  key
                INTEGER  K2             !  second key
                INTEGER  N              !  table size
                INTEGER  LIST1( N )     !  table to search for K1
                INTEGER  LIST2( N )     !  table to search for K2
                END FUNCTION FIND2
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION FIND3( K1, K2, K3,
     &                                  N, LIST1, LIST2, LIST3 )
                INTEGER  K1             !  first  key
                INTEGER  K2             !  second key
                INTEGER  K3             !  third  key
                INTEGER  N              !  table size
                INTEGER  LIST1( N )     !  table to search for K1
                INTEGER  LIST2( N )     !  table to search for K2
                INTEGER  LIST3( N )     !  table to search for K3
                END FUNCTION FIND3
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION FIND4( K1, K2, K3, K4, 
     &                                  N, LIST1, LIST2, LIST3, LIST4 )
                INTEGER  K1             !  first  key
                INTEGER  K2             !  second key
                INTEGER  K3             !  third  key
                INTEGER  K4             !  third  key
                INTEGER  N              !  table size
                INTEGER  LIST1( N )     !  table to search for K1
                INTEGER  LIST2( N )     !  table to search for K2
                INTEGER  LIST3( N )     !  table to search for K3
                INTEGER  LIST4( N )     !  table to search for K4
                END FUNCTION FIND4
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION FINDR1( K, N, LIST )
                REAL     K             !  first  key
                INTEGER  N             !  table size
                REAL     LIST( N )     !  table to search for K
                END FUNCTION FINDR1
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION FINDR2( K1, K2, N, LIST1, LIST2 )
                REAL     K1             !  first  key
                REAL     K2             !  second key
                INTEGER  N              !  table size
                REAL     LIST1( N )     !  table to search for K1
                REAL     LIST2( N )     !  table to search for K2
                END FUNCTION FINDR2
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION FINDR3( K1, K2, K3,
     &                                   N, LIST1, LIST2, LIST3 )
                REAL     K1             !  first  key
                REAL     K2             !  second key
                REAL     K3             !  third  key
                INTEGER  N              !  table size
                REAL     LIST1( N )     !  table to search for K1
                REAL     LIST2( N )     !  table to search for K2
                REAL     LIST3( N )     !  table to search for K3
                END FUNCTION FINDR3
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION FINDR4( K1, K2, K3, K4,
     &                                   N, LIST1, LIST2, LIST3, LIST4 )
                REAL     K1             !  first  key
                REAL     K2             !  second key
                REAL     K3             !  third  key
                REAL     K4             !  third  key
                INTEGER  N              !  table size
                REAL     LIST1( N )     !  table to search for K1
                REAL     LIST2( N )     !  table to search for K2
                REAL     LIST3( N )     !  table to search for K3
                REAL     LIST4( N )     !  table to search for K4
                END FUNCTION FINDR4
            END INTERFACE

            INTERFACE
                INTEGER  FUNCTION GCD( P , Q )
                    INTEGER         P , Q
                END FUNCTION GCD
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION GETDATE ( DEFAULT , PROMPT )
                INTEGER         DEFAULT         !  Default return date, YYYYDDD
                CHARACTER*(*)   PROMPT          !  Prompt for user
                END FUNCTION GETDATE
            END INTERFACE

            INTERFACE
                REAL*8 FUNCTION GETDBLE( LO, HI, DEFAULT, PROMPT )
                REAL*8          LO , HI
                REAL*8          DEFAULT
                CHARACTER*(*)   PROMPT
                END FUNCTION GETDBLE
            END INTERFACE                       ! GETDBLE 

            INTERFACE
                INTEGER FUNCTION GETDFILE( LNAME, RDONLY,
     &                                     FMTFLAG, RECLEN, CALLER )
                CHARACTER*(*) LNAME          !  logical file name
                LOGICAL       RDONLY         !  TRUE iff file is input-only
                LOGICAL       FMTFLAG        !  TRUE iff file should be formatted
                INTEGER       RECLEN         !  record length for direct access
                CHARACTER*(*) CALLER         !  caller-name for logging
                END FUNCTION GETDFILE
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION GETEFILE( LNAME,
     &                                     RDONLY, FMTFLAG, CALLER )
                CHARACTER*(*) LNAME          !  logical file name
                LOGICAL       RDONLY         !  TRUE iff file is input-only
                LOGICAL       FMTFLAG        !  TRUE iff file should be formatted
                CHARACTER*(*) CALLER         !  caller-name for logging
                END FUNCTION GETEFILE
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION GETFFILE( LNAME, RDONLY, FMTFLAG,
     &                                     RECLEN, CALLER )
                CHARACTER*(*) LNAME          !  logical file name
                LOGICAL       RDONLY         !  TRUE iff file is input-only
                LOGICAL       FMTFLAG        !  TRUE iff file should be formatted
                INTEGER       RECLEN         !  record length
                CHARACTER*(*) CALLER         !  caller-name for logging
                END FUNCTION GETFFILE
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION GETMENU( ITEMCNT, DEFAULT,
     &                                    PROMPT, CHOICES )
                INTEGER         ITEMCNT         !  number of choices
                INTEGER         DEFAULT         !  default response
                CHARACTER*(*)   PROMPT          !  prompt string
                CHARACTER*(*)   CHOICES ( * )   !  array of choice strings
                END FUNCTION GETMENU
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION GETNUM ( LO , HI , DEFAULT , PROMPT )
                INTEGER         LO , HI
                INTEGER         DEFAULT
                CHARACTER*(*)   PROMPT
                END FUNCTION GETNUM
            END INTERFACE

            INTERFACE
                REAL   FUNCTION GETREAL ( LO , HI , DEFAULT , PROMPT )
                REAL            LO , HI
                REAL     	DEFAULT
                CHARACTER*(*)   PROMPT
                END FUNCTION GETREAL
            END INTERFACE                       !  GETREAL

            INTERFACE
                SUBROUTINE GETSTR ( PROMPT, DEFAULT, RESPONSE )
                CHARACTER*(*)   PROMPT, DEFAULT, RESPONSE
                END SUBROUTINE GETSTR
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION  GETYN ( PROMPT , DEFAULT )
                CHARACTER*(*)   PROMPT
                LOGICAL         DEFAULT
                END FUNCTION  GETYN
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION GRDCHK3( FNAME, 
     &                                P_ALP, P_BET, P_GAM, XCENT, YCENT,
     &                                XORIG, YORIG, XCELL, YCELL,
     &                                NLAYS, VGTYP, VGTOP, VGLEV )
                CHARACTER(*)    FNAME
                REAL*8          P_ALP      ! first, second, third map
                REAL*8          P_BET      ! projection descriptive
                REAL*8          P_GAM      ! parameters.
                REAL*8          XCENT      ! lon for coord-system X=0
                REAL*8          YCENT      ! lat for coord-system Y=0
                REAL*8          XORIG      ! X-coordinate origin of grid (map units)
                REAL*8          YORIG      ! Y-coordinate origin of grid
                REAL*8          XCELL      ! X-coordinate cell dimension
                REAL*8          YCELL      ! Y-coordinate cell dimension
                INTEGER         NLAYS      ! number of layers
                INTEGER         VGTYP      ! grid type:  1=LAT-LON, 2=UTM, ...
                REAL            VGTOP
                REAL            VGLEV( * )
                END FUNCTION GRDCHK3
            END INTERFACE

            INTERFACE
                SUBROUTINE GTPZ0( CRDIN, INSYS, INZONE, TPARIN, INUNIT,
     &                            INSPH, IPR, JPR, LEMSG, LPARM,
     &                            CRDIO, IOSYS, IOZONE, TPARIO, IOUNIT,
     &                            LN27, LN83, FN27, FN83, LENGTH, IFLG )
                REAL*8 , INTENT( IN ) :: CRDIN(2), TPARIN(15)
                INTEGER, INTENT( IN ) :: INSYS, INZONE, INUNIT, INSPH
                INTEGER, INTENT( IN ) :: IPR, JPR, LEMSG, LPARM, IOUNIT
                INTEGER, INTENT( IN ) :: LN27, LN83, LENGTH
                CHARACTER(LEN=128), INTENT( IN ) :: FN27, FN83
                REAL*8 , INTENT( OUT ) :: CRDIO(2), TPARIO(15)
                INTEGER, INTENT( OUT ) :: IFLG
                END SUBROUTINE GTPZ0
            END INTERFACE

            INTERFACE
                CHARACTER(LEN=10) FUNCTION  HHMMSS( JTIME )
                INTEGER       JTIME   !  Julian time, coded YYYYDDD
                END FUNCTION  HHMMSS 
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION INDEX1( NAME, N, NLIST )
                CHARACTER*(*) NAME        !  Character string being searched for
                INTEGER       N           !  Length of array to be searched
                CHARACTER*(*) NLIST(*)    !  array to be searched
                END FUNCTION INDEX1
            END INTERFACE


            INTERFACE
                INTEGER FUNCTION INDEXINT1( IKEY, N, NLIST )
                INTEGER       IKEY        !  integer being searched for
                INTEGER       N           !  Length of array to be searched
                INTEGER       NLIST(*)    !  array to be searched
                END FUNCTION INDEXINT1
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION INTLIST( ENAME, EDESC,
     &                                    NMAX, NCNT, LIST )
                CHARACTER*(*)   ENAME   !  in:  environment variable for the list
                CHARACTER*(*)   EDESC   !  in:  environment variable description
                INTEGER         NMAX    !  in:  dimension for list
                INTEGER         NCNT    ! out:  actual number of entries in list
                INTEGER         LIST( NMAX )    ! out:  array of values found    
                END FUNCTION INTLIST
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION ISDSTIME( JDATE )
                INTEGER         JDATE   !  Julian date, coded YYYYDDD
                END FUNCTION ISDSTIME
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION JSTEP3( JDATE, JTIME,
     &                                   SDATE, STIME, TSTEP )
                INTEGER         JDATE   !  requested date YYYYDDD
                INTEGER         JTIME   !  requested time HHMMSS
                INTEGER         SDATE   !  starting date  YYYYDDD
                INTEGER         STIME   !  starting time  HHMMSS
                INTEGER         TSTEP   !  time step      H*MMSS
                END FUNCTION JSTEP3
            END INTERFACE

             INTERFACE
                INTEGER FUNCTION JULIAN( YEAR, MNTH, MDAY )
                INTEGER   YEAR            ! year YYYY
                INTEGER   MNTH            ! month 1...12
                INTEGER   MDAY            ! day-of-month 1...28,29,30,31
                END FUNCTION JULIAN
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION LBLANK( STRING )
                CHARACTER*(*), INTENT( IN ) ::   STRING
                END FUNCTION LBLANK
            END INTERFACE

           INTERFACE
                INTEGER FUNCTION LOCAT1( K1, N, LIST1 )
                INTEGER  K1             !  first  key
                INTEGER  N              !  table size
                INTEGER  LIST1( N )     !  table to search for K1
                END FUNCTION LOCAT1
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION LOCAT2( K1, K2, N, LIST1, LIST2 )
                INTEGER  K1             !  first  key
                INTEGER  K2             !  second key
                INTEGER  N              !  table size
                INTEGER  LIST1( N )     !  table to search for K1
                INTEGER  LIST2( N )     !  table to search for K2
                END FUNCTION LOCAT2
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION LOCAT3( K1, K2, K3, N, 
     &                                   LIST1, LIST2, LIST3, LIST4 )
                INTEGER  K1             !  first  key
                INTEGER  K2             !  second key
                INTEGER  K3             !  third  key
                INTEGER  N              !  table size
                INTEGER  LIST1( N )     !  table to search for K1
                INTEGER  LIST2( N )     !  table to search for K2
                INTEGER  LIST3( N )     !  table to search for K3
                END FUNCTION LOCAT3
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION LOCAT4( K1, K2, K3, K4, N, 
     &                                   LIST1, LIST2, LIST3, LIST4 )
                INTEGER  K1             !  first  key
                INTEGER  K2             !  second key
                INTEGER  K3             !  third  key
                INTEGER  K4             !  fourth key
                INTEGER  N              !  table size
                INTEGER  LIST1( N )     !  table to search for K1
                INTEGER  LIST2( N )     !  table to search for K2
                INTEGER  LIST3( N )     !  table to search for K3
                INTEGER  LIST4( N )     !  table to search for K4
                END FUNCTION LOCAT4
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION LOCATC( KEY, N, LIST )
                CHARACTER*(*)   KEY            !  first  key
                INTEGER         N              !  table size
                CHARACTER*(*)   LIST( N )      !  table to search for KEY
                END FUNCTION LOCATC
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION LOCATR1( K1, N, LIST1 )
                REAL     K1             !  first  key
                INTEGER  N              !  table size
                REAL     LIST1( N )     !  table to search for K1
                END FUNCTION LOCATR1
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION LOCATR2( K1, K2, N, LIST1, LIST2 )
                REAL     K1             !  first  key
                REAL     K2             !  second key
                INTEGER  N              !  table size
                REAL     LIST1( N )     !  table to search for K1
                REAL     LIST2( N )     !  table to search for K2
                END FUNCTION LOCATR2
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION LOCATR3( K1, K2, K3, N, 
     &                                    LIST1, LIST2, LIST3 )
                REAL     K1             !  first  key
                REAL     K2             !  second key
                REAL     K3             !  third  key
                INTEGER  N              !  table size
                REAL     LIST1( N )     !  table to search for K1
                REAL     LIST2( N )     !  table to search for K2
                REAL     LIST3( N )     !  table to search for K3
                END FUNCTION LOCATR3
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION LOCATR4( K1, K2, K3, K4, N, 
     &                                    LIST1, LIST2, LIST3, LIST4 )
                REAL     K1             !  first  key
                REAL     K2             !  second key
                REAL     K3             !  third  key
                REAL     K4             !  fourth key
                INTEGER  N              !  table size
                REAL     LIST1( N )     !  table to search for K1
                REAL     LIST2( N )     !  table to search for K2
                REAL     LIST3( N )     !  table to search for K3
                REAL     LIST4( N )     !  table to search for K4
                END FUNCTION LOCATR4
            END INTERFACE

            INTERFACE
                SUBROUTINE  LUSTR( STRING )
                CHARACTER*(*)   STRING
                END SUBROUTINE  LUSTR
            END INTERFACE

            INTERFACE
                SUBROUTINE M3EXIT( CALLER, JDATE, JTIME,
     &                             MSGTXT, EXITSTAT )
                CHARACTER*(*)   CALLER          !  name of the caller
                INTEGER         JDATE, JTIME    !  model date&time for the error
                CHARACTER*(*)   MSGTXT          !  error message
                INTEGER         EXITSTAT        !  exit status for program
                END SUBROUTINE M3EXIT
            END INTERFACE

            INTERFACE
                SUBROUTINE  M3FLUSH( IDEV )
                INTEGER         IDEV
                END SUBROUTINE  M3FLUSH
            END INTERFACE

            INTERFACE
                SUBROUTINE  M3MESG( MESSAGE )
                CHARACTER*(*)   MESSAGE
                END SUBROUTINE  M3MESG
            END INTERFACE

            INTERFACE
                SUBROUTINE  M3MSG2( MESSAGE )
                CHARACTER*(*)   MESSAGE
                END SUBROUTINE  M3MSG2
            END INTERFACE

            INTERFACE
                SUBROUTINE  M3PARAG( NMESG, MSGS )
                INTEGER         NMESG
                CHARACTER*(*)   MSGS( NMESG )
                END SUBROUTINE  M3PARAG
            END INTERFACE

            INTERFACE
                SUBROUTINE  M3PROMPT( MESSAGE, ANSWER, ISTAT )
                CHARACTER*(*)   MESSAGE
                CHARACTER*(*)   ANSWER
                INTEGER         ISTAT
                END SUBROUTINE  M3PROMPT
            END INTERFACE

            INTERFACE
                SUBROUTINE  M3WARN( CALLER, JDATE, JTIME, MSGTXT )
                CHARACTER*(*)   CALLER          !  name of the caller
                INTEGER         JDATE, JTIME    !  model date&time for the error
                CHARACTER*(*)   MSGTXT          !  error message
                END SUBROUTINE  M3WARN
            END INTERFACE

            INTERFACE
                CHARACTER*14 FUNCTION  MMDDYY ( JDATE )
                INTEGER  	       JDATE    !  Julian date, coded YYYYDDD
                END FUNCTION MMDDYY
            END INTERFACE

            INTERFACE
                SUBROUTINE NEXTIME( JDATE , JTIME, DTIME )
                INTEGER         JDATE           !  date (encoded YYYYDDD)
                INTEGER         JTIME           !  time (encoded  HHMMSS)
                INTEGER         DTIME           !  time increment (encoded HHMMSS)
                END SUBROUTINE NEXTIME
            END INTERFACE

            INTERFACE
                SUBROUTINE  PMATVEC( NCOLS, NROWS, NCOFF, N, I, U, V )
                INTEGER         NCOLS           ! length of input vector
                INTEGER         NROWS           ! length of output vector
                INTEGER         NCOFF           ! max number of coefficients
                INTEGER         N( NROWS )      ! # of entries per row
                INTEGER         I( NCOFF )      ! columns list
                REAL            U( NCOLS )      !  input vector
                REAL            V( NROWS )      ! output vector
                END SUBROUTINE  PMATVEC
            END INTERFACE

            INTERFACE
                REAL FUNCTION  POLY( XPT, XPTS, YPTS, NDEG )
                INTEGER         NDEG
                REAL            XPT
                REAL            XPTS ( NDEG + 1 )
                REAL            YPTS ( NDEG + 1 )
                END  FUNCTION  POLY
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION PROMPTDFILE( PROMPT,  RDONLY, FMTTED,
     &                                        RECLEN, DEFAULT, CALLER )
                CHARACTER*(*) PROMPT         !  prompt for user
                LOGICAL       RDONLY         !  TRUE iff file is input-only
                LOGICAL       FMTTED         !  TRUE iff file should be formatted
                INTEGER	      RECLEN         !  record length
                CHARACTER*(*) DEFAULT        !  default logical file name
                CHARACTER*(*) CALLER         !  caller-name for logging messages
                END FUNCTION PROMPTDFILE
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION PROMPTFFILE( PROMPT,  RDONLY, FMTTED,
     &                                        DEFAULT, CALLER )
                CHARACTER*(*) PROMPT         !  prompt for user
                LOGICAL       RDONLY         !  TRUE iff file is input-only
                LOGICAL       FMTTED         !  TRUE iff file should be formatted
                CHARACTER*(*) DEFAULT        !  default logical file name
                CHARACTER*(*) CALLER         !  caller-name for logging messages
                END FUNCTION  PROMPTFFILE
            END INTERFACE

            INTERFACE
                CHARACTER*16  FUNCTION PROMPTMFILE( PROMPT,  FMODE,
     &                                              DEFAULT, CALLER )
                CHARACTER*(*) PROMPT         !  prompt for user
                INTEGER       FMODE          !  file opening-mode
                CHARACTER*(*) DEFAULT        !  default logical file name
                CHARACTER*(*) CALLER         !  caller-name for logging messages
                END FUNCTION  PROMPTMFILE
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION REALIST( ENAME, EDESC,
     &                                    NMAX, NCNT, LIST )
                CHARACTER*(*)   ENAME   !  in:  environment variable for the list
                CHARACTER*(*)   EDESC   !  in:  environment variable description
                INTEGER         NMAX    !  in:  dimension for list
                INTEGER         NCNT    ! out:  actual number of entries in list
                REAL            LIST( NMAX )    ! out:  array of values found    
                END FUNCTION REALIST
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION SETENVVAR( LNAME, VALUE )
                CHARACTER*(*), INTENT( IN ) ::   LNAME
                CHARACTER*(*), INTENT( IN ) ::   VALUE
                END FUNCTION SETENVVAR
            END INTERFACE

            INTERFACE
                SUBROUTINE  SORTIC( N, IND, TBLC )
                INTEGER         N
                INTEGER         IND( N )
                CHARACTER*(*)   TBLC( * )
                END SUBROUTINE  SORTIC
            END INTERFACE

            INTERFACE
                SUBROUTINE  SORTI1( N, IND, TBL1 )
                INTEGER         N
                INTEGER         IND( N )
                INTEGER         TBL1( * )
                END SUBROUTINE  SORTI1
            END INTERFACE

            INTERFACE
                SUBROUTINE  SORTI2( N, IND, TBL1, TBL2 )
                INTEGER         N
                INTEGER         IND( N )
                INTEGER         TBL1( * )
                INTEGER         TBL2( * )
                END SUBROUTINE  SORTI2
            END INTERFACE

            INTERFACE
                SUBROUTINE  SORTI3( N, IND, TBL1, TBL2, TBL3 )
                INTEGER         N
                INTEGER         IND( N )
                INTEGER         TBL1( * )
                INTEGER         TBL2( * )
                INTEGER         TBL3( * )
                END SUBROUTINE  SORTI3
            END INTERFACE

            INTERFACE
                SUBROUTINE  SORTI4( N, IND, TBL1, TBL2, TBL3, TBL4 )
                INTEGER         N
                INTEGER         IND( N )
                INTEGER         TBL1( * )
                INTEGER         TBL2( * )
                INTEGER         TBL3( * )
                INTEGER         TBL4( * )
                END SUBROUTINE  SORTI4
            END INTERFACE

            INTERFACE
                SUBROUTINE  SORTR1( N, IND, TBL1 )
                INTEGER         N
                INTEGER         IND( N )
                REAL            TBL1( * )
                END SUBROUTINE  SORTR1
            END INTERFACE

            INTERFACE
                SUBROUTINE  SORTR2( N, IND, TBL1, TBL2 )
                INTEGER         N
                INTEGER         IND( N )
                REAL            TBL1( * )
                REAL            TBL2( * )
                END SUBROUTINE  SORTR2
            END INTERFACE

            INTERFACE
                SUBROUTINE  SORTR3( N, IND, TBL1, TBL2, TBL3 )
                INTEGER         N
                INTEGER         IND( N )
                REAL            TBL1( * )
                REAL            TBL2( * )
                REAL            TBL3( * )
                END SUBROUTINE  SORTR3
            END INTERFACE

            INTERFACE
                SUBROUTINE  SORTR4( N, IND, TBL1, TBL2, TBL3, TBL4 )
                INTEGER         N
                INTEGER         IND( N )
                REAL            TBL1( * )
                REAL            TBL2( * )
                REAL            TBL3( * )
                REAL            TBL4( * )
                END SUBROUTINE  SORTR4
            END INTERFACE

            INTERFACE
                REAL*8 FUNCTION STR2DBLE( STRING )
                CHARACTER*(*)   STRING
                END FUNCTION STR2DBLE
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION STR2INT( STRING )
                CHARACTER*(*)   STRING
                END FUNCTION STR2INT
            END INTERFACE

            INTERFACE
                REAL FUNCTION STR2REAL( STRING )
                CHARACTER*(*)   STRING
                END FUNCTION STR2REAL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION STRLIST( ENAME, EDESC,
     &                                    NMAX, NCNT, LIST )
                CHARACTER*(*)   ENAME   !  in:  environment variable for the list
                CHARACTER*(*)   EDESC   !  in:  environment variable description
                INTEGER         NMAX    !  in:  dimension for list
                INTEGER         NCNT    ! out:  actual number of entries in list
                CHARACTER*(*)   LIST( NMAX )    ! out:  array of values found    
                END FUNCTION STRLIST
            END INTERFACE

            INTERFACE
                INTEGER  FUNCTION  SEC2TIME ( SECS )
                INTEGER  	SECS
                END FUNCTION  SEC2TIME
            END INTERFACE

            INTERFACE
                INTEGER  FUNCTION  TIME2SEC ( TIME )
                INTEGER  	TIME    !  formatted HHMMSS
                END FUNCTION  TIME2SEC
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION SECSDIFF( ADATE, ATIME, ZDATE, ZTIME )
                INTEGER         ADATE, ATIME
                INTEGER         ZDATE, ZTIME
                END FUNCTION SECSDIFF
            END INTERFACE

            INTERFACE
                SUBROUTINE  UNGRIDB( NCOLS, NROWS, XORIG, YORIG,
     &                               XCELL, YCELL, NPTS, XLOC, YLOC,
     &                               NU, CU )
                INTEGER		NCOLS, NROWS	!  number of grid columns, rows
                REAL*8		XORIG, YORIG	!  X,Y coords of LL grid corner
                REAL*8		XCELL, YCELL	!  X,Y direction cell size
                INTEGER		NPTS	        !  number of (point-source) locations
                REAL		XLOC( NPTS ) 	!  X point coordinates
                REAL		YLOC( NPTS ) 	!  Y point coordinates
                INTEGER		NU( 4,NPTS )    !  single-indexed subscripts into grid
                REAL            CU( 4,NPTS )    !  coefficients
                END SUBROUTINE  UNGRIDB
            END INTERFACE

            INTERFACE
                SUBROUTINE  UNGRIDI( NCOLS, NROWS, XORIG, YORIG,
     &                               XCELL, YCELL, NPTS, XLOC, YLOC,
     &                               NX )
                INTEGER		NCOLS, NROWS	!  number of grid columns, rows
                REAL*8		XORIG, YORIG	!  X,Y coords of LL grid corner
                REAL*8		XCELL, YCELL	!  X,Y direction cell size
                INTEGER		NPTS	        !  number of (point-source) locations
                REAL		XLOC( NPTS ) 	!  X point coordinates
                REAL		YLOC( NPTS ) 	!  Y point coordinates
                INTEGER		  NX( NPTS )    !  single-indexed subscripts into grid
                END SUBROUTINE  UNGRIDI
            END INTERFACE

            INTERFACE
                SUBROUTINE  UPCASE ( BUFFER )
                CHARACTER*(*)   BUFFER
                END SUBROUTINE  UPCASE
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION WKDAY( JDATE )
                INTEGER         JDATE	!  date YYYYDDD = 1000 * YEAR + DAY
                END FUNCTION WKDAY
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION YEAR4 ( YY )
                INTEGER         YY    ! 2 digit year
                END FUNCTION YEAR4
            END INTERFACE

            INTERFACE
                REAL FUNCTION YR2DAY ( YEAR )
                INTEGER         YEAR  ! 4 digit year YYYY
                END FUNCTION YR2DAY
            END INTERFACE


!-=-=-=-=-=-=-=-=-=-=-=-=-  LAMBERT()  Related Interfaces  -=-=-=-=-=-=-=-

            INTERFACE
                LOGICAL FUNCTION LAMBERT( CNAME, A, B, C, X, Y )
                CHARACTER(LEN=*)  CNAME
                REAL           A          !  first secant latitude
                REAL           B          !  second secant latitude.  B > A
                REAL           C          !  central meridian
                REAL           X          !  Lambert easting  in meters
                REAL           Y          !  Lambert northing in meters
                END FUNCTION LAMBERT
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION POLSTE( CNAME, A, B, C, X, Y )
                CHARACTER(LEN=*)  CNAME
                REAL           A          !  first secant latitude
                REAL           B          !  second secant latitude.  B > A
                REAL           C          !  central meridian
                REAL           X          !  POLSTE easting  in meters
                REAL           Y          !  POLSTE northing in meters
                END FUNCTION POLSTE
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION EQMERC( CNAME, A, B, C, X, Y )
                CHARACTER(LEN=*)  CNAME
                REAL           A          !  first secant latitude
                REAL           B          !  second secant latitude.  B > A
                REAL           C          !  central meridian
                REAL           X          !  EQMERC easting  in meters
                REAL           Y          !  EQMERC northing in meters
                END FUNCTION EQMERC
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION TRMERC( CNAME, A, B, C, X, Y )
                CHARACTER(LEN=*)  CNAME
                REAL           A          !  first secant latitude
                REAL           B          !  second secant latitude.  B > A
                REAL           C          !  central meridian
                REAL           X          !  TRMERC easting  in meters
                REAL           Y          !  TRMERC northing in meters
                END FUNCTION TRMERC
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION ALBERS( CNAME, A, B, C, X, Y )
                CHARACTER(LEN=*)  CNAME
                REAL           A          !  first secant latitude
                REAL           B          !  second secant latitude.  B > A
                REAL           C          !  central meridian
                REAL           X          !  Lambert easting  in meters
                REAL           Y          !  Lambert northing in meters
                END FUNCTION ALBERS
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION SETLAM( A, B, C, X, Y )
                REAL           A          !  first secant latitude
                REAL           B          !  second secant latitude.  B > A
                REAL           C          !  central meridian
                REAL           X          !  Lambert easting  in meters
                REAL           Y          !  Lambert northing in meters
                END FUNCTION SETLAM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION SETPOL( A, B, C, X, Y )
                REAL           A          !  first secant latitude
                REAL           B          !  second secant latitude.  B > A
                REAL           C          !  central meridian
                REAL           X          !  Lambert easting  in meters
                REAL           Y          !  Lambert northing in meters
                END FUNCTION SETPOL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION SETEQM( A, B, C, X, Y )
                REAL           A          !  first secant latitude
                REAL           B          !  second secant latitude.  B > A
                REAL           C          !  central meridian
                REAL           X          !  Lambert easting  in meters
                REAL           Y          !  Lambert northing in meters
                END FUNCTION SETEQM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION SETTRM( A, B, C, X, Y )
                REAL           A          !  first secant latitude
                REAL           B          !  second secant latitude.  B > A
                REAL           C          !  central meridian
                REAL           X          !  Lambert easting  in meters
                REAL           Y          !  Lambert northing in meters
                END FUNCTION SETTRM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION SETALB( A, B, C, X, Y )
                REAL           A          !  first secant latitude
                REAL           B          !  second secant latitude.  B > A
                REAL           C          !  central meridian
                REAL           X          !  Lambert easting  in meters
                REAL           Y          !  Lambert northing in meters
                END FUNCTION SETALB
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION LAM2LL( X, Y, LON, LAT )
                REAL           X          !  Lambert easting  in meters
                REAL           Y          !  Lambert northing in meters
                REAL           LON        !  Lambert easting  in meters
                REAL           LAT        !  Lambert northing in meters
                END FUNCTION LAM2LL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION LL2LAM( LON, LAT, X, Y )
                REAL           X          !  Lambert easting  in meters
                REAL           Y          !  Lambert northing in meters
                REAL           LON        !  Lambert easting  in meters
                REAL           LAT        !  Lambert northing in meters
                END FUNCTION LL2LAM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION LAM2UTM( X, Y, Z, U, V )
                REAL           X          !  Lambert easting  in meters
                REAL           Y          !  Lambert northing in meters
                INTEGER        Z          !  UTM zone (1...36)
                REAL           U          !  UTM easting  in meters
                REAL           V          !  UTM northing in meters
                END FUNCTION LAM2UTM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION UTM2LAM( U, V, Z, X, Y )
                REAL           U          !  UTM easting  in meters
                REAL           V          !  UTM northing in meters
                INTEGER        Z          !  UTM zone (1...36)
                REAL           X          !  Lambert easting  in meters
                REAL           Y          !  Lambert northing in meters
                END FUNCTION UTM2LAM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION LAM2POL( X, Y, U, V )
                REAL           X          !  Lambert easting  in meters
                REAL           Y          !  Lambert northing in meters
                REAL           U          !  POL easting  in meters
                REAL           V          !  POL northing in meters
                END FUNCTION LAM2POL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION POL2LAM( U, V, X, Y )
                REAL           U          !  POL easting  in meters
                REAL           V          !  POL northing in meters
                REAL           X          !  Lambert easting  in meters
                REAL           Y          !  Lambert northing in meters
                END FUNCTION POL2LAM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION POL2LL( X, Y, LON, LAT )
                REAL           X          !  POL easting  in meters
                REAL           Y          !  POL northing in meters
                REAL           LON        !  longitude (degrees)
                REAL           LAT        !  latitude  (degrees)
                END FUNCTION POL2LL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION LL2POL( LON, LAT, X, Y )
                REAL           LON        !  longitude (degrees)
                REAL           LAT        !  latitude  (degrees)
                REAL           X          !  POL easting  in meters
                REAL           Y          !  POL northing in meters
                END FUNCTION LL2POL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION POL2UTM( X, Y, Z, U, V )
                REAL           X          !  POL easting  in meters
                REAL           Y          !  POL northing in meters
                INTEGER        Z          !  UTM zone (1...36)
                REAL           U          !  UTM easting  in meters
                REAL           V          !  UTM northing in meters
                END FUNCTION POL2UTM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION UTM2POL( U, V, Z, X, Y )
                REAL           U          !  UTM easting  in meters
                REAL           V          !  UTM northing in meters
                INTEGER        Z          !  UTM zone (1...36)
                REAL           X          !  POL easting  in meters
                REAL           Y          !  POL northing in meters
                END FUNCTION UTM2POL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION TRM2LL( X, Y, LON, LAT )
                REAL           X          !  TRM easting  in meters
                REAL           Y          !  TRM northing in meters
                REAL           LON        !  longitude (degrees)
                REAL           LAT        !  latitude  (degrees)
                END FUNCTION TRM2LL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION LL2TRM( LON, LAT, X, Y )
                REAL           LON        !  longitude (degrees)
                REAL           LAT        !  latitude  (degrees)
                REAL           X          !  TRM easting  in meters
                REAL           Y          !  TRM northing in meters
                END FUNCTION LL2TRM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION LAM2TRM( X, Y, U, V )
                REAL           X          !  Lambert easting  in meters
                REAL           Y          !  Lambert northing in meters
                REAL           U          !  TRM easting  in meters
                REAL           V          !  TRM northing in meters
                END FUNCTION LAM2TRM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION TRM2LAM( U, V, X, Y )
                REAL           U          !  TRM easting  in meters
                REAL           V          !  TRM northing in meters
                REAL           X          !  Lambert easting  in meters
                REAL           Y          !  Lambert northing in meters
                END FUNCTION TRM2LAM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION TRM2UTM( X, Y, Z, U, V )
                REAL           X          !  TRM easting  in meters
                REAL           Y          !  TRM northing in meters
                INTEGER        Z          !  UTM zone (1...36)
                REAL           U          !  UTM easting  in meters
                REAL           V          !  UTM northing in meters
                END FUNCTION TRM2UTM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION UTM2TRM( U, V, Z, X, Y )
                REAL           U          !  UTM easting  in meters
                REAL           V          !  UTM northing in meters
                INTEGER        Z          !  UTM zone (1...36)
                REAL           X          !  TRM easting  in meters
                REAL           Y          !  TRM northing in meters
                END FUNCTION UTM2TRM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION TRM2POL( U, V, X, Y )
                REAL           U          !  TRM easting  in meters
                REAL           V          !  TRM northing in meters
                REAL           X          !  POL easting  in meters
                REAL           Y          !  POL northing in meters
                END FUNCTION TRM2POL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION POL2TRM( X, Y, U, V )
                REAL           X          !  POL easting  in meters
                REAL           Y          !  POL northing in meters
                REAL           U          !  TRM easting  in meters
                REAL           V          !  TRM northing in meters
                END FUNCTION POL2TRM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION EQM2LL( X, Y, LON, LAT )
                REAL           X          !  EQM easting  in meters
                REAL           Y          !  EQM northing in meters
                REAL           LON        !  longitude (degrees)
                REAL           LAT        !  latitude  (degrees)
                END FUNCTION EQM2LL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION LL2EQM( LON, LAT, X, Y )
                REAL           LON        !  longitude (degrees)
                REAL           LAT        !  latitude  (degrees)
                REAL           X          !  EQM easting  in meters
                REAL           Y          !  EQM northing in meters
                END FUNCTION LL2EQM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION LAM2EQM( X, Y, U, V )
                REAL           X          !  Lambert easting  in meters
                REAL           Y          !  Lambert northing in meters
                REAL           U          !  EQM easting  in meters
                REAL           V          !  EQM northing in meters
                END FUNCTION LAM2EQM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION EQM2LAM( U, V, X, Y )
                REAL           U          !  EQM easting  in meters
                REAL           V          !  EQM northing in meters
                REAL           X          !  Lambert easting  in meters
                REAL           Y          !  Lambert northing in meters
                END FUNCTION EQM2LAM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION EQM2UTM( X, Y, Z, U, V )
                REAL           X          !  EQM easting  in meters
                REAL           Y          !  EQM northing in meters
                INTEGER        Z          !  UTM zone (1...36)
                REAL           U          !  UTM easting  in meters
                REAL           V          !  UTM northing in meters
                END FUNCTION EQM2UTM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION UTM2EQM( U, V, Z, X, Y )
                REAL           U          !  UTM easting  in meters
                REAL           V          !  UTM northing in meters
                INTEGER        Z          !  UTM zone (1...36)
                REAL           X          !  EQM easting  in meters
                REAL           Y          !  EQM northing in meters
                END FUNCTION UTM2EQM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION EQM2POL( U, V, X, Y )
                REAL           U          !  EQM easting  in meters
                REAL           V          !  EQM northing in meters
                REAL           X          !  POL easting  in meters
                REAL           Y          !  POL northing in meters
                END FUNCTION EQM2POL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION POL2EQM( X, Y, U, V )
                REAL           X          !  POL easting  in meters
                REAL           Y          !  POL northing in meters
                REAL           U          !  EQM easting  in meters
                REAL           V          !  EQM northing in meters
                END FUNCTION POL2EQM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION EQM2TRM( U, V, X, Y )
                REAL           U          !  EQM easting  in meters
                REAL           V          !  EQM northing in meters
                REAL           X          !  TRM easting  in meters
                REAL           Y          !  TRM northing in meters
                END FUNCTION EQM2TRM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION TRM2EQM( X, Y, U, V )
                REAL           X          !  TRM easting  in meters
                REAL           Y          !  TRM northing in meters
                REAL           U          !  EQM easting  in meters
                REAL           V          !  EQM northing in meters
                END FUNCTION TRM2EQM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION ALB2LL( X, Y, LON, LAT )
                REAL           X          !  Lambert easting  in meters
                REAL           Y          !  Lambert northing in meters
                REAL           LON        !  Lambert easting  in meters
                REAL           LAT        !  Lambert northing in meters
                END FUNCTION ALB2LL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION LL2ALB( LON, LAT, X, Y )
                REAL           X          !  Lambert easting  in meters
                REAL           Y          !  Lambert northing in meters
                REAL           LON        !  Lambert easting  in meters
                REAL           LAT        !  Lambert northing in meters
                END FUNCTION LL2ALB
            END INTERFACE


!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


        !!........  PRIVATE PARAMETER:

            CHARACTER(LEN=256), PRIVATE, PARAMETER :: ZEROS =
     &'0000000000000000000000000000000000000000000000000000000000000000'
     &//
     &'0000000000000000000000000000000000000000000000000000000000000000'
     &//
     &'0000000000000000000000000000000000000000000000000000000000000000'
     &//
     &'0000000000000000000000000000000000000000000000000000000000000000'


        CONTAINS


            SUBROUTINE SPLITLINE( LINE, NMAX, N, FIELD, EFLAG )

            ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
            !   Split LINE into fields FIELD( N )
            ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

            IMPLICIT NONE

            CHARACTER(LEN=*), INTENT(  IN )::  LINE
            INTEGER,          INTENT(  IN )::  NMAX
            INTEGER,          INTENT( OUT )::  N
            CHARACTER(LEN=*), INTENT( OUT )::  FIELD( NMAX )
            LOGICAL,          INTENT( OUT )::  EFLAG    ! error flag

            !!  PARAMETERS and their descriptions:

            CHARACTER*1,  PARAMETER :: BLANK  = ' '
            CHARACTER*1,  PARAMETER :: TAB    = '	'
            CHARACTER*1,  PARAMETER :: COMMA  = ','
            CHARACTER*1,  PARAMETER :: SEMI   = ';'
            CHARACTER*1,  PARAMETER :: BANG   = '!'
            CHARACTER*1,  PARAMETER :: POUND  = '#'
            CHARACTER*1,  PARAMETER :: DOLLAR = '$'
            CHARACTER*1,  PARAMETER :: QUOTE  = ''''
            CHARACTER*1,  PARAMETER :: QUOTES = '"'


            !!  LOCAL VARIABLES and their descriptions:

            INTEGER         I, J, K, L
            CHARACTER*1     CC, DD
            CHARACTER*256   MESG

            !!  STATEMENT FUNCTIONS: separator characters; comment characters

            CHARACTER*1  CH
            LOGICAL      ISSEP, ISCMT

            ISSEP( CH ) = ( ( CH .LE. BLANK ) .OR. ( CH .EQ. TAB  ) .OR.
     &                      ( CH .EQ. COMMA ) .OR. ( CH .EQ. SEMI ) )

            ISCMT( CH ) = ( ( CH .EQ. BANG  ) .OR.
     &                      ( CH .EQ. POUND ) .OR. ( CH .EQ. DOLLAR ) )

            ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

            EFLAG = .FALSE.
            J     = 0
            N     = 0

            DO  I = 1, NMAX

                DO K = J+1, 256
                    CC = LINE( K:K )
                    IF ( .NOT.ISSEP( CC ) ) THEN
                        L = K
                        GO TO  111
                    ELSE IF ( ISCMT( CC ) ) THEN
                        RETURN
                    END IF
                END DO

                !!  if you get to here:  end of names in list

                EXIT

111             CONTINUE

                !!  so CC = line( L:L ) at this point...

                IF ( CC .EQ.QUOTE ) THEN

                    DO K = L+1, 256
                        CC = LINE( K:K )
                        IF ( CC .EQ.QUOTE ) THEN
                            N = N + 1           !!  n=I<=NMAX by construction
                            FIELD( N ) = LINE( L+1:K-1 )
                            J = K
                            GO TO  122
                        END IF
                    END DO

                ELSE IF ( CC .EQ.QUOTES ) THEN

                    DO K = L+1, 256
                        CC = LINE( K:K )
                        IF ( CC .EQ.QUOTES ) THEN
                            N = N + 1           !!  n=I<=NMAX by construction
                            FIELD( N ) = LINE( L+1:K-1 )
                            J = K
                            GO TO  122
                        END IF
                    END DO

                ELSE

                    DO K = L+1, 256
                        CC = LINE( K:K )
                        IF ( ISSEP( CC ) ) THEN
                            N = N + 1           !!  n=I<=NMAX by construction
                            FIELD( N ) = LINE( L:K-1 )
                            J = K
                            GO TO  122
                        END IF
                    END DO

                END IF

                !!  if you get to here:  error

                WRITE( MESG, '( A, I3, 2X, 3A )' )
     &              'Badly formatted field', N,
     &              'in "', TRIM( LINE ), '"'
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.

122             CONTINUE

            END DO

            DO I = N+1, NMAX
                FIELD( I ) = BLANK
            END DO

            RETURN
            END SUBROUTINE SPLITLINE

        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

            SUBROUTINE FIXFIELD( FIELD )

            ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
            !   DESCRIPTION
            !       Convert "missing" = "-9" fields and leading blanks 
            !       in FIELD to all-zeros
            ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

            !!  Argument:

            CHARACTER(LEN=*), INTENT( INOUT )::  FIELD

            !!  Local Variables:

            INTEGER         I, L

            !!  begin body..........................................

            L = LEN( FIELD )

            IF( INDEX( FIELD, '-9' ) .GT.  0  .OR.
     &                 FIELD         .EQ. '0'      ) THEN

                FIELD = ZEROS

            ELSE

                DO  I = 1, L
                    IF( FIELD( I:I ) .EQ. ' ' ) THEN
                        FIELD( I:I ) = '0'
                    ELSE
                        EXIT
                    ENDIF
                END DO

            END IF

            RETURN
            END SUBROUTINE FIXFIELD



        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


            REAL FUNCTION KEYVAL( KEY )

            !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
            !  retrieve value of REAL KEY from FDESC3's FDESC3D fields
            !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

            !!........  Arguments:

            CHARACTER(LEN=*), INTENT( IN ) :: KEY


            !!........  Local Variables:

            INTEGER         L, C, K
            REAL            X


            !!........  begin body ........................................

            K = LEN_TRIM( KEY )

            DO L = 1, MXDESC3
                C = INDEX( FDESC3D( L ), KEY )
                IF ( C .GT. 0 ) THEN
                    KEYVAL = STR2REAL( FDESC3D( L )( C+K:80 ) )
                    RETURN
                END IF
            END DO

            KEYVAL = BADVAL3
            RETURN


            END FUNCTION KEYVAL


        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


            SUBROUTINE KEYSTR( KEY, VAL )

            !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
            !  retrieve value of KEY from FDESC3's FDESC3D fields
            !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

            !!........  Arguments:

            CHARACTER(LEN=*), INTENT( IN  ) :: KEY
            CHARACTER(LEN=*), INTENT( OUT ) :: VAL


            !!........  Local Variables:

            INTEGER         L, C, K
            REAL            X


            !!........  begin body ........................................

            K = LEN_TRIM( KEY )

            DO L = 1, MXDESC3
                C = INDEX( FDESC3D( L ), KEY )
                IF ( C .GT. 0 ) THEN
                    VAL = TRIM( ADJUSTL( FDESC3D( L )( C+K:80 ) ) )
                    RETURN
                END IF
            END DO

            VAL = CMISS3
            RETURN


            END SUBROUTINE KEYSTR


            !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


            SUBROUTINE LASTTIME( SDATE,STIME,TSTEP,NRECS, EDATE,ETIME )

            INTEGER, INTENT(IN   ) :: SDATE, STIME, TSTEP, NRECS
            INTEGER, INTENT(  OUT) :: EDATE, ETIME

            INTEGER, PARAMETER :: S365 = 365 * 24 * 60 * 60     !!  seconds
            INTEGER, PARAMETER :: T365 = 365 * 24 * 10000       !!  time as H*MMSS

            !!.......   LOCAL VARIABLES:  day and year components of date

            INTEGER     ISEC, STEP
            INTEGER*8   IREC, SECS

            !!  Normalized copies of the arguments:

            EDATE = SDATE
            ETIME = STIME
            CALL NEXTIME( EDATE, ETIME, 0 )

            IF ( TSTEP .EQ. 0 ) THEN   !  time-independent case:
                RETURN
            ELSE IF ( NRECS .LE. 1 ) THEN   !  at most 1 record
                RETURN
            END  IF

            IREC = NRECS - 1
            SECS = IREC * TIME2SEC( ABS( TSTEP ) )

            DO
                IF ( SECS .LT. S365 ) EXIT
                SECS = SECS - S365
                CALL NEXTIME( EDATE, ETIME, T365 )
            END DO

            ISEC = SECS
            STEP = SEC2TIME( ISEC )
            CALL NEXTIME( EDATE, ETIME, STEP )

            RETURN

            END SUBROUTINE LASTTIME


        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


            INTEGER FUNCTION LCM( I, J )
                INTEGER, INTENT( IN ) :: I, J
                LCM = ( I * J ) / GCD( I, J )
            END FUNCTION LCM


        END MODULE M3UTILIO

