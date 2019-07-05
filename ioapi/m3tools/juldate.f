
        PROGRAM JULDATE

C***********************************************************************
C Version "$Id: juldate.f 49 2007-07-06 16:20:50Z coats@borel $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr, and
C (C) 2002-2007 Baron Advanced Meteorological Systems, LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  98
C
C  DESCRIPTION:
C	interactively month, day, year;
C	get julian date YYYYDD back.
C
C  PRECONDITIONS REQUIRED:
C       none
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C	JULIAN, GETNUM
C
C  REVISION  HISTORY:
C	Prototype  8/95 by CJC
C       Enhanced   6/98 to support YESTERDAY, TODAY, TOMORROW.
C       Version 11/2001 by CJc for I/O API Version 2.1
C***********************************************************************

      IMPLICIT NONE

C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER		IARGC
        LOGICAL		ISDSTIME
        INTEGER		GETNUM
        INTEGER		JULIAN
        INTEGER		STR2INT
        CHARACTER*14    MMDDYY
        INTEGER		WKDAY

        EXTERNAL	ISDSTIME, GETNUM, JULIAN, MMDDYY, STR2INT,
     &                  WKDAY

C...........   PARAMETERS and their descriptions:

        CHARACTER*80    PROGVER
        DATA PROGVER /
     &'$Id:: juldate.f 49 2007-07-06 16:20:50Z coats@borel           $'
     &  /

C...........   LOCAL VARIABLES and their descriptions:

        INTEGER		MON
        INTEGER		DAY
        INTEGER		YR
        INTEGER		JDATE, JTIME
        INTEGER         ARGCNT  !  number of command-line args, from IARGC()
        LOGICAL         PFLAG
        CHARACTER*80    MONBUF
        CHARACTER*80    DAYBUF
        CHARACTER*80    YRBUF

C.......   Lookup table for months, days:

        INTEGER   	MLENS ( 12 )
        DATA       	MLENS
     &          /
     &      31 , 29 , 31, 30 , 31 , 30,
     &      31 , 31 , 30, 31 , 30 , 31
     &          /

        CHARACTER*3	MONTHS ( 12 )
        DATA       	MONTHS
     &          /
     &      'JAN' , 'FEB' , 'MAR', 'APR' , 'MAY' , 'JUN',
     &      'JUL' , 'AUG' , 'SEP', 'OCT' , 'NOV' , 'DEC'
     &          /

        CHARACTER*10     DAYS( 7 )
        DATA            DAYS
     &  /
     &  'Monday', 'Tuesday', 'Wednesday',
     &  'Thursday', 'Friday', 'Saturday', 'Sunday'
     &  /

        INTEGER         DLEN( 7 )	! string lengths
        DATA            DLEN
     &  /
     &  6, 7, 9,
     &  8, 6, 8, 6
     &  /


C***********************************************************************
C   begin body of program JULDATE

        ARGCNT = IARGC()
        CALL GETDTTIME( JDATE, JTIME )

        IF ( ARGCNT .EQ. 1 ) THEN

            CALL GETARG( 1, MONBUF )
            CALL UPCASE( MONBUF )
            IF ( MONBUF .EQ. 'TODAY' ) THEN
                GO TO  99
            ELSE IF ( MONBUF .EQ. 'YESTERDAY' ) THEN
                CALL NEXTIME( JDATE, JTIME, -240000 )
                GO TO  99
            ELSE IF ( MONBUF .EQ. 'TOMORROW' ) THEN
                CALL NEXTIME( JDATE, JTIME, 240000 )
                GO TO  99
            END IF

            PFLAG = .TRUE.      !  malformed input:  prompt user

        ELSE IF ( ARGCNT .EQ. 3 ) THEN

            CALL GETARG( 1, MONBUF )
            CALL UPCASE( MONBUF )
            DO  11  MON = 1, 12
                IF ( INDEX( MONBUF, MONTHS( MON ) ) .GT. 0 ) THEN
                    GO TO 12
                END IF
11          CONTINUE
            MON = STR2INT( MONBUF )
12          CONTINUE		!  month found by name

            CALL GETARG( 2, DAYBUF )
            DAY = STR2INT( DAYBUF )

            CALL GETARG( 3, YRBUF )
            YR = STR2INT( YRBUF )

            PFLAG = ( MON    .GT.   12  .OR.
     &                MON    .LT.    1  .OR.
     &                DAY    .GT.   31  .OR.
     &                DAY    .LT.    1  .OR.
     &                YR     .LT. 1000  .OR.
     &                YR     .GT. 9999 ) !  malformed input:  prompt user

        ELSE

            PFLAG = .TRUE.      !  prompt user

        END IF

        IF ( PFLAG ) THEN

            WRITE( *,92000 ) ' ', ' ',
     & 'Program JULDATE takes calendar date (in form Month DD YYYY)',
     & 'and returns the date in Julian-date form "YYYYDDD".',
     & ' ',
     & '    Usage:  "juldate [<MONTH DAY YEAR>]" ', ' ',
     & '(if the command-line arguments are missing, prompts the ',
     & 'user for them)',
     & ' ',
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

            CALL DAYMON( JDATE, MON, DAY )

            MON = GETNUM( 1, 12, MON, 'Enter month (1-12)' )

            DAY = GETNUM( 1, MLENS(MON), DAY, 'Enter day (1-31)' )

            YR = GETNUM( 1000, 9999, JDATE / 1000, 'Enter year' )

        END IF	!  if argcnt=3, or not

        JDATE = 1000 * YR  +  JULIAN( YR, MON, DAY )

99      CONTINUE		!  generate output

        DAY   = WKDAY( JDATE )
        IF ( ISDSTIME( JDATE ) ) THEN
            WRITE( *,92010 )
     &          DAYS( DAY )( 1:DLEN( DAY ) ),
     &          JDATE,
     &          'Daylight Savings Time in effect.'
        ELSE
            WRITE( *,92010 )
     &          DAYS( DAY )( 1:DLEN( DAY ) ),
     &          JDATE,
     &          'Standard Time in effect.'
        END IF

      CALL EXIT( JDATE )

C******************  FORMAT  STATEMENTS   ******************************

C...........   Informational (LOG) message formats... 92xxx

92000	FORMAT( 5X, A )

92010	FORMAT( /, 5X, A, ', ', I7.7, /5X, A, / )


        END

