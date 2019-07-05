
C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2004 Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        SUBROUTINE  M3WARN( CALLER, JDATE, JTIME, MSGTXT )

C***********************************************************************
C  subroutine body starts at line  65
C
C  FUNCTION:  Generate simple warning messages for Models-3 core;
C
C  PRECONDITIONS REQUIRED:  
C	JDATE:JTIME represented as YYYYDDD:HHMMSS, or 0 if not relevant
C
C  SUBROUTINES AND FUNCTIONS CALLED:  DT2STR, INIT3
C
C  REVISION  HISTORY:	
C	adapted   9/1995 by CJC from M3ERR()
C       modified  1/1997 by CJC to trim trailing blanks from MSGTXT
C       modified 10/1998 by CJC:  Factor all output through m3msg2(), to
C                      get around SGI OpenMP bug.
C                      Caution:  messages may not be sequenced correctly
C                      when called by multiple threads simultaneously
C       Modified  5/1998 by CJC for OpenMP thread-safety:  
C                       factors through M3MSG2
C       Modified  5/2003 by CJC:  factor all messages through M3MSG2()
C       Modified  4/2004 by CJC:  factor through M3PARAG() -- fixes
C       multi-thread sequencing problem.
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'IODECL3.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*)   CALLER          !  name of the caller
        INTEGER         JDATE, JTIME    !  model date&time for the error
        CHARACTER*(*)   MSGTXT          !  error message

                 
C...........   EXTERNAL FUNCTIONS and their descriptions:

        CHARACTER*24    DT2STR
        EXTERNAL        DT2STR


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        CHARACTER*256   MESG( 5 )


C***********************************************************************
C   begin body of subroutine  M3WARN

        MESG( 1 ) = ' '
        MESG( 2 ) = '>>--->> WARNING in subroutine ' // CALLER
        MESG( 3 ) = MSGTXT

        IF ( JDATE .GT. 0  .OR.  JTIME .GT. 0 ) THEN
            MESG( 4 ) = 'M3WARN:  DTBUF ' // DT2STR( JDATE, JTIME )
            MESG( 5 ) = ' '
            CALL M3PARAG( 4, MESG )
        ELSE
            MESG( 4 ) = ' '
            CALL M3PARAG( 4, MESG )
        END IF

        RETURN

        END
