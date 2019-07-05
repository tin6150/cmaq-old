
        SUBROUTINE  M3MSG2( MESSAGE )

C***********************************************************************
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2007  Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  Subroutine  M3MSG2   starts at line  103
C  Subroutine  M3MESG   starts at line  150
C  Subroutine  M3PROMPT starts at line  158
C  Subroutine  M3PARAG  starts at line  187
C  Subroutine  M3FLUSH  starts at line  219
C  subroutine  FLUSH    starts at line  229
C
C  FUNCTION:
C       M3MSG2:    Generate simple (1-line text) messages. 
C       M3PROMPT:  Generate prompt-and-response;
C       M3PARAG:   Generate multi-line text messages;
C       M3FLUSH:   wrapper around FLUSH() -- needed for IBM XLF.
C       FLUSH:     overide system FLUSH when IOAPI_NOFLUSH
C
C  PRECONDITIONS REQUIRED:  
C       message fits on one line
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       INIT3, TRIMLEN
C
C  REVISION  HISTORY:   
C       Adapted   1/1997 by CJC from M3MESG()
C
C       Modified  5/1998 by CJC for OpenMP thread-safety
C
C       Modified 10/1999 by CJC for AIX "flush"
C
C       Modified  5/2003 by CJC for control of optional messages via
C       STATE3 variable STDOUT; changes to critical sections.
C
C       Modified  6/2003 by CJC to deal with IRIX F90v7.4.x failure to
C       properly ANSWER and flush output on program exit:  new entries
C       M3PROMPT used by the GET*() and PROMPT*() routines, and 
C       M3FLUSH for use by user-code.
C
C       Modified  9/2003 by CJC: new entry "M3PARAG"
C
C       Modified 12/2003 by CJC:  Hack "CALL INITBLK3" to deal with 
C       failure of SGI version 7.4 compilers to follow industry standards;
C       re-structuring with INITLOG3() to avoid problems with nested
C       critical sections.
C
C       Modified 4/2004 by CJC:  absorbtion of M3MESG() into  M3MSG2()
C       as an overlapping ENTRY.
C
C       Modified 12/2007 by CJC:  mollify Intel "ifort" by turning this
C       back into a set of subroutines.  
C***********************************************************************

      IMPLICIT NONE

#ifdef   _AIX
#define  FLUSH flush_
#endif

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'IODECL3.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*)   MESSAGE

                 
C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER         INITLOG3
        INTEGER         TRIMLEN !  string length, exclusive of trailing blanks

        EXTERNAL        INITLOG3, TRIMLEN
        EXTERNAL        INITBLK3        !!  initialize STATE3 commons


C.......   Scratch LOCAL VARIABLES

        INTEGER		L, M
        LOGICAL         STDOUTFLAG


C......................................................................
C   begin body of subroutine  M3MSG2

        STDOUTFLAG = .FALSE.
#ifdef IOAPI_STDOUT
        STDOUTFLAG = .TRUE.
#endif

11      CONTINUE        !  target of "GO TO" in entry M3MESG()

        L = TRIMLEN( MESSAGE )

!$OMP CRITICAL( S_LOGDEV )

#if defined(__sgi)    || defined(__mips__)

            !!==========>  *HACK*  to deal with SGI v7.4 or later compilers,
            !!             that do not follow industry standards with respect
            !!             to how to make sure that BLOCK DATA modules are
            !!             properly linked into an executable:

            CALL INITBLK3

#endif

            IF ( LOGDEV .LT. 0 ) THEN
                LOGDEV = INITLOG3( 'M3MSG2' )
            END IF          !!  if logdev < 0 (initialization required)

            WRITE( LOGDEV,'( 5X, A )' ) MESSAGE( 1:L )
	    CALL FLUSH( LOGDEV )

            IF ( STDOUTFLAG .AND. LOGDEV .NE. 6 ) THEN
                WRITE( 6,'( 5X, A )' ) MESSAGE( 1:L )
	        CALL FLUSH( 6 )
            END IF		!  if logdev is not pointing at standard output

!$OMP END CRITICAL( S_LOGDEV )

        RETURN
        END


C-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
C   begin subroutine  M3MESG

        SUBROUTINE  M3MESG( MESSAGE )

        IMPLICIT NONE

#ifdef   _AIX
#define  FLUSH flush_
#endif

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'IODECL3.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*)   MESSAGE

                 
C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER         INITLOG3
        INTEGER         TRIMLEN !  string length, exclusive of trailing blanks

        EXTERNAL        INITLOG3, TRIMLEN
        EXTERNAL        INITBLK3        !!  initialize STATE3 commons


C.......   Scratch LOCAL VARIABLES

        INTEGER		L

C......................................................................
C   begin body of subroutine  M3MESG

        L = TRIMLEN( MESSAGE )

!$OMP CRITICAL( S_LOGDEV )

#if defined(__sgi)    || defined(__mips__)

            !!==========>  *HACK*  to deal with SGI v7.4 or later compilers,
            !!             that do not follow industry standards with respect
            !!             to how to make sure that BLOCK DATA modules are
            !!             properly linked into an executable:

            CALL INITBLK3

#endif

            IF ( LOGDEV .LT. 0 ) THEN
                LOGDEV = INITLOG3( 'M3MSG2/M3MESG' )
            END IF          !!  if logdev < 0 (initialization required)

            WRITE( LOGDEV,'( 5X, A )' ) MESSAGE( 1:L )
	    CALL FLUSH( LOGDEV )

!$OMP END CRITICAL( S_LOGDEV )

        RETURN
        END


C-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
C   begin subroutine  M3PROMPT

        SUBROUTINE M3PROMPT( MESSAGE, ANSWER, ISTAT )

        IMPLICIT NONE

#ifdef   _AIX
#define  FLUSH flush_
#endif

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'IODECL3.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*)   MESSAGE
        CHARACTER*(*)   ANSWER
        INTEGER         ISTAT
                 
C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL         ENVYN   !  get YES/NO environment variable
        INTEGER         INITLOG3
        INTEGER         TRIMLEN !  string length, exclusive of trailing blanks

        EXTERNAL        ENVYN, INITLOG3, TRIMLEN
        EXTERNAL        INITBLK3        !!  initialize STATE3 commons


C.......   Scratch LOCAL VARIABLES

        INTEGER		L, M


C......................................................................
C   begin body of subroutine  M3PROMPT

        L = TRIMLEN( MESSAGE )

!$OMP   CRITICAL( S_LOGDEV )

            IF ( LOGDEV .LT. 0 ) THEN
                LOGDEV = INITLOG3( 'M3MSG2/M3PROMPT' )
            END IF          !!  if logdev < 0 (initialization required)

            WRITE( LOGDEV,'( 5X, A, $ )' ) MESSAGE( 1:L )
	    CALL FLUSH( LOGDEV )
            ANSWER = ' '
            READ  ( *, '( A )', IOSTAT=ISTAT )  ANSWER

!$OMP   END CRITICAL( S_LOGDEV )

        IF ( ISTAT .LT. 0 ) THEN
            CALL M3EXIT( 'M3PROMPT',0,0,'END-OF-FILE encountered', 1 )
        ELSE IF ( ISTAT .EQ. 0 ) THEN
            L = INDEX( ANSWER, '!' )
            IF ( L .GT. 0 ) ANSWER( L : LEN( ANSWER ) ) = ' '
        END IF

        RETURN
        END


C-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
C   begin subroutine  M3PARAG

        SUBROUTINE M3PARAG( NMESG, MSGS )

        IMPLICIT NONE

#ifdef   _AIX
#define  FLUSH flush_
#endif

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'IODECL3.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER         NMESG
        CHARACTER*(*)   MSGS( NMESG )
                 
C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL         ENVYN   !  get YES/NO environment variable
        INTEGER         INITLOG3
        INTEGER         TRIMLEN !  string length, exclusive of trailing blanks

        EXTERNAL        ENVYN, INITLOG3, TRIMLEN
        EXTERNAL        INITBLK3        !!  initialize STATE3 commons


C.......   Scratch LOCAL VARIABLES

        INTEGER		L, M

C......................................................................
C   begin body of subroutine  M3PARAG

!$OMP   CRITICAL( S_LOGDEV )

            IF ( LOGDEV .LT. 0 ) THEN
                LOGDEV = INITLOG3( 'M3MSG2/M3PARAG')
            END IF          !!  if logdev < 0 (initialization required)

            DO L = 1, NMESG
                M = TRIMLEN( MSGS( L ) )
                WRITE( LOGDEV,'( 5X, A )' ) MSGS( L )( 1:M )
            END DO

#ifdef IOAPI_STDOUT

            IF ( LOGDEV .NE. 6 ) THEN
                DO L = 1, NMESG
                    M = TRIMLEN( MSGS( L ) )
                    WRITE( 6,'( 5X, A )' ) MSGS( L )( 1:M )
                END DO
            END IF		!  if logdev is not pointing at standard output
#endif

	    CALL FLUSH( LOGDEV )

!$OMP   END CRITICAL( S_LOGDEV )

        RETURN
        END

C-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
C   begin subroutine  M3FLUSH

        
        SUBROUTINE M3FLUSH( IDEV )

C...........   ARGUMENTS and their descriptions:

        INTEGER IDEV    !  fortran unit to be flushed

C......................................................................
C   begin body of subroutine  M3flush

!$OMP CRITICAL( S_LOGDEV )

	    CALL FLUSH( IDEV )

!$OMP END CRITICAL( S_LOGDEV )

        RETURN
        END


C-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
C   begin conditional subroutine  FLUSH

#ifdef IOAPI_NOFLUSH

        SUBROUTINE FLUSH( IDEV )
            IMPLICIT NONE
            INTEGER IDEV
            !!  present version:  do nothing
            RETURN
        END

#endif
