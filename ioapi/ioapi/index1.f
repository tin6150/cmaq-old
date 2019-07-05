
      INTEGER FUNCTION INDEX1 (NAME, N, NLIST)

C***********************************************************************
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2006 Baron Advanced Meteorological Systems, LLC.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  INDEX1    subroutine body starts at line 51
C  INDEXINT1 subroutine body starts at line 97
C
C  FUNCTION:
C
C       Search for character-string or integer key NAME or IKEY in list NLIST
C       and return the subscript (1...N) at which it is found, or return 0
C       when not found in NLIST
C
C  PRECONDITIONS REQUIRED:
C       none
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       none
C
C  REVISION HISTORY:
C    INDEX1:
C       5/1988   Modified for ROMNET
C       9/1994   Modified for Models-3 by CJC
C    INDEXINT1:
C       11/2004:  Prototype by CJC in MODULE M3UTILIO for I/O API v3
C        3/2006:  moved to file "index1.f"
C***********************************************************************

      IMPLICIT NONE
 
C.......   Arguments and their descriptions:

      CHARACTER*(*) NAME        !  Character string being searched for
      INTEGER       N           !  Length of array to be searched
      CHARACTER*(*) NLIST(*)    !  array to be searched

C.......   Local variable:

      INTEGER       I   !  loop counter

C.....................................................................
C.......   begin body of INDEX1()

        DO I = 1, N
            IF ( NAME .EQ. NLIST( I ) ) THEN    ! Found NAME in NLIST
                INDEX1 = I
                RETURN
            ENDIF
        END DO

        INDEX1 = 0        !  not found
        RETURN

        END


        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
  

        INTEGER FUNCTION INDEXINT1( IKEY, NLIST, KEYLIST )

        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        !  Look up integer key IKEY in unsorted list <NLIST,KEYLIST>
        !  of integer keys.  Return the subscript at which IKEY
        !  occurs, or 0 in case of failure
        !  
        !  PRECONDITIONS REQUIRED:
        !      none 
        !
        !  REVISION  HISTORY:
        !      Prototype  11/2004 by CJC
        !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        IMPLICIT NONE


        !!........  Arguments:

        INTEGER         IKEY
        INTEGER         NLIST
        INTEGER         KEYLIST( NLIST )


        !!........  Local Variables:

        INTEGER I

        !!........  begin body ........................................

        DO  I = 1, NLIST
            IF ( IKEY .EQ. KEYLIST( I ) ) THEN
                INDEXINT1 = I
                RETURN
            END IF
        END DO

        INDEXINT1 = 0

        RETURN
        END



