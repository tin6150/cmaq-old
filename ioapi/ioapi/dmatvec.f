
C.........................................................................
C Version "@(#)$Header: /env/proj/archive/cvs/ioapi/./ioapi/src/dmatvec.f,v 1.2 2000/11/28 21:22:38 smith_w Exp $"
C EDSS/Models-3 I/O API.  Copyright (C) 1992-1999 MCNC
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        SUBROUTINE  DMATVEC( N, A, V, C )

C***********************************************************************
C  subroutine body starts at line  44
C
C  FUNCTION:  apply a diagonal matrix to a vector
C
C  PRECONDITIONS REQUIRED:  none
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C	prototype 2/95 by CJC
C
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:
        
        INTEGER		N		! length of input vector
        REAL 		A( N )		! diagonal coeff matrix
        REAL		V( N )		! input  vector
        REAL		C( N )		! output vector


C...........   SCRATCH LOCAL VARIABLES and their descriptions:
        
        INTEGER		R


C***********************************************************************
C   begin body of subroutine  DMATVEC

        DO  22  R = 1, N
            
            C( R ) = A( R ) * V( R )
        
22      CONTINUE

        RETURN
        END

