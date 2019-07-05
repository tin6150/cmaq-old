
C.........................................................................
C Version "@(#)$Header$"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2004 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................

        SUBROUTINE  IOPARMS3( MXDLEN, NAMLEN, MXFILE, MXVARS,
     &                        MXDESC, MXLAYS, MXATTS )

C***********************************************************************
C  Subroutine body starts at line  52
C
C  FUNCTION:
C       Return "compiled-into-the-library dimensioning PARAMETER values
C       from PARMS3.EXT,so that user/model-level code can perform
C       consistency checks of INCLUDEd values against "libioapi.a"
C
C  PRECONDITIONS REQUIRED:
C       none
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       none
C
C  REVISION  HISTORY:   
C       Originated   4/2004 by Carlie J. Coats, Jr., BAMS
C***********************************************************************

      IMPLICIT NONE


C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'

C...........   ARGUMENTS and their descriptions:
            
        INTEGER         MXDLEN    !  description line length
        INTEGER         NAMLEN    !  name length (logical names, etc.)
        INTEGER         MXFILE    !  max number of open files
        INTEGER         MXVARS    !  max number of variables per file
        INTEGER         MXDESC    !  max number of description lines
        INTEGER         MXLAYS    !  max # of layers per file
        INTEGER         MXATTS    !  max # ATDSC .EXT attributes per variable

C***********************************************************************
C   begin body of subroutine  M3MSG2

        MXDLEN = MXDLEN3
        NAMLEN = NAMLEN3
        MXFILE = MXFILE3
        MXVARS = MXVARS3
        MXDESC = MXDESC3
        MXLAYS = MXLAYS3
        MXATTS = MXATTS3

        RETURN

        END
