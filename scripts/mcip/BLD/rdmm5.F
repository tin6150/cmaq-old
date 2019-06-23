
!***********************************************************************
!   Portions of Models-3/CMAQ software were developed or based on      *
!   information from various groups: Federal Government employees,     *
!   contractors working on a United States Government contract, and    *
!   non-Federal sources (including research institutions).  These      *
!   research institutions have given the Government permission to      *
!   use, prepare derivative works, and distribute copies of their      *
!   work in Models-3/CMAQ to the public and to permit others to do     *
!   so.  EPA therefore grants similar permissions for use of the       *
!   Models-3/CMAQ software, but users are requested to provide copies  *
!   of derivative works to the Government without restrictions as to   *
!   use by others.  Users are responsible for acquiring their own      *
!   copies of commercial software associated with Models-3/CMAQ and    *
!   for complying with vendor requirements.  Software copyrights by    *
!   the MCNC Environmental Modeling Center are used with their         *
!   permissions subject to the above restrictions.                     *
!***********************************************************************

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /project/work/rep/MCIP2/src/mcip2/rdmm5.F,v 1.1.1.1 2002/03/09 14:48:04 yoj Exp $ 


SUBROUTINE rdmm5 (mcip_now)

!-------------------------------------------------------------------------------
! Name:     Read MM5
! Purpose:  Reads incoming MM5v2 or MM5v3 output files for use in MCIP.
! Notes:    Option to use Pleim-Xiu prognostic surface fields is only
!           compatible with MM5v3.
! Revised:  20 Sep 2001  Original version.  This replaces MCIP v1 routines
!                        readmm1.F and readmm2.F.  This routine reads
!                        both MM5v2 and MM5v3 formatted input.  (T. Otte)
!           09 Jan 2002  Changed calls to "abort" to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  (T. Otte)
!-------------------------------------------------------------------------------

  USE metinfo

  IMPLICIT NONE

  CHARACTER*24,  INTENT(IN)    :: mcip_now
  CHARACTER*16,  PARAMETER     :: pname     = 'RDMM5'

!-------------------------------------------------------------------------------
! Call subroutine for input based on version of MM5 output format.
!-------------------------------------------------------------------------------

  IF ( met_iversion == 2 ) THEN
    CALL rdmm5v2 (mcip_now)
  ELSE IF ( met_iversion == 3 ) THEN
    CALL rdmm5v3 (mcip_now)
  ELSE
    WRITE (6,9000) met_iversion
    GOTO 1001
  ENDIF

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                   &
              /, 1x, '*** SUBROUTINE: RDMM5',                   &
              /, 1x, '***   UNKNOWN MM5 OUTPUT VERSION',        &
              /, 1x, '***   IVERSION = ', i3,                   &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE rdmm5
