
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
! $Header: /project/work/rep/MCIP2/src/mcip2/mdate2hdate.F,v 1.1.1.1 2002/03/09 14:48:02 yoj Exp $ 


SUBROUTINE mdate2hdate (mdate, hdate)

!-------------------------------------------------------------------------------
! Name:     MDATE to HDATE
! Purpose:  Convert date format from MM5v2 "MDATE" to MM5v3 "HDATE".
! Revised:  10 Sep 2001  Original version.  (T. Otte)
!           09 Jan 2002  Allow for MM5v2 output from 2000 and beyond.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  CHARACTER*2                  :: cc
  CHARACTER*24,  INTENT(OUT)   :: hdate
  INTEGER                      :: idd
  CHARACTER*2                  :: iddc
  INTEGER                      :: ihh
  CHARACTER*2                  :: ihhc
  INTEGER                      :: imm
  CHARACTER*2                  :: immc
  INTEGER                      :: iyy
  CHARACTER*2                  :: iyyc
  INTEGER,       INTENT(IN)    :: mdate

!-------------------------------------------------------------------------------
! Build MM5v3-style "currentdate" from "mdate".  Since MM5v2 is not
! Year-2000-Compliant, assume dates in MM5v2 are between 1970 and 2069.
!-------------------------------------------------------------------------------

  iyy = mdate / 1000000
  imm = MOD ( mdate/10000, iyy*100 )
  idd = MOD ( mdate/100, iyy*10000 + imm*100 )
  ihh = MOD ( mdate, iyy*1000000 + imm*10000 + idd*100 )

  WRITE ( iyyc, '(i2.2)' ) iyy
  WRITE ( immc, '(i2.2)' ) imm
  WRITE ( iddc, '(i2.2)' ) idd
  WRITE ( ihhc, '(i2.2)' ) ihh

  IF ( iyy < 70 ) THEN
    cc = '20'
  ELSE
    cc = '19'
  ENDIF

  hdate = cc // iyyc //'-'// immc //'-'// iddc //'-'// ihhc //':00:00.0000'

END SUBROUTINE mdate2hdate
