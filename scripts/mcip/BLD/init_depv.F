
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

SUBROUTINE init_depv

!-------------------------------------------------------------------------------
! Name:     Initialize Dry Deposition Arrays
! Purpose:  Initializes dry deposition arrays.
! Revised:  11 Aug 2005  Initial version.  (T. Otte and W. Hutzell)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE depvvars

  IMPLICIT NONE
 
!-------------------------------------------------------------------------------
! For RADMDRY (LDDEP=1) and basic M3DRY (LDDEP=2), set up core species.
!-------------------------------------------------------------------------------

  xdepspc( 1) = 'SO2'
  xdepspc( 2) = 'SULF'
  xdepspc( 3) = 'NO2'
  xdepspc( 4) = 'NO'
  xdepspc( 5) = 'O3'
  xdepspc( 6) = 'HNO3'
  xdepspc( 7) = 'H2O2'
  xdepspc( 8) = 'ALD'
  xdepspc( 9) = 'HCHO'
  xdepspc(10) = 'OP'
  xdepspc(11) = 'PAA'
  xdepspc(12) = 'ORA'
  xdepspc(13) = 'NH3'
  xdepspc(14) = 'PAN'
  xdepspc(15) = 'HONO'
  xdepspc(16) = 'CO'
  xdepspc(17) = 'METHANOL'
  xdepspc(18) = 'N2O5'
  xdepspc(19) = 'NO3'
  xdepspc(20) = 'GEN_ALD'

!-------------------------------------------------------------------------------
! For M3DRY plus chlorine (LDDEP=3) and M3DRY plus chlorine and mercury
! (LDDEP=4), add chlorine species.
!-------------------------------------------------------------------------------

  IF ( lddep >= 3 ) THEN
    xdepspc(21) = 'CL2'
    xdepspc(22) = 'HOCL'
    xdepspc(23) = 'HCL'
    xdepspc(24) = 'FMCL'
    xdepspc(25) = 'ICL1'
    xdepspc(26) = 'ICL2'
  ENDIF

!-------------------------------------------------------------------------------
! For M3DRY plus chlorine and mercury (LDDEP=4), add mercury species.
!-------------------------------------------------------------------------------

  IF ( lddep == 4 ) THEN
    xdepspc(27) = 'HG'
    xdepspc(28) = 'HGIIGAS'
  ENDIF

END SUBROUTINE init_depv
