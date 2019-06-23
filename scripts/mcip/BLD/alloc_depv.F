
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
! $Header: /project/work/rep/MCIP2/src/mcip2/alloc_depv.F,v 1.2 2006/03/01 20:13:47 tlotte Exp $


SUBROUTINE alloc_depv

!-------------------------------------------------------------------------------
! Name:     Allocate Dry Deposition Variables
! Purpose:  Allocate arrays for dry deposition.
! Revised:  19 Aug 2005  Original version.  (T. Otte and W. Hutzell)
!           27 Feb 2006  Added variable MESO.  (D. Schwede)
!-------------------------------------------------------------------------------

  USE depvvars
  USE mcipparm

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Allocate general dry deposition arrays.
!-------------------------------------------------------------------------------

  ALLOCATE ( xdepspc                   (ltotg) )
  ALLOCATE ( xvd     (ncols_x, nrows_x, ltotg) ) 
  ALLOCATE ( vd_c    (ncols,   nrows,   ltotg) )

!-------------------------------------------------------------------------------
! Allocate arrays based on dry deposition option.
!-------------------------------------------------------------------------------

  IF ( lddep == 1 ) THEN  ! RADMDRY

    ALLOCATE ( f0      (ltotg) )
    ALLOCATE ( hstr    (ltotg) )
    ALLOCATE ( rdif    (ltotg) )

  ELSE IF ( ( lddep == 2 ) .OR. ( lddep == 3 ) .OR.  &
            ( lddep == 4 ) ) THEN  ! M3DRY

    ALLOCATE ( a       (ltotg) )
    ALLOCATE ( alphs   (ltotg) )
    ALLOCATE ( dif0    (ltotg) )
    ALLOCATE ( dkhor   (ltotg) )
    ALLOCATE ( kh      (ltotg) )
    ALLOCATE ( meso    (ltotg) )
    ALLOCATE ( subname (ltotg) )

  ENDIF

END SUBROUTINE alloc_depv
