
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
! $Header: /project/work/rep/MCIP2/src/mcip2/dealloc_depv.F,v 1.2 2006/03/01 20:14:02 tlotte Exp $


SUBROUTINE dealloc_depv

!-------------------------------------------------------------------------------
! Name:     Deallocate Dry Deposition Variables
! Purpose:  Deallocate arrays for dry deposition.
! Revised:  19 Aug 2005  Original version.  (T. Otte and W. Hutzell)
!           27 Feb 2006  Added variable MESO.  (D. Schwede)
!-------------------------------------------------------------------------------

  USE depvvars

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Deallocate general dry deposition arrays.
!-------------------------------------------------------------------------------

  DEALLOCATE ( xdepspc )
  DEALLOCATE ( xvd     )
  DEALLOCATE ( vd_c    )

!-------------------------------------------------------------------------------
! Deallocate arrays based on dry deposition option.
!-------------------------------------------------------------------------------

  ! Arrays for RADMDRY.

  IF ( ALLOCATED ( f0      ) ) DEALLOCATE ( f0      )
  IF ( ALLOCATED ( hstr    ) ) DEALLOCATE ( hstr    )
  IF ( ALLOCATED ( rdif    ) ) DEALLOCATE ( rdif    )

  ! Arrays for M3DRY.

  IF ( ALLOCATED ( a       ) ) DEALLOCATE ( a       )
  IF ( ALLOCATED ( alphs   ) ) DEALLOCATE ( alphs   )
  IF ( ALLOCATED ( dif0    ) ) DEALLOCATE ( dif0    )
  IF ( ALLOCATED ( dkhor   ) ) DEALLOCATE ( dkhor   )
  IF ( ALLOCATED ( kh      ) ) DEALLOCATE ( kh      )
  IF ( ALLOCATED ( meso    ) ) DEALLOCATE ( meso    )
  IF ( ALLOCATED ( subname ) ) DEALLOCATE ( subname )

END SUBROUTINE dealloc_depv
