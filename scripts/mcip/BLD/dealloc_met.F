
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
! $Header: /project/work/rep/MCIP2/src/mcip2/dealloc_met.F,v 1.5 2005/08/29 14:30:20 tlotte Exp $ 


SUBROUTINE dealloc_met

!-------------------------------------------------------------------------------
! Name:     Deallocate Meteorology Variables
! Purpose:  Deallocate arrays for input meteorology variables.
! Revised:  10 Sep 2001  Original version.  (T. Otte)
!           29 May 2003  Added SNOWCOVR.  (D. Schwede)
!           09 Aug 2004  Added QGA, VEGOLD, and T2.  (D. Schwede and T. Otte)
!           29 Nov 2004  Added LUFRAC.  (T. Otte)
!           04 Apr 2005  Removed unused variables REGIME and MAVAIL.  Added PH,
!                        PHB, PB, MU, and MUB for WRF.  Added U10 and V10.
!                        (T. Otte and S.-B. Kim)
!           11 Aug 2005  Removed unused variable FSOIL.  (T. Otte)
!-------------------------------------------------------------------------------

  USE metinfo
  USE metvars
  USE mcipparm

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Deallocate time-invariant fields.
!-------------------------------------------------------------------------------

  DEALLOCATE ( albedo   )  ! <--- time-variant with Pleim-Xiu LSM
  DEALLOCATE ( emiss    )
  DEALLOCATE ( landuse  )
  DEALLOCATE ( latcrs   )
  DEALLOCATE ( latdot   )
  DEALLOCATE ( loncrs   )
  DEALLOCATE ( londot   )
  DEALLOCATE ( mapcrs   )
  DEALLOCATE ( mapdot   )
  DEALLOCATE ( sigmah   )
  DEALLOCATE ( sigmaf   )
  DEALLOCATE ( terrain  )
  DEALLOCATE ( znt      )

  IF ( iflufrc ) THEN  ! fractional land-use fields available
    DEALLOCATE ( lufrac )
  ENDIF

!-------------------------------------------------------------------------------
! Deallocate time-variant fields.
!-------------------------------------------------------------------------------

  DEALLOCATE ( glw     )
  DEALLOCATE ( groundt )
  DEALLOCATE ( hfx     )
  DEALLOCATE ( pp      )
  DEALLOCATE ( psa     )
  DEALLOCATE ( qca     )
  DEALLOCATE ( qfx     )
  DEALLOCATE ( qga     )
  DEALLOCATE ( qia     )
  DEALLOCATE ( qra     )
  DEALLOCATE ( qsa     )
  DEALLOCATE ( qva     )
  DEALLOCATE ( raincon )
  DEALLOCATE ( rainnon )
  DEALLOCATE ( rcold   )
  DEALLOCATE ( rgrnd   )
  DEALLOCATE ( rnold   )
  DEALLOCATE ( snowcovr)
  DEALLOCATE ( ta      )
  DEALLOCATE ( ua      )
  DEALLOCATE ( ust     )
  DEALLOCATE ( va      )
  DEALLOCATE ( wa      )
  DEALLOCATE ( zpbl    )

  IF ( ift2m ) THEN  ! 2-m temperature available
    DEALLOCATE ( t2    )
  ENDIF

  IF ( ifw10m ) THEN  ! 10-m wind components available
    DEALLOCATE ( u10   )
    DEALLOCATE ( v10   )
  ENDIF

  IF ( met_model == 2 ) THEN  ! WRF
    DEALLOCATE ( mu    )
    DEALLOCATE ( mub   )
    DEALLOCATE ( pb    )
    DEALLOCATE ( ph    )
    DEALLOCATE ( phb   )
  ENDIF

  IF ( px ) THEN  ! Pleim-Xiu land-surface model
    DEALLOCATE ( isltyp )
    DEALLOCATE ( lai    )
    DEALLOCATE ( mol    )
    DEALLOCATE ( ra     )
    DEALLOCATE ( rnet   )
    DEALLOCATE ( rstom  )
    DEALLOCATE ( soilt1 )
    DEALLOCATE ( soilt2 )
    DEALLOCATE ( veg    )
    DEALLOCATE ( vegold )
    DEALLOCATE ( w2     )
    DEALLOCATE ( wg     )
    DEALLOCATE ( wr     )
  ENDIF

END SUBROUTINE dealloc_met
