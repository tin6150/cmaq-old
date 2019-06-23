
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
! $Header: /project/work/rep/MCIP2/src/mcip2/dealloc_x.F,v 1.6 2005/08/29 14:30:33 tlotte Exp $ 


SUBROUTINE dealloc_x

!-------------------------------------------------------------------------------
! Name:     Deallocate X Arrays
! Purpose:  Deallocate X arrays with MCIP transfer array dimensions.
! Revised:  19 Sep 2001  Original version.  (T. Otte)
!           27 Feb 2002  Renamed XSURF1 as XTEMP1P5 and XSURF2 as XWIND10.
!                        (T. Otte)
!           18 Mar 2003  Removed XJDRATE.  (T. Otte)
!           29 May 2003  Added XF2DEF, XSNOCOV, XDELTA, XLSTWET, and XRH.
!                        (D. Schwede, T. Otte, and J. Pleim)
!           10 Aug 2004  Added XQGRAUP, XWSPD10, XWDIR10, and XT2.  Removed
!                        XFLAGS, XINDEX, XNAMES, and XLUSNAME.  Moved remaining
!                        contents of NULL_X to this routine to simplify code.
!                        (T. Otte and D. Schwede)
!           01 Dec 2004  Added XPURB.  (T. Otte)
!           04 Apr 2005  Removed unused variables XREGIME, XRTOLD, XPRSOLD, and
!                        XDENSAM_REF.  Moved XDFLUX and XPSRATE as local
!                        variables in VERTHYD.  Added XMU and XGEOF for WRF.
!                        Changed XUU and XVV to XUU_D and XVV_D, and changed
!                        XUHAT and XVHAT to XUU_S and XVV_T.  Added pointer
!                        indices for optional chlorine and mercury species.
!                        Removed XENTRP.  Added XU10 and XV10.  (T. Otte)
!           19 Aug 2005  Removed XDEPIDX and pointers to XDEPIDX.  Moved
!                        XDEPSPC and XVD to DEALLOC_DEPV.  Removed unused
!                        variables XCAPG, XMMPASS, and XFSOIL.  Removed XRH and
!                        made it a local scalar in M3DRY.  (T. Otte and
!                        W. Hutzell)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars

  IMPLICIT NONE

!-------------------------------------------------------------------------------
! Scalars and One-Dimensional Arrays  
!-------------------------------------------------------------------------------

  DEALLOCATE ( xx3face  )
  DEALLOCATE ( xx3midl  )
  DEALLOCATE ( xdx3     )

!-------------------------------------------------------------------------------
! Dot-Point 2D Arrays.
!-------------------------------------------------------------------------------

  DEALLOCATE ( xlatd )
  DEALLOCATE ( xlond )
  DEALLOCATE ( xmapd )

!-------------------------------------------------------------------------------
! Cross-Point 2D Arrays.
!-------------------------------------------------------------------------------

  DEALLOCATE ( xlatc  )
  DEALLOCATE ( xlonc  )
  DEALLOCATE ( xmapc  )
  DEALLOCATE ( xtopo  )

  DEALLOCATE ( xprsfc )
  DEALLOCATE ( xdenss )
  DEALLOCATE ( xtempg )
  DEALLOCATE ( xrainn )
  DEALLOCATE ( xrainc )
  DEALLOCATE ( xdluse )

  IF ( iflufrc ) THEN
    DEALLOCATE ( xpurb  )
  ENDIF

  DEALLOCATE ( xemiss  )
  DEALLOCATE ( xglw    )
  DEALLOCATE ( xgsw    )
  DEALLOCATE ( xhfx    )
  DEALLOCATE ( xqfx    )
  DEALLOCATE ( xustar  )
  DEALLOCATE ( xpbl    )
  DEALLOCATE ( xzruf   )
  DEALLOCATE ( xmol    )
  DEALLOCATE ( xcfrach )
  DEALLOCATE ( xcfracm )
  DEALLOCATE ( xcfracl )
  DEALLOCATE ( xrgrnd  )
  DEALLOCATE ( xrnet   )
  DEALLOCATE ( xtga    )
  DEALLOCATE ( xt2a    )
  DEALLOCATE ( xwga    )
  DEALLOCATE ( xw2a    )
  DEALLOCATE ( xwr     )
  DEALLOCATE ( xlai    )
  DEALLOCATE ( xveg    )
  DEALLOCATE ( xsltyp  )
  DEALLOCATE ( xf2def  )

  DEALLOCATE ( xwstar   )
  DEALLOCATE ( xrib     )
  DEALLOCATE ( xradyn   )
  DEALLOCATE ( xrbndy   )
  DEALLOCATE ( xrstom   )
  DEALLOCATE ( xtemp10  )
  DEALLOCATE ( xtemp1p5 )
  DEALLOCATE ( xwspd10  )
  DEALLOCATE ( xwdir10  )
  DEALLOCATE ( xalbedo  )
  DEALLOCATE ( xmavail  )
  DEALLOCATE ( xcfract  )
  DEALLOCATE ( xcldtop  )
  DEALLOCATE ( xcldbot  )
  DEALLOCATE ( xwbar    )
  DEALLOCATE ( xrstmin  )
  DEALLOCATE ( xsnocov  )
  DEALLOCATE ( xdelta   )
  DEALLOCATE ( xlstwet  )

  IF ( t2out ) THEN
    DEALLOCATE ( xt2    )
  ENDIF

  IF ( ALLOCATED ( xu10 ) ) DEALLOCATE ( xu10 )
  IF ( ALLOCATED ( xv10 ) ) DEALLOCATE ( xv10 )

  DEALLOCATE ( xluse    )

!-------------------------------------------------------------------------------
! Cross-Point 3D Arrays.
!-------------------------------------------------------------------------------

  DEALLOCATE ( xtempm   )
  DEALLOCATE ( xpresm   )
  DEALLOCATE ( xdensam  )
  DEALLOCATE ( xdenswm  )
  DEALLOCATE ( x3jacobf )
  DEALLOCATE ( x3jacobm )
  DEALLOCATE ( x3htf    )
  DEALLOCATE ( x3htm    )
  DEALLOCATE ( xwhat    )
  DEALLOCATE ( xwvapor  )
  DEALLOCATE ( xwwind   )
  DEALLOCATE ( xcldwtr  )
  DEALLOCATE ( xranwtr  )
  DEALLOCATE ( xqice    )
  DEALLOCATE ( xqsnow   )
  DEALLOCATE ( xqgraup  )

!-------------------------------------------------------------------------------
! Dot-Point (and Face-Point) 3D Arrays.
!-------------------------------------------------------------------------------

  DEALLOCATE ( xuu_d )
  DEALLOCATE ( xvv_d )
  DEALLOCATE ( xuu_s )
  DEALLOCATE ( xvv_t )

!-------------------------------------------------------------------------------
! Variables for WRF only.
!-------------------------------------------------------------------------------

  IF ( ALLOCATED (xmu)         ) DEALLOCATE ( xmu         )
  IF ( ALLOCATED (xgeof)       ) DEALLOCATE ( xgeof       )

!-------------------------------------------------------------------------------
! Reference state variables for non-hydrostatic MM5.
!-------------------------------------------------------------------------------

  IF ( ALLOCATED (xdensaf_ref) ) DEALLOCATE ( xdensaf_ref )

!-------------------------------------------------------------------------------
! Internal arrrays.
!-------------------------------------------------------------------------------

  DEALLOCATE ( xdx3htf )
  DEALLOCATE ( xdensaf )
  DEALLOCATE ( xpresf  )

END SUBROUTINE dealloc_x
