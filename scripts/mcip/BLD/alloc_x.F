
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
! $Header: /project/work/rep/MCIP2/src/mcip2/alloc_x.F,v 1.6 2005/08/29 14:28:55 tlotte Exp $ 


SUBROUTINE alloc_x

!-------------------------------------------------------------------------------
! Name:     Allocate X Arrays
! Purpose:  Allocate X arrays with MCIP transfer array dimensions.
! Revised:  19 Sep 2001  Original version.  (T. Otte)
!           14 Jan 2002  Added new dry deposition species, methanol.
!                        (Y. Wu and T. Otte)
!           27 Feb 2002  Renamed SURF2 as WIND10 and SURF1 as TEMP1P5. (T. Otte)
!           18 Mar 2003  Removed XJDRATE.  Expanded dot-point array dimensions
!                        to be (NCOLS_X+1, NROWS_X+1).  (T. Otte)
!           09 Jun 2003  Added XF2DEF, XSNOCOV, XDELTA, XLSTWET, and XRH.
!                        Added new dry deposition species: N2O5, NO3, and
!                        generic aldehyde.  Removed dry deposition species,
!                        ATRA and ATRAP, from output.  (D. Schwede, T. Otte,
!                        and J. Pleim)
!           10 Aug 2004  Added XQGRAUP, XWSPD10, XWDIR10, and XT2.  Removed
!                        XFLAGS, XINDEX, XNAMES, and XLUSNAME.  (T. Otte and
!                        D. Schwede)
!           01 Dec 2004  Added XPURB.  (T. Otte)
!           04 Apr 2005  Removed unused variables XREGIME, XRTOLD, XPRSOLD, and
!                        XDENSAM_REF.  Moved XDFLUX and XPSRATE as local
!                        variables in VERTHYD.  Added XMU and XGEOF for WRF.
!                        Changed XUU and XVV to XUU_D and XVV_D, and changed
!                        XUHAT and XVHAT to XUU_S and XVV_T.  Added pointer
!                        indices for optional chlorine and mercury species.
!                        Removed XENTRP.  Added XU10 and XV10.  (T. Otte)
!           19 Aug 2005  Removed XDEPIDX and pointers to XDEPIDX.  Moved
!                        XDEPSPC and XVD to ALLOC_DEPV.  Removed unused
!                        variables XCAPG, XMMPASS, and XFSOIL.  Removed XRH and
!                        made it a local scalar in M3DRY.  (T. Otte and
!                        W. Hutzell)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE metinfo, ONLY: met_model, ifw10m

!-------------------------------------------------------------------------------
! Scalars and One-Dimensional Arrays  
!-------------------------------------------------------------------------------

  ALLOCATE ( xx3face (0:metlay) )
  ALLOCATE ( xx3midl (  metlay) )
  ALLOCATE ( xdx3    (  metlay) )

!-------------------------------------------------------------------------------
! Dot-Point 2D Arrays.
!-------------------------------------------------------------------------------

  ALLOCATE ( xlatd  (ncols_x+1, nrows_x+1) )
  ALLOCATE ( xlond  (ncols_x+1, nrows_x+1) )
  ALLOCATE ( xmapd  (ncols_x+1, nrows_x+1) )

!-------------------------------------------------------------------------------
! Cross-Point 2D Arrays.
!-------------------------------------------------------------------------------

  ALLOCATE ( xlatc  (ncols_x, nrows_x) )
  ALLOCATE ( xlonc  (ncols_x, nrows_x) )
  ALLOCATE ( xmapc  (ncols_x, nrows_x) )
  ALLOCATE ( xtopo  (ncols_x, nrows_x) )

  ALLOCATE ( xprsfc (ncols_x, nrows_x) )
  ALLOCATE ( xdenss (ncols_x, nrows_x) )
  ALLOCATE ( xtempg (ncols_x, nrows_x) )
  ALLOCATE ( xrainn (ncols_x, nrows_x) )
  ALLOCATE ( xrainc (ncols_x, nrows_x) )
  ALLOCATE ( xdluse (ncols_x, nrows_x) )

  IF ( iflufrc ) THEN
    ALLOCATE ( xpurb  (ncols_x, nrows_x) )
  ENDIF

  ALLOCATE ( xemiss  (ncols_x, nrows_x)           )
  ALLOCATE ( xglw    (ncols_x, nrows_x)           )
  ALLOCATE ( xgsw    (ncols_x, nrows_x)           )
  ALLOCATE ( xhfx    (ncols_x, nrows_x)           )
  ALLOCATE ( xqfx    (ncols_x, nrows_x)           )
  ALLOCATE ( xustar  (ncols_x, nrows_x, 0:maxluc) )
  ALLOCATE ( xpbl    (ncols_x, nrows_x)           )
  ALLOCATE ( xzruf   (ncols_x, nrows_x)           )
  ALLOCATE ( xmol    (ncols_x, nrows_x)           )
  ALLOCATE ( xcfrach (ncols_x, nrows_x)           )
  ALLOCATE ( xcfracm (ncols_x, nrows_x)           )
  ALLOCATE ( xcfracl (ncols_x, nrows_x)           )
  ALLOCATE ( xrgrnd  (ncols_x, nrows_x)           )
  ALLOCATE ( xrnet   (ncols_x, nrows_x)           )
  ALLOCATE ( xtga    (ncols_x, nrows_x)           )
  ALLOCATE ( xt2a    (ncols_x, nrows_x)           )
  ALLOCATE ( xwga    (ncols_x, nrows_x)           )
  ALLOCATE ( xw2a    (ncols_x, nrows_x)           )
  ALLOCATE ( xwr     (ncols_x, nrows_x)           )
  ALLOCATE ( xlai    (ncols_x, nrows_x)           )
  ALLOCATE ( xveg    (ncols_x, nrows_x)           )
  ALLOCATE ( xsltyp  (ncols_x, nrows_x)           )
  ALLOCATE ( xf2def  (ncols_x, nrows_x)           )

  ALLOCATE ( xwstar  (ncols_x, nrows_x)           )
  ALLOCATE ( xrib    (ncols_x, nrows_x)           )
  ALLOCATE ( xradyn  (ncols_x, nrows_x, 0:maxluc) )
  ALLOCATE ( xrbndy  (ncols_x, nrows_x, 0:maxluc) )
  ALLOCATE ( xrstom  (ncols_x, nrows_x)           )
  ALLOCATE ( xtemp10 (ncols_x, nrows_x)           )
  ALLOCATE ( xtemp1p5(ncols_x, nrows_x)           )
  ALLOCATE ( xwspd10 (ncols_x, nrows_x)           )
  ALLOCATE ( xwdir10 (ncols_x, nrows_x)           )
  ALLOCATE ( xalbedo (ncols_x, nrows_x)           )
  ALLOCATE ( xmavail (ncols_x, nrows_x)           )
  ALLOCATE ( xcfract (ncols_x, nrows_x)           )
  ALLOCATE ( xcldtop (ncols_x, nrows_x)           )
  ALLOCATE ( xcldbot (ncols_x, nrows_x)           )
  ALLOCATE ( xwbar   (ncols_x, nrows_x)           )
  ALLOCATE ( xrstmin (ncols_x, nrows_x)           )
  ALLOCATE ( xsnocov (ncols_x, nrows_x)           )
  ALLOCATE ( xdelta  (ncols_x, nrows_x)           )
  ALLOCATE ( xlstwet (ncols_x, nrows_x)           )

  IF ( t2out ) THEN
    ALLOCATE ( xt2     (ncols_x, nrows_x)         )
  ENDIF

  IF ( ifw10m ) THEN
    ALLOCATE ( xu10    (ncols_x, nrows_x)         )
    ALLOCATE ( xv10    (ncols_x, nrows_x)         )
  ENDIF

  ALLOCATE ( xluse   (ncols_x, nrows_x, maxluc) )

!-------------------------------------------------------------------------------
! Cross-Point 3D Arrays.
!-------------------------------------------------------------------------------

  ALLOCATE ( xtempm   (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xpresm   (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xdensam  (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xdenswm  (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( x3jacobf (ncols_x, nrows_x, 0:metlay) )
  ALLOCATE ( x3jacobm (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( x3htf    (ncols_x, nrows_x, 0:metlay) )
  ALLOCATE ( x3htm    (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xwhat    (ncols_x, nrows_x, 0:metlay) )
  ALLOCATE ( xwvapor  (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xwwind   (ncols_x, nrows_x, 0:metlay) )
  ALLOCATE ( xcldwtr  (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xranwtr  (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xqice    (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xqsnow   (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xqgraup  (ncols_x, nrows_x,   metlay) )

!-------------------------------------------------------------------------------
! Dot-Point (and Face-Point) 3D Arrays.
!-------------------------------------------------------------------------------

  ALLOCATE ( xuu_d (ncols_x+1, nrows_x+1, metlay) )
  ALLOCATE ( xvv_d (ncols_x+1, nrows_x+1, metlay) )
  ALLOCATE ( xuu_s (ncols_x+1, nrows_x+1, metlay) )
  ALLOCATE ( xvv_t (ncols_x+1, nrows_x+1, metlay) )

!-------------------------------------------------------------------------------
! Variables for WRF only.
!-------------------------------------------------------------------------------

  IF ( met_model == 2 ) THEN  ! WRF
    ALLOCATE ( xmu   (ncols_x, nrows_x)           )
    ALLOCATE ( xgeof (ncols_x, nrows_x, 0:metlay) )
  ENDIF

!-------------------------------------------------------------------------------
! Reference state variables for non-hydrostatic MM5.
!-------------------------------------------------------------------------------

  IF ( met_model == 1 ) THEN  ! MM5
    ALLOCATE ( xdensaf_ref (ncols_x, nrows_x, 0:metlay) )
  ENDIF

!-------------------------------------------------------------------------------
! Internal Arrays.
!-------------------------------------------------------------------------------

  ALLOCATE ( xdx3htf (ncols_x, nrows_x,   metlay) )
  ALLOCATE ( xdensaf (ncols_x, nrows_x, 0:metlay) )
  ALLOCATE ( xpresf  (ncols_x, nrows_x, 0:metlay) )

END SUBROUTINE alloc_x
