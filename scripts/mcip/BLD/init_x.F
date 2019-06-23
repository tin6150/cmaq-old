
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
! $Header: /project/work/rep/MCIP2/src/mcip2/init_x.F,v 1.6 2005/08/29 14:32:59 tlotte Exp $ 


SUBROUTINE init_x

!-------------------------------------------------------------------------------
! Name:     Initialize X arrays.
! Purpose:  Initializes X arrays.
! Revised:  26 Jan 1997  Created for MCIP and generalized CTM.  (D. Byun)
!           04 Feb 1998  Changed include method nonglobal includes.  (D. Byun)
!           30 Apr 1999  Replaced PSTAR with PRSFC.  (D. Byun)
!           19 Sep 2001  Converted to free-form f90.  Removed SDATE and STIME
!                        from routine.  Changed routine name from INITX to
!                        INIT_X.  (T. Otte)
!           14 Jan 2002  Added new dry deposition species, methanol.
!                        (Y. Wu and T. Otte)
!           23 Jan 2002  Changed initialization of X-variables from 0.0 to
!                        BADVAL3 to avoid confusion.  (T. Otte)
!           27 Feb 2002  Renamed XSURF1 as XTEMP1P5 and XSURF2 as XWIND10.
!                        (T. Otte)
!           18 Mar 2003  Removed XJDRATE.  (T. Otte)
!           09 Jun 2003  Added XF2DEF, XSNOCOV, XDELTA, XLSTWET, XRH.  Added
!                        new dry deposition species:  N2O5, NO3, and generic
!                        aldehyde.  (D. Schwede, T. Otte, and J. Pleim)
!                        Removed extraneous variables from output.  (T. Otte)
!           09 Aug 2004  Added XQGRAUP, XWSPD10, XWDIR10, and XT2.  Removed
!                        XFLAGS, XINDEX, and XNAMES.  (T. Otte and D. Schwede)
!           01 Dec 2004  Added XPURB.  (T. Otte)
!           04 Apr 2005  Removed unused variables XREGIME and XRTOLD.  Added
!                        initialization of WRF variables.  Changed XUU and XVV
!                        to XUU_D and XVV_D, and changed XUHAT and XVHAT to
!                        XUU_S and XVV_T.  Added pointer indices for optional
!                        chlorine and mercury species.  Removed XENTRP.  Added
!                        XU10 and XV10.  (T. Otte, S.-B. Kim, G. Sarwar, and
!                        R. Bullock)
!           19 Aug 2005  Removed initialization of XDEPIDX and XVD.  Moved
!                        XDEPSPC to INIT_DEPV.  Removed unused variables XCAPG,
!                        XMMPASS, and XFSOIL.  Removed array XRH and made it a
!                        local scalar in M3DRY.  (T. Otte and W. Hutzell)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE parms3, ONLY: badval3
  USE metinfo, ONLY: met_model, ifw10m

  IMPLICIT NONE
 
!-------------------------------------------------------------------------------
! Initialize X-arrays.
!-------------------------------------------------------------------------------

  xx3face (:)     = badval3  ;    xx3midl (:)     = badval3

  xalbedo (:,:)   = badval3  ;    xcfrach (:,:)   = badval3
  xcfracl (:,:)   = badval3  ;    xcfracm (:,:)   = badval3
  xcfract (:,:)   = badval3  ;    xcldbot (:,:)   = badval3
  xcldtop (:,:)   = badval3  ;    xdelta  (:,:)   = badval3
  xdenss  (:,:)   = badval3  ;    xdluse  (:,:)   = badval3
  xemiss  (:,:)   = badval3  ;    xf2def  (:,:)   = badval3
  xglw    (:,:)   = badval3  ;    xgsw    (:,:)   = badval3
  xhfx    (:,:)   = badval3  ;    xlai    (:,:)   = badval3
  xlatc   (:,:)   = badval3  ;    xlatd   (:,:)   = badval3
  xlonc   (:,:)   = badval3  ;    xlond   (:,:)   = badval3
  xlstwet (:,:)   = badval3  ;    xmapc   (:,:)   = badval3
  xmapd   (:,:)   = badval3  ;    xmol    (:,:)   = badval3
  xpbl    (:,:)   = badval3  ;    xprsfc  (:,:)   = badval3
  xqfx    (:,:)   = badval3  ;    xrainc  (:,:)   = badval3
  xrainn  (:,:)   = badval3  ;    xrgrnd  (:,:)   = badval3
  xrib    (:,:)   = badval3  ;    xrnet   (:,:)   = badval3
  xrstmin (:,:)   = badval3  ;    xrstom  (:,:)   = badval3
  xsltyp  (:,:)   = badval3  ;    xsnocov (:,:)   = badval3
  xt2a    (:,:)   = badval3  ;    xtemp10 (:,:)   = badval3
  xtemp1p5(:,:)   = badval3  ;    xtempg  (:,:)   = badval3
  xtga    (:,:)   = badval3  ;    xtopo   (:,:)   = badval3
  xveg    (:,:)   = badval3  ;    xw2a    (:,:)   = badval3
  xwbar   (:,:)   = badval3  ;    xwdir10 (:,:)   = badval3
  xwga    (:,:)   = badval3  ;    xwr     (:,:)   = badval3
  xwspd10 (:,:)   = badval3  ;    xwstar  (:,:)   = badval3
  xzruf   (:,:)   = badval3

  IF ( t2out ) THEN
    xt2   (:,:)   = badval3
  ENDIF

  IF ( ifw10m ) THEN
    xu10  (:,:)   = badval3  ;    xv10    (:,:)   = badval3
  ENDIF

  IF ( iflufrc ) THEN
    xpurb (:,:)   = badval3
  ENDIF

  IF ( met_model == 2 ) THEN  ! WRF
    xmu   (:,:)   = badval3
    xgeof (:,:,:) = badval3
  ENDIF

  x3htf   (:,:,:) = badval3  ;    x3htm   (:,:,:) = badval3
  x3jacobf(:,:,:) = badval3  ;    x3jacobm(:,:,:) = badval3
  xcldwtr (:,:,:) = badval3  ;    xdensam (:,:,:) = badval3
  xdenswm (:,:,:) = badval3  ;    xdx3htf (:,:,:) = badval3
  xluse   (:,:,:) = badval3  ;    xpresm  (:,:,:) = badval3
  xqgraup (:,:,:) = badval3  ;    xqice   (:,:,:) = badval3
  xqsnow  (:,:,:) = badval3  ;    xradyn  (:,:,:) = badval3
  xranwtr (:,:,:) = badval3  ;    xrbndy  (:,:,:) = badval3
  xtempm  (:,:,:) = badval3  ;    xustar  (:,:,:) = badval3
  xuu_d   (:,:,:) = badval3  ;    xuu_s   (:,:,:) = badval3
  xvv_d   (:,:,:) = badval3  ;    xvv_t   (:,:,:) = badval3
  xwhat   (:,:,:) = badval3  ;    xwvapor (:,:,:) = badval3
  xwwind  (:,:,:) = badval3

END SUBROUTINE init_x
