
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
! $Header: /project/work/rep/MCIP2/src/mcip2/xvars_mod.F,v 1.6 2005/08/29 14:38:43 tlotte Exp $ 


MODULE xvars

!-------------------------------------------------------------------------------
! Name:     X-Variables
! Purpose:  Contains X-variables (CTM arrays plus boundary in horizontal).
! Revised:  25 Jan 1997  Original version.  (D. Byun)
!           20 May 1997  For Models-3 Beta Version.  (???)
!           05 Nov 1997  Added nonhydrostatic/hydrostatic output fnc.  (???)
!           05 Jan 1998  Added mass consistency error, XMCONERR.  (???)
!           30 Apr 1999  Replaced PSTAR with PRSFC.  (???)
!           19 Sep 2001  Converted to free-form f90 and changed name from
!                        MCIPCOM.EXT to module_xvars.f90.  Added XQICE
!                        and XQSNOW.  Removed user-definable control parameters
!                        LWINDMOD, LNOMAPS, LNOTOPO, LM3DDEP, LSANITY, and
!                        LMASSOP.  Changed arrays to allocatable.  Removed
!                        COMMON blocks.  Moved METCOL, METROW, and METLAY to
!                        MCIPPARM.  Removed MET1, MET2, LUSE, and KFFILE since
!                        they are not used.  Changed input date variables.
!                        Moved user input variables to MCIPPARM.  Changed
!                        routine to XVARS.  Added PX variables.  Removed unused
!                        arrays for LAMDA and MCONERR.  (T. Otte)
!           14 Jan 2002  Added new dry deposition species, methanol.
!                        (Y. Wu and T. Otte)
!           27 Feb 2002  Renamed XSURF1 as XTEMP1P5 and XSURF2 as XWIND10.
!                        (T. Otte)
!           18 Mar 2003  Removed XJDRATE.  (T. Otte)
!           09 Jun 2003  Added XF2DEF, XSNOCOV, XDELTA, XLSTWET, and XRH.
!                        Added new dry deposition species:  N2O5, NO3, and
!                        generic aldehyde.  Removed dry deposition species,
!                        ATRA and ATRAP, from output.  (T. Otte, J. Pleim,
!                        and D. Schwede)
!           10 Aug 2004  Added XQGRAUP, XWSPD10, XWDIR10, and XT2.  Removed
!                        XFLAGS, XINDEX, XNAMES, the pointers to XNAMES, and
!                        XLUSNAME.  (T. Otte and D. Schwede)
!           29 Nov 2004  Added XPURB.  (T. Otte)
!           04 Apr 2005  Removed unused variables XREGIME, XRTOLD, XPRSOLD,
!                        XENTRP, and XDENSAM_REF.  Moved XDFLUX and XPSRATE
!                        as local variables in VERTHYD.  Added XMU and XGEOF
!                        for WRF.  Changed XUU and XVV to XUU_D and XVV_D, and
!                        changed XUHAT and XVHAT to XUU_S and XVV_T.  Added
!                        pointers for optional chlorine and mercury species.
!                        Added XU10 and XV10.  (T. Otte, S.-B. Kim, G. Sarwar,
!                        and R. Bullock)
!           19 Aug 2005  Removed XDEPIDX and pointers to XDEPIDX.  Moved
!                        XDEPSPC and XVD to DEPVVARS_MOD.  Removed unused
!                        variables XCAPG, XMMPASS, XFSOIL, and X_RESOL.  Removed
!                        XRH and made it a local scalar in M3DRY.  (T. Otte and
!                        W. Hutzell)
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Scalars and One-Dimensional Arrays.
!-------------------------------------------------------------------------------

  REAL              :: x3top             ! top of X-array data

  REAL, ALLOCATABLE :: xx3face ( : )     ! layer face of X-array data
  REAL, ALLOCATABLE :: xx3midl ( : )     ! layer middle of X-array data
  REAL, ALLOCATABLE :: xdx3    ( : )     ! layer thickness (positive always)

!-------------------------------------------------------------------------------
! Dot-Point 2D Arrays.
!-------------------------------------------------------------------------------

  REAL, ALLOCATABLE :: xlatd ( : , : )   ! latitude  at DOT [degrees]
  REAL, ALLOCATABLE :: xlond ( : , : )   ! longitude at DOT [degrees]
  REAL, ALLOCATABLE :: xmapd ( : , : )   ! map scale at DOT [dim'less]

!-------------------------------------------------------------------------------
! Cross-Point 2D Arrays.
!-------------------------------------------------------------------------------

  REAL, ALLOCATABLE :: xlatc  ( : , : )  ! latitude  at CROSS [degree]
  REAL, ALLOCATABLE :: xlonc  ( : , : )  ! longitude at CROSS [degree]
  REAL, ALLOCATABLE :: xmapc  ( : , : )  ! map scale at CROSS [dim'less]
  REAL, ALLOCATABLE :: xtopo  ( : , : )  ! topographic height (MSL) [m]

  REAL, ALLOCATABLE :: xprsfc ( : , : )  ! sfc pressure at cross [Pa]
  REAL, ALLOCATABLE :: xdenss ( : , : )  ! surface air density
  REAL, ALLOCATABLE :: xtempg ( : , : )  ! ground surface temperature [K]
  REAL, ALLOCATABLE :: xrainn ( : , : )  ! nonconvective rain (cumulative)
  REAL, ALLOCATABLE :: xrainc ( : , : )  ! convective rain (cumulative)
  REAL, ALLOCATABLE :: xdluse ( : , : )  ! dominant land use category
  REAL, ALLOCATABLE :: xpurb  ( : , : )  ! percentage of urban area [%]

  REAL, ALLOCATABLE :: xemiss  ( : , : )     ! surface emissivity
  REAL, ALLOCATABLE :: xglw    ( : , : )
  REAL, ALLOCATABLE :: xgsw    ( : , : )
  REAL, ALLOCATABLE :: xhfx    ( : , : )
  REAL, ALLOCATABLE :: xqfx    ( : , : )
  REAL, ALLOCATABLE :: xustar  ( : , : , : ) ! 0 = avg, others = LUSE specific
  REAL, ALLOCATABLE :: xpbl    ( : , : )
  REAL, ALLOCATABLE :: xzruf   ( : , : )     ! surface roughness [m]
  REAL, ALLOCATABLE :: xmol    ( : , : )     ! Monin-Obukhov length [m] 
  REAL, ALLOCATABLE :: xcfrach ( : , : )
  REAL, ALLOCATABLE :: xcfracm ( : , : )
  REAL, ALLOCATABLE :: xcfracl ( : , : )
  REAL, ALLOCATABLE :: xrgrnd  ( : , : )
  REAL, ALLOCATABLE :: xrnet   ( : , : )

  REAL, ALLOCATABLE :: xwstar  ( : , : )
  REAL, ALLOCATABLE :: xrib    ( : , : )
  REAL, ALLOCATABLE :: xradyn  ( : , : , : ) ! 0 = avg, others = LUSE specific
  REAL, ALLOCATABLE :: xrbndy  ( : , : , : ) ! 0 = avg, others = LUSE specific
  REAL, ALLOCATABLE :: xrstom  ( : , : )
  REAL, ALLOCATABLE :: xtemp10 ( : , : )
  REAL, ALLOCATABLE :: xtemp1p5( : , : )
  REAL, ALLOCATABLE :: xwspd10 ( : , : )
  REAL, ALLOCATABLE :: xwdir10 ( : , : )
  REAL, ALLOCATABLE :: xalbedo ( : , : )
  REAL, ALLOCATABLE :: xmavail ( : , : )
  REAL, ALLOCATABLE :: xcfract ( : , : )
  REAL, ALLOCATABLE :: xcldtop ( : , : ) 
  REAL, ALLOCATABLE :: xcldbot ( : , : ) 
  REAL, ALLOCATABLE :: xwbar   ( : , : ) 
  REAL, ALLOCATABLE :: xrstmin ( : , : )
  REAL, ALLOCATABLE :: xsnocov ( : , : )     ! snow cover [1=yes, 0=no]
  REAL, ALLOCATABLE :: xdelta  ( : , : )     ! delta in dry deposition
  REAL, ALLOCATABLE :: xlstwet ( : , : )     ! wetness in dry deposition

  REAL, ALLOCATABLE :: xt2     ( : , : )     ! 2-m temperature
  REAL, ALLOCATABLE :: xu10    ( : , : )     ! 10-m u-component wind
  REAL, ALLOCATABLE :: xv10    ( : , : )     ! 10-m v-component wind

  REAL, ALLOCATABLE :: xtga    ( : , : )     ! ground temperature [K]
  REAL, ALLOCATABLE :: xt2a    ( : , : )     ! deep layer soil temperature [K]
  REAL, ALLOCATABLE :: xwga    ( : , : )     ! ground sfc soil moisture [m3/m3]
  REAL, ALLOCATABLE :: xw2a    ( : , : )     ! deep layer soil moisture [m3/m3]
  REAL, ALLOCATABLE :: xwr     ( : , : )     ! precip intercepted by canopy [m]
  REAL, ALLOCATABLE :: xlai    ( : , : )     ! leaf area index [area/area]
  REAL, ALLOCATABLE :: xveg    ( : , : )     ! vegetation coverage [fraction]
  REAL, ALLOCATABLE :: xsltyp  ( : , : )     ! soil texture type [category]
  REAL, ALLOCATABLE :: xf2def  ( : , : )     ! f2 as a function of land use

  REAL, ALLOCATABLE :: xluse   ( : , : , : ) ! landuse fractions [0-1]

!-------------------------------------------------------------------------------
! Cross-Point 3D arrays.
!-------------------------------------------------------------------------------

  REAL, ALLOCATABLE :: xtempm  ( : , : , : )  ! temp. at layer middle [K]
  REAL, ALLOCATABLE :: xpresm  ( : , : , : )  ! pressure at layer middle [Pa]
  REAL, ALLOCATABLE :: xdensam ( : , : , : )  ! air density at middle [kg/m^3]
  REAL, ALLOCATABLE :: xdenswm ( : , : , : )  ! vapor density at middle [kg/m^3]
  REAL, ALLOCATABLE :: x3jacobf( : , : , : )  ! Jacobian at layer face [m]
  REAL, ALLOCATABLE :: x3jacobm( : , : , : )  ! Jacobian at layer middle [m]
  REAL, ALLOCATABLE :: x3htf   ( : , : , : )  ! AGL height at layer face [m]
  REAL, ALLOCATABLE :: x3htm   ( : , : , : )  ! AGL height at layer middle [m]
  REAL, ALLOCATABLE :: xwhat   ( : , : , : )  ! contra-w wind at face [m/s]
  REAL, ALLOCATABLE :: xwvapor ( : , : , : )  ! vapor specific humidity [qv]
  REAL, ALLOCATABLE :: xwwind  ( : , : , : )  ! vertical wind at face [m/s]
  REAL, ALLOCATABLE :: xcldwtr ( : , : , : )  ! cloud water mixing ratio
  REAL, ALLOCATABLE :: xranwtr ( : , : , : )  ! rain water mixing ratio
  REAL, ALLOCATABLE :: xqice   ( : , : , : )  ! ice mixing ratio
  REAL, ALLOCATABLE :: xqsnow  ( : , : , : )  ! snow mixing ratio
  REAL, ALLOCATABLE :: xqgraup ( : , : , : )  ! graupel mixing ratio

!-------------------------------------------------------------------------------
! Dot-Point (and Face-Point) 3D Arrays.
!-------------------------------------------------------------------------------

  REAL, ALLOCATABLE :: xuu_d   ( : , : , : )  ! u comp. wind on dot pts [m/s]
  REAL, ALLOCATABLE :: xvv_d   ( : , : , : )  ! v comp. wind on dot pts [m/s]
  REAL, ALLOCATABLE :: xuu_s   ( : , : , : )  ! u comp. wind on flux pts [m/s]
  REAL, ALLOCATABLE :: xvv_t   ( : , : , : )  ! v comp. wind on flux pts [m/s]

!-------------------------------------------------------------------------------
! Arrays for WRF only.
!-------------------------------------------------------------------------------

  REAL, ALLOCATABLE :: xmu     ( : , : )      ! Mu at cross points
  REAL, ALLOCATABLE :: xgeof   ( : , : , : )  ! geopotential at face points

!-------------------------------------------------------------------------------
! Reference state variables for non-hydrostatic MM5.
!-------------------------------------------------------------------------------

  REAL, ALLOCATABLE :: xdensaf_ref ( : , : , : )  ! full-lvl ref dens  [Kg/m^3]

!-------------------------------------------------------------------------------
! Internal Arrays.
!-------------------------------------------------------------------------------

  REAL, ALLOCATABLE :: xdx3htf ( : , : , : )  ! layer thickness [m]
  REAL, ALLOCATABLE :: xdensaf ( : , : , : )  ! total air density at interface
  REAL, ALLOCATABLE :: xpresf  ( : , : , : )  ! total air pressure at face

END MODULE xvars
