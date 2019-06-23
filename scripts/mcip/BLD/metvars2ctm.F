
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
! $Header: /project/work/rep/MCIP2/src/mcip2/metvars2ctm.F,v 1.7 2006/03/01 20:13:02 tlotte Exp $


SUBROUTINE metvars2ctm

!-------------------------------------------------------------------------------
! Name:     Meteorology Variables to CTM (X) Grid
! Purpose:  Puts time-variant meteorology arrays on CTM (X) grid.
! Notes:    MM5 algorithms taken from original getmet_mm5.F.  Vegetation
!           information does not include seasonal changes.
! Revised:  20 Sep 2001  Original version.  (T. Otte)
!           03 Oct 2001  Updated calculation of XRNET.  (T. Otte and J. Pleim)
!           16 Oct 2001  Corrected error in translation between input
!                        domain and MCIP "X" domain.  (T. Otte)
!           21 Dec 2001  Changed order of variable declarations in interface
!                        to improve portability.  (S. Howard and T. Otte)
!           23 Jan 2002  Changed calls to "abort" to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  Added updates
!                        to XFLAGS for variables filled here.  Changed missing
!                        value on XRSTOM to BADVAL3.  (T. Otte)
!           26 Mar 2003  Simplified algorithm to map input meteorology to
!                        MCIP_X domain.  Enabled dot-point MCIP_X arrays to
!                        be filled to the correct dimensions.  (T. Otte)
!           29 May 2003  Added minimum leaf area index and vegetation values
!                        to represent winter conditions for USGS land use.
!                        LAI and VEG are based on values used by Vegeland (for
!                        P-X) in MM5.  Also added F2 as a function of USGS land
!                        use.  Added translation of snow cover to MCIP_X domain.
!                        (T. Otte, J. Pleim, and D. Schwede)
!           09 Aug 2004  Added graupel processing.  Added 2-m temperature, if
!                        it is available from incoming meteorology.  Removed
!                        XFLAGS.  (T. Otte and D. Schwede)
!           26 May 2005  Removed NDX and option to interpolate to finer scale
!                        meteorology.  Added optimization of loops using F90
!                        implicit loop structures.  Removed unused variables
!                        REGIME and MAVAIL.  Changed XUU and XVV to XUU_D and
!                        XVV_D, and changed definitions to be input model-
!                        specific.  Added definitions of XUU_S and XVV_T.
!                        Added contents of MET3DSUP.  Added WRF algorithms.
!                        Added processing of 10-m wind components.  Added
!                        floor value for QV.  (T. Otte)
!           19 Aug 2005  Removed unused variables FSOIL and XFSOIL.  Changed
!                        internal variable EPSILON to EPSILONQ to avoid
!                        confusion with F90 intrinsic function.  (T. Otte)
!           13 Sep 2005  Changed logic for defining surface density from 2-m
!                        temperature to prevent "null pointer" error with
!                        PGF90 compiler.  (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE metvars
  USE metinfo, inhyd => met_inhyd
  USE const
  USE const_pbl
  USE parms3, ONLY: badval3

  IMPLICIT NONE

  INTEGER                      :: c
  REAL                         :: deta
  INTEGER                      :: ec
  REAL,          PARAMETER     :: epsilonq   = 1.0e-30
  REAL,          PARAMETER     :: epsilonqv  = 1.0e-14
  INTEGER                      :: er
  REAL                         :: f_vtemp
  REAL                         :: f2def24    (24)
  REAL,          PARAMETER     :: giwrf      = 1.0 / 9.81
  REAL,          PARAMETER     :: gravi      = 1.0 / grav
  INTEGER                      :: i
  INTEGER                      :: ii
  INTEGER                      :: iil
  INTEGER                      :: ilu
  INTEGER                      :: jj
  INTEGER                      :: jjl
  INTEGER                      :: k
  INTEGER                      :: kp1
  REAL                         :: lai13      (13)
  REAL                         :: lai24      (24)
  REAL                         :: laimn24    (24)
  INTEGER                      :: lvl
  INTEGER                      :: lvlf1
  CHARACTER*16,  PARAMETER     :: pname      = 'METVARS2CTM'
  REAL,    SAVE, ALLOCATABLE   :: pstar      ( : , : )
  REAL                         :: qf
  REAL                         :: qmix
  INTEGER                      :: r
  REAL,          PARAMETER     :: rdwrf      = 287.0  ! [J/kg/K]
  REAL                         :: rst13      (13)
  REAL                         :: rst24      (24)
  REAL,          PARAMETER     :: rvwrf      = 461.6  ! [J/kg/K]
  INTEGER                      :: sc
  INTEGER                      :: sr
  REAL                         :: temp
  REAL                         :: tf
  REAL                         :: veg13      (13)
  REAL                         :: veg24      (24)
  REAL                         :: vegmn24    (24)
  REAL                         :: wgt1
  REAL                         :: wgt2
  REAL                         :: z0
  REAL                         :: z1
  REAL                         :: z2

  INTERFACE

    SUBROUTINE layht (xx3face, xx3midl, x3jacobf, x3jacobm, x3htf, x3htm)
      IMPLICIT NONE
      REAL,          INTENT(OUT)   :: x3htf      ( : , : , : )
      REAL,          INTENT(OUT)   :: x3htm      ( : , : , : )
      REAL,          INTENT(IN)    :: x3jacobf   ( : , : , : )
      REAL,          INTENT(IN)    :: x3jacobm   ( : , : , : )
      REAL,          INTENT(IN)    :: xx3face    ( : )
      REAL,          INTENT(IN)    :: xx3midl    ( : )
    END SUBROUTINE layht

  END INTERFACE

  DATA (lai13(i),i=1,13)   /   2.0,    3.0,    3.0,    5.0,    4.5,    5.0,   &
                               0.0,    2.0,    0.5,    1.0,    0.0,    5.0,   &
                               2.0 /

  DATA (rst13(i),i=1,13)   / 150.0,   70.0,   83.0,  183.0,  150.0,  200.0,   &
                            9999.0,  164.0,  100.0,  150.0, 9999.0,  200.0,   &
                             120.0 /

  DATA (veg13(i),i=1,13)   /   0.40,   0.95,   0.95,   0.95,   0.95,   0.95,  &
                               0.00,   0.75,   0.10,   0.20,   0.00,   0.90,  &
                               0.80 /

  DATA (lai24(i),i=1,24)   /   2.0,    3.0,    3.0,    3.0,    2.5,    4.0,   &
                               2.5,    3.0,    3.0,    2.0,    5.0,    5.0,   &
                               5.0,    4.0,    5.0,    0.0,    2.0,    5.0,   &
                               0.5,    1.0,    1.0,    1.0,    0.1,    0.1 /

  DATA (laimn24(i),i=1,24) /   0.5,    0.5,    0.5,    0.5,    1.0,    1.5,   &
                               1.0,    1.0,    1.0,    1.0,    1.0,    1.0,   &
                               4.0,    3.0,    2.0,    0.0,    1.0,    3.0,   &
                               0.2,    0.5,    0.5,    0.5,    0.1,    0.1 /

  DATA (rst24(i),i=1,24)   / 150.0,   70.0,   60.0,   70.0,   80.0,  180.0,   &
                              83.0,  200.0,  150.0,  120.0,  200.0,  175.0,   &
                             120.0,  175.0,  200.0, 9999.0,  164.0,  200.0,   &
                             100.0,  150.0,  200.0,  150.0,  100.0,  300.0 /

  DATA (veg24(i),i=1,24)   /   0.40,   0.95,   0.95,   0.95,   0.95,   0.95,  &
                               0.95,   0.70,   0.85,   0.80,   0.95,   0.95,  &
                               0.95,   0.90,   0.95,   0.00,   0.60,   0.90,  &
                               0.10,   0.20,   0.30,   0.20,   0.05,   0.05 /

  DATA (vegmn24(i),i=1,24) /   0.20,   0.15,   0.10,   0.15,   0.35,   0.40,  &
                               0.70,   0.50,   0.60,   0.60,   0.50,   0.50,  &
                               0.85,   0.80,   0.60,   0.00,   0.40,   0.80,  &
                               0.05,   0.10,   0.10,   0.05,   0.02,   0.02 /

  DATA (f2def24(i),i=1,24) /   0.80,   0.85,   0.98,   0.90,   0.80,   0.90,  &
                               0.70,   0.50,   0.60,   0.60,   0.90,   0.90,  &
                               0.90,   0.90,   0.90,   1.00,   0.99,   0.99,  &
                               0.30,   0.40,   0.50,   0.60,   0.20,   0.99 /

!------------------------------------------------------------------------------
! Statement functions.
!------------------------------------------------------------------------------
 
  ! Virtual Temperature [K] ... temp [K], qmix = specific humidity

  f_vtemp(temp,qmix) = temp * ( 1.0 + 0.6077 * qmix )

!-------------------------------------------------------------------------------
! Put time-variant cross-point arrays on MCIP_X grid.
!-------------------------------------------------------------------------------

  sc = x0
  ec = x0 + ncols_x - 1
  sr = y0
  er = y0 + nrows_x - 1

  xtempg (:,:)   = groundt (sc:ec,sr:er)
  xrainc (:,:)   = raincon (sc:ec,sr:er)  ! rain is already incremental here
  xrainn (:,:)   = rainnon (sc:ec,sr:er)  ! rain is already incremental here
  xpbl   (:,:)   = zpbl    (sc:ec,sr:er)
  xhfx   (:,:)   = hfx     (sc:ec,sr:er)
  xqfx   (:,:)   = qfx     (sc:ec,sr:er)
  xustar (:,:,0) = ust     (sc:ec,sr:er)
  xrgrnd (:,:)   = rgrnd   (sc:ec,sr:er)
  xglw   (:,:)   = glw     (sc:ec,sr:er)
  xzruf  (:,:)   = znt     (sc:ec,sr:er)
  xalbedo(:,:)   = albedo  (sc:ec,sr:er)
  xemiss (:,:)   = emiss   (sc:ec,sr:er)
  xsnocov(:,:)   = snowcovr(sc:ec,sr:er)

  xgsw   (:,:)   = xrgrnd(:,:) * ( 1.0 - xalbedo(:,:) )

  IF ( ift2m ) THEN
    xt2  (:,:)   = t2      (sc:ec,sr:er)
  ENDIF

  IF ( ifw10m ) THEN
    xu10 (:,:)   = u10     (sc:ec,sr:er)
    xv10 (:,:)   = v10     (sc:ec,sr:er)
  ENDIF

  IF ( px ) THEN

    xmol   (:,:)   =        mol   (sc:ec,sr:er)
    xrnet  (:,:)   =        rnet  (sc:ec,sr:er)
    xwga   (:,:)   =        wg    (sc:ec,sr:er)
    xw2a   (:,:)   =        w2    (sc:ec,sr:er)
    xwr    (:,:)   =        wr    (sc:ec,sr:er)
    xradyn (:,:,0) =        ra    (sc:ec,sr:er)
    xrstom (:,:)   =        rstom (sc:ec,sr:er)
    xlai   (:,:)   =        lai   (sc:ec,sr:er)
    xveg   (:,:)   =        veg   (sc:ec,sr:er)
    xsltyp (:,:)   = FLOAT( isltyp(sc:ec,sr:er) )
    xtga   (:,:)   =        soilt1(sc:ec,sr:er)
    xt2a   (:,:)   =        soilt2(sc:ec,sr:er)

    ! At water points, stomatal resistance is 0.0.  Since
    ! inverse of XRSTOM is output in metcro.F, need to avoid
    ! division by 0.0.  Use BADVAL3 as place-holder.

    WHERE ( xrstom == 0.0 ) xrstom = badval3

  ELSE

    xrnet(:,:) = xgsw(:,:) - ( xemiss(:,:) *  &
                             (stfblz * xtempg(:,:)**4 - xglw(:,:)) )

    DO c = 1, ncols_x
      ii = x0 + c - 1
      DO r = 1, nrows_x
        jj = y0 + r - 1

        ilu = landuse(ii,jj)

        IF ( ( met_lu_water == 7    ) .OR.  &
             ( met_lu_water == -999 ) ) THEN  ! "old" 13-category land use
          xlai   (c,r) = lai13(ilu)
          xrstmin(c,r) = rst13(ilu)
          xveg   (c,r) = veg13(ilu)
        ELSE IF ( met_lu_water == 16 ) THEN   ! USGS 24-category land use
          xrstmin(c,r) =   rst24(ilu)
          xf2def (c,r) = f2def24(ilu)
          IF ( met_season == 1 ) THEN         ! summer
            xveg (c,r) = veg24(ilu)
            xlai (c,r) = lai24(ilu)
          ELSE IF ( met_season == 2 ) THEN    ! winter
            xveg (c,r) = vegmn24(ilu)
            xlai (c,r) = laimn24(ilu)
          ENDIF
        ELSE
          WRITE (6,9000) met_lu_water
          GOTO 1001
        ENDIF

      ENDDO
    ENDDO

  ENDIF

  xtempm (:,:,:)  = ta (sc:ec,sr:er,:)
  xwvapor(:,:,:)  = qva(sc:ec,sr:er,:)
  xcldwtr(:,:,:)  = qca(sc:ec,sr:er,:)
  xranwtr(:,:,:)  = qra(sc:ec,sr:er,:)
  xqice  (:,:,:)  = qia(sc:ec,sr:er,:)
  xqsnow (:,:,:)  = qsa(sc:ec,sr:er,:)
  xqgraup(:,:,:)  = qga(sc:ec,sr:er,:)

  xwwind (:,:,0:) = wa(sc:ec,sr:er,1:)

  ! Ensure that very small (and sometimes negative!) values from WRF moisture
  ! fields are not used.  Here, EPSILONQ is the same minimum value as is set
  ! in metcro.F.  Floor value for XWVAPOR (EPSILONQV) is based on MM5 value.

  WHERE ( xwvapor < epsilonqv ) xwvapor = epsilonqv
  WHERE ( xcldwtr < epsilonq  ) xcldwtr = 0.0
  WHERE ( xranwtr < epsilonq  ) xranwtr = 0.0
  WHERE ( xqice   < epsilonq  ) xqice   = 0.0
  WHERE ( xqsnow  < epsilonq  ) xqsnow  = 0.0
  WHERE ( xqgraup < epsilonq  ) xqgraup = 0.0

!-------------------------------------------------------------------------------
! Put time-variant dot-point arrays on MCIP_X grid.  XUU_D and XVV_D are on
! B-grid (dot points).  XUU_S and XVV_T are on C-grid (face points).
!-------------------------------------------------------------------------------

  sc = x0
  ec = x0 + ncols_x
  sr = y0
  er = y0 + nrows_x

  IF ( met_model == 1 ) THEN  ! MM5: UA and VA on B-grid (dot points)

    xuu_d(:,:,:)         = ua(sc:ec,sr:er,:)
    xvv_d(:,:,:)         = va(sc:ec,sr:er,:)

    xuu_s(:,1:nrows_x,:) = 0.5 * (ua(sc:ec,sr:er-1,:) + ua(sc:ec,sr+1:er,:))
    xuu_s(:,nrows_x+1,:) = xuu_s(:,nrows_x,:)
    xvv_t(1:ncols_x,:,:) = 0.5 * (va(sc:ec-1,sr:er,:) + va(sc+1:ec,sr:er,:))
    xvv_t(ncols_x+1,:,:) = xvv_t(ncols_x,:,:)

  ELSE IF ( met_model == 2 ) THEN  ! WRF: UA and VA on C-grid (face points)

    xuu_d(:,1,        :) = ua(sc:ec,sr,:)
    xuu_d(:,2:nrows_x,:) = 0.5 * (ua(sc:ec,sr:er-2,:) + ua(sc:ec,sr+1:er-1,:))
    xuu_d(:,nrows_x+1,:) = ua(sc:ec,er-1,:)
    xvv_d(1,        :,:) = va(sc,sr:er,:)
    xvv_d(2:ncols_x,:,:) = 0.5 * (va(sc:ec-2,sr:er,:) + va(sc+1:ec-1,sr:er,:))
    xvv_d(ncols_x+1,:,:) = va(ec-1,sr:er,:)

    xuu_s(:,:,:)         = ua(sc:ec,sr:er,:)
    xvv_t(:,:,:)         = va(sc:ec,sr:er,:)

  ENDIF

!------------------------------------------------------------------------------
! Compute pressure.
!------------------------------------------------------------------------------

  sc = x0
  ec = x0 + ncols_x - 1
  sr = y0
  er = y0 + nrows_x - 1

  IF ( met_model == 1 ) THEN  ! MM5

    IF ( .NOT. ALLOCATED ( pstar ) ) ALLOCATE ( pstar ( ncols_x, nrows_x ) )

    pstar (:,:)   = psa(sc:ec,sr:er) + pp(sc:ec,sr:er,1)  ! PSA contains PSTAR
    xprsfc(:,:)   = pstar(:,:) + x3top
    xpresf(:,:,0) = xprsfc(:,:)

    IF ( inhyd == 1 ) THEN  ! nonhydrostatic MM5

      DO k = 1, metlay
        xpresm(:,:,k) = psa(sc:ec,sr:er) * (1.0 - xx3midl(k)) +  &
                        pp (sc:ec,sr:er,k) + x3top
        xpresf(:,:,k) = - pstar(:,:) * ( xx3face(k) - xx3midl(k) )  &
                        + xpresm(:,:,k)
      ENDDO

    ELSE  ! hydrostatic MM5

      DO k = 1, metlay
        xpresf(:,:,k) = pstar(:,:) * (1.0 - xx3face(k)) + x3top
        xpresm(:,:,k) = pstar(:,:) * (1.0 - xx3midl(k)) + x3top
      ENDDO

    ENDIF

  ELSE IF ( met_model == 2 ) THEN  ! WRF

    xpresm(:,:,:) = pb (sc:ec,sr:er,:) + pp(sc:ec,sr:er,:)
    xmu   (:,:)   = mub(sc:ec,sr:er)   + mu(sc:ec,sr:er)
    xgeof (:,:,:) = phb(sc:ec,sr:er,:) + ph(sc:ec,sr:er,:)

    ! As of WRFv2.0.3.1, PSFC = 0.0 at WRF initial time.

    IF ( MINVAL(psa) > 0.0 ) THEN
      xprsfc(:,:) = psa(sc:ec,sr:er)  ! PSA contains PSFC for WRF
    ELSE
      DO c = 1, ncols_x
        DO r = 1, nrows_x
          z0   = xgeof(c,r,0)   ! Z0, Z1, Z2 really defined as Zx/grav
          z1   = 0.5 * ( xgeof(c,r,0) + xgeof(c,r,1) )
          z2   = 0.5 * ( xgeof(c,r,1) + xgeof(c,r,2) )
          wgt1 = (z2 - z0) / (z2 - z1)
          wgt2 = 1.0 - wgt1
          xprsfc(c,r) = wgt1 * xpresm(c,r,1) + wgt2 * xpresm(c,r,2)
        ENDDO
      ENDDO
    ENDIF

    xpresf(:,:,0) = xprsfc(:,:)

    ! Calculate full-level pressure from geopotential and hydrostatic equation.
    ! Assume temperature at mid-layers is sufficient "average" between full
    ! levels.

    DO k = 1, metlay
      xpresf(:,:,k) = xpresf(:,:,k-1) *  &
                      EXP( (xgeof(:,:,k-1) - xgeof(:,:,k)) /  &
                           (rdgas * xtempm(:,:,k)) )
    ENDDO

  ENDIF

!------------------------------------------------------------------------------
! Compute density.
!------------------------------------------------------------------------------

  IF ( met_model == 1 ) THEN  ! MM5

    DO k = 1, metlay
      kp1 = MIN(k+1,metlay)

      DO c = 1, ncols_x
        DO r = 1, nrows_x

          xdensam(c,r,k) = xpresm(c,r,k) / ( rdgas *                    &
                           f_vtemp(xtempm(c,r,k), xwvapor(c,r,k)) )

          xdensaf(c,r,k) = 2.0 * xpresf(c,r,k) / ( rdgas *                &
                           ( f_vtemp(xtempm(c,r,k),   xwvapor(c,r,k)   )  &
                           + f_vtemp(xtempm(c,r,kp1), xwvapor(c,r,kp1) ) ) )

        ENDDO
      ENDDO
    ENDDO

    IF ( ift2m ) THEN
      IF ( MAXVAL(xt2) > 0.0 ) THEN
        DO c = 1, ncols_x
          DO r = 1, nrows_x
            xdensaf(c,r,0) = xpresf(c,r,0) / ( rdgas *  &
                             f_vtemp(xt2(c,r), xwvapor(c,r,1)) )
          ENDDO
        ENDDO
      ELSE  ! 2-m temp = 0 at init, use layer-1 temperature
        DO c = 1, ncols_x
          DO r = 1, nrows_x
            xdensaf(c,r,0) = xpresf(c,r,0) / ( rdgas *  &
                             f_vtemp(xtempm(c,r,1), xwvapor(c,r,1)) )
          ENDDO
        ENDDO
      ENDIF
    ELSE  ! use layer-1 temperature
      DO c = 1, ncols_x
        DO r = 1, nrows_x
          xdensaf(c,r,0) = xpresf(c,r,0) / ( rdgas *  &
                           f_vtemp(xtempm(c,r,1), xwvapor(c,r,1)) )
        ENDDO
      ENDDO
    ENDIF
    xdenss(:,:) = xdensaf(:,:,0)

  ELSE IF ( met_model == 2 ) THEN  ! WRF

    DO k = 1, metlay
      kp1 = MIN(k+1,metlay)

      DO c = 1, ncols_x
        DO r = 1, nrows_x

          ! Use formula for "alt" (total inverse density, alpha) from WRF's
          ! module_initialize_real.F, and replace potential temperature with
          ! temperature using Poisson's equation.  Density is 1./alt.  Note
          ! that this computed density matches well (to 6 or so decimal places)
          ! with WRF's prognostic density, expressed as 1./(alb+al); alb and al
          ! are not in the default WRF Registry as output to the history file
          ! (as of v2.0.3.1).  It only matches to 3 decimal places with 1./alt
          ! when alt is output in the WRF history file.

          xdensam(c,r,k) = ( xpresm(c,r,k) / ( rdwrf * xtempm(c,r,k) *  &
                             (1.0 + rvwrf*xwvapor(c,r,k)/rdwrf) ) )

          tf = 0.5 * (xtempm (c,r,k) + xtempm (c,r,kp1))
          qf = 0.5 * (xwvapor(c,r,k) + xwvapor(c,r,kp1))

          xdensaf(c,r,k) = ( xpresf(c,r,k) / ( rdwrf * tf *  &
                             (1.0 + rvwrf*qf/rdwrf) ) )

        ENDDO
      ENDDO
    ENDDO

    IF ( ( ift2m ) .AND. ( MAXVAL(xt2) > 0.0 ) ) THEN  ! 2-m temp = 0 at init
      xdensaf(:,:,0) = ( xpresf(:,:,0) / ( rdwrf * xt2(:,:) *      &
                         (1.0 + rvwrf*xwvapor(:,:,1)/rdwrf) ) )
    ELSE
      xdensaf(:,:,0) = ( xpresf(:,:,0) / ( rdwrf * xtempm(:,:,1) *  &
                         (1.0 + rvwrf*xwvapor(:,:,1)/rdwrf) ) )
    ENDIF
    xdenss(:,:) = xdensaf(:,:,0)

  ENDIF

  xdenswm(:,:,:) = xdensam(:,:,:) * xwvapor(:,:,:) / ( 1.0 + xwvapor(:,:,:) )

!-------------------------------------------------------------------------------
! If input meteorology has a time-varying vertical coordinate, compute Jacobian
! and layer heights.
!-------------------------------------------------------------------------------

  IF ( ( met_model == 1 ) .AND. (( inhyd /= 1 ) .OR. ( lhydout == 2 )) ) THEN

    DO k = 0, metlay
      x3jacobf(:,:,k) = gravi * pstar(:,:) / xdensaf(:,:,k)
      IF ( k == 0 ) CYCLE
      x3jacobm(:,:,k) = gravi * pstar(:,:) / xdensam(:,:,k)
    ENDDO

    CALL layht  (xx3face, xx3midl, x3jacobf, x3jacobm, x3htf, x3htm)

  ELSE IF ( met_model == 2 ) THEN

    DO k = 0, metlay
      ! Calculate Jacobian from WRF relation:
      !   J*g = d(phi)/d(eta) = d(g z)/d(eta) = - mu alpha = - mu/rho
      x3jacobf(:,:,k) = giwrf * xmu(:,:) / xdensaf(:,:,k)
      IF ( k == 0 ) CYCLE
      x3jacobm(:,:,k) = giwrf * xmu(:,:) / xdensam(:,:,k)
!!!   deta = xx3face(k) - xx3face(k-1)  ! negative delta eta
!!!   x3jacobm(:,:,k) = giwrf * (xgeof(:,:,k) - xgeof(:,:,k-1)) / deta
    ENDDO

    CALL layht  (xx3face, xx3midl, x3jacobf, x3jacobm, x3htf, x3htm)

  ENDIF

!-------------------------------------------------------------------------------
! Calculate height differences.
!-------------------------------------------------------------------------------

  DO k = 1, metlay
    xdx3htf(:,:,k) = x3htf(:,:,k) - x3htf(:,:,k-1)  ! X3HTF starts at 0 in vert
  ENDDO

!-------------------------------------------------------------------------------
! Calculate contravariant velocity (w-component).
!-------------------------------------------------------------------------------

  IF ( met_model == 1 ) THEN  ! MM5
    IF ( inhyd == 1 ) THEN
      SELECT CASE ( lhydout )
        CASE ( 1 )  ! non-hydrostatic output
          CALL vertnhy
        CASE ( 2 )  ! hydrostatic output
          CALL verthyd
        CASE DEFAULT
          WRITE (6,9100) lhydout
          GOTO 1001
      END SELECT
    ELSE
      CALL verthyd
    ENDIF
  ELSE IF ( met_model == 2 ) THEN  ! WRF
    CALL vertnhy_wrf
  ENDIF

!-------------------------------------------------------------------------------
! Deallocate arrays.
!-------------------------------------------------------------------------------

! DEALLOCATE ( pstar )  ! commented out to avoid memory fragmentation

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: METVARS2CTM',                        &
              /, 1x, '***   UNKNOWN LAND USE INPUT DATA SOURCE FROM MM5',  &
              /, 1x, '***   WATER CATEGORY IS ', i4,                       &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: METVARS2CTM',                        &
              /, 1x, '***   IMPROPER VALUE FOR LHYDOUT',                   &
              /, 1x, '***   LHYDOUT MUST BE 1 OR 2',                       &
              /, 1x, '***   LHYDOUT = ', i2,                               &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE metvars2ctm
