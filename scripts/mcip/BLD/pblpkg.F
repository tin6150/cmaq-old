
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
! $Header: /project/work/rep/MCIP2/src/mcip2/pblpkg.F,v 1.4 2005/08/29 14:35:45 tlotte Exp $ 


SUBROUTINE pblpkg (sdate, gmt)

!-------------------------------------------------------------------------------
! Name:     PBL Package
! Purpose:  Computes all the 2-D parameters for describing PBL and
!           meteorology.
! Notes:    For "MAXK" layer meteorology new method for the estimation of
!           surface fluxes following Byun (Nov 1990, Jan 1997).
! Revised:  17 Jan 1997  Original version.  (D. Byun)
!           04 Feb 1998  Changed include method nonglobal includes.  (D. Byun)
!           19 Sep 2001  Converted to free-form f90.  Removed dependence
!                        on module VGRD.  Changed arrays from over-sized
!                        MAXK dimension to allocatable on NLAYS.  Removed
!                        unused variable DTHDZ.  Removed ISESN from argument
!                        list since it's now in MCIPPARM.  Changed JUDATE to
!                        SDATE.  Removed argument PBLMIN from SFCFLUX. (T. Otte)
!           09 Jan 2002  Changed calls to "abort" to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  (T. Otte)
!           27 Feb 2002  Renamed XSURF1 as XTEMP1P5 and XSURF2 as XWIND10.
!                        (T. Otte)
!           26 Mar 2003  Enabled last row and column of XUU and XVV to be
!                        used in calculation of UNS and VNS.  Constrained
!                        calculation of non-PBL parameters with user
!                        option LRAD.  (T. Otte and J. Pleim)
!           04 Aug 2004  Changed local array allocation to occur only on
!                        initial call to subroutine to avoid memory
!                        fragmentation.  Removed XFLAGS.  Changed XWIND10
!                        to XWSPD10.  (T. Otte)
!           14 Feb 2005  Removed variable NDEP from module MCIPPARM, and made
!                        usage explicit here.  Use fill XMAVAIL from RADM tables
!                        regardless of setting for LRAD.  Changed wind speed
!                        calculation to use wind components native to the
!                        input meteorology model.  Captured layer 1 wind speed
!                        to use in wind direction calculation.  (T. Otte)
!           19 Jul 2005  Corrected FCORIS for Southern Hemisphere.  Added error-
!                        checking to prevent using this code too close to the
!                        equator.  (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE const_mete
  USE const_pbl
  USE lradmdat
  USE metinfo

  IMPLICIT NONE

  REAL                         :: a1                ! 9.*ZU/ABS(MOL)
  REAL,          PARAMETER     :: airdens    = 1.2250  ! [kg m^-3]
  REAL                         :: alatdeg           ! [deg]
  REAL                         :: albedo
  REAL                         :: amol              ! Monin-Obukhov length
  REAL,          PARAMETER     :: ang1       = deg2rad * 90.0 / 91.3125
  REAL                         :: arg
  REAL                         :: argch
  REAL                         :: b1                ! 9.*D /ABS(MOL)
  REAL                         :: betab             ! (4.7/0.74)*PBLHT/MOL
  INTEGER                      :: c
  REAL                         :: ckust             ! VKAR*USTAR
  REAL                         :: ckwst             ! VKAR*WSTAR
  REAL                         :: coszen
  INTEGER                      :: cp1
  REAL,          EXTERNAL      :: cvmgp
  REAL,          EXTERNAL      :: cvmgz
  REAL                         :: d
  REAL                         :: dd                ! displacement height
  REAL                         :: decrad            ! [rad]
  REAL                         :: densd
  REAL                         :: densw
  REAL                         :: drib
  REAL                         :: dtemp
  REAL,          PARAMETER     :: dtempmax   =  5.0 ! max sfc_T - air_T
  REAL,          PARAMETER     :: dtempmin   = -3.0 ! min sfc_t - air_T
  REAL,    SAVE, ALLOCATABLE   :: dtmpv      ( : , : )
  REAL                         :: e_aerk
  REAL,          PARAMETER     :: emsvtya    = 0.90 ! emiss of sky
  REAL,          PARAMETER     :: emsvtyg    = 0.97 ! emiss of grassy soil
  REAL                         :: f_ptemp
  REAL                         :: f_vtemp
  REAL,    SAVE, ALLOCATABLE   :: fcoris     ( : , : )
  REAL                         :: fint
  REAL,          INTENT(IN)    :: gmt
  REAL                         :: hfxmax
  REAL                         :: hpbl
  REAL                         :: hrangle
  REAL                         :: htmax
  INTEGER                      :: iflag
  INTEGER                      :: ilu
  INTEGER,       PARAMETER     :: iter       = 6
  INTEGER                      :: iz
  INTEGER                      :: k
  INTEGER                      :: kpbl
  INTEGER                      :: kpblbulk
  INTEGER                      :: kpblht
  INTEGER                      :: kpblmax
  INTEGER                      :: l
  REAL                         :: latrad                ! [rad]
  INTEGER                      :: lbnd
  INTEGER                      :: lbnd2
  REAL,    SAVE, ALLOCATABLE   :: pl         ( : )
  CHARACTER*16,  PARAMETER     :: pname      = 'PBLPKG'
  REAL                         :: pp
  REAL                         :: press
  REAL                         :: psfc
  REAL                         :: psurf
  REAL                         :: qht
  REAL,    SAVE, ALLOCATABLE   :: ql         ( : )
  REAL                         :: qmix
  REAL,    SAVE, ALLOCATABLE   :: qratio     ( : , : )  ! lyr 1 mx
  REAL,    SAVE, ALLOCATABLE   :: qsurf      ( : , : )  ! sfc mx rat
  REAL                         :: qvm
  REAL                         :: qvref
  REAL                         :: qvstar
  INTEGER                      :: r
  REAL                         :: radiat
  REAL                         :: rasp
  REAL                         :: rass
  REAL                         :: raum
  REAL                         :: raus
  REAL                         :: rhumid
  REAL,    SAVE, ALLOCATABLE   :: ribk       ( : )
  INTEGER                      :: rp1
  REAL                         :: rr
  INTEGER,       INTENT(IN)    :: sdate
  REAL,          PARAMETER     :: small      = 1.0e-4
  REAL                         :: temp
  REAL                         :: tempc
  REAL                         :: thm
  REAL                         :: thref
  REAL                         :: thstar
  REAL                         :: tht
  REAL,    SAVE, ALLOCATABLE   :: tl         ( : )
  REAL                         :: tranac
  REAL,          PARAMETER     :: tranacld   = 0.85  ! absorp transm for mid cld
  REAL                         :: transc
  REAL,          PARAMETER     :: transcld   = 0.60  ! scat transm for mid cld
  REAL                         :: transm
  REAL                         :: tsfc
  REAL,    SAVE, ALLOCATABLE   :: tsurf      ( : , : )
  REAL,          PARAMETER     :: ublmin     = 300.0 ! min UBL hgt for 50% urb
  REAL                         :: uht
  REAL,    SAVE, ALLOCATABLE   :: ul         ( : )
  REAL,          PARAMETER     :: ustmin     = 0.05  ! min USTAR for land
  REAL,          PARAMETER     :: ustminu    = 0.20  ! min USTAR for urban
  REAL                         :: uns
  REAL                         :: upbl
  REAL,    SAVE, ALLOCATABLE   :: usfc       ( : , : )
  REAL                         :: ustar
  REAL                         :: ustar1
  REAL,    SAVE, ALLOCATABLE   :: uustc      ( : , : )
  REAL                         :: vns
  REAL                         :: vpress
  REAL,    SAVE, ALLOCATABLE   :: vptemp     ( : , : )
  REAL,    SAVE, ALLOCATABLE   :: vptempk    ( : )
  REAL                         :: vptsurf
  REAL                         :: vsat
  REAL,    SAVE, ALLOCATABLE   :: vsfc       ( : , : )
  REAL                         :: wm
  REAL,    SAVE, ALLOCATABLE   :: wspeed     ( : , : , : )
  REAL                         :: x1                 ! Z1/PBLHT
  REAL                         :: x2                 ! Z2/PBLHT
  REAL,    SAVE, ALLOCATABLE   :: z0         ( : , : , : )
  REAL,    SAVE, ALLOCATABLE   :: z0b1       ( : , : )
  REAL                         :: zenith
  REAL                         :: zeta
  REAL,    SAVE, ALLOCATABLE   :: zf         ( : )
  REAL                         :: zf1
  REAL,    SAVE, ALLOCATABLE   :: zl         ( : )
  REAL                         :: zmean1
  REAL,    SAVE, ALLOCATABLE   :: zr         ( : )
  REAL                         :: zruf
  REAL                         :: zsl                ! sfc lyr hgt
  REAL                         :: ztemp
  REAL,    SAVE, ALLOCATABLE   :: ztop       ( : )
  REAL                         :: zu                 ! upr lim of integration
  REAL                         :: zu1
  REAL                         :: zu2

  INTERFACE

    SUBROUTINE pblpwr (ul, tl, ql, pl, zl, zf, metlay, ztemp, hpbl, amol,  &
                       zruf, tsfc, psfc, uht, tht, qht, pp, rr, iflag)
      IMPLICIT NONE
      REAL,          INTENT(IN)    :: amol
      REAL,          INTENT(IN)    :: hpbl
      INTEGER,       INTENT(OUT)   :: iflag
      INTEGER,       INTENT(IN)    :: metlay
      REAL,          INTENT(IN)    :: pl         ( : )
      REAL,          INTENT(OUT)   :: pp
      REAL,          INTENT(IN)    :: psfc
      REAL,          INTENT(OUT)   :: qht
      REAL,          INTENT(IN)    :: ql         ( : )
      REAL,          INTENT(OUT)   :: rr
      REAL,          INTENT(OUT)   :: tht
      REAL,          INTENT(IN)    :: tl         ( : )
      REAL,          INTENT(IN)    :: tsfc
      REAL,          INTENT(OUT)   :: uht
      REAL,          INTENT(IN)    :: ul         ( : )
      REAL,          INTENT(IN)    :: zf         ( : )
      REAL,          INTENT(IN)    :: zl         ( : )
      REAL,          INTENT(IN)    :: zruf
      REAL,          INTENT(IN)    :: ztemp
    END SUBROUTINE pblpwr

  END INTERFACE

!-------------------------------------------------------------------------------
! Statement functions.
!-------------------------------------------------------------------------------

  ! Aerodynamic resistance for a stable surface layer

  rass(d,zu,amol,ckust) = ( pro * LOG(zu / d) + betam * (zu-d) / amol ) / ckust


  ! Aerodynamic resistance for an unstable surface layer

  raus(a1,b1,ckust) = pro * LOG( ( ( 2.0 + a1 ) - 2.0 * SQRT( 1.0 + a1 ) ) *  &
          ( ( 2.0 + b1 ) + 2.0 * SQRT( 1.0 + b1 ) ) / ( a1 * b1 ) ) / ckust


  ! Aerodynamic resistance for a stable PBL

  rasp(x1,x2,betab,ckust) = pro * ( 2.0 * ( betab + 1.0 ) *                 &
          ( 1.0 / SQRT( 1.0 - x2 ) - 1.0 / SQRT( 1.0 - x1 ) ) +             &
          LOG( ABS( ( -1.0 + SQRT( 1.0 - x2 ) )                             &
               * ( 1.0 + SQRT( 1.0 - x1 ) ) / ( ( 1.0 + SQRT( 1.0 - x2 ) )  &
               * ( -1.0 + SQRT( 1.0 - x1 ) ) ) ) ) ) / ckust


  ! Aerodynamic resistance for a mixed layer

  raum(x1,x2,ckwst) = LOG( x2 * ( 1.0 - x1 ) / ( x1 * ( 1.0 - x2 ) ) ) / ckwst


  ! Saturation vapor pressure [Pa]

  e_aerk(tempc) = vp0 * EXP( 17.625 * tempc / ( 243.04 + tempc ) )


  ! Virtual temperature [K]

  f_vtemp(temp,qmix) = temp * ( 1.0 + 0.6077 * qmix )


  ! Potential temperature [K]

  f_ptemp(temp,press) = temp * ( 100000.0 / press )**(2.0/7.0)

!-------------------------------------------------------------------------------
! Allocate necessary arrays.
!-------------------------------------------------------------------------------
 
  IF ( .NOT. ALLOCATED ( dtmpv   ) )  &
    ALLOCATE ( dtmpv   ( ncols_x, nrows_x )              )
  IF ( .NOT. ALLOCATED ( fcoris  ) )  &
    ALLOCATE ( fcoris  ( ncols_x, nrows_x )              )
  IF ( .NOT. ALLOCATED ( pl      ) )  &
    ALLOCATE ( pl                           ( metlay )   )
  IF ( .NOT. ALLOCATED ( ql      ) )  &
    ALLOCATE ( ql                           ( metlay )   )
  IF ( .NOT. ALLOCATED ( qratio  ) )  &
    ALLOCATE ( qratio  ( ncols_x, nrows_x )              )
  IF ( .NOT. ALLOCATED ( qsurf   ) )  &
    ALLOCATE ( qsurf   ( ncols_x, nrows_x )              )
  IF ( .NOT. ALLOCATED ( ribk    ) )  &
    ALLOCATE ( ribk                         ( metlay )   )
  IF ( .NOT. ALLOCATED ( tl      ) )  &
    ALLOCATE ( tl                           ( metlay )   )
  IF ( .NOT. ALLOCATED ( tsurf   ) )  &
    ALLOCATE ( tsurf   ( ncols_x, nrows_x )              )
  IF ( .NOT. ALLOCATED ( ul      ) )  &
    ALLOCATE ( ul                           ( metlay )   )
  IF ( .NOT. ALLOCATED ( usfc    ) )  &
    ALLOCATE ( usfc    ( ncols_x, nrows_x )              )
  IF ( .NOT. ALLOCATED ( uustc   ) )  &
    ALLOCATE ( uustc   ( ncols_x, nrows_x )              )
  IF ( .NOT. ALLOCATED ( vptemp  ) )  &
    ALLOCATE ( vptemp  ( ncols_x, nrows_x )              )
  IF ( .NOT. ALLOCATED ( vptempk ) )  &
    ALLOCATE ( vptempk                      ( metlay )   )
  IF ( .NOT. ALLOCATED ( vsfc    ) )  &
    ALLOCATE ( vsfc    ( ncols_x, nrows_x )              )
  IF ( .NOT. ALLOCATED ( wspeed  ) )  &
    ALLOCATE ( wspeed  ( ncols_x, nrows_x,    metlay )   )
  IF ( .NOT. ALLOCATED ( z0      ) )  &
    ALLOCATE ( z0      ( ncols_x, nrows_x,    luradm )   )
  IF ( .NOT. ALLOCATED ( z0b1    ) )  &
    ALLOCATE ( z0b1    ( ncols_x, nrows_x )              )
  IF ( .NOT. ALLOCATED ( zf      ) )  &
    ALLOCATE ( zf                         ( 0:metlay )   )
  IF ( .NOT. ALLOCATED ( zl      ) )  &
    ALLOCATE ( zl                           ( metlay )   )
  IF ( .NOT. ALLOCATED ( zr      ) )  &
    ALLOCATE ( zr                           ( metlay+1 ) )
  IF ( .NOT. ALLOCATED ( ztop    ) )  &
    ALLOCATE ( ztop                         ( metlay )   )

!-------------------------------------------------------------------------------
! Calculate Coriolis factor.
!-------------------------------------------------------------------------------

  IF ( MINVAL(ABS(xlatc)) <= 1.0 ) THEN  ! +/- 1.0 deg lat of equator; arbitrary
    WRITE (6,9000)
    GOTO 1001
  ENDIF
 
  fcoris(:,:) = ABS( 2.0*eomega * SIN( deg2rad * xlatc(:,:) ) )  ! ABS for S.Hem

!-------------------------------------------------------------------------------
! Determine KPBLMAX.
!-------------------------------------------------------------------------------

  kpblmax = 1
  htmax   = pblmin

  DO k = 1, metlay

    ! Find maximum layer height for given K

    DO c = 1, ncols_x
      DO r = 1, nrows_x
        IF ( x3htf(c,r,k) > htmax ) htmax = x3htf(c,r,k)
      ENDDO
    ENDDO        

    ! Find out KPBLMAX.

    IF ( htmax < pblmax ) kpblmax = kpblmax + 1
               
  ENDDO

  kpblmax = MIN(kpblmax, metlay-2)  ! for safety

!-------------------------------------------------------------------------------
! Compute wind speed profile from true horizontal wind data.
!-------------------------------------------------------------------------------
 
  IF ( met_model == 1 ) THEN  ! MM5: use native dot-point winds

    DO k = 1, metlay
      DO c = 1, ncols_x
        cp1 = c + 1
        DO r = 1, nrows_x
          rp1 = r + 1

          uns = 0.25 * ( xuu_d(c,r,  k) + xuu_d(cp1,r,  k) +  &
                         xuu_d(c,rp1,k) + xuu_d(cp1,rp1,k) )

          vns = 0.25 * ( xvv_d(c,r,  k) + xvv_d(cp1,r,  k) +  &
                         xvv_d(c,rp1,k) + xvv_d(cp1,rp1,k) )

          wspeed(c,r,k) = MAX( 0.5, SQRT( uns**2 + vns**2 ) )

          IF ( k == 1 ) THEN
            usfc(c,r) = uns
            vsfc(c,r) = vns
          ENDIF

        ENDDO
      ENDDO        
    ENDDO

  ELSE IF ( met_model == 2 ) THEN  ! WRF: use native flux point winds

    DO k = 1, metlay
      DO c = 1, ncols_x
        cp1 = c + 1
        DO r = 1, nrows_x
          rp1 = r + 1

          uns           = 0.5 * ( xuu_s(c,r,k) + xuu_s(cp1,r,k) )
          vns           = 0.5 * ( xvv_t(c,r,k) + xvv_t(c,rp1,k) )
          wspeed(c,r,k) = MAX( 0.5, SQRT( uns**2 + vns**2 ) )

          IF ( k == 1 ) THEN
            usfc(c,r) = uns
            vsfc(c,r) = vns
          ENDIF

        ENDDO
      ENDDO        
    ENDDO

  ENDIF

!-------------------------------------------------------------------------------
! Compute difference of virtual temperature.
!-------------------------------------------------------------------------------
 
  DO c = 1, ncols_x
    DO r = 1, nrows_x

      ! Saturation vapor pressure over water [mb]

      ! Estimating water vapor at surface with TSURF = 0.5*(TEMPM(1)+TEMPG)
      ! assuming it has the same relative humidity as the layer 1 air,
      ! but at the reference temperature

      vsat       = e_aerk( xtempm(c,r,1) - stdtemp )
      vpress     = xdenswm(c,r,1) * rwvap * xtempm(c,r,1)

      rhumid     = vpress / vsat

      psurf      = xpresm(c,r,1) + grav*xdensam(c,r,1)*x3htm(c,r,1)  ! [Pa]
      tsurf(c,r) = 0.5 * ( xtempm(c,r,1) + xtempg(c,r) )
      vsat       = e_aerk( tsurf(c,r) - stdtemp )                    ! [Pa]

      vpress     = vsat * rhumid
      densw      = vpress / ( rwvap * tsurf(c,r) )
      densd      = xdensaf(c,r,0)

      qsurf(c,r) = densw / densd 

      ! Virtual temperature of ground and air above ground

      vptsurf = f_vtemp( tsurf(c,r), qsurf(c,r) )
      vptsurf = f_ptemp( vptsurf, psurf )

      qratio(c,r) = xdenswm(c,r,1) / ( xdensam(c,r,1) - xdenswm(c,r,1) )

      vptemp(c,r) = f_vtemp( xtempm(c,r,1), qratio(c,r) )
      vptemp(c,r) = f_ptemp( vptemp(c,r), xpresm(c,r,1) )

      dtmpv(c,r) = vptsurf - vptemp(c,r)
      IF ( dtmpv(c,r) < dtempmin ) dtmpv(c,r) = dtempmin 
      IF ( dtmpv(c,r) > dtempmax ) dtmpv(c,r) = dtempmax 
      dtmpv(c,r) = cvmgz( 1.0e-10, dtmpv(c,r), dtmpv(c,r) )
            
    ENDDO        
  ENDDO

!-------------------------------------------------------------------------------
! Calculate PBL height.
!-------------------------------------------------------------------------------
 
  DO c = 1, ncols_x
    DO r = 1, nrows_x
      kpblbulk = kpblmax

      ! Compute bulk Richardson number with respect to surface.

      DO k = 1, metlay

        vptempk(k) = f_vtemp( xtempm(c,r,k), qratio(c,r) )
        vptempk(k) = f_ptemp( vptempk(k), xpresm(c,r,k) )

        dtemp = - ( dtmpv(c,r) + vptempk(1) - vptempk(k) )
        dtemp = cvmgz( 1.0e-10, dtemp, dtemp )

        ribk(k) = grav * x3htm(c,r,k) * dtemp /  &
                  ( tsurf(c,r) * wspeed(c,r,k)**2 )
        ribk(k) = cvmgz( -1.0e-10, ribk(k), ribk(k) )

        IF ( dtemp > 0.1 ) kpblbulk = MIN(kpblbulk, k)

      ENDDO
            
      xrib(c,r) = ribk(1)

      ! Estimate PBL height.

      pblloop: DO k = 1, kpblmax
        IF ( ribk(k) >= rimax ) EXIT pblloop
      ENDDO pblloop

      IF ( k == 1 ) THEN
        kpbl = 1
        xpbl(c,r) = MAX(pblmin,x3htf(c,r,1))
      ELSE
        kpbl = k 
      ENDIF

      IF ( kpbl > 1 ) THEN

        drib = ribk(kpbl) - ribk(kpbl-1)

        IF ( ABS(drib) < 1.0e-6 ) THEN
          fint = 1.0
        ELSE
          fint = ( rimax - ribk(kpbl-1) ) / drib
        ENDIF

        IF ( fint > 0.5 ) THEN
          kpblht = kpbl
          fint   = fint - 0.5
        ELSE
          kpblht = kpbl - 1
          fint   = fint + 0.5
        ENDIF

        xpbl(c,r) = fint * ( x3htf(c,r,kpblht) - x3htf(c,r,kpblht-1) ) +  &
                    x3htf(c,r,kpblht-1)

        xpbl(c,r) = MAX( xpbl(c,r), pblmin )
        xpbl(c,r) = MAX( xpbl(c,r), x3htm(c,r,kpblbulk) )
        xpbl(c,r) = MIN( xpbl(c,r), pblmax )

      ENDIF

    ENDDO        
  ENDDO

  ! Modify min PBL height for urban cell

  DO c = 1, ncols_x
    DO r = 1, nrows_x
      upbl      = (1.0-xluse(c,r,1))*pblmin + xluse(c,r,1)*ublmin
      xpbl(c,r) = MAX(xpbl(c,r),upbl)
    ENDDO        
  ENDDO
      
!-------------------------------------------------------------------------------
! Calculate non-PBL parameters.
!-------------------------------------------------------------------------------

  ! Calculate time-independent moisture availability.
  xmavail(:,:) = 0.0
  DO ilu = 1, luradm
    xmavail(:,:) = xmavail(:,:) + mavtabl(ilu,isesn) * xluse(:,:,ilu)
  ENDDO

  IF ( lrad /= 1 ) THEN

    ! Calculate time-independent albedo.
    xalbedo(:,:) = 0.0
    DO ilu = 1, luradm
      xalbedo(:,:) = xalbedo(:,:) + albtabl(ilu,isesn) * xluse(:,:,ilu)
    ENDDO

    DO c = 1, ncols_x
      DO r = 1, nrows_x

        ! Insolation at given grid point from solar angle
        ! formula for gmt time at a longitude in North America

        ! local hour angle, inclination angle, and latitude angle [rad]

        hrangle = ( gmt + xlonc(c,r) / 15.0  - 12.0 ) * 15.0 * deg2rad
        decrad  = deg2rad * 23.5 * SIN( ( MOD(sdate, 1000) - 81.1875 ) * ang1 )
        latrad  = deg2rad * xlatc(c,r)

        ! Radiation calculation [W m^-2]

        coszen  = SIN( latrad ) * SIN( decrad ) +   &
                  COS( latrad ) * COS( decrad ) * COS( hrangle )

        zenith  = ACOS( coszen )
        arg     = 0.003286 * (zenith / deg2rad)**1.5
        albedo  = xalbedo(c,r) + 0.01 * ( EXP( arg ) - 1.0 )
        xalbedo(c,r) = MIN(albedo, 1.0)

        ! Previous hour's cloud fraction is used to estimate net radiation
        ! to set constraint on sensible heat flux (next section of this
        ! routine).  Radiative parameters calculated below will overwrite
        ! input meteorology, but are later overwritten in solar.F.

        tranac  = 1.0 - ( 1.0 - tranacld ) * MAX(xcfract(c,r), 0.0)
        transc  = 1.0 - ( 1.0 - transcld ) * MAX(xcfract(c,r), 0.0)
        transm  = tranac * transc / ( 1.0 - tranac * ( 1.0 - transc ) * albedo )

        radiat  = solcnst * coszen * transm  ! short-wave radiation

        xrgrnd(c,r) = MAX1( radiat, 0.0 )  
        xgsw  (c,r) = xrgrnd(c,r) * ( 1.0 - albedo )
        xglw  (c,r) = ( emsvtyg * stfblz * xtempg(c,r)**4 -  &
                        emsvtya * stfblz * xtempm(c,r,1)**4 )
        xrnet (c,r) = xgsw(c,r) - xglw(c,r)               ! net radiation

      ENDDO        
    ENDDO

  ENDIF

!-------------------------------------------------------------------------------
! Compute the logarithmic average surface roughness for each grid area
! Byun and Wesley's formula (in preparation)
!-------------------------------------------------------------------------------

  DO ilu = 1, luradm
    z0(:,:,ilu) = z00(ilu,isesn)
  ENDDO

  lbnd = LBOUND(xradyn,3)

  iterations: DO iz = 1, iter

    xzruf (:,:)      = 0.0
    z0b1  (:,:)      = 0.0
    xradyn(:,:,lbnd) = 0.0  ! for inverse...lbnd should be 0
               
    DO c = 1, ncols_x
      DO r = 1, nrows_x

        DO ilu = 1, luradm
          z0b1(c,r) = z0b1(c,r) + xluse(c,r,ilu) *  &
                      SQRT( LOG( zref / z0(c,r,ilu) ) )
        ENDDO

        xzruf(c,r) = zref * EXP( -z0b1(c,r)**2 )

      ENDDO
    ENDDO

    ! Now, XZRUF contains regular roughness length in m
    ! Recompute sea-surface rougness using Charnock's relation
    ! Recompute cell-average surface fluxes
    ! --- this part is not land use dependent ---
    ! ---   except for adjusting for water    ---

    DO c = 1, ncols_x
      DO r = 1, nrows_x

        alatdeg = xlatc (c,r)
        wm      = wspeed(c,r,1)
        thm     = vptemp(c,r)
        qvm     = qratio(c,r)
        thref   = dtmpv (c,r) + vptemp(c,r)
        qvref   = qsurf (c,r)
        zf1     = x3htf (c,r,1)
        zmean1  = x3htm (c,r,1)
        zruf    = xzruf (c,r)
        hpbl    = xpbl  (c,r)

        hfxmax  = ( 1.0 - cvmgp( 0.19, 0.32, xrnet(c,r) ) ) * xrnet(c,r)
        hfxmax  = MAX( small, hfxmax / 1255.8 )  ! [K m s^-1]

        SELECT CASE ( lpbl )

          CASE ( 2 )  ! Re-calculate values using PBL similarity
            CALL sfcflux (alatdeg, wm, thm, qvm, thref, qvref, zf1, zmean1,   &
                          zruf, hpbl, hfxmax, pp, rr, ustar, thstar,          & 
                          qvstar, amol)

          CASE ( 3 )  ! Re-calculate values using only surface layer similarity
            CALL slflux (alatdeg, wm, thm, qvm, thref, qvref, zf1, zmean1,  &
                         zruf, hfxmax, ustar, thstar, qvstar, amol)

          CASE DEFAULT
            WRITE (6,9100) lpbl
            GOTO 1001

        END SELECT

        ! PBL height modification based on surface layer similarity 
        ! predictions.

        ! For stable conditions update based on
        ! Zilitinkevich's formula. with CH = 0.8 & ALAMDAO = 0.07
        ! For unstable conditions update based on neutral value.
        ! Minimum unstable PBL height is modified by ( 0.07*USTAR / FCORI ).

        IF ( amol > 0.0 ) THEN
          argch = vkar * ch * ch * amol / alamdao
          hpbl  = 0.5 * ( - argch + SQRT( argch**2 +  &
                  4.0*vkar*ustar*ch*ch*amol / fcoris(c,r) ) )
        ELSE
          hpbl  = alamdao * ustar / fcoris(c,r)
        ENDIF

        xpbl(c,r) = MAX ( hpbl, xpbl(c,r) )
        upbl      = (1.0-xluse(c,r,1))*pblmin + xluse(c,r,1)*ublmin
        xpbl(c,r) = MAX ( xpbl(c,r), upbl)

        IF ( xluse(c,r,7) < 0.5 ) THEN 
          ustar1 = MAX ( ustar, ustmin )
        ELSE
          ustar1 = ustar
        ENDIF

        xustar(c,r,0) = MAX (ustminu, ustar1) * xluse(c,r,1) +   &
                        ustar1 * ( 1.0 - xluse(c,r,1) )

        xmol  (c,r) =   amol
        uustc (c,r) =   xustar(c,r,0) * wspeed(c,r,1)
        xhfx  (c,r) = - xustar(c,r,0) * thstar
        xwstar(c,r) =   xustar(c,r,0) * ( xpbl(c,r) /  &
                        ( vkar * ABS( xmol(c,r) ) ) )**0.3333333
        xwstar(c,r) = cvmgp( 0.0, xwstar(c,r), xmol(c,r) )

        xqfx  (c,r) = - xmavail(c,r) * xustar(c,r,0) * qvstar

      ENDDO
    ENDDO

    ! Ocean/water roughness is computed from friction velocity
    ! Charnock's relation a = 0.0185 (Wu, 1982)
    ! Charnock, h. (1955) wind stress on a water surface. 
    !   Q.J.R.M. Soc.81, 639-640.
    ! Wu, j. (1982) wind-stress coefficients over sea surface
    !   from breeze to huricane. J. Geophys. Res. 87,9704-9706.

    ! Adjustment of surface roughness for water surface where ustar is
    ! modified according to UUST = const and roughness adjusts accordingly

    ! Since we just care how the friction velocity will be modified
    ! for the different surface roughness for each landuse,
    ! just use neutral formula to update landuse dependent friction
    ! velocity: this is the same as assuming that stability and PBL height
    ! over different landuse is the same as for the cell average values.
    ! (Since sfc temp, profiles are cell averaged values, we cannot do
    ! any better)  Next eq. for USTAR is from Byun and Wesley
    ! (in preparation)

    xustar(:,:,lwater) = SQRT( vkar * uustc(:,:) /  &
                                     LOG( zref / z0(:,:,lwater) ) )
    z0    (:,:,lwater) = 0.0185 * xustar(:,:,lwater)**2 / grav + 0.0001

  ENDDO iterations

!-------------------------------------------------------------------------------
! Convert to Models-3 standard unit convention for heat flux [W m^-2]
!-------------------------------------------------------------------------------

  DO c = 1, ncols_x
    DO r = 1, nrows_x

      xhfx(c,r) = cpd * airdens * xhfx(c,r)

      thref     = dtmpv(c,r) + vptemp(c,r)               
      xqfx(c,r) = ( 2.501 - 0.00237 * (thref - stdtemp) ) *  &
                    1.0e06 * airdens * xqfx(c,r)

    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Compute land use dependent friction velocity.
!-------------------------------------------------------------------------------

  DO ilu = 1, luradm
    xustar(:,:,ilu) = SQRT( vkar * uustc(:,:) / LOG( zref / z0 (:,:,ilu) ) )
  ENDDO
      
!-------------------------------------------------------------------------------
! Compute aerodynamic resistance.
!-------------------------------------------------------------------------------

  raloop: DO ilu = 1, luradm

    ra_c: DO c = 1, ncols_x
      ra_r: DO r = 1, nrows_x
         
        zsl   = 0.1 * xpbl(c,r)
        ztemp = MIN( x3htm(c,r,1), zsl )
        zu1   = MAX( xzruf(c,r), ztemp )
            
        ! When setting integration limits, we need to avoid singular
        ! points: minimum ht is set to Z0 and maximum ht. is set to
        ! 0.9*PBLHT + 0.05 [m]

        zu2   = MAX( MIN( x3htm(c,r,1), xpbl(c,r) ), zu1) + 0.05
        x1    = MIN( 0.9, zu1 / xpbl(c,r) )
        x2    = MIN( 0.9 + 0.05 / xpbl(c,r), zu2 / xpbl(c,r) )
            
        betab = 6.345 * xpbl(c,r) / xmol(c,r)
        a1    = 9.0 * zu1 / ABS( xmol(c,r) )
        b1    = 9.0 * xzruf(c,r) / ABS( xmol(c,r) )
            
        ! Protect against x2 being greater than or equal to 1.0.

        IF ( x2 >= 0.9999 ) x2 = 0.9999
            
        xwstar(c,r) = xustar(c,r,0) * ( xpbl(c,r) /   &
                      ( vkar * ABS( xmol(c,r) ) ) )**0.3333333
     
        xwstar(c,r) = cvmgp(1.0e-20, xwstar(c,r), xmol(c,r) )

        IF ( zu2 < zsl ) THEN   ! surface resistance alone is enough

          xradyn(c,r,ilu) = cvmgp( rass(xzruf(c,r), zu1, xmol(c,r),        &
                                        vkar * xustar(c,r,ilu) ),          &
                                   raus(a1, b1, vkar * xustar(c,r,ilu)),   &
                                   xmol(c,r) )

        ELSE

          xradyn(c,r,ilu) = cvmgp( ( rass(xzruf(c,r), zu1, xmol(c,r),         &
                                          vkar * xustar(c,r,ilu))             &
                            + rasp(x1, x2, betab, vkar * xustar(c,r,ilu)) ),  &
                            ( raus(a1, b1, vkar * xustar(c,r,ilu))            &
                            + raum(x1, x2, vkar * xwstar(c,r)) ), xmol(c,r) )

        ENDIF

        ! Compute cell average (using inverse directly).

        xradyn(c,r,lbnd) = xradyn(c,r,lbnd) + xluse(c,r,ilu) / xradyn(c,r,ilu)

      ENDDO ra_r
    ENDDO ra_c

    ! Compute cell average (using inverse directly).

  ENDDO raloop

  ! Now XRADYN has real dynamic resistance.

  xradyn(:,:,lbnd) = 1.0 / xradyn(:,:,lbnd)

!-------------------------------------------------------------------------------
! Estimate temperatures and winds at 1.5 m and 10 m.
!-------------------------------------------------------------------------------

  lbnd  = LBOUND(zf,1)
  lbnd2 = LBOUND(x3htf,3)

  DO c = 1, ncols_x
    DO r = 1, nrows_x

      zf(lbnd) = x3htf(c,r,lbnd2)

      DO k = 1, metlay
        zl(k)      = x3htm  (c,r,k)
        zf(lbnd+k) = x3htf  (c,r,lbnd+k)
        ul(k)      = wspeed (c,r,k)
        tl(k)      = xtempm (c,r,k)
        ql(k)      = xdenswm(c,r,k) / ( xdensam(c,r,k) - xdenswm(c,r,k) )
        pl(k)      = xpresm (c,r,k) / 100.0    ! [hPa] or [mb]
      ENDDO

      tsfc  = tsurf(c,r)
      amol  = xmol (c,r)
      zruf  = xzruf(c,r)
      hpbl  = xpbl (c,r)

      psurf = xprsfc(c,r)    ! [Pa]
      psfc  = psurf / 100.0  ! [hPa] or [mb]

      ! Calculate near-surface wind.  10-m wind speed will be calculated using
      ! similarity theory in pblpwr.  10-m wind direction  is calculated from
      ! layer-1 wind components (i.e., no directional shear assumed between
      ! layer-1 and surface).

      CALL wind (usfc(c,r), vsfc(c,r), xwspd10(c,r), xwdir10(c,r),  &
                 xlonc(c,r), met_x_centd, met_cone_fac)

      ztemp = 10.0  ! [m]

      CALL pblpwr (ul, tl, ql, pl, zl, zf, metlay, ztemp, hpbl, amol,  &
                   zruf, tsfc, psfc, uht, tht, qht, pp, rr, iflag)

      xtemp10(c,r) = tht
      xwspd10(c,r) = uht   ! for reference

      ztemp = 1.5  ! [m]

      CALL pblpwr (ul, tl, ql, pl, zl, zf, metlay, ztemp, hpbl, amol,  &
                   zruf, tsfc, psfc, uht, tht, qht, pp, rr, iflag)

      xtemp1p5(c,r) = tht

    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Deallocate arrays.
!-------------------------------------------------------------------------------
 
! DEALLOCATE ( dtmpv   )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( fcoris  )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( pl      )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( ql      )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( qratio  )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( qsurf   )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( ribk    )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( tl      )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( tsurf   )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( ul      )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( usfc    )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( uustc   )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( vptemp  )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( vptempk )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( vsfc    )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( wspeed  )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( z0      )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( z0b1    )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( zf      )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( zl      )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( zr      )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( ztop    )  ! commented out to avoid memory fragmentation

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: PBLPKG',                         &
              /, 1x, '***   TOO CLOSE TO EQUATOR',                     &
              /, 1x, '***   CODE WILL FAIL IN HPBL CALC WITH FCORIS',  &
              /, 1x, 70('*'))


 9100 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: PBLPKG',                         &
              /, 1x, '***   IMPROPER VALUE FOR LPBL',                  &
              /, 1x, '***   LPBL MUST BE 2 OR 3 IN THIS ROUTINE',      &
              /, 1x, '***   LPBL = ', i2,                              &
              /, 1x, 70('*'))
 
 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE pblpkg
