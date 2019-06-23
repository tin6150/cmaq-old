
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
! $Header: /project/work/rep/MCIP2/src/mcip2/radmdry.F,v 1.4 2005/08/29 14:36:08 tlotte Exp $ 


SUBROUTINE radmdry

!-------------------------------------------------------------------------------
! Name:     RADM Dry Deposition
! Purpose:  Computes deposition velocities for species listed in DDEPSPC.
! Notes:    Original algorithms from Wesely's RADM dry deposition code.
! Revised:  25 Jan 1997  Redeveloped for MCIP from Wesely's code.  (D. Byun)
!                          1) Changed data structure for whole domain
!                          2) Computes VD using input PBL and Met data
!                          3) Integrates over LU at the outer loop
!           04 Feb 1998  Changed include method for nonglobals.  (D. Byun)
!           17 Sep 2001  Converted to free-form f90.  Modified significant
!                        sections of code.  Added dry deposition species
!                        for atrazine.  (T. Otte and W. Hutzell)
!           14 Jan 2002  Added new dry deposition species, methanol.
!                        (Y. Wu and T. Otte)
!           23 Jan 2002  Changed missing value for XRBNDY to BADVAL3.  (T. Otte)
!           09 Jun 2003  Added new dry deposition species:  N2O5, NO3, and
!                        generic aldehyde.  Updated dry deposition for winter
!                        snow coverage.  (D. Schwede)
!                        Removed dry deposition species, ATRA and ATRAP, from
!                        output.  (T. Otte)
!           08 Jul 2004  Changed local array allocation to occur only on
!                        initial call to subroutine to avoid memory
!                        fragmentation.  Removed XFLAGS.  (T. Otte)
!           17 Feb 2005  Removed variable NDEP from module MCIPPARM, and made
!                        its usage explicit here.  Changed Fortran method used
!                        to set HSTR, F0, and RDIF.  (T. Otte)
!           21 Jul 2005  Added restriction that RESISTCALC is only called if
!                        PX was not used (and PBL is passed through).  Also
!                        added filling of aerodynamic resistance for RADM land
!                        use if PX was used.  Added provisions to prevent
!                        negative dry deposition velocities from occurring in
!                        output.  Added non-zero definitions for XRADYN and
!                        XRSTOM at initialization period when PX is used.
!                        Added provision to alternatively use XPBL to check for
!                        meteorology model initialization time.  (T. Otte)
!           11 Aug 2005  Replaced species pointers from XDEPIDX with species
!                        names from XDEPSPC.  Moved declarations of F0, HSTR,
!                        and RDIF to DEPVVARS_MOD.  (T. Otte and W. Hutzell)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE depvvars
  USE const
  USE const_pbl
  USE lradmdat
  USE parms3, ONLY: badval3, amiss3

  IMPLICIT NONE

  REAL                         :: arg1
  REAL                         :: arg2
  REAL                         :: arg3
  INTEGER                      :: c
  REAL,          EXTERNAL      :: cvmgp
  REAL,    SAVE, ALLOCATABLE   :: dew        ( : , : )  ! y=1,n=0
  REAL                         :: drain      ! rain for the hour
  INTEGER                      :: ilu
  INTEGER                      :: isesn2
  INTEGER                      :: l
  INTEGER                      :: lspec
  INTEGER                      :: lu
  REAL,    SAVE, ALLOCATABLE   :: pblzdep    ( : , : )
  CHARACTER*16,  PARAMETER     :: pname      = 'RADMDRY'
  INTEGER                      :: r
  REAL                         :: rac        ( numsea, luradm )
  REAL,    SAVE, ALLOCATABLE   :: rain       ( : , : )! y=1,n=0
  REAL,          PARAMETER     :: raint0     = 0.01  ! cm/hr?  
  REAL                         :: rcan
  REAL                         :: rcl1
  REAL                         :: rclo       ( numsea, luradm )
  REAL                         :: rcls       ( numsea, luradm )
  REAL                         :: rdc
  REAL                         :: rdcl
  REAL                         :: rg
  REAL                         :: rgs1
  REAL                         :: rgso       ( numsea, luradm )
  REAL                         :: rgss       ( numsea, luradm )
  REAL,    SAVE, ALLOCATABLE   :: ri0        ( : , : )
  REAL                         :: rl
  REAL                         :: rl1
  REAL,    SAVE, ALLOCATABLE   :: rlo3       ( : , : )
  REAL                         :: rlu        ( numsea, luradm )
  REAL                         :: rsmin      ( numsea, luradm )
  REAL                         :: rst
  REAL                         :: rtrm
  REAL                         :: tc
  REAL                         :: tdpt       ! dew point temperature
  REAL                         :: ttrm1
  REAL                         :: ttrm2
  REAL                         :: v1
  REAL                         :: vdsmax     ( luradm, numsea )
  REAL                         :: vo3
  REAL                         :: vpress     ! vapor pressure
  REAL                         :: wet

!-------------------------------------------------------------------------------
! Deposition species dependent parameters.
!-------------------------------------------------------------------------------

  hstr( 1) =    1.e5   ;  f0( 1) = 0.0  ;  rdif( 1) = 1.9
  hstr( 2) =    0.0    ;  f0( 2) = 0.0  ;  rdif( 2) = 0.0
  hstr( 3) =    0.01   ;  f0( 3) = 0.1  ;  rdif( 3) = 1.6
  hstr( 4) =    0.002  ;  f0( 4) = 0.0  ;  rdif( 4) = 1.3
  hstr( 5) =    0.01   ;  f0( 5) = 1.0  ;  rdif( 5) = 1.6
  hstr( 6) =    1.e14  ;  f0( 6) = 0.0  ;  rdif( 6) = 1.9
  hstr( 7) =    1.e5   ;  f0( 7) = 1.0  ;  rdif( 7) = 1.4
  hstr( 8) =   15.0    ;  f0( 8) = 0.0  ;  rdif( 8) = 1.6
  hstr( 9) = 6000.0    ;  f0( 9) = 0.0  ;  rdif( 9) = 1.3
  hstr(10) =  240.0    ;  f0(10) = 0.1  ;  rdif(10) = 1.6
  hstr(11) =  540.0    ;  f0(11) = 0.1  ;  rdif(11) = 2.0
  hstr(12) =    4.e6   ;  f0(12) = 0.0  ;  rdif(12) = 1.6
  hstr(13) =    2.e4   ;  f0(13) = 0.0  ;  rdif(13) = 1.0
  hstr(14) =    3.6    ;  f0(14) = 0.1  ;  rdif(14) = 2.6
  hstr(15) =    1.e5   ;  f0(15) = 0.1  ;  rdif(15) = 1.6
  hstr(16) =    0.001  ;  f0(16) = 0.0  ;  rdif(16) = 1.2
  hstr(17) =  219.80   ;  f0(17) = 0.0  ;  rdif(17) = 1.33
  hstr(18) =    1.e14  ;  f0(18) = 0.0  ;  rdif(18) = 2.4
  hstr(19) =    1.e14  ;  f0(19) = 0.0  ;  rdif(19) = 1.9
  hstr(20) =   15.0    ;  f0(20) = 0.0  ;  rdif(20) = 1.6

!-------------------------------------------------------------------------------
! Values of surface resistance components [s/m] by season and land use type.
!     1 = urban land
!     2 = agriculture
!     3 = range
!     4 = deciduous forest
!     5 = coniferous forest
!     6 = mixed forest wetland
!     7 = water
!     8 = barren land
!     9 = non-forested wetland
!    10 = mixed agriculture/rangeland
!    11 = rocky open areas with low shrubs
!-------------------------------------------------------------------------------

  ! ISESN = 1, summer (Jun., Jul., Aug. in N. Hem.; Dec., Jan. Feb. in S. Hem.)

  DATA (rsmin(1,l),l=1,luradm) /1.E5, 60., 120., 70., 130.,       &
         100., 1.E5, 1.E5, 80., 100., 150./

  DATA (rlu(1,l),l=1,luradm) /1.E5, 2000., 2000., 2000., 2000.,   &
         2000., 1.E5, 1.E5, 2500., 2000., 4000./

  DATA (rac(1,l),l=1,luradm) /100., 200., 100., 2000., 2000.,     &
         2000., 0.1, 0.1, 300., 150., 200./

  DATA (rgss(1,l),l=1,luradm) /400., 150., 350., 500., 500.,      &
         100., 0.1, 1000., 0.1, 220., 400./

  DATA (rgso(1,l),l=1,luradm) /300., 150., 200., 200., 200.,      &
         300., 2000., 400., 1000., 180., 200./

  DATA (rcls(1,l),l=1,luradm) /1.E5, 2000., 2000., 2000., 2000.,  &
         2000., 1.E5, 1.E5, 2500., 2000., 4000./

  DATA (rclo(1,l),l=1,luradm) /1.E5, 1000., 1000., 1000., 1000.,  &
         1000., 1.E5, 1.E5, 1000., 1000., 1000./


  ! ISESN = 2, autumn (Sep., Oct., Nov. in N. Hem.; Mar., Apr., May in S. Hem)

  DATA (rsmin(2,l),l=1,luradm) /1.E5, 1.E5, 1.E5, 1.E5, 250.,     &
         500., 1.E5, 1.E5, 1.E5, 1.E5, 1.E5/

  DATA (rlu(2,l),l=1,luradm) /1.E5, 9000., 9000., 9000., 4000.,   &
         8000., 1.E5, 1.E5,  9000.,  9000., 9000./

  DATA (rac(2,l),l=1,luradm) /100., 150., 100., 1500., 2000.,     &
         1700., 0.1, 0.1, 200., 120., 140./

  DATA (rgss(2,l),l=1,luradm) /400., 200., 350., 500., 500.,      &
         100., 0.1, 1000., 0.1, 300., 400./

  DATA (rgso(2,l),l=1,luradm) /300., 150., 200., 200., 200.,      &
         300., 2000., 400., 800., 180., 200./

  DATA (rcls(2,l),l=1,luradm) /1.E5, 9000., 9000., 9000., 2000.,  &
         4000., 1.E5, 1.E5, 9000., 9000., 9000./

  DATA (rclo(2,l),l=1,luradm) /1.E5, 400., 400., 400., 1000.,     &
         600., 1.E5, 1.E5 , 400., 400., 400./


  ! ISESN = 3, winter (Dec., Jan., Feb. in N. Hem.; Jun., Jul., Aug. in S. Hem.)

  DATA (rsmin(3,l),l=1,luradm) /1.E5, 1.E5, 1.E5, 1.E5, 250.,     &
         500., 1.E5, 1.E5, 1.E5, 1.E5, 1.E5/

  DATA (rlu(3,l),l=1,luradm) /1.E5, 1.E5, 9000., 9000., 4000.,    &
         8000., 1.E5, 1.E5, 9000., 9000,  9000./

  DATA (rac(3,l),l=1,luradm) /100., 10., 100., 1000., 2000.,      &
         1500., 0.1, 0.1, 100., 50., 120./

  DATA (rgss(3,l),l=1,luradm) /400., 150., 350., 500., 500.,      &
         200., 0.1, 1000., 0.1, 200., 400./

  DATA (rgso(3,l),l=1,luradm) /300., 150., 200., 200., 200.,      &
         300., 2000., 400., 1000., 180., 200./

  DATA (rcls(3,l),l=1,luradm) /1.E5, 1.E5, 9000., 9000., 3000.,   &
         6000., 1.E5, 1.E5, 9000., 9000., 9000./

  DATA (rclo(3,l),l=1,luradm) /1.E5, 1000., 400., 400., 1000.,    &
         600., 1.E5, 1.E5, 800., 600., 600./


  ! ISESN = 4, winter, with snow

  DATA (rsmin(4,l),l=1,luradm) /1.E5, 1.E5, 1.E5, 1.E5, 400.,     &
         800., 1.E5, 1.E5, 1.E5, 1.E5, 1.E5/

  DATA (rlu(4,l),l=1,luradm) /1.E5, 1.E5, 1.E5, 1.E5, 6000.,      &
         9000., 1.E5, 1.E5, 9000., 9000., 9000./

  DATA (rac(4,l),l=1,luradm) /100., 10., 10., 1000., 2000.,       &
         1500., 0.1, 0.1, 50., 10., 50./

  DATA (rgss(4,l),l=1,luradm) /100., 100., 100., 100., 100.,      &
         100., 0.1, 1000., 100., 100., 50./

  DATA (rgso(4,l),l=1,luradm) /600., 3500., 3500., 3500., 3500.,  &
         3500., 2000., 400., 3500., 3500., 3500./

  DATA (rcls(4,l),l=1,luradm) /1.E5, 1.E5, 1.E5, 9000., 200.,     &
         400., 1.E5, 1.E5, 9000., 1.E5, 9000./

  DATA (rclo(4,l),l=1,luradm) /1.E5, 1000., 1000., 400., 1500.,   &
         600., 1.E5, 1.E5, 800., 1000., 800./


  ! ISESN = 5, Transitional spring with partial green coverage
  !            (Mar., Apr., May in N. Hem.; Sep., Oct., Nov. in S. Hem.)

  DATA (rsmin(5,l),l=1,luradm) /1.E5, 120., 240., 140., 250.,     &
         190., 1.E5, 1.E5 , 160., 200., 300./

  DATA (rlu(5,l),l=1,luradm) /1.E5, 4000., 4000., 4000., 2000.,   &
         3000., 1.E5, 1.E5, 4000., 4000., 8000./

  DATA (rac(5,l),l=1,luradm) /100., 50., 80., 1200., 2000.,       &
         1500., 0.1, 0.1, 200., 60., 120./

  DATA (rgss(5,l),l=1,luradm) /500., 150., 350., 500., 500.,      &
         200., 0.1, 1000., 0.1, 250., 400./

  DATA (rgso(5,l),l=1,luradm) /300., 150., 200., 200., 200.,      &
         300., 2000., 400., 1000., 180., 200./

  DATA (rcls(5,l),l=1,luradm) /1.E5, 4000., 4000., 4000., 2000.,  &
         3000., 1.E5, 1.E5, 4000., 4000., 8000./

  DATA (rclo(5,l),l=1,luradm) /1.E5, 1000., 500., 500., 1500.,    &
         700., 1.E5, 1.E5, 600., 800., 800./


!-------------------------------------------------------------------------------
! Maximum SO4 surface deposition velocities [m/s] by season and land use type.
!-------------------------------------------------------------------------------

  ! ISESN = 1 (Jun., Jul., Aug. in N. Hem.; Dec., Jan., Feb. in S. Hem.)

  DATA (vdsmax(l,1),l=1,luradm) /0.001, 0.01,  0.01,  0.01,  0.01,   &
                                 0.01,  0.001, 0.01,  0.01,  0.01,  0.01/

  ! ISESN = 2 (Sep., Oct., Nov. in N. Hem.; Mar., Apr., May in S. Hem.)

  DATA (vdsmax(l,2),l=1,luradm) /0.001, 0.01,  0.01,  0.001, 0.008,  &
                                 0.004, 0.001, 0.01,  0.01,  0.01,  0.01/

  ! ISESN = 3 (Dec., Jan., Feb. in N. Hem.; Jun., Jul., Aug. in S. Hem.)

  DATA (vdsmax(l,3),l=1,luradm) /0.001, 0.01,  0.01,  0.001, 0.008,  &
                                 0.004, 0.001, 0.01,  0.01,  0.01,  0.01/

  ! ISESN = 4 (winter-snow on ground)

  DATA (vdsmax(l,4),l=1,luradm) /0.001, 0.01,  0.01,  0.001, 0.008,  &
                                 0.004, 0.001, 0.01,  0.01,  0.01,  0.01/

  ! ISESN = 5 (Mar., Apr., May in N. Hem.; Sep., Oct., Nov. in S. Hem.)

  DATA (vdsmax(l,5),l=1,luradm) /0.001, 0.01,  0.01,  0.01,  0.01,   &
                                 0.01,  0.001, 0.01,  0.01,  0.01,  0.01/

!-------------------------------------------------------------------------------
! For the time period that corresponds to a meteorology model initialization
! time, many PBL variables are not defined.  At the initialization time for the
! meteorology model, the XUSTAR array may contain all 0.0 values or the XPBL
! array may contain all 0.0 values.  In either case, set place-holder values for
! variables that would otherwise be calculated in this routine.
!-------------------------------------------------------------------------------

  xvd(:,:,:) = 0.0

  IF ( ( MAXVAL(xustar) == 0.0 ) .OR.  &
       ( MAXVAL(xpbl)   == 0.0 ) ) THEN  ! assume initialization period

    xrbndy(:,:,:) = badval3     ! inverse taken in metcro.F
    IF ( px ) THEN
      xradyn(:,:,:) = badval3   ! inverse taken in metcro.F
      xrstom(:,:)   = badval3   ! inverse taken in metcro.F
    ENDIF

  ELSE

!-------------------------------------------------------------------------------
! Allocate necessary arrays.
!-------------------------------------------------------------------------------

    IF ( .NOT. ALLOCATED ( dew     ) ) ALLOCATE ( dew     ( ncols_x, nrows_x ) )
    IF ( .NOT. ALLOCATED ( pblzdep ) ) ALLOCATE ( pblzdep ( ncols_x, nrows_x ) )
    IF ( .NOT. ALLOCATED ( rain    ) ) ALLOCATE ( rain    ( ncols_x, nrows_x ) )
    IF ( .NOT. ALLOCATED ( ri0     ) ) ALLOCATE ( ri0     ( ncols_x, nrows_x ) )
    IF ( .NOT. ALLOCATED ( rlo3    ) ) ALLOCATE ( rlo3    ( ncols_x, nrows_x ) )

!-------------------------------------------------------------------------------
! If using PBL variables directly from meteorology input, get resistances.
!-------------------------------------------------------------------------------

    IF ( px ) THEN

      DO c = 1, ncols_x
        DO r = 1, nrows_x
          lu = NINT( xdluse(c,r) )
          xradyn(c,r,1:luradm) = badval3  ! inverse taken in metcro.F
          xradyn(c,r,lu)       = xradyn(c,r,0)
        ENDDO
      ENDDO

    ELSE

      IF ( lpbl == 1 ) THEN
        CALL resistcalc
      ENDIF

    ENDIF

!-------------------------------------------------------------------------------
! Loop over grid points to calculate dry deposition velocities for each
! species and land-use category.
!-------------------------------------------------------------------------------

    col_loop: DO c = 1, ncols_x
      row_loop: DO r = 1, nrows_x

        !-----------------------------------------------------------------------
        ! Calculate PBL height/deposition layer thickness.
        !-----------------------------------------------------------------------

         pblzdep(c,r) = cvmgp(MIN( xpbl(c,r)*0.5 / xdx3htf(c,r,1), 1.0 ),  &
                              1.0, xmol(c,r))

        !-----------------------------------------------------------------------
        ! Estimate DEW and RAIN.
        !-----------------------------------------------------------------------

        !   vapor_pressure [Pa] = density_wvapor * Rwvapor * Temp

        vpress = MAX ( rwvap * xdenswm(c,r,1) * xtempm(c,r,1), 1.0e-18  )

        ! Dew point temperature [K]

        tdpt = 5417.4 / (19.83 - LOG( vpress / 610.78 ) )

        ! Dew (1.0 or 0.0) i.e., Yes/No logic            

        dew(c,r) = cvmgp( 0.0, 1.0, ( xtempg(c,r) - tdpt ) )
        dew(c,r) = cvmgp( dew(c,r), 0.0, ( xtempg(c,r) - stdtemp ) )

        ! now XRAINN and XRAINC contain [cm/hour]

        drain = ( xrainn(c,r) + xrainc(c,r) )

        rain(c,r) = cvmgp( 1.0, 0.0, ( drain - raint0 ) )
        rain(c,r) = cvmgp( rain(c,r), 0.0, ( xtempg(c,r) - stdtemp ) )

        !--------------------------------------------------------------------
        ! Check for snow cover in this grid cell.  If present, reset the 
        ! season to use the appropriate values of resistances.  Use ISESN2
        ! below so as not to change value held in program.
        !--------------------------------------------------------------------
        
        IF ( xsnocov(c,r) > 0.5 ) THEN
          isesn2 = 4
        ELSE
          isesn2 = isesn
        ENDIF

        !-----------------------------------------------------------------------
        ! Loop over land use types to calculate species-dependent deposition
        ! velocities.  The following code has the same technique for Vd
        ! estimation reported by Walcek, et al., 1986, Atmos. Env., Vol. 30,
        ! No. 5, 946-964.  D. Byun updated following Wesely (1989).
        !-----------------------------------------------------------------------

        xrbndy(c,r,0) = 0.0

        lu_loop: DO ilu = 1, luradm

          ! Compute species dependent RB.

          IF ( lpbl == 1 ) THEN
            IF ( ilu == NINT(xdluse(c,r)) ) THEN
              xrbndy(c,r,ilu) = 2.83 / ( vkar * xustar(c,r,0) )
            ELSE
              xrbndy(c,r,ilu) = badval3    ! inverse taken in metcro.F
            ENDIF
          ELSE
            xrbndy(c,r,ilu) = 2.83 / ( vkar * xustar(c,r,ilu) )
          ENDIF 

          !---------------------------------------------------------------------
          ! Loop over dry deposition species.  Some species have special
          ! calculations; others are generic.
          !---------------------------------------------------------------------

          spec_loop: DO lspec = 1, ltotg

            !-------------------------------------------------------------------
            ! Dry deposition of SO2.
            !-------------------------------------------------------------------

            IF ( TRIM(xdepspc(lspec)) == 'SO2' ) THEN

              ! Compute surface resistance.

              rtrm = 200.0 / ( xgsw(c,r) + 0.1 )    ! what unit should XGSW be?
              tc   = xtempg(c,r) - stdtemp

              IF ( ( tc > 0.1 ) .AND. ( tc < 39.9 ) ) THEN
                ttrm1 = 400.0 / ( tc * ( 40.0 - tc ) )
              ELSE
                ttrm1 = 100.0
              ENDIF

              ttrm2    = 1000.0 * EXP( - ( tc + 4.0 ) )
              wet      = 1.0 + 2.0 * MIN( 1.0, (dew(c,r) + rain(c,r)) )
              ri0(c,r) = rsmin(isesn2,ilu) * wet * ( 1.0  + rtrm * rtrm ) * ttrm1

              ! Compute stomatal and mesophylic resistance.

              rst = ri0(c,r) * rdif(lspec) + 1.0 / ( hstr(lspec) / 3000.0 +  &
                    f0(lspec) * 100.0 )

              rl1 = rlu(isesn2,ilu) + ttrm2
              rl  = rl1

              IF ( ( rl < 9999.0 ) .AND. ( dew(c,r)  > 0.9 ) ) rl = 100.0

              IF ( ( rl < 9999.0 ) .AND. ( rain(c,r) > 0.9 ) )  &
                                            rl = 1.0 / ( 0.0002 + 0.333 / rl1 )

              IF ( ( ilu == 1 ) .AND. ( wet > 1.5 ) ) rl = 50.0

              rdcl = 100.0 * ( 1.0 + 1000.0 / ( xgsw(c,r) + 10.0 ) ) +  &
                     rcls(isesn2,ilu) + ttrm2

              rg   = rgss(isesn2,ilu) + ttrm2 + rac(isesn2,ilu)

              rcan = 1.0 / (  1.0/rst + 1.0/rl + 1.0/rdcl + 1.0/rg )
              IF ( xradyn(c,r,ilu) > amiss3 ) THEN  ! XRADYN is defined for ILU
                v1 = pblzdep(c,r) /  &
                       ( xradyn(c,r,ilu) + xrbndy(c,r,ilu) + rcan )
              ELSE  ! BADVAL3 < AMISS3
                v1 = 0.0
              ENDIF

              xvd(c,r,lspec) = xvd(c,r,lspec) + xluse(c,r,ilu) * v1
              IF ( xvd(c,r,lspec) < 0.0 ) GOTO 8000

            !-------------------------------------------------------------------
            ! Dry deposition of SO4.
            !-------------------------------------------------------------------

            ELSE IF ( TRIM(xdepspc(lspec)) == 'SULF' ) THEN

              ! Scale maximum deposition velocity according to stability.

              arg1 = cvmgp( 1.0,                                             &
                            1.0 + ( ABS( -300.0 / xmol(c,r) ) )**(2.0/3.0),  &
                            xmol(c,r) )

              arg2 = cvmgp( 0.002 * arg1,                                            &
                            0.0009 * ( ABS( - xpbl(c,r) / xmol(c,r) ) )**(2.0/3.0),  &
                            xpbl(c,r) / xmol(c,r) + 30.0 )

              ! Set Vd's to be less than VDSMAX.

              IF ( xustar(c,r,ilu) /= 0.0 ) THEN
                arg3 = 1.0 / MIN( vdsmax(ilu,isesn2), xustar(c,r,ilu)*arg2 )
              ELSE
                arg3 = 0.0
              ENDIF

              IF ( xradyn(c,r,ilu) > amiss3 ) THEN  ! XRADYN is defined for ILU
                xvd(c,r,lspec) = xvd(c,r,lspec) + xluse(c,r,ilu) /  &
                                 ( xradyn(c,r,ilu) + arg3 )
              ENDIF
              IF ( xvd(c,r,lspec) < 0.0 ) GOTO 8000

            !-------------------------------------------------------------------
            ! Dry deposition of O3.
            !-------------------------------------------------------------------

            ELSE IF ( TRIM(xdepspc(lspec)) == 'O3' ) THEN

              tc    = xtempg(c,r) - stdtemp
              ttrm2 = 1000.0 * EXP( -tc - 4.0 )
              rst   = ri0(c,r) * rdif(lspec) +  &
                      1.0 / ( hstr(lspec) / 3000.0 + f0(lspec) * 100.0 )
              rl1   = rlu(isesn2,ilu) + ttrm2

              rlo3(c,r) = rl1

              IF ( ( rl < 9999.0 ) .AND. ( dew(c,r)  > 0.9 ) )  &
                          rlo3(c,r) = 1.0 / ( 0.3333333E-3 + 0.33333333 / rl1 )

              IF ( ( rl < 9999.0 ) .AND. ( rain(c,r) > 0.9 ) )  &
                             rlo3(c,r) = 1.0 / ( 0.001 + 0.33333333 / rl1 )

              rdcl = 100.0 * (1.0 + 1000.0 / ( xgsw(c,r) + 10.0 ) ) +  &
                     rclo(isesn2,ilu) + ttrm2
              rg   = rgso(isesn2,ilu) + ttrm2 + rac(isesn2,ilu)
              rcan = 1.0 / (  1.0/rst + 1.0/rlo3(c,r) + 1.0/rdcl + 1.0/rg )

              IF ( xradyn(c,r,ilu) > amiss3 ) THEN  ! XRADYN is defined for ILU
                vo3 = pblzdep(c,r) /  &
                        ( xradyn(c,r,ilu) + xrbndy(c,r,ilu) + rcan )
              ELSE  ! BADVAL3 < AMISS3
                vo3 = 0.0
              ENDIF

              xvd(c,r,lspec) = xvd(c,r,lspec) + xluse(c,r,ilu) * vo3
              IF ( xvd(c,r,lspec) < 0.0 ) GOTO 8000
              xrstom(c,r)  = rst   ! XRSTOM has stomatal resistance for O3

            !-------------------------------------------------------------------
            ! Dry deposition of all other gases.
            !-------------------------------------------------------------------

            ELSE

              tc    = xtempg(c,r) - stdtemp
              ttrm2 = 1000.0 * EXP( -tc - 4.0 )
              wet   = 1.0 + 2.0 * MIN( 1.0, ( dew(c,r) + rain(c,r) ) )
              rst   = ri0(c,r) * rdif(lspec) +  &
                              1.0 / ( hstr(lspec) / 3000.0 + f0(lspec) * 100.0 )
              rl    = ( rlu(isesn2,ilu) + ttrm2 ) /   &
                      ( hstr(lspec) / 1.0e5 + f0(lspec) )

              IF ( ( rl < 9999.0) .AND. ( wet > 1.5 ) )      &
                    rl  = 1.0 / (  hstr(lspec) / 1.0e7 +     &
                          f0(lspec) / rlo3(c,r) + 0.3333 / rl )

              rdc   = 100.0 * ( 1.0 + 1000.0 / ( xgsw(c,r) + 10.0 ) )

              rcl1  = 1.0 / ( hstr(lspec) * 1.0e-5 / ( rcls(isesn2,ilu) +  &
                      ttrm2 ) + f0(lspec) / ( rclo(isesn2,ilu) + ttrm2 ) )

              rgs1  = 1.0 / ( hstr(lspec) * 1.0e-5 / ( rgss(isesn2,ilu) +  &
                      ttrm2 ) + f0(lspec) / ( rgso(isesn2,ilu) + ttrm2 ) )

              rcan  = 1.0 / (  1.0/rst + 1.0/rl + 1.0/( rdc + rcl1 ) +  &
                    1.0 / ( rac(isesn2,ilu) + rgs1 ) )

              IF ( xradyn(c,r,ilu) > amiss3 ) THEN  ! XRADYN is defined for ILU
                v1 = pblzdep(c,r) /  &
                       ( xradyn(c,r,ilu) + xrbndy(c,r,ilu) + rcan )
              ELSE  ! BADVAL3 < AMISS3
                v1 = 0.0
              ENDIF

              xvd(c,r,lspec) = xvd(c,r,lspec) + xluse(c,r,ilu) * v1
              IF ( xvd(c,r,lspec) < 0.0 ) GOTO 8000

            ENDIF

          ENDDO spec_loop

          ! add inverse               
          IF ( xrbndy(c,r,ilu) > amiss3 ) THEN  ! XRBNDY is defined for ILU
            xrbndy(c,r,0) = xrbndy(c,r,0) + xluse(c,r,ilu) / xrbndy(c,r,ilu)
          ENDIF

        ENDDO lu_loop
 
        xrbndy(c,r,0) = 1.0 / xrbndy(c,r,0) 

      ENDDO row_loop
    ENDDO col_loop

!-------------------------------------------------------------------------------
! Deallocate arrays.
!-------------------------------------------------------------------------------

!   DEALLOCATE ( dew     )  ! commented out to avoid memory fragmentation
!   DEALLOCATE ( pblzdep )  ! commented out to avoid memory fragmentation
!   DEALLOCATE ( rain    )  ! commented out to avoid memory fragmentation
!   DEALLOCATE ( ri0     )  ! commented out to avoid memory fragmentation
!   DEALLOCATE ( rlo3    )  ! commented out to avoid memory fragmentation

  ENDIF

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8000 WRITE (6,9000) c, r, TRIM(xdepspc(lspec)), xvd(c,r,lspec)
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: RADMDRY',                            &
              /, 1x, '***   NEGATIVE DRY DEPOSITION VELOCITY',             &
              /, 1x, '***   POINT   = ', 2i5,                              &
              /, 1x, '***   SPECIES = ', a,                                &
              /, 1x, '***   VD      = ', e13.6,                            &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE radmdry
