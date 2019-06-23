
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
! $Header: /project/work/rep/MCIP2/src/mcip2/m3dry.F,v 1.6 2006/03/01 20:12:42 tlotte Exp $ 


SUBROUTINE m3dry

!-------------------------------------------------------------------------------
! Name:     Models-3 Dry Deposition
! Purpose:  Computes dry deposition velocities using Rst and Ra, and
!           elements of ADOM DD model.
! Revised:  21 Jan 1998  Original version.  (J. Pleim and A. Bourgeois)
!           18 Sep 2001  Converted to free-form f90.  Modified significant
!                        sections of code.  Added characteristics for
!                        dry deposition of atrazine and atrazine product.
!                        Made general for USGS 24-category system.  
!                        (T. Otte, J. Pleim, and W. Hutzell)
!           14 Jan 2002  Added temperature dependence to Henry's Law
!                        constants.  Added temperature and pressure
!                        dependence to diffusivity.  Added new dry
!                        deposition species, methanol.  (Y. Wu and T. Otte)
!           18 Jan 2002  Changed the reference wet cuticle resistance.
!                        (J. Pleim)
!           23 Jan 2002  Changed missing value on XRADYN, XRBNDY, and XRSTOM
!                        to BADVAL3.  (T. Otte)
!           26 Feb 2002  Changed logic to define water point using dominant
!                        land use category.  (T. Otte)
!           09 Jun 2003  Added logic for modeling snow covered surfaces.
!                        Changed the reactivities for SO2, HNO3 and NH3.
!                        Changed pH values to have an east-west variation.
!                        Using the Henry's law constant function from CMAQ in
!                        place of local code.  Also changed the code for
!                        deposition to water to use a pH of 8.1 and the
!                        temperature of water in calculating the Henry's law
!                        constant.  Adjusted values of RSNOW0 = 1000 and
!                        A(NH3) = 20.  Added new dry deposition species: N2O5,
!                        NO3, Generic_aldehyde.  Corrected diffusivities of
!                        chemicals and water and viscosity of air to all be at
!                        the same temperature (273.15K).  Temperature and
!                        pressure adjustments to the values are not needed
!                        because the diffusivities and viscosity are always used
!                        as ratios, so the temperature-pressure dependence was
!                        removed.  Removed dry deposition species, ATRA and
!                        ATRAP, from output.  (D. Schwede, J. Pleim, and
!                        T. Otte)
!           04 Aug 2004  Added provisions to prevent negative dry deposition
!                        velocities from occurring in output.  Removed XFLAGS.
!                        Changed XWIND10 to XWSPD10.  (T. Otte)
!           28 Feb 2005  Added optional dry deposition species for chlorine
!                        and mercury.  (G. Sarwar, R. Bullock, and T. Otte)
!           21 Jul 2005  Added provision to alternatively use XPBL to check for
!                        meteorology model initialization time.  (T. Otte)
!           11 Aug 2005  Replaced species pointers from XDEPIDX with species
!                        names from XDEPSPC.  Changed XRH to a local scalar.
!                        Moved declarations of A, ALPHS, DIF0, DKHOR, KH, and
!                        SUBNAME to DEPVVARS_MOD.  (T. Otte and W. Hutzell)
!            2 Feb 2006  Added mesophyll resistance to dry deposition velocity
!                        calculation, and defined non-zero value for mercury.
!                        (D. Schwede, J. Pleim, and R. Bullock)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE depvvars
  USE lradmdat
  USE const
  USE parms3, ONLY: badval3, amiss3
  USE metinfo

  IMPLICIT NONE

  REAL,          PARAMETER     :: a0         = 8.0     ! [dim'less]
  INTEGER                      :: c
  REAL,          PARAMETER     :: d3         = 4.8e-4  ! [dim'less]
  REAL                         :: diffu
  REAL,          PARAMETER     :: dwat       = 0.2178  ! [cm^2 / s] at 273.15K
  LOGICAL                      :: effective            ! true=compute effective Henry's Law const
  REAL                         :: es
  REAL                         :: h                    ! [dim'less]
  REAL                         :: hcan
  REAL                         :: heff                 ! effective Henry's Law constant
  REAL,          EXTERNAL      :: hlconst              ! [M / atm]
  REAL                         :: hplus
  INTEGER                      :: ifsnow               ! 1=snow
  REAL,          PARAMETER     :: kvis       = 0.132   ! [cm^2 / s] at 273.15K
  INTEGER                      :: l
  CHARACTER*16,  PARAMETER     :: pname      = 'M3DRY'
  REAL,          PARAMETER     :: pr         = 0.709   ! [dim'less]
  REAL                         :: qss
  INTEGER                      :: r
  REAL                         :: rac
  REAL                         :: rbc
  REAL                         :: rbsulf
  REAL                         :: rbw
  REAL                         :: rci
  REAL                         :: rci_old
  REAL                         :: rcut
  REAL,          PARAMETER     :: rcut0      = 3000.0  ! [s/m]
  REAL,          PARAMETER     :: rcw0       = 125000.0 ! acc'd'g to Padro and
                                                       ! adapted from Slinn 78
  REAL,          PARAMETER     :: rg0        = 1000.0  ! [s/m]
  REAL                         :: rgnd
  REAL                         :: rgndc
  REAL                         :: rgw                  ! resist for water-covered sfc
  REAL,          PARAMETER     :: rgwet0     = 25000
  REAL                         :: rinc
  REAL                         :: rnph
  REAL,          PARAMETER     :: rnph0      = 4.3     ! default pH of rain water
  REAL,          PARAMETER     :: rsndiff    = 10.0    ! snow diffusivity fac
  REAL                         :: rsnow
  REAL,          PARAMETER     :: rsnow0     = 1000.0
  REAL                         :: rstom
  REAL                         :: rsurf
  REAL                         :: rsurf_old
  REAL                         :: rwet                 ! wet sfc resist (cuticle or grd)
  REAL                         :: rwetsfc
  REAL                         :: scc
  REAL                         :: scw
  REAL,          PARAMETER     :: svp2       = 17.67   ! from MM5
  REAL,          PARAMETER     :: svp3       = 29.65   ! from MM5
  REAL                         :: wrmax
  REAL                         :: xm                   ! liquid water mass frac
  REAL                         :: xrh                  ! relative humidity

!-------------------------------------------------------------------------------
! Soil Characteristics by Type
!
!   #  SOIL TYPE  WSAT  WFC  WWLT    B   CGSAT   JP   AS   C2R  C1SAT
!   _  _________  ____  ___  ____  ____  _____   ___  ___  ___  _____
!   1  SAND       .395 .135  .068  4.05  3.222    4  .387  3.9  .082
!   2  LOAMY SAND .410 .150  .075  4.38  3.057    4  .404  3.7  .098
!   3  SANDY LOAM .435 .195  .114  4.90  3.560    4  .219  1.8  .132
!   4  SILT LOAM  .485 .255  .179  5.30  4.418    6  .105  0.8  .153
!   5  LOAM       .451 .240  .155  5.39  4.111    6  .148  0.8  .191
!   6  SND CLY LM .420 .255  .175  7.12  3.670    6  .135  0.8  .213
!   7  SLT CLY LM .477 .322  .218  7.75  3.593    8  .127  0.4  .385
!   8  CLAY LOAM  .476 .325  .250  8.52  3.995   10  .084  0.6  .227
!   9  SANDY CLAY .426 .310  .219 10.40  3.058    8  .139  0.3  .421
!  10  SILTY CLAY .482 .370  .283 10.40  3.729   10  .075  0.3  .375
!  11  CLAY       .482 .367  .286 11.40  3.600   12  .083  0.3  .342
!
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!-- Chemical-Dependent Parameters (Original Source: Modified ADOM - Padro)
!
!                                                          at 298.15 K
!     Species   Dif0(cm2/s) Alphastar   Reactivity     -DKHOR [K]  KH0 [M/atm]
!     _______   ___________ _________   __________     _________  __________
!  1   SO2      0.1089 ~    1000.       8.00              3100.0 ~     1.2e00 ~
!  2   SULF      --         --           --                --           --
!  3   NO2      0.1361 ~    1.00        8.00 2.00*        2500.0 ~     1.2e-2 ~
!  4   NO       0.1802 ~    1           2.0+              1500.0 ~     1.9e-3 ~
!  5   O3       0.1444 ~    10.00       15.0  8@          2700.0 ~     1.2e-2 ~
!  6   HNO3     0.1628 ~    1E9         18.0 800* 8000**  8700.0 ~     2.6e+6 ~
!  7   H2O2     0.2402 ~    1.00        12.0 30*          6600.0 ~     9.7e+4 ~
!  8   ACET ALD 0.1525 ~    1           10.+              5700.0 ~     1.3e+1 ~
!  9   HCHO     0.1877 ~    1.00        10.+              6400.0 ~     7.0e+3 ~
! 10   OP       0.1525 ~    1           10.0+             5200.0 ~     3.1e+2 ~
! 11   PAA      0.1220 ~    1           20+               5300.0 ~     8.4e+2 ~
! 12   ORA      0.1525 ~    1           20+               5700.0 ~     3.7e+3 ~
! 13   NH3      0.1978 ~    1E5         10.0              4100.0 ~     5.8e+1 ~
! 14   PAN      0.0938 ~    1           4.0               5900.0 ~     2.9e00 ~
! 15   HONO     0.1525 ~    1           20+               4800.0 ~     4.9e+1 ~
! 16   CO       0.1807 ~    1           5.0               1600.0 ~     9.5e-4 ~
! 17   METHANOL 0.1363 ~    1.0 ~       2.0 ~             5200.0 ~     2.2e+2 ~
! --   CO2      0.1381 ~                                  2400.0 ~     3.4e-2 ~
! 18   N2O5     0.0808 ^^               5000.0**
! 19   NO3      0.1153 ^^               5000.0**
! 20   GEN ALD  0.1525 ##   1.0         10.0##
! 21   CL2      0.1080 %                10.0 %
! 22   HOCL     0.1300 %                10.0 %
! 23   HCL      0.1510 %                8000.0 %
! 24   FMCL     0.1094 %                10.0 %
! 25   ICL1     0.0664 %                10.0 %
! 26   ICL2     0.0664 %                10.0 %
! 27   HG       0.1194 $                0.1 $
! 28   HGIIGAS  0.0976 $                8000.0 $
!
!---------Notes
!  * Updates based on literature review 7/96 JEP
!  # Diff and H based on Wesely (1988) same as RADM
!  + Estimated by JEP 2/97 
!  @ Updated by JEP 9/01
!  ~ Added by YW 1/02.  Dif0 based on Massman (1998).  Henry's Law constant
!    is defined here as: h=cg/ca, where cg is the concentration of a species
!    in gas-phase, and ca is its aqueous-phase concentration.  The smaller h,
!    the larger solubility.  Henry's Law constant in another definition (KH):
!    KH = ca/pg [M/atm], KH = KH0 * exp(-DKH/R(1/T-1/T0)), where KH0 and -DKH
!    values are from Rolf Sander (1999).  h=1/(KH*R*T).
! ** Update by DBS based on estimates by JEP 1/03
! ^^ From Bill Massman, personal communication 4/03
! ## Diffusivity calculated by SPARC, reactivity = other aldehydes
! ++ Dif0 in Massman is diffusivity at temperature 0C and 1 atm (101.325kPa), so
!    chemicals that were not in Massman's paper need to be adjusted.  We assume
!    JEP's original values were for 25C and 1 atm.
!  % Added by G. Sarwar (10/04)
!  $ Added by R. Bullock (02/05) HG diffusivity is from Massman (1999).  
!    HGIIGAS diffusivity calculated from the HG value and a mol. wt. scaling 
!    factor of MW**(-2/3) from EPA/600/3-87/015. ORD, Athens, GA.  HGIIGAS 
!    mol.wt. used is that of HgCl2.  Reactivity of HG is 1/20th of NO and NO2 
!    values based on general atmospheric lifetimes of each species.  Reactivity 
!    of HGIIGAS is based on HNO3 surrogate.
!-------------------------------------------------------------------------------

  subname( 1) = 'SO2             '  ;  dif0( 1) = 0.1089  ;  a( 1) =   10.0  ;  meso( 1) = 0.0
  subname( 2) = 'SULFATE         '  ;  dif0( 2) = 0.0000  ;  a( 2) =    0.0  ;  meso( 2) = 0.0
  subname( 3) = 'NO2             '  ;  dif0( 3) = 0.1361  ;  a( 3) =    2.0  ;  meso( 3) = 0.0
  subname( 4) = 'NO              '  ;  dif0( 4) = 0.1802  ;  a( 4) =    2.0  ;  meso( 4) = 0.0
  subname( 5) = 'O3              '  ;  dif0( 5) = 0.1444  ;  a( 5) =    8.0  ;  meso( 5) = 0.0
  subname( 6) = 'HNO3            '  ;  dif0( 6) = 0.1067  ;  a( 6) = 8000.0  ;  meso( 6) = 0.0
  subname( 7) = 'H2O2            '  ;  dif0( 7) = 0.1300  ;  a( 7) =   30.0  ;  meso( 7) = 0.0
  subname( 8) = 'ACETALDEHYDE    '  ;  dif0( 8) = 0.1111  ;  a( 8) =   10.0  ;  meso( 8) = 0.0
  subname( 9) = 'FORMALDEHYDE    '  ;  dif0( 9) = 0.1554  ;  a( 9) =   10.0  ;  meso( 9) = 0.0
  subname(10) = 'METHYLHYDROPEROX'  ;  dif0(10) = 0.1179  ;  a(10) =   10.0  ;  meso(10) = 0.0
  subname(11) = 'PEROXYACETIC_ACI'  ;  dif0(11) = 0.0868  ;  a(11) =   20.0  ;  meso(11) = 0.0
  subname(12) = 'ACETIC_ACID     '  ;  dif0(12) = 0.0944  ;  a(12) =   20.0  ;  meso(12) = 0.0
  subname(13) = 'NH3             '  ;  dif0(13) = 0.1978  ;  a(13) =   20.0  ;  meso(13) = 0.0
  subname(14) = 'PAN             '  ;  dif0(14) = 0.0687  ;  a(14) =    4.0  ;  meso(14) = 0.0
  subname(15) = 'HNO2            '  ;  dif0(15) = 0.1349  ;  a(15) =   20.0  ;  meso(15) = 0.0
  subname(16) = 'CO              '  ;  dif0(16) = 0.1807  ;  a(16) =    5.0  ;  meso(16) = 0.0
  subname(17) = 'METHANOL        '  ;  dif0(17) = 0.1329  ;  a(17) =    2.0  ;  meso(17) = 0.0
  subname(18) = 'N2O5            '  ;  dif0(18) = 0.0808  ;  a(18) = 5000.0  ;  meso(18) = 0.0
  subname(19) = 'NO3             '  ;  dif0(19) = 0.1153  ;  a(19) = 5000.0  ;  meso(19) = 0.0
  subname(20) = 'GENERIC_ALDEHYDE'  ;  dif0(20) = 0.0916  ;  a(20) =   10.0  ;  meso(20) = 0.0

  IF ( lddep >= 3 ) THEN

  subname(21) = 'CL2             '  ;  dif0(21) = 0.1080  ;  a(21) =   10.0  ;  meso(21) = 0.0
  subname(22) = 'HOCL            '  ;  dif0(22) = 0.1300  ;  a(22) =   10.0  ;  meso(22) = 0.0
  subname(23) = 'HCL             '  ;  dif0(23) = 0.1510  ;  a(23) = 8000.0  ;  meso(23) = 0.0
  subname(24) = 'FMCL            '  ;  dif0(24) = 0.1094  ;  a(24) =   10.0  ;  meso(24) = 0.0
  subname(25) = 'ICL1            '  ;  dif0(25) = 0.0664  ;  a(25) =   10.0  ;  meso(25) = 0.0
  subname(26) = 'ICL2            '  ;  dif0(26) = 0.0664  ;  a(26) =   10.0  ;  meso(26) = 0.0

  ENDIF

  IF ( lddep == 4 ) THEN

  subname(27) = 'HG              '  ;  dif0(27) = 0.1194  ;  a(27) =    0.1  ;  meso(27) = 5000.0
  subname(28) = 'HGIIGAS         '  ;  dif0(28) = 0.0976  ;  a(28) = 8000.0  ;  meso(28) = 0.0

  ENDIF

!-------------------------------------------------------------------------------
! For the time period that corresponds to a meteorology model initialization
! time, many PBL variables are not defined.  At the initialization time for the
! meteorology model, the XUSTAR array may contain all 0.0 values or the XPBL
! array may contain all 0.0 values.  In either case, set place-holder values for
! variables that would otherwise be calculated in this routine.
!-------------------------------------------------------------------------------

  effective = .TRUE.

  xvd(:,:,:) = 0.0

  IF ( ( MAXVAL(xustar) == 0.0 ) .OR.  &
       ( MAXVAL(xpbl)   == 0.0 ) ) THEN  ! assume initialization period

    xradyn(:,:,:) = badval3   ! inverse taken in metcro.F
    xrbndy(:,:,:) = badval3   ! inverse taken in metcro.F
    xrstom(:,:)   = badval3   ! inverse taken in metcro.F

  ELSE

!-------------------------------------------------------------------------------
! Calculate aerodynamic and stomatal resistances.
!-------------------------------------------------------------------------------

    IF ( .NOT. px ) CALL resistcalc

!-------------------------------------------------------------------------------
! Loop over grid cells and calculate dry deposition.
!-------------------------------------------------------------------------------

    DO c = 1, ncols_x
      DO r = 1, nrows_x

        ! Calculate the relative humidity.

        IF ( xtemp1p5(c,r) <= stdtemp ) THEN
          es = vp0 * EXP( 22.514 - (6.15e3 / xtemp1p5(c,r)) )
        ELSE
          es = vp0 * EXP( svp2 * (xtemp1p5(c,r) - stdtemp) /   &
                          (xtemp1p5(c,r) - svp3) )
        ENDIF

        qss = es * 0.622 / (xprsfc(c,r) - es)
        xrh = 100.0 * xwvapor(c,r,1) / qss
        xrh = MIN( 100.0, xrh )


        ddloop: DO l = 1, ltotg

          IF ( TRIM(xdepspc(l)) == 'SULF' ) THEN  ! Sulfate (SULF)

            ! Sulfate calculation follows Wesely (1985), Eqn. 11.

            rbsulf = 1.0 / (0.002*(xustar(c,r,0)**2 + 0.24*xwstar(c,r)**2) /   &
                     xustar(c,r,0))
            xvd(c,r,l) = 1.0 / (xradyn(c,r,0) + rbsulf)

          ELSE

            ! Assign a pH for rain water based on longitude if US simulation.
            ! Otherwise use default pH.

            IF ( ( met_y_centd >=   30.0 ) .AND. ( met_y_centd <=  45.0 ) .AND.  &
                 ( met_x_centd >= -120.0 ) .AND. ( met_x_centd <= -70.0 ) ) THEN
              IF ( xlonc(c,r) > -100.0 ) THEN
                rnph = 4.5
              ELSE
                rnph = 5.5
              ENDIF
            ELSE
              rnph = rnph0
            ENDIF

            ! Assign diffusivity from table.

            diffu = dif0(l)

            IF ( NINT(xdluse(c,r)) == lwater ) THEN  ! water

              IF ( TRIM(xdepspc(l)) == 'HG' ) THEN  ! elemental mercury gas

                ! Water bodies are usually supersaturated with dissolved elemental
                ! mercury, so set the surface resistance to a large value.

                rsurf = 1.0e30

              ELSE

                ! Use CMAQ function for calculating the effective Henry's Law
                ! constant.  Note that original M3DRY was inverse, non-dimensional
                ! Henry's Law (caq/cg).   Water pH is different than rain, and
                ! we need to use the water temperature.

                hplus = 10.0**(-8.1)
                heff  = hlconst(subname(l), xtempg(c,r), effective, hplus)

                ! Make Henry's Law constant non-dimensional.

                heff  = heff * 0.08205 * xtempg(c,r)

                rgw   = 1.0 / (heff * d3 * xustar(c,r,0))
                rsurf = rgw

              ENDIF

            ELSE

              ! Use CMAQ function for calculating the effective Henry's Law
              ! constant.  Note that original M3DRY was inverse, non-dimensional
              ! Henry's Law (caq/cg).

              hplus = 10.0**(-rnph)
              heff  = hlconst(subname(l), xtempm(c,r,1), effective, hplus)

              ! Make Henry's Law constant non-dimensional.

              heff  = heff * 0.08205 * xtempm(c,r,1)

              ! Wet surface resistance.  (Note DELTA = CWC in ADOM lingo.)
              ! This now applies to cuticle and ground.

              IF ( .NOT. px ) THEN  ! approximate canopy wetness - dew from Wesely

                IF ( ( xrainn(c,r) + xrainc(c,r) > 0.025 ) .OR.  &  ! more than a trace
                     ( (0.6 + xwspd10(c,r))*(100.0-xrh) <= 19.0 ) ) THEN

                  xdelta(c,r)  = 1.0
                  xlstwet(c,r) = 0.0

                ELSE

                  IF ( xrnet(c,r) > 10.0 ) THEN  ! day (at night, persist delta)

                    xlstwet(c,r) = xlstwet(c,r) + 1.0

                    IF ( ( xlstwet(c,r) >  0.0 ) .AND.  &
                         ( xlstwet(c,r) <= 2.0 ) ) THEN  ! assume canopy dries out after 2 h
                       xdelta(c,r) = 1.0
                    ELSE
                       xdelta(c,r) = 0.0
                    ENDIF

                  ELSE

                    IF ( xlstwet(c,r) < amiss3 ) THEN  ! initialize to zero
                      xlstwet(c,r) = 0.0
                      xdelta(c,r)  = 0.0
                    ENDIF

                  ENDIF

                ENDIF

              ELSE  ! Pleim-Xiu run

                wrmax = 0.2e-3 * xveg(c,r) * xlai(c,r)   ! [m]
                IF ( xwr(c,r) <= 0.0 ) THEN
                  xdelta(c,r) = 0.0
                ELSE
                  xdelta(c,r) = xwr(c,r) / wrmax   ! refer to SiB model
                  xdelta(c,r) = MIN( xdelta(c,r), 1.0 )
                ENDIF

              ENDIF

              rwet = rcw0 / heff


              ! Dry snow resistance.

              rsnow = rsnow0 * a0 / a(l)


              ! If the surface is cold and wet, use dry snow.

              IF ( xtempg(c,r) < stdtemp ) THEN
                rwetsfc = rsnow
              ELSE
                rwetsfc = rwet
              ENDIF


              ! Dry cuticle resistance.

              IF ( TRIM(xdepspc(l)) == 'NH3' ) THEN
                rcut = 4000.0 * EXP( -0.054 * xrh )
              ELSE
                rcut = rcut0 * a0 / a(l)
              ENDIF


              ! Dry Ground resistance.  (revised according to Erisman)

              hcan  = xzruf(c,r) * 10.0
              rinc  = 14.0 * xlai(c,r) * hcan / xustar(c,r,0)
              rgnd  = rg0 * a0 / a(l)
              rgndc = rgnd + rinc       ! Add in-canopy part


              ! Determine the snow liquid water mass fraction (0.0 to 0.5).

              xm = 0.02 * (xtemp1p5(c,r) - (stdtemp - 1.0))**2
              xm = MIN (xm, 0.5)
              xm = MAX (xm, 0.0)
              IF ( xtemp1p5(c,r) < (stdtemp - 1.0) ) xm = 0.0

              ifsnow = NINT(xsnocov(c,r))
              ifsnow = MAX(0, ifsnow)


              ! Bulk stomatal resistance; include meophyll resistance

              rstom = xrstom(c,r) * dwat / diffu + meso(l) / xlai(c,r)

              ! Bulk surface resistance.

              rci = xveg(c,r) * (1.0/rstom + (1.0-xdelta(c,r)) * xlai(c,r) / rcut +  &
                    ( xdelta(c,r) * xlai(c,r) / rwetsfc ) + (1.0/rgndc)) +           &
                    (1-ifsnow) * ( (1.0 - xveg(c,r)) * ( (1.0-xdelta(c,r)) /         &
                    rgnd + xdelta(c,r) / rwetsfc ) ) +                               &
                    ifsnow * ( (1.0 - xm) / rsnow + xm / (rsndiff + rwet) )

              rsurf = 1.0 / rci

            ENDIF

            ! Aerodynamic and deposition layer resistances.
            ! The RA read in from MM5PX includes ra + rb for water vapor.
            ! Therefore, RA for chemical species is RAC = RA-RBW+RBC
            ! where RBW is rb for water and RBC is rb for the chemical.

            scw = kvis / dwat
            scc = kvis / diffu
            rbw = 5.0 / xustar(c,r,0) * (scw/pr)**0.66667
            rbc = 5.0 / xustar(c,r,0) * (scc/pr)**0.66667
            rac = xradyn(c,r,0) + rbc

            ! Store boundary resistance for water for file output.

            xrbndy(c,r,0) = rbw

            ! Dry deposition velocity.

            xvd(c,r,l) = 1.0 / (rsurf + rac)

            IF ( xvd(c,r,l) < 0.0 ) GOTO 8000

          ENDIF

        ENDDO ddloop

      ENDDO
    ENDDO

  ENDIF

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8000 WRITE (6,9000) c, r, TRIM(subname(l)), xvd(c,r,l)
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: M3DRY',                              &
              /, 1x, '***   NEGATIVE DRY DEPOSITION VELOCITY',             &
              /, 1x, '***   POINT   = ', 2i5,                              &
              /, 1x, '***   SPECIES = ', a,                                &
              /, 1x, '***   VD      = ', e13.6,                            &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE m3dry
