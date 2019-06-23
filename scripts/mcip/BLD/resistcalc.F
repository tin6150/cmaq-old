
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
! $Header: /project/work/rep/MCIP2/src/mcip2/resistcalc.F,v 1.4 2005/08/29 14:37:06 tlotte Exp $ 


SUBROUTINE resistcalc

!-------------------------------------------------------------------------------
! Name:     Resistance Calculation
! Purpose:  Calculates aerodynamic and stomatal resistances required
!           to compute dry deposition velocities.
! Notes:    Does not include effects of seasons.  Assumed maximum leaf area
!           index for each land use category.
! Revised:  18 Sep 2001  Original version.  (J. Pleim and T. Otte)
!           21 Dec 2001  Added IMPLICIT NONE and missing variable
!                        declarations.  (S. Howard and T. Otte)
!           23 Jan 2002  Changed missing value on XRADYN and XRSTOM to
!                        BADVAL3.  (T. Otte)
!           07 Feb 2002  Changed logic to define water point using dominant
!                        land use category.  (T. Otte)
!           10 Jun 2003  Changed definition of F2 to be based on land use
!                        category.  Changed variable name GS to GSFC to avoid
!                        confusion with (future) variable GS in MCIPPARM.
!                        Added snow condition to calculation of saturation
!                        vapor pressure.  (J. Pleim and T. Otte)
!           07 Jul 2004  Removed XFLAGS.  (T. Otte)
!           09 Mar 2005  Removed unused variable W2AVAIL.  (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE const
  USE const_pbl
  USE lradmdat
  USE xvars
  USE parms3, ONLY: badval3

  IMPLICIT NONE

  REAL                         :: alogz1z0
  INTEGER                      :: c
  REAL                         :: es
  REAL                         :: f1
  REAL                         :: f2
  REAL                         :: f3
  REAL,          PARAMETER     :: f3min      = 0.25
  REAL                         :: f4
  REAL,          PARAMETER     :: ftmin      = 0.0000001 ! [m/s]
  REAL                         :: ftot
  REAL                         :: ga
  REAL                         :: gsfc
  INTEGER                      :: lu
  CHARACTER*16,  PARAMETER     :: pname      = 'RESISTCALC'
  REAL                         :: psih
  REAL                         :: psih0
  REAL                         :: q1
  REAL                         :: qss
  INTEGER                      :: r
  REAL                         :: radf
  REAL                         :: radl
  REAL                         :: raw
  REAL,          PARAMETER     :: rsmax      = 5000.0   ! [s/m]
  REAL,          PARAMETER     :: svp2       = 17.67    ! from MM5
  REAL,          PARAMETER     :: svp3       = 29.65    ! from MM5
  REAL                         :: t1
  REAL,          PARAMETER     :: wfc        = 0.240
  REAL,          PARAMETER     :: wsat       = 0.451    ! vol frac for loam
  REAL,          PARAMETER     :: wwlt       = 0.155
  REAL                         :: z1
  REAL                         :: z1ol
  REAL                         :: zntol

  REAL,          PARAMETER     :: w2mxav     = wfc - wwlt

!-------------------------------------------------------------------------------
! Loop over grid cells to calculate aerodynamic and stomatal resistances.
!-------------------------------------------------------------------------------

  DO c = 1, ncols_x
    DO r = 1, nrows_x

!-------------------------------------------------------------------------------
! Calculate aerodynamic resistance XRADYN.
!-------------------------------------------------------------------------------

      t1       = xtempm (c,r,1)
      q1       = xwvapor(c,r,1)
      lu       = NINT( xdluse (c,r) )
      z1       = x3htm  (c,r,1)
      z1ol     = z1 / xmol(c,r)
      zntol    = xzruf(c,r) / xmol(c,r)
      alogz1z0 = ALOG(z1/xzruf(c,r))

      IF ( z1ol >= 0.0 ) THEN 

        IF ( z1ol > 1.0 ) THEN
           psih0 = 1.0 - betah - z1ol
        ELSE
           psih0 = -betah * z1ol
        ENDIF

        IF ( zntol > 1.0 ) THEN
          psih = psih0 - (1.0 - betah - zntol)
        ELSE
          psih = psih0 + betah * zntol
        ENDIF

      ELSE

        psih = 2.0 * ALOG( (1.0 + SQRT(1.0 - gamah*z1ol)) /  &
                           (1.0 + SQRT(1.0 - gamah*zntol)) )

      ENDIF

      xradyn(c,r,0) = pro * ( alogz1z0 - psih ) / ( vkar * xustar(c,r,0) )

      ! Compute species dependent RA.

      xradyn(c,r,1:luradm) = badval3   ! inverse taken in metcro.F
      xradyn(c,r,lu)       = xradyn(c,r,0)

!-------------------------------------------------------------------------------
! Calculate stomatal resistance XRSTOM.
!-------------------------------------------------------------------------------

      ! Effects of transpiration.

      IF ( lu == lwater ) THEN  ! water

        xrstom(c,r) = badval3   ! inverse taken in metcro.F

      ELSE

        ! Effects of radiation.

        IF ( xrstmin(c,r) > 130.0 ) THEN
          radl = 30.0   ! [W/m**2]
        ELSE
          radl = 100.0  ! [W/m**2]
        ENDIF

        radf = 1.1 * xrgrnd(c,r) / ( radl * xlai(c,r) )  ! NP89 - EQN34
        f1   = (xrstmin(c,r) / rsmax + radf) / (1.0 + radf)

        ! Effects of soil moisture
        ! When not using LSM, soil moisture is estimated by moisture
        ! availability as a function of dominant land use category.
        ! Soil parameters here are based on loam.  This formulation
        ! does not include effects of precipitation.

        f2 = xf2def(c,r)
 
        ! Effects of air temperature following Avissar (1985) and Xiu (7/95).

        IF ( t1 <= 302.15 ) THEN
          f4 = 1.0 / (1.0 + EXP(-0.41 * (t1 - 282.05)))
        ELSE
          f4 = 1.0 / (1.0 + EXP( 0.50 * (t1 - 314.00)))
        ENDIF

        ftot = MAX( (xlai(c,r) * f1 * f2 * f4), ftmin )
        gsfc = ftot / xrstmin(c,r)

        ! rb(water) = 2/(k*ust) (Scw/Pran)^2/3
        !           = 5/ust (0.599/0.709)^2/3
        !           = 4.47/ust

        raw = xradyn(c,r,0) + 4.47 / xustar(c,r,0)    ! 4.47 = (Scw/Pran)^2/3
        ga  = 1.0 / raw

        ! Compute the saturated mixing ratio at surface temperature (XTEMPG).
        ! Saturation vapor pressure [mb] of water.

        IF ( ( xsnocov(c,r) > 0.0 ) .OR. ( xtempg(c,r) <= stdtemp ) ) THEN
          es = vp0 * EXP(22.514 - 6.15e3/xtempg(c,r))
        ELSE        
          es = vp0 * EXP(svp2 * (xtempg(c,r) - stdtemp) / (xtempg(c,r) - svp3))
        ENDIF

        qss  = es * 0.622 / (xprsfc(c,r) - es)

        ! Compute humidity effect according to RH at leaf surface.

        f3 = 0.5 * (gsfc - ga + SQRT(ga * ga + ga * gsfc * (4.0 * q1 /   &
                                     qss - 2.0) + gsfc * gsfc)) / gsfc
        f3 = MIN( MAX(f3,f3min), 1.0 ) 

        xrstom(c,r) = 1.0 / (gsfc * f3)

      ENDIF

    ENDDO
  ENDDO

END SUBROUTINE resistcalc
