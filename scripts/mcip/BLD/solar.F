
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
! $Header: /project/work/rep/MCIP2/src/mcip2/solar.F,v 1.2 2004/08/30 16:45:47 tlotte Exp $ 


SUBROUTINE solar (sdate, gmt)

!-------------------------------------------------------------------------------
! Name:     Solar Radiation
! Purpose:  Computes
!             1) solar radiation reaching the ground (W/M2),
!             2) shortwave radiation absorbed at the ground (W/M2),
!             3) long wave radiation at the ground (W/M2), 
!             4) net radiation (W/M2),
!             5) and a time-dependent adjustment to albedo, based on
!                zenith angle.  
! Revised:  24 Mar 1998  Original version.  (A. Bourgeois)
!           10 Sep 2001  Converted to free-form f90.  Changed JUDATE to
!                        SDATE.  (T. Otte)
!           07 Jul 2004  Removed XFLAGS.  (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE const_mete

  IMPLICIT NONE

  REAL                         :: albedo
  REAL,          PARAMETER     :: ang1       = deg2rad * 90.0 / 91.3125
  REAL                         :: arg
  REAL                         :: bscbar
  INTEGER                      :: c
  REAL                         :: cldfr      ( 3 )    ! low, middle, high
  REAL                         :: coszen
  REAL                         :: decrad
  REAL                         :: delz
  REAL,          PARAMETER     :: denw       = 1000.0 ! water density [kg m^-3]
  REAL                         :: dzero               ! radian angle of orbit
  REAL,          PARAMETER     :: emsvtya    = 0.90   ! emiss of sky
  REAL,          PARAMETER     :: emsvtyg    = 0.97   ! emiss of grassy soil
  REAL                         :: fbsc
  REAL                         :: fbscd
  REAL                         :: frax
  REAL                         :: ftabs
  REAL                         :: ftabsd
  REAL                         :: ftsca
  REAL                         :: ftscad
  REAL,          INTENT(IN)    :: gmt
  REAL                         :: hrangle
  INTEGER                      :: jday
  INTEGER                      :: k
  REAL                         :: latrad
  INTEGER                      :: lc
  REAL                         :: path                ! path length
  CHARACTER*16,  PARAMETER     :: pname      = 'SOLAR'
  REAL                         :: prw                 ! precipitable water [cm]
  INTEGER                      :: r
  REAL                         :: radiat
  INTEGER,       INTENT(IN)    :: sdate
  REAL                         :: sp                  ! sfc P : std sfc P
  REAL                         :: sqadr
  REAL                         :: sunup
  REAL                         :: tranac              ! absorp transm thru cld
  REAL,          PARAMETER     :: tranacld(3) = (/ 0.98, 0.85, 0.80 /)
  REAL                         :: transc              ! scat transm thru cld
  REAL,          PARAMETER     :: transcld(3) = (/ 0.80, 0.60, 0.48 /)
  REAL                         :: transmis            ! shortwave transmis
  REAL                         :: xc
  REAL                         :: zenith

!-------------------------------------------------------------------------------
! Calculate SQADR, the square of the ratio of the average distance of the
! earth from the sun to its location at any time of the year.
! See Pielke, p.211, Eq.(8-58A).
!
! DZERO is defined in terms of the day number starting with 0
! on January 1 and ending on 364 on December 31.
!-------------------------------------------------------------------------------

  jday   = MOD(sdate, 1000)
  dzero  = 2.0 * pi * (jday - 1.0) / 365.0
  sqadr  = 1.000110 + 0.034221*COS(dzero) + 0.001280*SIN(dzero) +  &
           0.000719*COS(2.0*dzero) + 0.000077*SIN(2.0*dzero)

!-------------------------------------------------------------------------------
! Loop over horizontal array and compute solar radiation terms.
!-------------------------------------------------------------------------------

  DO c = 1, ncols_x
    DO r = 1, nrows_x

      ! Insolation at given grid point from solar angle formula for GMT time
      ! at a longitude in North America

      ! local hour angle, inclination angle [rad], and latitude angle [rad].

      hrangle = ( gmt + xlonc(c,r) / 15.0  - 12.0 ) * 15.0 * deg2rad
      decrad  = deg2rad * 23.5 * SIN(( MOD(sdate, 1000) - 81.1875 ) * ang1)
      latrad  = deg2rad * xlatc(c,r)

      ! Radiation calculation [watt/m^2]

      coszen  = SIN(latrad) * SIN(decrad) +  &
                COS(latrad) * COS(decrad) * COS(hrangle)

      zenith  = ACOS(coszen)
      albedo  = xalbedo(c,r)

      ! Calculate short-wave irradiance if sun is above horizon.

      radiat = 0.0
      sunup  = AMAX1(0.0,coszen)
      prw    = 0.0

      IF ( sunup /= 0.0 ) THEN   ! sun is above horizon

        path = 1.0 / AMAX1(coszen,0.00005)

        ! Precipitable water --  PRW(CM)

        DO K = 1, metlay
          delz = x3htf(c,r,k) - x3htf(c,r,k-1)
          prw  = prw + 100.0/denw * xwvapor(c,r,k) * xdensam(c,r,k) * delz
        ENDDO

        ! SW transmissivity determined from look-up table from
        ! shortwave model of Carlson and Boland (JAM, 1978).

        ! Direct radiation: Use actual solar path length.
        ! FBSC is SW backscattering coefficient for direct radiation.

        CALL transm (path, prw, ftabs, ftsca, fbsc)

        ! TRANSMISSIONS FROM TABLE ARE VALID FOR SURFACE PRESSURE OF
        ! 1013.25 MB.  WE CORRECT BELOW FOR THE ACTUAL SURFACE PRESSURE
        ! ASSUMING THAT ATTENUATIVE EFFECTS OF WATER VAPOR ARE
        ! SECONDARY TO THOSE OF OTHER ACTIVE CONSTITUENTS
        ! (AIR MOLECULES, AEROSOLS, OZONE)

        sp    = xpresf(c,r,0) / 101325.0
        ftabs = sp*(ftabs - 1.0) + 1.0    ! SW absorption transm (direct rad)
        ftsca = sp*(ftsca - 1.0) + 1.0    ! SW scattering transm (direct rad)

        ! Diffuse ratiation: Use path length (diffusivity factor)=1.67
        ! from Rodgers and Walshaw, 1967.
        ! FBSCD is SW backscattering coefficient for diffuse radiation.

        path = 1.67
        CALL transm (path, prw, ftabsd, ftscad, fbscd)

        ftabsd = sp*(ftabsd - 1.0) + 1.0  ! SW absorption transm (diffuse rad)
        ftscad = sp*(ftscad - 1.0) + 1.0  ! SW scattering transm (diffuse rad)

        tranac = 1.0
        transc = 1.0

        IF ( lcld == 1 ) THEN

          cldfr(1) = xcfrach(c,r)
          cldfr(2) = xcfracm(c,r)
          cldfr(3) = xcfracl(c,r)

          DO lc = 1, 3
            tranac = tranac * (1.0 - ((1.0 - tranacld(lc)) * cldfr(lc)))
            transc = transc * (1.0 - ((1.0 - transcld(lc)) * cldfr(lc)))   
          ENDDO

          ! Minimum absorption and scattering transmissivities
          ! through cloud from Drummond and Hickey (1971).

          tranac = AMAX1 ( tranac, 0.70 )
          transc = AMAX1 ( transc, 0.44 )

        ENDIF

        ! Calculate mean backscattering coefficient (BSCBAR) including
        ! effect of clouds.

        bscbar   = ( fbscd * (1.0 - ftscad) + (1.0 - transc) ) /  &
                   ( (1.0 - ftscad) + (1.0 - transc) )

        xc       = bscbar * (1.0 - ftscad * transc) * ftabsd * tranac

        transmis = tranac * transc * ftabs * ( ftsca +  &
                   (1.0 - ftsca) * (1.0 - fbsc) ) / (1.0 - xc * albedo)

        frax = MAX ( coszen - 0.0001, 0.0 )

        radiat = solcnst * transmis * sqadr * frax   ! sw radiation

      ENDIF

      xrgrnd(c,r) = MAX1( radiat, 0.0 )  
      xgsw  (c,r) = xrgrnd(c,r) * ( 1.0 - albedo )
      xglw  (c,r) = ( emsvtyg * stfblz * xtempg(c,r)**4  &
                    - emsvtya * stfblz * xtempm(c,r,1)**4 )
      xrnet (c,r) = xgsw(c,r) - xglw(c,r)           ! net radiation

    ENDDO        
  ENDDO

END SUBROUTINE solar
