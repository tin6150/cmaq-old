
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


SUBROUTINE gridgeometry (inner, f, smap, xlon, xlat)

!-------------------------------------------------------------------------------
! Name:     Grid Geometry
! Purpose:  Calculates latitude, longitude, map-scale factors, and Coriolis
!           parameter based on map projection information.
! Notes:    Liberally adapted from NCAR's MM5/TERRAIN/src/terrestial_info.F.
!           Assumes calculations only occur for coarsest domain in projection.
!           05 Dec 2001  Original version in TERRAIN.  (NCAR)
!           05 Aug 2004  Modified for MCIP.  (T. Otte)
!           14 Feb 2005  Changed naming convention for I and J variables to
!                        Y and X to make code more general.  (T. Otte)
!           18 Jul 2005  Corrected calculation of PSI1 for Southern Hemisphere.
!                        (T. Otte)
!-------------------------------------------------------------------------------

  USE metinfo, nx => met_nx, ny => met_ny

  IMPLICIT NONE

  REAL,          PARAMETER     :: a_km      = 6370.0  ! [km]
  REAL                         :: c2
  REAL                         :: cell
  REAL                         :: cell1
  REAL                         :: cell2
  REAL                         :: cntrx
  REAL                         :: cntrxm
  REAL                         :: cntry
  REAL                         :: cntrym
  REAL(8)                      :: conv
  REAL                         :: deglat
  REAL(8)                      :: degran
  REAL                         :: dx_km
  REAL,          INTENT(OUT)   :: f         ( : , : )
  REAL                         :: fac
  REAL                         :: flp
  REAL                         :: flpp
  INTEGER                      :: i
  INTEGER                      :: ii1
  INTEGER,       INTENT(IN)    :: inner                ! 1 = inner, 0 = outer
  INTEGER                      :: j
  INTEGER                      :: jj1
  REAL,          PARAMETER     :: omega2    = 2.0 * 7.2921e-5  ! sidereal day
  REAL                         :: phi1
  REAL                         :: phictr
  REAL(8)                      :: pi
  CHARACTER*16,  PARAMETER     :: pname     = 'GRIDGEOMETRY'
  REAL                         :: pole
  REAL                         :: psi1
  REAL                         :: psix
  REAL                         :: psx
  REAL                         :: r
  REAL,          INTENT(OUT)   :: smap      ( : , : )
  REAL                         :: x
  REAL                         :: xcntr
  REAL,          INTENT(OUT)   :: xlat      ( : , : )
  REAL                         :: xlat1
  REAL,          INTENT(OUT)   :: xlon      ( : , : )
  REAL                         :: y
  REAL                         :: ycntr

!-------------------------------------------------------------------------------
! Calculate geometrical conversion factors.
!-------------------------------------------------------------------------------

  pi     = 4.0d0 * DATAN(1.0d0)
  conv   = 1.8d2 / pi
  degran = 1.0d0 / conv

  dx_km  = met_resoln / 1000.0

!-------------------------------------------------------------------------------
! Set array dimensions.
!-------------------------------------------------------------------------------

  IF ( ( SIZE(f) /= SIZE(smap) ) .OR. ( SIZE(f) /= SIZE(xlat) ) .OR.  &
       ( SIZE(f) /= SIZE(xlon) ) ) GOTO 8000

!-------------------------------------------------------------------------------
! Get geometrical parameters based on map projection.
!-------------------------------------------------------------------------------

  IF ( met_y_centd < 0.0 ) THEN
    fac = -1.0  ! Southern Hemisphere
  ELSE
    fac =  1.0  ! Northern Hemisphere
  ENDIF

  pole   = 90.0 * fac

  cntrxm = FLOAT( met_nxcoarse + 1 ) / 2.0
  cntrym = FLOAT( met_nycoarse + 1 ) / 2.0

  cntrx  = (cntrxm - met_x_11)*FLOAT(met_gratio) - 0.5*FLOAT(inner) + 1.0
  cntry  = (cntrym - met_y_11)*FLOAT(met_gratio) - 0.5*FLOAT(inner) + 1.0

  IF ( met_mapproj /= 3 ) THEN  ! NOT Mercator

    psi1 = ( 90.0 - fac*met_tru1 ) * degran
    psi1 = psi1 * fac
    psx  = ( pole - met_y_centd  ) * degran

    IF ( met_mapproj == 2 ) THEN  ! polar stereographic
      cell  = a_km * SIN(psx) / met_cone_fac
      cell2 = (1.0 + COS(psi1)) / (1.0 + COS(psx))
    ELSE IF ( met_mapproj == 1 ) THEN  ! Lambert conformal
      cell  = a_km * SIN(psi1) / met_cone_fac
      cell2 = ( TAN(psx/2.0) ) / ( TAN(psi1/2.0) )
    ENDIF

    r     = cell * (cell2)**met_cone_fac
    xcntr = 0.0
    ycntr = -r

  ELSE  ! Mercator true at PHI1

    phi1   = 0.0
    phi1   = phi1 * degran
    c2     = a_km * COS(phi1)
    xcntr  = 0.0
    phictr = met_y_centd * degran
    cell   = COS(phictr) / (1.0 + SIN(phictr))
    ycntr  = -c2 * ALOG(cell)

  ENDIF

!-------------------------------------------------------------------------------
! Calculate latitude, longitude, map-scale factors, and Coriolis parameter.
!-------------------------------------------------------------------------------

  ii1 = nx - inner
  jj1 = ny - inner

  DO i = 1, ii1

    x = xcntr + (i-cntrx)*dx_km

    DO j = 1, jj1

      y = ycntr + (j-cntry)*dx_km

      IF ( met_mapproj /= 3 ) THEN  ! NOT Mercator

        r = SQRT(x*x + y*y)
        IF ( met_y_centd < 0.0 ) r = -r

        IF ( y == 0.0 ) THEN
          IF ( x >= 0.0 ) THEN
            flp =  90.0 * degran
          ELSE
            flp = -90.0 * degran
          ENDIF
        ELSE
          IF ( met_y_centd < 0.0 ) THEN
            flp = ATAN2(x,y)
          ELSE
            flp = ATAN2(x,-y)
          ENDIF
        ENDIF

        flpp = flp / met_cone_fac / degran + met_x_centd
        IF ( flpp >  180.0 ) flpp = flpp - 360.0
        IF ( flpp < -180.0 ) flpp = flpp + 360.0
        xlon(i,j) = flpp

        IF ( met_mapproj == 1 ) THEN  ! Lambert conformal
          cell  = r * met_cone_fac / (a_km * SIN(psi1))
          cell1 = TAN(psi1/2.0) * cell**(1.0/met_cone_fac)
        ELSE IF ( met_mapproj == 2 ) THEN  ! polar stereographic
          cell  = r / a_km
          cell1 = cell / (1.0 + COS(psi1))
        ENDIF

        cell2     = ATAN(cell1)
        psx       = 2.0 * cell2 / degran
        xlat(i,j) = pole - psx
        xlat1     = xlat(i,j) * degran
        psix      = psx * degran

        IF ( met_mapproj == 1 ) THEN  ! Lambert conformal
          f(i,j)    = omega2 * SIN(xlat1)
          smap(i,j) = (SIN(psi1) / SIN(psix)) *  &
                      ( (TAN(psix/2.0) / TAN(psi1/2.0))**met_cone_fac )
        ELSE IF ( met_mapproj == 2 ) THEN  ! polar stereographic
          f(i,j)    = omega2 * SIN(xlat1)
          smap(i,j) = ( (1.0 + COS(psi1)) / (1.0 + COS(psix)) )**met_cone_fac
        ENDIF

      ELSE IF ( met_mapproj == 3 ) THEN  ! Mercator

        xlon(i,j) = met_x_centd + ((x-xcntr) / c2) / degran
        IF ( xlon(i,j) >  180.0 ) xlon(i,j) = xlon(i,j) - 360.0
        IF ( xlon(i,j) < -180.0 ) xlon(i,j) = xlon(i,j) + 360.0

        cell      = EXP(y/c2)
        xlat(i,j) = 2.0 * (ATAN(cell) / degran) - 90.0
        deglat    = xlat(i,j) * degran
        smap(i,j) = COS(phi1) / COS(deglat)
        f(i,j)    = omega2 * SIN(deglat)

      ENDIF

    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! If this calculation is for the inner staggered domain, fill last row and
! column with data buffered from previous row and column.  These data are
! non-physical, so they will not be used.  However, for visualization purposes,
! "reasonable" values are used so the array bounds (max and min) are not thrown
! off by an unreasonably large (positive or negative) value.
!-------------------------------------------------------------------------------

  IF ( inner == 1 ) THEN

    f   (nx,:) = f   (nx-1,:)   ;   f   (:,ny) = f   (:,ny-1)
    smap(nx,:) = smap(nx-1,:)   ;   smap(:,ny) = smap(:,ny-1)
    xlat(nx,:) = xlat(nx-1,:)   ;   xlat(:,ny) = xlat(:,ny-1)
    xlon(nx,:) = xlon(nx-1,:)   ;   xlon(:,ny) = xlon(:,ny-1)

  ENDIF

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8000 WRITE (6,9000)
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                                      &
              /, 1x, '*** SUBROUTINE: GRIDGEOMETRY',               &
              /, 1x, '***   INPUT ARRAYS ARE NOT SAME SIZE',       &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE gridgeometry
