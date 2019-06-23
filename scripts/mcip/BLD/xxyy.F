
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


SUBROUTINE xxyy (ylat, ylon, xx, yy, xlonc, xlatc, mapproj, xn, trulat1, pole)

!-------------------------------------------------------------------------------
! Name:     Get X and Y
! Purpose:  Get distance from center from latitude and longitude.
! Notes:    Based on Stauffer's PSU GINT code.
! Revised:  12 Jan 1999  Original version.  (T. Otte)
!           05 Oct 1999  Converted to free-form f90.  (T. Otte)
!           23 Feb 2005  Modified for MCIP.  (T. Otte)
!-------------------------------------------------------------------------------

  USE const, ONLY: pi180

  IMPLICIT NONE

  REAL,          PARAMETER     :: a          = 6370.0  ! kilometers
  REAL                         :: c2
  REAL                         :: cell
  REAL                         :: cell2
  REAL                         :: cl3
  REAL                         :: flp
  REAL                         :: flpp
  INTEGER,       INTENT(IN)    :: mapproj
  REAL                         :: phir
  CHARACTER*16,  PARAMETER     :: pname      = 'XXYY'
  REAL,          INTENT(IN)    :: pole
  REAL                         :: psi1
  REAL                         :: psx
  REAL                         :: r
  REAL,          INTENT(IN)    :: trulat1
  REAL,          INTENT(IN)    :: xlatc
  REAL,          INTENT(IN)    :: xlonc
  REAL,          INTENT(IN)    :: xn
  REAL                         :: xrot
  REAL,          INTENT(OUT)   :: xx
  REAL,          INTENT(IN)    :: ylat
  REAL,          INTENT(IN)    :: ylon
  REAL,          INTENT(OUT)   :: yy

!-------------------------------------------------------------------------------
! Convert latitude and longitude coordinates to distance from center point
! (in kilometers) based on map projection.
!-------------------------------------------------------------------------------

  SELECT CASE ( mapproj )

    CASE ( 1 )  ! Lambert Conformal

      xrot  = xlonc + pole / xn
      psi1  = ( pole - trulat1 ) * pi180
      flpp  = ( ylon - xrot ) * pi180
      flp   = xn * flpp
      psx   = ( pole - ylat ) * pi180
      cell  = a * SIN ( psi1 ) / xn
      cell2 = ( TAN ( psx / 2.0 ) ) / ( TAN ( psi1 / 2.0 ) )
      r     = cell * ( cell2 ) ** xn
      xx    = r * COS ( flp )
        IF ( xlatc < 0.0 ) xx = -xx
      yy    = r * SIN ( flp )

    CASE ( 2 )  ! Polar Stereographic

      xrot  = xlonc + pole / xn
      psi1  = ( pole - trulat1 ) * pi180
      flpp  = ( ylon - xrot ) * pi180
      flp   = xn * flpp
      psx   = ( pole - ylat ) * pi180
      cell  = a * SIN ( psx ) / xn
      cl3   = 1. + COS ( psx )
        IF ( cl3 < 0.01 ) cl3 = 0.01
      cell2 = ( 1. + COS ( psi1 ) ) / cl3
      r     = cell * ( cell2 ) ** xn
      xx    = r * COS ( flp )
        IF ( xlatc < 0.0 ) xx = -xx
      yy    = r * SIN ( flp )

    CASE ( 3 )  ! Mercator

      c2   = a * COS ( trulat1 )
      phir = ylat * pi180
      cell = COS ( phir ) / ( 1.0 + SIN ( phir ) )
      xx   = c2 * ( ylon - xlonc ) * pi180
      yy   = -c2 * ALOG ( cell )

    CASE DEFAULT

      GOTO 8000

  END SELECT

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8000 WRITE (6,9000) mapproj
      GOTO 1001

 9000 FORMAT (/, 1X, 70('*'),                                    &
              /, 1X, '*** SUBROUTINE: XXYY',                     &
              /, 1X, '***   UNKNOWN MAP PROJECTION IDENTIFIER',  &
              /, 1X, '***   MAPPROJ IS ', i2,                    &
              /, 1X, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE xxyy
