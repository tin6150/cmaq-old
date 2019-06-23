
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
! $Header: /project/work/rep/MCIP2/src/mcip2/slflux.F,v 1.2 2005/08/29 14:37:47 tlotte Exp $ 


SUBROUTINE slflux (alatdeg, wm, thm, qvm, thref, qvref, zf1, zmean1, zo,  &
                   hfxmax, ustar, thstar, qvstar, amol)

!-------------------------------------------------------------------------------
! Name:     Surface Layer Flux
! Purpose:  Computes surface fluxes from MM5 first layer mean wind and
!           temperature using the surface-layer similarity functions.
! Notes:    - Refer to Byun (1990):  "On the Analytical Solutions of
!             Flux-Profile Relationships for the Atmospheric Surface Layer."
!           - Also see Byun:  "Flux-Profile Relationship for Strongly Stable
!             Atmospheric Surface Layer."  It deals with additional equation
!             to deal with very strong stable layer (z/L > 1) obtained through
!             the integration of 
!               phim = betam + z/L
!               phih = PRO * ( betah + z/L )
!           - With the adition of this similarity function, the routine 
!             is valid for RI values less than PRO (=0.74)
!           - Here, assume layer-1 values are within the constant flux layer.
! Revised:  16 Aug 1996  Original version.  (D. Byun)
!           04 Feb 1998  Changed include method nonglobal includes.  (D. Byun)
!           10 Sep 2001  Converted to free-form f90.  (T. Otte)
!           12 Aug 2005  Removed unused statement functions FPHIH and FPHIM.
!                        (T. Otte)
!-------------------------------------------------------------------------------

  USE const_mete
  USE const_pbl

  IMPLICIT NONE

  REAL                         :: alat
  REAL,          INTENT(IN)    :: alatdeg                  ! [deg]
  REAL                         :: alnzzo
  REAL,          INTENT(OUT)   :: amol                     ! [m]
  REAL                         :: arg2
  REAL                         :: bh
  REAL,          PARAMETER     :: big        = 1.0e4
  REAL                         :: bm
  REAL,          EXTERNAL      :: cvmgp
  REAL                         :: det
  REAL                         :: dqv
  REAL                         :: fcori
  REAL                         :: fpi
  REAL                         :: fpsih
  REAL                         :: fpsim
  REAL                         :: fqi
  REAL                         :: fthti
  REAL                         :: fx
  REAL                         :: fy
  REAL                         :: fzs
  REAL                         :: fzss
  REAL                         :: fzu
  REAL                         :: gh
  REAL                         :: gm
  REAL                         :: hfxmax
  REAL                         :: pb
  REAL                         :: pii
  REAL                         :: qb
  REAL                         :: qi
  REAL,          INTENT(IN)    :: qvm                      ! [kg kg^-1]
  REAL,          INTENT(IN)    :: qvref                    ! [kg kg^-1]
  REAL,          INTENT(OUT)   :: qvstar                   ! [kg kg^-1]
  REAL                         :: ri
  REAL                         :: ribest
  REAL                         :: ribone
  REAL                         :: ribtwo
  REAL                         :: ricut
  REAL                         :: sb
  REAL                         :: si
  REAL,          PARAMETER     :: small      = 1.0e-4
  REAL                         :: speed
  REAL,          INTENT(IN)    :: thm                      ! [K]
  REAL,          INTENT(IN)    :: thref                    ! [K]
  REAL                         :: thref1
  REAL,          INTENT(OUT)   :: thstar                   ! [K]
  REAL                         :: thtb
  REAL                         :: thti
  REAL,          INTENT(OUT)   :: ustar                    ! [m s^-1]
  REAL,          INTENT(IN)    :: wm                       ! [m s^-1]
  REAL                         :: x
  REAL                         :: x1
  REAL                         :: xo
  REAL                         :: y
  REAL                         :: y1
  REAL                         :: yo
  REAL                         :: zeta
  REAL                         :: zeta1
  REAL,          PARAMETER     :: zetamin    = rimin
  REAL                         :: zetao
  REAL                         :: zetap
  REAL,          INTENT(IN)    :: zf1                      ! [m]
  REAL                         :: zinv
  REAL,          INTENT(IN)    :: zmean1                   ! [m]
  REAL,          INTENT(IN)    :: zo                       ! [m]
  REAL                         :: zzo

!-------------------------------------------------------------------------------
! Statement functions.
!-------------------------------------------------------------------------------

  ! Functions for stable and unstable cases.

  fx(zeta)      = cvmgp( 0.0, ABS( 1.0 - gamam * zeta )**0.25, zeta )

  fy(zeta)      = cvmgp( 0.0, ABS( 1.0 - gamah * zeta )**0.05, zeta )

  fpsim(zeta,x) = cvmgp( - betam * zeta,  &
                         2.0*ALOG( x+1.0 ) + ALOG(1.0+x*x) - 2.0*ATAN(x), zeta )

  fpsih(zeta,y) = cvmgp( - betah * zeta, 2.0 * ALOG( y+1.0 ), zeta )


  ! Surface layer analytical solution for Ri:  stable case

  fzs(ri,bm,bh) = ( -2.0 * bh * ri + 1.0 -                            &
                    SQRT( 1.0 + 4.0 * ( bh - bm ) * ri / pro ) )  /   &
                    ( 2.0 * bh * ( bm * ri - 1.0 ) )


  ! Surface layer analytical solution for Ri:  very stable case Z/L

  fzss(ri,bm,bh) = bm * ( (ri-0.5) + SQRT( (ri-0.5)**2 + ri*(pro-ri) ) ) /  &
                   (pro - ri)


  ! Surface layer analytical solution for Ri:  unstable case

  fzu(qi,thti,gm) = -2.0*SQRT(qi)*COS(thti/3.0) + 1.0/(3.0*gm)

  fqi(gm,gh,si)   = ( 1.0/gm**2 + 3.0*gh*si*si/gm ) / 9.0

  fpi(gm,gh,si)   = ( -2.0/gm**3 + 9.0*(-gm/gm+3.0)*si*si/gm ) / 54.0

  fthti(pii,qi)   = ACOS( pii / qi**1.5 )

!-------------------------------------------------------------------------------
! Compute surface parameters.
!-------------------------------------------------------------------------------

  ricut  = pro * (betah + 1.0) / (betam + 1.0)**2    ! Cut-off at z/L = 1

  alat   = deg2rad * alatdeg
  fcori  = 2.0 * eomega * SIN(alat)
  speed  = MAX( 1.0, wm )
  zzo    = MAX( zmean1/zo, 10.0 )   ! limit Z/ZO to 10.0
  zinv   = 1.0 / zzo
  alnzzo = LOG( zzo )

  ! Limit moisture to move one direction (surface to air).

  dqv = cvmgp( (qvm - qvref), 0.0, -(qvm - qvref) )

!-------------------------------------------------------------------------------
! Estimate fluxes using surface layer similarity - analytical solution.
!-------------------------------------------------------------------------------

  ! Estimate bulk Richardson number.

  ribest = grav * (zmean1-zo) * (thm - thref) / (thref * speed * speed)

  ! Compute RibOne = criteria for z/L = 1 from Eq(18) of Byun (1990).

  ribone = pro * ( 1.0-zinv ) * ( alnzzo + betah * ( 1.0-zinv ) ) /  &
                                ( alnzzo + betam * ( 1.0-zinv ) )**2

  ! Compute RibTwo = criteria for z/L from Holstag.

  ribtwo = pro * ( 1.0-zinv ) * ( betah * alnzzo + ( 1.0-zinv ) ) /  &
                                ( betam * alnzzo + ( 1.0-zinv ) )**2 

!-------------------------------------------------------------------------------
! Limit the temperature difference (THM-THREF) to follow RICR allowed.
!-------------------------------------------------------------------------------

  IF ( ribest > ricr ) THEN

    thref1 = grav * ( zmean1-zo ) * thm /  &
             ( speed * speed * ricr + grav * ( zmean1-zo ) )
    ribest = ricr

  ELSE IF ( ribest < rimin ) THEN  ! RIMIN=-4.75 , ???-2.0 a la MM5

    thref1 = grav * ( zmean1-zo ) * thm /  &
             ( speed * speed * rimin + grav * ( zmean1-zo ) )
    ribest = rimin

  ELSE

    thref1 = thref

  ENDIF

!-------------------------------------------------------------------------------
! Define ZETA1 and ZETAP.
!-------------------------------------------------------------------------------

  IF ( ( ribest <= ricr ) .AND. ( ribest >= MIN(ribone,ribtwo) ) ) THEN

    zetap = fzss(ribest,betam,betah)
    zeta1 = alnzzo * zetap / ( 1.0 - zinv )

  ELSE IF ( ( ribest < ricut ) .AND. ( ribest >= 0.005 ) ) THEN

    zetap = fzs(ribest,betam,betah)
    zeta1 = alnzzo * zetap / ( 1.0 - zinv )

  ELSE IF ( ribest < -small ) THEN

    sb  = RIBEST / PRO
    qb  = fqi(gamam, gamah, sb)
    pb  = fpi(gamam, gamah, sb)
    det = qb**3 - pb * pb
         
    IF ( det >= 0.0 ) THEN  ! test determinant
      thtb  = fthti(pb, qb)
      zetap = fzu(qb, thtb, gamam)
    ELSE
      arg2  = ( SQRT(-det) + ABS(pb) )**(1.0/3.0)
      zetap = -( arg2 + qb/arg2 ) + 1.0 / (3.0 * gamam)
    ENDIF

    zeta1 = alnzzo * zetap / (1.0 - zinv)
    zeta1 = MAX(zetamin,zeta1)  ! limitation of current theory

  ELSE

    zetap = SIGN(1.0,ribest) / big           ! neutral case
    zeta1 = alnzzo * zetap / ( 1.0 - zinv )  ! neutral case

  ENDIF

!-------------------------------------------------------------------------------
! Define USTAR, THSTAR, and QVSTAR.
!-------------------------------------------------------------------------------

  IF ( ribest >= ricut ) THEN

    ustar  = vkar * speed / ( betam * alnzzo + zetap )
    thstar = vkar * (thm - thref1) / ( pro * ( betah * alnzzo ) + zetap )
    qvstar = vkar * dqv            / ( pro * ( betah * alnzzo ) + zetap )

  ELSE

    zetao  = zeta1 * zo / zmean1
    xo     = fx( zetao )
    yo     = fy( zetao )
    x1     = fx( zeta1 )
    y1     = fy( zeta1 )

    ustar  = vkar * speed / ( alnzzo - fpsim(zeta1,x1) + fpsim(zetao,xo) )
    thstar = vkar * (thm - thref1) /  &
             (  pro * ( alnzzo - fpsih(zeta1,y1) + fpsih(zetao,yo) )  )
    qvstar = vkar * dqv  /  &
             (  pro * ( alnzzo - fpsih(zeta1,y1) + fpsih(zetao,yo) )  )

  ENDIF

!-------------------------------------------------------------------------------
! Everything above is based on the surface layer similarity.
!-------------------------------------------------------------------------------

  IF ( ABS(thstar) > small*0.01 ) THEN
    amol = thref * ustar * ustar / ( vkar * grav * thstar )
  ELSE
    amol = SIGN( 1.0, (thm-thref1) ) * big
  ENDIF

END SUBROUTINE slflux
