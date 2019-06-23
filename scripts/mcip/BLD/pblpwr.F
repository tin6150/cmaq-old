
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
! $Header: /project/work/rep/MCIP2/src/mcip2/pblpwr.F,v 1.1.1.1 2002/03/09 14:48:03 yoj Exp $ 


SUBROUTINE pblpwr (ul, tl, ql, pl, zl, zf, metlay, hgt, pbl, amol, zruf,  &
                   tsfc, psfc, uhgt, thgt, qhgt, phgt, rhgt, jflag)

!-------------------------------------------------------------------------------
! Name:     PBL Power
! Purpose:  Interpolates U, T, Q, P, and RH at a given height.
! Notes:    Uses PBL similarity proposed by Byun (1991) to get around
!           the limit of using surface layer similarity.
! Revised:  24 Feb 1991  Original version.  (D. Byun)
!           10 Sep 2001  Converted to free-form f90.  Changed input argument
!                        from NZCELL to METLAYS (should be same values).
!                        Removed dependence on module VGRD.  Changed vertical
!                        arrays dependent on NZCELL and NLAYS to allocatable.
!                        Removed need for NLAYS.  (T. Otte)
!           27 Feb 2002  Changed this routine to use constants that are common
!                        to the rest of the code by using modules.  In some
!                        cases, definitions of constants were either made more
!                        precise or changed outright to be consistent with
!                        other PBL rediagnosis routines (e.g., surface layer
!                        similarity constants now follow Hogstrom rather than
!                        Businger et al.).  (T. Otte)
!-------------------------------------------------------------------------------

  USE const
  USE const_mete
  USE const_pbl

  IMPLICIT NONE

  REAL                         :: akapa
  REAL                         :: alogns
  REAL,          INTENT(IN)    :: amol                       ! [m]
  REAL                         :: amu
  REAL                         :: ans
  REAL                         :: at
  REAL                         :: au
  REAL                         :: cora
  REAL                         :: corb
  REAL,          EXTERNAL      :: cvmgp
  REAL,          PARAMETER     :: deltah     = 0.01
  REAL,          PARAMETER     :: deltam     = 0.01
  REAL,          ALLOCATABLE   :: dql        ( : )
  REAL,          ALLOCATABLE   :: dthtvl     ( : )
  REAL                         :: dtpbln
  REAL,          ALLOCATABLE   :: dzf        ( : )
  REAL                         :: dzsum
  REAL                         :: ehgt
  REAL                         :: es
  REAL                         :: eshgt
  REAL                         :: eta
  REAL                         :: etao
  REAL                         :: etas       = 0.1
  REAL                         :: fat
  REAL                         :: fau
  REAL                         :: fgamu
  REAL                         :: fhh
  REAL                         :: fhh1
  REAL                         :: fhm
  REAL                         :: fhm1
  REAL                         :: fphih
  REAL                         :: fphim
  REAL                         :: fps
  REAL                         :: fpsih
  REAL                         :: fpsim
  REAL                         :: frs
  REAL                         :: fvptmp
  REAL                         :: fx
  REAL                         :: fy
  REAL                         :: g1p
  REAL                         :: g1r
  REAL                         :: gp
  REAL                         :: gr
  REAL,          INTENT(IN)    :: hgt
  INTEGER                      :: it
  INTEGER,       PARAMETER     :: itmax      = 20
  INTEGER                      :: itp
  INTEGER                      :: itr
  INTEGER,       INTENT(OUT)   :: jflag
  INTEGER                      :: k
  INTEGER                      :: khgt
  INTEGER                      :: kpbl
  INTEGER                      :: lbnd
  INTEGER,       INTENT(IN)    :: metlay
  REAL                         :: p
  REAL                         :: p1
  REAL,          INTENT(IN)    :: pbl                 ! [m]
  REAL,          INTENT(OUT)   :: phgt
  REAL,          INTENT(IN)    :: pl         ( : )    ! [mb]
  REAL                         :: pnew
  REAL                         :: ps
  REAL,          INTENT(IN)    :: psfc
  REAL                         :: q
  REAL                         :: qdzsum
  REAL,          INTENT(OUT)   :: qhgt
  REAL,          INTENT(IN)    :: ql         ( : )    ! [g g^-1]
  REAL,          PARAMETER     :: qmin       = 0.001
  REAL                         :: qpbl
  REAL                         :: qsfc
  REAL                         :: qshgt
  REAL                         :: qstar
  REAL                         :: qsthat
  REAL                         :: r
  REAL                         :: r1
  REAL,          INTENT(OUT)   :: rhgt
  REAL                         :: rnew
  REAL                         :: rs
  REAL,          PARAMETER     :: small      = 0.000001
  REAL                         :: t
  REAL                         :: tes
  REAL                         :: thdzsum
  REAL,          INTENT(OUT)   :: thgt
  REAL                         :: thstar
  REAL                         :: thsthat
  REAL,          ALLOCATABLE   :: thtvl      ( : )
  REAL                         :: thvhgt
  REAL                         :: thvpbl
  REAL,          INTENT(IN)    :: tl         ( : )    ! [K]
  REAL,          INTENT(IN)    :: tsfc                ! [K]
  REAL,          INTENT(OUT)   :: uhgt
  REAL,          INTENT(IN)    :: ul         ( : )    ! [m s^-1]
  REAL,          PARAMETER     :: umin       = 0.1
  REAL                         :: upbl
  REAL                         :: upbln
  REAL                         :: usdzsum
  REAL                         :: ustar
  REAL                         :: usthat
  REAL,          PARAMETER     :: ustmin     = 0.05
  REAL                         :: vptsfc
  REAL                         :: x
  REAL                         :: xa
  REAL                         :: xo
  REAL                         :: xs
  REAL                         :: y
  REAL                         :: ya
  REAL                         :: yo
  REAL                         :: ys
  REAL                         :: zeta
  REAL,          INTENT(IN)    :: zf         ( : )    ! [m]
  REAL,          INTENT(IN)    :: zl         ( : )    ! [m]
  REAL,          INTENT(IN)    :: zruf                ! [m]

!-------------------------------------------------------------------------------
! Statement functions.
!-------------------------------------------------------------------------------

  ! Virtual potential temperature

  fvptmp(t,p,q)= t * (1.0 + 0.61 * q) * (1000.0 / p)**rovcp


  ! Functions for stable and unstable cases

  fx(zeta) = cvmgp(0.0, ABS(1.0-gamam*zeta)**0.25, zeta)
  fy(zeta) = cvmgp(0.0, ABS(1.0-gamah*zeta)**0.5,  zeta)

  fphim(zeta) = cvmgp(1.0+betam*zeta, ABS(1.0-gamam*zeta)**(-0.25), zeta)
  fphih(zeta) = cvmgp(pro*(1.0+betah*zeta), ABS(1.0-gamah*zeta)**(-0.5), zeta)

  fpsim(zeta,x) = cvmgp(-betam*zeta, &
                  2.0*ALOG(x+1.0)+ALOG(1.0+x*x)-2.*ATAN(x), zeta)
  fpsih(zeta,y) = cvmgp(-betah*zeta, 2.0*ALOG(y+1.0), zeta)

  fgamu(zeta,x) = cvmgp(-0.5*betam*zeta*zeta, -( 4.0*X**3/3.0-X**4 +           &
                  (x**4-1.)*(ALOG(1.+x*x)+2.*ALOG(1.+x)-2.*ATAN(x)) )/gamam,   &
                  zeta)


  ! Functions for surface power

  fps(amu,etas,etao,xs,xo) = fphim(etas*amu) / (ALOG(etas/etao) -  &
                             fpsim(etas*amu,xs) + fpsim(etao*amu,xo))
  frs(amu,etas,etao,ys,yo) = fphih(etas*amu) / (pro*(ALOG(etas/etao) -  &
                             fpsih(etas*amu,ys) + fpsih(etao*amu,yo)))


  ! Functions for coefficients for PBL profiles

  fau(amu,etao,p,xa,xo) = -((p+1.0) / (p*(1.0-etao)**p)) *                  &
                          ( etao*ALOG(etao)/(1.-etao) + 1. - fpsim(amu,xa)  &
                          + (fgamu(amu,xa) - fgamu(amu*etao,xo)) /          &
                          (amu*(1.0-etao)) )
  fat(amu,etao,akapa,r) = (1.0 / (r*(1.0-etao)**(r-1)))*                    &
                          ( vkar * akapa / pro - fphih(amu) / pro - 1.0 )


  ! Functions for the estimation of powers.

  fhm(amu,etas,etao,xa,xs,xo) =                                             &
        ABS( ALOG(etas/etao) - fpsim(amu*etas,xs) + fpsim(amu*etao,xo))     &
        / ABS( etao*ALOG(etao)/(1.0-etao) + 1.0 - fpsim(amu,xa)             &
        + (fgamu(amu,xa) - fgamu(amu*etao,xo)) / (amu*(1.0-etao)) )
  fhh(amu,etas,etao,ys,yo,akapa) =                                          &
        ABS( ALOG(etas/etao) - fpsih(amu*etas,ys) + fpsih(amu*etao,yo))     &
        / (ABS( vkar*akapa/pro-fphih(amu)/pro-1.0 ) * (1.0-etao))


  ! Functions for normalized PBL profiles.

  upbln(amu,eta,etao,p,au,x,xo)  = fpsim(amu*etao,xo) + ALOG(eta/etao) -  &
                                   fpsim(amu*eta,x)   + au*(eta-etao)**p
  dtpbln(amu,eta,etao,r,at,y,yo) = fpsih(amu*etao,yo) + ALOG(eta/etao) -  &
                                   fpsih(amu*eta,y)   + at*(eta-etao)**r

!-------------------------------------------------------------------------------
! Allocate necessary arrays.
!-------------------------------------------------------------------------------

  ALLOCATE ( dql    ( metlay ) )
  ALLOCATE ( dthtvl ( metlay ) )
  ALLOCATE ( dzf    ( metlay ) )
  ALLOCATE ( thtvl  ( metlay ) )

!-------------------------------------------------------------------------------
! Compute surface virtual potential temperature, specified humidity.
!-------------------------------------------------------------------------------

  tes    = MIN(tsfc, tl(1))
  es     = 6.1078 * EXP( 5384.21*(1.0/stdtemp - 1.0/tes) )

  qsfc   = 0.622 * es / (psfc-es)
  vptsfc = fvptmp(tsfc,psfc,qsfc)

!-------------------------------------------------------------------------------
! Compute virtual potential temperature (THTVL) for whole layers.
!-------------------------------------------------------------------------------

  DO k = 1, metlay
    thtvl(k)  = fvptmp(tl(k),pl(k),ql(k))
    dthtvl(k) = thtvl(k) - vptsfc
    dql(k)    = ql(k) - qsfc
  ENDDO

!-------------------------------------------------------------------------------
! Determine layer index for PBL.
!-------------------------------------------------------------------------------

  pbllyr: DO k = 1, metlay
    IF ( pbl < zl(k) ) THEN
      kpbl = k
      EXIT pbllyr
    ENDIF
    IF ( k == metlay ) kpbl = metlay
  ENDDO pbllyr

!-------------------------------------------------------------------------------
! Determine layer index for HGT.
!-------------------------------------------------------------------------------

  hgtlyr: DO k = 1, metlay
    IF ( hgt < zl(k) ) THEN
      khgt = k
      EXIT hgtlyr
    ENDIF
    IF ( k == metlay ) khgt = metlay
  ENDDO hgtlyr

!-------------------------------------------------------------------------------
! Compute DZD: delta_Z between 2 levels.
!-------------------------------------------------------------------------------

  lbnd = LBOUND(zf,1)

  DO k = 1, metlay
    dzf(k) = zf(lbnd+k) - zf(lbnd+k-1)
  ENDDO

!-------------------------------------------------------------------------------
! Find powers P and R. (P, R independent of AMU.)
!-------------------------------------------------------------------------------

  akapa  = 0.0        ! Assume stratification aloft is not important
  etas   = 0.1
  etao   = zruf / pbl
  amu    = pbl / amol
  xo     = fx(etao*amu)
  xs     = fx(etas*amu)
  xa     = fx(1.0 *amu)
  yo     = fy(etao*amu)
  ys     = fy(etas*amu)
  ya     = fy(1.0 *amu)
  ans    = (etas-etao) / (1.0-etao)
  alogns = ALOG(ans)

  IF ( ABS(AMOL) > 1000.0 ) THEN  ! Treat as neutral

    p1 = 1.43
    r1 = p1

  ELSE  ! Diabatic condition

    ps = fps(amu,etas,etao,xs,xo)
    rs = frs(amu,etas,etao,ys,yo)

    ! Estimate P and R using the asymptotic magnitude comparisons
    ! Use iteration for P starting with initial P = PS

    p1   = ps
    fhm1 = fhm(amu,etas,etao,xa,xs,xo)

    newtrap1: DO it = 1, itmax  ! NEWTON-RAPSON method

      gp   = ALOG((p1+1.0)/p1) + p1*alogns - ALOG(deltam*fhm1)  
      g1p  = alogns - 1.0 / (p1*(1.0+p1))
      pnew = p1 - gp/g1p
      IF ( ABS((pnew-p1)/pnew) < small ) THEN
        p1  = pnew
        itp = it
        EXIT newtrap1
      ELSE
        p1 = pnew
      ENDIF
      IF ( it == itmax ) THEN  ! Not converged within ITMAX
        jflag = -1
        WRITE (6,1100) jflag
        RETURN
      ENDIF

    ENDDO newtrap1

    ! Use iteration for R starting with initial R = RS.

    r1   = rs
    fhh1 = fhh(amu,etas,etao,ys,yo,akapa)

    newtrap2: DO it = 1, itmax  ! NEWTON-RAPSON METHOD

      gr   = -ALOG(r1) + r1*alogns - ALOG(deltah*fhh1)
      g1r  = -1.0/r1 + alogns
      rnew = r1 - gr/g1r
      IF( ABS((rnew-r1)/rnew) < small ) THEN
        r1  = rnew
        itr = it
        EXIT newtrap2
      ELSE
        r1 = rnew
      ENDIF
      IF ( it == itmax ) THEN  ! Not converged within ITMAX
        jflag = -2
        WRITE (6,1200) jflag
        RETURN
      ENDIF

    ENDDO newtrap2

  ENDIF

  p = p1
  r = r1

  ! Diabatic P, R also determined.

  au = fau(amu,etao,p,xa,xo)
  at = fat(amu,etao,akapa,r)

!-------------------------------------------------------------------------------
! Compute USTHAT, THSTHAT.
!-------------------------------------------------------------------------------

  IF ( kpbl <= 1 ) THEN

    k       = 1
    eta     = zl(k) / pbl
    x       = fx(eta*amu)
    y       = fy(eta*amu)
    ustar   = vkar * ul(k)     / upbln(amu,eta,etao,p,au,x,xo)
    thstar  = vkar * dthtvl(k) / dtpbln(amu,eta,etao,r,at,y,yo)
    qstar   = vkar * dql(k)    / dtpbln(amu,eta,etao,r,at,y,yo)

    usthat  = AMAX1(ustar, ustmin)
    thsthat = thstar 
    qsthat  = qstar

  ELSE

    usdzsum = 0.0
    thdzsum = 0.0
    qdzsum  = 0.0
    dzsum   = 0.0

    DO k = 1, kpbl-1

      eta    = zl(k) / pbl
      x      = fx(eta*amu)
      y      = fy(eta*amu)
      ustar  = vkar * ul(k)     / upbln(amu,eta,etao,p,au,x,xo)
      thstar = vkar * dthtvl(k) / dtpbln(amu,eta,etao,r,at,y,yo)
      qstar  = vkar * dql(k)    / dtpbln(amu,eta,etao,r,at,y,yo)

      usdzsum = usdzsum + dzf(k)*ustar
      thdzsum = thdzsum + dzf(k)*thstar
      qdzsum  = qdzsum  + dzf(k)*qstar
      dzsum   = dzsum   + dzf(k)

    ENDDO

    usthat  = AMAX1((usdzsum/dzsum),ustmin)
    thsthat = thdzsum / dzsum
    qsthat  = qdzsum  / dzsum

  ENDIF

!-------------------------------------------------------------------------------
!  Now we have good estimation of USTHAT, THSTHAT, QSTHAT
!  depending on the relative position of PBL and HGT, estimate
!  Met. parameters using the profile function and linear interpolation
!-------------------------------------------------------------------------------

  IF ( hgt <= pbl ) THEN

    ! HGT is in PBL layer: use parameterized PBL profile.

    eta    = hgt / pbl
    x      = fx(eta*amu)
    y      = fy(eta*amu)
    uhgt   = usthat * upbln(amu,eta,etao,p,au,x,xo) / vkar
    thvhgt = vptsfc + thsthat * dtpbln(amu,eta,etao,r,at,y,yo) / vkar
    qhgt   = qsfc   + qsthat  * dtpbln(amu,eta,etao,r,at,y,yo) / vkar

    uhgt   = AMAX1( uhgt, umin )
    qhgt   = AMAX1( qhgt, qmin )

  ELSE IF ( ( pbl < hgt ) .AND. ( hgt < zl(kpbl) ) ) THEN

    ! HGT is above the boundary layer but below ZL(KPBL):
    ! use parameterized profile till ZSL and interpolate with
    ! values at PBL and at ZL(KPBL)

    ! Compute ETA, X, and Y at PBL.

    eta    = 1.0
    x      = fx(eta*amu)
    y      = fy(eta*amu)
    upbl   = usthat * upbln(amu,eta,etao,p,au,x,xo) / vkar
    thvpbl = vptsfc + thsthat * dtpbln(amu,eta,etao,r,at,y,yo) / vkar
    qpbl   = qsfc   + qsthat  * dtpbln(amu,eta,etao,r,at,y,yo) / vkar

    ! Interpolate.

    cora   = (zl(kpbl) - hgt) / (zl(kpbl) - pbl) 
    corb   = 1.0 - cora

    uhgt   = AMAX1( (ul(kpbl)*corb + upbl*cora), umin )
    thvhgt = (thtvl(kpbl) * corb) + (thvpbl * cora)
    qhgt   = AMAX1( (ql(kpbl)*corb + qpbl*cora), qmin )

  ELSE

    ! HGT is located above the mixed layer:
    ! use simple linear interpolation using adjacent levels

    cora   = (zl(khgt) - hgt) / (zl(khgt) - zl(khgt-1)) 
    corb   = 1.0 - cora

    uhgt   = AMAX1( (ul(khgt)*corb + ul(khgt-1)*cora), umin )
    thvhgt = (thtvl(khgt) * corb) + (thtvl(khgt-1) * cora)
    qhgt   = AMAX1( (ql(khgt)*corb + ql(khgt-1)*cora), qmin )

  ENDIF

!-------------------------------------------------------------------------------
! Convert virtual potential temperature to normal temperature.
!-------------------------------------------------------------------------------

  phgt = pl(khgt) * EXP( (zl(khgt) - hgt) / (rog * thtvl(khgt)) )
  thgt = (thvhgt / (1.0 + 0.61 * qhgt)) * (phgt / 1000.0)**rovcp

!-------------------------------------------------------------------------------
! Compute relative humidity.
!-------------------------------------------------------------------------------

  eshgt = 6.1078 * EXP(5384.21 * (1.0/stdtemp - 1.0/thgt))
  qshgt = 0.622 * eshgt / (phgt - eshgt)
  ehgt  = phgt * qhgt / (qhgt + 0.622)
  rhgt  = ehgt / eshgt

!-------------------------------------------------------------------------------
! Deallocate arrays.
!-------------------------------------------------------------------------------

  DEALLOCATE ( dql    )
  DEALLOCATE ( dthtvl )
  DEALLOCATE ( dzf    )
  DEALLOCATE ( thtvl  )

!-------------------------------------------------------------------------------
! Format statements.
!-------------------------------------------------------------------------------

1100 FORMAT (' CONVERGENCE FAILED FOR POWER P IN PBLPWR', /)
1200 FORMAT (' CONVERGENCE FAILED FOR POWER R IN PBLPWR', /  &
             ' ERROR RETURN FLAG= ', i4)

END SUBROUTINE pblpwr
