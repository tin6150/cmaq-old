
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
! $Header: /project/work/rep/MCIP2/src/mcip2/sfcflux.F,v 1.2 2005/08/29 14:37:38 tlotte Exp $ 


SUBROUTINE sfcflux (alatdeg, wm, thm, qvm, thref, qvref, zf1, zmean1, zO,  &
                    hpbl, hfxmax, pp, rr, ustar, thstar, qvstar, amol)

!-------------------------------------------------------------------------------
! Name:     Surface Flux
! Purpose:  Computes surface fluxes from MM5 first layer mean wind and
!           temperature using the power-lay similarity of profile functions.
! Notes:    - Refer to Byun (1991):  "Determination of Similarity Functions of
!             the Resistance Laws for the Planetary Boundary Layer Using
!             Surface Layer Similarity Functions."
!           - Here we neglect turning of wind to simplify our calculation.
!           - Minimum boundary layer height = top of the first layer
! Revised:  12 Apr 1991  Original version.  (D. Byun)
!           04 Feb 1998  Changed include method nonglobal includes.  (D. Byun)
!           13 Sep 2001  Converted to free-form f90.  Removed argument PBLMIN
!                        since it's in CONST_PBL.  (T. Otte)
!           09 Jan 2002  Changed "stop" statements to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  (T. Otte)
!           27 Feb 2002  Replaced GOTO/CONTINUE section with DO loop.  (T. Otte)
!           19 Jul 2005  Corrected FCORI for Southern Hemisphere.  Added error-
!                        checking to prevent using this code too close to
!                        equator.  (T. Otte)
!           12 Aug 2005  Removed unused statement functions DTPBLN, DTSFCN,
!                        FAV, UPBLN, USFCN, and VPBLN.  (T. Otte)
!-------------------------------------------------------------------------------

  USE const_mete
  USE const_pbl

  IMPLICIT NONE

  REAL                         :: a1         = -0.06666667
  REAL                         :: akapa
  REAL                         :: alamda
  REAL                         :: alat
  REAL,          INTENT(IN)    :: alatdeg                  ! [deg]
  REAL                         :: alnetao
  REAL                         :: alnzzo
  REAL                         :: alogns
  REAL,          INTENT(OUT)   :: amol
  REAL                         :: amol1
  REAL                         :: amolnew
  REAL                         :: amu
  REAL                         :: ans
  REAL                         :: arg2
  REAL                         :: argch
  REAL                         :: at
  REAL                         :: atneut
  REAL                         :: au
  REAL                         :: auneut
  REAL                         :: av
  REAL                         :: bh
  REAL,          PARAMETER     :: big        = 1.0e4
  REAL                         :: bm
  REAL,          EXTERNAL      :: cvmgp
  REAL                         :: deltah
  REAL                         :: deltam
  REAL                         :: det
  REAL                         :: dqv
  REAL                         :: err0
  REAL                         :: err1
  REAL                         :: err2
  REAL                         :: eta
  REAL                         :: etaf
  REAL                         :: etao
  REAL                         :: etas
  REAL                         :: eterm1
  REAL                         :: fat
  REAL                         :: fau
  REAL                         :: fcori
  REAL                         :: fgamh
  REAL                         :: fgamu
  REAL                         :: fhh
  REAL                         :: fhh1
  REAL                         :: fhm
  REAL                         :: fhm1
  REAL                         :: fphih
  REAL                         :: fphim
  REAL                         :: fpi
  REAL                         :: fps
  REAL                         :: fpsih
  REAL                         :: fpsim
  REAL                         :: fqi
  REAL                         :: frs
  REAL                         :: fthti
  REAL                         :: fx
  REAL                         :: fy
  REAL                         :: fzs
  REAL                         :: fzu
  REAL                         :: g1p
  REAL                         :: g1r
  REAL                         :: gh
  REAL                         :: gm
  REAL                         :: gp
  REAL                         :: gr
  REAL,          INTENT(IN)    :: hfxmax                   ! [K m s^-1]
  REAL,          INTENT(INOUT) :: hpbl                     ! [m]
  REAL                         :: hratio
  INTEGER                      :: it
  INTEGER                      :: iter
  INTEGER,       PARAMETER     :: itmax      = 20
  INTEGER                      :: itp
  INTEGER                      :: itr
  REAL                         :: p
  REAL                         :: p1
  REAL                         :: pb
  REAL                         :: pii
  REAL,          PARAMETER     :: pmax       = 3.0
  REAL,          PARAMETER     :: pmin       = 1.0
  CHARACTER*16,  PARAMETER     :: pname      = 'SFCFLUX'
  REAL                         :: pneut
  REAL                         :: pnew
  REAL,          INTENT(OUT)   :: pp
  REAL                         :: ps
  REAL                         :: q
  REAL                         :: qb
  REAL                         :: qi
  REAL                         :: qneut
  REAL,          INTENT(IN)    :: qvm                      ! [kg kg^-1]
  REAL,          INTENT(IN)    :: qvref                    ! [kg kg^-1]
  REAL,          INTENT(OUT)   :: qvstar                   ! [kg kg^-1]
  REAL                         :: r
  REAL                         :: r1
  REAL                         :: ri
  REAL                         :: ribest
  REAL,          PARAMETER     :: rmax       = 2.5
  REAL,          PARAMETER     :: rmin       = 1.0
  REAL                         :: rneut
  REAL                         :: rnew
  REAL,          INTENT(OUT)   :: rr
  REAL                         :: rs
  REAL                         :: sb
  REAL                         :: si
  REAL,          PARAMETER     :: small      = 1.0e-4
  REAL                         :: speed
  REAL                         :: term1
  REAL                         :: term2
  REAL                         :: term3
  REAL                         :: term4
  REAL                         :: term5
  REAL                         :: term6
  REAL                         :: termo
  REAL,          INTENT(IN)    :: thm                      ! [K]
  REAL,          INTENT(INOUT) :: thref                    ! [K]
  REAL                         :: thref1
  REAL                         :: thsnew
  REAL,          INTENT(OUT)   :: thstar                   ! [K]
  REAL                         :: thstar1
  REAL                         :: thtb
  REAL                         :: thti
  REAL,          PARAMETER     :: tol        = 1.0e-3
  REAL                         :: usnew
  REAL,          INTENT(OUT)   :: ustar                    ! [m s^-1]
  REAL                         :: ustar1
  REAL,          INTENT(IN)    :: wm                       ! [m s^-1]
  REAL                         :: x
  REAL                         :: x1
  REAL                         :: xa
  REAL                         :: xf
  REAL                         :: xo
  REAL                         :: xs
  REAL                         :: y
  REAL                         :: y1
  REAL                         :: yf
  REAL                         :: yo
  REAL                         :: ys
  REAL                         :: zeta
  REAL                         :: zeta1
  REAL                         :: zetao
  REAL,          INTENT(IN)    :: zf1                      ! [m]
  REAL                         :: zinv
  REAL,          INTENT(IN)    :: zmean1                   ! [m]
  REAL,          INTENT(IN)    :: zo                       ! [m]
  REAL                         :: zzo

!-------------------------------------------------------------------------------
! Statement functions.
!-------------------------------------------------------------------------------

  ! Functions for stable and unstable cases.

  fx(zeta) = cvmgp(0.0, ABS(1.0-gamam*zeta)**0.25, zeta)
  fy(zeta) = cvmgp(0.0, ABS(1.0-gamah*zeta)**0.5,  zeta)

  fphim(zeta) = cvmgp(1.0+betam*zeta, ABS(1.0-gamam*zeta)**(-0.25), zeta)
  fphih(zeta) = cvmgp(pro*(1.0+betah*zeta), ABS(1.0-gamah*zeta)**(-0.5), zeta)

  fpsim(zeta,x) = cvmgp(-betam*zeta, &
                  2.0*ALOG(x+1.0)+ALOG(1.0+x*x)-2.*ATAN(x), zeta)
  fpsih(zeta,y) = cvmgp(-betah*zeta, 2.0*ALOG(y+1.0), zeta)

  fgamu(zeta,x) = cvmgp(-0.5*betam*zeta*zeta, -( 4.0*X**3/3.0-X**4 +          &
                  (x**4-1.)*(ALOG(1.+x*x)+2.*ALOG(1.+x)-2.*ATAN(x)) )/gamam,  &
                  zeta)
  fgamh(zeta,y) = cvmgp(-0.5*betah*zeta*zeta, &
                        -( 2.0*(y*y-1.0)*ALOG(1.0+y)+2.0*y-y*y )/gamah, zeta)


  ! Functions for surface power

  fps(amu,etas,etao,xs,xo) = fphim(etas*amu) / (ALOG(etas/etao) -  &
                             fpsim(etas*amu,xs) + fpsim(etao*amu,xo))
  frs(amu,etas,etao,ys,yo) = fphih(etas*amu) / (pro*(ALOG(etas/etao) -  &
                             fpsih(etas*amu,ys) + fpsih(etao*amu,yo)))


  ! Functions for coefficients for PBL profiles

  fau(amu,etao,p,xa,xo)  = -((p+1.0) / (p*(1.0-etao)**p)) *                  &
                           ( etao*ALOG(etao)/(1.-etao) + 1. - fpsim(amu,xa)  &
                           + (fgamu(amu,xa) - fgamu(amu*etao,xo)) /          &
                           (amu*(1.0-etao)) )
  fat(amu,etao,akapa,r)  = (1.0 / (r*(1.0-etao)**(r-1)))*                    &
                           ( vkar * akapa / pro - fphih(amu) / pro - 1.0 )


  ! Functions for the estimation of powers.

  fhm(amu,etas,etao,xa,xs,xo) =                                             &
        ABS( ALOG(etas/etao) - fpsim(amu*etas,xs) + fpsim(amu*etao,xo))     &
        / ABS( etao*ALOG(etao)/(1.0-etao) + 1.0 - fpsim(amu,xa)             &
        + (fgamu(amu,xa) - fgamu(amu*etao,xo)) / (amu*(1.0-etao)) )
  fhh(amu,etas,etao,ys,yo,akapa) =                                          &
        ABS( ALOG(etas/etao) - fpsih(amu*etas,ys) + fpsih(amu*etao,yo))     &
        / (ABS( vkar*akapa/pro-fphih(amu)/pro-1.0 ) * (1.0-etao))


  ! Surface layer analytical solution for Ri:  stable case

  fzs(ri,bm,bh) = (-2.0*bh*ri+1.0-SQRT(1.0+4.0*(bh-bm)*ri/pro)) /  &
                  ( 2.0*bh*(bm*ri-1.0))

  ! Surface layer analytical solution for Ri:  unstable case

  fzu(qi,thti,a1) = -2.0*SQRT(qi)*COS(thti/3.0)-a1/3.0
  fqi(gm,gh,si)   = (1.0/gm**2+3.*gh*si*si/gm)/9.0
  fpi(gm,gh,si)   = ( -2./gm**3+9.*(-gh/gm+3.)*si*si/gm )/54.
  fthti(pii,qi)   = ACOS(pii/qi**1.5)

!-------------------------------------------------------------------------------
! Limit moisture to move one direction (surface to air).
!-------------------------------------------------------------------------------

  dqv = cvmgp((qvm-qvref), 0.0, -(qvm-qvref))

!-------------------------------------------------------------------------------
! Compute surface parameters.
!-------------------------------------------------------------------------------

  hpbl   = MAX(hpbl, zf1, zo/similim)

  IF ( ABS(alatdeg) <= 1.0 ) THEN  ! +/- 1.0 deg latitude of equator; arbitrary
    WRITE (6,9000)
    GOTO 1001
  ENDIF

  alat   = deg2rad * alatdeg
  fcori  = ABS(2.0 * eomega * SIN(alat))  ! ABS for Southern Hemisphere
  akapa  = 0.0
  speed  = MAX(1.0, wm)
  zzo    = MAX(zmean1/zo, 10.0)   ! Limit Z/ZO 10.0
  zinv   = 1.0 / zzo
  alnzzo = ALOG(zzo)

!-------------------------------------------------------------------------------
! Estimate fluxes using surface layer similarity -- analytical solution.
! - Estimate bulk Richardson number, and initially set M-O length to a big num.
!-------------------------------------------------------------------------------

  ribest  = grav * (zmean1-zo) * (thm-thref) / (thref*speed*speed)
  amolnew = big

!-------------------------------------------------------------------------------
! Limit the temperature difference (THM-THREF) to follow RIMAX allowed.
!-------------------------------------------------------------------------------

  IF ( ribest > rimax ) THEN
    thref1 = grav * (zmean1-zo) * thm / (speed*speed*rimax+grav*(zmean1-zo))
  ELSE IF ( ribest < rimin) THEN  ! RIMIN=-4.75
    thref1 = grav * (zmean1-zo) * thm / (speed*speed*rimin+grav*(zmean1-zo))
  ELSE
    thref1 = thref
  ENDIF

  IF ( ribest > 0.005 ) THEN

    zeta1 = alnzzo*fzs(ribest,betam,betah) / (1.0-zinv)
    zeta1 = AMIN1(2.0, zeta1)       ! limitation of current theory

  ELSE IF ( ribest < -small ) THEN

    sb   = ribest / pro
    qb   = fqi(gamam,gamah,sb)
    pb   = fpi(gamam,gamah,sb)
    det  = qb**3 - pb*pb
    IF ( det >= 0.0 ) THEN          ! test determinant
      thtb  = fthti(pb,qb)
      zeta1 = fzu(qb,thtb,a1)
    ELSE
      arg2  = (SQRT(-det) + ABS(pb))**(1./3.)
      zeta1 = -( arg2 + qb/arg2 ) + 1./(3.*gamam)
    END IF
    zeta1 = alnzzo * zeta1 / (1.0 - zinv)
    zeta1 = MAX(-5.0, zeta1)      ! limitation of current theory

  ELSE

    zeta1 = SIGN(1.0,ribest) / big  ! neutral case

  ENDIF

  zetao   = zeta1 * zo / zmean1
  xo      = fx(zetao)
  yo      = fy(zetao)
  x1      = fx(zeta1)
  y1      = fy(zeta1)
  ustar1  = vkar * speed / (alnzzo-fpsim(zeta1,x1)+fpsim(zetao,xo))
  thstar1 = vkar * (thm-thref1) /  &
            (  pro*( alnzzo-fpsih(zeta1,y1)+fpsih(zetao,yo) )  )

!-------------------------------------------------------------------------------
! Everything above is based on the surface layer similarity.
! Now, try to find fluxes using PBL similarity theory.
!-------------------------------------------------------------------------------

  thref  = thref1
  ustar  = ustar1
  thstar = thstar1
  hpbl   = cvmgp(hpbl,MAX(alamdao*ustar/fcori,hpbl),zeta1)

  IF ( (zo/hpbl) > similim ) THEN  ! SIMILIM is ETAO limit for similarity
    hpbl = zo / similim            ! raise HPBL at least ZO*(1/SIMILIM) (.008)
  ENDIF

  IF ( ABS(ribest) < small ) THEN  ! assume neutral condition...PNEUT known

    DO it = 1, 2  ! iterate once to update

      etao   = MIN(zo/hpbl, 0.01)
      etao   = MAX(1.0E-6, etao)
      etas   = MAX(10.0*etao, 0.1)
      etaf   = MIN(1.0, zf1/hpbl)
      etaf   = MAX(1.1*etas, etaf)
      eterm1 = etao * ALOG(etao) / (1.0- etao)
      amol1  = SIGN(1.0,(thm-thref1))*big
      pneut  = (eterm1+1.0) / (aneut - eterm1 - 1.0)
      qneut  = 1.0 / (alamdao * bneut * (1.0-etao) / vkar - 1.0)
      rneut  = pneut
      auneut = -((pneut+1.0)/(pneut*(1.0-etao)**pneut)) *  &
               ( etao*ALOG(etao)/(1.0-etao) + 1.0 )
      atneut = fat(0.0,etao,akapa,rneut)
      termo  = etaf*ALOG(etaf/etao) + etao - etaf
      term3  = auneut * (etaf-etao)**(pneut+1.0) / (pneut+1.0)
      term6  = atneut * (etaf-etao)**(rneut+1.0) / (rneut+1.0)
      usnew  = vkar * speed * (etaf-etao) / (termo+term3)
      thsnew = vkar * (thm-thref) * (etaf-etao) / pro
      thstar = thsnew / (termo+term6)
      ustar  = usnew
      amol   = amol1  ! essentially a big number
      hpbl   = MAX(alamdao*ustar/fcori, hpbl)

      pp = pneut
      rr = rneut

    ENDDO

  ELSE  ! for diabatic cases

    amol1 = thref1 * ustar1 * ustar1 / (vkar*grav*thstar1)
    amol  = SIGN(1.,thstar1) * AMIN1(10000.0, ABS(amol1))
    iter  = 0

    ! Compute powers for surface layer for variable value of PP (depends on AMU)

    etao   = MIN(zo/hpbl, 0.01)
    etao   = MAX(1.0e-6, etao)
    etas   = MAX(10.0*etao, 0.1)
    etaf   = MIN(1.0, zf1/hpbl)
    etaf   = MAX(1.1*etas, etaf)
    eterm1 = etao * ALOG(etao) / (1.0 - etao)
    amu    = hpbl / amol
    pneut  = (eterm1+1.0) / (aneut - eterm1 - 1.0)
    qneut  = 1.0 / (alamdao * bneut * (1.0 - etao) / vkar - 1.0)
    amol1  = SIGN(1.0, (thm-thref1)) * big
    rneut  = pneut
    auneut = -((pneut+1.0) / (pneut * (1.0-etao)**pneut)) *  &
             ( etao*ALOG(etao) / (1.0-etao) + 1.0 )

    deltam = ABS(auneut*(0.1)**pneut / ALOG(0.1/etao))
    deltah = deltam

    xo = fx(etao*amu)
    xs = fx(etas*amu)

    yo = fy(etao*amu)
    ys = fy(etas*amu)

    ps = fps(amu,etas,etao,xs,xo)
    rs = frs(amu,etas,etao,ys,yo)
    pp = ps
    rr = rs

    diabatic: DO

    iter = iter + 1

    IF ( iter > itmax ) THEN
      WRITE(*,*) ' TOO MUCH ITERATION REQUIRED: '
      WRITE(*,*) ' ESTIMATION OF PBL FLUXES FAILED '
      GOTO 999
    ENDIF

    ! For stable case...

    IF ( amol > 0.0 ) THEN  ! Estimate HPBL using Zilitinkevich's interpolation.

      argch = vkar * ch * ch * amol / alamdao
      hpbl  = 0.5*(-argch+SQRT((argch*argch)+(4.*vkar*ustar*ch*ch*amol)/fcori))

      ! Similarity theory requires (HPBL > 100*ZO) at least (SIMILIM=.008)

      hpbl = MAX(hpbl,zo/similim)

      ! If PBL method fails, use surface layer similarity estimated values.

      IF ( ( hpbl < zf1 ) .AND. ( iter >= itmax ) ) THEN
        thref  = thref1
        amol   = amol1
        ustar  = ustar1
        thstar = thstar1
        hpbl   = MAX(hpbl,zf1,pblmin)
        EXIT diabatic
      ELSE
        hpbl   = MAX(hpbl,zf1,pblmin)
      ENDIF

    END IF

    amu    = hpbl / amol
    alamda = fcori * hpbl / ustar
    zetao  = zo  / amol
    etao   = MIN(zo/hpbl, 0.01)
    etao   = MAX(1.0e-6, etao)
    etas   = MAX(10.0*etao, 0.1)
    etaf   = MIN(1.0, zf1/hpbl)
    etaf   = MAX(1.1*etas, etaf)

    xo = fx(etao*amu)
    xa = fx(1.0* amu)
    xs = fx(etas*amu)
    xf = fx(etaf*amu)

    yo = fy(etao*amu)
    ys = fy(etas*amu)
    yf = fy(etaf*amu)

    ! Find power PP and RR.

    ans    = (etas - etao) / (1.0 - etao)
    alogns = ALOG(ans)

    ! Estimate PP and RR using the asymptotic magnitude comparisons.

    p1   = pp
    fhm1 = fhm(amu,etas,etao,xa,xs,xo)

    newtrap1: DO it = 1, itmax  ! NEWTON-RAPSON METHOD

      gp   = ALOG((p1+1.0)/p1) + p1*alogns - ALOG(deltam*fhm1)
      g1p  = alogns - 1.0 / (p1 * (1.0+p1))
      pnew = p1 - gp/g1p

      ! Sometimes, the profiles provided by the MM5 do not make sense
      ! compared to the idealized horizontally homogeneous steady state PBL.
      ! --- TO PREVENT ABNORMAL POWERS, LIMIT FLOOR AND CEILING OF THE POWER

      pnew = MIN(pmax,pnew)
      pnew = MAX(pmin,pnew)
      IF ( ABS((pnew-p1)/pnew) < tol ) THEN
        p1  = pnew
        itp = it
        EXIT newtrap1
      ELSE
        p1 = pnew
      ENDIF

      IF ( it == itmax ) THEN  ! not converged within ITMAX
        WRITE (6,9100) 'PP', alamda, akapa, amu
        GOTO 1001
      ENDIF

    ENDDO newtrap1

    ! Use iteration for R starting with initial RR = RS.

    r1   = rr
    fhh1 = fhh(amu,etas,etao,ys,yo,akapa)

    newtrap2: DO it = 1, itmax  ! NEWTON-RAPSON METHOD

      gr   = -ALOG(r1) + r1*alogns - ALOG(deltah*fhh1)
      g1r  = -1.0/r1 + alogns
      rnew = r1 - gr/g1r

      ! TO PREVENT ABNORMAL POWERS, LIMIT FLOOR AND CEILING OF THE POWER

      rnew = MIN(rmax,rnew)
      rnew = MAX(rmin,rnew)
      IF ( ABS((rnew-r1)/rnew) < tol ) THEN
        r1  = rnew
        itr = it
        EXIT newtrap2
      ELSE
        r1 = rnew
      ENDIF

      IF ( it == itmax ) THEN  ! not converged within ITMAX
        WRITE (6,9000) 'R', alamda, akapa, amu
        GOTO 1001
      ENDIF

    ENDDO newtrap2

    pp = MIN(p1, pmax)
    pp = MAX(pp, pmin)
    rr = MIN(r1, rmax)
    rr = MAX(rr, rmin)

    ! Compute new USTAR, THSTAR, and AMOL.

    alnetao = ALOG(etao)
    au      = fau(amu,etao,pp,xa,xo)
    at      = fat(amu,etao,akapa,rr)

    termo   = etaf * ALOG(etaf/etao) + etao - etaf

    term1   = (etaf-etao) * fpsim(amu*etao,xo)
    term2   = -(fgamu(amu*etaf,xf) - fgamu(amu*etao,xo)) / amu
    term3   = au * (etaf-etao)**(pp+1.0) / (pp+1.0)

    term4   = (etaf-etao) * fpsih(amu*etao,yo)
    term5   = -(fgamh(amu*etaf,yf) - fgamh(amu*etao,yo)) / amu
    term6   = at * (etaf-etao)**(rr+1.0) / (rr+1.0)

    usnew   = vkar * speed * (etaf-etao) / (termO + term1 + term2 + term3)
    thsnew  = vkar * (thm-thref) * (etaf-etao) / pro
    thsnew  = thsnew / (termo + term4 + term5 + term6)

    err0    = ABS( (amol-amolnew) / amol )
    err1    = ABS( (ustar-usnew)  / ustar )

    IF ( ABS(thstar) > small*0.01) THEN
      amolnew = thref * usnew * usnew / (vkar * grav * thsnew)
      err2    = ABS( (thstar-thsnew)/thstar )
    ELSE
      amolnew = SIGN(1.0,(thm-thref)) * big
      err2    = 0.0
    ENDIF

    amolnew = SIGN(1.0,amolnew) * AMIN1(ABS(amolnew),big)
    thstar  = thsnew
    ustar   = usnew
    amol    = amolnew

    IF ( amu < -2.0 ) THEN  ! Limit USTAR*THSTAR with HFXMAX

      hratio = - ustar * thstar / hfxmax

      ! If unstable and too high heat flux, 
      ! limit sfc-air temperature difference, but keep the AMOL the same.

      IF ( hratio > 1.0 ) THEN
        ustar  = ustar  / hratio**0.333333
        thstar = thstar / hratio**0.666666
      ENDIF

    ENDIF

    IF ( err0 < tol ) EXIT diabatic

    ENDDO diabatic

  ENDIF

  ! Estimate qstar
            
  IF ( ABS(thstar) <= small ) THEN
    qvstar = thstar
  ELSE
    qvstar = thstar * dqv / ( thm-thref )
  ENDIF
      
  RETURN

  999 CONTINUE

!-------------------------------------------------------------------------------
! Since conversion for PBL similarity failed, use prediction by
! surface layer similarity.
!-------------------------------------------------------------------------------

  thref  = thref1
  amol   = amol1
  ustar  = ustar1
  thstar = thstar1
  iter   = 0
      
  IF ( ABS(thstar) <= small ) THEN
    qvstar = thstar
  ELSE
    qvstar = thstar * dqv / ( thm-thref )
  ENDIF
      
  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SFCFLUX',                            &
              /, 1x, '***   TOO CLOSE TO EQUATOR',                         &
              /, 1x, '***   CODE WILL FAIL IN HPBL CALC WITH FCORI',       &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SFCFLUX',                            &
              /, 1x, '***   CONVERGENCE FAILED FOR POWER ', a,             &
              /, 1x, '***   ALAMDA, AKAPA, AMU = ', 3(2x, f10.4),          &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE sfcflux
