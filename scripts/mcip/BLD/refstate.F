
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
! $Header: /project/work/rep/MCIP2/src/mcip2/refstate.F,v 1.3 2005/08/29 14:36:54 tlotte Exp $ 


SUBROUTINE refstate

!-------------------------------------------------------------------------------
! Name:     Reference State
! Purpose:  Calculate non-hydrostatic reference state from meteorology input.
! Revised:  10 Sep 2001  Original version.  (T. Otte)
!           07 Jul 2004  Removed XFLAGS.  (T. Otte)
!           30 Mar 2005  Updated layer height calculations to use formulae
!                        defined by NCAR for MM5 base state rather than
!                        subroutine LAYHT.  Removed unused variables XDENSAM_REF
!                        and XENTRP.  (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE const
  USE metinfo, aa => met_tlp, p00 => met_p00, ts0 => met_ts0

  IMPLICIT NONE

  REAL                         :: alogpf
  REAL                         :: alogpm
  INTEGER                      :: c
  REAL                         :: g2ora
  REAL,          PARAMETER     :: gor        = grav / rdgas
  INTEGER                      :: k
  INTEGER                      :: lvl
  REAL                         :: presf
  REAL                         :: presm
  REAL                         :: press
  REAL                         :: pstar0
  INTEGER                      :: r
  REAL                         :: rao2g
  REAL,          PARAMETER     :: rog        = rdgas / grav
  REAL                         :: rogt
  REAL                         :: tempf
  REAL                         :: tempm
  REAL                         :: ts0oa
  REAL                         :: tsfc

!-------------------------------------------------------------------------------
! Extract and define constants for reference calculations.
!-------------------------------------------------------------------------------

  ts0oa  = ts0 / aa
  g2ora  = 2.0 * gor / aa
  rao2g  = 0.5 * rog * aa
  rogt   = rog * ts0

!-------------------------------------------------------------------------------
! Calculate reference density, reference Jacobian, and reference height.
! Formulae for reference pressure, temperature, and height from MM5
! documentation (e.g., on-line tutorial notes in INTERPF section).
!-------------------------------------------------------------------------------

  DO c = 1, ncols_x
    DO r = 1, nrows_x

      press  = p00 * EXP (- ts0oa + SQRT(ts0oa**2 - g2ora * xtopo(c,r)))

      tsfc   = ts0 + aa * LOG( press / p00 )

      pstar0 = press - x3top

      xdenss  (c,r)   = press / ( rdgas * tsfc )
      xdensaf (c,r,0) = xdenss(c,r)
      x3jacobf(c,r,0) = pstar0 / ( grav * xdenss(c,r) )  ! for JACOBS0
      x3htf   (c,r,0) = 0.0

      DO lvl = 1, metlay

        presm = x3top + pstar0 * ( 1.0 - xx3midl(lvl) )
        presf = x3top + pstar0 * ( 1.0 - xx3face(lvl) )

        alogpm = ALOG(presm/p00)
        alogpf = ALOG(presf/p00)

        tempm = ts0 + aa * alogpm
        tempf = ts0 + aa * alogpf

        xdensam(c,r,lvl)  = presm / ( rdgas * tempm )
        xdensaf(c,r,lvl)  = presf / ( rdgas * tempf )

        x3jacobm(c,r,lvl) = pstar0 / ( grav * xdensam(c,r,lvl) )
        x3jacobf(c,r,lvl) = pstar0 / ( grav * xdensaf(c,r,lvl) )

        x3htm(c,r,lvl)    = - (rao2g * alogpm**2 + rogt * alogpm)
        x3htm(c,r,lvl)    = x3htm(c,r,lvl) - xtopo(c,r)

        x3htf(c,r,lvl)    = - (rao2g * alogpf**2 + rogt * alogpf)
        x3htf(c,r,lvl)    = x3htf(c,r,lvl) - xtopo(c,r)

      ENDDO

    ENDDO
  ENDDO

  xdensaf_ref(:,:,:) = xdensaf(:,:,:)

  DO k = 1, metlay
    xdx3htf(:,:,k) = x3htf(:,:,k) - x3htf(:,:,k-1)  ! X3HTF vert starts at 0
  ENDDO

END SUBROUTINE refstate
