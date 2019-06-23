
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
! $Header: /project/work/rep/MCIP2/src/mcip2/metgrid2ctm.F,v 1.4 2005/08/29 14:34:50 tlotte Exp $ 


SUBROUTINE metgrid2ctm

!-------------------------------------------------------------------------------
! Name:     Meteorology Grid to CTM Grid
! Purpose:  Puts time-invariant meteorology arrays on CTM grid.
! Notes:    Algorithms taken from MCIP v1 getmet_mm5.F.
! Revised:  17 Sep 2001  Original version.  (T. Otte)
!           16 Oct 2001  Corrected error in translation between input
!                        domain and MCIP "X" domain.  (T. Otte)
!           21 Dec 2001  Changed order of variable declarations in interface
!                        to improve portability.  (S. Howard and T. Otte)
!           22 Jan 2002  Added definition of XPRSFC for non-hydrostatic
!                        runs.  (T. Otte)
!           26 Mar 2003  Simplified algorithm to map input meteorology to
!                        MCIP_X domain.  Enabled dot-point MCIP_X arrays to
!                        be filled to the correct dimensions.  (T. Otte)
!           07 Jul 2004  Removed XFLAGS.  (T. Otte)
!           21 Jan 2005  Removed NDX and option to interpolate to finer scale
!                        meteorology.  Changed I and J to X and Y to make
!                        code more general.  Added optimization of loops
!                        using F90 implicit loop structures.  (T. Otte and
!                        D. Wong)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE metvars
  USE metinfo

  IMPLICIT NONE

  INTEGER                      :: col
  INTEGER                      :: ec
  INTEGER                      :: er
  INTEGER                      :: ii
  INTEGER                      :: iil
  INTEGER                      :: jw
  INTEGER                      :: jj
  INTEGER                      :: jjl
  INTEGER                      :: lbndface
  INTEGER                      :: lbndsigf
  INTEGER                      :: lu
  INTEGER                      :: maxface
  CHARACTER*16,  PARAMETER     :: pname     = "METGRID2CTM"
  INTEGER                      :: row
  INTEGER                      :: sc
  INTEGER                      :: sr
  INTEGER                      :: ubndsigf
  REAL,          ALLOCATABLE   :: x3workf   ( : )

!-------------------------------------------------------------------------------
! Set up vertical coordinate, where XX3FACE and XX3MIDL are layer face and
! middle definitions.  XX3FACE and XX3MIDL are dimensioned in MCIPCOM
! and increase monotonically with height.
!-------------------------------------------------------------------------------

  lbndsigf = LBOUND(sigmaf,1)
  lbndface = LBOUND(xx3face,1)
  ubndsigf = UBOUND(sigmaf,1)
  maxface  = lbndface + SIZE(sigmaf) - 1

  ALLOCATE ( x3workf ( lbndsigf:ubndsigf ) )

  IF ( sigmaf(lbndsigf) > sigmaf(ubndsigf) ) THEN  ! NOT monotonic increase
    DO jw = lbndsigf, ubndsigf
      x3workf(jw) = sigmaf(lbndsigf) - sigmaf(jw)
    ENDDO
  ELSE
    DO jw = lbndsigf, ubndsigf
      x3workf(jw) = sigmaf(jw) - sigmaf(lbndsigf)
    ENDDO
  ENDIF

  xx3face(lbndface:maxface) = x3workf(lbndsigf:ubndsigf)

  DO jw = 1, SIZE(sigmah)
    xx3midl(jw) = 0.5 * ( xx3face(jw-1) + xx3face(jw) )
  ENDDO

  DEALLOCATE ( x3workf )

!-------------------------------------------------------------------------------
! Put time-invariant cross-point arrays on MCIP_X grid.
!-------------------------------------------------------------------------------

  sc = x0
  ec = x0 + ncols_x - 1
  sr = y0
  er = y0 + nrows_x - 1

  xtopo(:,:) = terrain(sc:ec,sr:er)
  xmapc(:,:) = mapcrs (sc:ec,sr:er)
  xlatc(:,:) = latcrs (sc:ec,sr:er)
  xlonc(:,:) = loncrs (sc:ec,sr:er)

  IF ( ( met_model == 1 ) .AND. ( met_inhyd == 1 ) ) THEN  ! non-hydrostatic MM5
    xprsfc(:,:) = psa(sc:ec,sr:er) + x3top
  ENDIF

!-------------------------------------------------------------------------------
! Put time-invariant dot-point arrays on MCIP_X grid.
!-------------------------------------------------------------------------------

  sc = x0
  ec = x0 + ncols_x
  sr = y0
  er = y0 + nrows_x

  xmapd(:,:) = mapdot(sc:ec,sr:er)
  xlatd(:,:) = latdot(sc:ec,sr:er)
  xlond(:,:) = londot(sc:ec,sr:er)

END SUBROUTINE metgrid2ctm
