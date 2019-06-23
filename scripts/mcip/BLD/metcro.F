
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
! $Header: /project/work/rep/MCIP2/src/mcip2/metcro.F,v 1.6 2005/08/29 14:34:26 tlotte Exp $ 


SUBROUTINE metcro (sdate, stime)

!-------------------------------------------------------------------------------
! Name:     Meteorology on Cross Points
! Purpose:  Compute and output time-dependent, cross-point parameters.
!             MET_CRO_2D     MET_CRO_3D     MET_BDY_3D
! Revised:  15 Jan 1997  Created for MCIP and generalized CTM.  (D. Byun)
!           04 Feb 1998  LSM method for nonglobal variables changed.  (???)
!           30 Apr 1999  PSTAR was replaced with PRSFC and MAXK.  (D. Byun)
!           20 Sep 2001  Converted to free-form f90.  Added interface for
!                        COLLAPX and removed WORKC.  Added interface for
!                        SANITY and removed NCELLS.  Removed IFTYPE and
!                        dependence of TSTEP3D on this variable.  This routine
!                        always accesses time-dependent data.  Removed ISTAT
!                        from argument list.  Changed DUMARAY0 and DUMARAY1 to
!                        allocatable.  Removed NLAYS from argument lists
!                        for COMHEADER and COLLAPX.  Added QICE, QSNOW, SOIM1,
!                        SOIM2, SOIT1, SOIT2, and SLTYP to output.  Removed
!                        unused arrays for LAMDA and MCONERR.  (T. Otte)
!           11 Oct 2001  Changed lower-limit restrictions on MC3 and MB3 with
!                        epsilon.  (T. Otte)
!           20 Nov 2001  Removed "sanity" checks.  (T. Otte)
!           27 Feb 2002  Changed "stop" statements to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  Added logic
!                        to enable "missing" variables in output to have
!                        values of BADVAL3 < AMISS3, or 0.0, as appropriate.
!                        Removed RIB from output.  Renamed SURF2 as WIND10
!                        and SURF1 as TEMP1P5.  (T. Otte)
!           27 Mar 2003  Corrected calculation of WGHT_TOP and WGHT_BOT that
!                        is used to get WHAT_JD_C and WHAT_JD_B on full levels.
!                        Removed extraneous calculation of X3MIDL.  Changed
!                        calls to COLLAPX to be on Jacobian coupled with
!                        density instead of just Jacobian for better mass
!                        conservation reasons.  Changed subsequent calculations
!                        of output variables based on Jacobian.  Removed
!                        JDRATE.  (J. Pleim and T. Otte)
!           09 Jun 2003  Added SNOCOV to output.  (D. Schwede)
!                        Removed extraneous variables from output.  Also
!                        removed unused file METBDY2D.  (T. Otte)
!           09 Aug 2004  Modified code so that arrays are made available in
!                        output only if user options in MM5 generate those
!                        data.  Added graupel (QG), full Jacobian (JACOBS and
!                        JACOBF), 10-m wind speed and direction (WSPD10 and
!                        WDIR10), passed-through 2-m temperature (TEMP2), and
!                        leaf-area index from Pleim-Xiu LSM to output.
!                        Changed local array allocation to occur only on initial
!                        call to subroutine to avoid memory fragmentation.
!                        Added initialization of NTHIK3D for I/O API output.
!                        (T. Otte and D. Schwede)
!           31 Jan 2005  Changed output file names by removing "_G1".  (T. Otte)
!           19 Aug 2005  Added USE statement for new DEPVVARS module.  Changed
!                        internal variable EPSILON to EPSILONQ and internal
!                        variable INDEX to IDX to avoid confusion with F90
!                        intrinsic functions.  Modified call to COLLAPX to
!                        reflect that there is one option for layer collapsing.
!                        (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE depvvars
  USE coord
  USE fdesc3
  USE file
  USE iodecl3
  USE mcoutcom
  USE vgrd
  USE parms3

  IMPLICIT NONE

  INTEGER                      :: c
  INTEGER                      :: col
  REAL,    SAVE, ALLOCATABLE   :: dumaray0    ( : , : , : , : )
  REAL,    SAVE, ALLOCATABLE   :: dumaray1    ( : , : , : , : )
  REAL,          PARAMETER     :: epsilonq    = 1.0e-30
  LOGICAL,       SAVE          :: first       = .TRUE.
  INTEGER                      :: idx
  CHARACTER*63                 :: ifmt1
  INTEGER                      :: ilg
  INTEGER                      :: intt2flag   = 0
  INTEGER                      :: jdate
  INTEGER                      :: k
  INTEGER                      :: k1
  INTEGER                      :: k2
  INTEGER                      :: l
  INTEGER                      :: lvl
  INTEGER                      :: n
  CHARACTER*16,  PARAMETER     :: pname       = 'METCRO'
  INTEGER                      :: r
  INTEGER                      :: row
  INTEGER,       INTENT(IN)    :: sdate
  INTEGER,       INTENT(IN)    :: stime
  CHARACTER*2                  :: str1
  CHARACTER*2                  :: str2
  CHARACTER*16                 :: spcname
  REAL,    SAVE, ALLOCATABLE   :: wght_bot    ( : )
  REAL,    SAVE, ALLOCATABLE   :: wght_top    ( : )
  REAL                         :: x3jfmin
  REAL                         :: x3jmmin
  REAL                         :: xdnamin
  REAL                         :: xdnjmin
  REAL                         :: xmapmin
  REAL,    SAVE, ALLOCATABLE   :: xrhojf      ( : , : , : )
  REAL,    SAVE, ALLOCATABLE   :: xrhojm      ( : , : , : )

  INTERFACE

    SUBROUTINE collapx (aa, vertin, vertout)
      IMPLICIT NONE
      REAL,          INTENT(INOUT) :: aa         ( : , : , : )
      REAL,          INTENT(IN)    :: vertin     ( : )
      REAL,          INTENT(IN)    :: vertout    ( : )
    END SUBROUTINE collapx

  END INTERFACE

!-------------------------------------------------------------------------------
! Allocate necessary variables.
!-------------------------------------------------------------------------------

  IF ( .NOT. ALLOCATED ( xrhojf ) )  &
    ALLOCATE ( xrhojf ( ncols_x, nrows_x, 0:metlay ) )
  IF ( .NOT. ALLOCATED ( xrhojm ) )  &
    ALLOCATE ( xrhojm ( ncols_x, nrows_x,   metlay ) )

!-------------------------------------------------------------------------------
! If first time, build headers for files.
!-------------------------------------------------------------------------------

  IF ( first ) THEN

    ALLOCATE ( wght_top (nlays) )
    ALLOCATE ( wght_bot (nlays) )

    ! Build common header.

    CALL comheader (sdate, stime)

    !---------------------------------------------------------------------------
    ! Build and write header for METCRO2D file.
    !---------------------------------------------------------------------------

    DO idx = 1, mc2index
      vtype3d(idx) = m3real
      vname3d(idx) = mc2vname(idx)
      units3d(idx) = mc2units(idx)
      vdesc3d(idx) = mc2vdesc(idx)
    ENDDO

    IF ( t2out ) THEN
      intt2flag = 1
      idx = mc2index + intt2flag
      vtype3d(idx) = m3real
      vname3d(idx) = t2vname
      units3d(idx) = t2units
      vdesc3d(idx) = t2vdesc
    ENDIF

    IF ( px ) THEN
      DO ilg = 1, pc2index
        idx = mc2index + intt2flag + ilg
        vtype3d(idx) = m3real
        vname3d(idx) = pc2vname(ilg)
        units3d(idx) = pc2units(ilg)
        vdesc3d(idx) = pc2vdesc(ilg)
      ENDDO
    ENDIF

    ! Set up header for deposition velocity arrays.

    DO ilg = 1, ltotg
      idx = mc2index + intt2flag + npxfields + ilg
      vtype3d(idx) = m3real
      spcname      = TRIM(xdepspc(ilg))
      vname3d(idx) = 'VD_' // spcname
      units3d(idx) = 'M/S'
      vdesc3d(idx) = 'deposition velocity for species ' // spcname
    ENDDO

    gdnam3d = TRIM(pname) // '_' // TRIM(grdnam) // '_CROSS'

    ftype3d = grdded3
    nvars3d = idx
    nlays3d = 1
    ncols3d = ncols
    nrows3d = nrows
    nthik3d = nthik
    tstep3d = grstep

    IF ( .NOT. open3 (metcro2d, fsunkn3, pname) ) THEN
      WRITE (6,9000) TRIM(metcro2d)
      GOTO 1001
    ENDIF

    !---------------------------------------------------------------------------
    ! Build and write header for METCRO3D file.
    !---------------------------------------------------------------------------

    DO idx = 1, mc3index
      vtype3d(idx) = m3real
      vname3d(idx) = mc3vname(idx)
      units3d(idx) = mc3units(idx)
      vdesc3d(idx) = mc3vdesc(idx)
    ENDDO

    DO ilg = 1, nqspecies
      idx = mc3index + ilg
      vtype3d(idx) = m3real
      vname3d(idx) = qc3vname(ilg)
      units3d(idx) = qc3units(ilg)
      vdesc3d(idx) = qc3vdesc(ilg)
    ENDDO

    gdnam3d = TRIM(pname) // '_' // TRIM(grdnam) // '_CROSS'

    xorig3d = xorig_gd
    yorig3d = yorig_gd
    ncols3d = ncols
    nrows3d = nrows
    nthik3d = nthik

    ftype3d = grdded3
    nvars3d = mc3index + nqspecies
    nlays3d = nlays

    IF ( .NOT. open3 (metcro3d, fsunkn3, pname) ) THEN
      WRITE (6,9000) TRIM(metcro3d)
      GOTO 1001
    ENDIF

    !---------------------------------------------------------------------------
    ! Build and write header for METBDY3D file.
    ! Header information is the same as METCRO3D except for file type.
    !---------------------------------------------------------------------------

    ftype3d = bndary3
    nvars3d = mc3index + nqspecies
    nlays3d = nlays

    IF ( .NOT. open3 (metbdy3d, fsunkn3, pname) ) THEN
      WRITE (6,9000) TRIM(metbdy3d)
      GOTO 1001
    ENDIF

    ! Compute linear interpolation coefficients based on layer thickness.

    DO lvl = 1, nlays-1
      wght_top(lvl) = ( x3face_gd(lvl)   - x3midl(lvl) )  &
                      / ( x3midl (lvl+1) - x3midl(lvl) )
      wght_bot(lvl) = 1.0 - wght_top(lvl)
    ENDDO

    first = .FALSE.

  ENDIF

!-------------------------------------------------------------------------------
! Build common header.
!-------------------------------------------------------------------------------

  CALL comheader (sdate, stime)

!-------------------------------------------------------------------------------
! Assign arrays in MET_CRO_2D (time dependent).
!-------------------------------------------------------------------------------

  xmapmin = MINVAL(xmapc)
  x3jfmin = MINVAL(x3jacobf)

  DO row = 1, nrows
    r = row + nthik
    DO col = 1, ncols
      c = col + nthik

      prsfc_c  (col,row) = xprsfc  (c,r)

      IF ( ( x3jfmin < amiss3 ) .OR.  &
           ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
        jacobs_c (col,row) = badval3
      ELSE
        jacobs_c (col,row) = x3jacobf(c,r,0) / xmapc(c,r)**2  ! sfc total Jacob
      ENDIF

      ustar_c  (col,row) = xustar  (c,r,0)
      wstar_c  (col,row) = xwstar  (c,r)
      pbl_c    (col,row) = xpbl    (c,r)
      zzero_c  (col,row) = xzruf   (c,r)

      IF ( xmol(c,r) < amiss3 ) THEN  ! BADVAL3 < AMISS3
        moli_c (col,row) = 0.0
      ELSE
        moli_c (col,row) = 1.0 / xmol (c,r)
      ENDIF

      hfx_c    (col,row) = xhfx    (c,r)
      qfx_c    (col,row) = xqfx    (c,r)

      IF ( xradyn(c,r,0) < amiss3 ) THEN  ! BADVAL3 < AMISS3
        radyni_c (col,row) = 0.0
      ELSE
        radyni_c (col,row) = 1.0 / xradyn  (c,r,0)
      ENDIF

      IF ( xrbndy(c,r,0) < amiss3 ) THEN  ! BADVAL3 < AMISS3
        rbndyi_c (col,row) = 0.0
      ELSE
        rbndyi_c (col,row) = 1.0 / xrbndy  (c,r,0)
      ENDIF

      IF ( xrstom(c,r)   < amiss3 ) THEN  ! BADVAL3 < AMISS3
        rstomi_c (col,row) = 0.0
      ELSE
        rstomi_c (col,row) = 1.0 / xrstom  (c,r)
      ENDIF

      tempg_c  (col,row) = xtempg  (c,r)
      temp10_c (col,row) = xtemp10 (c,r)
      tem1p5_c (col,row) = xtemp1p5(c,r)
      wspd10_c (col,row) = xwspd10 (c,r)
      wdir10_c (col,row) = xwdir10 (c,r)
      glw_c    (col,row) = xglw    (c,r)
      gsw_c    (col,row) = xgsw    (c,r)
      rgrnd_c  (col,row) = xrgrnd  (c,r)

      rainn_c  (col,row) = xrainn  (c,r)
      rainc_c  (col,row) = xrainc  (c,r)
      cfract_c (col,row) = xcfract (c,r)
      cldtop_c (col,row) = xcldtop (c,r)
      cldbot_c (col,row) = xcldbot (c,r)
      wbar_c   (col,row) = xwbar   (c,r) 

      IF ( t2out ) THEN
        temp2_c  (col,row) = xt2     (c,r)
      ENDIF

      IF ( px ) THEN
        soim1_c  (col,row) = xwga    (c,r)
        soim2_c  (col,row) = xw2a    (c,r)
        soit1_c  (col,row) = xtga    (c,r)
        soit2_c  (col,row) = xt2a    (c,r)
        sltyp_c  (col,row) = xsltyp  (c,r)
        lai_c    (col,row) = xlai    (c,r)
      ENDIF

      snocov_c (col,row) = xsnocov (c,r)

      ! Put in deposition velocities

      DO ilg = 1, ltotg
        vd_c(col,row,ilg) = xvd(c,r,ilg)
      ENDDO

    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Calculate density*Jacobian on mid-layers and full levels.  Use this for
! collapsing rather than Jacobian alone.
!-------------------------------------------------------------------------------

  DO k = 1, metlay
    DO r = 1, nrows_x
      DO c = 1, ncols_x

        xrhojm(c,r,k) = xdensam(c,r,k) * x3jacobm(c,r,k)

      ENDDO
    ENDDO
  ENDDO

  DO k = 0, metlay
    DO r = 1, nrows_x
      DO c = 1, ncols_x

        xrhojf(c,r,k) = xdensaf(c,r,k) * x3jacobf(c,r,k)

      ENDDO
    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Collapse 3-D 'X' arrays.  (Original values are replaced.)
! For safe collpasing, store information first in DUMARRAY.
!    IMODE = 1 : weighted layer averaging
!    IMODE = 2 : selective layer collection 
!-------------------------------------------------------------------------------

  IF ( metlay /= nlays ) THEN

    IF ( .NOT. ALLOCATED ( dumaray0 ) ) &
      ALLOCATE ( dumaray0 ( ncols_x, nrows_x, 0:metlay, 5 ) )
    IF ( .NOT. ALLOCATED ( dumaray1 ) ) &
      ALLOCATE ( dumaray1 ( ncols_x, nrows_x, 1:metlay, 6 + nqspecies ) )

    DO k = 1, metlay
      DO r = 1, nrows_x
        DO c = 1, ncols_x

          dumaray1(c,r,k, 1) = xrhojm  (c,r,k)
          dumaray1(c,r,k, 2) = xdensam (c,r,k)
          dumaray1(c,r,k, 3) = xpresm  (c,r,k)
          dumaray1(c,r,k, 4) = xtempm  (c,r,k)
          dumaray1(c,r,k, 5) = xwvapor (c,r,k)
          dumaray1(c,r,k, 6) = x3htm   (c,r,k)

          IF ( nqspecies >= 2 ) THEN
            dumaray1(c,r,k, 7) = xcldwtr (c,r,k)
            dumaray1(c,r,k, 8) = xranwtr (c,r,k)
            IF ( nqspecies >= 4 ) THEN
              dumaray1(c,r,k, 9) = xqice   (c,r,k)
              dumaray1(c,r,k,10) = xqsnow  (c,r,k)
              IF ( nqspecies == 5 ) THEN
                dumaray1(c,r,k,11) = xqgraup (c,r,k)
              ENDIF
            ENDIF
          ENDIF

        ENDDO 
      ENDDO
    ENDDO

    DO k = 0, metlay
      DO r = 1, nrows_x
        DO c = 1, ncols_x

          dumaray0(c,r,k,1) = xrhojf  (c,r,k)
          dumaray0(c,r,k,2) = xwhat   (c,r,k)
          dumaray0(c,r,k,3) = xwwind  (c,r,k)
          dumaray0(c,r,k,4) = x3htf   (c,r,k)
          dumaray0(c,r,k,5) = xdensaf (c,r,k)

        ENDDO 
      ENDDO
    ENDDO    
      
    CALL collapx (xrhojm,  xx3midl, x3midl)
    CALL collapx (xdensam, xx3midl, x3midl)
    CALL collapx (xpresm,  xx3midl, x3midl)
    CALL collapx (xtempm,  xx3midl, x3midl)
    CALL collapx (xwvapor, xx3midl, x3midl)
    CALL collapx (x3htm,   xx3midl, x3midl)
    IF ( nqspecies >= 2 ) THEN
      CALL collapx (xcldwtr, xx3midl, x3midl)
      CALL collapx (xranwtr, xx3midl, x3midl)
      IF ( nqspecies >= 4 ) THEN
        CALL collapx (xqice,   xx3midl, x3midl)
        CALL collapx (xqsnow,  xx3midl, x3midl)
        IF ( nqspecies == 5 ) THEN
          CALL collapx (xqgraup, xx3midl, x3midl)
        ENDIF
      ENDIF
    ENDIF

    CALL collapx (xrhojf,  xx3face, x3face)
    CALL collapx (xwhat,   xx3face, x3face)
    CALL collapx (xwwind,  xx3face, x3face)
    CALL collapx (x3htf,   xx3face, x3face)
    CALL collapx (xdensaf, xx3face, x3face)
 
  ENDIF  

!-------------------------------------------------------------------------------
! Assign arrays in MET_CRO_3D (time dependent).
!-------------------------------------------------------------------------------

  x3jmmin = MINVAL(x3jacobm)
  xdnamin = MINVAL(xdensam)

  DO row = 1, nrows
    r = row + nthik
    DO col = 1, ncols
      c = col + nthik
      DO lvl = 1, nlays

        ! Essential for generalized CTM

        IF ( ( x3jfmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobf_c (col,row,lvl) = badval3
        ELSE
          jacobf_c (col,row,lvl) = ( xrhojf(c,r,lvl) / xdensaf(c,r,lvl) ) /   &
                                   xmapc(c,r)**2
        ENDIF

        IF ( ( x3jmmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobm_c (col,row,lvl) = badval3
        ELSE
          jacobm_c (col,row,lvl) = ( xrhojm(c,r,lvl) / xdensam(c,r,lvl) ) /   &
                                   xmapc(c,r)**2
        ENDIF

        IF ( ( xdnamin < amiss3 ) .OR.  &
             ( x3jmmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          densa_j_c(col,row,lvl) = badval3
        ELSE
          densa_j_c(col,row,lvl) = xrhojm(c,r,lvl) / xmapc(c,r)**2
        ENDIF

        ! Following for testing consistency in met. data

        tempa_c  (col,row,lvl) = xtempm  (c,r,lvl)
        wvapor_c (col,row,lvl) = xwvapor (c,r,lvl)
        press_c  (col,row,lvl) = xpresm  (c,r,lvl)
        densa_c  (col,row,lvl) = xdensam (c,r,lvl)
        wwind_c  (col,row,lvl) = xwwind  (c,r,lvl)
        x3htf_c  (col,row,lvl) = x3htf   (c,r,lvl)
        x3htm_c  (col,row,lvl) = x3htm   (c,r,lvl)

        ! Used for cloud and AQCHEM

        IF ( nqspecies >= 2 ) THEN
          cldwtr_c (col,row,lvl) = xcldwtr (c,r,lvl)
          ranwtr_c (col,row,lvl) = xranwtr (c,r,lvl)
          IF ( nqspecies >= 4 ) THEN
            qice_c   (col,row,lvl) = xqice   (c,r,lvl)
            qsnow_c  (col,row,lvl) = xqsnow  (c,r,lvl)
            IF ( nqspecies == 5 ) THEN
              qgraup_c (col,row,lvl) = xqgraup (c,r,lvl)
            ENDIF
          ENDIF
        ENDIF

      ENDDO
    ENDDO
  ENDDO

  ! Added for mass consistency

  xdnjmin = MINVAL(densa_j_c)

  IF ( xdnjmin < amiss3 ) THEN  ! BADVAL3 < AMISS3

    what_jd_c(:,:,:) = badval3

  ELSE

    DO row = 1, nrows
      r = row + nthik
      DO col = 1, ncols
        c = col + nthik
        DO lvl = 1, nlays-1

          what_jd_c(col,row,lvl) = xwhat(c,r,lvl) *                           &
                                   ( wght_bot(lvl) * densa_j_c(col,row,lvl)   & 
                                   + wght_top(lvl) * densa_j_c(col,row,lvl+1) )
        ENDDO

        what_jd_c(col,row,nlays) = 0.0

      ENDDO
    ENDDO

  ENDIF

!-------------------------------------------------------------------------------
! Assign arrays in MET_BDY_3D (time dependent).
!-------------------------------------------------------------------------------

  idx = 0
  DO r = 1, nthikd
    DO c = 1 + nthik, ncols_x
      idx = idx + 1
      DO lvl = 1, nlays

        ! Essential for generalized CTM

        IF ( ( x3jfmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobf_b (idx,lvl) = badval3
        ELSE
          jacobf_b (idx,lvl) = ( xrhojf(c,r,lvl) / xdensaf(c,r,lvl) ) /  &
                                 xmapc(c,r)**2
        ENDIF

        IF ( ( x3jmmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobm_b (idx,lvl) = badval3
        ELSE
          jacobm_b (idx,lvl) = ( xrhojm(c,r,lvl) / xdensam(c,r,lvl) ) /  &
                                 xmapc(c,r)**2
        ENDIF

        IF ( ( xdnamin < amiss3 ) .OR.  &
             ( x3jmmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          densa_j_b(idx,lvl) = badval3
        ELSE
          densa_j_b(idx,lvl) = xrhojm(c,r,lvl) / xmapc(c,r)**2
        ENDIF

        ! Following for testing consistency in met. data

        tempa_b  (idx,lvl) = xtempm  (c,r,lvl)
        wvapor_b (idx,lvl) = xwvapor (c,r,lvl)
        press_b  (idx,lvl) = xpresm  (c,r,lvl)
        densa_b  (idx,lvl) = xdensam (c,r,lvl)
        wwind_b  (idx,lvl) = xwwind  (c,r,lvl)
        x3htf_b  (idx,lvl) = x3htf   (c,r,lvl)
        x3htm_b  (idx,lvl) = x3htm   (c,r,lvl)

        ! Used for cloud and AQCHEM

        IF ( nqspecies >= 2 ) THEN
          cldwtr_b (idx,lvl) = xcldwtr (c,r,lvl)
          ranwtr_b (idx,lvl) = xranwtr (c,r,lvl)
          IF ( nqspecies >= 4 ) THEN
            qice_b   (idx,lvl) = xqice   (c,r,lvl)
            qsnow_b  (idx,lvl) = xqsnow  (c,r,lvl)
            IF ( nqspecies == 5 ) THEN
              qgraup_b (idx,lvl) = xqgraup (c,r,lvl)
            ENDIF
          ENDIF
        ENDIF

      ENDDO 

      ! Added for mass consistency

      IF ( xdnjmin < amiss3 ) THEN  ! BADVAL3 < AMISS3

        what_jd_b(idx,:) = badval3

      ELSE

        DO lvl = 1, nlays-1
          what_jd_b(idx,lvl) = xwhat(c,r,lvl) *                         &
                                 ( wght_bot(lvl) * densa_j_b(idx,lvl)   &
                                 + wght_top(lvl) * densa_j_b(idx,lvl+1) )
        ENDDO

        what_jd_b(idx,nlays) = 0.0

      ENDIF

    ENDDO
  ENDDO

  ! Side I: add one more index at the end
  ! for NTHIK=0, march 1 more position

  IF ( nthik == 0 ) idx = idx + 1 

  DO r = 1+nthik, nrows_x
    DO l = 1, nthikd
      c = ncols_x - nthikd + l
      idx = idx + 1
      DO lvl = 1, nlays

        ! Essential for generalized CTM

        IF ( ( x3jfmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobf_b (idx,lvl) = badval3
        ELSE
          jacobf_b (idx,lvl) = ( xrhojf(c,r,lvl) / xdensaf(c,r,lvl) ) /  &
                                 xmapc(c,r)**2
        ENDIF

        IF ( ( x3jmmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobm_b (idx,lvl) = badval3
        ELSE
          jacobm_b (idx,lvl) = ( xrhojm(c,r,lvl) / xdensam(c,r,lvl) ) /  &
                                 xmapc(c,r)**2
        ENDIF

        IF ( ( xdnamin < amiss3 ) .OR.  &
             ( x3jmmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          densa_j_b(idx,lvl) = badval3
        ELSE
          densa_j_b(idx,lvl) = xrhojm(c,r,lvl) / xmapc(c,r)**2
        ENDIF

        ! Following for testing consistency in met. data

        tempa_b  (idx,lvl) = xtempm  (c,r,lvl)
        wvapor_b (idx,lvl) = xwvapor (c,r,lvl)
        press_b  (idx,lvl) = xpresm  (c,r,lvl)
        densa_b  (idx,lvl) = xdensam (c,r,lvl)
        wwind_b  (idx,lvl) = xwwind  (c,r,lvl)
        x3htf_b  (idx,lvl) = x3htf   (c,r,lvl)
        x3htm_b  (idx,lvl) = x3htm   (c,r,lvl)

        ! Used for cloud and AQCHEM

        IF ( nqspecies >= 2 ) THEN
          cldwtr_b (idx,lvl) = xcldwtr (c,r,lvl)
          ranwtr_b (idx,lvl) = xranwtr (c,r,lvl)
          IF ( nqspecies >= 4 ) THEN
            qice_b   (idx,lvl) = xqice   (c,r,lvl)
            qsnow_b  (idx,lvl) = xqsnow  (c,r,lvl)
            IF ( nqspecies == 5 ) THEN
              qgraup_b (idx,lvl) = xqgraup (c,r,lvl)
            ENDIF
          ENDIF
        ENDIF

      ENDDO 

      ! Added for mass consistency

      IF ( xdnjmin < amiss3 ) THEN  ! BADVAL3 < AMISS3

        what_jd_b(idx,:) = badval3

      ELSE

        DO lvl = 1, nlays-1
          what_jd_b(idx,lvl) = xwhat(c,r,lvl) *                         &
                                 ( wght_bot(lvl) * densa_j_b(idx,lvl)   &
                                 + wght_top(lvl) * densa_j_b(idx,lvl+1) )
        ENDDO

        what_jd_b(idx,nlays) = 0.0

      ENDIF

    ENDDO
  ENDDO

  ! Side II: add one more index at the end
  ! for NTHIK=0, march 1 more position

  IF ( nthik == 0 ) idx = idx + 1 
      
  ! side III add one more index before
  ! for NTHIK=0, march 1 more position

  IF ( nthik == 0 ) idx = idx + 1 
      
  DO l = 1, nthikd
    r = nrows_x - nthikd + l
    DO c = 1, ncols_x - nthik

      idx = idx + 1

      DO lvl = 1, nlays

        ! Essential for generalized CTM

        IF ( ( x3jfmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobf_b (idx,lvl) = badval3
        ELSE
          jacobf_b (idx,lvl) = ( xrhojf(c,r,lvl) / xdensaf(c,r,lvl) ) /  &
                                 xmapc(c,r)**2
        ENDIF

        IF ( ( x3jmmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobm_b (idx,lvl) = badval3
        ELSE
          jacobm_b (idx,lvl) = ( xrhojm(c,r,lvl) / xdensam(c,r,lvl) ) /  &
                                 xmapc(c,r)**2
        ENDIF

        IF ( ( xdnamin < amiss3 ) .OR.  &
             ( x3jmmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          densa_j_b(idx,lvl) = badval3
        ELSE
          densa_j_b(idx,lvl) = xrhojm(c,r,lvl) / xmapc(c,r)**2
        ENDIF

        ! Following for testing consistency in met. data

        tempa_b  (idx,lvl) = xtempm  (c,r,lvl)
        wvapor_b (idx,lvl) = xwvapor (c,r,lvl)
        press_b  (idx,lvl) = xpresm  (c,r,lvl)
        densa_b  (idx,lvl) = xdensam (c,r,lvl)
        wwind_b  (idx,lvl) = xwwind  (c,r,lvl)
        x3htf_b  (idx,lvl) = x3htf   (c,r,lvl)
        x3htm_b  (idx,lvl) = x3htm   (c,r,lvl)

        ! Used for cloud and AQCHEM

        IF ( nqspecies >= 2 ) THEN
          cldwtr_b (idx,lvl) = xcldwtr (c,r,lvl)
          ranwtr_b (idx,lvl) = xranwtr (c,r,lvl)
          IF ( nqspecies >= 4 ) THEN
            qice_b   (idx,lvl) = xqice   (c,r,lvl)
            qsnow_b  (idx,lvl) = xqsnow  (c,r,lvl)
            IF ( nqspecies == 5 ) THEN
              qgraup_b (idx,lvl) = xqgraup (c,r,lvl)
            ENDIF
          ENDIF
        ENDIF

      ENDDO 

      ! Added for mass consistency

      IF ( xdnjmin < amiss3 ) THEN  ! BADVAL3 < AMISS3

        what_jd_b(idx,:) = badval3

      ELSE

        DO lvl = 1, nlays-1
          what_jd_b(idx,lvl) = xwhat(c,r,lvl) *               &
                        ( wght_bot(lvl) * densa_j_b(idx,lvl)  & 
                        + wght_top(lvl) * densa_j_b(idx,lvl+1) )
        ENDDO

        what_jd_b(idx,nlays) = 0.0

      ENDIF

    ENDDO
  ENDDO
       
  ! Side IV: add one more index before
  ! for NTHIK=0, march 1 more position

  IF ( nthik == 0 ) idx = idx + 1      

  DO r = 1, nrows_x - nthik
    DO c = 1, nthikd
      idx = idx + 1
      DO lvl = 1, nlays

        ! Essential for generalized CTM

        IF ( ( x3jfmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobf_b (idx,lvl) = badval3
        ELSE
          jacobf_b (idx,lvl) = ( xrhojf(c,r,lvl) / xdensaf(c,r,lvl) ) /  &
                                 xmapc(c,r)**2
        ENDIF

        IF ( ( x3jmmin < amiss3 ) .OR.  &
             ( xmapmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          jacobm_b (idx,lvl) = badval3
        ELSE
          jacobm_b (idx,lvl) = ( xrhojm(c,r,lvl) / xdensam(c,r,lvl) ) /  &
                                 xmapc(c,r)**2
        ENDIF

        IF ( ( xdnamin < amiss3 ) .OR.  &
             ( x3jmmin < amiss3 ) ) THEN  ! BADVAL3 < AMISS3
          densa_j_b(idx,lvl) = badval3
        ELSE
          densa_j_b(idx,lvl) = xrhojm(c,r,lvl) / xmapc(c,r)**2
        ENDIF

        ! Following for testing consistency in met. data

        tempa_b  (idx,lvl) = xtempm  (c,r,lvl)
        wvapor_b (idx,lvl) = xwvapor (c,r,lvl)
        press_b  (idx,lvl) = xpresm  (c,r,lvl)
        densa_b  (idx,lvl) = xdensam (c,r,lvl)
        wwind_b  (idx,lvl) = xwwind  (c,r,lvl)
        x3htf_b  (idx,lvl) = x3htf   (c,r,lvl)
        x3htm_b  (idx,lvl) = x3htm   (c,r,lvl)

        ! Used for cloud and AQCHEM

        IF ( nqspecies >= 2 ) THEN
          cldwtr_b (idx,lvl) = xcldwtr (c,r,lvl)
          ranwtr_b (idx,lvl) = xranwtr (c,r,lvl)
          IF ( nqspecies >= 4 ) THEN
            qice_b   (idx,lvl) = xqice   (c,r,lvl)
            qsnow_b  (idx,lvl) = xqsnow  (c,r,lvl)
            IF ( nqspecies == 5 ) THEN
              qgraup_b (idx,lvl) = xqgraup (c,r,lvl)
            ENDIF
          ENDIF
        ENDIF

      ENDDO 

      ! Added for mass consistency

      IF ( xdnjmin < amiss3 ) THEN  ! BADVAL3 < AMISS3

        what_jd_b(idx,:) = badval3

      ELSE

        DO lvl = 1, nlays-1
          what_jd_b(idx,lvl) = xwhat(c,r,lvl) *                         &
                                 ( wght_bot(lvl) * densa_j_b(idx,lvl)   &
                                 + wght_top(lvl) * densa_j_b(idx,lvl+1) )
        ENDDO

        what_jd_b(idx,nlays) = 0.0

      ENDIF

    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! Write MET_CRO_2D data (time dependent data).
!-------------------------------------------------------------------------------

  IF ( .NOT. desc3 (metcro2d) ) THEN
    CALL m3err ('METCRO', sdate, stime,  &
                'Could not read DESC of ' // metcro2d // ' file', .TRUE.)
  ENDIF

  DO l = 1, mc2index
    IF ( .NOT. write3 (metcro2d, vname3d(l), sdate, stime,  &
                       mc2(1,1,l)) ) THEN
      WRITE (6,9100) TRIM(metcro2d)
      GOTO 1001
    ENDIF
  ENDDO

  IF ( t2out ) THEN
    idx = mc2index + intt2flag
    IF ( .NOT. write3 (metcro2d, vname3d(idx), sdate, stime,  &
                       temp2_c) ) THEN
      WRITE (6,9100) TRIM(metcro2d)
      GOTO 1001
    ENDIF
  ENDIF

  IF ( px ) THEN
    DO l = 1, pc2index
      idx = mc2index + intt2flag + l
      IF ( .NOT. write3 (metcro2d, vname3d(idx), sdate, stime,  &
                         pc2(1,1,l)) ) THEN
        WRITE (6,9100) TRIM(metcro2d)
        GOTO 1001
      ENDIF
    ENDDO
  ENDIF

  DO l = 1, ltotg
    idx = l + mc2index + npxfields + intt2flag
    IF (.NOT. write3 (metcro2d, vname3d(idx), sdate, stime,  &
                      vd_c(1,1,l)) ) THEN
      WRITE (6,9100) TRIM(metcro2d)
      GOTO 1001
    ENDIF
  ENDDO

!-------------------------------------------------------------------------------
! Write MET_CRO_3D data (time dependent data).
!-------------------------------------------------------------------------------

  IF ( .NOT. desc3 (metcro3d) ) THEN
    CALL m3err ('METCRO', sdate, stime,  &
                'Could not read DESC of ' // metcro3d // ' file', .TRUE.)
  ENDIF

  WHERE ( ABS(mc3) < epsilonq ) mc3 = 0.0
  WHERE ( ABS(qc3) < epsilonq ) qc3 = 0.0

  DO l = 1, mc3index
    IF ( .NOT. write3 (metcro3d, vname3d(l), sdate, stime,  &
                       mc3(1,1,1,l)) ) THEN
      WRITE (6,9100) TRIM(metcro3d)
      GOTO 1001
    ENDIF
  ENDDO

  DO l = 1, nqspecies
    IF ( .NOT. write3 (metcro3d, vname3d(mc3index+l), sdate, stime,  &
                       qc3(1,1,1,l)) ) THEN
      WRITE (6,9100) TRIM(metcro3d)
      GOTO 1001
    ENDIF
  ENDDO

!-------------------------------------------------------------------------------
! Write MET_BDY_3D data (time dependent data).
!-------------------------------------------------------------------------------

  IF ( .NOT. desc3 (metbdy3d) ) THEN
    CALL m3err ('METCRO', sdate, stime,  &
                'Could not read DESC of ' // metbdy3d // ' file', .TRUE.)
  ENDIF

  WHERE ( ABS(mb3) < epsilonq ) mb3 = 0.0
  WHERE ( ABS(qb3) < epsilonq ) qb3 = 0.0

  DO l = 1, mc3index
    IF ( .NOT. write3 (metbdy3d, vname3d(l), sdate, stime,  &
                       mb3(1,1,l)) ) THEN
      WRITE (6,9100) TRIM(metbdy3d)
      GOTO 1001
    ENDIF
  ENDDO

  DO l = 1, nqspecies
    IF ( .NOT. write3 (metbdy3d, vname3d(mc3index+l), sdate, stime,  &
                       qb3(1,1,l)) ) THEN
      WRITE (6,9100) TRIM(metbdy3d)
      GOTO 1001
    ENDIF
  ENDDO

!-------------------------------------------------------------------------------
! For safe collpasing, restore information from DUMARAY.
!-------------------------------------------------------------------------------

  IF ( metlay /= nlays ) THEN

    DO k = 1, metlay
      DO r = 1, nrows_x
        DO c = 1, ncols_x
          xrhojm  (c,r,k) = dumaray1(c,r,k, 1)
          xdensam (c,r,k) = dumaray1(c,r,k, 2)
          xpresm  (c,r,k) = dumaray1(c,r,k, 3)
          xtempm  (c,r,k) = dumaray1(c,r,k, 4)
          xwvapor (c,r,k) = dumaray1(c,r,k, 5)
          x3htm   (c,r,k) = dumaray1(c,r,k, 6)
          IF ( nqspecies >= 2 ) THEN
            xcldwtr (c,r,k) = dumaray1(c,r,k, 7)
            xranwtr (c,r,k) = dumaray1(c,r,k, 8)
            IF ( nqspecies >= 4 ) THEN
              xqice   (c,r,k) = dumaray1(c,r,k, 9)
              xqsnow  (c,r,k) = dumaray1(c,r,k,10)
              IF ( nqspecies == 5 ) THEN
                xqgraup (c,r,k) = dumaray1(c,r,k,11)
              ENDIF
            ENDIF
          ENDIF
        ENDDO 
      ENDDO
    ENDDO

    DO k = 0, metlay
      DO r = 1, nrows_x
        DO c = 1, ncols_x
          xrhojf  (c,r,k) = dumaray0(c,r,k,1)
          xwhat   (c,r,k) = dumaray0(c,r,k,2)
          xwwind  (c,r,k) = dumaray0(c,r,k,3)
          x3htf   (c,r,k) = dumaray0(c,r,k,4)
          xdensaf (c,r,k) = dumaray0(c,r,k,5)
        ENDDO 
      ENDDO
    ENDDO 

!   DEALLOCATE ( dumaray0 )  ! commented out to avoid memory fragmentation
!   DEALLOCATE ( dumaray1 )  ! commented out to avoid memory fragmentation

  ENDIF

!-------------------------------------------------------------------------------
! Print sample output to log file.
!-------------------------------------------------------------------------------

  k1 = nlays / 5
  k2 = MOD(nlays, 5)

  WRITE ( str1, '(i2)' ) k1 - 1
  WRITE ( str2, '(i2)' ) k2

  IF ( (k1 - 1) > 0 ) THEN
    IF ( k2 > 0 ) THEN
      ifmt1 = "(/,1x,a9,5(2x,f12.4)," // str1 // "(/,10x,5(2x,f12.4)),/,10x,"   &
         &    // str2 // "(2x,f12.4))"
    ELSE
      ifmt1 = "(/,1x,a9,5(2x,f12.4)," // str1 // "(/,10x,5(2x,f12.4)))"
    ENDIF
  ELSE
    IF ( k2 > 0 ) THEN
      ifmt1 = "(/,1x,a9,5(2x,f12.4),/,10x," // str2 // "(2x,f12.4))"
    ELSE
      ifmt1 = "(/,1x,a9,5(2x,f12.4))"
    ENDIF
  ENDIF

  WRITE (*,'(/,a,/)') '- METCRO: Printing sample cells in output grid'

  DO n = 1, mc2index
    WRITE (*,6000) TRIM(mc2vname(n)), mc2(lprt_col,lprt_row,n), mc2units(n)
  ENDDO

  IF ( t2out ) THEN
    WRITE (*,6000) TRIM(t2vname), temp2_c(lprt_col,lprt_row), t2units
  ENDIF

  IF ( px ) THEN
    DO n = 1, pc2index
      WRITE (*,6000) TRIM(pc2vname(n)), pc2(lprt_col,lprt_row,n), pc2units(n)
    ENDDO
  ENDIF

  DO n = 1, ltotg
    WRITE (*,6000) 'VD_' // TRIM(xdepspc(n)),  &
                   vd_c(lprt_col,lprt_row,n), 'm/s'
  ENDDO

  DO n = 1, mc3index
    WRITE (*,ifmt1) TRIM(mc3vname(n)), mc3(lprt_col,lprt_row,:,n)
  ENDDO

  DO n = 1, nqspecies
    WRITE (*,ifmt1) TRIM(qc3vname(n)), qc3(lprt_col,lprt_row,:,n)
  ENDDO

!-------------------------------------------------------------------------------
! Deallocate variables.
!-------------------------------------------------------------------------------

! DEALLOCATE ( xrhojf )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( xrhojm )  ! commented out to avoid memory fragmentation

  RETURN

!-------------------------------------------------------------------------------
! Format statements.
!-------------------------------------------------------------------------------

 6000 FORMAT (1x, a9, 2x, f12.4, 2x, a)

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: METCRO',                             &
              /, 1x, '***   ERROR OPENING FILE ', a,                       &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: METCRO',                             &
              /, 1x, '***   ERROR WRITING TO FILE ', a,                    &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE metcro
