
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
! $Header: /project/work/rep/MCIP2/src/mcip2/setgriddefs.F,v 1.5 2006/03/01 20:14:16 tlotte Exp $


SUBROUTINE setgriddefs

!-------------------------------------------------------------------------------
! Name:     Set Grid Definitions
! Purpose:  Gets information from user input and input meteorology, and
!           sets up grid definitions.
! Notes:    Some algorithms taken from MCIP v1 getmet_mm5.F.
! Revised:  20 Sep 2001  Original version.  (T. Otte)
!           16 Oct 2001  Added variable COORDNAM.  Corrected definition
!                        of P_GAM_GD.  Added provisions for windowing a
!                        subset of the meteorology domain.  (T. Otte)
!           20 Nov 2001  Corrected setting of XORIG_GD and YORIG_GD.  (T. Otte)
!           10 Jan 2002  Changed calls to "abort" to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  (T. Otte)
!           27 Feb 2002  Removed minimum grid size on windows.  (T. Otte)
!           06 Mar 2003  Modified definitions of XORIG_GD and Y0RIG_GD to
!                        minimize real number round-off issues which can
!                        cause header mismatches in CMAQ.  Added double
!                        precision attributes to variables assigned to
!                        I/O API header.  (T. Otte)
!           10 Aug 2004  Added new grid projections.  Moved definition of
!                        PX to setupv2 and setupv3.  Added T2OUT.  Removed
!                        checks from obsolete land-use input sources. (T. Otte)
!           24 Feb 2005  Removed NDX and option to interpolate to finer scale
!                        meteorology.  Changed I and J naming conventions to
!                        Y and X to make code more general.  Added vertical
!                        coordinate definition for WRF.  Modified prints of
!                        user options to reflect new LDDEP = 3 or 4 for dry
!                        deposition velocity calculations of chlorine and
!                        mercury species with M3DRY.  Added calculation of
!                        XORIG and YORIG for WRF grid definitions.  Added logic
!                        to make center latitude more precise for WRF. (T. Otte)
!           15 Jul 2005  Added debugging for variable retrievals from WRF
!                        files.  Added error-exit for problems defining YORIG
!                        for WRF.  Added provisions for defining WRF XORIG and 
!                        YORIG when dot-point lower-left corner coordinate is
!                        not available in WRF output.  (T. Otte)
!           11 Aug 2005  Removed unused variable X_RESOL.  (T. Otte)
!           27 Feb 2006  Changed tolerances on XORIG_M and YORIG_M for WRF to
!                        allow center of projection be at a face point or scalar
!                        point, rather than only on dot points.  Updated
!                        vertical coordinate definitions to correspond with
!                        updates to I/O API PARMS3.EXT for WRF.  Corrected
!                        I/O API grid definition for Mercator projection.
!                        (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE xvars
  USE metinfo
  USE coord
  USE parms3
  USE file
  USE wrf_netcdf

  REAL                         :: div
  REAL,          ALLOCATABLE   :: dum2d     ( : , : )
  CHARACTER*256                :: fl
  INTEGER                      :: nxm
  INTEGER                      :: nym
  CHARACTER*60                 :: option
  CHARACTER*16,  PARAMETER     :: pname     = 'SETGRIDDEFS'
  REAL,          PARAMETER     :: pole      = 90.0  ! degrees
  INTEGER                      :: rcode
  REAL                         :: rnthik
  LOGICAL                      :: usecrslat = .FALSE.
  LOGICAL                      :: usecrslon = .FALSE.
  REAL                         :: xcntr 
  REAL                         :: xorig_ctm
  REAL                         :: xorig_m
  REAL                         :: xorig_x
  REAL                         :: ycntr
  REAL                         :: ylat
  REAL                         :: ylon
  REAL                         :: yorig_ctm
  REAL                         :: yorig_m
  REAL                         :: yorig_x

!-------------------------------------------------------------------------------
! Define MCIP grid coordinate information from meteorology grid input.
!-------------------------------------------------------------------------------

  metcol = met_nx
  metrow = met_ny
  metlay = met_nz

  IF ( nbdrytrim >= 0 ) THEN  ! not windowing...need to define NCOLS, NROWS
    ncols = met_nx - (2 * nbdrytrim) - (2 * nthik) - 1
    nrows = met_ny - (2 * nbdrytrim) - (2 * nthik) - 1
  ENDIF

  nrows_x = nrows + 2 * nthik
  ncols_x = ncols + 2 * nthik

  nbndy   = 2 * nthik  * (ncols + nrows + 2*nthik)
  nbndyd  = 2 * nthikd * (ncols + nrows + 2*nthikd)

!-------------------------------------------------------------------------------
! Check dimensions of domain.
!-------------------------------------------------------------------------------

  IF ( ( x0 < 1          ) .OR. ( y0 < 1          ) .OR.  &
       ( x0 > met_nx - 1 ) .OR. ( y0 > met_ny - 1 ) ) THEN
    WRITE (6,9000) x0, y0, met_nx, met_ny
    GOTO 1001
  ENDIF

  IF ( ( ncols < 1 ) .OR. ( nrows < 1 ) ) THEN
    WRITE (6,9050) ncols, nrows
    GOTO 1001
  ENDIF

  IF ( ( met_nx < (ncols_x + 1) ) .OR.  &
       ( met_ny < (nrows_x + 1) ) ) THEN
    WRITE (6,9100) met_nx, met_ny, ncols_x + 1, nrows_x + 1
    GOTO 1001
  ENDIF

  IF ( ( ncols > met_nx-2*nthik-1 ) .OR.  &
       ( nrows > met_ny-2*nthik-1 ) ) THEN
    WRITE (6,9200) ncols, nrows, met_nx, met_ny,  &
                   met_nx-2*nthik-1, met_ny-2*nthik-1
    GOTO 1001
  ENDIF

  IF ( ( x0+2*nthik+ncols-1 > met_nx-1 ) .OR.  &
       ( y0+2*nthik+nrows-1 > met_ny-1 ) ) THEN
    WRITE (6,9250) met_nx, met_ny, x0+2*nthik+ncols-1, y0+2*nthik+nrows-1
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Calculate window domain size in terms of MET grid.
!-------------------------------------------------------------------------------

  ncg_x = 1 + INT( ncols + 2 * nthik - 1 )
  ncg_y = 1 + INT( nrows + 2 * nthik - 1 )

!-------------------------------------------------------------------------------
! GDTYP_GD:
! The map projection type:
!   1: LATGRD for lat-lon coordinates (unused)
!   2: LAMGRD for Lambert coordinates
!   3: MERGRD for Mercator coordinates
!   4: STEGRD for Stereographic coordinates
!   5: UTMGRD for UTM coordinates
!   6: POLGRD for polar stereographic coordinates
!   7: EQMGRD for equatorial Mercator coordinates
!   8: TRMGRD for transverse Mercator coordinates
!-------------------------------------------------------------------------------

  IF ( met_mapproj == 1 ) THEN       ! Lambert conformal
    gdtyp_gd = lamgrd3
  ELSE IF ( met_mapproj == 2 ) THEN  ! polar stereographic
    gdtyp_gd = stegrd3
  ELSE IF ( met_mapproj == 3 ) THEN  ! Mercator
    gdtyp_gd = mergrd3
  ELSE
    WRITE (6,9275) met_mapproj
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! The definitions of the map projection specification parameters, 
! P_ALP_GD (alpha),  P_BET_GD (beta), and P_GAM_GD (gamma), depend upon the
! projection type.  (Note: if P_ALP_GD < AMISS, then the grid description is
! missing or invalid.)
!
! The following descriptions were liberally borrowed from the I/O API
! grid definition page:  http://www.baronams.com/products/ioapi/GRIDS.html
!
! lat-lon:       P_ALP_GD, P_BET_GD, and P_GAM_GD are all currently unused.
!                Coordinate units are degrees, with -180.0 < X <= 180.0,
!                -90.0 <= Y <= 90.0.  Western longitudes and southern latitudes
!                are negative.
!
! Lambert:       P_ALP_GD <= P_BET_GD are the two latitudes that
!                determine the projection cone; P_GAM_GD is the
!                central meridian.
!
! Mercator:      P_ALP_GD and P_BET_GD are the latitude and longitude
!                of the coordinate origin (within the tangent circle);
!                P_GAM_GD is the angle between the cylinder axis
!                and the North polar axis.
!
! Stereographic: P_ALP_GD is the "true latitude", the latitude at which
!                the stereographic plane is secant to the earth (or 90, if
!                the projection is a North Polar tangent stereographic).
!                P_BET_GD is the angle of rotation of the Y-axis relative
!                to the Greenwich meridian, i.e., the longitude meridian
!                which becomes the negative Y axis.  P_GAM_GD is unused.
!   
! UTM:           P_ALP_GD is the UTM zone.
!                P_BET_GD and P_GAM_GD are unused.
!
! Polar:         P_ALP_GD is 1.0 for North Polar and -1.0 for South Polar.
!                P_BET_GD is the secant latitude (latitude of true scale).
!                P_GAM_GD is the central meridian.
!
! Equatorial     P_ALP_GD is the latitude of the true scale.  P_BET_GD is
! Mercator:      unused.  P_GAM_GD is the longitude of the central meridian.
!
! Transverse     P_ALP_GD is the latitude of the true scale.  P_BET_GD is
! Mercator:      unused.  P_GAM_GD is the longitude of the central meridian.
!   
!-------------------------------------------------------------------------------

  p_alp_gd   =  DBLE(met_p_alp_d)   ! [degrees]
  p_bet_gd   =  DBLE(met_p_bet_d)   ! [degrees]
  p_gam_gd   =  DBLE(met_p_gam_d)   ! [degrees]

!-------------------------------------------------------------------------------
! (XCENT_GD, YCENT_GD):
! For most projections, these are the longitude, -180 < X <= 180, and the
!   latitude, -90 <= Y <= 90, for the center of the grid's respective Cartesian
!   coordinate system.  Units are meters.
! For UTM:  UTM coordinates of the origin for offset UTM coordinates (or are
!           (0,0) for Equator-based UTM coordinates).
! For Lat-Lon:  unused
!-------------------------------------------------------------------------------

  ! For WRF, center latitude is calculated from other geographic information,
  ! and it can be imprecise.  Adjust center latitude by assuming that user
  ! will not define center of projection at finer resolution than 1/4 degree.

  IF ( met_model == 2 ) THEN  ! WRF
    div = MODULO(met_y_centd, 1.0)
    div = REAL (NINT ( (div * 100.0) / 25.0) ) * 0.25
    met_y_centd = REAL(INT(met_y_centd)) + div
  ENDIF

  xcent_gd   =  DBLE(met_x_centd)   ! [degrees longitude]
  ycent_gd   =  DBLE(met_y_centd)   ! [degrees latitude]

!-------------------------------------------------------------------------------
! (XCELL_GD, YCELL_GD):
! The X-direction and Y-direction cell dimensions (m) for a regular grid
! If zero, the grid is assumed irregular and described by other means (e.g.
! a grid-geometry file).
!-------------------------------------------------------------------------------

  xcell_gd   =  DBLE(met_resoln)  ! [m]
  ycell_gd   =  DBLE(met_resoln)  ! [m]

!-------------------------------------------------------------------------------
! VGTYP_GD:
! The vertical grid type:
!   1: VGSGPH3 for hydrostatic sigma-P coordinates
!   2: VGSGPN3 for non-hydrostatic sigma-P0 coordinates
!   3: VGSIGZ3 for sigma-Z coordinates
!   4: VGPRES3 for pressure (mb) coordinates
!   5: VGZVAL3 for Z (meters above mean sea level)
!   6: VHZVAL3 for H (meters above ground)
!   7: VGWRFEM for WRF mass-core sigma
!   8: VGWRFNM for WRF NMM
!   -: IMISS   for vertical coordinates not stored in VGLVSD
!              (e.g., temporally or spatially changing vertical coordinates)
!-------------------------------------------------------------------------------

  IF ( met_model == 1 ) THEN       ! MM5
    IF ( met_iversion == 3 ) THEN  !    v3: terrain-following non-hydrostatic
      vgtyp_gd = vgsgpn3
    ELSE                           !    v2
      IF ( met_inhyd == 1 ) THEN   !      : terrain-following non-hydrostatic
        vgtyp_gd = vgsgpn3
      ELSE                         !      : terrain-following hydrostatic
        vgtyp_gd = vgsgph3
      ENDIF
    ENDIF
  ELSE IF ( met_model == 2 ) THEN  ! WRF EM
    vgtyp_gd = vgwrfem             ! terrain-following dry hydrostatic pressure
  ENDIF

!-------------------------------------------------------------------------------
! VGTPUN_GD:
! The units of the vertical coordinate top.
!-------------------------------------------------------------------------------

  vgtpun_gd  = 'Pa'

!-------------------------------------------------------------------------------
! VGTOP_GD:
! The value for the model top used in the definition of the sigma
! coordinate systems in the VGTPUN_GD units
! For sigma-P, the relationship between pressure levels P and sigma-P is
! given by the following formula:
!    sigma-P = ( P - VGTOP_GD ) / (P_srf - VGTOP_GD ),
! where P_srf is the surface pressure.
!-------------------------------------------------------------------------------

  vgtop_gd   = met_ptop
  x3top      = met_ptop

!-------------------------------------------------------------------------------
! VGLVUN_GD:
! The units of the vertical coordinate surface values
!-------------------------------------------------------------------------------

  vglvun_gd  = 'none'

!-------------------------------------------------------------------------------
! COORDNAM_GD:
! The coordinate system name used for I/O-API description and GRIDDESC.
!-------------------------------------------------------------------------------

  coordnam_gd  = coordnam

!-------------------------------------------------------------------------------
! GDNAME_GD:
! The grid name used for I/O-API description and GRIDDESC.
!-------------------------------------------------------------------------------

  gdname_gd  = grdnam

!-------------------------------------------------------------------------------
! Check origins of output MCIP domain and met from offsets.  Take into account
! resolution of MET, MCIP, and NTHIK.
!   (X0, Y0) = (COL_OFFSET, ROW_OFFSET)
!-------------------------------------------------------------------------------

  IF ( met_model == 1 ) THEN  ! MM5

    xorig_m = ( ( met_x_11 - 0.5 * FLOAT(met_nxcoarse + 1) ) *  &
                  met_gratio ) * met_resoln

    yorig_m = ( ( met_y_11 - 0.5 * FLOAT(met_nycoarse + 1) ) *  &
                  met_gratio ) * met_resoln

  ELSE IF ( met_model == 2 ) THEN  ! WRF

    nxm = met_nx - 1
    nym = met_ny - 1

    fl = file_mm(1)

    CALL get_var_real_cdf (fl, 'LAT_LL_D', ylat, 1, 1, rcode)
    IF ( rcode /= 0 ) THEN
      IF ( .NOT. ALLOCATED (dum2d) ) ALLOCATE ( dum2d (nxm, nym) )
      CALL get_var_2d_real_cdf (fl, 'XLAT', dum2d, nxm, nym, 1, rcode)
      IF ( rcode == 0 ) THEN
        usecrslat = .TRUE.
        ylat      = dum2d(1,1)
      ELSE
        WRITE (6,9800) 'LAT_LL_D and XLAT', rcode
        GOTO 1001
      ENDIF
    ENDIF

    CALL get_var_real_cdf (fl, 'LON_LL_D', ylon, 1, 1, rcode)
    IF ( rcode /= 0 ) THEN
      IF ( .NOT. ALLOCATED (dum2d) ) ALLOCATE ( dum2d (nxm, nym) )
      CALL get_var_2d_real_cdf (fl, 'XLONG', dum2d, nxm, nym, 1, rcode)
      IF ( rcode == 0 ) THEN
        usecrslon = .TRUE.
        ylon      = dum2d(1,1)
      ELSE
        WRITE (6,9800) 'LON_LL_D', rcode
        GOTO 1001
      ENDIF
    ENDIF

    CALL xxyy (met_y_centd, met_x_centd, xcntr,   ycntr,   met_x_centd,  &
               met_y_centd, met_mapproj, met_cone_fac, met_tru1, pole)

    CALL xxyy (ylat,        ylon,        xorig_m, yorig_m, met_x_centd,  &
               met_y_centd, met_mapproj, met_cone_fac, met_tru1, pole)

    ! If dot-point coordinates were not in WRF output file, adjust XORIG_M
    ! and YORIG_M by moving points 0.5*DX and/or 0.5*DY away from center.

    IF ( usecrslat ) THEN
      xorig_m = xorig_m - 0.5 * met_resoln/1000.0
    ENDIF

    IF ( usecrslon ) THEN
      yorig_m = yorig_m - 0.5 * met_resoln/1000.0
    ENDIF

    ! Since XORIG and YORIG are in increments of the horizontal grid spacing
    ! from the center of the projection, ensure that XORIG and YORIG are
    ! correctly rounded.  Note that the center of the projection can be on
    ! dot points, scalar points, or face points.

    IF ( MOD (CEILING(xorig_m), NINT(met_resoln/2000.0)) == 0 ) THEN
      xorig_m = REAL ( CEILING(xorig_m) ) * 1000.0
    ELSE IF ( MOD (FLOOR(xorig_m), NINT(met_resoln/2000.0)) == 0 ) THEN
      xorig_m = REAL ( FLOOR(xorig_m) ) * 1000.0
    ELSE
      WRITE (6,9700) 'XORIG', xorig_m
      GOTO 1001
    ENDIF

    IF ( MOD (CEILING(yorig_m-ycntr), NINT(met_resoln/2000.0)) == 0 ) THEN
      yorig_m = REAL ( CEILING(yorig_m-ycntr) ) * 1000.0
    ELSE IF ( MOD (FLOOR(yorig_m-ycntr), NINT(met_resoln/2000.0)) == 0 ) THEN
      yorig_m = REAL ( FLOOR(yorig_m-ycntr) ) * 1000.0
    ELSE
      WRITE (6,9700) 'YORIG', yorig_m-ycntr
      GOTO 1001
    ENDIF

    IF ( ALLOCATED ( dum2d ) ) DEALLOCATE ( dum2d )

  ENDIF

  xorig_x = xorig_m + FLOAT(x0-nthik) * xcell_gd
  yorig_x = yorig_m + FLOAT(y0-nthik) * ycell_gd

  rnthik = FLOAT(nthik)

  xorig_ctm = xorig_x + rnthik * xcell_gd
  yorig_ctm = yorig_x + rnthik * ycell_gd

!-------------------------------------------------------------------------------
! (XORIG_GD, YORIG_GD):
! For Lambert, Mercator, Stereographic, and UTM these are the
!     location in map units (Km) of the origin cell (1,1) (lower left corner)
!     of the of the horizontal grid measured from (XCENT_GD, YCENT_GD).
! For Lat-Lon: units are degrees - unused
! *** Note: This function assumes that the origin of the grid can be measured
!           in whole kilometers.
!-------------------------------------------------------------------------------

  xorig_gd   = DBLE(NINT(xorig_ctm))  ! X-origin [m]
  yorig_gd   = DBLE(NINT(yorig_ctm))  ! Y-origin [m]

!-------------------------------------------------------------------------------
! Check user-defined MCIP output time info against input meteorology.
!-------------------------------------------------------------------------------

  IF ( intvl < NINT(met_tapfrq) ) THEN
    WRITE (6,9300) intvl, met_tapfrq
    GOTO 1001
  ENDIF

  IF ( mcip_start < met_startdate ) THEN
    WRITE (6,9400) mcip_start, met_startdate
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Set up output variables based on input meteorology.
!-------------------------------------------------------------------------------

  t2out = ift2m

!-------------------------------------------------------------------------------
! Set up coordinates for diagnostic print on all domains.
!-------------------------------------------------------------------------------

  IF ( ( lprt_col > ncols ) .OR. ( lprt_row > nrows ) .OR.  &
       ( lprt_col < 0     ) .OR. ( lprt_row < 0     ) ) THEN
    WRITE (6,9600) lprt_col, lprt_row, ncols, nrows
    GOTO 1001
  ENDIF

  IF ( lprt_col == 0 ) lprt_col = ncols / 2
  IF ( lprt_row == 0 ) lprt_row = nrows / 2

  lprt_xcol = lprt_col + nthik
  lprt_xrow = lprt_row + nthik

  lprt_metx = lprt_col + x0
  lprt_mety = lprt_row + y0

!-------------------------------------------------------------------------------
! Echo user options and grid definitions to log file.
!-------------------------------------------------------------------------------

  WRITE (*, "(/, 1x, 78('-'), /)")
  WRITE (*, "(24x, a, /)") 'USER OPTIONS AND GRID DEFINITIONS'

  WRITE (*,6000) mcip_start, mcip_end, intvl

  SELECT CASE ( lpbl )
    CASE ( 1 )
      option = 'Using PBL parameters from input meteorology'
    CASE ( 2 )
      option = 'Re-calculating PBL parameters using PBL similarity'
    CASE ( 3 )
      option = 'Re-calculating PBL parameters using surface similarity'
    CASE DEFAULT
      option = '*** invalid ***'
  END SELECT
  WRITE (*,6100) 'LPBL   ', lpbl, TRIM(option)

  SELECT CASE ( lrad )
    CASE ( 1 )
      option = 'Using radiation fields from input meteorology'
    CASE ( 2 )
      option = 'Re-calculating radiation fields using MCIPv1 algorithm'
    CASE DEFAULT
      option = '*** invalid ***'
  END SELECT
  WRITE (*,6100) 'LRAD   ', lrad, TRIM(option)
  
  SELECT CASE ( lddep )
    CASE ( 1 )
      option = 'Using RADM (Wesely) dry deposition'
    CASE ( 2 )
      option = 'Using Models-3 (Pleim) dry deposition'
    CASE ( 3 )
      option = 'Using Models-3 dry deposition with chlorine'
    CASE ( 4 )
      option = 'Using Models-3 dry deposition with chlorine and mercury'
    CASE DEFAULT
      option = '*** invalid ***'
  END SELECT
  WRITE (*,6100) 'LDDEP  ', lddep, TRIM(option)

  SELECT CASE ( lcld )
    CASE ( 1 )
      option = 'Calculating cloud fields'
    CASE DEFAULT
      option = '*** invalid ***'
  END SELECT
  WRITE (*,6100) 'LCLD   ', lcld, TRIM(option)

  SELECT CASE ( lhydout )
    CASE ( 1 )
      option = 'Output fields will be non-hydrostatic'
    CASE ( 2 )
      option = 'Output fields will be hydrostatic'
    CASE DEFAULT
      option = '*** invalid ***'
  END SELECT
  WRITE (*,6100) 'LHYDOUT', lhydout, TRIM(option)

  WRITE (*,'(/)')
  WRITE (*,6200) 'Met   ', metcol,  metrow,  metlay
  WRITE (*,6200) 'MCIP X', ncols_x, nrows_x, metlay
  WRITE (*,6200) 'Output', ncols,   nrows,   nlays
  WRITE (*,'(/)')

  WRITE (*,*) 'Output grid resolution: ', xcell_gd / 1000.0,  ' km'
  WRITE (*,*) 'Window domain origin on met domain (col,row):     ',  &
              x0, ', ', y0
  WRITE (*,*) 'Window domain far corner on met domain (col,row): ',  &
              x0 + ncg_x, ', ', y0 + ncg_y

  WRITE (*,'(/)')
  WRITE (*,"(' Cells and points for diagnostic prints')")
  WRITE (*,6300) 'LPRT_COL ', 'LPRT_ROW ', lprt_col,  lprt_row
  WRITE (*,6300) 'LPRT_XCOL', 'LPRT_XROW', lprt_xcol, lprt_xrow
  WRITE (*,6300) 'LPRT_METX', 'LPRT_METY', lprt_metx, lprt_mety

  WRITE (*,'(/)')
  WRITE (*,"(' IOAPI header variables:')")

  WRITE (*,6400) 'GDTYP3D', gdtyp_gd
  WRITE (*,6500) 'GDNAM3D', gdname_gd
  WRITE (*,6600) 'P_ALP3D', p_alp_gd
  WRITE (*,6600) 'P_BET3D', p_bet_gd
  WRITE (*,6600) 'P_GAM3D', p_gam_gd
  WRITE (*,6600) 'XCENT3D', xcent_gd
  WRITE (*,6600) 'YCENT3D', ycent_gd
  WRITE (*,6600) 'XORIG3D', xorig_gd
  WRITE (*,6600) 'YORIG3D', yorig_gd
  WRITE (*,6600) 'XCELL3D', xcell_gd
  WRITE (*,6600) 'YCELL3D', ycell_gd
  WRITE (*,6400) 'VGTYP3D', vgtyp_gd
  WRITE (*,6600) 'VGTOP3D', vgtop_gd

  RETURN

!-------------------------------------------------------------------------------
! Format statements.
!-------------------------------------------------------------------------------

 6000 FORMAT (/, 1x, 'Output start date = ', a,  &
              /, 1x, 'Output end date   = ', a,  &
              /, 1x, 'Output interval   = ', i3, ' minutes', // )
 6100 FORMAT (1x, a, ' = ', i3, ':  ', a)
 6200 FORMAT (1x, a, ' domain dimensions (col, row, lay):', 3(2x, i3))
 6300 FORMAT (4x, a, 1x, a, 3x, i4, 2x, i4)
 6400 FORMAT (4x, a, 2x, i10)
 6500 FORMAT (4x, a, 2x, a)
 6600 FORMAT (4x, a, 2x, f10.1)

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   MCIP lower-left corner is not in met domain',     &
              /, 1x, '***   X0, Y0 = ', 2(2x, i4),                            &
              /, 1x, '***   NX, NY = ', 2(2x, i4),                            &
              /, 1x, 70('*'))

 9050 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   MCIP domain must have 1 or more cells per side',  &
              /, 1x, '***   NCOLS, NROWS = ', 2(2x, i4),                      &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   Requested MCIP X domain exceeds met domain',      &
              /, 1x, '***   METCOL, METROW = ', i4, 2x, i4,                   &
              /, 1x, '***   MCIP domain (col, row) = ', i4, 2x, i4,           &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   Requested MCIP dim. exceed the actual Met. dim.', &
              /, 1x, '***   Requested MCIP dim.: ', i4, ' x ', i4,            &
              /, 1x, '***   Met. dim.: ', i4, ' x ', i4,                      &
              /, 1x, '***   Max. allowable dim.: ', i4, ' x ', i4,            &
              /, 1x, 70('*'))

 9250 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   Requested MCIP dim. exceed the actual Met. dim.', &
              /, 1x, '***   Input meteorology dimensions: ', 2(2x, i4),       &
              /, 1x, '***   MCIP output domain in terms of met: ', 2(2x, i4), &
              /, 1x, 70('*'))

 9275 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   INVALID INPUT METEOROLOGY MAP PROJECTION ', i4,   &
              /, 1x, 70('*'))

 9300 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '      Requested MCIP output interval cannot be shorter',&
              /, 1x, '        than input meteorology',                        &
              /, 1x, '      User-defined MCIP output interval = ', i3,        &
              /, 1x, '      Meteorology output interval       = ', f5.1,      &
              /, 1x, 70('*'))

 9400 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   Requested MCIP start date is earlier than input ',&
              /, 1x, '***     meteorology start time',                        &
              /, 1x, '***   User-defined MCIP start date = ', a,              &
              /, 1x, '***   Input meteorology start date = ', a,              &
              /, 1x, 70('*'))

 9600 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   Diagnostic print cell is outside domain',         &
              /, 1x, '***   Input LPRT_COL and LPRT_ROW are ', i4, 2x, i4,    &
              /, 1x, '***   Output domain NCOLS and NROWS are ', i4, 2x, i4,  &
              /, 1x, 70('*'))

 9700 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   Trouble calculating origin from WRF input',       &
              /, 1x, '***   ', a, ' = ', f11.4,                               &
              /, 1x, 70('*'))

 9800 FORMAT (/, 1x, 70('*'),                                                 &
              /, 1x, '*** SUBROUTINE: SETGRIDDEFS',                           &
              /, 1x, '***   ERROR RETRIEVING VARIABLE FROM WRF FILE',         &
              /, 1x, '***   VARIABLE = ', a,                                  &
              /, 1x, '***   RCODE = ', i3,                                    &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE setgriddefs
