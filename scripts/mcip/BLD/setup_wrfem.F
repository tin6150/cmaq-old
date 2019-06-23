
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

SUBROUTINE setup_wrfem (ctmlays)

!-------------------------------------------------------------------------------
! Name:     Set Up the WRF Domain Attributes
! Purpose:  Establishes bounds for WRF post-processing.
! Revised:  ?? Jun 2004  Modified from MCIP2.2 for WRF. (S.-B. Kim)
!           26 May 2005  Changed vertical dimension to reflect full-layer
!                        dimension in WRFv2 header.  Added dynamic calculation
!                        of MET_TAPFRQ.  Converted dimensions to X,Y as opposed
!                        to the (former) convention that aligned with MM5.
!                        Included updates from MCIPv2.3.  Added calculation of
!                        cone factor.  Added logic for moist species, 2-m
!                        temperature, and 10-m winds.  Added definitions for
!                        WRF base state variables.  Added capability to use all
!                        WRF layers for MCIP without defining a priori.
!                        Cleaned up code.  (T. Otte)
!           15 Jul 2005  Added debugging on variable retrievals.  Changed check
!                        on 3D mixing ratios from rain to ice.  Corrected RADM
!                        seasons for Southern Hemisphere.  Corrected variable
!                        name for retrieval of surface physics option. (T. Otte)
!           18 Aug 2005  Changed internal variable SIGN to FAC to avoid
!                        confusion with F90 intrinsic function.  (T. Otte)
!-------------------------------------------------------------------------------

  USE metinfo
  USE date_pack
  USE mcipparm
  USE file
  USE parms3, ONLY: badval3
  USE wrf_netcdf
  USE const, ONLY: pi180

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER                      :: cdfid
  REAL,          INTENT(INOUT) :: ctmlays   ( maxlays )
  CHARACTER*19                 :: date_init
  REAL                         :: dx
  REAL                         :: dy
  REAL                         :: fac
  CHARACTER*256                :: fl
  CHARACTER*256                :: fl2
  INTEGER                      :: idtsec
  LOGICAL                      :: ifu10m
  LOGICAL                      :: ifv10m
  INTEGER                      :: ival
  INTEGER,       PARAMETER     :: max_times = 150
  CHARACTER*2                  :: month
  INTEGER                      :: n_times
  CHARACTER*16,  PARAMETER     :: pname     = 'SETUP_WRFEM'
  INTEGER                      :: rcode
  CHARACTER*80                 :: times     ( max_times )
  CHARACTER*80                 :: times2    ( max_times )

!-------------------------------------------------------------------------------
! Extract NX, NY, and NZ.
!-------------------------------------------------------------------------------

  WRITE (6,6000)

  fl = file_mm(1)

  CALL get_gl_att_int_cdf  (fl, 'WEST-EAST_GRID_DIMENSION',   met_nx, rcode)
  IF ( rcode /= 0 ) THEN
    WRITE (6,9400) 'WEST-EAST_GRID_DIMENSION', rcode
    GOTO 1001
  ENDIF

  CALL get_gl_att_int_cdf  (fl, 'SOUTH-NORTH_GRID_DIMENSION', met_ny, rcode)
  IF ( rcode /= 0 ) THEN
    WRITE (6,9400) 'SOUTH-NORTH_GRID_DIMENSION', rcode
    GOTO 1001
  ENDIF

  CALL get_gl_att_int_cdf  (fl, 'BOTTOM-TOP_GRID_DIMENSION',  ival,   rcode)
  IF ( rcode /= 0 ) THEN
    WRITE (6,9400) 'BOTTOM-TOP_GRID_DIMENSION', rcode
    GOTO 1001
  ELSE
    met_nz = ival - 1
  ENDIF

  WRITE (6,6100) met_nx, met_ny, met_nz

!-------------------------------------------------------------------------------
! If layer structure was not defined in user namelist, use WRF layers.
!-------------------------------------------------------------------------------

  IF ( needlayers ) THEN
    nlays = met_nz
    CALL get_var_1d_real_cdf (fl, 'ZNW', ctmlays(1:nlays+1), nlays+1, 1, rcode)
    IF ( rcode /= 0 ) THEN
      WRITE (6,9400) 'ZNW', rcode
      GOTO 1001
    ENDIF
  ENDIF

!-------------------------------------------------------------------------------
! Extract domain attributes.
!-------------------------------------------------------------------------------

  CALL get_gl_att_real_cdf (fl, 'DX', dx, rcode)
  IF ( rcode /= 0 ) THEN
    WRITE (6,9400) 'DX', rcode
    GOTO 1001
  ENDIF

  CALL get_gl_att_real_cdf (fl, 'DY', dy, rcode)
  IF ( rcode /= 0 ) THEN
    WRITE (6,9400) 'DY', rcode
    GOTO 1001
  ENDIF

  IF (dx == dy) THEN
    met_resoln = dx
  ELSE
    GOTO 8000
  ENDIF

  met_nxcoarse = met_nx 
  met_nycoarse = met_ny
  met_gratio   = 1
  met_x_11     = 1
  met_y_11     = 1

  CALL get_gl_att_int_cdf  (fl, 'MAP_PROJ',     met_mapproj, rcode)
  IF ( rcode /= 0 ) THEN
    WRITE (6,9400) 'MAP_PROJ', rcode
    GOTO 1001
  ENDIF

  CALL get_gl_att_real_cdf (fl, 'STAND_LON',    met_x_centd, rcode)
  IF ( rcode /= 0 ) THEN
    WRITE (6,9400) 'STAND_LON', rcode
    GOTO 1001
  ENDIF

  CALL get_gl_att_real_cdf (fl, 'MOAD_CEN_LAT', met_y_centd, rcode)
  IF ( rcode /= 0 ) THEN
    WRITE (6,9400) 'MOAD_CEN_LAT', rcode
    GOTO 1001
  ENDIF

  CALL get_gl_att_real_cdf (fl, 'TRUELAT1',     met_tru1,    rcode)
  IF ( rcode /= 0 ) THEN
    WRITE (6,9400) 'TRUELAT1', rcode
    GOTO 1001
  ENDIF

  CALL get_gl_att_real_cdf (fl, 'TRUELAT2',     met_tru2,    rcode)
  IF ( rcode /= 0 ) THEN
    WRITE (6,9400) 'TRUELAT2', rcode
    GOTO 1001
  ENDIF

  SELECT CASE ( met_mapproj )
    
    CASE (1)  ! Lambert conformal 
      met_p_alp_d  = MIN(met_tru1, met_tru2)  ! true latitude 1  [degrees]
      met_p_bet_d  = MAX(met_tru1, met_tru2)  ! true latitude 2  [degrees]
      met_p_gam_d  = met_x_centd              ! central meridian [degrees]
      IF ( met_y_centd < 0.0 ) THEN
        fac = -1.0  ! Southern Hemisphere
      ELSE
        fac =  1.0  ! Northern Hemisphere
      ENDIF
      IF ( ABS(met_tru1 - met_tru2) > 1.0e-1 ) THEN
        met_cone_fac = ALOG10(COS(met_tru1 * pi180)) -  &
                       ALOG10(COS(met_tru2 * pi180))
        met_cone_fac = met_cone_fac /                                      &
                       ( ALOG10(TAN((45.0 - fac*met_tru1/2.0) * pi180)) -  &
                         ALOG10(TAN((45.0 - fac*met_tru2/2.0) * pi180)) )
      ELSE
        met_cone_fac = fac * SIN(met_tru1*pi180)
      ENDIF
    
    CASE (2)  ! polar stereographic
      met_p_alp_d  = met_tru1                 ! true latitude    [degrees]
      met_p_bet_d  = met_x_centd              ! central meridian [degrees]
      met_p_gam_d  = 0.0                      ! unused
      met_cone_fac = 1.0                      ! cone factor
    
    CASE (3)  ! Mercator
      met_p_alp_d  = 0.0                      ! latitude of true scale [degrees]
      met_p_bet_d  = badval3                  ! unused
      met_p_gam_d  = met_x_centd              ! central meridian [degrees]
      met_cone_fac = 0.0                      ! cone factor
    
    CASE DEFAULT
      met_p_bet_d  = badval3                  ! missing
      met_p_alp_d  = badval3                  ! missing
      met_p_gam_d  = badval3                  ! missing
      met_cone_fac = badval3                  ! missing
  
  END SELECT

!-------------------------------------------------------------------------------
! Extract model run options.
!-------------------------------------------------------------------------------

  CALL get_gl_att_int_cdf (fl, 'ISWATER',            met_lu_water,   rcode)
  IF ( rcode /= 0 ) THEN
    WRITE (6,9400) 'ISWATER', rcode
    GOTO 1001
  ENDIF
  IF ( met_lu_water /= 16 ) GOTO 8100

  CALL get_gl_att_int_cdf (fl, 'RA_LW_PHYSICS',      met_radiation,  rcode)
  IF ( rcode /= 0 ) THEN
    WRITE (6,9400) 'RA_LW_PHYSICS', rcode
    GOTO 1001
  ENDIF

  CALL get_gl_att_int_cdf (fl, 'CU_PHYSICS',         met_cumulus,    rcode)
  IF ( rcode /= 0 ) THEN
    WRITE (6,9400) 'CU_PHYSICS', rcode
    GOTO 1001
  ENDIF

  CALL get_gl_att_int_cdf (fl, 'MP_PHYSICS',         met_expl_moist, rcode)
  IF ( rcode /= 0 ) THEN
    WRITE (6,9400) 'MP_PHYSICS', rcode
    GOTO 1001
  ENDIF

  CALL get_gl_att_int_cdf (fl, 'BL_PBL_PHYSICS',     met_pbl,        rcode)
  IF ( rcode /= 0 ) THEN
    WRITE (6,9400) 'BL_PBL_PHYSICS', rcode
    GOTO 1001
  ENDIF

  CALL get_gl_att_int_cdf (fl, 'SF_SURFACE_PHYSICS', met_soil_lsm,   rcode)
  IF ( rcode /= 0 ) THEN
    WRITE (6,9400) 'SF_SURFACE_PHYSICS', rcode
    GOTO 1001
  ENDIF

  met_snow_opt = 1  ! not used for WRF yet
  met_restart  = 0  ! not used for WRF yet

!-------------------------------------------------------------------------------
! Extract WRF start date and time information.
!-------------------------------------------------------------------------------

  CALL get_gl_att_text_cdf (fl, 'START_DATE', date_init, rcode)
  IF ( rcode /= 0 ) THEN
    WRITE (6,9400) 'START_DATE', rcode
    GOTO 1001
  ENDIF
  met_startdate =  date_init(1:19) // '.0000'
  met_startdate(11:11) = "-"  ! change from "_" to "-" for consistency

  CALL get_times_cdf (fl, times, n_times, max_times, rcode)
  IF ( rcode /= 0 ) THEN
    WRITE (6,9400) 'TIMES', rcode
    GOTO 1001
  ENDIF
  IF ( n_times > 1 ) THEN
    CALL geth_idts (times(2)(1:19), times(1)(1:19), idtsec)
  ELSE
    fl2 = file_mm(2)
    IF ( fl2(1:10) == '          ' ) THEN
      WRITE (6,9500)
      idtsec = 60.0
    ELSE
      CALL get_times_cdf (fl2, times2, n_times, max_times, rcode)
      IF ( rcode == 0 ) THEN
        CALL geth_idts (times2(1)(1:19), times(1)(1:19), idtsec)
      ELSE
        WRITE (6,9400) 'TIMES2', rcode
        GOTO 1001
      ENDIF
    ENDIF
  ENDIF
  met_tapfrq = REAL(idtsec / 60)  ! convert sec --> min

!-------------------------------------------------------------------------------
! Determine MCIP season for RADM dry deposition.  MCIP season 4 is a snow-
! covered windter, and it is determined in radmdry.F.
!
! Northern Hemisphere:
!   Winter = Dec., Jan., Feb.  (ISESN = 3)
!   Spring = Mar., Apr., May   (ISESN = 5)
!   Summer = Jun., Jul., Aug.  (ISESN = 1)
!   Autumn = Sep., Oct., Nov.  (ISESN = 2)
!
! Southern Hemisphere:
!   Winter = Jun., Jul., Aug.  (ISESN = 3)
!   Spring = Sep., Oct., Nov.  (ISESN = 5)
!   Summer = Dec., Jan., Feb.  (ISESN = 1)
!   Autumn = Mar., Apr., May   (ISESN = 2)
!-------------------------------------------------------------------------------

  month = met_startdate(6:7)

  IF ( met_y_centd >= 0.0 ) THEN  ! Northern Hemisphere

  IF (( month == "12" ) .OR. ( month == "01" ) .OR. ( month == "02" )) THEN
    isesn = 3  ! winter
  ELSE IF (( month == "03" ) .OR. ( month == "04" ) .OR. ( month == "05" )) THEN
    isesn = 5  ! spring
  ELSE IF (( month == "06" ) .OR. ( month == "07" ) .OR. ( month == "08" )) THEN
    isesn = 1  ! summer
  ELSE IF (( month == "09" ) .OR. ( month == "10" ) .OR. ( month == "11" )) THEN
    isesn = 2  ! autumn
  ELSE
    GOTO 8200
  ENDIF

  ELSE  ! Southern Hemisphere

  IF (( month == "06" ) .OR. ( month == "07" ) .OR. ( month == "08" )) THEN
    isesn = 3  ! winter
  ELSE IF (( month == "09" ) .OR. ( month == "10" ) .OR. ( month == "11" )) THEN
    isesn = 5  ! spring
  ELSE IF (( month == "12" ) .OR. ( month == "01" ) .OR. ( month == "02" )) THEN
    isesn = 1  ! summer
  ELSE IF (( month == "03" ) .OR. ( month == "04" ) .OR. ( month == "05" )) THEN
    isesn = 2  ! autumn
  ELSE
    GOTO 8200
  ENDIF

  ENDIF

!-------------------------------------------------------------------------------
! Set variables for non-hydrostatic base state.  There is no option for
! hydrostatic run in WRF.  The base state variables are not currently output
! (as of WRF 2.0.3.1), so fill in "default" values from WRF namelist.
!-------------------------------------------------------------------------------

  CALL get_var_real_cdf (fl, 'P_TOP', met_ptop, 1, 1, rcode)
  IF ( rcode /= 0 ) THEN
    WRITE (6,9400) 'P_TOP', rcode
    GOTO 1001
  ENDIF

  met_inhyd = 1        ! non-hydrostatic simulation
  met_p00   = 100000.0 ! base state sea-level pressure [Pa]
  met_ts0   =    290.0 ! base state sea-level temperature [K]
  met_tlp   =     50.0 ! base state lapse rate d(T)/d(ln P) from 1000 to 300 mb
  met_tiso  = badval3  ! base state stratospheric isothermal T [K]  ! not used

!-------------------------------------------------------------------------------
! For now, set the fractional land use flag to false since fractional land
! use is not in WRF output.
!-------------------------------------------------------------------------------

  iflufrc = .FALSE.

!-------------------------------------------------------------------------------
! Determine whether or not the 2-m temperature and 10-m wind components are in
! the output, and set the flags appropriately.
!-------------------------------------------------------------------------------

  cdfid = ncopn (fl, ncnowrit, rcode)
  rcode = nf_inq_varid (cdfid, 'T2', rcode)
  IF ( rcode == 0 ) THEN
    ift2m = .TRUE.  ! 2-m temperature is in the file
  ELSE
    ift2m = .FALSE. ! 2-m temperature is not in the file
  ENDIF
  CALL ncclos (cdfid, rcode)

  cdfid = ncopn (fl, ncnowrit, rcode)
  rcode = nf_inq_varid (cdfid, 'U10', rcode)
  IF ( rcode == 0 ) THEN
    ifu10m = .TRUE.  ! 10-m u-component wind is in the file
  ELSE
    ifu10m = .FALSE. ! 10-m u-component wind is not in the file
  ENDIF
  CALL ncclos (cdfid, rcode)

  cdfid = ncopn (fl, ncnowrit, rcode)
  rcode = nf_inq_varid (cdfid, 'V10', rcode)
  IF ( rcode == 0 ) THEN
    ifv10m = .TRUE.  ! 10-m v-component wind is in the file
  ELSE
    ifv10m = .FALSE. ! 10-m v-component wind is not in the file
  ENDIF
  CALL ncclos (cdfid, rcode)

  IF ( ( ifu10m ) .AND. ( ifv10m ) ) THEN
    ifw10m = .TRUE.
  ELSE
    ifw10m = .FALSE.
  ENDIF

!-------------------------------------------------------------------------------
! Determine the number of 3D cloud moisture species.  Assume that cloud water
! mixing ratio and cloud rain water mixing ratio are part of the output.
! Also assume that cloud ice mixing ratio and cloud snow mixing ratio will
! occur together, but check for availability.  Check for graupel, as well.
!-------------------------------------------------------------------------------

  nqspecies = 2  ! assume QCLOUD and QRAIN are already there

  cdfid = ncopn (fl, ncnowrit, rcode)
  rcode = nf_inq_varid (cdfid, 'QICE', rcode)
  IF ( rcode == 0 ) THEN
    nqspecies = nqspecies + 1  ! QICE is in the file
  ENDIF
  CALL ncclos (cdfid, rcode)

  cdfid = ncopn (fl, ncnowrit, rcode)
  rcode = nf_inq_varid (cdfid, 'QSNOW', rcode)
  IF ( rcode == 0 ) THEN
    nqspecies = nqspecies + 1  ! QSNOW is in the file
  ENDIF
  CALL ncclos (cdfid, rcode)

  IF ( nqspecies == 3 ) GOTO 8300  ! not set up for QI w/o QS or vice versa

  cdfid = ncopn (fl, ncnowrit, rcode)
  rcode = nf_inq_varid (cdfid, 'QGRAUP', rcode)
  IF ( rcode == 0 ) THEN
    nqspecies = nqspecies + 1  ! QGRAUP is in the file
  ENDIF
  CALL ncclos (cdfid, rcode)

  IF ( nqspecies == 3 ) GOTO 8300  ! not set up for QG without QI and QS

  RETURN

!-------------------------------------------------------------------------------
! Format statements.
!-------------------------------------------------------------------------------

 6000 FORMAT (/, 1x, '- SUBROUTINE SETUP_WRFEM - READING WRF HEADER')
 6100 FORMAT (3x, 'WRF GRID DIMENSIONS (X,Y,Z) ', i4, 1x, i4, 1x, i3, //)

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8000 WRITE (6,9000) dx, dy
      GOTO 1001

 8100 WRITE (6,9100)
      GOTO 1001

 8200 WRITE (6,9200) month
      GOTO 1001

 8300 WRITE (6,9300)
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_WRFEM',                        &
              /, 1x, '***   MISMATCH IN DX AND DY',                        &
              /, 1x, '***   DX, DY = ', 2(f7.2),                           &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_WRFEM',                        &
              /, 1x, '***   MUST USE USGS 24-CATEGORY LAND USE WITH MCIP', &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_WRFEM',                        &
              /, 1x, '***   INVALID MONTH IN WRF DATE',                    &
              /, 1x, '***   MONTH IS ', i4,                                &
              /, 1x, 70('*'))

 9300 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_WRFEM',                        &
              /, 1x, '***   NQSPECIES SET AT 3',                           &
              /, 1x, '***   MCIP NEEDS TO BE MODIFIED FOR THIS CASE',      &
              /, 1x, 70('*'))

 9400 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_WRFEM',                        &
              /, 1x, '***   ERROR RETRIEVING VARIABLE FROM WRF FILE',      &
              /, 1x, '***   VARIABLE = ', a,                               &
              /, 1x, '***   RCODE = ', i3,                                 &
              /, 1x, 70('*'))

 9500 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_WRFEM',                        &
              /, 1x, '***   ERROR SETTING OUTPUT FREQUENCY FROM WRF FILE', &
              /, 1x, '***   ONLY FOUND ONE FILE WITH ONE TIME PERIOD',     &
              /, 1x, '***   SETTING OUTPUT FREQUENCY TO 1 MINUTE',         &
              /, 1x, 70('*'))


 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE setup_wrfem
