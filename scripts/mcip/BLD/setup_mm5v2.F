
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
! $Header: /project/work/rep/MCIP2/src/mcip2/setup_mm5v2.F,v 1.1 2005/08/29 14:42:36 tlotte Exp $ 


SUBROUTINE setup_mm5v2 (ctmlays)

!-------------------------------------------------------------------------------
! Name:     Set Up the MM5 Domain Attributes from MM5 Version 2
! Purpose:  Establishes bounds for MM5 post-processing.
! Revised:  17 Sep 2001  Original version.  (T. Otte)
!           11 Oct 2001  Corrected definitions of MET_P_ALP_D, MET_P_BET_D, and
!                        MET_P_GAM_D based on map projection.  (T. Otte)
!           23 Jan 2002  Changed calls to "abort" to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  Changed missing
!                        value on MET_P_ALP_D, MET_P_BET_D, and MET_P_GAM_D
!                        to BADVAL3.  (T. Otte)
!           09 Aug 2004  Corrected header settings for polar stereographic
!                        and Mercator projections.  Modified code so that
!                        arrays are made available in output only if user
!                        options in MM5 generate those data.  Added definition
!                        for MET_SNOW_OPT, cone factor, and true latitude 1.
!                        Removed unused variables MET_SDATE and MET_STIME.
!                        Added IFT2M.  (T. Otte)
!           29 Nov 2004  Added IFLUFRC for fractional land use fields, and
!                        set variable to "false" since MM5v2 format has not
!                        been modified for this feature.  Added NUMMETLU
!                        to include the number of land use categories in
!                        incoming meteorology data.  (T. Otte)
!           26 May 2005  Changed I and J variable names to Y and X to make
!                        code more general.  Changed subroutine name from
!                        SETUPV2 to SETUP_MM5V2.  Removed unused variables
!                        MET_IEXPAND, MET_IOFFSET,  and MET_JOFFSET.  Added
!                        MET_TRU2 and IFW10M.  Added capability to use all MM5
!                        layers for MCIP without defining a priori.  (T. Otte)
!           15 Jul 2005  Corrected RADM seasons for Southern Hemisphere.
!                        (T. Otte)
!           18 Aug 2005  Changed internal variable INDEX to IDX to avoid
!                        confusion with F90 intrinsic function.  (T. Otte)
!-------------------------------------------------------------------------------

  USE file
  USE metinfo
  USE mcipparm
  USE parms3, ONLY: badval3

  IMPLICIT NONE

  REAL,          INTENT(INOUT) :: ctmlays   ( maxlays )
  INTEGER                      :: idx
  INTEGER                      :: istat
  INTEGER                      :: k
  INTEGER                      :: kk
  INTEGER                      :: km1
  INTEGER                      :: mdate
  INTEGER,       ALLOCATABLE   :: mif       ( : , : )
  CHARACTER*80,  ALLOCATABLE   :: mifc      ( : , : )
  CHARACTER*2                  :: month
  REAL,          ALLOCATABLE   :: mrf       ( : , : )
  CHARACTER*80,  ALLOCATABLE   :: mrfc      ( : , : )
  INTEGER,       PARAMETER     :: numprogs  = 20
  INTEGER,       PARAMETER     :: numvals   = 1000
  CHARACTER*16,  PARAMETER     :: pname     = 'SETUP_MM5V2'
  REAL,          ALLOCATABLE   :: temp1d    ( : )

  INTERFACE

    SUBROUTINE getgist (hdr_int, hdr_real, hdr_int_desc, hdr_real_desc)
      IMPLICIT NONE
      INTEGER,       INTENT(IN)    :: hdr_int       ( : , : )
      CHARACTER*80,  INTENT(IN)    :: hdr_int_desc  ( : , : )
      REAL,          INTENT(IN)    :: hdr_real      ( : , : )
      CHARACTER*80,  INTENT(IN)    :: hdr_real_desc ( : , : )
    END SUBROUTINE getgist

  END INTERFACE

!-------------------------------------------------------------------------------
! Allocate necessary arrays.
!-------------------------------------------------------------------------------

  ALLOCATE ( mif  (numvals, numprogs) )
  ALLOCATE ( mifc (numvals, numprogs) )
  ALLOCATE ( mrf  (numvals, numprogs) )
  ALLOCATE ( mrfc (numvals, numprogs) )

  WRITE (6,6000)

!-------------------------------------------------------------------------------
! Read MM5 header for this domain.
!-------------------------------------------------------------------------------

  READ (iutmm, IOSTAT=istat, ERR=8000, END=8100) mif, mrf, mifc, mrfc

!-------------------------------------------------------------------------------
! Extract NX, NY, and NZ.
!-------------------------------------------------------------------------------

  idx = mif(1,1)
  IF ( idx /= 6 ) THEN
    WRITE (6,9200) idx
    GOTO 1001
  ENDIF

  met_ny = mif(104,1)          ! MM5 IX
  met_nx = mif(105,1)          ! MM5 JX
  met_nz = NINT(mrf(101,idx))  ! MM5 KX

  WRITE (6,6100) met_ny, met_nx, met_nz

  met_nycoarse = mif(2,1)        ! MM5 IXCOARSE
  met_nxcoarse = mif(3,1)        ! MM5 JXCOARSE

  WRITE (6,6200) iuthdr
  CALL getgist (mif, mrf, mifc, mrfc)

!-------------------------------------------------------------------------------
! Extract domain attributes.
!-------------------------------------------------------------------------------

  met_gratio   = mif(108,1)          ! grid ratio w.r.t. coarse domain
  met_y_centd  = mrf(2,1)            ! center latitude [degrees]
  met_x_centd  = mrf(3,1)            ! center longitude [degrees]
  met_resoln   = mrf(101,1) * 1000.0 ! horizontal grid spacing [m]
  met_y_11     = mrf(102,1)          ! coarse dom loc of this dom's i=1
  met_x_11     = mrf(103,1)          ! coarse dom loc of this dom's j=1
  met_cone_fac = mrf(4,1)            ! projection cone factor
  met_tru1     = mrf(5,1)            ! true latitude 1 [degrees]
  met_tru2     = mrf(6,1)            ! true latitude 2 [degrees]

  met_mapproj  = mif(4,1)            ! map projection

  SELECT CASE ( met_mapproj )

    CASE (1)  ! Lambert conformal
      met_p_alp_d = MIN(met_tru1, met_tru2)  ! true latitude 1  [degrees]
      met_p_bet_d = MAX(met_tru1, met_tru2)  ! true latitude 2  [degrees]
      met_p_gam_d = met_x_centd              ! central meridian [degrees]

    CASE (2)  ! polar stereographic
      met_p_alp_d = met_tru1         ! true latitude 1 [degrees]
      met_p_bet_d = met_x_centd      ! central meridian [degrees]
      met_p_gam_d = 0.0              ! unused

    CASE (3)  ! Mercator
      met_p_alp_d = 0.0              ! latitude of true scale [degrees]
      met_p_bet_d = badval3          ! unused
      met_p_gam_d = met_x_centd      ! central meridian [degrees]

    CASE DEFAULT
      met_p_bet_d = badval3          ! missing
      met_p_alp_d = badval3          ! missing
      met_p_gam_d = badval3          ! missing

  END SELECT

!-------------------------------------------------------------------------------
! Extract model run options.
!-------------------------------------------------------------------------------

  met_lu_water   = mif(11,1)    ! water category in land use

  IF ( ( met_lu_water == 7 ) .OR. ( met_lu_water == -999 ) ) THEN
    nummetlu = 13
  ELSE IF ( met_lu_water == 16 ) THEN
    nummetlu = 24
  ELSE IF ( met_lu_water == 15 ) THEN
    nummetlu = 16
  ENDIF

  met_restart    = mif(301,6)   ! "restarted" run? 1=yes, 0=no

  met_radiation  = mif(306,6)   ! radiation scheme
  met_pbl        = mif(315,6)   ! PBL scheme
  met_cumulus    = mif(318,6)   ! cumulus parameterization scheme
  met_expl_moist = mif(353,6)   ! explicit moist physics scheme
  met_soil_lsm   = mif(355,6)   ! surface/soil or LSM

  met_snow_opt   = mif(324,6)   ! snow option (0=no effects, 1=yes/no)

  px = .FALSE.
  npxfields = 0

  ift2m  = .FALSE.
  ifw10m = .FALSE.

  SELECT CASE ( met_expl_moist )

    CASE ( 0:3 )  ! simple microphysics, no ice
      nqspecies = 0

    CASE (  4  )  ! simple ice
      nqspecies = 2

    CASE (  5  )  ! mixed-phase with ice and snow
      nqspecies = 4

    CASE ( 6:8 )  ! mixed-phase plus graupel
      nqspecies = 5

    CASE DEFAULT
      nqspecies = 0

  END SELECT

!-------------------------------------------------------------------------------
! Extract MM5 start date and time information.
!-------------------------------------------------------------------------------

  mdate = mif(2,idx)
  CALL mdate2hdate (mdate, met_startdate)

  met_tapfrq = mrf(302,6)
  
!-------------------------------------------------------------------------------
! Determine MCIP season for RADM dry deposition.  MCIP season 4 is a snow-
! covered winter, and it is determined in radmdry.F.
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
    WRITE (6,9300) month
    GOTO 1001
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
    WRITE (6,9300) month
    GOTO 1001
  ENDIF

  ENDIF

!-------------------------------------------------------------------------------
! Extract non-hydrostatic reference variables from header.  If hydrostatic
! MM5 run, set up default values based on reasonable guess for climatic
! region and time of year.
!-------------------------------------------------------------------------------

  met_inhyd = mif(5,6)          ! 1=non-hydrostatic, 0=hydrostatic
  met_ptop  = mrf(1,2) * 100.0  ! model top [hPa --> Pa]
  met_tiso  = 0.0               ! v3 ref isothermal stratospheric T [K]

  IF ( met_inhyd == 1 ) THEN  ! non-hydrostatic run

    met_p00 = mrf(2,idx)  ! Pa
    met_ts0 = mrf(3,idx)  ! K
    met_tlp = mrf(4,idx)  ! K/500 mb

  ELSE

    WRITE (6,7000)
    met_p00 = 100000.0    ! Pa = 1000.0 mb
    met_tlp =     50.0    ! K/500 mb

    IF ( ABS(met_y_centd) < 15.0 ) THEN       ! tropics
      met_ts0 = 300.0  ! K
    ELSE IF ( ABS(met_y_centd) > 75.0 ) THEN  ! polar
      met_ts0 = 270.0  ! K
    ELSE IF ( isesn == 3 ) THEN                     ! mid-latitude winter
      met_ts0 = 280.0  ! K
    ELSE IF ( isesn == 1 ) THEN                     ! mid-latitude summer
      met_ts0 = 290.0  ! K
    ELSE                                            ! mid-latitude spring/autumn
      met_ts0 = 285.0  ! K
    ENDIF

  ENDIF

!-------------------------------------------------------------------------------
! Set flag for fractional land use categories to "false" since this code
! has not been modified for that format.
!-------------------------------------------------------------------------------

  iflufrc = .FALSE.

!-------------------------------------------------------------------------------
! If MCIP vertical structure was not defined in user input namelist file,
! read sigma structure from MM5 and use all layers from MM5 data.
!-------------------------------------------------------------------------------

  IF ( needlayers ) THEN

    nlays = met_nz

    ALLOCATE ( temp1d (met_nz) )

    DO k = 1, met_nz
      kk = 102 + met_nz - k
      temp1d(k) = mrf(kk,idx)
    ENDDO

    ctmlays(1)        = 1.0
    ctmlays(met_nz+1) = 0.0
    DO k = 2, met_nz
      km1 = k - 1
      ctmlays(k) = ( 2.0 * temp1d(km1) ) - ctmlays(km1)
    ENDDO

    DEALLOCATE ( temp1d )

  ENDIF

!-------------------------------------------------------------------------------
! Rewind model output file.
!-------------------------------------------------------------------------------

  REWIND (iutmm)

!-------------------------------------------------------------------------------
! Deallocate arrays.
!-------------------------------------------------------------------------------

  DEALLOCATE ( mif  )
  DEALLOCATE ( mifc )
  DEALLOCATE ( mrf  )
  DEALLOCATE ( mrfc )

  RETURN

!-------------------------------------------------------------------------------
! Format statements.
!-------------------------------------------------------------------------------

 6000 FORMAT (/, 1x, '- SUBROUTINE SETUP_MM5V2 - READING MM5 HEADER')
 6100 FORMAT (3x, 'MM5 GRID DIMENSIONS (I,J,K) ', i4, 1x, i4, 1x, i3)
 6200 FORMAT (//, 1x, '- PRINTING CONTENTS OF MM5 HEADER ON UNIT ', i3, //)

 7000 FORMAT (/, 1x, 70('-'),                                                  &
              /, 1x, '*** WARNING:  USING HYDROSTATIC MM5v2 RUN AS INPUT',     &
              /, 1x, '***   SETTING UP A DEFAULT NON-HYDROSTATIC BASE STATE',  &
              /, 1x, 70('-'), /)

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8000 WRITE (6,9000) iutmm, istat
      GOTO 1001

 8100 WRITE (6,9100) iutmm, istat
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_MM5V2',                        &
              /, 1x, '***   ERROR READING HEADER RECORD',                  &
              /, 1x, '***   UNIT = ', i3,                                  &
              /, 1x, '***   IOSTAT = ', i4,                                &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_MM5V2',                        &
              /, 1x, '***   UNEXPECTED END-OF-FILE REACHED ON UNIT ', i3,  &
              /, 1x, '***   IOSTAT = ', i5,                                &
              /, 1x, '***   VERIFY THAT THE FILE EXISTS!!!'                &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_MM5V2',                        &
              /, 1x, '***   INAPPROPRIATE INPUT FILE'                      &
              /, 1x, '***   MUST BE MM5 OUTPUT (MMOUT)'                    &
              /, 1x, '***   INDEX IS ', i2,                                &
              /, 1x, 70('*'))

 9300 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_MM5V2',                        &
              /, 1x, '***   INVALID MONTH IN MM5v2 DATE',                  &
              /, 1x, '***   MONTH IS ', i4,                                &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE setup_mm5v2
