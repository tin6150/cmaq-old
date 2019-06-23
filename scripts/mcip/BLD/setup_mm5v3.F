
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
! $Header: /project/work/rep/MCIP2/src/mcip2/setup_mm5v3.F,v 1.1 2005/08/29 14:42:56 tlotte Exp $ 


SUBROUTINE setup_mm5v3 (ctmlays)

!-------------------------------------------------------------------------------
! Name:     Set Up the MM5 Domain Attributes from MM5 Version 3.
! Purpose:  Establishes bounds for MM5 post-processing.
! Revised:  17 Sep 2001  Original version.  (T. Otte)
!           11 Oct 2001  Corrected definitions of MET_P_ALP_D, MET_P_BET_D, and
!                        MET_P_GAM_D based on map projection.  (T. Otte)
!           23 Jan 2002  Changed calls to "abort" to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  Changed missing
!                        value for MET_P_ALP_D, MET_P_BET_D, and MET_P_GAM_D
!                        to BADVAL3.  (T. Otte)
!           09 Aug 2004  Corrected header settings for polar stereographic
!                        and Mercator projections.  Modified code so that
!                        arrays are made available in output only if user
!                        options in MM5 generate those data.  Added definition
!                        for MET_SNOW_OPT, and error-checking for using MM5
!                        output with Pleim-Xiu LSM and IFSNOW=2.  Added MM5
!                        cone factor (MET_CONE_FAC) and true latitude 1.
!                        Removed unused variables MET_SDATE and MET_STIME.
!                        Added IFT2M, and rearranged code to search for
!                        2-m temperature in MM5 file.  (T. Otte)
!           30 Nov 2004  Added setting for fractional land use fields flag
!                        based on file availability and based on whether or
!                        not the fields are there.  (MM5 users must have set
!                        IEXTRA flag in TERRAIN namelist to generate these
!                        fields.)  Added NUMMETLU to include number of land
!                        use categories in incoming meteorology data.  (T. Otte)
!           26 May 2005  Changed I and J variable names to Y and X to make
!                        code more general.  Changed subroutine name from
!                        SETUPV3 to SETUP_MM5V3.  Removed unused variables
!                        MET_IEXPAND, MET_IOFFSET, and MET_JOFFSET.  Added
!                        MET_TRU2 and IFW10M.  Added capability to use all MM5
!                        layers for MCIP without defining a priori.  (T. Otte)
!           18 Jul 2005  Corrected RADM seasons for Southern Hemisphere.
!                        Improved error-checking on header comparisons for
!                        MMOUT and TERRAIN files.  (T. Otte)
!           18 Aug 2005  Corrected declaration of START_INDEX from CHARACTER*4
!                        to INTEGER.  Changed internal variable INDEX to IDX
!                        and internal variable TIME to TIMEOUT to avoid
!                        confusion with F90 intrinsic functions.  Removed
!                        inadvertent TAB in format.  (T. Otte)
!-------------------------------------------------------------------------------

  USE file
  USE metinfo
  USE mcipparm
  USE parms3, ONLY: badval3
  USE mcoutcom, ONLY: pc2index

  IMPLICIT NONE

  INTEGER,       ALLOCATABLE   :: bhi       ( : , : )
  CHARACTER*80,  ALLOCATABLE   :: bhic      ( : , : )
  REAL,          ALLOCATABLE   :: bhr       ( : , : )
  CHARACTER*80,  ALLOCATABLE   :: bhrc      ( : , : )
  REAL,          INTENT(INOUT) :: ctmlays   ( maxlays )
  CHARACTER*24                 :: currentdate
  REAL,          ALLOCATABLE   :: data      ( : , : , : , : )
  INTEGER                      :: dd
  CHARACTER*2                  :: ddc
  CHARACTER*46                 :: description
  INTEGER                      :: end_index ( 4 )
  LOGICAL                      :: gotlays
  INTEGER                      :: hh
  CHARACTER*2                  :: hhc
  INTEGER                      :: idx
  INTEGER                      :: iflag
  LOGICAL                      :: ifter
  LOGICAL                      :: ifu10m
  LOGICAL                      :: ifv10m
  INTEGER                      :: istat
  INTEGER                      :: k
  INTEGER                      :: km1
  CHARACTER*2                  :: minc
  INTEGER                      :: mins
  INTEGER                      :: mm
  CHARACTER*2                  :: mmc
  CHARACTER*2                  :: month
  CHARACTER*9                  :: name
  INTEGER                      :: ndim
  INTEGER,       PARAMETER     :: numprogs  = 20
  INTEGER,       PARAMETER     :: numvalsi  = 50
  INTEGER,       PARAMETER     :: numvalsr  = 20
  LOGICAL                      :: ok        = .TRUE.
  CHARACTER*4                  :: ordering
  CHARACTER*16,  PARAMETER     :: pname     = 'SETUP_MM5V3'
  CHARACTER*4                  :: staggering
  INTEGER                      :: start_index ( 4 )
  REAL,          ALLOCATABLE   :: temp1d      ( : )
  CHARACTER*256                :: terfile
  REAL                         :: timeout
  CHARACTER*25                 :: units
  CHARACTER*9                  :: var
  INTEGER                      :: yyyy
  CHARACTER*4                  :: yyyyc

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

  ALLOCATE ( bhi  (numvalsi, numprogs) )
  ALLOCATE ( bhic (numvalsi, numprogs) )
  ALLOCATE ( bhr  (numvalsr, numprogs) )
  ALLOCATE ( bhrc (numvalsr, numprogs) )

  WRITE (6,6000)

!-------------------------------------------------------------------------------
! Loop through one time period of the MM5V3 output file.  Extract information
! from the MM5 header.  Then, look for the 2-m temperature field, the 10-m
! wind components, and, if necessary, vertical structure.
!-------------------------------------------------------------------------------

  ift2m  = .FALSE.  ! initialize flags to false
  ifu10m = .FALSE.
  ifv10m = .FALSE.
  ifw10m = .FALSE.

  IF ( needlayers ) THEN
    gotlays = .FALSE.
  ELSE
    gotlays = .TRUE.
  ENDIF

  v3data: DO

    var = 'IFLAG    '
    READ (iutmm, IOSTAT=istat, ERR=8000, END=999) iflag
  
    IF ( iflag == 0 ) THEN
      var = 'BIG HEADR'
      READ (iutmm, IOSTAT=istat, ERR=8000, END=8100) bhi, bhr, bhic, bhrc

!-------------------------------------------------------------------------------
!     Extract NX, NY, and NZ.
!-------------------------------------------------------------------------------

      idx = bhi(1,1)
      IF ( idx /= 11 ) THEN
        WRITE (6,9300) idx
        GOTO 1001
      ENDIF

      met_ny = bhi(16,1)     ! MM5 IX
      met_nx = bhi(17,1)     ! MM5 JX
      met_nz = bhi(12,idx)   ! MM5 KX

      WRITE (6,6100) met_ny, met_nx, met_nz

      met_nycoarse = bhi(5,1)  ! MM5 IXCOARSE
      met_nxcoarse = bhi(6,1)  ! MM5 JXCOARSE

      WRITE (6,6200) iuthdr
      CALL getgist (bhi, bhr, bhic, bhrc)

!-------------------------------------------------------------------------------
!     Extract domain attributes.
!-------------------------------------------------------------------------------

      met_gratio   = bhi(20,1)    ! grid ratio w.r.t. coarse domain
      met_y_centd  = bhr(2,1)     ! center latitude [degrees]
      met_x_centd  = bhr(3,1)     ! center longitude [degrees]
      met_resoln   = bhr(9,1)     ! horizontal grid spacing [m]
      met_y_11     = bhr(10,1)    ! coarse dom loc of this dom's i=1
      met_x_11     = bhr(11,1)    ! coarse dom loc of this dom's j=1
      met_cone_fac = bhr(4,1)     ! cone factor
      met_tru1     = bhr(5,1)     ! true latitude 1 [degrees]
      met_tru2     = bhr(6,1)     ! true latitude 2 [degrees]

      met_mapproj  = bhi(7,1)     ! map projection

      SELECT CASE ( met_mapproj )

        CASE (1)  ! Lambert conformal
          met_p_alp_d = MIN(met_tru1, met_tru2)  ! true latitude 1  [degrees]
          met_p_bet_d = MAX(met_tru1, met_tru2)  ! true latitude 2  [degrees]
          met_p_gam_d = met_x_centd              ! central meridian [degrees]

        CASE (2)  ! polar stereographic
          met_p_alp_d = met_tru1                 ! true latitude [degrees]
          met_p_bet_d = met_x_centd              ! central meridian [degrees]
          met_p_gam_d = 0.0                      ! unused

        CASE (3)  ! Mercator
          met_p_alp_d = 0.0                      ! latitude of true scale [degrees]
          met_p_bet_d = badval3                  ! unused
          met_p_gam_d = met_x_centd              ! central meridian [degrees]

        CASE DEFAULT
          met_p_bet_d = badval3   ! missing
          met_p_alp_d = badval3   ! missing
          met_p_gam_d = badval3   ! missing

      END SELECT

!-------------------------------------------------------------------------------
!     Extract model run options.
!-------------------------------------------------------------------------------

      met_lu_water   = bhi(23,1)    ! water category in land use

      IF ( ( met_lu_water == 7 ) .OR. ( met_lu_water == -999 ) ) THEN
        nummetlu = 13
      ELSE IF ( met_lu_water == 16 ) THEN
        nummetlu = 24
      ELSE IF ( met_lu_water == 15 ) THEN
        nummetlu = 16 
      ENDIF

      met_restart    = bhi(1,12)    ! "restarted" run? 1=yes, 0=no

      met_radiation  = bhi(1,13)    ! radiation scheme
      met_cumulus    = bhi(2,13)    ! cumulus parameterization scheme
      met_expl_moist = bhi(3,13)    ! explicit moist physics scheme
      met_pbl        = bhi(4,13)    ! PBL scheme
      met_soil_lsm   = bhi(5,13)    ! surface/soil or LSM

      met_snow_opt   = bhi(16,13)   ! snow option (0=none, 1=yes/no, 2=liq equiv)

      IF ( ( met_pbl == 7 ) .AND. ( met_soil_lsm == 3 ) ) THEN  ! Pleim-Xiu
        IF ( met_snow_opt == 2 ) GOTO 8350
        px = .TRUE.
        npxfields = pc2index
      ELSE
        px = .FALSE.
        npxfields = 0
      ENDIF

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

      IF ( met_lu_water == 15 ) THEN  ! SiB land-use database
        WRITE (6,9400)
        GOTO 1001
      ENDIF

!-------------------------------------------------------------------------------
!     Extract MM5 start date and time information.
!-------------------------------------------------------------------------------

      yyyy = bhi(5,11)
      mm   = bhi(6,11)
      dd   = bhi(7,11)
      hh   = bhi(8,11)
      mins = bhi(9,11)

      WRITE ( yyyyc, '(i4.4)' ) yyyy
      WRITE ( mmc,   '(i2.2)' ) mm
      WRITE ( ddc,   '(i2.2)' ) dd
      WRITE ( hhc,   '(i2.2)' ) hh
      WRITE ( minc,  '(i2.2)' ) mins

      met_startdate =  &
          yyyyc //'-'// mmc //'-'// ddc //'-'// hhc //':'// minc // ':00.0000'

      met_tapfrq = bhr(4,12)
  
!-------------------------------------------------------------------------------
!     Determine MCIP season for RADM dry deposition.  MCIP season 4 is a snow-
!     covered winter, and it is determined in radmdry.F.
!
!     Northern Hemisphere:
!       Winter = Dec., Jan., Feb.  (ISESN = 3)
!       Spring = Mar., Apr., May   (ISESN = 5)
!       Summer = Jun., Jul., Aug.  (ISESN = 1)
!       Autumn = Sep., Oct., Nov.  (ISESN = 2)
!
!     Southern Hemisphere:
!       Winter = Jun., Jul., Aug.  (ISESN = 3)
!       Spring = Sep., Oct., Nov.  (ISESN = 5)
!       Summer = Dec., Jan., Feb.  (ISESN = 1)
!       Autumn = Mar., Apr., May   (ISESN = 2)
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
        WRITE (6,9500) month
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
        WRITE (6,9500) month
        GOTO 1001
      ENDIF

      ENDIF

!-------------------------------------------------------------------------------
!     Extract non-hydrostatic reference variables from header.  There is no
!     option for hydrostatic run in standard releases of MM5v3, so these
!     variables will always be defined.
!-------------------------------------------------------------------------------

      met_inhyd = 1         ! always non-hydrostatic
      met_ptop  = bhr(2,2)  ! model top [Pa]
      met_p00   = bhr(2,5)  ! base state sea-level pressure [Pa]
      met_ts0   = bhr(3,5)  ! base state sea-level temperature [K]
      met_tlp   = bhr(4,5)  ! base state lapse rate d(T)/d(ln P)
      met_tiso  = bhr(5,5)  ! base state stratospheric isothermal T [K]

!-------------------------------------------------------------------------------
!   Read fields in MM5 output.
!-------------------------------------------------------------------------------

    ELSE IF ( iflag == 1 ) THEN

      var = 'SM HEADER'
      READ (iutmm, IOSTAT=istat, ERR=8000, END=8600) ndim, start_index,   &
            end_index, timeout, staggering, ordering, currentdate, name,  &
            units, description

      IF ( name == 'T2       ' ) THEN
        ift2m = .TRUE.
      ENDIF

      IF ( name == 'U10      ' ) THEN
        ifu10m = .TRUE.
      ENDIF

      IF ( name == 'V10      ' ) THEN
        ifv10m = .TRUE.
      ENDIF

      IF ( ndim == 1 ) THEN
        ALLOCATE ( data(end_index(1), 1, 1, 1) )
      ELSE IF ( ndim == 2 ) THEN
        ALLOCATE ( data(end_index(1), end_index(2), 1, 1) )
      ELSE IF ( ndim == 3 ) THEN
        ALLOCATE ( data(end_index(1), end_index(2), end_index(3), 1) )
      ELSE IF ( ndim == 4 ) THEN
        ALLOCATE ( data(end_index(1), end_index(2), end_index(3), end_index(4)))
      ENDIF

      var = name
      READ (iutmm, IOSTAT=istat, ERR=8000, END=8700) data

      IF ( ( name == 'SIGMAH   ' ) .AND. ( .NOT. gotlays ) ) THEN
        nlays = met_nz
        ALLOCATE ( temp1d (SIZE(data,1)) )
        temp1d(:) = data(met_nz:1:-1,1,1,1)
        ctmlays(1)        = 1.0
        ctmlays(met_nz+1) = 0.0
        DO k = 2, met_nz
          km1 = k - 1
          ctmlays(k) = ( 2.0 * temp1d(km1) ) - ctmlays(km1)
        ENDDO
        DEALLOCATE ( temp1d)
        gotlays = .TRUE.
      ENDIF

      DEALLOCATE ( data )

!-------------------------------------------------------------------------------
!   End-of-period flag found.
!-------------------------------------------------------------------------------

    ELSE IF ( iflag == 2 ) THEN

      IF ( ( ifu10m ) .AND. ( ifv10m ) ) THEN
        ifw10m = .TRUE.
      ENDIF

      EXIT v3data

    ELSE

      WRITE (6,9200) iflag, iutmm
      GOTO 1001

    ENDIF

    CYCLE v3data
 999 EXIT v3data

  ENDDO v3data

!-------------------------------------------------------------------------------
! Make sure we collected the arrays we need.
!-------------------------------------------------------------------------------

  IF ( .NOT. gotlays ) THEN
    WRITE (6,9950) 'SIGMAH'
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Rewind model output file.
!-------------------------------------------------------------------------------

  REWIND (iutmm)

!-------------------------------------------------------------------------------
! Determine if TERRAIN file exists.  If so, make sure the domain is the same
! as contained in the MM5 file, above.  Then determine if the fractional
! land use fields are in the file.
!-------------------------------------------------------------------------------

  terfile = TRIM( file_ter )
  INQUIRE ( FILE=terfile, EXIST=ifter )

  IF ( .NOT. ifter ) THEN

    WRITE (6,9800)
    iflufrc = .FALSE.

  ELSE

    OPEN (UNIT=iutter,  FILE=terfile, FORM='UNFORMATTED', STATUS='OLD',  &
          IOSTAT=istat, ERR=8810)

    terdata: DO

      var = 'IFLAG    '
      READ (iutter, IOSTAT=istat, ERR=8820, END=888) iflag
  
      IF ( iflag == 0 ) THEN
        var = 'BIG HEADR'
        READ (iutter, IOSTAT=istat, ERR=8820, END=8830) bhi, bhr, bhic, bhrc

        ! Verify that the header for this file matches the MM5 file above.

        ok = ok .AND. ( met_ny          == bhi(16,1) )
        ok = ok .AND. ( met_nx          == bhi(17,1) )
        ok = ok .AND. ( met_nycoarse    == bhi(5,1)  )
        ok = ok .AND. ( met_nxcoarse    == bhi(6,1)  )
        ok = ok .AND. ( met_mapproj     == bhi(7,1)  )
        ok = ok .AND. ( met_gratio      == bhi(20,1) )

        ok = ok .AND. ( ABS(met_y_centd  - bhr(2,1)) < 1.0e-6 )
        ok = ok .AND. ( ABS(met_x_centd  - bhr(3,1)) < 1.0e-6 )
        ok = ok .AND. ( ABS(met_tru1     - bhr(5,1)) < 1.0e-6 )
        ok = ok .AND. ( ABS(met_cone_fac - bhr(4,1)) < 1.0e-6 )

        SELECT CASE ( met_mapproj )
          CASE (1)  ! Lambert conformal
            ok = ok .AND. ( ABS(met_p_alp_d - MIN(bhr(5,1),bhr(6,1))) < 1.0e-6 )
            ok = ok .AND. ( ABS(met_p_bet_d - MAX(bhr(5,1),bhr(6,1))) < 1.0e-6 )
            ok = ok .AND. ( ABS(met_p_gam_d - bhr(3,1))               < 1.0e-6 )
          CASE (2)  ! polar stereographic
            ok = ok .AND. ( ABS(met_p_alp_d - bhr(5,1))               < 1.0e-6 )
            ok = ok .AND. ( ABS(met_p_bet_d - bhr(3,1))               < 1.0e-6 )
          CASE (3)  ! Mercator
            ok = ok .AND. ( ABS(met_p_gam_d - bhr(3,1))               < 1.0e-6 )
        END SELECT

        ok = ok .AND. ( ABS(met_resoln     - bhr(9,1))  < 1.0e-6 )
        ok = ok .AND. ( ABS(met_y_11       - bhr(10,1)) < 1.0e-6 )
        ok = ok .AND. ( ABS(met_x_11       - bhr(11,1)) < 1.0e-6 )
        ok = ok .AND. ( met_lu_water      == bhi(23,1)  )

        IF ( .NOT. ok ) THEN
          WRITE (6,9900) iutter, iutmm
          GOTO 1001
        ENDIF

      ELSE IF ( iflag == 1 ) THEN

        var = 'SM HEADER'
        READ (iutter, IOSTAT=istat, ERR=8820, END=8840) ndim, start_index,  &
              end_index, timeout, staggering, ordering, currentdate, name,  &
              units, description

        IF ( ndim == 1 ) THEN
          ALLOCATE ( data(end_index(1), 1, 1, 1) )
        ELSE IF ( ndim == 2 ) THEN
          ALLOCATE ( data(end_index(1), end_index(2), 1, 1) )
        ELSE IF ( ndim == 3 ) THEN
          ALLOCATE ( data(end_index(1), end_index(2), end_index(3), 1) )
        ELSE IF ( ndim == 4 ) THEN
          ALLOCATE ( data(end_index(1), end_index(2), end_index(3), end_index(4)))
        ENDIF

        IF ( name(1:6) == 'VEGCAT' ) THEN
          iflufrc = .TRUE.
          EXIT terdata
        ENDIF

        var = name
        READ (iutter, IOSTAT=istat, ERR=8820, END=8850) data

        DEALLOCATE ( data )

      ELSE IF ( iflag == 2 ) THEN

        iflufrc = .FALSE.  ! did not find fractional land use data in TERRAIN file
        EXIT terdata

      ELSE

        WRITE (6,9200) iflag, iutter
        GOTO 1001

      ENDIF

      CYCLE terdata
 888   EXIT terdata

    ENDDO terdata

    REWIND (iutter)

  ENDIF

!-------------------------------------------------------------------------------
! Deallocate arrays.
!-------------------------------------------------------------------------------

  DEALLOCATE ( bhi  )
  DEALLOCATE ( bhic )
  DEALLOCATE ( bhr  )
  DEALLOCATE ( bhrc )

  RETURN

!-------------------------------------------------------------------------------
! Format statements.
!-------------------------------------------------------------------------------

 6000 FORMAT (/, 1x, '- SUBROUTINE SETUP_MM5V3 - READING MM5 HEADER')
 6100 FORMAT (3x, 'MM5 GRID DIMENSIONS (I,J,K) ', i4, 1x, i4, 1x, i3)
 6200 FORMAT (//, 1x, '- PRINTING CONTENTS OF MM5 HEADER ON UNIT ', i3, //)

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8000 WRITE (6,9000) iutmm, istat
      GOTO 1001

 8100 WRITE (6,9100) iutmm, istat
      GOTO 1001

 8350 WRITE (6,9350)
      GOTO 1001

 8600 WRITE (6,9600) iutmm, istat
      GOTO 1001

 8700 WRITE (6,9700) iutmm, TRIM(var), istat
      GOTO 1001

 8810 WRITE (6,9810) iutter, istat

 8820 WRITE (6,9000) iutter, istat
      GOTO 1001

 8830 WRITE (6,9100) iutter, istat
      GOTO 1001

 8840 WRITE (6,9600) iutter, istat
      GOTO 1001

 8850 WRITE (6,9700) iutter, TRIM(var), istat
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_MM5V3',                        &
              /, 1x, '***   ERROR READING FILE ON UNIT = ', i3,            &
              /, 1x, '***   IOSTAT = ', i4,                                &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_MM5V3',                        &
              /, 1x, '***   UNEXPECTED END-OF-FILE REACHED ON UNIT ', i3,  &
              /, 1x, '***   IOSTAT = ', i5,                                &
              /, 1x, '***   VERIFY THAT THE FILE EXISTS!!!'                &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_MM5V3',                        &
              /, 1x, '***   UNEXPECTED FLAG FOUND IN VERSION 3 HEADER',    &
              /, 1X, '***   IFLAG = ', i3,                                 &
              /, 1x, '***   UNIT  = ', i3,                                 &
              /, 1x, 70('*'))

 9300 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_MM5V3',                        &
              /, 1x, '***   INAPPROPRIATE INPUT FILE'                      &
              /, 1x, '***   MUST BE MM5 OUTPUT (MMOUT)'                    &
              /, 1x, '***   INDEX IS ', i2,                                &
              /, 1x, 70('*'))

 9350 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_MM5V3',                        &
              /, 1x, '***   INCOMPATIBLE OPTIONS IN MM5 OUTPUT',           &
              /, 1x, '***   RAN PLEIM-XIU LSM WITH IFSNOW=2',              &
              /, 1x, '***   QUESTIONABLE OUTPUT, ESP. SNOWCOVR/WEASD',     &
              /, 1x, 70('*'))

 9400 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_MM5V3',                        &
              /, 1x, '***   CANNOT USE SiB LAND USE CATEGORIES WITH MCIP', &
              /, 1x, 70('*'))

 9500 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_MM5V3',                        &
              /, 1x, '***   INVALID MONTH IN MM5v3 DATE',                  &
              /, 1x, '***   MONTH IS ', i4,                                &
              /, 1x, 70('*'))

 9600 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_MM5V3',                        &
              /, 1x, '***   UNEXPECTED END-OF-FILE, UNIT = ', i3,          &
              /, 1x, '***   VARIABLE = SMALL HEADER',                      &
              /, 1x, '***   IOSTAT = ', i4,                                &
              /, 1x, 70('*'))

 9700 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_MM5V3',                        &
              /, 1x, '***   UNEXPECTED END-OF-FILE, UNIT = ', i3,          &
              /, 1x, '***   VARIABLE = ', a,                               &
              /, 1x, '***   IOSTAT = ', i4,                                &
              /, 1x, 70('*'))

 9800 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_MM5V3',                        &
              /, 1x, '***   DID NOT FIND TERRAIN FILE'                     &
              /, 1x, '***   WILL NOT USE FRACTIONAL LAND USE DATA'         &
              /, 1x, 70('*'))

 9810 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_MM5V3',                        &
              /, 1x, '***   ERROR OPENING TERRAIN FILE',                   &
              /, 1x, '***   UNIT = ', i3,                                  &
              /, 1x, '***   IOSTAT = ', i4,                                &
              /, 1x, 70('*'))

 9900 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_MM5V3',                        &
              /, 1x, '***   INPUT FILES DO NOT SEEM TO BE SAME GRID',      &
              /, 1x, '***   CHECK FORTRAN UNITS ', i3, ' AND ', i3,        &
              /, 1x, 70('*'))

 9950 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: SETUP_MM5V3',                        &
              /, 1x, '***   DID NOT FIND ARRAY ', a,                       &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE setup_mm5v3
