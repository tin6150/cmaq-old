
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
! $Header: /project/work/rep/MCIP2/src/mcip2/rdmm5v2.F,v 1.6 2005/08/29 14:36:19 tlotte Exp $ 


SUBROUTINE rdmm5v2 (mcip_now)
   
!-------------------------------------------------------------------------------
! Name:     Read MM5 Version 2
! Purpose:  Reads incoming MM5v2-formatted output files for use in MCIP.
! Notes:    Option to use Pleim-Xiu prognostic surface fields is only
!           compatible with MM5v3.
! Revised:  20 Sep 2001  Original version.  This replaces MCIP v1 routines
!                        readmm1.F and readmm2.F.  (T. Otte)
!           18 Oct 2001  Corrected calculation of precipitation rates for
!                        MCIP runs starting after MM5 output start time.
!                        Added error-checking on new file headers.  Added
!                        capability to process 24-category land use.  (T. Otte)
!           20 Nov 2001  Now allow code to run in the absence of input
!                        cloud and rain mixing ratio arrays.  Also add time
!                        stamp information as input time periods are
!                        processed.  (T. Otte)
!           07 Jan 2002  Changed file name to explicit file rather than
!                        Fortran unit to improve portability.  (S. Howard
!                        and T. Otte)
!           09 Jan 2002  Changed calls to "abort" to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  (T. Otte)
!           13 Mar 2002  Corrected size of MMFILE.  (T. Otte)
!           05 Jun 2003  Renamed local variable SEASON to MET_SEASON and
!                        moved it to module METINFO.  Commented out the need
!                        for PBL variables on input if they are set to be
!                        rediagnosed (for compatibility with MM5v2.6 and
!                        prior releases).  Added snow cover to input.
!                        (T. Otte, J. Pleim, and D. Schwede)
!           05 Aug 2004  Added graupel.  Changed local array allocation to occur
!                        only on initial call to subroutine to avoid memory
!                        fragmentation.  Corrected header settings for polar
!                        stereographic and Mercator projections.  (T. Otte and
!                        D. Schwede)
!           31 Jan 2005  Changed code so that input meteorology arrays are
!                        loaded X-Y rather than Y-X.  Removed unused variables
!                        MET_IEXPAND, MET_IOFFSET, MET_JOFFSET, REGIME and
!                        MAVAIL.  Added MET_TRU2.  (T. Otte)
!           15 Jul 2005  Corrected MM5 seasons for Southern Hemisphere.
!                        (T. Otte)
!           19 Aug 2005  Changed internal variable INDEX to IDX to avoid
!                        confusion with F90 intrinsic function.  Eliminated
!                        use of F90 intrinsic function TRANSPOSE to avoid
!                        bounds checking issues on Linux Intel compilers.
!                        (T. Otte)
!-------------------------------------------------------------------------------

  USE date_pack
  USE file
  USE metinfo, nx => met_nx, ny => met_ny, nz => met_nz
  USE metvars
  USE mcipparm

  IMPLICIT NONE

  REAL                         :: albd13     ( 13, 2 )
  REAL                         :: albd24     ( 24, 2 )
  CHARACTER*24                 :: currentdate
  REAL,    SAVE, ALLOCATABLE   :: dum2d      ( : , : )
  REAL,          ALLOCATABLE   :: dum2d2     ( : , : )
  REAL,    SAVE, ALLOCATABLE   :: dum3d      ( : , : , : )
  CHARACTER*24                 :: endseas
  LOGICAL,       SAVE          :: first      = .TRUE.
  INTEGER                      :: i
  INTEGER                      :: idts_end
  INTEGER                      :: idts_start
  INTEGER                      :: idtsec
  INTEGER                      :: idx
  LOGICAL                      :: ifmm
  CHARACTER*60                 :: ifmt1
  CHARACTER*60                 :: ifmt1a
  INTEGER                      :: istat
  INTEGER                      :: j
  INTEGER                      :: k
  INTEGER                      :: k1
  INTEGER                      :: k2
  INTEGER                      :: keep
  INTEGER                      :: kk
  INTEGER                      :: km1
  INTEGER                      :: lbnd
  CHARACTER*24,  INTENT(IN)    :: mcip_now
  INTEGER                      :: mdate
  INTEGER,       ALLOCATABLE   :: mif        ( : , : )
  CHARACTER*80,  ALLOCATABLE   :: mifc       ( : , : )
  CHARACTER*256                :: mmfile
  REAL,          ALLOCATABLE   :: mrf        ( : , : )
  INTEGER                      :: n
  LOGICAL                      :: newfile
  INTEGER                      :: num2d
  INTEGER                      :: num3d
  INTEGER,       PARAMETER     :: numprogs   = 20
  INTEGER,       PARAMETER     :: numvals    = 1000
  LOGICAL                      :: ok         = .TRUE.
  CHARACTER*16,  PARAMETER     :: pname      = 'RDMM5V2'
  REAL,          ALLOCATABLE   :: psd        ( : , : )
  REAL                         :: sfem13     ( 13, 2 )
  REAL                         :: sfem24     ( 24, 2 )
  REAL                         :: sfz013     ( 13, 2 )
  REAL                         :: sfz024     ( 24, 2 )
  CHARACTER*24                 :: startdate
  CHARACTER*24                 :: startseas
  CHARACTER*2                  :: str1
  CHARACTER*2                  :: str2
  INTEGER,       PARAMETER     :: ttol       = 300  ! [sec]
  INTEGER                      :: ubnd
  CHARACTER*8                  :: var


  ! Data capture verification flags.

  LOGICAL :: gotglw,     gotgroundt, gothfx,     gotlanduse, gotlatcrs,   &
             gotloncrs,  gotmapcrs,  gotmapdot,  gotpp,      gotpsa,      &
             gotqca,     gotqfx,     gotqga,     gotqia,     gotqra,      &
             gotqsa,     gotqva,     gotraincon, gotrainnon, gotrgrnd,    &
             gotsnowcov, gotta,      gotterrain, gotua,      gotust,      &
             gotva,      gotwa,      gotzpbl

  ! Define roughness length, albedo, emissivity, and moisture availability
  ! by season as functions of land use and season since it typically is not
  ! part of MM5v2 output.

  DATA (sfz013(i,1),i=1,13) / 50.0,  15.0,  12.0,  50.0,  50.0,  40.0,   &
                               0.01, 20.0,  10.0,  10.0,   5.0,  50.0,   &
                              15.0 /  ! summer values [cm]

  DATA (sfz024(i,1),i=1,24) / 50.0,  15.0,  15.0,  15.0,  14.0,  20.0,   &
                              12.0,  10.0,  11.0,  15.0,  50.0,  50.0,   &
                              50.0,  50.0,  50.0,   0.01, 20.0,  40.0,   &
                              10.0,  10.0,  30.0,  15.0,  10.0,   5.0 /  ! summer values [cm]

  DATA (sfz013(i,2),i=1,13) / 50.0,   5.0,  10.0,  50.0,  50.0,  40.0,   &
                               0.01, 20.0,  10.0,  10.0,   5.0,  50.0,   &
                              15.0 /  ! winter values [cm]

  DATA (sfz024(i,2),i=1,24) / 50.0,   5.0,   5.0,   5.0,   5.0,  20.0,   &
                              10.0,  10.0,  10.0,  15.0,  50.0,  50.0,   &
                              50.0,  50.0,  50.0,   0.01, 20.0,  40.0,   &
                              10.0,  10.0,  30.0,  15.0,   5.0,   5.0 /  ! winter values [cm]

  DATA (albd13(i,1),i=1,13) /  0.18,  0.17,  0.19,  0.16,  0.12,  0.14,  &
                               0.08,  0.14,  0.25,  0.15,  0.55,  0.12,  &
                               0.20 /  ! summer values [dec]

  DATA (albd24(i,1),i=1,24) /  0.18,  0.17,  0.18,  0.18,  0.18,  0.16,  &
                               0.19,  0.22,  0.20,  0.20,  0.16,  0.14,  &
                               0.12,  0.12,  0.13,  0.08,  0.14,  0.14,  &
                               0.25,  0.15,  0.15,  0.15,  0.25,  0.55 /  ! summer values [dec]

  DATA (albd13(i,2),i=1,13) /  0.18,  0.23,  0.23,  0.17,  0.12,  0.14,  &
                               0.08,  0.14,  0.25,  0.70,  0.70,  0.12,  &
                               0.20 /  ! winter values [dec]

  DATA (albd24(i,2),i=1,24) /  0.18,  0.23,  0.23,  0.23,  0.23,  0.20,  &
                               0.23,  0.25,  0.24,  0.20,  0.17,  0.15,  &
                               0.12,  0.12,  0.14,  0.08,  0.14,  0.14,  &
                               0.25,  0.60,  0.50,  0.55,  0.70,  0.70 /  ! winter values [dec]

  DATA (sfem13(i,1),i=1,13) / 88.0,  92.0,  92.0,  93.0,  95.0,  95.0,   &
                              98.0,  95.0,  85.0,  92.0,  95.0,  95.0,   &
                              92.0 /  ! summer values [%]

  DATA (sfem24(i,1),i=1,24) / 88.0,  92.0,  92.0,  92.0,  92.0,  93.0,   &
                              92.0,  88.0,  90.0,  92.0,  93.0,  94.0,   &
                              95.0,  95.0,  94.0,  98.0,  95.0,  95.0,   &
                              85.0,  92.0,  93.0,  92.0,  85.0,  95.0 /  ! summer values [%]

  DATA (sfem13(i,2),i=1,13) / 88.0,  92.0,  92.0,  93.0,  95.0,  95.0,   &
                              98.0,  95.0,  85.0,  92.0,  95.0,  95.0,   &
                              92.0 /  ! winter values [%]

  DATA (sfem24(i,2),i=1,24) / 88.0,  92.0,  92.0,  92.0,  92.0,  93.0,   &
                              92.0,  88.0,  90.0,  92.0,  93.0,  93.0,   &
                              95.0,  95.0,  94.0,  98.0,  95.0,  95.0,   &
                              85.0,  92.0,  93.0,  92.0,  95.0,  95.0 /  ! winter values [%]

  INTERFACE

    SUBROUTINE crs2dot (varcrs, vardot)
      IMPLICIT NONE
      REAL,          INTENT(IN)    :: varcrs     ( : , : )
      REAL,          INTENT(OUT)   :: vardot     ( : , : )
    END SUBROUTINE crs2dot

    SUBROUTINE gridgeometry (inner, f, smap, xlon, xlat)
      IMPLICIT NONE
      REAL,          INTENT(OUT)   :: f         ( : , : )
      INTEGER,       INTENT(IN)    :: inner
      REAL,          INTENT(OUT)   :: smap      ( : , : )
      REAL,          INTENT(OUT)   :: xlat      ( : , : )
      REAL,          INTENT(OUT)   :: xlon      ( : , : )
    END SUBROUTINE gridgeometry

  END INTERFACE

!-------------------------------------------------------------------------------
! Allocate necessary arrays.
!-------------------------------------------------------------------------------

  IF ( .NOT. ALLOCATED ( dum2d ) ) ALLOCATE ( dum2d (ny, nx) )      ! (Y,X)
  IF ( .NOT. ALLOCATED ( dum3d ) ) ALLOCATE ( dum3d (ny, nx, nz) )  ! (Y,X,Z)
  IF ( .NOT. ALLOCATED ( psd   ) ) ALLOCATE ( psd   (nx, ny) )

  ALLOCATE ( mif   (numvals, numprogs) )
  ALLOCATE ( mifc  (numvals, numprogs) )
  ALLOCATE ( mrf   (numvals, numprogs) )

!-------------------------------------------------------------------------------
! Initialize data capture flags.  Need to do this here (rather than on
! declaration line) since Sun seems to default all variables to "SAVE".
! That is, on Sun, the variables will not be re-initialized from the
! declaration line on subsequent calls; they hold the value from the last call.
!-------------------------------------------------------------------------------

  IF ( first ) THEN  ! Need to read time-invariant fields.

    gotlanduse = .FALSE.  ;  gotlatcrs  = .FALSE.  ;  gotloncrs  = .FALSE.
    gotmapcrs  = .FALSE.  ;  gotmapdot  = .FALSE.  ;  gotterrain = .FALSE.

  ELSE  ! Already stored time-invariant fields, and do not need to re-store.

    gotlanduse = .TRUE.   ;  gotlatcrs  = .TRUE.   ;  gotloncrs  = .TRUE.
    gotmapcrs  = .TRUE.   ;  gotmapdot  = .TRUE.   ;  gotterrain = .TRUE.

  ENDIF

  ! Need to read these time-variant fields each time.

  gotglw     = .FALSE.  ;  gotgroundt = .FALSE.  ;  gothfx     = .FALSE.
  gotpp      = .FALSE.  ;  gotpsa     = .FALSE.  ;  gotqca     = .FALSE.
  gotqfx     = .FALSE.  ;  gotqga     = .FALSE.  ;  gotqia     = .FALSE.
  gotqra     = .FALSE.  ;  gotqsa     = .FALSE.  ;  gotqva     = .FALSE.
  gotraincon = .FALSE.  ;  gotrainnon = .FALSE.  ;  gotrgrnd   = .FALSE.
  gotsnowcov = .FALSE.  ;  gotta      = .FALSE.  ;  gotua      = .FALSE.
  gotust     = .FALSE.  ;  gotva      = .FALSE.  ;  gotwa      = .FALSE.
  gotzpbl    = .FALSE.

!-------------------------------------------------------------------------------
! Set up print statements.
!-------------------------------------------------------------------------------

  k1 = nz / 5
  k2 = MOD(nz, 5)

  WRITE ( str1, '(i2)' ) k1 - 1
  WRITE ( str2, '(i2)' ) k2

  IF ( (k1 - 1) > 0 ) THEN
    IF ( k2 > 0 ) THEN
      ifmt1 = "(/,4x,a,5(2x,f9.4)," // str1 // "(/,13x,5(2x,f9.4)),/,13x,"   &
         &    // str2 // "(2x,f9.4))"
    ELSE
      ifmt1 = "(/,4x,a,5(2x,f9.4)," // str1 // "(/,13x,5(2x,f9.4)))"
    ENDIF
  ELSE
    IF ( k2 > 0 ) THEN
      ifmt1 = "(/,4x,a,5(2x,f9.4),/,13x," // str2 // "(2x,f9.4))"
    ELSE
      ifmt1 = "(/,4x,a,5(2x,f9.4))"
    ENDIF
  ENDIF

  k1 = (nz+1) / 5
  k2 = MOD(nz+1, 5)

  WRITE ( str1, '(i2)' ) k1 - 1
  WRITE ( str2, '(i2)' ) k2

  IF ( (k1 - 1) > 0 ) THEN
    IF ( k2 > 0 ) THEN
      ifmt1a = "(/,4x,a,5(2x,f9.4)," // str1 // "(/,13x,5(2x,f9.4)),/,13x,"   &
         &     // str2 // "(2x,f9.4))"
    ELSE
      ifmt1a = "(/,4x,a,5(2x,f9.4)," // str1 // "(/,13x,5(2x,f9.4)))"
    ENDIF
  ELSE
    IF ( k2 > 0 ) THEN
      ifmt1a = "(/,4x,a,5(2x,f9.4),/,13x," // str2 // "(2x,f9.4))"
    ELSE
      ifmt1a = "(/,4x,a,5(2x,f9.4))"
    ENDIF
  ENDIF

!-------------------------------------------------------------------------------
! Read MM5 header for this domain.  Note that the character header information
! (MRFC) is not read and not used here.
!-------------------------------------------------------------------------------

  keep    = 0
  newfile = .FALSE.

  findtime: DO

    var = 'HEADER  '
    READ (iutmm, IOSTAT=istat, ERR=8000, END=999) mif, mrf, mifc

!-------------------------------------------------------------------------------
! Extract some variables from header.
!-------------------------------------------------------------------------------

    idx = mif(1,1)
    IF ( idx /= 6 ) THEN  ! this was checked in "setup" and should be OK
      WRITE (6,9050) idx
      GOTO 1001
    ENDIF

    IF ( ( met_lu_water /=  7 ) .AND. ( met_lu_water /= -999 ) .AND.  &
         ( met_lu_water /= 16 ) ) THEN
      WRITE (6,9075) met_lu_water
      GOTO 1001
    ENDIF

    mdate = mif(2,idx)
    CALL mdate2hdate (mdate, startdate)

    mdate = mif(1,idx)
    CALL mdate2hdate (mdate, currentdate)

    WRITE (*,'(/,a,2x,f15.5," Hours"/)') currentdate, mrf(1,6)/60.0

    num3d = mif(201,idx)
    num2d = mif(202,idx)

    IF ( newfile ) THEN

      ! Check to see if the grids and options are the same.

      ok = ok .AND. ( ny             ==      mif(104,1)          )
      ok = ok .AND. ( nx             ==      mif(105,1)          )
      ok = ok .AND. ( nz             == NINT(mrf(101,6))         )
      ok = ok .AND. ( met_nycoarse   ==      mif(2,1)            )
      ok = ok .AND. ( met_nxcoarse   ==      mif(3,1)            )
      ok = ok .AND. ( met_mapproj    ==      mif(4,1)            )
      ok = ok .AND. ( met_gratio     ==      mif(108,1)          )
      ok = ok .AND. ( met_y_centd    ==      mrf(2,1)            )
      ok = ok .AND. ( met_x_centd    ==      mrf(3,1)            )
      ok = ok .AND. ( met_cone_fac   ==      mrf(4,1)            )
      ok = ok .AND. ( met_tru1       ==      mrf(5,1)            )
      ok = ok .AND. ( met_tru2       ==      mrf(6,1)            )

      SELECT CASE ( met_mapproj )
        CASE (1)  ! Lambert conformal
          ok = ok .AND. ( met_p_alp_d == MIN(mrf(5,1),mrf(6,1)) )
          ok = ok .AND. ( met_p_bet_d == MAX(mrf(5,1),mrf(6,1)) )
          ok = ok .AND. ( met_p_gam_d == mrf(3,1) )
        CASE (2)  ! polar stereographic
          ok = ok .AND. ( met_p_alp_d == mrf(5,1) )
          ok = ok .AND. ( met_p_bet_d == mrf(3,1) )
        CASE (3)  ! Mercator
          ok = ok .AND. ( met_p_gam_d == mrf(3,1) )
      END SELECT

      ok = ok .AND. ( met_resoln     ==      mrf(101,1) * 1000.0 )
      ok = ok .AND. ( met_y_11       ==      mrf(102,1)          )
      ok = ok .AND. ( met_x_11       ==      mrf(103,1)          )
      ok = ok .AND. ( met_lu_water   ==      mif(11,1)           )
      ok = ok .AND. ( met_radiation  ==      mif(306,6)          )
      ok = ok .AND. ( met_pbl        ==      mif(315,6)          )
      ok = ok .AND. ( met_cumulus    ==      mif(318,6)          )
      ok = ok .AND. ( met_expl_moist ==      mif(353,6)          )
      ok = ok .AND. ( met_soil_lsm   ==      mif(355,6)          )
      ok = ok .AND. ( met_tapfrq     ==      mrf(302,6)          )
      ok = ok .AND. ( met_inhyd      ==      mif(5,6)            )
      ok = ok .AND. ( met_ptop       ==      mrf(1,2)   *  100.0 )

      IF ( met_inhyd == 1 ) THEN
        ok = ok .AND. ( met_p00 == mrf(2,6) )
        ok = ok .AND. ( met_ts0 == mrf(3,6) )
        ok = ok .AND. ( met_tlp == mrf(4,6) )
      ENDIF

      IF ( .NOT. ok ) THEN
        WRITE (6,9300) iutmm-1, iutmm
        GOTO 1001
      ENDIF

      ! If the new file is not a continuation of a previous run
      ! (i.e., "restart"), then stop since there is a physical
      ! discontinuity.

      met_restart = mif(301,6)
      IF ( met_restart /= 1 ) THEN
        WRITE (6,9400) iutmm-1, iutmm
        GOTO 1001
      ENDIF

      newfile = .FALSE.

    ENDIF

!-------------------------------------------------------------------------------
! Compare CURRENTDATE to CTM_NOW to see if this meteorology output time
! should be processed.  Since MM5 output is processed as REAL numbers within
! MM5, there is occasional drift in the output times, so they often are not
! exactly on the hour (for example).  To overcome this, a time tolerance is
! set so the appropriate times can be captured.
!-------------------------------------------------------------------------------

    CALL geth_idts (currentdate, mcip_now, idtsec)
    IF ( ABS(idtsec) <= ttol ) THEN
      keep = 1
    ELSE
      keep = 0
    ENDIF

!-------------------------------------------------------------------------------
! Extract half-sigma values and calculate full-sigma values.  Invert
! order so k=1 at surface and k=nz at model top.
!-------------------------------------------------------------------------------

    IF ( ( first ) .AND. ( keep == 1 ) ) THEN

      DO k = 1, nz
        kk = 102 + nz - k
        sigmah(k) = mrf(kk,idx)
      ENDDO

      lbnd = LBOUND(sigmaf,1)   ! first element is mapped correctly (0 or 1)
      ubnd = UBOUND(sigmaf,1)   ! last element is mapped correctly (nz or nz+1)

      sigmaf(lbnd) = 1.0
      sigmaf(ubnd) = 0.0
      DO k = lbnd+1, ubnd-1
        km1 = k - 1
        sigmaf(k) = ( 2.0 * sigmah(km1) ) - sigmaf(km1)
      ENDDO

    ENDIF

!-------------------------------------------------------------------------------
! Read 3-d variables.  Invert vertical index so that k=1 at surface and
! k=nz at model top.  Note that most 3-d variables are coupled with (or
! multiplied by) p-star which is in units of kPa.
!-------------------------------------------------------------------------------

    read3d: DO n = 1, num3d

      var = mifc(204+n,idx)(1:8)

      IF ( var == 'W       ' ) THEN
        READ (iutmm, IOSTAT=istat, ERR=8000, END=8000) dum3d, dum2d
      ELSE
        READ (iutmm, IOSTAT=istat, ERR=8000, END=8000) dum3d
      ENDIF
        
      keep3d: IF ( keep == 1 ) THEN

        IF ( mif(204+n,idx) / 10 == 0 ) THEN  ! cross point field
          dum3d(ny,:,:) = dum3d(ny-1,:,:)
          dum3d(:,nx,:) = dum3d(:,nx-1,:)
        ENDIF

        SELECT CASE ( var )

          CASE ( 'U       ' )  ! u-component wind [kPa m s^-1]
            DO k = 1, nz
              DO j = 1, nx
                DO i = 1, ny
                  ua(j,i,k) = dum3d(i,j,nz-k+1)
                ENDDO
              ENDDO
            ENDDO
            gotua = .TRUE.

          CASE ( 'V       ' )  ! v-component wind [kPa m s^-1]
            DO k = 1, nz
              DO j = 1, nx
                DO i = 1, ny
                  va(j,i,k) = dum3d(i,j,nz-k+1)
                ENDDO
              ENDDO
            ENDDO
            gotva = .TRUE.

          CASE ( 'T       ' )  ! temperature [kPa K]
            DO k = 1, nz
              DO j = 1, nx
                DO i = 1, ny
                  ta(j,i,k) = dum3d(i,j,nz-k+1)
                ENDDO
              ENDDO
            ENDDO
            gotta = .TRUE.

          CASE ( 'Q       ' )  ! mixing ratio [kPa kg kg^-1]
            DO k = 1, nz
              DO j = 1, nx
                DO i = 1, ny
                  qva(j,i,k) = dum3d(i,j,nz-k+1)
                ENDDO
              ENDDO
            ENDDO
            gotqva = .TRUE.

          CASE ( 'CLW     ' )  ! cloud water mixing ratio [kPa kg kg^-1]
            DO k = 1, nz
              DO j = 1, nx
                DO i = 1, ny
                  qca(j,i,k) = dum3d(i,j,nz-k+1)
                ENDDO
              ENDDO
            ENDDO
            gotqca = .TRUE.

          CASE ( 'RNW     ' )  ! rain water mixing ratio [kPa kg kg^-1]
            DO k = 1, nz
              DO j = 1, nx
                DO i = 1, ny
                  qra(j,i,k) = dum3d(i,j,nz-k+1)
                ENDDO
              ENDDO
            ENDDO
            gotqra = .TRUE.

          CASE ( 'ICE     ' )  ! ice water mixing ratio [kPa kg kg^-1]
            DO k = 1, nz
              DO j = 1, nx
                DO i = 1, ny
                  qia(j,i,k) = dum3d(i,j,nz-k+1)
                ENDDO
              ENDDO
            ENDDO
            gotqia = .TRUE.

          CASE ( 'SNOW    ' )  ! snow water mixing ratio [kPa kg kg^-1]
            DO k = 1, nz
              DO j = 1, nx
                DO i = 1, ny
                  qsa(j,i,k) = dum3d(i,j,nz-k+1)
                ENDDO
              ENDDO
            ENDDO
            gotqsa = .TRUE.

          CASE ( 'GRAUPEL ' )  ! graupel water mixing ratio [kPa kg kg^-1]
            DO k = 1, nz
              DO j = 1, nx
                DO i = 1, ny
                  qga(j,i,k) = dum3d(i,j,nz-k+1)
                ENDDO
              ENDDO
            ENDDO
            gotqga = .TRUE.

          CASE ( 'W       ' )  ! vertical wind [kPa m s^-1]
            lbnd = LBOUND(wa,3)  ! first element is mapped correctly (0 or 1)
            ubnd = UBOUND(wa,3)  ! last element is mapped correctly (nz or nz+1)
            DO j = 1, nx
              DO i = 1, ny
                wa(j,i,lbnd) = dum2d(i,j)
                DO k = lbnd+1, ubnd
                  wa(j,i,k) = dum3d(i,j,nz-k+1)
                ENDDO
              ENDDO
            ENDDO
            gotwa = .TRUE.

          CASE ( 'PP      ' )  ! perturbation pressure [kPa Pa]
            DO k = 1, nz
              DO j = 1, nx
                DO i = 1, ny
                  pp(j,i,k) = dum3d(i,j,nz-k+1)
                ENDDO
              ENDDO
            ENDDO
            gotpp = .TRUE.

        END SELECT

      ENDIF keep3d

    ENDDO read3d

!-------------------------------------------------------------------------------
! Read 2-d variables.
!-------------------------------------------------------------------------------

    read2d: DO n = 1, num2d

      var = mifc(204+num3d+n,idx)(1:8)

      READ (iutmm, IOSTAT=istat, ERR=8000, END=8000) dum2d

      IF ( var == 'RAIN CON' ) THEN  ! convective precip [cm]
        DO j = 1, nx
          DO i = 1, ny
            raincon(j,i) = dum2d(i,j)
            ! incremental total in cm
            raincon(j,i) = MAX(0.0, (raincon(j,i) - rcold(j,i)))
            ! accumulated total to subtract from next interval
            rcold  (j,i) = dum2d(i,j)
          ENDDO
        ENDDO
        IF ( keep == 1 ) THEN
          gotraincon = .TRUE.
          WRITE (*,6000) var, raincon(lprt_metx, lprt_mety), 'cm'
        ENDIF
      ENDIF

      IF ( var == 'RAIN NON' ) THEN  ! non-convective precip [cm]
        DO j = 1, nx
          DO i = 1, ny
            rainnon(j,i) = dum2d(i,j)
            ! incremental total in cm
            rainnon(j,i) = MAX(0.0, (rainnon(j,i) - rnold(j,i)))
            ! accumulated total to subtract from next interval
            rnold  (j,i) = dum2d(i,j)
          ENDDO
        ENDDO
        IF ( keep == 1 ) THEN
          gotrainnon = .TRUE.
          WRITE (*,6000) var, rainnon(lprt_metx, lprt_mety), 'cm'
        ENDIF
      ENDIF

      keep2d: IF ( keep == 1 ) THEN

        IF ( mif(204+num3d+n,idx) / 10 == 0 ) THEN  ! cross point field
          dum2d(ny,:) = dum2d(ny-1,:)
          dum2d(:,nx) = dum2d(:,nx-1)
        ENDIF

        ! Collect time-invariant 2-d arrays.

        IF ( first ) THEN

          SELECT CASE ( var )

            CASE ( 'TERRAIN ' )  ! terrain elevation [m]
              DO j = 1, nx
                DO i = 1, ny
                  terrain(j,i) = dum2d(i,j)
                ENDDO
              ENDDO
              gotterrain = .TRUE.
              WRITE (*,6000) var, terrain(lprt_metx, lprt_mety), 'm'

            CASE ( 'MAPFACCR' )  ! map-scale factor on cross points [dim'less]
              DO j = 1, nx
                DO i = 1, ny
                  mapcrs(j,i) = dum2d(i,j)
                ENDDO
              ENDDO
              gotmapcrs = .TRUE.
              WRITE (*,6000) var, mapcrs(lprt_metx, lprt_mety), 'dimensionless'

            CASE ( 'MAPFACDT' )  ! map-scale factor on dot points [dim'less]
              DO j = 1, nx
                DO i = 1, ny
                  mapdot(j,i) = dum2d(i,j)
                ENDDO
              ENDDO
              gotmapdot = .TRUE.
              WRITE (*,6000) var, mapdot(lprt_metx, lprt_mety), 'dimensionless'

            CASE ( 'LATITCRS' )  ! latitude on cross points [degree]
              DO j = 1, nx
                DO i = 1, ny
                  latcrs(j,i) = dum2d(i,j)
                ENDDO
              ENDDO
              gotlatcrs = .TRUE.
              WRITE (*,6000) var, latcrs(lprt_metx, lprt_mety), 'degree'

            CASE ( 'LONGICRS' )  ! longitude on cross points [degree]
              DO j = 1, nx
                DO i = 1, ny
                  loncrs(j,i) = dum2d(i,j)
                ENDDO
              ENDDO
              gotloncrs = .TRUE.
              WRITE (*,6000) var, loncrs(lprt_metx, lprt_mety), 'degree'

            CASE ( 'LAND USE' )  ! land use [category]
              DO j = 1, nx
                DO i = 1, ny
                  landuse(j,i) = NINT(dum2d(i,j))
                ENDDO
              ENDDO
              gotlanduse = .TRUE.
              WRITE (*,6100) var, landuse(lprt_metx, lprt_mety), 'category'

          END SELECT

        ENDIF

        ! Collect variable 2-d arrays.

        SELECT CASE ( var )

          CASE ( 'PSTARCRS' )  ! p-star [kPa]
            DO j = 1, nx
              DO i = 1, ny
                psa(j,i) = dum2d(i,j)
              ENDDO
            ENDDO
            gotpsa = .TRUE.

          CASE ( 'GROUND T' )  ! ground temperature [K]
            DO j = 1, nx
              DO i = 1, ny
                groundt(j,i) = dum2d(i,j)
              ENDDO
            ENDDO
            gotgroundt = .TRUE.
            WRITE (*,6000) var, groundt(lprt_metx, lprt_mety), 'K'

          CASE ( 'PBL HGT ' )  ! PBL height [m]
            DO j = 1, nx
              DO i = 1, ny
                zpbl(j,i) = dum2d(i,j)
              ENDDO
            ENDDO
            gotzpbl = .TRUE.
            WRITE (*,6000) var, zpbl(lprt_metx, lprt_mety), 'm'

          CASE ( 'SHFLUX  ' )  ! sensible heat flux [W m^-2]
            DO j = 1, nx
              DO i = 1, ny
                hfx(j,i) = dum2d(i,j)
              ENDDO
            ENDDO
            gothfx = .TRUE.
            WRITE (*,6000) var, hfx(lprt_metx, lprt_mety), 'W m^-2'

          CASE ( 'LHFLUX  ' )  ! latent heat flux [W m^-2]
            DO j = 1, nx
              DO i = 1, ny
                qfx(j,i) = dum2d(i,j)
              ENDDO
            ENDDO
            gotqfx = .TRUE.
            WRITE (*,6000) var, qfx(lprt_metx, lprt_mety), 'W m^-2'

          CASE ( 'UST     ' )  ! frictional velocity [m s^-1]
            DO j = 1, nx
              DO i = 1, ny
                ust(j,i) = dum2d(i,j)
              ENDDO
            ENDDO
            gotust = .TRUE.
            WRITE (*,6000) var, ust(lprt_metx, lprt_mety), 'm s^-1'

          CASE ( 'SWDOWN  ' )  ! surface downward shortwave radiation [W m^-2]
            DO j = 1, nx
              DO i = 1, ny
                rgrnd(j,i) = dum2d(i,j)
              ENDDO
            ENDDO
            gotrgrnd = .TRUE.
            WRITE (*,6000) var, rgrnd(lprt_metx, lprt_mety), 'W m^-2'

          CASE ( 'LWDOWN  ' )  ! surface downward longwave radiation [W m^-2]
            DO j = 1, nx
              DO i = 1, ny
                glw(j,i) = dum2d(i,j)
              ENDDO
            ENDDO
            gotglw = .TRUE.
            WRITE (*,6000) var, glw(lprt_metx, lprt_mety), 'W m^-2'

          CASE ( 'SNOWCOVR' )  ! snow cover [1=yes, 0=no]
            DO j = 1, nx
              DO i = 1, ny
                snowcovr(j,i) = dum2d(i,j)
              ENDDO
            ENDDO
            gotsnowcov = .TRUE.
            WRITE (*,6000) var, snowcovr(lprt_metx, lprt_mety), 'dimensionless'

        END SELECT

      ENDIF keep2d

    ENDDO read2d

    IF ( keep == 1 ) THEN
      EXIT findtime
    ELSE
      CYCLE findtime
    ENDIF

!-------------------------------------------------------------------------------
! If the end-of-file marker was reached, try to open another input file to
! get the time period we need.
!-------------------------------------------------------------------------------

 999 CONTINUE

    CLOSE (iutmm)
    PRINT*, '+++++ End-of-file marker found on unit ', iutmm

    iutmm = iutmm + 1
    PRINT*, '+++++ Attempting to open unit ', iutmm

    mmfile = TRIM( file_mm(iutmm-iutmmi+1) )
    INQUIRE ( FILE=mmfile, EXIST=ifmm )
    IF ( .NOT. ifmm ) GOTO 8100

    OPEN (UNIT=iutmm,   FILE=mmfile, FORM='UNFORMATTED', STATUS='OLD',  &
          IOSTAT=istat, ERR=8200)

    newfile = .TRUE.

  ENDDO findtime
   
!-------------------------------------------------------------------------------
! Make sure we collected the arrays we need.
!-------------------------------------------------------------------------------

  IF ( .NOT. gotua ) THEN
    WRITE (6,9500) 'U'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotva ) THEN
    WRITE (6,9500) 'V'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotta ) THEN
    WRITE (6,9500) 'T'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotqva ) THEN
    WRITE (6,9500) 'Q'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotqca ) THEN
    IF ( met_expl_moist < 4 ) THEN  ! MM5 without ice-phase scheme
      qca(:,:,:) = 0.0
      gotqca = .TRUE.
    ELSE
      WRITE (6,9500) 'CLW'
      GOTO 1001
    ENDIF
  ENDIF

  IF ( .NOT. gotqra ) THEN
    IF ( met_expl_moist < 4 ) THEN  ! MM5 without ice-phase scheme
      qra(:,:,:) = 0.0
      gotqra = .TRUE.
    ELSE
      WRITE (6,9500) 'RNW'
      GOTO 1001
    ENDIF
  ENDIF

  IF ( .NOT. gotqia ) THEN
    IF ( met_expl_moist < 5 ) THEN  ! MM5 without mixed-phase scheme
      qia(:,:,:) = 0.0
      gotqia = .TRUE.
    ELSE
      WRITE (6,9500) 'ICE'
      GOTO 1001
    ENDIF
  ENDIF

  IF ( .NOT. gotqsa ) THEN
    IF ( met_expl_moist < 5 ) THEN  ! MM5 without mixed-phase scheme
      qsa(:,:,:) = 0.0
      gotqsa = .TRUE.
    ELSE
      WRITE (6,9500) 'SNOW'
      GOTO 1001
    ENDIF
  ENDIF

  IF ( .NOT. gotqga ) THEN
    IF ( met_expl_moist < 6 ) THEN  ! MM5 without graupel
      qga(:,:,:) = 0.0
      gotqga = .TRUE.
    ELSE
      WRITE (6,9500) 'GRAUPEL'
      GOTO 1001
    ENDIF
  ENDIF

  IF ( .NOT. gotwa ) THEN
    IF ( mif(5,6) /= 1 ) THEN  ! hydrostatic run
      wa(:,:,:) = 0.0
      gotwa = .TRUE.
    ELSE
      WRITE (6,9500) 'W'
      GOTO 1001
    ENDIF
  ENDIF

  IF ( .NOT. gotpp ) THEN
    IF ( mif(5,6) /= 1 ) THEN  ! hydrostatic run
      pp(:,:,:) = 0.0
      gotpp = .TRUE.
    ELSE
      WRITE (6,9500) 'PP'
      GOTO 1001
    ENDIF
  ENDIF

  IF ( .NOT. gotpsa ) THEN
    WRITE (6,9500) 'PSTARCRS'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotgroundt ) THEN
    WRITE (6,9500) 'GROUND T'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotraincon ) THEN
    WRITE (6,9500) 'RAIN CON'
    GOTO 1001
  ENDIF

  IF ( .NOT. gotrainnon ) THEN
    WRITE (6,9500) 'RAIN NON'
    GOTO 1001
  ENDIF

  IF ( first ) THEN

    IF ( .NOT. gotterrain ) THEN
      WRITE (6,9500) 'TERRAIN'
      GOTO 1001
    ENDIF

    IF ( .NOT. gotmapcrs ) THEN
      WRITE (6,9500) 'MAPFACCR'
      GOTO 1001
    ENDIF

    IF ( .NOT. gotmapdot ) THEN
      WRITE (6,9500) 'MAPFACDT'
      GOTO 1001
    ENDIF

    IF ( .NOT. gotlatcrs ) THEN
      WRITE (6,9500) 'LATITCRS'
      GOTO 1001
    ENDIF

    IF ( .NOT. gotloncrs ) THEN
      WRITE (6,9500) 'LATITDOT'
      GOTO 1001
    ENDIF

    IF ( .NOT. gotlanduse ) THEN
      WRITE (6,9500) 'LAND USE'
      GOTO 1001
    ENDIF

  ENDIF

  IF ( .NOT. gotzpbl ) THEN
    IF ( ( ( lpbl == 2 ) .OR. ( lpbl == 3 ) ) .AND. ( lrad == 2 ) ) THEN
      WRITE (6,9600) 'PBL HGT', lpbl, lrad
      zpbl(:,:) = 0.0
      gotzpbl = .TRUE.
    ELSE
      WRITE (6,9500) 'PBL HGT'
      GOTO 1001
    ENDIF
  ENDIF

  IF ( .NOT. gothfx ) THEN
    IF ( ( ( lpbl == 2 ) .OR. ( lpbl == 3 ) ) .AND. ( lrad == 2 ) ) THEN
      WRITE (6,9600) 'SHFLUX', lpbl, lrad
      hfx(:,:) = 0.0
      gothfx = .TRUE.
    ELSE
      WRITE (6,9500) 'SHFLUX'
      GOTO 1001
    ENDIF
  ENDIF

  IF ( .NOT. gotqfx ) THEN
    IF ( ( ( lpbl == 2 ) .OR. ( lpbl == 3 ) ) .AND. ( lrad == 2 ) ) THEN
      WRITE (6,9600) 'LHFLUX', lpbl, lrad
      qfx(:,:) = 0.0
      gotqfx = .TRUE.
    ELSE
      WRITE (6,9500) 'LHFLUX'
      GOTO 1001
    ENDIF
  ENDIF

  IF ( .NOT. gotust ) THEN
    IF ( ( ( lpbl == 2 ) .OR. ( lpbl == 3 ) ) .AND. ( lrad == 2 ) ) THEN
      WRITE (6,9600) 'UST', lpbl, lrad
      ust(:,:) = 0.0
      gotust = .TRUE.
    ELSE
      WRITE (6,9500) 'UST'
      GOTO 1001
    ENDIF
  ENDIF

  IF ( .NOT. gotrgrnd ) THEN
    IF ( ( ( lpbl == 2 ) .OR. ( lpbl == 3 ) ) .AND. ( lrad == 2 ) ) THEN
      WRITE (6,9600) 'SWDOWN', lpbl, lrad
      rgrnd(:,:) = 0.0
      gotrgrnd = .TRUE.
    ELSE
      WRITE (6,9500) 'SWDOWN'
      GOTO 1001
    ENDIF
  ENDIF

  IF ( .NOT. gotglw ) THEN
    IF ( ( ( lpbl == 2 ) .OR. ( lpbl == 3 ) ) .AND. ( lrad == 2 ) ) THEN
      WRITE (6,9600) 'LWDOWN', lpbl, lrad
      glw(:,:) = 0.0
      gotglw = .TRUE.
    ELSE
      WRITE (6,9500) 'LWDOWN'
      GOTO 1001
    ENDIF
  ENDIF

  IF ( .NOT. gotsnowcov ) THEN
    WRITE (6,9700) 'SNOWCOVR'
    snowcovr(:,:) = 0.0
    gotsnowcov = .TRUE.
  ENDIF

!-------------------------------------------------------------------------------
! Calculate dot-point p-star.
!-------------------------------------------------------------------------------

  CALL crs2dot (psa, psd)

!-------------------------------------------------------------------------------
! Decouple arrays by dividing by p-star.  Then convert p-star to standard units.
!-------------------------------------------------------------------------------

  decouple: DO k = 1, nz
    
    ua (:,:,k) = ua (:,:,k) / psd(:,:)
    va (:,:,k) = va (:,:,k) / psd(:,:)

    ta (:,:,k) = ta (:,:,k) / psa(:,:)
    qva(:,:,k) = qva(:,:,k) / psa(:,:)
    qca(:,:,k) = qca(:,:,k) / psa(:,:)
    qra(:,:,k) = qra(:,:,k) / psa(:,:)
    qia(:,:,k) = qia(:,:,k) / psa(:,:)
    qsa(:,:,k) = qsa(:,:,k) / psa(:,:)
    qga(:,:,k) = qga(:,:,k) / psa(:,:)
    pp (:,:,k) = pp (:,:,k) / psa(:,:)

  ENDDO decouple

  lbnd = LBOUND(wa,3)  ! first element is mapped correctly (0 or 1)
  ubnd = UBOUND(wa,3)  ! last element is mapped correctly  (nz or nz+1)

  decouple_w: DO k = lbnd, ubnd
    wa(:,:,k) = wa(:,:,k) / psa(:,:)
  ENDDO decouple_w

  psa = psa * 1000.0  ! convert p-star from kPa to Pa

  WRITE (*,ifmt1)  'UA       ', ua (lprt_metx, lprt_mety, :)
  WRITE (*,ifmt1)  'VA       ', va (lprt_metx, lprt_mety, :)
  WRITE (*,ifmt1)  'TA       ', ta (lprt_metx, lprt_mety, :)
  WRITE (*,ifmt1)  'QVA      ', qva(lprt_metx, lprt_mety, :)
  WRITE (*,ifmt1)  'QCA      ', qca(lprt_metx, lprt_mety, :)
  WRITE (*,ifmt1)  'QRA      ', qra(lprt_metx, lprt_mety, :)
  WRITE (*,ifmt1)  'QIA      ', qia(lprt_metx, lprt_mety, :)
  WRITE (*,ifmt1)  'QSA      ', qsa(lprt_metx, lprt_mety, :)
  WRITE (*,ifmt1)  'QGA      ', qga(lprt_metx, lprt_mety, :)
  WRITE (*,ifmt1a) 'WA       ', wa (lprt_metx, lprt_mety, :)
  WRITE (*,ifmt1)  'PP       ', pp (lprt_metx, lprt_mety, :)
  WRITE (*,6000)   'PSA     ',  psa(lprt_metx, lprt_mety), 'Pa'

!-------------------------------------------------------------------------------
! Get latitude and longitude on dot points.  Then, get roughness length,
! albedo, and emissivity from land use category.
!-------------------------------------------------------------------------------

  IF ( first ) THEN

    ALLOCATE ( dum2d2 (nx, ny) )      ! (X,Y)

    CALL gridgeometry (0, dum2d2, mapdot, londot, latdot)

    DEALLOCATE ( dum2d2 )

    ! See which MM5 "season" we are in.
 
    startseas = startdate(1:4) // "-04-15-00:00:00.0000"
    endseas   = startdate(1:4) // "-10-15-00:00:00.0000"

    CALL geth_idts ( startdate, startseas, idts_start )
    CALL geth_idts ( endseas,   startdate, idts_end )

    IF ( ( idts_start < 0 ) .OR. ( idts_end < 0 ) ) THEN
      IF ( met_y_centd >= 0.0 ) THEN  ! Northern Hemisphere
        met_season = 2   ! winter
      ELSE  ! Southern Hemisphere
        met_season = 1   ! summer
      ENDIF
    ELSE
      IF ( met_y_centd >= 0.0 ) THEN  ! Northern Hemisphere
        met_season = 1   ! summer
      ELSE
        met_season = 2   ! winter
      ENDIF
    ENDIF

    DO j = 1, ny-1
      DO i = 1, nx-1
        IF ( ( met_lu_water == 7 ) .OR. ( met_lu_water == -999 ) ) THEN
          znt   (i,j) = sfz013(landuse(i,j),met_season) * 0.01  ! sfz0: [cm --> m]
          albedo(i,j) = albd13(landuse(i,j),met_season)
          emiss (i,j) = sfem13(landuse(i,j),met_season) * 0.01  ! sfem: [% --> dec]
        ELSE IF ( met_lu_water == 16 ) THEN
          znt   (i,j) = sfz024(landuse(i,j),met_season) * 0.01  ! sfz0: [cm --> m]
          albedo(i,j) = albd24(landuse(i,j),met_season)
          emiss (i,j) = sfem24(landuse(i,j),met_season) * 0.01  ! sfem: [% --> dec]
        ENDIF
      ENDDO
    ENDDO
    znt   (:,ny) = znt   (:,ny-1)
    znt   (nx,:) = znt   (nx-1,:)
    albedo(:,ny) = albedo(:,ny-1)
    albedo(nx,:) = albedo(nx-1,:)
    emiss (:,ny) = emiss (:,ny-1)
    emiss (nx,:) = emiss (nx-1,:)

    WRITE (*,6000) 'ZNT     ', znt   (lprt_metx, lprt_mety), 'm'
    WRITE (*,6000) 'ALBEDO  ', albedo(lprt_metx, lprt_mety), 'fraction'
    WRITE (*,6000) 'EMISS   ', emiss (lprt_metx, lprt_mety), 'fraction'

    first = .FALSE.

  ENDIF

!-------------------------------------------------------------------------------
! Deallocate arrays.
!-------------------------------------------------------------------------------

! DEALLOCATE ( dum2d )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum3d )  ! commented out to avoid memory fragmentation
  DEALLOCATE ( mif   )
  DEALLOCATE ( mifc  )
  DEALLOCATE ( mrf   )
! DEALLOCATE ( psd   )  ! commented out to avoid memory fragmentation

  RETURN

!-------------------------------------------------------------------------------
! Format statements.
!-------------------------------------------------------------------------------

 6000 FORMAT (4x, a, 1x, f11.4, 2x, a)
 6100 FORMAT (4x, a, 1x, i11,   2x, a)

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8000 WRITE (6,9000) iutmm, TRIM(var), istat
      GOTO 1001

 8100 WRITE (6,9100) mcip_now, iutmm
      GOTO 1001

 8200 WRITE (6,9200) iutmm, istat
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V2',                     &
              /, 1x, '***   ERROR READING FILE, UNIT = ', i3,       &
              /, 1x, '***   VARIABLE = ', a,                        &
              /, 1x, '***   IOSTAT = ', i4,                         &
              /, 1x, 70('*'))

 9050 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V2',                     &
              /, 1x, '***   DOES NOT APPEAR TO BE MM5 OUTPUT FILE', &
              /, 1x, '***   INDEX = ', i4,                          &
              /, 1x, 70('*'))

 9075 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V2',                     &
              /, 1x, '***   UNKNOWN LAND USE DATA SOURCE FROM MM5', &
              /, 1x, '***   WATER CATEGORY IS ', i4,                &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V2',                     &
              /, 1x, '***   LOOKING FOR INPUT MET AT TIME ', a,     &
              /, 1x, '***   COULD NOT FIND INPUT MET ON UNIT ', i3, &
              /, 1x, '***   FILE MAY NOT EXIST',                    &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V2',                     &
              /, 1x, '***   ERROR OPENING MM5 FILE ON UNIT ', i3,   &
              /, 1x, '***   IOSTAT = ', i4,                         &
              /, 1x, 70('*'))

 9300 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V2',                     &
              /, 1x, '***   MM5 FILES DO NOT SEEM TO BE SAME GRID', &
              /, 1x, '***   CHECK FORTRAN UNITS ', i3, ' AND ', i3, &
              /, 1x, 70('*'))

 9400 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V2',                     &
              /, 1x, '***   MM5 FILES MAY HAVE A DISCONTINUITY',    &
              /, 1x, '***   CHECK FORTRAN UNITS ', i3, ' AND ', i3, &
              /, 1x, 70('*'))

 9500 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V2',                     &
              /, 1x, '***   DID NOT FIND ARRAY ', a,                &
              /, 1x, 70('*'))

 9600 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V2',                     &
              /, 1x, '***   DID NOT FIND ARRAY ', a,                &
              /, 1x, '***   WILL CALCULATE WITH LPBL, LRAD = ', 2i3,&
              /, 1x, 70('*'))

 9700 FORMAT (/, 1x, 70('*'),                                       &
              /, 1x, '*** SUBROUTINE: RDMM5V2',                     &
              /, 1x, '***   DID NOT FIND ARRAY ', a,                &
              /, 1x, '***   WILL SET TO 0.0 FOR ALL GRID CELLS',    &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE rdmm5v2
