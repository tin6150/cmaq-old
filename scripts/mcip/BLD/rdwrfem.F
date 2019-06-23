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

SUBROUTINE rdwrfem (mcip_now)

!-------------------------------------------------------------------------------
! Name:     Read WRFv2.0 (Eulerian Mass Core) Output
! Purpose:  Reads incoming WRF2.0 output files for use in MCIP.
! Notes:    Adapted from S.-B. Kim's get_wrf.F in WCIP.
! Revised:  31 Mar 2005  Original version.  (T. Otte)
!           15 Jul 2005  Modified variable retrievals so that the code will
!                        stop if a variable is not found.  Corrected print
!                        statement for sample output.  (T. Otte)
!-------------------------------------------------------------------------------

  USE date_pack
  USE file
  USE metinfo, nx => met_nx, ny => met_ny, nz => met_nz
  USE metvars
  USE mcipparm
  USE wrf_netcdf

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER                      :: cdfid
  REAL,    SAVE, ALLOCATABLE   :: dum2d      ( : , : )
  REAL,          ALLOCATABLE   :: dum2d_d    ( : , : )
  REAL,    SAVE, ALLOCATABLE   :: dum3d_p    ( : , : , : )
  REAL,    SAVE, ALLOCATABLE   :: dum3d_t    ( : , : , : )
  REAL,    SAVE, ALLOCATABLE   :: dum3d_u    ( : , : , : )
  REAL,    SAVE, ALLOCATABLE   :: dum3d_v    ( : , : , : )
  REAL,    SAVE, ALLOCATABLE   :: dum3d_w    ( : , : , : )
  CHARACTER*24                 :: endseas
  LOGICAL,       SAVE          :: first      = .TRUE.
  CHARACTER*256                :: fl
  INTEGER                      :: i
  INTEGER                      :: idts_end
  INTEGER                      :: idts_start
  INTEGER                      :: idtsec
  LOGICAL                      :: iffl
  CHARACTER*64                 :: ifmt1
  CHARACTER*64                 :: ifmt1a
  INTEGER                      :: it
  INTEGER,       SAVE          :: it_start
  INTEGER                      :: k1
  INTEGER                      :: k2
  INTEGER,       PARAMETER     :: max_times  = 150
  CHARACTER*24,  INTENT(IN)    :: mcip_now
  INTEGER,       SAVE          :: mmcount    = 1
  INTEGER,       SAVE          :: n_times
  LOGICAL,       SAVE          :: newfile    = .TRUE.
  INTEGER                      :: nxm
  INTEGER                      :: nym
  INTEGER                      :: nzp
  CHARACTER*16,  PARAMETER     :: pname      = 'RDWRFEM'
  INTEGER                      :: rcode
  REAL,          PARAMETER     :: rdovcp     = 2.0 / 7.0
  CHARACTER*24                 :: startseas
  CHARACTER*2                  :: str1
  CHARACTER*2                  :: str2
  CHARACTER*80,  SAVE          :: times      ( max_times )
  INTEGER,       PARAMETER     :: ttol       = 300  ! [sec]

  INTERFACE

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
! Define additional staggered grid dimensions.
!-------------------------------------------------------------------------------

  nxm = nx - 1
  nym = ny - 1
  nzp = nz + 1

!-------------------------------------------------------------------------------
! Set up print statements.
!-------------------------------------------------------------------------------

  k1 = nz / 5
  k2 = MOD(nz, 5)

  WRITE ( str1, '(i2)' ) k1 - 1
  WRITE ( str2, '(i2)' ) k2

  IF ( (k1 - 1) > 0 ) THEN
    IF ( k2 > 0 ) THEN
      ifmt1 = "(/,1x,a,5(1x,f12.4)," // str1 // "(/,10x,5(1x,f12.4)),/,10x,"   &
         &    // str2 // "(1x,f12.4))"
    ELSE
      ifmt1 = "(/,1x,a,5(1x,f12.4)," // str1 // "(/,10x,5(1x,f12.4)))"
    ENDIF
  ELSE
    IF ( k2 > 0 ) THEN
      ifmt1 = "(/,1x,a,5(1x,f12.4),/,11x," // str2 // "(1x,f12.4))"
    ELSE
      ifmt1 = "(/,1x,a,5(1x,f12.4))"
    ENDIF
  ENDIF

  k1 = (nzp) / 5
  k2 = MOD(nzp, 5)

  WRITE ( str1, '(i2)' ) k1 - 1
  WRITE ( str2, '(i2)' ) k2

  IF ( (k1 - 1) > 0 ) THEN
    IF ( k2 > 0 ) THEN
      ifmt1a = "(/,1x,a,5(1x,f12.4)," // str1 // "(/,10x,5(1x,f12.4)),/,10x,"   &
         &     // str2 // "(1x,f12.4))"
    ELSE
      ifmt1a = "(/,1x,a,5(1x,f12.4)," // str1 // "(/,10x,5(1x,f12.4)))"
    ENDIF
  ELSE
    IF ( k2 > 0 ) THEN
      ifmt1a = "(/,1x,a,5(1x,f12.4),/,10x," // str2 // "(1x,f12.4))"
    ELSE
      ifmt1a = "(/,1x,a,5(1x,f12.4))"
    ENDIF
  ENDIF

!-------------------------------------------------------------------------------
! Allocate necessary variables.
!-------------------------------------------------------------------------------

  IF ( .NOT. ALLOCATED ( dum2d   ) )  & 
    ALLOCATE ( dum2d   (nxm, nym)      )  ! 2D array on cross points
  IF ( .NOT. ALLOCATED ( dum3d_p ) )  &
    ALLOCATE ( dum3d_p (nxm, nym, nz ) )  ! 3D array on cross points, half lvls
  IF ( .NOT. ALLOCATED ( dum3d_t ) )  &
    ALLOCATE ( dum3d_t (nxm, nym, nz ) )  ! 3D array on cross points, half lvls
  IF ( .NOT. ALLOCATED ( dum3d_u ) )  &
    ALLOCATE ( dum3d_u (nx,  nym, nz ) )  ! 3D array on E-W flux pts, half lvls
  IF ( .NOT. ALLOCATED ( dum3d_v ) )  &
    ALLOCATE ( dum3d_v (nxm, ny,  nz ) )  ! 3D array on N-S flux pts, half lvls
  IF ( .NOT. ALLOCATED ( dum3d_w ) )  &
    ALLOCATE ( dum3d_w (nxm, nym, nzp) )  ! 3D array on cross points, full lvls

!-------------------------------------------------------------------------------
! Find time index (IT) for MCIP_NOW in WRF output file.
!-------------------------------------------------------------------------------

  fl = file_mm(mmcount)

  findit: DO
    IF ( newfile ) THEN
      CALL get_times_cdf (fl, times, n_times, max_times, rcode)
      IF ( rcode == 0 ) THEN
        newfile  = .FALSE.
        it_start = 1
      ELSE
        WRITE (6,9000) rcode
        GOTO 1001
      ENDIF
    ENDIF
    DO i = it_start, n_times
      CALL geth_idts (times(i), mcip_now, idtsec)
      IF ( ABS(idtsec) <= ttol ) THEN  ! found MCIP_NOW in WRF output
        it = i
        IF ( i < n_times ) it_start = i + 1
        EXIT findit
      ENDIF
    ENDDO
    IF ( i > n_times ) THEN
      newfile = .TRUE.
      mmcount = mmcount + 1
      IF ( mmcount > max_mm ) THEN
        WRITE (6,9100) mcip_now
        GOTO 1001
      ENDIF
      fl = file_mm(mmcount)
      IF ( fl(1:10) == '          ' ) THEN
        WRITE (6,9200) mcip_now, mmcount
        GOTO 1001
      ENDIF
      INQUIRE (FILE=fl, EXIST=iffl)
      IF ( .NOT. iffl ) THEN
        WRITE (6,9300) mcip_now, TRIM(fl)
        GOTO 1001
      ENDIF
    ENDIF
  ENDDO findit

!-------------------------------------------------------------------------------
! Read WRF data for this domain.
!-------------------------------------------------------------------------------

  CALL get_var_3d_real_cdf (fl, 'U',        dum3d_u, nx,  nym, nz,  it, rcode)
  IF ( rcode == 0 ) THEN
    ua(:,1:nym,:) = dum3d_u(:,:,:)
    ua(:,  ny, :) = ua(:,nym,:)
    WRITE (*,ifmt1) 'U        ', ua(lprt_metx,lprt_mety,:)
  ELSE
    WRITE (6,9400) 'U', rcode
    GOTO 1001
  ENDIF

  CALL get_var_3d_real_cdf (fl, 'V',        dum3d_v, nxm, ny,  nz,  it, rcode)
  IF ( rcode == 0 ) THEN
    va(1:nxm,:,:) = dum3d_v(:,:,:)
    va(  nx, :,:) = va(nxm,:,:)
    WRITE (*,ifmt1) 'V        ', va(lprt_metx,lprt_mety,:)
  ELSE
    WRITE (6,9400) 'V', rcode
    GOTO 1001
  ENDIF

  CALL get_var_3d_real_cdf (fl, 'W',        dum3d_w, nxm, nym, nzp, it, rcode)
  IF ( rcode == 0 ) THEN
    wa(1:nxm,1:nym,:) = dum3d_w(:,:,:)
    wa(  nx,  :,   :) = wa(nxm,:,:)
    wa( :,     ny, :) = wa(:,nym,:)
    WRITE (*,ifmt1a) 'W        ', wa(lprt_metx,lprt_mety,:)
  ELSE
    WRITE (6,9400) 'W', rcode
    GOTO 1001
  ENDIF

  CALL get_var_3d_real_cdf (fl, 'PH',       dum3d_w, nxm, nym, nzp ,it, rcode)
  IF ( rcode == 0 ) THEN
    ph(1:nxm,1:nym,:) = dum3d_w(:,:,:)
    ph(  nx,  :,   :) = ph(nxm,:,:)
    ph( :,     ny, :) = ph(:,nym,:)
    WRITE (*,ifmt1a) 'PH       ', ph(lprt_metx,lprt_mety,:)
  ELSE
    WRITE (6,9400) 'PH', rcode
    GOTO 1001
  ENDIF

  CALL get_var_3d_real_cdf (fl, 'PHB',      dum3d_w, nxm, nym, nzp, it, rcode)
  IF ( rcode == 0 ) THEN
    phb(1:nxm,1:nym,:) = dum3d_w(:,:,:)
    phb(  nx,  :,   :) = phb(nxm,:,:)
    phb( :,     ny, :) = phb(:,nym,:)
    WRITE (*,ifmt1a) 'PHB      ', phb(lprt_metx,lprt_mety,:)
  ELSE
    WRITE (6,9400) 'PHB', rcode
    GOTO 1001
  ENDIF

  CALL get_var_3d_real_cdf (fl, 'P',        dum3d_p, nxm, nym, nz,  it, rcode)
  IF ( rcode == 0 ) THEN
    pp(1:nxm,1:nym,:) = dum3d_p(:,:,:)
    pp(  nx,  :,   :) = pp(nxm,:,:)
    pp( :,     ny, :) = pp(:,nym,:)
    WRITE (*,ifmt1) 'P        ', pp(lprt_metx,lprt_mety,:)
  ELSE
    WRITE (6,9400) 'P', rcode
    GOTO 1001
  ENDIF

  CALL get_var_3d_real_cdf (fl, 'PB',       dum3d_p, nxm, nym, nz,  it, rcode)
  IF ( rcode == 0 ) THEN
    pb(1:nxm,1:nym,:) = dum3d_p(:,:,:)
    pb(  nx,  :,   :) = pb(nxm,:,:)
    pb( :,     ny, :) = pb(:,nym,:)
    WRITE (*,ifmt1) 'PB       ', pb(lprt_metx,lprt_mety,:)
  ELSE
    WRITE (6,9400) 'PB', rcode
    GOTO 1001
  ENDIF

  CALL get_var_3d_real_cdf (fl, 'T',        dum3d_t, nxm, nym, nz,  it, rcode)
  IF ( rcode == 0 ) THEN
    dum3d_p(:,:,:)    = dum3d_p(:,:,:) + pp(1:nxm,1:nym,:)   ! pressure [Pa]
    dum3d_t(:,:,:)    = dum3d_t(:,:,:) + 300.0               ! theta [K]
    ta(1:nxm,1:nym,:) = dum3d_t(:,:,:) * (dum3d_p(:,:,:)/100000.0)**rdovcp
    ta(  nx,  :,   :) = ta(nxm,:,:)
    ta( :,     ny, :) = ta(:,nym,:)
    WRITE (*,ifmt1) 'T        ', ta(lprt_metx,lprt_mety,:)
  ELSE
    WRITE (6,9400) 'T', rcode
    GOTO 1001
  ENDIF

  CALL get_var_3d_real_cdf (fl, 'QVAPOR',   dum3d_t, nxm, nym, nz,  it, rcode)
  IF ( rcode == 0 ) THEN
    qva(1:nxm,1:nym,:) = dum3d_t(:,:,:)
    qva(  nx,  :,   :) = qva(nxm,:,:)
    qva( :,     ny, :) = qva(:,nym,:)
    WRITE (*,ifmt1) 'QVAPOR   ', qva(lprt_metx,lprt_mety,:)
  ELSE
    WRITE (6,9400) 'QVAPOR', rcode
    GOTO 1001
  ENDIF

  CALL get_var_3d_real_cdf (fl, 'QCLOUD',   dum3d_t, nxm, nym, nz,  it, rcode)
  IF ( rcode == 0 ) THEN
    qca(1:nxm,1:nym,:) = dum3d_t(:,:,:)
    qca(  nx,  :,   :) = qca(nxm,:,:)
    qca( :,     ny, :) = qca(:,nym,:)
    WRITE (*,ifmt1) 'QCLOUD   ', qca(lprt_metx,lprt_mety,:)
  ELSE
    WRITE (6,9400) 'QCLOUD', rcode
    GOTO 1001
  ENDIF

  CALL get_var_3d_real_cdf (fl, 'QRAIN',    dum3d_t, nxm, nym, nz,  it, rcode)
  IF ( rcode == 0 ) THEN
    qra(1:nxm,1:nym,:) = dum3d_t(:,:,:)
    qra(  nx,  :,   :) = qra(nxm,:,:)
    qra( :,     ny, :) = qra(:,nym,:)
    WRITE (*,ifmt1) 'QRAIN    ', qra(lprt_metx,lprt_mety,:)
  ELSE
    WRITE (6,9400) 'QRAIN', rcode
    GOTO 1001
  ENDIF

  cdfid = ncopn (fl, ncnowrit, rcode)
  rcode = nf_inq_varid (cdfid, 'QICE', rcode)
  IF ( rcode == 0 ) THEN
    CALL get_var_3d_real_cdf (fl, 'QICE',   dum3d_t, nxm, nym, nz,  it, rcode)
    IF ( rcode == 0 ) THEN
      qia(1:nxm,1:nym,:) = dum3d_t(:,:,:)
      qia(  nx,  :,   :) = qia(nxm,:,:)
      qia( :,     ny, :) = qia(:,nym,:)
      WRITE (*,ifmt1) 'QICE     ', qia(lprt_metx,lprt_mety,:)
    ELSE
      WRITE (6,9400) 'QICE', rcode
      GOTO 1001
    ENDIF
  ELSE
    qia(:,:,:) = 0.0
  ENDIF
  CALL ncclos (cdfid, rcode)

  cdfid = ncopn (fl, ncnowrit, rcode)
  rcode = nf_inq_varid (cdfid, 'QSNOW', rcode)
  IF ( rcode == 0 ) THEN
    CALL get_var_3d_real_cdf (fl, 'QSNOW',  dum3d_t, nxm, nym, nz,  it, rcode)
    IF ( rcode == 0 ) THEN
      qsa(1:nxm,1:nym,:) = dum3d_t(:,:,:)
      qsa(  nx,  :,   :) = qsa(nxm,:,:)
      qsa( :,     ny, :) = qsa(:,nym,:)
      WRITE (*,ifmt1) 'QSNOW    ', qsa(lprt_metx,lprt_mety,:)
    ELSE
      WRITE (6,9400) 'QSNOW', rcode
      GOTO 1001
    ENDIF
  ELSE
    qsa(:,:,:) = 0.0
  ENDIF
  CALL ncclos (cdfid, rcode)

  cdfid = ncopn (fl, ncnowrit, rcode)
  rcode = nf_inq_varid (cdfid, 'QGRAUP', rcode)
  IF ( rcode == 0 ) THEN
    CALL get_var_3d_real_cdf (fl, 'QGRAUP', dum3d_t, nxm, nym, nz,  it, rcode)
    IF ( rcode == 0 ) THEN
      qga(1:nxm,1:nym,:) = dum3d_t(:,:,:)
      qga(  nx,  :,   :) = qga(nxm,:,:)
      qga( :,     ny, :) = qga(:,nym,:)
      WRITE (*,ifmt1) 'QGRAUP   ', qga(lprt_metx,lprt_mety,:)
    ELSE
      WRITE (6,9400) 'QGRAUP', rcode
      GOTO 1001
    ENDIF
  ELSE
    qga(:,:,:) = 0.0
  ENDIF
  CALL ncclos (cdfid, rcode)

  CALL get_var_2d_real_cdf (fl, 'MU',       dum2d, nxm, nym, it, rcode)
  IF ( rcode == 0 ) THEN
    mu(1:nxm,1:nym) = dum2d(:,:)
    mu(nx,:) = mu(nxm,:)
    mu(:,ny) = mu(:,nym)
    WRITE (*,6000) 'MU       ', mu(lprt_metx, lprt_mety), 'Pa'
  ELSE
    WRITE (6,9400) 'MU', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (fl, 'MUB',      dum2d, nxm, nym, it, rcode)
  IF ( rcode == 0 ) THEN
    mub(1:nxm,1:nym) = dum2d(:,:)
    mub(nx,:) = mub(nxm,:)
    mub(:,ny) = mub(:,nym)
    WRITE (*,6000) 'MUB      ', mub(lprt_metx, lprt_mety), 'Pa'
  ELSE
    WRITE (6,9400) 'MUB', rcode
    GOTO 1001
  ENDIF

  IF ( ift2m ) THEN
    CALL get_var_2d_real_cdf (fl, 'T2',     dum2d, nxm, nym, it, rcode)
    IF ( rcode == 0 ) THEN
      t2(1:nxm,1:nym) = dum2d(:,:)
      t2(nx,:) = t2(nxm,:)
      t2(:,ny) = t2(:,nym)
      WRITE (*,6000) 'T2       ', t2(lprt_metx, lprt_mety), 'K'
    ELSE
      WRITE (6,9400) 'T2', rcode
      GOTO 1001
    ENDIF
  ENDIF

  IF ( ifw10m ) THEN
    CALL get_var_2d_real_cdf (fl, 'U10',    dum2d, nxm, nym, it, rcode)
    IF ( rcode == 0 ) THEN
      u10(1:nxm,1:nym) = dum2d(:,:)
      u10(nx,:) = u10(nxm,:)
      u10(:,ny) = u10(:,nym)
      WRITE (*,6000) 'U10      ', u10(lprt_metx, lprt_mety), 'K'
    ELSE
      WRITE (6,9400) 'U10', rcode
      GOTO 1001
    ENDIF
    CALL get_var_2d_real_cdf (fl, 'V10',    dum2d, nxm, nym, it, rcode)
    IF ( rcode == 0 ) THEN
      v10(1:nxm,1:nym) = dum2d(:,:)
      v10(nx,:) = v10(nxm,:)
      v10(:,ny) = v10(:,nym)
      WRITE (*,6000) 'V10      ', v10(lprt_metx, lprt_mety), 'K'
    ELSE
      WRITE (6,9400) 'V10', rcode
      GOTO 1001
    ENDIF
  ENDIF

  CALL get_var_2d_real_cdf (fl, 'PSFC',     dum2d, nxm, nym, it, rcode)
  IF ( rcode == 0 ) THEN
    psa(1:nxm,1:nym) = dum2d(:,:)
    psa(nx,:) = psa(nxm,:)
    psa(:,ny) = psa(:,nym)
    WRITE (*,6000) 'PSFC     ', psa(lprt_metx, lprt_mety), 'Pa'
  ELSE
    WRITE (6,9400) 'PSFC', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (fl, 'MAPFAC_M', dum2d, nxm, nym, it, rcode)
  IF ( rcode == 0 ) THEN
    mapcrs(1:nxm,1:nym) = dum2d(:,:)
    mapcrs(nx,:) = mapcrs(nxm,:)
    mapcrs(:,ny) = mapcrs(:,nym)
    WRITE (*,6000) 'MAPFAC_M ', mapcrs(lprt_metx, lprt_mety), 'dimensionless'
  ELSE
    WRITE (6,9400) 'MAPFAC_M', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (fl, 'HGT',      dum2d, nxm, nym, it, rcode)
  IF ( rcode == 0 ) THEN
    terrain(1:nxm,1:nym) = dum2d(:,:)
    terrain(nx,:) = terrain(nxm,:)
    terrain(:,ny) = terrain(:,nym)
    WRITE (*,6000) 'HGT      ', terrain(lprt_metx, lprt_mety), 'm'
  ELSE
    WRITE (6,9400) 'HGT', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (fl, 'TSK',      dum2d, nxm, nym, it, rcode)
  IF ( rcode == 0 ) THEN
    groundt(1:nxm,1:nym) = dum2d(:,:)
    groundt(nx,:) = groundt(nxm,:)
    groundt(:,ny) = groundt(:,nym)
    WRITE (*,6000) 'TSK      ', groundt(lprt_metx, lprt_mety), 'K'
  ELSE
    WRITE (6,9400) 'TSK', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (fl, 'RAINC',    dum2d, nxm, nym, it, rcode)
  IF ( rcode == 0 ) THEN
    raincon(1:nxm,1:nym) = MAX(0.0, (dum2d(:,:) - rcold(1:nxm,1:nym))/10.0 )
    raincon(nx,:) = raincon(nxm,:)
    raincon(:,ny) = raincon(:,nym)
    rcold(1:nxm,1:nym) = dum2d(:,:)
    WRITE (*,6000) 'RAINC    ', raincon(lprt_metx, lprt_mety), 'cm'
  ELSE
    WRITE (6,9400) 'RAINC', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (fl, 'RAINNC',   dum2d, nxm, nym, it, rcode)
  IF ( rcode == 0 ) THEN
    rainnon(1:nxm,1:nym) = MAX(0.0, (dum2d(:,:) - rnold(1:nxm,1:nym))/10.0 )
    rainnon(nx,:) = rainnon(nxm,:)
    rainnon(:,ny) = rainnon(:,nym)
    rnold(1:nxm,1:nym) = dum2d(:,:)
    WRITE (*,6000) 'RAINNC   ', rainnon(lprt_metx, lprt_mety), 'cm'
  ELSE
    WRITE (6,9400) 'RAINNC', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (fl, 'SWDOWN',   dum2d, nxm, nym, it, rcode)
  IF ( rcode == 0 ) THEN
    rgrnd(1:nxm,1:nym) = dum2d(:,:)
    rgrnd(nx,:) = rgrnd(nxm,:)
    rgrnd(:,ny) = rgrnd(:,nym)
    WRITE (*,6000) 'SWDOWN   ', rgrnd(lprt_metx, lprt_mety), 'W m^-2'
  ELSE
    WRITE (6,9400) 'SWDOWN', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (fl, 'GLW',      dum2d, nxm, nym, it, rcode)
  IF ( rcode == 0 ) THEN
    glw(1:nxm,1:nym) = dum2d(:,:)
    glw(nx,:) = glw(nxm,:)
    glw(:,ny) = glw(:,nym)
    WRITE (*,6000) 'GLW      ', glw(lprt_metx, lprt_mety), 'W m^-2'
  ELSE
    WRITE (6,9400) 'GLW', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (fl, 'XLAT',     dum2d, nxm, nym, it, rcode)
  IF ( rcode == 0 ) THEN
    latcrs(1:nxm,1:nym) = dum2d(:,:)
    latcrs(nx,:) = latcrs(nxm,:)
    latcrs(:,ny) = latcrs(:,nym)
    WRITE (*,6000) 'XLAT     ', latcrs(lprt_metx, lprt_mety), 'degrees'
  ELSE
    WRITE (6,9400) 'XLAT', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (fl, 'XLONG',    dum2d, nxm, nym, it, rcode)
  IF ( rcode == 0 ) THEN
    loncrs(1:nxm,1:nym) = dum2d(:,:)
    loncrs(nx,:) = loncrs(nxm,:)
    loncrs(:,ny) = loncrs(:,nym)
    WRITE (*,6000) 'XLONG    ', loncrs(lprt_metx, lprt_mety), 'degrees'
  ELSE
    WRITE (6,9400) 'XLONG', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (fl, 'LU_INDEX', dum2d, nxm, nym, it, rcode)
  IF ( rcode == 0 ) THEN
    landuse(1:nxm,1:nym) = NINT(dum2d(:,:))
    landuse(nx,:) = landuse(nxm,:)
    landuse(:,ny) = landuse(:,nym)
    WRITE (*,6100) 'LU_INDEX ', landuse(lprt_metx, lprt_mety), 'category'
  ELSE
    WRITE (6,9400) 'LU_INDEX', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (fl, 'HFX',      dum2d, nxm, nym, it, rcode)
  IF ( rcode == 0 ) THEN
    hfx(1:nxm,1:nym) = dum2d(:,:)
    hfx(nx,:) = hfx(nxm,:)
    hfx(:,ny) = hfx(:,nym)
    WRITE (*,6000) 'HFX      ', hfx(lprt_metx, lprt_mety), 'W m^-2'
  ELSE
    WRITE (6,9400) 'HFX', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (fl, 'LH',       dum2d, nxm, nym, it, rcode)
  IF ( rcode == 0 ) THEN
    qfx(1:nxm,1:nym) = dum2d(:,:)
    qfx(nx,:) = qfx(nxm,:)
    qfx(:,ny) = qfx(:,nym)
    WRITE (*,6000) 'LH       ', qfx(lprt_metx, lprt_mety), 'W m^-2'
  ELSE
    WRITE (6,9400) 'LH', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (fl, 'UST',      dum2d, nxm, nym, it, rcode)
  IF ( rcode == 0 ) THEN
    ust(1:nxm,1:nym) = dum2d(:,:)
    ust(nx,:) = ust(nxm,:)
    ust(:,ny) = ust(:,nym)
    WRITE (*,6000) 'UST      ', ust(lprt_metx, lprt_mety), 'm'
  ELSE
    WRITE (6,9400) 'UST', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (fl, 'PBLH',     dum2d, nxm, nym, it, rcode)
  IF ( rcode == 0 ) THEN
    zpbl(1:nxm,1:nym) = dum2d(:,:)
    zpbl(nx,:) = zpbl(nxm,:)
    zpbl(:,ny) = zpbl(:,nym)
    WRITE (*,6000) 'PBLH     ', zpbl(lprt_metx, lprt_mety), 'm'
  ELSE
    WRITE (6,9400) 'PBLH', rcode
    GOTO 1001
  ENDIF

  IF ( first ) THEN
    CALL get_var_2d_real_cdf (fl, 'ALBEDO', dum2d, nxm, nym, it, rcode)
    IF ( rcode == 0 ) THEN
      albedo(1:nxm,1:nym) = dum2d(:,:)
      albedo(nx,:) = albedo(nxm,:)
      albedo(:,ny) = albedo(:,nym)
      WRITE (*,6000) 'ALBEDO   ', albedo(lprt_metx, lprt_mety), 'fraction'
    ELSE
      WRITE (6,9400) 'ALBEDO', rcode
      GOTO 1001
    ENDIF
    CALL get_var_2d_real_cdf (fl, 'EMISS',  dum2d, nxm, nym, it, rcode)
    IF ( rcode == 0 ) THEN
      emiss(1:nxm,1:nym) = dum2d(:,:)
      emiss(nx,:) = emiss(nxm,:)
      emiss(:,ny) = emiss(:,nym)
      WRITE (*,6000) 'EMISS    ', emiss(lprt_metx, lprt_mety),  'fraction'
    ELSE
      WRITE (6,9400) 'EMISS', rcode
      GOTO 1001
    ENDIF
  ENDIF

  CALL get_var_2d_real_cdf (fl, 'ZNT',    dum2d, nxm, nym, it, rcode)
  IF ( rcode == 0 ) THEN
    znt(1:nxm,1:nym) = dum2d(:,:)
    znt(nx,:) = znt(nxm,:)
    znt(:,ny) = znt(:,nym)
    WRITE (*,6000) 'ZNT      ', znt(lprt_metx, lprt_mety),    'm'
  ELSE
    WRITE (6,9400) 'ZNT', rcode
    GOTO 1001
  ENDIF

  CALL get_var_2d_real_cdf (fl, 'SNOWC',    dum2d, nxm, nym, it, rcode)
  IF ( rcode == 0 ) THEN
    snowcovr(1:nxm,1:nym) = dum2d(:,:)
    snowcovr(nx,:) = snowcovr(nxm,:)
    snowcovr(:,ny) = snowcovr(:,nym)
    WRITE (*,6000) 'SNOWC    ', snowcovr(lprt_metx, lprt_mety), 'category'
  ELSE
    WRITE (6,9400) 'SNOWC', rcode
    GOTO 1001
  ENDIF

  CALL get_var_1d_real_cdf (fl, 'ZNU', sigmah, nz,  it, rcode)
  IF ( rcode /= 0 ) THEN
    WRITE (6,9400) 'ZNU', rcode
    GOTO 1001
  ENDIF

  CALL get_var_1d_real_cdf (fl, 'ZNW', sigmaf, nzp, it, rcode)
  IF ( rcode /= 0 ) THEN
    WRITE (6,9400) 'ZNW', rcode
    GOTO 1001
  ENDIF

!-------------------------------------------------------------------------------
! Get latitude and longitude on dot points, and determine season to set LAI.
!-------------------------------------------------------------------------------

  IF ( first ) THEN

    ALLOCATE ( dum2d_d (nx, ny) )
    CALL gridgeometry (0, dum2d_d, mapdot, londot, latdot)
    DEALLOCATE ( dum2d_d )

    IF ( .NOT. px ) THEN

      ! See which "season" we are in to get LAI.

      startseas = met_startdate(1:4) // "-04-15-00:00:00.0000"
      endseas   = met_startdate(1:4) // "-10-15-00:00:00.0000"

      CALL geth_idts (met_startdate, startseas,     idts_start)
      CALL geth_idts (endseas,       met_startdate, idts_end)

      IF ( ( idts_start < 0 ) .OR. ( idts_end < 0 ) ) THEN
        met_season = 2  ! winter
      ELSE
        met_season = 1  ! summer
      ENDIF

    ENDIF

    first = .FALSE.

  ENDIF
  
!-------------------------------------------------------------------------------
! Format statements.
!-------------------------------------------------------------------------------

 6000 FORMAT (1x, a, 1x, f12.4, 2x, a)
 6100 FORMAT (1x, a, 1x, i12,   2x, a)

!-------------------------------------------------------------------------------
! Deallocate arrays.
!-------------------------------------------------------------------------------

! DEALLOCATE ( dum2d )    ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum3d_p )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum3d_t )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum3d_u )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum3d_v )  ! commented out to avoid memory fragmentation
! DEALLOCATE ( dum3d_w )  ! commented out to avoid memory fragmentation
 
  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: RDWRFEM',                        &
              /, 1x, '***   ERROR RETRIEVING TIMES FROM WRF FILE',     &
              /, 1X, '***   RCODE = ', i3,                             &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: RDWRFEM',                        &
              /, 1x, '***   LOOKING FOR INPUT MET AT TIME ', a,        &
              /, 1x, '***   NO MORE INPUT WRF FILES',                  &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: RDWRFEM',                        &
              /, 1x, '***   LOOKING FOR INPUT MET AT TIME ', a,        &
              /, 1x, '***   INPUT FILE NUMBER ', i3, ' IS BLANK',      &
              /, 1x, 70('*'))

 9300 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: RDWRFEM',                        &
              /, 1x, '***   LOOKING FOR INPUT MET AT TIME ', a,        &
              /, 1x, '***   COULD NOT FIND FILE ', a,                  &
              /, 1x, '***   FILE MAY NOT EXIST',                       &
              /, 1x, 70('*'))

 9400 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: RDWRFEM',                        &
              /, 1x, '***   ERROR RETRIEVING VARIABLE FROM WRF FILE',  &
              /, 1x, '***   VARIABLE = ', a,                           &
              /, 1x, '***   RCODE = ', i3,                             &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE rdwrfem
