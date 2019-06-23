
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

MODULE wrf_netcdf

!-------------------------------------------------------------------------------
! Name:     WRF NetCDF
! Purpose:  Contains routines to read NetCDF output in the Weather Research and
!           Forecasting (WRF) Model I/O API format.
! Revised:  31 Aug 2004  Original version provided by U. Houston.  (S.-B. Kim)
!           21 Jul 2005  Updated to include error-handling on NetCDF functions
!                        and to conform to NetCDF standard routines for
!                        retrieving data (to eliminate type mismatches).
!                        Updated formatting.  (T. Otte)
!           02 Aug 2005  Changed order of variable declarations in some
!                        subroutines to avoid compile failure on some
!                        machines.  (T. Otte)
!-------------------------------------------------------------------------------

CONTAINS

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE get_times_cdf (file, times, n_times, max_times, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER                      :: cdfid
  LOGICAL                      :: debug      = .FALSE.
  INTEGER                      :: dimids     ( 10 )
  CHARACTER*256, INTENT(IN)    :: file
  INTEGER                      :: i
  INTEGER                      :: id_time
  INTEGER                      :: idims      ( 10 )
  INTEGER                      :: iend       ( 10 )
  INTEGER                      :: istart     ( 10 )
  INTEGER                      :: ivtype
  INTEGER,       INTENT(IN)    :: max_times  
  INTEGER,       INTENT(OUT)   :: n_times
  INTEGER                      :: natts
  INTEGER                      :: ndims
  INTEGER,       INTENT(OUT)   :: rcode
  CHARACTER*80                 :: time1
  CHARACTER*80,  INTENT(OUT)   :: times      ( max_times )
  CHARACTER*80                 :: varnam

  cdfid  = ncopn (file, NCNOWRIT, rcode)
  IF ( rcode == 0 ) THEN
    IF ( debug ) WRITE (6,*) 'open netcdf file ', TRIM(file)
  ELSE
    WRITE (6,*) 'error opening netcdf file ', TRIM(file)
    RETURN
  ENDIF

  id_time = ncvid (cdfid, 'Times', rcode)
  IF ( rcode /= 0 ) RETURN

  rcode = nf_inq_var (cdfid, id_time, varnam, ivtype, ndims, dimids, natts)
  IF ( rcode /= 0 ) RETURN
  IF ( debug ) THEN
    WRITE (6,*) 'number of dims for Time ', ndims
  ENDIF

  DO i = 1, ndims
    rcode = nf_inq_dimlen (cdfid, dimids(i), idims(i))
    IF ( rcode /= 0 ) RETURN
    IF ( debug ) WRITE (6,*) 'dimension ', i, idims(i)
  ENDDO

  ! Get the times.

  n_times = idims(2)
  DO i = 1, idims(2)
    istart(1) = 1  ;  iend(1) = idims(1)
    istart(2) = i  ;  iend(2) = 1
    rcode = nf_get_vara_text (cdfid, id_time, istart, iend, times(i))
    IF ( rcode /= 0 ) RETURN
    time1 = times(i)
    IF ( debug ) WRITE (6,*) TRIM(file), time1(1:19)
  ENDDO

  CALL ncclos (cdfid, rcode)
  IF ( rcode /= 0 ) RETURN

END SUBROUTINE get_times_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE get_dims_cdf (file, var, dims, ndims, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER                      :: cdfid
  LOGICAL                      :: debug      = .FALSE.
  INTEGER                      :: dimids     ( 10 )
  INTEGER,       INTENT(OUT)   :: dims       ( 4 )
  CHARACTER*256, INTENT(IN)    :: file
  INTEGER                      :: i
  INTEGER                      :: id_time
  INTEGER                      :: iend       ( 10 ) 
  INTEGER                      :: istart     ( 10 )
  INTEGER                      :: ivtype
  INTEGER                      :: natts
  INTEGER,       INTENT(OUT)   :: ndims
  CHARACTER*80                 :: time1
  INTEGER,       INTENT(OUT)   :: rcode
  CHARACTER(*),  INTENT(IN)    :: var
  CHARACTER*80                 :: varnam

  cdfid  = ncopn (file, NCNOWRIT, rcode)
  IF ( rcode == 0 ) THEN
    IF ( debug ) WRITE (6,*) 'open netcdf file ', TRIM(file)
  ELSE
    WRITE (6,*) 'error opening netcdf file ', TRIM(file)
    RETURN
  ENDIF

  id_time = ncvid (cdfid, var, rcode)
  IF ( rcode /= 0 ) RETURN

  rcode = nf_inq_var (cdfid, id_time, varnam, ivtype, ndims, dimids, natts)
  IF ( rcode /= 0 ) RETURN
  IF ( debug ) THEN
    WRITE (6,*) 'number of dims for ', var, ' ', ndims
  ENDIF

  DO i = 1, ndims
    rcode = nf_inq_dimlen (cdfid, dimids(i), dims(i))
    IF ( rcode /= 0 ) RETURN
    IF ( debug ) WRITE (6,*) 'dimension ', i, dims(i)
  ENDDO

  CALL ncclos (cdfid, rcode)
  IF ( rcode /= 0 ) RETURN

END SUBROUTINE get_dims_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE get_gl_att_int_cdf (file, att_name, value, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  CHARACTER(*),  INTENT(IN)    :: att_name
  INTEGER                      :: cdfid
  LOGICAL                      :: debug      = .FALSE.
  CHARACTER*256, INTENT(IN)    :: file
  INTEGER                      :: id_time
  INTEGER,       INTENT(OUT)   :: rcode
  INTEGER,       INTENT(OUT)   :: value

  cdfid = ncopn (file, NCNOWRIT, rcode)
  IF ( rcode == 0 ) THEN
    IF ( debug ) WRITE (6,*) 'open netcdf file ', TRIM(file)
  ELSE
    WRITE (6,*) 'error opening netcdf file ', TRIM(file)
    RETURN
  ENDIF

  rcode = nf_get_att_int (cdfid, nf_global, att_name, value)
  IF ( rcode /= 0 ) RETURN

  CALL ncclos (cdfid, rcode)
  IF ( rcode /= 0 ) RETURN

  IF ( debug ) WRITE (6,*) 'global attribute ', att_name, ' is ', value

END SUBROUTINE get_gl_att_int_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE get_gl_att_real_cdf (file, att_name, value, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  CHARACTER(*),  INTENT(IN)    :: att_name
  INTEGER                      :: cdfid
  LOGICAL                      :: debug      = .FALSE.
  CHARACTER*256, INTENT(IN)    :: file
  INTEGER                      :: id_time
  INTEGER,       INTENT(OUT)   :: rcode
  REAL,          INTENT(OUT)   :: value

  cdfid = ncopn (file, NCNOWRIT, rcode)
  IF ( rcode == 0 ) THEN
    IF ( debug ) WRITE (6,*) 'open netcdf file ', TRIM(file)
  ELSE
    WRITE (6,*) 'error opening netcdf file ', TRIM(file)
    RETURN
  ENDIF

  rcode = nf_get_att_real (cdfid, nf_global, att_name, value)
  IF ( rcode /= 0 ) RETURN

  CALL ncclos (cdfid, rcode)
  IF ( rcode /= 0 ) RETURN

  IF ( debug ) WRITE (6,*) 'global attribute ', att_name, ' is ', value

END SUBROUTINE get_gl_att_real_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE get_gl_att_text_cdf (file, att_name, value, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  CHARACTER(*),  INTENT(IN)    :: att_name
  INTEGER                      :: cdfid
  LOGICAL                      :: debug      = .FALSE.
  CHARACTER*256, INTENT(IN)    :: file
  INTEGER                      :: id_time
  INTEGER,       INTENT(OUT)   :: rcode
  CHARACTER*19,  INTENT(OUT)   :: value

  cdfid = ncopn (file, NCNOWRIT, rcode)
  IF ( rcode == 0 ) THEN
    IF ( debug ) WRITE (6,*) 'open netcdf file ', TRIM(file)
  ELSE
    WRITE (6,*) 'error opening netcdf file ', TRIM(file)
    RETURN
  ENDIF

  rcode = nf_get_att_text (cdfid, nf_global, att_name, value)
  IF ( rcode /= 0 ) RETURN

  CALL ncclos (cdfid, rcode)
  IF ( rcode /= 0 ) RETURN

  IF ( debug ) WRITE (6,*) 'global attribute ', att_name, ' is ', value

END SUBROUTINE get_gl_att_text_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE get_var_3d_real_cdf (file, var, data, i1, i2, i3, time, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER,       INTENT(IN)    :: i1
  INTEGER,       INTENT(IN)    :: i2
  INTEGER,       INTENT(IN)    :: i3

  INTEGER                      :: cdfid
  REAL,          INTENT(OUT)   :: data       ( i1, i2, i3 )
  LOGICAL                      :: debug      = .FALSE.
  INTEGER                      :: dimids     ( 10 )
  CHARACTER*256, INTENT(IN)    :: file
  INTEGER                      :: i
  INTEGER                      :: id_data
  INTEGER                      :: idims      ( 10 )
  INTEGER                      :: iend       ( 10 )
  INTEGER                      :: istart     ( 10 )
  INTEGER                      :: ivtype
  INTEGER                      :: natts
  INTEGER                      :: ndims
  INTEGER,       INTENT(OUT)   :: rcode
  INTEGER,       INTENT(IN)    :: time
  CHARACTER*80                 :: time1
  CHARACTER(*),  INTENT(IN)    :: var
  CHARACTER*80                 :: varnam

  cdfid = ncopn (file, NCNOWRIT, rcode)
  IF ( rcode == 0 ) THEN
    IF ( debug ) WRITE (6,*) 'open netcdf file ', TRIM(file)
  ELSE
    WRITE (6,*) 'error opening netcdf file ', TRIM(file)
    RETURN
  ENDIF

  id_data = ncvid (cdfid, var, rcode)
  IF ( rcode /= 0 ) RETURN
  rcode = nf_inq_var (cdfid, id_data, varnam, ivtype, ndims, dimids, natts)
  IF ( rcode /= 0 ) RETURN

  IF ( debug ) THEN
    WRITE (6,*) 'number of dims for ', var, ' ', ndims
  ENDIF

  DO i = 1, ndims
    rcode = nf_inq_dimlen (cdfid, dimids(i), idims(i))
    IF ( rcode /= 0 ) RETURN
    IF ( debug ) WRITE (6,*) 'dimension ', i, idims(i)
  ENDDO

  ! Check the dimensions.

  IF ( ( i1   /= idims(1) ) .OR.  &
       ( i2   /= idims(2) ) .OR.  &
       ( i3   /= idims(3) ) .OR.  &
       ( time >  idims(4) ) )  THEN

    WRITE (6,*) 'error in 3d_var_real read, dimension problem'
    WRITE (6,*) i1, idims(1)
    WRITE (6,*) i2, idims(2)
    WRITE (6,*) i3, idims(3)
    WRITE (6,*) time, idims(4)
    WRITE (6,*) ' error stop '
    STOP

  ENDIF

  ! Get the data.

  istart(1) = 1     ;  iend(1) = i1
  istart(2) = 1     ;  iend(2) = i2
  istart(3) = 1     ;  iend(3) = i3
  istart(4) = time  ;  iend(4) = 1

  rcode = nf_get_vara_real (cdfid, id_data, istart, iend, data)
  IF ( rcode /= 0 ) RETURN

  CALL ncclos (cdfid, rcode)
  IF ( rcode /= 0 ) RETURN

END SUBROUTINE get_var_3d_real_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE get_var_2d_real_cdf (file, var, data, i1, i2, time, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER,       INTENT(IN)    :: i1
  INTEGER,       INTENT(IN)    :: i2

  INTEGER                      :: cdfid
  REAL,          INTENT(OUT)   :: data       ( i1, i2 )
  LOGICAL                      :: debug      = .FALSE.
  INTEGER                      :: dimids     ( 10 )
  CHARACTER*256, INTENT(IN)    :: file
  INTEGER                      :: i
  INTEGER                      :: id_data
  INTEGER                      :: idims      ( 10 )
  INTEGER                      :: iend       ( 10 )
  INTEGER                      :: istart     ( 10 )
  INTEGER                      :: ivtype
  INTEGER                      :: natts
  INTEGER                      :: ndims
  INTEGER,       INTENT(OUT)   :: rcode
  INTEGER,       INTENT(IN)    :: time
  CHARACTER*80                 :: time1
  CHARACTER(*),  INTENT(IN)    :: var
  CHARACTER*80                 :: varnam

  cdfid = ncopn (file, NCNOWRIT, rcode)
  IF ( rcode == 0 ) THEN
    IF ( debug ) WRITE (6,*) 'open netcdf file ', TRIM(file)
  ELSE
    WRITE (6,*) 'error opening netcdf file ', TRIM(file)
    RETURN
  ENDIF

  id_data = ncvid (cdfid, var, rcode)
  IF ( rcode /= 0 ) RETURN

  rcode = nf_inq_var (cdfid, id_data, varnam, ivtype, ndims, dimids, natts)
  IF ( rcode /= 0 ) RETURN

  IF ( debug ) THEN
    WRITE (6,*) 'number of dims for ', var, ' ', ndims
  ENDIF

  DO i = 1, ndims
    rcode = nf_inq_dimlen (cdfid, dimids(i), idims(i))
    IF ( rcode /= 0 ) RETURN
    IF ( debug ) WRITE (6,*) 'dimension ', i, idims(i)
  ENDDO

  ! Check the dimensions.

  IF ( ( i1  /= idims(1) ) .OR.  &
       ( i2  /= idims(2) ) .OR.  &
       ( time > idims(3) ) ) THEN

    WRITE (6,*) 'error in 2d_var_real read, dimension problem'
    WRITE (6,*) i1, idims(1)
    WRITE (6,*) i2, idims(2)
    WRITE (6,*) time, idims(4)
    WRITE (6,*) 'error stop'
    STOP

  ENDIF

  ! Get the data.

  istart(1) = 1     ;  iend(1) = i1
  istart(2) = 1     ;  iend(2) = i2
  istart(3) = time  ;  iend(3) = 1

  rcode = nf_get_vara_real (cdfid, id_data, istart, iend, data)
  IF ( rcode /= 0 ) RETURN

  CALL ncclos (cdfid, rcode)
  IF ( rcode /= 0 ) RETURN

END SUBROUTINE get_var_2d_real_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE get_var_2d_int_cdf (file, var, data, i1, i2, time, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER,       INTENT(IN)    :: i1
  INTEGER,       INTENT(IN)    :: i2

  INTEGER                      :: cdfid
  INTEGER,       INTENT(OUT)   :: data       ( i1, i2 )
  LOGICAL                      :: debug      = .FALSE.
  INTEGER                      :: dimids     ( 10 )
  CHARACTER*256, INTENT(IN)    :: file
  INTEGER                      :: i
  INTEGER                      :: id_data
  INTEGER                      :: idims      ( 10 )
  INTEGER                      :: iend       ( 10 )
  INTEGER                      :: istart     ( 10 )
  INTEGER                      :: ivtype
  INTEGER                      :: natts
  INTEGER                      :: ndims
  INTEGER,       INTENT(OUT)   :: rcode
  INTEGER,       INTENT(IN)    :: time
  CHARACTER*80                 :: time1
  CHARACTER(*),  INTENT(IN)    :: var
  CHARACTER*80                 :: varnam

  cdfid = ncopn (file, NCNOWRIT, rcode)
  IF ( rcode == 0 ) THEN
    IF ( debug ) WRITE (6,*) 'open netcdf file ', TRIM(file)
  ELSE
    WRITE (6,*) 'error opening netcdf file ', TRIM(file)
    RETURN
  ENDIF

  id_data = ncvid (cdfid, var, rcode)
  IF ( rcode /= 0 ) RETURN

  rcode = nf_inq_var (cdfid, id_data, varnam, ivtype, ndims, dimids, natts)
  IF ( rcode /= 0 ) RETURN
  IF ( debug ) THEN
    WRITE (6,*) 'number of dims for ', var, ' ', ndims
  ENDIF

  DO i = 1, ndims
    rcode = nf_inq_dimlen (cdfid, dimids(i), idims(i))
    IF ( rcode /= 0 ) RETURN
    IF ( debug ) WRITE (6,*) 'dimension ', i, idims(i)
  ENDDO

  ! Check the dimensions.

  IF ( ( i1   /= idims(1) ) .OR.  &
       ( i2   /= idims(2) ) .OR.  &
       ( time >  idims(3) ) )  THEN

    WRITE (6,*) 'error in 2d_var_real read, dimension problem'
    WRITE (6,*) i1, idims(1)
    WRITE (6,*) i2, idims(2)
    WRITE (6,*) time, idims(4)
    WRITE (6,*) 'error stop'
    STOP

  ENDIF

  ! Get the data.

  istart(1) = 1     ;  iend(1) = i1
  istart(2) = 1     ;  iend(2) = i2
  istart(3) = time  ;  iend(3) = 1

  rcode = nf_get_vara_int (cdfid, id_data, istart, iend, data)
  IF ( rcode /= 0 ) RETURN

  CALL ncclos (cdfid, rcode)
  IF ( rcode /= 0 ) RETURN

END SUBROUTINE get_var_2d_int_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE get_var_1d_real_cdf (file, var, data, i1, time, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER,       INTENT(IN)    :: i1

  INTEGER                      :: cdfid
  REAL,          INTENT(OUT)   :: data       ( i1 )
  LOGICAL                      :: debug      = .FALSE.
  INTEGER                      :: dimids     ( 10 )
  CHARACTER*256, INTENT(IN)    :: file
  INTEGER                      :: i
  INTEGER                      :: id_data
  INTEGER                      :: idims      ( 10 )
  INTEGER                      :: iend       ( 10 )
  INTEGER                      :: istart     ( 10 )
  INTEGER                      :: ivtype
  INTEGER                      :: natts
  INTEGER                      :: ndims
  INTEGER,       INTENT(OUT)   :: rcode
  INTEGER,       INTENT(IN)    :: time
  CHARACTER*80                 :: time1
  CHARACTER(*),  INTENT(IN)    :: var
  CHARACTER*80                 :: varnam

  cdfid = ncopn (file, NCNOWRIT, rcode)
  IF ( rcode == 0 ) THEN
    IF ( debug ) WRITE (6,*) 'open netcdf file ', TRIM(file)
  ELSE
    WRITE (6,*) 'error opening netcdf file ', TRIM(file)
    RETURN
  ENDIF

  id_data = ncvid (cdfid, var, rcode)
  IF ( rcode /= 0 ) RETURN

  rcode = nf_inq_var (cdfid, id_data, varnam, ivtype, ndims, dimids, natts)
  IF ( rcode /= 0 ) RETURN
  IF ( debug ) THEN
    WRITE (6,*) 'number of dims for ', var, ' ', ndims
  ENDIF

  DO i = 1, ndims
    rcode = nf_inq_dimlen (cdfid, dimids(i), idims(i))
    IF ( rcode /= 0 ) RETURN
    IF ( debug ) WRITE (6,*) 'dimension ', i, idims(i)
  ENDDO

  ! Check the dimensions.

  IF ( ( i1  /= idims(1) ) .OR.  &
       ( time > idims(2) ) )  THEN

    WRITE (6,*) 'error in 1d_var_real read, dimension problem'
    WRITE (6,*) i1, idims(1)
    WRITE (6,*) time, idims(2)
    WRITE (6,*) 'error stop'
    STOP

  ENDIF

  ! Get the data.

  istart(1) = 1     ;  iend(1) = i1
  istart(2) = time  ;  iend(2) = 1

  rcode = nf_get_vara_real (cdfid, id_data, istart, iend, data)
  IF ( rcode /= 0 ) RETURN

  CALL ncclos (cdfid, rcode)
  IF ( rcode /= 0 ) RETURN

END SUBROUTINE get_var_1d_real_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE get_var_real_cdf (file, var, data, i1, time, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER,       INTENT(IN)    :: i1

  INTEGER                      :: cdfid
  REAL,          INTENT(OUT)   :: data
  REAL                         :: data2      ( i1 )
  LOGICAL                      :: debug      = .FALSE.
  INTEGER                      :: dimids     ( 10 )
  CHARACTER*256, INTENT(IN)    :: file
  INTEGER                      :: i
  INTEGER                      :: id_data
  INTEGER                      :: idims      ( 10 )
  INTEGER                      :: iend       ( 10 )
  INTEGER                      :: istart     ( 10 )
  INTEGER                      :: ivtype
  INTEGER                      :: natts
  INTEGER                      :: ndims
  INTEGER,       INTENT(OUT)   :: rcode
  INTEGER,       INTENT(IN)    :: time
  CHARACTER*80                 :: time1
  CHARACTER(*),  INTENT(IN)    :: var
  CHARACTER*80                 :: varnam

  cdfid = ncopn (file, NCNOWRIT, rcode)
  IF ( rcode == 0 ) THEN
    IF ( debug ) WRITE (6,*) 'open netcdf file ', TRIM(file)
  ELSE
    WRITE (6,*) 'error opening netcdf file ', TRIM(file)
    RETURN
  ENDIF

  id_data = ncvid (cdfid, var, rcode)
  IF ( rcode /= 0 ) RETURN

  rcode = nf_inq_var (cdfid, id_data, varnam, ivtype, ndims, dimids, natts)
  IF ( rcode /= 0 ) RETURN
  IF ( debug ) THEN
    WRITE (6,*) 'number of dims for ', var, ' ', ndims
  ENDIF

  DO i = 1, ndims
    rcode = nf_inq_dimlen (cdfid, dimids(i), idims(i))
    IF ( rcode /= 0 ) RETURN
    IF ( debug ) WRITE (6,*) 'dimension ', i, idims(i)
  ENDDO

  ! Check the dimensions.

  IF ( ( i1   /= idims(1) ) .OR.  & 
       ( time >  idims(2) ) )  THEN

    WRITE (6,*) 'error in 1d_var_real read, dimension problem '
    WRITE (6,*) i1, idims(1)
    WRITE (6,*) time, idims(2)
    WRITE (6,*) 'error stop'
    STOP

  ENDIF

  ! Get the data.

  istart(1) = 1     ;  iend(1) = i1
  istart(2) = time  ;  iend(2) = 1

  rcode = nf_get_vara_real (cdfid, id_data, istart, iend, data2)
  IF ( rcode /= 0 ) RETURN

  data = data2(i1)

  CALL ncclos (cdfid, rcode)
  IF ( rcode /= 0 ) RETURN

END SUBROUTINE get_var_real_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

SUBROUTINE get_dim_att_int_cdf (file, att_name, value, rcode)

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  CHARACTER(*),  INTENT(IN)    :: att_name
  INTEGER                      :: cdfid
  LOGICAL                      :: debug      = .FALSE.
  CHARACTER*256, INTENT(IN)    :: file
  INTEGER                      :: id
  INTEGER,       INTENT(OUT)   :: rcode
  INTEGER,       INTENT(OUT)   :: value

  cdfid = ncopn (file, NCNOWRIT, rcode)
  IF ( rcode == 0 ) THEN
    IF ( debug ) WRITE (6,*) 'open netcdf file ', TRIM(file)
  ELSE
    WRITE (6,*) 'error opening netcdf file ', TRIM(file)
    RETURN
  ENDIF

  rcode = nf_inq_dimid (cdfid, att_name, id)
  IF ( rcode /= 0 ) RETURN
  rcode = nf_inq_dim (cdfid, id, att_name, value)
  IF ( rcode /= 0 ) RETURN

  CALL ncclos (cdfid, rcode)
  IF ( rcode /= 0 ) RETURN

  IF ( debug ) WRITE (6,*) 'Dimensions ', att_name, ' is ', value

END SUBROUTINE get_dim_att_int_cdf

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

END MODULE wrf_netcdf
