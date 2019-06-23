
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

SUBROUTINE setup (ctmlays)

!-------------------------------------------------------------------------------
! Name:     Set Up the Input Meteorology Domain Attributes
! Purpose:  Establishes bounds for MM5 or WRF post-processing.
! Revised:  10 Sep 2001  Original version.  (T. Otte)
!           07 Jan 2002  Changed file name to explicit file rather than
!                        Fortran unit to improve portability.  (S. Howard
!                        and T. Otte)
!           09 Jan 2002  Changed calls to "abort" to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  (T. Otte)
!           26 May 2005  Added WRF capability.  Changed routine name from
!                        SETUPMM5 to SETUP to make code more general.  (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm
  USE metinfo
  USE file
  USE wrf_netcdf

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER                      :: cdfid
  REAL,          INTENT(INOUT) :: ctmlays   ( maxlays )
  CHARACTER*19                 :: gridtype
  INTEGER                      :: istat
  CHARACTER*16,  PARAMETER     :: pname     = 'SETUP'
  INTEGER                      :: rcode

!-------------------------------------------------------------------------------
! Try to determine if input meteorology file is in NetCDF format or not.
! If NetCDF format, it is probably WRF.  Otherwise, assume it is MM5.
!-------------------------------------------------------------------------------

  cdfid = ncopn (file_mm(1), ncnowrit, rcode)
  IF ( rcode == 0 ) THEN  ! successfully opened NetCDF file; assume WRF
    met_model = 2
    CALL ncclos (cdfid, rcode)
  ELSE  ! error opening file as NetCDF; assume MM5
    met_model = 1
  ENDIF

!-------------------------------------------------------------------------------
! Set up Fortran unit for (first) MM5 input file.
! Call subroutine for set-up based on version of MM5 output format.
!-------------------------------------------------------------------------------

  IF ( met_model == 1 ) THEN  ! MM5

    iutmm = iutmmi

    OPEN (UNIT=iutmmi,  FILE=file_mm(1), FORM='UNFORMATTED', STATUS='OLD',  &
          IOSTAT=istat, ERR=8000)

    CALL getversion

    IF ( met_iversion == 2 ) THEN
      CALL setup_mm5v2 (ctmlays)
    ELSE IF ( met_iversion == 3 ) THEN
      CALL setup_mm5v3 (ctmlays)
    ELSE
      GOTO 8100
    ENDIF

!-------------------------------------------------------------------------------
! IF WRF, determine whether or not the Eulerian Mass (EM, or Advanced Research)
! version was used.
!-------------------------------------------------------------------------------

  ELSE IF ( met_model == 2 ) THEN  ! WRF

    CALL get_gl_att_int_cdf  ( file_mm(1), 'DYN_OPT',  met_iversion, rcode)
    IF ( rcode /= 0 ) THEN
      WRITE (6,9400) 'DYN_OPT', rcode
      GOTO 1001
    ENDIF

    CALL get_gl_att_text_cdf ( file_mm(1), 'GRIDTYPE', gridtype,     rcode)
    IF ( rcode /= 0 ) THEN
      WRITE (6,9400) 'GRIDTYPE', rcode
      GOTO 1001
    ENDIF

    IF ( ( met_iversion == 2 ) .AND. ( gridtype(1:1) == "C" ) ) THEN
      CALL setup_wrfem (ctmlays)
    ELSE
      GOTO 8200
    ENDIF

!-------------------------------------------------------------------------------
! If not MM5 or WRF, stop the code now.  This would be where other models
! could be added.
!-------------------------------------------------------------------------------

  ELSE

    GOTO 8300

  ENDIF

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 8000 WRITE (6,9000) iutmmi, TRIM(file_mm(1)), istat
      GOTO 1001

 8100 WRITE (6,9100) met_iversion
      GOTO 1001

 8200 WRITE (6,9200) met_iversion, gridtype
      GOTO 1001

 8300 WRITE (6,9300) met_model
      GOTO 1001

 9000 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: SETUP',                          &
              /, 1x, '***   ERROR OPENING MM5 FILE ON UNIT ', i3,      &
              /, 1x, '***   MM5 FILE NAME = ', a,                      &
              /, 1x, '***   IOSTAT = ', i4,                            &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: SETUP',                          &
              /, 1x, '***   UNKNOWN MM5 OUTPUT VERSION',               &
              /, 1x, '***   IVERSION = ', i3,                          &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: SETUP',                          &
              /, 1x, '***   UNKNOWN WRF OUTPUT VERSION',               &
              /, 1x, '***   IVERSION = ', i3,                          &
              /, 1x, '***   GRIDTYPE = ', a,                           &
              /, 1x, 70('*'))

 9300 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: SETUP',                          &
              /, 1x, '***   UNKNOWN INPUT METEOROLOGY MODEL',          &
              /, 1x, '***   MODEL ID = ', i3,                          &
              /, 1x, 70('*'))

 9400 FORMAT (/, 1x, 70('*'),                                          &
              /, 1x, '*** SUBROUTINE: SETUP',                          &
              /, 1x, '***   ERROR RETRIEVING VARIABLE FROM WRF FILE',  &
              /, 1x, '***   VARIABLE = ', a,                           &
              /, 1x, '***   RCODE = ', i3,                             &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE setup
