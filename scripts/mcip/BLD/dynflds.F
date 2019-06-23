
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
! $Header: /project/work/rep/MCIP2/src/mcip2/dynflds.F,v 1.3 2006/03/01 20:14:34 tlotte Exp $ 


SUBROUTINE dynflds (sdate, stime, gmt)

!-------------------------------------------------------------------------------
! Name:     Dynamic Fields
! Purpose:  Maps and calculates time-variant fields on MCIP X grids.
! Revised:  12 Sep 2001  Original version.  (T. Otte)
!           09 Jan 2002  Changed calls to "abort" to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  (T. Otte)
!           28 Feb 2005  Allowed new user options for LDDEP = 3 or 4 for dry
!                        deposition velocity calculations for additional
!                        chlorine and mercury species in M3DRY.  Removed call
!                        to MET3DSUP since that subroutine was combined into
!                        METVARS2CTM.  (T. Otte)
!           27 Feb 2006  Removed unnecessary dependence on module METINFO.
!                        (T. Otte)
!-------------------------------------------------------------------------------

  USE mcipparm

  IMPLICIT NONE

  REAL,          INTENT(IN)    :: gmt        ! HH.MI (.MI = MI/60)
  CHARACTER*16,  PARAMETER     :: pname      = 'DYNFLDS'
  INTEGER,       INTENT(IN)    :: sdate      ! YYYYDDD
  INTEGER,       INTENT(IN)    :: stime      ! HHMISS

!-------------------------------------------------------------------------------
! Put input meteorology variables on MCIP (X) grid and calculate required
! state variables for CTM.
!-------------------------------------------------------------------------------

  CALL metvars2ctm

!-------------------------------------------------------------------------------
! Calculate planetary boundary layer fields.
!-------------------------------------------------------------------------------

  SELECT CASE ( lpbl )

    CASE (  1  )  ! Use PBL values from input meteorology
      CALL pblsup

    CASE ( 2:3 )  ! Re-calculate PBL values
      CALL pblpkg (sdate, gmt)

    CASE DEFAULT
      WRITE (6,9000) lpbl
      GOTO 1001

  END SELECT

!-------------------------------------------------------------------------------
! Calculate cloud fields.
!-------------------------------------------------------------------------------

  SELECT CASE ( lcld )

    CASE ( 1 )  ! Recompute cloud values
      CALL bcldprc_ak

    CASE DEFAULT
      WRITE (6,9100) lcld
      GOTO 1001

  END SELECT

!-------------------------------------------------------------------------------
! Calculate radiation fields.
!-------------------------------------------------------------------------------

  SELECT CASE ( lrad )

    CASE ( 1 )  ! Use radiative fluxes from input meteorology

    CASE ( 2 )  ! Re-calculate values using MCIP Version 1 algorithm
      CALL solar (sdate, gmt)

    CASE DEFAULT
      WRITE (6,9200) lrad
      GOTO 1001

  END SELECT

!-------------------------------------------------------------------------------
! Calculate dry deposition.
!-------------------------------------------------------------------------------

  SELECT CASE ( lddep )

    CASE (  1  )  ! Use RADM (Wesely) dry deposition
      CALL radmdry

    CASE ( 2:4 )  ! Use Models-3 (Pleim) dry deposition
      CALL m3dry

    CASE DEFAULT
      WRITE (6,9300) lddep
      GOTO 1001

  END SELECT

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                             &
              /, 1x, '*** SUBROUTINE: DYNFLDS',           &
              /, 1x, '***   IMPROPER VALUE FOR LPBL',     &
              /, 1x, '***   LPBL MUST BE 1, 2, OR 3',     &
              /, 1x, '***   LPBL = ', i2,                 &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                             &
              /, 1x, '*** SUBROUTINE: DYNFLDS',           &
              /, 1x, '***   IMPROPER VALUE FOR LCLD',     &
              /, 1x, '***   LCLD MUST BE 1',              &
              /, 1x, '***   LCLD = ', i2,                 &
              /, 1x, 70('*'))

 9200 FORMAT (/, 1x, 70('*'),                             &
              /, 1x, '*** SUBROUTINE: DYNFLDS',           &
              /, 1x, '***   IMPROPER VALUE FOR LRAD',     &
              /, 1x, '***   LRAD MUST BE 1 OR 2',         &
              /, 1x, '***   LRAD = ', i2,                 &
              /, 1x, 70('*'))

 9300 FORMAT (/, 1x, 70('*'),                             &
              /, 1x, '*** SUBROUTINE: DYNFLDS',           &
              /, 1x, '***   IMPROPER VALUE FOR LDDEP',    &
              /, 1x, '***   LDDEP MUST BE 1 OR 2',        &
              /, 1x, '***   LDDEP = ', i2,                &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE dynflds
