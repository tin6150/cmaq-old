
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
! $Header: /project/work/rep/MCIP2/src/mcip2/crs2dot.F,v 1.1.1.1 2002/03/09 14:47:59 yoj Exp $ 


SUBROUTINE crs2dot (varcrs, vardot)

!-------------------------------------------------------------------------------
! Name:     Cross to Dot 
! Purpose:  Interpolates in horizontal from cross to dot points.
! Notes:    Based on PSU/NCAR model routine.
! Revised:  20 Apr 1999  Original version.  (T. Otte)
!           29 Oct 1999  Converted to free-form f90.  (T. Otte)
!           10 Sep 2001  Removed test code.  (T. Otte)
!           09 Jan 2002  Changed "stop" statements to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  INTEGER                      :: i
  INTEGER                      :: ie
  INTEGER                      :: ix
  INTEGER                      :: j
  INTEGER                      :: je
  INTEGER                      :: jx
  CHARACTER*16,  PARAMETER     :: pname     = 'CRS2DOT'
  REAL,          INTENT(IN)    :: varcrs ( : , : )
  REAL,          INTENT(OUT)   :: vardot ( : , : )

!-------------------------------------------------------------------------------
! Extract domain dimensions.
!-------------------------------------------------------------------------------

  IF ( SIZE(varcrs,1) /= SIZE(vardot,1) ) THEN
    WRITE (6,9000) SIZE(varcrs,1), SIZE(vardot,1)
    GOTO 1001
  ENDIF

  IF ( SIZE(varcrs,2) /= SIZE(vardot,2) ) THEN
    WRITE (6,9100) SIZE(varcrs,2), SIZE(vardot,2)
    GOTO 1001
  ENDIF

  ix = SIZE(varcrs,1)
  jx = SIZE(varcrs,2)

  ie = ix - 1
  je = jx - 1

!-------------------------------------------------------------------------------
! For interior of grid, interpolate cross point values to dot points using
! four-point interpolation.
!-------------------------------------------------------------------------------

  DO j = 2, je
    DO i = 2, ie
      vardot(i,j) = 0.25 * ( varcrs(i,j)   + varcrs(i-1,j)  &
                           + varcrs(i,j-1) + varcrs(i-1,j-1) )
    ENDDO
  ENDDO

!-------------------------------------------------------------------------------
! For outermost rows and columns, interpolate cross point values to dot points
! using two-point interpolation.  In row and column 1, there are no cross points
! below or to the left of the dow row that is being interpolated.  In row JX
! and column IX, cross points are not defined.
!-------------------------------------------------------------------------------

  DO i = 2, ie
    vardot(i,1)  = 0.5 * ( varcrs(i,1)  + varcrs(i-1,1)  )
    vardot(i,jx) = 0.5 * ( varcrs(i,je) + varcrs(i-1,je) )
  ENDDO

  DO j = 2, je
    vardot(1, j) = 0.5 * ( varcrs(1, j) + varcrs(1, j-1) )
    vardot(ix,j) = 0.5 * ( varcrs(ie,j) + varcrs(ie,j-1) )
  ENDDO

!-------------------------------------------------------------------------------
! Define dot point corners by persisting cross point corners.
!-------------------------------------------------------------------------------

  vardot(1, 1)  = varcrs(1, 1)
  vardot(1, jx) = varcrs(1, je)
  vardot(ix,jx) = varcrs(ie,je)
  vardot(ix,1)  = varcrs(ie,1)

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: CRS2DOT',                            &
              /, 1x, '***   ARRAY SIZE MISMATCH IN FIRST DIMENSION',       &
              /, 1x, '***   1ST DIM OF VARCRS, VARDOT = ', 2(2x, i4),      &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: CRS2DOT',                            &
              /, 1x, '***   ARRAY SIZE MISMATCH IN SECOND DIMENSION',      &
              /, 1x, '***   2ND DIM OF VARCRS, VARDOT = ', 2(2x, i4),      &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE crs2dot
