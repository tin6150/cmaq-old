
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
! $Header: /project/work/rep/MCIP2/src/mcip2/const_mete_mod.F,v 1.1.1.1 2002/03/09 14:47:59 yoj Exp $ 


MODULE const_mete

!-------------------------------------------------------------------------------
! Name:     Constants for Meteorology
! Purpose:  Contains derived constants for meteorology.
! Revised:  07 Jan 1997  Original version.  (F. Binkowski)
!           15 Jan 1997  Added derived constants and commments.  (D. Byun)
!           10 Sep 2001  Converted to free-form f90.  (T. Otte)
!-------------------------------------------------------------------------------

  USE const

! Derived Geodetic and Geometric Constants:

  ! angular speed of earth's rotation [ 7.2921159E-5 rad/sec ]
  REAL,          PARAMETER     :: eomega = 2.0 * pi / siday

  ! conversion factor from degrees to radians [ 0.1745329252 ]
  REAL,          PARAMETER     :: deg2rad = pi / 180.0

  ! conversion factor from radians to degrees [ 57.295779513 ]
  REAL,          PARAMETER     :: rad2deg = 180.0 / pi


! Atmospheric Constants:      

  ! ratio of mol wt of water vapor to mol wt of air [ 0.62201513666 ]
  REAL,          PARAMETER     :: mvoma = mwwat /mwair

  ! gas constant over gravity [ 29.274836781 m/K ]
  REAL,          PARAMETER     :: rog = rdgas / grav

  ! gravity over gas constant [ 0.03415902905 K/m ]
  REAL,          PARAMETER     :: gor = 1.0 / rog

  ! gas constant over Cp [ 0.28571428571 ]
  REAL,          PARAMETER     :: rovcp = rdgas / cpd

  ! gas constant times Cp [ 288443.17924 ]
  REAL,          PARAMETER     :: rdcp = rdgas * cpd

  ! dry adiabatic lapse rate [ 9.7597226E-3 K/m ]
  REAL,          PARAMETER     :: dalr = grav / cpd

  ! ratio of latent heat of vaporization to specific heat [ 2489.1411967 ]
  !   - FSB The following is for cases when no correction for 
  !     temperature or moisture is made.
  REAL,          PARAMETER     :: lvocp = lv0 / cpd

END MODULE const_mete
