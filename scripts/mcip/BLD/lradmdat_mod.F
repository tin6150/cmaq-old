
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
! $Header: /project/work/rep/MCIP2/src/mcip2/lradmdat_mod.F,v 1.2 2004/08/30 16:52:53 tlotte Exp $ 


MODULE lradmdat

!-------------------------------------------------------------------------------
! Name:     Land Use Parameters and Data for RADM Algorithm
! Purpose:  Contains land use parameters and data for RADM algorithm.
! Revised:  28 Jan 1997  Original version.  (D. Byun)
!           16 May 1997  Moved land use parameters into LANDPARM.EXT for
!                        Models-3 beta version only.  (???)
!           10 Sep 2001  Converted to free-form f90.  Removed LMAX_C and
!                        LMAX_R which are no longer needed.  Removed EXIOFF
!                        and EXJOFF which are not used.  Removed explicit
!                        definition of LUMET which can vary in MM5.  Removed
!                        dependence on module MCIPPARM.  (T. Otte)
!           11 Aug 2004  Moved LWATER to MCIPPARM to be generic.  (T. Otte)
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! RADM Land Use Categories:
!
!   1   Urban Land
!   2   Agriculture
!   3   Range
!   4   Deciduous Forest
!   5   Coniferous Forest
!   6   Mixed Forest and Wet Land
!   7   Water
!   8   Barren Land
!   9   Non-forest Wet Land
!  10   Mixed Agriculture/Range Land
!  11   Rock, Open Shrub
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: numsea =  5    ! season categories (#4 is not used)
  INTEGER, PARAMETER :: luradm = 11    ! number of RADM landuse categories     

!-------------------------------------------------------------------------------
! Data for variables indexed by land use and season.
!-------------------------------------------------------------------------------

  ! Local arrays for tables

  REAL :: albtabl (luradm, numsea) ! average albedo in RADM landuse category
  REAL :: mavtabl (luradm, numsea) ! moisture availability in RADM landuse
  REAL :: z00     (luradm, numsea) ! landuse dependent "Z0" table

  INTEGER :: jlu

  ! #1 midsummer, lush vegetation Z0 (m) for type 1-10

  DATA (z00(jlu,1),jlu=1,luradm)                                       &
          /   0.5,      0.25,     0.05,     0.5,      0.5,             &
              0.4,      0.0001,   0.002,    0.15,     0.1,      0.1    /
  DATA (albtabl(jlu,1), jlu=1,luradm)                                  &
          /   0.18,     0.17,     0.19,     0.16,     0.12,            &
              0.14,     0.08,     0.15,     0.25,     0.19,     0.25   /
  DATA (mavtabl(jlu,1), jlu=1,luradm)                                  &
          /   0.05,     0.30,     0.15,     0.30,     0.30,            &
              0.35,     1.00,     0.50,     0.50,     0.50,     0.02   /

  ! #2 autumn with unharvested cropland Z0

  DATA (z00(jlu,2),jlu=1,luradm)                                       &
          /   0.5,      0.1,      0.05,     0.5,      0.5,             &
              0.4,      0.0001,   0.002,    0.1,      0.08,     0.08   /
  DATA (albtabl(jlu,2), jlu=1,luradm)                                  &
          /   0.18,     0.23,     0.23,     0.17,     0.12,            &
              0.14,     0.08,     0.70,     0.25,     0.23,     0.25   /
  DATA (mavtabl(jlu,2), jlu=1,luradm)                                  &
          /   0.10,     0.60,     0.30,     0.60,     0.60,            &
              0.70,     1.00,     0.90,     0.75,     0.50,     0.05   /

  ! #3 late autumn after frost Z0

  DATA (z00(jlu,3),jlu=1,luradm)                                       &
          /   0.5,      0.005,    0.05,     0.5,      0.5,             &
              0.4,      0.0001,   0.002,    0.1,      0.02,     0.06   /
  DATA (albtabl(jlu,3), jlu=1,luradm)                                  &
          /   0.18,     0.23,     0.23,     0.17,     0.12,            &
              0.14,     0.08,     0.70,     0.25,     0.23,     0.25   /
  DATA (mavtabl(jlu,3), jlu=1,luradm)                                  &
          /   0.10,     0.60,     0.30,     0.60,     0.60,            &
              0.70,     1.00,     0.90,     0.75,     0.50,     0.05   /

  ! #4 winter-snow on ground, subfreezing Z0

  DATA (z00(jlu,4),jlu=1,luradm)                                       &
          /   0.5,      0.001,    0.001,    0.5,      0.5,             &
              0.4,      0.0001,   0.002,    0.001,    0.001,    0.04   /
  DATA (albtabl(jlu,4), jlu=1,luradm)                                  &
          /   0.18,     0.17,     0.19,     0.16,     0.12,            &
              0.14,     0.08,     0.15,     0.25,     0.19,     0.25   /
  DATA (mavtabl(jlu,4), jlu=1,luradm)                                  &
          /   0.05,     0.30,     0.15,     0.30,     0.30,            &
              0.35,     1.00,     0.50,     0.50,     0.50,     0.02   /

  ! #5 transitional, spring & partially green short annuals Z0

  DATA (z00(jlu,5),jlu=1,luradm)                                       &
          /   0.5,      0.03,     0.02,     0.5,      0.5,             &
              0.4,      0.0001,   0.002,    0.1,      0.03,     0.06   /
  DATA (albtabl(jlu,5), jlu=1,luradm)                                  &
          /   0.18,     0.17,     0.19,     0.16,     0.12,            &
              0.14,     0.08,     0.15,     0.25,     0.19,     0.25   /
  DATA (mavtabl(jlu,5), jlu=1,luradm)                                  &
          /   0.05,     0.30,     0.15,     0.30,     0.30,            &
              0.35,     1.00,     0.50,     0.50,     0.50,     0.02   /

END MODULE lradmdat
