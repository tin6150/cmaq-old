
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
! $Header: /project/work/rep/MCIP2/src/mcip2/metinfo_mod.F,v 1.4 2005/08/29 14:35:07 tlotte Exp $ 


MODULE metinfo

!-------------------------------------------------------------------------------
! Name:     Meteorology Information
! Purpose:  Contains information about the input meteorology fields.
! Revised:  10 Sep 2001  Original version.  (T. Otte)
!           29 May 2003  Added MM5 season.  (T. Otte and J. Pleim)
!           05 Aug 2004  Added MM5 snow option (MET_SNOW_OPT), cone factor
!                        (MET_CONE_FAC), and true latitude 1 (MET_TRU1).
!                        Removed unused variables MET_SDATE and MET_STIME.
!                        Added IFT2M.  (T. Otte)
!           31 Mar 2005  Changed variable names from I,J,K representation
!                        to Y,X,Z representation to make code more general.
!                        Added new variable MET_MODEL.  Allowed MET_IVERSION
!                        to be more generic to support MM5 and WRF.  Removed
!                        unused variables MET_MDATE, MET_IEXPAND, MET_IOFFSET,
!                        and MET_JOFFSET.  Added MET_TRU2 and IFW10M.  (T. Otte)
!-------------------------------------------------------------------------------

  ! Input data format.

  INTEGER      :: met_model       ! 1=MM5, 2=WRF
  INTEGER      :: met_iversion    ! MM5: 2=v2, 3=v3; WRF: 2=EM

  ! Grid information.

  REAL         :: met_cone_fac    ! cone factor
  INTEGER      :: met_gratio      ! grid ratio w.r.t. coarse dom
  INTEGER      :: met_mapproj     ! map proj: 1-lamcon, 2-pol, 3-utm
  INTEGER      :: met_nx          ! this domain X (E-W) dimension
  INTEGER      :: met_nxcoarse    ! coarse dom X (E-W) dimension
  INTEGER      :: met_ny          ! this domain Y (N-S) dimension
  INTEGER      :: met_nycoarse    ! coarse dom Y (N-S) dimension
  INTEGER      :: met_nz          ! number of half-sigma layers
  REAL         :: met_p_alp_d     ! projection alpha
  REAL         :: met_p_bet_d     ! projection beta
  REAL         :: met_p_gam_d     ! projection gamma
  REAL         :: met_ptop        ! model top [Pa]
  REAL         :: met_resoln      ! horiz grid spacing [m]
  REAL         :: met_tapfrq      ! output interval [min]
  REAL         :: met_tru1        ! true latitude 1 [degrees]
  REAL         :: met_tru2        ! true latitude 2 [degrees]
  REAL         :: met_x_11        ! coarse dom loc of this dom's x=1
  REAL         :: met_x_centd     ! center longitude
  REAL         :: met_y_11        ! coarse dom loc of this dom's y=1
  REAL         :: met_y_centd     ! center latitude

  ! Non-hydrostatic base state variables.

  INTEGER      :: met_inhyd       ! 1=nonhyd, 0=hyd
  REAL         :: met_p00         ! non-hyd ref pressure [Pa]
  REAL         :: met_tiso        ! non-hyd ref isothermal temp [K]
  REAL         :: met_tlp         ! non-hyd ref lapse [K/500 hPa]
  REAL         :: met_ts0         ! non-hyd ref sfc temp [K]

  ! Physics options and other user input.

  INTEGER      :: met_cumulus     ! cumulus parameterization scheme
  INTEGER      :: met_expl_moist  ! explicit moist physics scheme
  INTEGER      :: met_lu_water    ! water category in land use
  INTEGER      :: met_pbl         ! PBL scheme
  INTEGER      :: met_radiation   ! radiation scheme
  INTEGER      :: met_soil_lsm    ! surface/soil scheme or LSM
  INTEGER      :: met_snow_opt    ! snow option

  LOGICAL      :: ift2m           ! if 2-m temperature is in input file
  LOGICAL      :: ifw10m          ! if 10-m wind components are in input file

  ! Date and time information.

  INTEGER      :: met_restart     ! "restarted" run? 1=yes, 0=no
  INTEGER      :: met_season      ! 1=summer, 2=winter
  CHARACTER*24 :: met_startdate   ! YYYY-MM-DD-HH:MM:SS.SSSS

END MODULE metinfo
