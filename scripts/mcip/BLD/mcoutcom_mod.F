
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
! $Header: /project/work/rep/MCIP2/src/mcip2/mcoutcom_mod.F,v 1.6 2005/08/29 14:33:57 tlotte Exp $ 


MODULE mcoutcom

!-------------------------------------------------------------------------------
! Name:     Meteorology Cross-Point Output Common Blocks
! Purpose:  Contains MCIP meteorology cross-point output common blocks.
! Revised:  07 Jan 1997  Original version.  (D. Byun)
!           02 May 2000  Replace NBNDY with NBDNYD.  (???)
!           14 Sep 2001  Converted to free-form f90.  Changed grid-dependent
!                        arrays to allocatable.  Added arrays for QICE, QSNOW,
!                        SOIM1, SOIM2, SOIT1, SOIT2, and SLTYP.  Removed unused
!                        arrays for LAMDA and MCONERR.  (T. Otte)
!           27 Feb 2002  Corrected definition of RSTOMI and SURF2 in MC2VDESC.
!                        Corrected units of SURF2 in MC2UNITS.   Changed QICE
!                        and QSNOW to QI and QS to be consistent with CCTM.
!                        Removed RIB from output.  Renamed SURF2 as WIND10.
!                        (T. Otte)
!           18 Mar 2003  Removed JDRATE and reduced MC3INDEX to reflect
!                        one less array.  (T. Otte)
!           09 Jun 2003  Added SNOCOV to METCRO2D.  (D. Schwede)
!                        Removed extraneous variables from output.  (T. Otte)
!           09 Aug 2004  Added JACOBS, WSPD10, WDIR10, SOIM1, SOIM2, SOIT1,
!                        SOIT2, SLTYP, JACOBF, QG, TEMP2, and LAI.  Modified
!                        code so that arrays are made available in output only
!                        if user options in MM5 generate those data.  (T. Otte
!                        and D. Schwede)
!           18 Mar 2005  Clarified the descriptions of the 3D output fields
!                        that contain density.  (T. Otte)
!           19 Aug 2005  Moved VD_C from a pointer to MC2 to an individual
!                        array in DEPVVARS_MOD.  (T. Otte)
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Time dependent cross 2D arrays for CTM domain.  (MET_CRO_2D)
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: mc2index   = 27

  REAL, ALLOCATABLE, TARGET :: mc2        ( : , : , : )

  ! Parameters related with Vertical Grid.

  REAL, POINTER :: prsfc_c    ( : , : )      ! surface pressure [Pa]
  REAL, POINTER :: jacobs_c   ( : , : )      ! surface total Jacobian [m]

  ! Parameters for PBL.

  REAL, POINTER :: ustar_c    ( : , : )      ! cell-avg friction velocity [m/s]
  REAL, POINTER :: wstar_c    ( : , : )      ! convective velocity scale [m/s]
  REAL, POINTER :: pbl_c      ( : , : )      ! PBL height [m]
  REAL, POINTER :: zzero_c    ( : , : )      ! roughness length
  REAL, POINTER :: moli_c     ( : , : )      ! inverse Monin-Obukhov length [1/m] 
  REAL, POINTER :: hfx_c      ( : , : )      ! sensible heat flux [W/m**2]
  REAL, POINTER :: qfx_c      ( : , : )      ! latent heat flux [W/m**2]

  ! Resistances

  REAL, POINTER :: radyni_c   ( : , : )      ! aerodynamic resistance [s/m]
  REAL, POINTER :: rbndyi_c   ( : , : )      ! laminar BL resistance [s/m]
  REAL, POINTER :: rstomi_c   ( : , : )      ! bulk stomatal resist [s/m]

  ! Parameters for surface meteorology.

  REAL, POINTER :: tempg_c    ( : , : )      ! ground skin temperature [K]
  REAL, POINTER :: temp10_c   ( : , : )      ! temperature at 10 m [K]
  REAL, POINTER :: tem1p5_c   ( : , : )      ! temperature at 1.5 m [K]
  REAL, POINTER :: wspd10_c   ( : , : )      ! wind speed at 10 m [m/s]
  REAL, POINTER :: wdir10_c   ( : , : )      ! wind direction at 10 m [deg]

  ! Parameters for radiation at surface.

  REAL, POINTER :: glw_c      ( : , : )      ! longwave radiation at ground
  REAL, POINTER :: gsw_c      ( : , : )      ! sol rad absorbed at ground
  REAL, POINTER :: rgrnd_c    ( : , : )      ! sol rad reaching ground=GSW_C/(1-ALB)

  ! Parameters for rain and cloud.
    !!! note -- we store pcpn rate (per met. time step) rather than the 
    !!!         accumulated value

  REAL, POINTER :: rainn_c    ( : , : )      ! nonconvective precip [cm]
  REAL, POINTER :: rainc_c    ( : , : )      ! convective precip [cm]

  ! If recomputing cloud values.

  REAL, POINTER :: cfract_c   ( : , : )      ! total fractional cloud coverage
  REAL, POINTER :: cldtop_c   ( : , : )      ! cloud top (K index in real)  ???
  REAL, POINTER :: cldbot_c   ( : , : )      ! cloud bottom (K index in real) ???
  REAL, POINTER :: wbar_c     ( : , : )      ! avg liquid water content of clouds

  ! Snow cover

  REAL, POINTER :: snocov_c   ( : , : )      ! snow cover (1=yes, 0=no)

  ! For header information.

  CHARACTER*16 :: mc2vname ( mc2index ) 
  CHARACTER*16 :: mc2units ( mc2index ) 
  CHARACTER*80 :: mc2vdesc ( mc2index ) 

  ! Header description.

  DATA mc2vname / 'PRSFC',      'JACOBS',     'USTAR',      'WSTAR',       &
                  'PBL',        'ZRUF',       'MOLI',       'HFX',         &
                  'QFX',        'RADYNI',     'RBNDYI',     'RSTOMI',      &
                  'TEMPG',      'TEMP10',     'TEMP1P5',    'WSPD10',      &
                  'WDIR10',     'GLW',        'GSW',        'RGRND',       &
                  'RN',         'RC',         'CFRAC',      'CLDT',        &
                  'CLDB',       'WBAR',       'SNOCOV' /

  DATA mc2units / 'Pascal',     'M',          'M/S',        'M/S',         &
                  'M',          'M',          '1/M',        'WATTS/M**2',  &
                  'WATTS/M**2', 'M/S',        'M/S',        'M/S',         &
                  'K',          'K',          'K',          'M/S',         &
                  'DEGREES',    'WATTS/M**2', 'WATTS/M**2', 'WATTS/M**2',  &
                  'CM',         'CM',         'FRACTION',   'M',           &
                  'M',          'G/M**3',     'NODIM' /

  DATA mc2vdesc( 1) / 'surface pressure'                    /
  DATA mc2vdesc( 2) / 'total Jacobian at surface'           /
  DATA mc2vdesc( 3) / 'cell averaged friction velocity'     /
  DATA mc2vdesc( 4) / 'convective velocity scale'           /
  DATA mc2vdesc( 5) / 'PBL height'                          /
  DATA mc2vdesc( 6) / 'surface rouhness length'             /
  DATA mc2vdesc( 7) / 'inverse of Monin-Obukhov length'     /
  DATA mc2vdesc( 8) / 'sensible heat flux'                  /
  DATA mc2vdesc( 9) / 'latent heat flux'                    /
  DATA mc2vdesc(10) / 'inverse of aerodynamic resistance'   /
  DATA mc2vdesc(11) / 'inverse laminar bnd layer resistance'/
  DATA mc2vdesc(12) / 'inverse of bulk stomatal resistance' /
  DATA mc2vdesc(13) / 'skin temperature at ground'          /
  DATA mc2vdesc(14) / 'air temperature at 10 m'             /
  DATA mc2vdesc(15) / 'air temperature at 1.5 m'            /
  DATA mc2vdesc(16) / 'wind speed at 10 m'                  /
  DATA mc2vdesc(17) / 'wind direction at 10 m'              /
  DATA mc2vdesc(18) / 'longwave radiation at ground'        /
  DATA mc2vdesc(19) / 'solar radiation absorbed at ground'  /
  DATA mc2vdesc(20) / 'solar rad reaching sfc'              /
  DATA mc2vdesc(21) / 'nonconvec. pcpn per met TSTEP'       /
  DATA mc2vdesc(22) / 'convective pcpn per met TSTEP'       /
  DATA mc2vdesc(23) / 'total cloud fraction'                /
  DATA mc2vdesc(24) / 'cloud top layer height (m)  '        /
  DATA mc2vdesc(25) / 'cloud bottom layer height (m)  '     /
  DATA mc2vdesc(26) / 'avg. liquid water content of cloud'  /
  DATA mc2vdesc(27) / 'snow cover (1=yes, 0=no)'            /


  ! 2-m temperature.

  REAL, ALLOCATABLE :: temp2_c ( : , : )

  CHARACTER*16, PARAMETER :: t2vname = 'TEMP2'
  CHARACTER*16, PARAMETER :: t2units = 'K'
  CHARACTER*80, PARAMETER :: t2vdesc = '2-m temperature passed through from met'


  ! Pleim-Xiu LSM variables.

  INTEGER, PARAMETER :: pc2index   = 6

  REAL, ALLOCATABLE, TARGET :: pc2        ( : , : , : )

  ! Soil moisture and temperature.

  REAL, POINTER :: soim1_c    ( : , : )      ! soil moisture, top cm [m3/m3]
  REAL, POINTER :: soim2_c    ( : , : )      ! soil moisture, top m  [m3/m3]
  REAL, POINTER :: soit1_c    ( : , : )      ! soil temperature, top cm [K]
  REAL, POINTER :: soit2_c    ( : , : )      ! soil temperature, top m  [K]
  REAL, POINTER :: sltyp_c    ( : , : )      ! soil texture type [category]
  REAL, POINTER :: lai_c      ( : , : )      ! leaf-area index

  ! For header information

  CHARACTER*16 :: pc2vname ( pc2index ) 
  CHARACTER*16 :: pc2units ( pc2index ) 
  CHARACTER*80 :: pc2vdesc ( pc2index ) 

  ! Header description.

  DATA pc2vname / 'SOIM1',      'SOIM2',      'SOIT1',      'SOIT2',       &
                  'SLTYP',      'LAI' /

  DATA pc2units / 'M**3/M**3',  'M**3/M**3',  'K',          'K',           &
                  'CATEGORY',   'AREA/AREA' /

  DATA pc2vdesc( 1) / 'volumetric soil moisture in top cm'  /
  DATA pc2vdesc( 2) / 'volumetric soil moisture in top m'   /
  DATA pc2vdesc( 3) / 'soil temperature in top cm'          /
  DATA pc2vdesc( 4) / 'soil temperature in top m'           /
  DATA pc2vdesc( 5) / 'soil texture type by USDA category'  /
  DATA pc2vdesc( 6) / 'leaf-area index'                     /

!-------------------------------------------------------------------------------
! Time dependent cross 3D arrays for CTM domain.  (MET_CRO_3D)
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: mc3index   = 11

  REAL, ALLOCATABLE, TARGET :: mc3        ( : , : , : , : )

  ! Essential for generalized CTM.

  REAL, POINTER :: jacobf_c   ( : , : , : )  ! tot Jacobian at layer face
  REAL, POINTER :: jacobm_c   ( : , : , : )  ! tot Jacobian at mid-layer
  REAL, POINTER :: densa_j_c  ( : , : , : )  ! J weighted tot air density
  REAL, POINTER :: what_jd_c  ( : , : , : )  ! J-dens wgtd contra-W wind

  ! Following for testing consistency in met data.

  REAL, POINTER :: tempa_c    ( : , : , : )  ! air temperature [K]
  REAL, POINTER :: wvapor_c   ( : , : , : )  ! water vapor mixing ratio
  REAL, POINTER :: press_c    ( : , : , : )  ! pressure [Pa] 
  REAL, POINTER :: densa_c    ( : , : , : )  ! density [kg/m^3]
  REAL, POINTER :: wwind_c    ( : , : , : )  ! Z-comp of true wind [m/s]
  REAL, POINTER :: x3htm_c    ( : , : , : )  ! mid-layer height
  REAL, POINTER :: x3htf_c    ( : , : , : )  ! layer face height

  ! For header information.

  CHARACTER*16 :: mc3vname ( mc3index ) 
  CHARACTER*16 :: mc3units ( mc3index ) 
  CHARACTER*80 :: mc3vdesc ( mc3index ) 

  ! Header description.

  DATA mc3vname / 'JACOBF',     'JACOBM',     'DENSA_J',    'WHAT_JD',     &
                  'TA',         'QV',         'PRES',       'DENS',        &
                  'WWIND',      'ZH',         'ZF' /

  DATA mc3units / 'M',          'M',          'KG/M**2',    'KG/(M*S)',    &
                  'K',          'KG/KG',      'Pa',         'KG/M**3',     &
                  'M/S',        'M',          'M' /

  DATA mc3vdesc( 1)  / 'total Jacobian at layer face'                         /
  DATA mc3vdesc( 2)  / 'total Jacobian at layer middle'                       /
  DATA mc3vdesc( 3)  / 'J-weighted air density: MM5-total dens, WRF-dry dens' /
  DATA mc3vdesc( 4)  / 'J & Density weighted vertical contra-W'               /
  DATA mc3vdesc( 5)  / 'air temperature'                                      /
  DATA mc3vdesc( 6)  / 'water vapor mixing ratio'                             /
  DATA mc3vdesc( 7)  / 'pressure'                                             /
  DATA mc3vdesc( 8)  / 'density of air: MM5-total density, WRF-dry density'   /
  DATA mc3vdesc( 9)  / 'true W component of wind'                             /
  DATA mc3vdesc(10)  / 'mid-layer height above ground'                        /
  DATA mc3vdesc(11)  / 'full-layer height above ground'                       /


  ! Hydrometeor species.

  INTEGER, PARAMETER :: qc3index   = 5

  REAL, ALLOCATABLE, TARGET :: qc3        ( : , : , : , : )

  ! Used for cloud and AQCHEM.

  REAL, POINTER :: cldwtr_c   ( : , : , : )  ! cloud water mixing ratio
  REAL, POINTER :: ranwtr_c   ( : , : , : )  ! rain  water mixing ratio
  REAL, POINTER :: qice_c     ( : , : , : )  ! ice  mixing ratio
  REAL, POINTER :: qsnow_c    ( : , : , : )  ! snow mixing ratio
  REAL, POINTER :: qgraup_c   ( : , : , : )  ! graupel mixing ratio

  ! For header information.

  CHARACTER*16 :: qc3vname ( qc3index ) 
  CHARACTER*16 :: qc3units ( qc3index ) 
  CHARACTER*80 :: qc3vdesc ( qc3index ) 

  ! Header description.

  DATA qc3vname / 'QC',         'QR',         'QI',         'QS',          &
                  'QG' /

  DATA qc3units / 'KG/KG',      'KG/KG',      'KG/KG',      'KG/KG',       &
                  'KG/KG' /

  DATA qc3vdesc( 1)  / 'cloud water mixing ratio'                             /
  DATA qc3vdesc( 2)  / 'rain water mixing ratio'                              /
  DATA qc3vdesc( 3)  / 'ice mixing ratio'                                     /
  DATA qc3vdesc( 4)  / 'snow mixing ratio'                                    /
  DATA qc3vdesc( 5)  / 'graupel mixing ratio'                                 /

!-------------------------------------------------------------------------------
! Time dependent boundary 3D arrays for CTM domain.  (MET_BDY_3D)
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: mb3index   = mc3index

  REAL, ALLOCATABLE, TARGET :: mb3        ( : , : , : )

  ! Essential for generalized CTM.

  REAL, POINTER :: jacobf_b   ( : , : )  ! Total Jacobian at layer face
  REAL, POINTER :: jacobm_b   ( : , : )  ! Total Jacobian at mid-layer
  REAL, POINTER :: densa_j_b  ( : , : )  ! J weighted total air density
  REAL, POINTER :: what_jd_b  ( : , : )  ! J-density wghtd contra-W wind

  ! Following for testing consistency in met data.

  REAL, POINTER :: tempa_b    ( : , : )  ! air temperature [K]
  REAL, POINTER :: wvapor_b   ( : , : )  ! water vapor mixing ratio
  REAL, POINTER :: press_b    ( : , : )  ! pressure [Pa] 
  REAL, POINTER :: densa_b    ( : , : )  ! density [kg/m^3]
  REAL, POINTER :: wwind_b    ( : , : )  ! Z component of true wind [m/s]
  REAL, POINTER :: x3htm_b    ( : , : )  ! mid-layer height
  REAL, POINTER :: x3htf_b    ( : , : )  ! layer face height


  ! Hydrometeor species.

  INTEGER, PARAMETER :: qb3index   = qc3index

  REAL, ALLOCATABLE, TARGET :: qb3        ( : , : , : )

  ! Used for cloud and AQCHEM.

  REAL, POINTER :: cldwtr_b   ( : , : )  ! cloud water mixing ratio
  REAL, POINTER :: ranwtr_b   ( : , : )  ! rain  water mixing ratio
  REAL, POINTER :: qice_b     ( : , : )  ! ice  mixing ratio
  REAL, POINTER :: qsnow_b    ( : , : )  ! snow mixing ratio
  REAL, POINTER :: qgraup_b   ( : , : )  ! graupel mixing ratio

END MODULE mcoutcom
