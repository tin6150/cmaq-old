
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
! $Header: /project/work/rep/MCIP2/src/mcip2/parms3_mod.F,v 1.3 2006/03/01 20:13:16 tlotte Exp $ 


MODULE parms3

!-------------------------------------------------------------------------------
! Name:     Parameters for Models-3
! Purpose:  Contains dimensioning parameters, standard file-type, grid-type,
!           (etc.) token values for FORTRAN Models-3 I/O System API.
! Notes:    Dependent upon  M3INT, M3REAL, M3DBLE consistent with NETCDF.EXT
! Revised:  ?? Mar 1992  Prototype.  (C. Coats)
!           ?? Dec 1992  New map projection type STEGRD3.  (C. Coats)
!           ?? Jun 1994  I/O API revisions.  (C. Coats)
!           ?? Dec 1996  Support for new file types.  (C. Coats)
!           10 Sep 2001  Converted to free-form f90.  (T. Otte)
!           08 Jul 2004  Updated for consistency with I/O API.  (T. Otte)
!           17 Feb 2006  Updated for consistency with I/O API.  (T. Otte)
!-------------------------------------------------------------------------------

! Dimensioning parameters

  INTEGER,      PARAMETER :: mxatts3 =  20   !  max ATDSC3.EXT attribs per var
  INTEGER,      PARAMETER :: mxdesc3 =  60   !  max number of description lines
  INTEGER,      PARAMETER :: mxdlen3 =  80   !  description line length
  INTEGER,      PARAMETER :: mxfile3 =  64   !  max number of open files
  INTEGER,      PARAMETER :: mxlays3 = 100   !  max # of layers per file
  INTEGER,      PARAMETER :: mxvars3 = 120   !  max number of vars per file
  INTEGER,      PARAMETER :: namlen3 =  16   !  name len (logical names, etc.)


! Token-values ("magic numbers")

  ! File type values

  INTEGER,      PARAMETER :: kfevnt3 = -3    !  KF cloud event
  INTEGER,      PARAMETER :: dgraph3 = -2    !  directed graph
  INTEGER,      PARAMETER :: custom3 = -1    !  user-structured custom
  INTEGER,      PARAMETER :: dctnry3 =  0    !  dictionary
  INTEGER,      PARAMETER :: grdded3 =  1    !  gridded
  INTEGER,      PARAMETER :: bndary3 =  2    !  boundary-condition
  INTEGER,      PARAMETER :: iddata3 =  3    !  ID-referenced data
  INTEGER,      PARAMETER :: profil3 =  4    !  rawind vertical profiles
  INTEGER,      PARAMETER :: grnest3 =  5    !  nested-grid
  INTEGER,      PARAMETER :: smatrx3 =  6    !  sparse matrix
  INTEGER,      PARAMETER :: tsries3 =  7    !  time series
  INTEGER,      PARAMETER :: ptrfly3 =  8    !  pointer-flyer


  ! Variable type values

  INTEGER,      PARAMETER :: m3byte  =  1    !  byte
  INTEGER,      PARAMETER :: m3char  =  2    !  character string
  INTEGER,      PARAMETER :: m3int   =  4    !  integer
  INTEGER,      PARAMETER :: m3real  =  5    !  real
  INTEGER,      PARAMETER :: m3dble  =  6    !  double precision


  ! Values for CDFID

  INTEGER,      PARAMETER :: buffil3 = -1    !  buffered file
  INTEGER,      PARAMETER :: virfil3 = -2    !  virtual file
  INTEGER,      PARAMETER :: lstfil3 = -3    !  file list
  INTEGER,      PARAMETER :: binfil3 = -4    !  native binary file


  ! OPEN3() flags

  INTEGER,      PARAMETER :: fsread3 =  1    !  old, read-only
  INTEGER,      PARAMETER :: fsrdwr3 =  2    !  old, read-write
  INTEGER,      PARAMETER :: fsnew3  =  3    !  new, read-write
  INTEGER,      PARAMETER :: fsunkn3 =  4    !  unknown, read-write
  INTEGER,      PARAMETER :: fscrea3 =  5    !  unknown, write, truncate exist'g


  ! Grid type values

  INTEGER,      PARAMETER :: latgrd3 =  1    !  lat-lon coords
  INTEGER,      PARAMETER :: lamgrd3 =  2    !  Lambert (RADM)
  INTEGER,      PARAMETER :: mergrd3 =  3    !  Mercator coords
  INTEGER,      PARAMETER :: stegrd3 =  4    !  stereographic
  INTEGER,      PARAMETER :: utmgrd3 =  5    !  UTM (special case Mercator)
  INTEGER,      PARAMETER :: polgrd3 =  6    !  polar stereographic
  INTEGER,      PARAMETER :: eqmgrd3 =  7    !  equatorial Mercator
  INTEGER,      PARAMETER :: trmgrd3 =  8    !  transverse Mercator


  ! Vertical coordinate types
 
  INTEGER,      PARAMETER :: vgsgph3 =  1    !  hydrostatic sigma-P
  INTEGER,      PARAMETER :: vgsgpn3 =  2    !  non-h sigma-P
  INTEGER,      PARAMETER :: vgsigz3 =  3    !  sigma-Z
  INTEGER,      PARAMETER :: vgpres3 =  4    !  pressure (mb)
  INTEGER,      PARAMETER :: vgzval3 =  5    !  Z (m) (above sea lvl)
  INTEGER,      PARAMETER :: vghval3 =  6    !  H (m) (above ground)
  INTEGER,      PARAMETER :: vgwrfem =  7    !  WRF mass-core sigma
  INTEGER,      PARAMETER :: vgwrfnm =  8    !  WRF NMM


  ! Flag values for read

  INTEGER,      PARAMETER :: allays3 = -1    !  read all layers
  CHARACTER*16, PARAMETER :: allvar3 = 'ALL' !  read all variables


  ! Flag values for data quality

  REAL,         PARAMETER :: amiss3  = -9.000E36  !  BADVAL3 < AMISS3 always
  REAL,         PARAMETER :: badval3 = -9.999E36  !  real: "bad" or "missing"
  CHARACTER*16, PARAMETER :: cmiss3  = "????????????????"  !  char: "missing"
  INTEGER,      PARAMETER :: imiss3  = -9999      !  integer: "bad" or "missing"
  INTEGER,      PARAMETER :: okflag3 =  5461      !  integer: "good"


  ! Flag values for error-checking

  INTEGER,      PARAMETER :: xstat0  =  0         !  Normal, successful completion
  INTEGER,      PARAMETER :: xstat1  =  1         !  File I/O error
  INTEGER,      PARAMETER :: xstat2  =  2         !  Execution error
  INTEGER,      PARAMETER :: xstat3  =  3         !  Special error


  ! Flag values for bit order, matches BSD-style "endian.h" values

  INTEGER,      PARAMETER :: little_endian = 1234
  INTEGER,      PARAMETER :: big_endian    = 4321
  INTEGER,      PARAMETER :: pdp_endian    = 3412

END MODULE parms3
