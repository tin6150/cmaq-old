
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
! $Header: /project/work/rep/MCIP2/src/mcip2/mcipparm_mod.F,v 1.4 2005/08/29 14:33:35 tlotte Exp $ 


MODULE mcipparm

!-------------------------------------------------------------------------------
! Name:     MCIP Parameters
! Purpose:  Contains MCIP parameters.
! Revised:  28 Jan 1997  Original version.  (D. Byun)
!           16 May 1997  Moved land use parameters into LANDPARM.EXT.  (???)
!           23 May 1997  Include MAXI, MAXJ, MAXK definitions.  (???)
!           01 May 2000  Added NTHIK=0 option.  (???)
!           20 Sep 2001  Converted to free-form f90 and changed name from
!                        MCIPPARM.EXT to module_mcipparm.F.  Removed
!                        explicit parameter definitions of grid dimensions.
!                        Added several variables formerly contained in
!                        MCIPCOM such that all user definitions (aside from
!                        those in the input meteorology) are contained in
!                        this module.  Added two new dry deposition species
!                        for atrazine.  (T. Otte)
!           03 Oct 2001  Added variable COORDNAM.  (T. Otte)
!           14 Jan 2002  Added new dry deposition species, methanol.
!                        (Y. Wu and T. Otte)
!           09 Jun 2003  Added new dry deposition species:  N2O5, NO3, and
!                        generic aldehyde.  Removed dry deposition species,
!                        ATRA and ATRAP, from output.  (D. Schwede and T. Otte)
!           11 Aug 2004  Modified code so that arrays are made available in
!                        output only if user options in MM5 generate those
!                        data, so NQSPECIES and NPXFIELDS are new variables.
!                        Added T2OUT.  Removed MAXVAR and LUTYPE.  Moved LWATER
!                        here from LRADMDAT.  (T. Otte)
!           29 Nov 2004  Added IFLUFRC to determine if fractional land use
!                        fields will be processed.  Added NUMMETLU to include
!                        number of land use categories in incoming meteorology
!                        data.  (T. Otte)
!           26 May 2005  Removed NDX and option to interpolate to finer scale
!                        meteorology.  Removed I0LUSE, J0LUSE, and BMAX, which
!                        are no longer used.  Removed NDEP and made its usage
!                        explicit in PBLPKG and RADMDRY.  Removed IWIND and
!                        made its usage explicit in VERTHYD.  Changed NCG_I and
!                        NCG_J to NCG_X and NCG_Y, changed I0 and J0 to X0 and
!                        Y0, and changed LPRT_METI and LPRT_METJ to LPRT_METX
!                        and LPRT_METY to make code more general.  Added
!                        new variables LDDGAS, LDDCL, and LDDHG to account for
!                        optional dry deposition of chlorine and mercury
!                        species.  Allowed LTOTG to be defined later from
!                        LDDGAS, LDDCL, and LDDHG.  Increased MAXLAYS from 50
!                        to 100.  Added logical variable NEEDLAYERS to allow
!                        users to use all input meteorology layers in MCIP
!                        without specifying in the namelist a priori.  (T. Otte)
!           19 Aug 2005  Removed unused variables NO_MAX and NO_MIN.  Updated
!                        comment on definition of ISESN.  (T. Otte)
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Dimensions of CTM domain.
!-------------------------------------------------------------------------------

  INTEGER            :: ncols         ! number of grid columns (X direction)
  INTEGER            :: nrows         ! number of grid rows (Y direction)
  INTEGER            :: nlays         ! number of vertical layers
  INTEGER, PARAMETER :: nthik = 1     ! boundary thickness (cells)
  INTEGER            :: nbndy         ! number of cells in one layer of boundary
  INTEGER            :: nbdrytrim     ! number of meteorology "boundary" points
                                      ! to remove (on each of four sides)
  INTEGER            :: ncg_x         ! coarse grid X
  INTEGER            :: ncg_y         ! coarse grid Y

  INTEGER, PARAMETER :: maxlays = 100 ! max allowed in NLAYS
  LOGICAL            :: needlayers    ! "TRUE" if using all input met layers
                                      ! without defining in namelist

!-------------------------------------------------------------------------------
! Horizontal dimensions of "X" domain (CTM + BNDARY area).
!-------------------------------------------------------------------------------

  INTEGER            :: nrows_x
  INTEGER            :: ncols_x

!-------------------------------------------------------------------------------
! Dimensions for MET input data.
!-------------------------------------------------------------------------------

  INTEGER            :: metrow      ! met. grid dimension for rows (N-S)
  INTEGER            :: metcol      ! met. grid dimension for columns (E-W)
  INTEGER            :: metlay      ! met. grid dimension for layers

!-------------------------------------------------------------------------------
! Other derived dimensional parameters.
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: maxluc = 100  ! max number of land use categories
  INTEGER, PARAMETER :: lddgas =  20  ! number of base species for dry dep
  INTEGER, PARAMETER :: lddcl  =   6  ! number of chlorine species for dry dep
  INTEGER, PARAMETER :: lddhg  =   2  ! number of mercury species for dry dep
  INTEGER            :: ltotg         ! total number of species for dry dep
  INTEGER            :: lwater        ! water category in land use
  INTEGER            :: nqspecies     ! number of hydrometeor species in met
  INTEGER            :: npxfields     ! number of output fields for P-X LSM
  LOGICAL            :: t2out         ! output 2-m temperature from met
  LOGICAL            :: iflufrc       ! fractional land use available?
  INTEGER            :: nummetlu      ! number of met. land use categories

!-------------------------------------------------------------------------------
! Additional parameters to allow MCIP to process full domain of MET input to
! run CCTM.  This option allows using MET input used for other AQMs to link
! with CMAQ CTM.  To activate this function, set NTHIK=0.  For all other cases,
! NTHIKD = NTHIK = 1.
!-------------------------------------------------------------------------------

  INTEGER, PARAMETER :: nthikd = 1
  INTEGER            :: nbndyd

!-------------------------------------------------------------------------------
! Run Options.
!-------------------------------------------------------------------------------

  INTEGER :: lpbl            ! user input: 1 = use PBL from input meteorology
                             !             2 = re-calc, use PBL similarity
                             !             3 = re-calc, use sfc similarity
  INTEGER :: lcld            ! user input: 1 = recompute cloud values
  INTEGER :: lrad            ! user input: 1 = use radiative flux from input met
                             !             2 = re-calc with MCIPv1 algorithm
  INTEGER :: lddep           ! user input: 1 = RADM (Wesely) dry deposition
                             !             2 = Models-3 (Pleim) dry deposition
                             !             3 = Models-3 dry dep plus Cl
                             !             4 = Models-3 dry dep plus Cl and Hg
  INTEGER :: lhydout         ! user input: 1 = non-hydrostatic output
                             !             2 = hydrostatic output
  LOGICAL :: px              ! Pleim-Xiu land-surface model used in met model?

!-------------------------------------------------------------------------------
! Grid/Domain Related Parameters
!-------------------------------------------------------------------------------

  INTEGER :: x0              ! user input: lower-left corner of CTM on met grid
  INTEGER :: y0              ! user input: lower-left corner of CTM on met grid

!-------------------------------------------------------------------------------
! Run Time Specifications
!-------------------------------------------------------------------------------

  CHARACTER*24 :: mcip_start ! user input: YYYY-MO-DD-HH:MI:SS.SSSS
  CHARACTER*24 :: mcip_end   ! user input: YYYY-MO-DD-HH:MI:SS.SSSS

  INTEGER      :: intvl      ! user input: time interval for output [min]
  INTEGER      :: grstep     ! calculated from INTVL (HHMMSS)

  CHARACTER*16 :: coordnam   ! user input: Coordinate name
  CHARACTER*16 :: grdnam     ! user input: Grid name 

!-------------------------------------------------------------------------------
! MCIP season for RADM dry deposition.
!   Summer (ISESN=1): Jun., Jul., Aug. in N. Hem.; Dec., Jan., Feb. in S. Hem.
!   Autumn (ISESN=2): Sep., Oct., Nov. in N. Hem.; Mar., Apr., May  in S. Hem.
!   Winter:           Dec., Jan., Feb. in N. Hem.; Jun., Jul., Aug. in S. Hem.
!     ...without snow (ISESN=3)
!     ...with snow    (ISESN=4) [only set and used in radmdry]
!   Spring (ISESN=5): Mar., Apr., May  in N. Hem.; Sep., Oct., Nov. in S. Hem.
!-------------------------------------------------------------------------------

  INTEGER            :: isesn

!-------------------------------------------------------------------------------
! Coordinates for diagnostic prints.
!-------------------------------------------------------------------------------

  INTEGER            :: lprt_metx  ! X-coordinate in MET domain
  INTEGER            :: lprt_mety  ! Y-coordinate in MET domain
  INTEGER            :: lprt_col   ! COL-coordinate in output domain
  INTEGER            :: lprt_row   ! ROW-coordinate in output domain
  INTEGER            :: lprt_xcol  ! COL-coordinate in X domain
  INTEGER            :: lprt_xrow  ! ROW-coordinate in X domain

END MODULE mcipparm
