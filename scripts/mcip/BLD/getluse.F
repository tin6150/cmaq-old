
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
! $Header: /project/work/rep/MCIP2/src/mcip2/getluse.F,v 1.3 2005/08/29 14:31:10 tlotte Exp $ 


SUBROUTINE getluse

!-------------------------------------------------------------------------------
! Name:     Get Land Use
! Purpose:  Reads in land use fractions and reclassifies categories
!           following RADM as needed for use with PBLPKG and RADMDRY.
! Revised:  15 Jan 1997  Created for MCIP and generalized CTM.  (D. Byun)
!           20 May 1997  Adapted for Models-3 BETA system.  (D. Byun)
!           04 Feb 1998  Changed include for nonglobal includes.  (D. Byun)
!           14 Apr 2000  Added more LU data options.  (D. Byun)
!           10 Sep 2001  Converted to free-form f90.  Removed retrieval of BMAX
!                        from environment and added it to MCIPPARM.  Converted
!                        arrays to allocatable based on run-time definitions.
!                        Removed re-calculations of NCG_I and NCG_J.  Changed
!                        MAXI and MAXJ to METROW and METCOL.  Removed argument
!                        list.  Changed 11-category water from "7" to LWATER.
!                        Added calculations for LUTYPE=3.  Moved calculations
!                        for each definition of LUTYPE to separate subroutines.
!                        (T. Otte)
!           09 Jan 2002  Changed calls to "abort" to calls to "m3exit" for
!                        graceful shut-down of I/O API files.  (T. Otte)
!           26 Mar 2003  Simplified algorithm to map input meteorology to
!                        MCIP_X domain.  (T. Otte)
!           11 Aug 2004  Removed obsolete land-use input sources so that all
!                        land-use input is assumed to come directly from MM5
!                        to be converted to RADM categories.  Added algorithms
!                        from lutrans3.F here.  Removed XFLAGS.  Removed
!                        conversion of land use to RADM categories if RADM dry
!                        deposition and/or PBL recalculation is not used.
!                        (T. Otte)
!           01 Dec 2004  Added processing for fractional land use categories
!                        if those fields are available.  (T. Otte)
!           08 Apr 2005  Removed NDX and option to interpolate to finer scale
!                        meteorology.  Changed I and J dimensions to Y and X
!                        to make the code more general.  Added optimization
!                        of loops using F90 implicit loop structures.  (T. Otte)
!           20 Jul 2005  Recoded nested WHERE-ELSEWHERE-END WHERE for known bug
!                        in PGF90v5.2 compiler.  (T. Otte)
!-------------------------------------------------------------------------------

  USE metvars
  USE metinfo, nx => met_nx, ny => met_ny
  USE lradmdat
  USE xvars
  USE mcipparm

  IMPLICIT NONE

  INTEGER                      :: col
  INTEGER                      :: ec
  INTEGER                      :: er
  INTEGER                      :: i
  INTEGER                      :: idom
  INTEGER                      :: iendlu
  INTEGER                      :: ii
  INTEGER                      :: iil
  INTEGER                      :: j
  INTEGER                      :: jj
  INTEGER                      :: jjl
  INTEGER                      :: lu
  REAL,          ALLOCATABLE   :: luradmcat ( : , : , : )
  INTEGER                      :: lumax
  CHARACTER*16,  PARAMETER     :: pname     = 'GETLUSE'
  INTEGER                      :: row
  INTEGER                      :: sc
  INTEGER                      :: sr

!-------------------------------------------------------------------------------
! If recalculating PBL variables and/or using RADM dry deposition, map
! land use categories from MM5 to RADM categories.
!-------------------------------------------------------------------------------

  IF ( ( lpbl == 2 ) .OR. ( lpbl == 3 ) .OR.  &
       ( lddep == 1 ) ) THEN  !  RADM land use categories needed

    lwater = 7
    lumax  = luradm

    ALLOCATE ( luradmcat (nx, ny, luradm) )

    IF ( SIZE(luradmcat,3) > SIZE(xluse,3) ) THEN
      WRITE (6,9000) SIZE(xluse,3), SIZE(luradmcat,3)
      GOTO 1001
    ELSE
      iendlu = SIZE(luradmcat,3)
    ENDIF

!-------------------------------------------------------------------------------
!   Based on the water ID from the MM5 header, define the MM5 land-use type,
!   and use the appropriate conversion to 11 RADM land-use categories.
!
!   RADM Categories:
!     1     Urban Land
!     2     Agriculture
!     3     Range
!     4     Deciduous Forest
!     5     Coniferous Forest
!     6     Mixed Forest and Wetland
!     7     Water
!     8     Barren Land
!     9     Non-Forest Wetland
!    10     Mixed Agriculture and Range
!    11     Rocky and Open Shrub
!-------------------------------------------------------------------------------

    luradmcat = 0.0  ! init fraction of each RADM category to 0.0 at each pt

    IF ( met_lu_water == 16 ) THEN  ! 24-category USGS

      WRITE (*,'(/,a)') 'Landuse 24-category to 11-category translation'

!     -------------------------------------------------------------
!        MM5        Categories                             RADM
!     -------------------------------------------------------------
!         1      Urban Land                                   1
!         2      Dryland Cropland and Pasture                 2
!         3      Irrigated Cropland and Pasture               2 
!         4      Mixed Dryland and Irrigated Cropland         2
!         5      Cropland/Grassland Mosaic                    2
!         6      Cropland/Woodland Mosaic                    10
!         7      Grassland                                    3
!         8      Shrubland                                    3
!         9      Mixed Shrubland/Grassland                    3
!        10      Savannah                                    10
!        11      Deciduous Broadleaf Forest                   4
!        12      Deciduous Needleleaf Forest                  4
!        13      Evergreen Broadleaf Forest                   5
!        14      Evergreen Needleleaf Forest                  5
!        15      Mixed Forest                                 6
!        16      Water                                        7
!        17      Herbaceous Wetland                           9
!        18      Wooded Wetland                               5
!        19      Barren or Sparsely Vegetated                 8
!        20      Herbaceous Tundra                           11
!        21      Wooded Tundra                                5
!        22      Mixed Tundra                                10
!        23      Bare Ground Tundra                           8
!        24      Snow or Ice                                 11
!     -------------------------------------------------------------

      IF ( iflufrc ) THEN

        luradmcat(:,:, 1) = lufrac(:,:, 1)
        luradmcat(:,:, 2) = lufrac(:,:, 2) + lufrac(:,:, 3) + &
                            lufrac(:,:, 4) + lufrac(:,:, 5)
        luradmcat(:,:, 3) = lufrac(:,:, 7) + lufrac(:,:, 8) + &
                            lufrac(:,:, 9)
        luradmcat(:,:, 4) = lufrac(:,:,11) + lufrac(:,:,12)
        luradmcat(:,:, 5) = lufrac(:,:,13) + lufrac(:,:,14) + &
                            lufrac(:,:,18) + lufrac(:,:,21)
        luradmcat(:,:, 6) = lufrac(:,:,15)
        luradmcat(:,:, 7) = lufrac(:,:,16)
        luradmcat(:,:, 8) = lufrac(:,:,19) + lufrac(:,:,23)
        luradmcat(:,:, 9) = lufrac(:,:,17)
        luradmcat(:,:,10) = lufrac(:,:, 6) + lufrac(:,:,10) + &
                            lufrac(:,:,22)
        luradmcat(:,:,11) = lufrac(:,:,20) + lufrac(:,:,24)

      ELSE

        DO j = 1, ny
          DO i = 1, nx

            lu = landuse(i,j)

            IF ( lu ==  1 ) luradmcat(i,j, 1) = 1.0
            IF ( lu ==  2 ) luradmcat(i,j, 2) = 1.0
            IF ( lu ==  3 ) luradmcat(i,j, 2) = 1.0
            IF ( lu ==  4 ) luradmcat(i,j, 2) = 1.0
            IF ( lu ==  5 ) luradmcat(i,j, 2) = 1.0
            IF ( lu ==  6 ) luradmcat(i,j,10) = 1.0
            IF ( lu ==  7 ) luradmcat(i,j, 3) = 1.0
            IF ( lu ==  8 ) luradmcat(i,j, 3) = 1.0
            IF ( lu ==  9 ) luradmcat(i,j, 3) = 1.0
            IF ( lu == 10 ) luradmcat(i,j,10) = 1.0
            IF ( lu == 11 ) luradmcat(i,j, 4) = 1.0
            IF ( lu == 12 ) luradmcat(i,j, 4) = 1.0
            IF ( lu == 13 ) luradmcat(i,j, 5) = 1.0
            IF ( lu == 14 ) luradmcat(i,j, 5) = 1.0
            IF ( lu == 15 ) luradmcat(i,j, 6) = 1.0
            IF ( lu == 16 ) luradmcat(i,j, 7) = 1.0
            IF ( lu == 17 ) luradmcat(i,j, 9) = 1.0
            IF ( lu == 18 ) luradmcat(i,j, 5) = 1.0
            IF ( lu == 19 ) luradmcat(i,j, 8) = 1.0
            IF ( lu == 20 ) luradmcat(i,j,11) = 1.0
            IF ( lu == 21 ) luradmcat(i,j, 5) = 1.0
            IF ( lu == 22 ) luradmcat(i,j,10) = 1.0
            IF ( lu == 23 ) luradmcat(i,j, 8) = 1.0
            IF ( lu == 24 ) luradmcat(i,j,11) = 1.0

          ENDDO
        ENDDO

      ENDIF

    ELSE IF ( ( met_lu_water ==    7 ) .OR.  &
              ( met_lu_water == -999 ) ) THEN  ! 13-category original MM5

      WRITE (*,'(/,a)') 'Landuse 13-category to 11-category translation'

!     -------------------------------------------------------------
!        MM5        CATEGORIES                             RADM
!     -------------------------------------------------------------
!         1      Urban Land                                   1
!         2      Agriculture Land                             2
!         3      Range-Grassland                              3
!         4      Deciduous Forest                             4
!         5      Coniferous Forest                            5
!         6      Mixed Forest and Wetland                     6
!         7      Water                                        7
!         8      Marsh or Wetland                             9
!         9      Desert                                      11
!        10      Tundra                                       8
!        11      Permanent Ice                                7
!        12      Tropical Forest                              3
!        13      Savannah                                     3
!         ?      Mixed Agriculture/Rangeland                 10
!     -------------------------------------------------------------

      IF ( iflufrc ) THEN

        luradmcat(:,:, 1) = lufrac(:,:, 1)
        luradmcat(:,:, 2) = lufrac(:,:, 2)
        luradmcat(:,:, 3) = lufrac(:,:, 3) + lufrac(:,:,12) + &
                            lufrac(:,:,13)
        luradmcat(:,:, 4) = lufrac(:,:, 4)
        luradmcat(:,:, 5) = lufrac(:,:, 5)
        luradmcat(:,:, 6) = lufrac(:,:, 6)
        luradmcat(:,:, 7) = lufrac(:,:, 7) + lufrac(:,:,11)
        luradmcat(:,:, 8) = lufrac(:,:,10)
        luradmcat(:,:, 9) = lufrac(:,:, 8)
        luradmcat(:,:,11) = lufrac(:,:, 9)

      ELSE

        DO j = 1, ny
          DO i = 1, nx

            lu = landuse(i,j)

            IF ( lu <=  7 ) luradmcat(i,j,lu) = 1.0
            IF ( lu ==  8 ) luradmcat(i,j, 9) = 1.0
            IF ( lu ==  9 ) luradmcat(i,j,11) = 1.0
            IF ( lu == 10 ) luradmcat(i,j, 8) = 1.0
            IF ( lu == 11 ) luradmcat(i,j, 7) = 1.0
            IF ( lu == 12 ) luradmcat(i,j, 3) = 1.0
            IF ( lu == 13 ) luradmcat(i,j, 3) = 1.0

          ENDDO
        ENDDO

      ENDIF

    ELSE IF ( met_lu_water == 15 ) THEN  ! 16-category SiB

      WRITE (*,'(/,a)') 'Landuse 16-category to 11-category translation'

!     -------------------------------------------------------------
!        MM5        CATEGORIES                             RADM
!     -------------------------------------------------------------
!         1      Evergreen Broadleaf Forest
!         2      Deciduous Broadleaf Forest
!         3      Deciduous and Evergreen Forest
!         4      Evergreen Needleleaf Forest
!         5      Deciduous Needleleaf Forest
!         6      Groundcover with Trees and Shrubs
!         7      Groundcover Only
!         8      Broadleaf Shrubs w/ Perennial Groundcover
!         9      Broadleaf Shrubs w/ Bare Soil
!        10      Groundcover with Dwarf Trees and Shrubs
!        11      Bare Soil
!        12      Agriculture or C3 Grassland
!        13      Persistent Wetland
!        14      Dry Coastal Complexes
!        15      Water
!        16      Ice Cap and Glacier
!         ?      Urban Land                                   1
!     -------------------------------------------------------------

      WRITE (6,9100)
      GOTO 1001

    ENDIF

!-------------------------------------------------------------------------------
!   Put land use on MCIP "X" grid.
!-------------------------------------------------------------------------------

    sc = x0
    ec = x0 + ncols_x - 1
    sr = y0
    er = y0 + nrows_x - 1

    xluse(:,:,1:iendlu) = luradmcat(sc:ec,sr:er,:)

    DEALLOCATE ( luradmcat )

    ! Fill dominant land-use array.

    xdluse(:,:) = 1.0

    DO row = 1, nrows_x
      DO col = 1, ncols_x

        idom = 1
        DO lu = 2, lumax
          IF ( xluse(col,row,lu) > xluse(col,row,idom) ) THEN
            xdluse(col,row) = FLOAT(lu)
            idom            = lu
          ENDIF
        ENDDO

      ENDDO
    ENDDO

!-------------------------------------------------------------------------------
! Otherwise, fill input land use from meteorological model into output arrays.
!-------------------------------------------------------------------------------

  ELSE

    lumax  = nummetlu
    lwater = MAX (met_lu_water, 7)  ! accounts for -999 prior to MM5v2.12

    IF ( iflufrc ) THEN

      sc = x0
      ec = x0 + ncols_x - 1
      sr = y0
      er = y0 + nrows_x - 1

      xluse (:,:,1:lumax) = lufrac (sc:ec,sr:er,:)
      xdluse(:,:)         = landuse(sc:ec,sr:er)

    ELSE

      DO col = 1, ncols_x
        ii = x0 + col - 1
        DO row = 1, nrows_x
          jj = y0 + row - 1

          lu = landuse(ii,jj)

          xluse (col,row,:)  = 0.0
          xluse (col,row,lu) = 1.0
          xdluse(col,row)    = lu

        ENDDO
      ENDDO

    ENDIF

  ENDIF

!-------------------------------------------------------------------------------
! Fill percentage of urban area based on amount of land in grid cell.
! For RADM, USGS, and old MM5, urban is category 1.
!-------------------------------------------------------------------------------

  IF ( iflufrc ) THEN
    DO row = 1, nrows_x
      DO col = 1, ncols_x
 
        IF ( xdluse(col,row) == lwater ) THEN  ! water is dominant category
          xpurb(col,row) = 0.0
        ELSE  ! land is dominant over water in cell
          IF ( xluse(col,row,lwater) < 1.0 ) THEN
            xpurb(col,row) = ( xluse(col,row,1) /  &
                               (1.0 - xluse(col,row,lwater)) ) * 100.0
          ELSE
            xpurb(col,row) = 0.0
          ENDIF
        ENDIF

      ENDDO
    ENDDO
  ENDIF

  RETURN

!-------------------------------------------------------------------------------
! Error-handling section.
!-------------------------------------------------------------------------------

 9000 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: GETLUSE',                            &
              /, 1x, '***   TOO MANY INPUT LAND USE CATEGORIES',           &
              /, 1x, '***   MAXIMUM ALLOWED (MAXLUC) = ', i4,              &
              /, 1x, '***   ATTEMPTED SIZE = ', i4,                        &
              /, 1x, 70('*'))

 9100 FORMAT (/, 1x, 70('*'),                                              &
              /, 1x, '*** SUBROUTINE: GETLUSE',                            &
              /, 1x, '***   CANNOT USE SiB LAND USE CATEGORIES WITH MCIP', &
              /, 1x, 70('*'))

 1001 CALL graceful_stop (pname)
      RETURN

END SUBROUTINE getluse
