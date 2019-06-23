
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
! $Header: /project/work/rep/MCIP2/src/mcip2/transm.F,v 1.1.1.1 2002/03/09 14:48:05 yoj Exp $ 


SUBROUTINE transm (path, prw, ftabs, ftscat, fbscat)

!-------------------------------------------------------------------------------
! Name:     Transmissivity
! Purpose:  Obtains shortwave transmissivity as a function of
!             1) path length (1-10) (path length = 1 when zenith angle is 0)
!             2) precipitable water (0-5 cm)
!           using bilinear interpolation from look-up tables determined from
!           a shortwave model of Carlson and Borland (JAM, 1978).
! Revised:  ?? ??? 19??  Original version.  (???)
!           10 Sep 2001  Converted to free-form f90.  (T. Otte)
!-------------------------------------------------------------------------------

  IMPLICIT NONE

  REAL,          INTENT(OUT)   :: fbscat     ! backscattering coeff [0-1]
  REAL                         :: fract
  REAL                         :: fract1
  REAL                         :: fract2 
  REAL,          INTENT(OUT)   :: ftabs      ! absorption transmissivity [0-1]
  REAL,          INTENT(OUT)   :: ftscat     ! scattering transmissivity [0-1]
  REAL                         :: gract
  REAL                         :: gract1
  REAL                         :: gract2 
  INTEGER                      :: iom
  INTEGER                      :: ipath
  INTEGER                      :: jom
  INTEGER                      :: jpath
  REAL,          INTENT(INOUT) :: path       ! path length
  REAL,          INTENT(INOUT) :: prw        ! precipitable water [cm]
  REAL                         :: tran(13,46)

  DATA tran /                                                            &
      .926,.868,.855,.846,.838,.832,.827,.822,.818,.814,.811,.776,.314,  &
      .915,.854,.840,.831,.823,.817,.811,.806,.802,.798,.794,.740,.313,  &
      .903,.841,.826,.816,.808,.802,.796,.791,.787,.782,.779,.707,.312,  &
      .892,.828,.813,.803,.795,.788,.782,.777,.772,.768,.764,.676,.311,  &
      .881,.815,.800,.790,.782,.775,.769,.763,.758,.754,.750,.648,.310,  &
      .870,.803,.788,.777,.769,.762,.756,.750,.745,.741,.737,.620,.310,  &
      .860,.792,.776,.765,.757,.750,.743,.738,.733,.728,.724,.595,.309,  &
      .850,.781,.765,.754,.745,.738,.731,.726,.721,.716,.712,.571,.308,  &
      .839,.770,.753,.742,.733,.726,.720,.714,.709,.704,.700,.549,.308,  &
      .830,.759,.743,.731,.722,.715,.709,.703,.698,.693,.689,.527,.307,  &
      .820,.748,.732,.721,.712,.704,.698,.692,.687,.682,.678,.507,.307,  &
      .810,.738,.722,.710,.701,.694,.687,.681,.676,.671,.667,.488,.307,  &
      .801,.728,.712,.700,.691,.683,.677,.671,.666,.661,.656,.470,.306,  &
      .791,.719,.702,.690,.681,.674,.667,.661,.656,.651,.646,.453,.306,  &
      .782,.709,.692,.681,.671,.664,.657,.651,.646,.641,.636,.437,.306,  &
      .773,.700,.683,.671,.662,.654,.648,.642,.636,.631,.627,.422,.305,  &
      .764,.691,.674,.662,.653,.645,.638,.632,.627,.622,.618,.407,.305,  &
      .756,.682,.665,.653,.644,.636,.629,.623,.618,.613,.608,.393,.305,  &
      .747,.673,.656,.644,.635,.627,.621,.615,.609,.604,.600,.380,.305,  &
      .738,.665,.647,.636,.626,.619,.612,.606,.600,.596,.591,.367,.305,  &
      .730,.656,.639,.627,.618,.610,.603,.597,.592,.587,.582,.355,.305,  &
      .722,.648,.631,.619,.610,.602,.595,.589,.584,.579,.574,.344,.304,  &
      .714,.640,.623,.611,.602,.594,.587,.581,.576,.571,.566,.333,.304,  &
      .706,.632,.615,.603,.594,.586,.579,.573,.568,.563,.558,.322,.304,  &
      .698,.624,.607,.595,.586,.578,.571,.565,.560,.555,.550,.312,.304,  &
      .690,.616,.599,.588,.578,.571,.564,.558,.552,.547,.543,.302,.304,  &
      .683,.609,.592,.580,.571,.563,.556,.550,.545,.540,.535,.293,.304,  &
      .675,.602,.585,.573,.564,.556,.549,.543,.538,.533,.528,.284,.304,  &
      .668,.594,.577,.566,.556,.549,.542,.536,.531,.526,.521,.276,.304,  &
      .661,.587,.570,.559,.549,.542,.535,.529,.524,.519,.514,.268,.304,  &
      .653,.580,.563,.552,.542,.535,.528,.522,.517,.512,.507,.260,.304,  &
      .646,.573,.556,.545,.536,.528,.521,.515,.510,.505,.501,.253,.304,  &
      .639,.567,.550,.538,.529,.521,.515,.509,.503,.499,.494,.245,.304,  &
      .633,.560,.543,.532,.522,.515,.508,.502,.497,.492,.488,.238,.304,  &
      .626,.553,.537,.525,.516,.508,.502,.496,.491,.486,.481,.232,.304,  &
      .619,.547,.530,.519,.510,.502,.496,.490,.484,.480,.475,.225,.304,  &
      .613,.541,.524,.512,.503,.496,.489,.484,.478,.473,.469,.219,.304,  &
      .606,.534,.518,.506,.497,.490,.483,.477,.472,.467,.463,.213,.304,  &
      .600,.528,.512,.500,.491,.484,.477,.472,.466,.462,.457,.208,.304,  &
      .594,.522,.506,.494,.486,.478,.472,.466,.461,.456,.451,.202,.304,  &
      .587,.516,.500,.489,.480,.472,.466,.460,.455,.450,.446,.197,.304,  &
      .581,.511,.494,.483,.474,.467,.460,.454,.449,.444,.440,.192,.304,  &
      .575,.505,.488,.477,.468,.461,.455,.449,.444,.439,.435,.187,.304,  &
      .569,.499,.483,.472,.463,.456,.449,.444,.438,.434,.429,.182,.304,  &
      .563,.494,.477,.466,.458,.450,.444,.438,.433,.428,.424,.178,.304,  &
      .558,.488,.472,.461,.452,.445,.439,.433,.428,.423,.419,.173,.304/

!-------------------------------------------------------------------------------
! Ensure path length and precipitable water values are within bounds.
!-------------------------------------------------------------------------------

  path = AMIN1( path, 9.99 )
  path = AMAX1( path, 1.01 )
  prw  = AMIN1( prw,  4.99 )
  prw  = AMAX1( prw,  0.01 )

!-------------------------------------------------------------------------------
! 46 path lengths, 1-10, interval=0.2.
!-------------------------------------------------------------------------------

  fract  = 5.0 * (path-1.0) + 1.0
  ipath  = IFIX(fract+0.001)
  jpath  = ipath + 1
  fract1 = fract - ipath
  fract2 = 1.0 - fract1

!-------------------------------------------------------------------------------
! 11 precip water amts., 0-5 cm, interval=0.5.
!-------------------------------------------------------------------------------

  gract  = 2.0 * prw + 1.0
  iom    = IFIX(gract+0.001)
  jom    = iom + 1
  gract1 = gract - iom
  gract2 = 1.0 - gract1

!-------------------------------------------------------------------------------
! Interpolate to get absorption, scattering, and backscattering values.
! - bilinear interpolation for abs (F(path,omega)
! - linear interpolation for scat, bscat (F(path only))
!-------------------------------------------------------------------------------

  ftabs  = fract2 * gract2 * tran(iom,ipath) +   &
           fract1 * gract1 * tran(jom,jpath) +   &
           fract1 * gract2 * tran(iom,jpath) +   &
           fract2 * gract1 * tran(jom,ipath)

  ftscat = fract2*tran(12,ipath) + fract1*tran(12,jpath)
  fbscat = fract2*tran(13,ipath) + fract1*tran(13,jpath)

END SUBROUTINE transm
