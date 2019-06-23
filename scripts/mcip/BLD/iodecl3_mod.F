
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
! $Header: /project/work/rep/MCIP2/src/mcip2/iodecl3_mod.F,v 1.2 2004/08/30 16:46:04 tlotte Exp $ 


MODULE iodecl3

!-------------------------------------------------------------------------------
! Name:     I/O Declarations for Models-3
! Purpose:  Contains declarations and usage comments for the Models-3 (M3)
!           Interprocess Communication Applications Programming Interface (API).
!           (etc.) token values for FORTRAN Models-3 I/O System API.
! Revised:  ?? Mar 1992  Prototype.  (C. Coats)
!           10 Sep 2001  Converted to free-form f90.  (T. Otte)
!           08 Jul 2004  Updated to reflect changes since last version.
!                        (T. Otte)
!-------------------------------------------------------------------------------

  LOGICAL,      EXTERNAL  :: check3   !  is JDATE:JTIME available for FNAME?
  LOGICAL,      EXTERNAL  :: close3   !  close FNAME
  LOGICAL,      EXTERNAL  :: desc3    !  Puts M3 file desc into FDESC3.EXT
  LOGICAL,      EXTERNAL  :: filchk3  !  check file type and dimensions
  INTEGER,      EXTERNAL  :: init3    !  Inits M3 API and returns unit for log
  LOGICAL,      EXTERNAL  :: shut3    !  Shuts down API
  LOGICAL,      EXTERNAL  :: open3    !  opens an M3 file
  LOGICAL,      EXTERNAL  :: read3    !  read M3 file for var,layer,timestep
  LOGICAL,      EXTERNAL  :: write3   !  write timestep to M3 file
  LOGICAL,      EXTERNAL  :: xtract3  !  extract window from timestep in M3 file
  LOGICAL,      EXTERNAL  :: interp3  !  do time interpolation from a M3 file
  LOGICAL,      EXTERNAL  :: ddtvar3  !  do time derivative from M3 file

  LOGICAL,      EXTERNAL  :: interpx  !  time interpolation from a window
                                      !  extraction from an M3 gridded file
! LOGICAL,      EXTERNAL  :: pinterpb !  parallel time interpolation from an
                                      !  M3 boundary file

  LOGICAL,      EXTERNAL  :: inqatt3  !  inquire attributes in M3 file
  LOGICAL,      EXTERNAL  :: rdatt3   !  read numeric attributes by name from M3 file
  LOGICAL,      EXTERNAL  :: wratt3   !  add new numeric attributes
  LOGICAL,      EXTERNAL  :: rdattc   !  read CHAR attributes
  LOGICAL,      EXTERNAL  :: wrattc   !  add new CHAR attributes

  LOGICAL,      EXTERNAL  :: sync3    !  flushes file to disk, etc.

!.......................................................................
!..................  API FUNCTION USAGE AND EXAMPLES  ..................
!.......
!.......   In the examples below, names (FILENAME, PROGNAME, VARNAME)
!.......   should be CHARACTER*16, STATUS and RDFLAG are LOGICAL, dates
!.......   are INTEGER, coding the Julian date as YYYYDDD, times are
!.......   INTEGER, coding the time as HHMMSS, and layer,
!.......   row, and column specifications use INTEGER FORTRAN array
!.......   index conventions (in particular, they are based at 1, not
!.......   based at 0, as in C).
!.......   Parameter values for "ALL...", for grid and file type IDs,
!.......   and for API dimensioning values are given in PARMS3.EXT;
!.......   file descriptions are passed via commons BDESC3 and CDESC3
!.......   in file FDESC3.EXT.
!.......
!.......   CHECK3():  check whether timestep JDATE:JTIME is available 
!.......   for variable VNAME in file FILENAME.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = CHECK3 ( FILENAME, VNAME, JDATE, JTIME )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (data-record not available in file FNAME)
!.......       END IF
!.......
!.......   CLOSE3():  check whether timestep JDATE:JTIME is available 
!.......   for variable VNAME in file FILENAME.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = CLOSE3 ( FILENAME )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... could not flush file to disk successfully,
!.......           or else file not currently open.
!.......       END IF
!.......
!.......   DESC3():   return description of file FILENAME to the user
!.......   in commons BDESC3 and CDESC3, file FDESC3.EXT.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = DESC3 ( FILENAME )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (file not yet opened)
!.......       END IF
!.......       ...
!.......       (Now common FDESC3 (file FDESC3.EXT) contains the descriptive
!.......       information for this file.)
!.......
!.......   FILCHK3():   check whether file type and dimensions for file 
!.......   FILENAME match the type and dimensions supplied by the user.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = FILCHK3 ( FILENAME, FTYPE, NCOLS, NROWS, NLAYS, NTHIK )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (file type and dimensions do not match
!.......                the supplied FTYPE, NCOLS, NROWS, NLAYS, NTHIK)
!.......       END IF
!.......       ...
!.......
!.......   INIT3():  set up the M3 API, open the program's log file, and
!.......   return the unit FORTRAN number for log file.  May be called
!.......   multiple times (in which case, it always returns the log-file's
!.......   unit number).  Note that block data INITBLK3.FOR must also be
!.......   linked in.
!.......   FORTRAN usage is:
!.......
!.......       LOGDEV = INIT3 ( )
!.......       IF ( LOGDEV .LT. 0 ) THEN
!.......           ... (can't proceed:  probably can't open the log.
!.......                Stop the program)
!.......       END IF
!.......
!.......   OPEN3():  open file FILENAME from program PROGNAME, with
!.......   requested read-write/old-new status.  For files opened for WRITE,
!.......   record program-name and other history info in their headers.
!.......   May be called multiple times for the same file (in which case,
!.......   it returns true unless the request is for READ-WRITE status
!.......   for a file already opened READ-ONLY).  Legal statuses are:
!.......   FSREAD3: "old read-only"
!.......   FSRDWR3: "old read-write"
!.......   FSNEW3:  "new (read-write)"
!.......   FSUNKN3: "unknown (read_write)"
!.......   FORTRAN usage is:
!.......
!.......       STATUS = OPEN3 ( FILENAME, FSTATUS, PROGNAME )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (process the error)
!.......       END IF
!.......
!.......   READ3():  read data from FILENAME for timestep JDATE:JTIME,
!.......   variable VNAME, layer LAY, into location  ARRAY.
!.......   If VNAME==ALLVARS3=='ALL         ', reads all variables;
!.......   if LAY==ALLAYS3==-1, reads all layers.
!.......   Offers random access to the data by filename, date&time, variable,
!.......   and layer.  For DICTIONARY files, logical name for file being
!.......   requested maps into the VNAME argument.  For time-independent
!.......   files (including DICTIONARY files), JDATE and JTIME are ignored.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = READ3 ( FILENAME, VNAME, LAY, JDATE, JTIME, ARRAY )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (read failed -- process this error.)
!.......       END IF
!.......
!.......   SHUT3():  Flushes and closes down all M3 files currently open.
!.......   Must be called before program termination; if it returns FALSE
!.......   the run must be considered suspect.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = SHUT3 ( )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (Flush of files to disk probably didn't work;
!.......                look at netCDF error messages)
!.......       END IF
!.......
!.......   WRITE3():  write data from ARRAY to file FILENAME for timestep
!.......   JDATE:JTIME.  For GRIDDED, BUONDARY, and CUSTOM files, VNAME
!.......   must be a variable found in the file, or else ALLVARS3=='ALL'
!.......   to write all variables from ARRAY.  For other file types,
!.......   VNAME _must_ be ALLVARS3.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = WRITE3 ( FILENAME, VNAME, JDATE, JTIME, ARRAY )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (write failed -- process this error.)
!.......       END IF
!.......
!.......   XTRACT3():  read/extract gridded data into location  ARRAY
!.......   from FILENAME for time step JDATE:JTIME, variable VNAME
!.......   and the data window defined by
!.......       LOLAY  <=  layer   <=  HILAY,
!.......       LOROW  <=  row     <=  HIROW,
!.......       LOCOL  <=  column  <=  HICOL
!.......   FORTRAN usage is:
!.......
!.......       STATUS = XTRACT3 ( FILENAME, VNAME,
!.......   &                      LOLAY, HILAY,
!.......   &                      LOROW, HIROW,
!.......   &                      LOCOL, HICOL,
!.......   &                      JDATE, JTIME, ARRAY )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (extract failed -- process this error.)
!.......       END IF
!.......
!.......   INTERP3():  read/interpolate gridded, boundary, or custom data 
!.......   into location  ARRAY from FILENAME for time JDATE:JTIME, variable 
!.......   VNAME, and all layers.  Note use of ASIZE = transaction size =
!.......   size of ARRAY, for error-checking.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = INTERP3 ( FILENAME, VNAME, JDATE, JTIME,
!.......   &                      ASIZE, ARRAY )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (interpolate failed -- process this error.)
!.......       END IF
!.......
!.......   INTERPX():  read/interpolate/window gridded, boundary, or custom
!.......   data into location  ARRAY from FILENAME for time JDATE:JTIME, 
!.......   variable VNAME, and all layers.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = INTERPX ( FILENAME, VNAME, CALLER, 
!.......   &                      COL0, COL1, ROW0, ROW1, LAY0, LAY1,
!.......   &                      JDATE, JTIME, ARRAY )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (windowed interpolate failed -- process this error.)
!.......       END IF
!.......
!.......   DDTVAR3():  read and calculate mean time derivative (per second) 
!.......   for gridded, boundary, or custom data.  Put result into location  
!.......   ARRAY from FILENAME for time JDATE:JTIME, variable VNAME, and all 
!.......   layers.  Note use of ASIZE = transaction size = size of ARRAY, 
!.......   for error-checking.  Note  d/dt( time-independent )==0.0
!.......   FORTRAN usage is:
!.......
!.......       STATUS = DDTVAR3 ( FILENAME, VNAME, JDATE, JTIME,
!.......   &                      ASIZE, ARRAY )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (operation failed -- process this error.)
!.......       END IF
!.......
!.......   INQATT():  inquire how many attributes there are for a
!.......   particular file and variable (or for the file globally,
!.......   if the variable-name ALLVAR3 is used)), and what the 
!.......   names, types, and array-dimensions of these attributes are.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = INQATT3( FNAME, VNAME, MXATTS, 
!.......   &                     NATTS, ANAMES, ATYPES, ASIZES )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (operation failed -- process this error.)
!.......       END IF
!....... 
!.......   RDATT3():  Reads an INTEGER, REAL, or DOUBLE attribute by name
!.......   for a specified file and variable into a user-specified array.
!.......   If variable name is ALLVAR3, reads the file-global attribute.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = RDATT3( FNAME, VNAME, ANAME, ATYPE, AMAX,
!.......   &                    ASIZE, AVAL )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (operation failed -- process this error.)
!.......       END IF
!.......
!.......   WRATT3():  Writes an INTEGER, REAL, or DOUBLE attribute by name
!.......   for a specified file and variable.  If variable name is ALLVAR3, 
!.......   reads the file-global attribute.
!.......
!.......       STATUS =  WRATT3( FNAME, VNAME, 
!.......   &                     ANAME, ATYPE, AMAX, AVAL )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (operation failed -- process this error.)
!.......       END IF
!.......   
!.......   RDATTC():  Reads a CHARACTER string attribute by name
!.......   for a specified file and variable into a user-specified array.
!.......   If variable name is ALLVAR3, reads the file-global attribute.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = RDATTC( FNAME, VNAME, ANAME, CVAL )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (operation failed -- process this error.)
!.......       END IF
!.......
!.......   WRATT3():  Writes a CHARACTER string attribute by name
!.......   for a specified file and variable.  If variable name is ALLVAR3, 
!.......   reads the file-global attribute.
!.......
!.......       STATUS =  WRATTC( FNAME, VNAME, ANAME, CVAL )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (operation failed -- process this error.)
!.......       END IF
!.......
!.......   SYNC3():   Synchronize FILENAME with disk (flush output;
!.......   re-read header and invalidate data-buffers for input.
!.......   FORTRAN usage is:
!.......
!.......       STATUS = SYNC3 ( FILENAME )
!.......       IF ( .NOT. STATUS ) THEN
!.......           ... (file not yet opened, or disk-synch failed)
!.......       END IF
!.......       ...
!.......

END MODULE iodecl3
