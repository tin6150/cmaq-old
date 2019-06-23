                     README for CMAQv4.5.1 - 15 Mar 2006

renamed to rst by tin, so to be able to add notes (with highlight)
----------
	search for *vv* or *>>* 


 This README file outlines the steps necessary to build and run CMAQ 
 models. The code has been tested on a variety of platforms, but the build 
 and run scripts that are included in the tar files are set up to compile 
 and run on Linux (we tested on Redhat Linux 8.0 with the Portland Group 
 F90 compiler pgf90 version 5.0).  In case you want to run on another 
 platform, the C-shell scripts are easy to modify for any Unix 
 implementation.

 CMAQ version 4.5.1 (CMAQv4.5.1) is released to the user community 
 accompanied by example scripts that invoke a specific configuration of the 
 model. This configuration has been used by U.S. EPA in operational 
 evaluation studies prior to release of the model, and the results of these 
 evaluations accompany this release.  There are other features and options 
 within the CMAQ modeling system within this release, beyond this specific 
 model configuration, that have not yet been fully evaluated and 
 documented, that are also available to users.  As always, we are 
 interested in feedback from the user community on experiences using the 
 CMAQ modeling system.

 The Stand-Alone CMAQ package contains data and scripts to execute a series 
 of tutorial runs that demonstrate the CMAQ system. The tutorial case 
 produces concentrations for a 32 km horizontal resolution grid (coarse 
 domain) and a nested 8 km grid (fine domain).

 Input datasets for the tutorial case are provided in file M3DATA.tar.gz; 
 the user must work through the system of CMAQ models to produce the inputs 
 required for the downstream model(s).  These inputs (preprocessor model 
 outputs) must reside in the specified subdirectories (see below in item 
 13).  Model-generated datasets have been provided in a separate gzipped 
 tar file, M3DATA_REF.tar.gz, to compare with your outputs.  The run 
 scripts and comparison output data reflect the scenario period for the 
 tutorial runs:  two 24-hour periods starting 0Z:1999183 (2 July 1999) and 
 0Z:1999184 (3 July 1999).

 The distribution package consists of the following files:

 o README                   - this readme text file
 o TUTORIAL_PROCEDURE       - text file describing how to work through the
                              tutorial runs
 o AEROSOL_NOTES            - text file containing comments on aerosol
                              updates in CMAQv4.5.1
*vv* 
 o CVS_NETCDF               - text file explaining the CVS configuration
                              management system and the netCDF data system
                              and how to set them up
*vv* 
 o IOAPI                    - text file explaining how to get the I/O API
                              system and set up the libraries
 o RELEASE_NOTES            - text file containing a list of the changes
                              since the last release
 o MODELS.tar.gz            - gzipped tar file (~5.1 Mb) containing CVS source 
                              code archives for models, tools, and libraries
 o M3DATA.tar.gz            - gzipped tar file (~109 Mb) containing the
                              required datasets not produced by this package
							  *>>* where is this file??
							  		got from 4.7.1 download.  ~tin/gs/Downloads/CMAQ


 o SCRIPTS.tar.gz           - gzipped tar file (~16 Kb) containing C-Shell
                              scripts to build and execute the CMAQ models
 o M3DATA_REF.tar.gz        - gzipped tar file (~346 Mb) containing reference
                              data to compare with datasets produced by the
                              tutorial on a Linux workstation

 ** NOTE: You must have CVS, IOAPI and netCDF (see the CVS_NETCDF and IOAPI
          readme files for additional information).

 The following outlines a sequence of steps you can follow to build and run 
 the codes:

 1) Set environment variable (path) for M3HOME:

       setenv M3HOME /project/air5/sjr/CMAS4.5.1/rel

 2) Set environment variables (paths) for M3MODEL, M3LIB and M3DATA as:

       setenv M3MODEL  $M3HOME/models
       setenv M3LIB    $M3HOME/lib  (you need to create this subdirectory)
       setenv M3DATA   $M3HOME/data


*vv*
	https://www.cmascenter.org/download/software/cmaq/cmaq_4-5-1.cfm?DB=TRUE
	talks about M3DATA.CMAQv4.5.1.tar and M3DATA_REF.CMAQv4.5.1.tar 
	but does not show such file for download :(
	
	for now, downloaded from v4.7.1 
	https://www.cmascenter.org/download/software/cmaq/cmaq_4-7-1.cfm?DB=TRUE

	CMAQ default data files	Download (58 MB)	6/28/2010
		https://drive.google.com/uc?export=download&id=0B4Gx-y00i4D0TWF4SHNtc29vZ2c

	CMAQ benchmark output data for CMAQ	Download (291 MB)	6/28/2010


 3) cd to $M3HOME and gunzip and untar the data tar file, M3DATA.tar.gz.
    This will produce the following subdirectories:

       data/
           bcon/   <<<<<<< empty, to be filled by the user
           cctm/   <<<<<<< empty, to be filled by the user
           emis/
              tut02/
           icon/   <<<<<<< empty, to be filled by the user
           jproc/  <<<<<<< empty, to be filled by the user
           mcip3/
              M_08_99NASH/
              M_32_99NASH/
           pdm/    <<<<<<< empty, only needed if running pdm & ping
           procan/
           raw/
              bcon/
              icon/
              phot/

 4) Create (mkdir) the subdirectory $M3LIB and the following subdirectories
    under $M3LIB:

          build/
          ioapi_3/
          netCDF/
          pario/
          stenex/

  **Concerning netCDF:  The scripts assume that netCDF resides in the 
    $M3LIB path as $M3LIB/netCDF.  If netCDF is installed elsewhere on your 
    system, create a symbolic link in $M3LIB/netCDF to the existing netCDF 
    (see CVS_NETCDF).

    Example for Linux cluster:

       mkdir -p $M3LIB/netCDF/Linux
       cd $M3LIB/netCDF/Linux
       ln -s /project/air5/sjr/CMAS4.5.1/rel/lib/netCDF/Linux/lib/libnetcdf.a .

  **Concerning ioapi_3:  We recommend that you download IOAPI version 3.0
    from the CMAS/EMC web site and compile the libraries that you need.
    This is done by editing the appropriate Makeinclude file(s) for the
    compiler flags, if necessary, and setting the "BIN" environment variable
    appropriately - see the IOAPI readme file included with this release.

			**end** 

 5) In $M3HOME gunzip and untar the models archive tar file, MODELS.tar.gz. 
    This will produce the following subdirectories:

		**>>** using ~/gs/Downloads/CMAQ/M3MODELS.CMAQv4.5.1.tar.gz


       models/
              BCON/
              BUILD/
              CCTM/
              ICON/
              JPROC/
              PARIO/
              PDM/
              PROCAN/
              STENEX/
              TOOLS/
              include/

 6) Make a working directory (NOT in either the $M3MODEL, $M3LIB or $M3DATA
    trees), cd there and gunzip and untar SCRIPTS.tar.gz. This will produce
    the following subdirectories, which contain "bldit" and "run" C-shell
    scripts and a GRIDDESC file (see item 17(b). under "other details"
    below):

       scripts/
              GRIDDESC1
              bcon/
              build/
              cctm/
              icon/
              jproc/
              pario/
              pdm/
              procan/
              stenex/

    Not necessary, but for the sake of further discussion create an 
    environment variable for the "scripts" working directory, $WORK.

 7) First, create the IOAPI library required for the models.  See the
    IOAPI readme file included with this release.

       mkdir $M3LIB/ioapi_3   <<<<<<< install here

 8) Next create the stencil exchange library required for parallel
    processing (se_snl) and serial processing (sef90_noop):

       cd $WORK/stenex
       Execute (type) bldit.se.pgf
       Execute (type) bldit.se_noop.pgf

 9) For parallel CCTM operation create the parallel I/O library (pario):

       cd $WORK/pario
       Execute (type) bldit.pario.pgf

10) Create m3bld, the tool required to build the executables for the CMAQ
    processors, model and tools.

       cd $WORK/build
       execute (type) bldit.m3bld

    Note: Although m3bld is really a tool, we put it in with the "libraries."

11) Now create the model executables:  JPROC is created and run only once
    for the tutorial; ICON and BCON need to be compiled and run separately 
    for profile data (coarse grid) and for nest data (fine grid); CCTM is 
    compiled only once.  See the TUTORIAL_PROCEDURE readme file for details.

    Generally, you will need to get the MCIP3 code and run it to create met 
    data from MM5 for CCTM.  MCIP3 can be downloaded from the same site as 
    this distribution package.  And of course, you will need "model-ready" 
    emissions data - presumably from SMOKE.  See the SMOKE readme file 
    included with this package.  For this tutorial release we have provided 
    the model-ready emissions and met data.

    Start with JPROC (cd to $WORK/jproc).  Invoke "bldit.jproc.pgf".  There 
    will be a lot of text displayed to standard out (which you can capture 
    of course, by redirecting to a file).  The process should end with a 
    JPROC executable, which is invoked in the second script, "run.jproc", 
    producing output data files.  These data files will be inserted into the 
    path predefined in the run script, $M3DATA/jproc.
    
    **Note: The "run.jproc" script is set up to produce daily J-value 
      tables for the cb4_ae4_aq mechanism starting from 30 June to 14 July 
      1999. This works as long as you're not using TOMS data, in which case 
      you would need to run one day at a time.

    **Note: It's always a good idea to capture in a log file the text 
      written to standard out when running these models. In each "run" 
      script, near the top, is a suggested method (e.g. for JPROC):

           run.jproc >&! jproc.log &

12) Check the JPROC log file to ensure complete and correct execution. 
    Then cd to $WORK/icon and follow the same procedure; invoke 
    "bldit.icon.pgf", followed by "run.icon >&! icon.log &".  This will 
    produce the first (profile) dataset for the first run of CCTM on the 
    coarse domain. After CCTM finishes, you will need to generate a nest 
    dataset for the fine domain. See the TUTORIAL_PROCEDURE readme file for 
    details.

13) Follow this procedure for BCON and CCTM.  If you are running through the
    tutorial, see the TUTORIAL_PROCEDURE readme file.

14) Finishing with CCTM, you should have a complete collection of datasets, 
    which you can compare with the distribution datasets in 
    M3DATA_REF.tar.gz.  Unless you modify the run scripts, the output data 
    from all the models will reside in the following (automatically 
    generated) paths:

       $M3DATA/
              bcon/
              cctm/
              icon/
              jproc/

15) Concerning parallel CCTM operation: We have tested the "bldit" script 
    for both serial and parallel compilation.  The source code is the same 
    for both. Only some libraries are different as well as the run scripts. 
    The "stenex" library for parallel is different than for serial; "pario" 
    is needed only for parallel.  We ran successfully on a Scyld Beowulf 
    Linux cluster, but this release was set up and tested for a "standard" 
    MPICH linux cluster, requiring the addition of a C code that 
    distributes the run time environment from the node that launches the 
    run to the other participating nodes.  Thanks to Bo Wang and Zion Wang 
    of CERT-UC-Riverside, who developed and tested this code.  Also, see 
    the PARALLEL_NOTES readme file.  (Note: The initial concentrations pre-
    processor, ICON can also be executed in parallel, but we have not tested
    this for Linux clusters.)

16) Concerning non-parallel CCTM operation (to run the model in serial):

    Modify the bldit.cctm.linux script as follows and build the single 
    processor version of CMAQ:

      43c43
      <  set APPL  = e2a
      ---
      >  set APPL  = e1a
      52c52
      <  set ParOpt             # set for multiple PE's; comment out for single PE
      ---
      > #set ParOpt             # set for multiple PE's; comment out for single PE


    Then modify the run.cctm script as follows:

      7c7
      < # Usage: run.cctm >&! cctm_e2a.log &                                  #
      ---
      > # Usage: run.cctm >&! cctm_e1a.log &                                  #
      22,23c22,23
      <  set APPL     = e2a
      <  set CFG      = e2a
      ---
      >  set APPL     = e1a
      >  set CFG      = e1a
      28,29c28,29
      < #setenv NPCOL_NPROW "1 1"; set NPROCS   = 1 # single processor setting
      <  setenv NPCOL_NPROW "4 2"; set NPROCS   = 8
      ---
      >  setenv NPCOL_NPROW "1 1"; set NPROCS   = 1 # single processor setting
      > #setenv NPCOL_NPROW "4 2"; set NPROCS   = 8
      188c188
      < # time  $BASE/$EXEC
      ---
      >   time  $BASE/$EXEC
      191,194c191,194
      <  set MPIRUN = /share/linux/bin/mpich-ch_p4/bin/mpirun
      <  set TASKMAP = $BASE/machines8
      <  cat $TASKMAP
      <  time $MPIRUN -v -machinefile $TASKMAP -np $NPROCS $BASE/$EXEC
      ---
      > #set MPIRUN = /share/linux/bin/mpich-ch_p4/bin/mpirun
      > #set TASKMAP = $BASE/machines8
      > #cat $TASKMAP
      > #time $MPIRUN -v -machinefile $TASKMAP -np $NPROCS $BASE/$EXEC


    Note: You can change the default script by using the Unix "patch" 
          utility. Cut the indented section listed above into a file, say 
          "mod." Then type "patch run.cctm mod."

17) Other details:

 a. You can check output ioapi file headers (and data) using the netCDF
    utility ncdump. This utility will be located in the same place as
    netcdf, mentioned in (4) above.

 b. The GRIDDESC file contains horizontal projection and grid domain
    definitions that are required input for many CMAQ models. The run
    scripts for ICON, BCON, and CCTM contain environment variables that
    point to the GRIDDESC file.
    
    The horizontal grid definition can be set to window from the met and 
    emissions input files. However, the window must be a "proper subset" 
    (i.e., a subset from the interior of the domain and not including 
    boundaries).  Note:  The domains represented by the met and emissions 
    data must be the same.

 c. Running CCTM for a windowed domain or a higher resolution nested domain 
    from larger or coarser met and emissions datasets requires creating
    initial and boundary data for the target domain using ICON and BCON.

