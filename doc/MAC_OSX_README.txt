   
                        CMAQ 4.3 README, ANNOTATED FOR DARWIN/XLF

 This README file outlines the steps necessary to build and run CMAQ models.
 The code has been tested on a variety of platforms, but the build and run
 scripts that are included in the tar files are set up to compile and run on
 Linux (we tested on Redhat Linux 7.3 with the Portland Group F90 compiler
 (pgf90 version 4.0-2). We did not include scripts nor test for use with the
 Intel F90 compiler as in the previous "interim" release. In case you want to
 run on another platform, the C-shell scripts are easy to modify for any Unix
 implementation.

 The Stand-Alone CMAQ package contains data and default setups to execute a
 series of tutorial runs to demonstrate the usage of the scripts and data.
 The tutorial produces concentrations for a 32 km horizontal resolution grid
 (coarse domain) and a nested 8 km grid (fine domain).

 The package will have datasets in the $M3DATA input directories only; the user
 must work through the system of CMAQ models to produce the inputs required for
 the downstream model(s). These inputs (preprocessor model outputs) must reside
 in the specified $M3DATA subdirectories (see below in item 13). We have
 provided model-generated datasets for the purpose of comparison with your
 outputs in a separate tar file, M3DATA_REF.tar. The run scripts and comparison
 output data reflect the scenario period for the tutorial runs: two 24 hour
 periods starting 0Z:1999183 (2 July 1999) and 0Z:1999184 (3 July 1999).

 The distribution package consists of the following files:
 o README                   - this readme text file
 o CVS_NETCDF               - text file explaining the CVS configuration
                              management system and the netCDF data system
                              and how to set them up
 o IOAPI                    - text file explaining how to get the I/O API
                              system and set up the libraries
 o SMOKE                    - text file describing the SMOKE emissions system
                              and how to set it up (not required for the
                              tutorial)
 o PARALLEL_NOTES           - text file containing comments related to running
                              the CMAQ CCTM in Linux MPICH clusters
 o TUTORIAL_PROCEDURE       - text file describing how to work through the
                              tutorial
 o RELEASE_NOTES            - text file containing a list of the major changes
                              since the last release, along with the previous
                              release notes
 o MODELS.tar.gz            - gzipped tar file (~2.7 Mb) containing model,
                              tools and libraries source code CVS archives
 o M3DATA.tar.gz            - gzipped tar file (~106 Mb) containing the
                              required datasets not produced by this package
 o SCRIPTS.tar.gz           - gzipped tar file (~12 Kb) containing C-Shell
                              scripts to build and execute the CMAQ models
 o M3DATA_REF.tar.gz        - gzipped tar file (~315 Mb) containing reference
                              data to compare with datasets produced by the
                              tutorial on a Linux workstation

 ** NOTE: You must have CVS, IOAPI and netCDF (see the CVS_NETCDF and IOAPI
          readme's for help).

 The following outlines a sequence of steps you can follow to build and run
 the codes:

 1) Set environment variable (path) for M3HOME, e.g. user "yoj" could set:
       setenv M3HOME /project/cmaq/yoj

 2) Set environment variables (paths) for M3MODEL, M3LIB and M3DATA as:
       setenv M3MODEL  $M3HOME/models
       setenv M3LIB    $M3HOME/lib  (you may need to create this subdirectory)
       setenv M3DATA   $M3HOME/data

 3) cd to $M3HOME and gunzip and untar the data tar file, M3DATA.tar. This will
    produce the following subdirectories:
       data/
           cctm/   <<<<<<< empty, to be filled by the user
           bcon/   <<<<<<< empty, to be filled by the user
           icon/   <<<<<<< empty, to be filled by the user
           jproc/  <<<<<<< empty, to be filled by the user
           mcip2/
              M_32_99NASH/
              M_08_99NASH/
           emis/
              tut02/
           raw/
              phot/
              icon/
              bcon/

 4) Create (mkdir) the subdirectory $M3LIB and the following subdirectories
    under $M3LIB:
          build/
          netCDF/
          ioapi_22/
          stenex/
          pario/
          dynmem/
          
    Concerning netCDF: The scripts assume that netCDF resides in the $M3LIB
    path as $M3LIB/netCDF. You need to install your own netCDF libraries
    built for Linux (SunOS5) in this directory (see CVS_NETCDF) or symbolically
    link to the existing netcdf on your system.

    Example for Linux cluster:
       mkdir -p $M3LIB/netCDF/Linux
       cd $M3LIB/netCDF/Linux
       ln -s /home/showard/netcdf-3.4_linux/lib/libnetcdf.a libnetcdf.a

    Concerning ioapi_22: We no longer support the I/O API. We recommend that
    you download IOAPI version 2.2 from the CMAS/EMC web site and compile the
    libraries that you need. This is done by editing the appropriate
    Makeinclude file(s) for the compiler flags, if necessary, and setting the
   "BIN" environment variable appropriately - see the IOAPI readme included
    with this release.

 5) In $M3HOME untar the models archive tar file, MODELS.tar. This will
    produce the following subdirectories:
       models/
              CCTM/
              PARIO/
              include/
              BUILD/
              DYNMEM/
              STENEX/
              PROCAN/
              JPROC/
              ICON/
              BCON/

 6) Make a working directory (NOT in either the $M3MODEL, $M3LIB or $M3DATA
    trees), cd there and untar the SCRIPTS.tar. This will produce the following
    subdirectories, which contain "bldit" and "run" C-shell scripts and a
    GRIDDESC file (see item c. under "other details" below):
       bcon/
       icon/
       cctm/
       build/
       stenex/
       jproc/
       pario/
       dynmem/
       GRIDDESC1

    Not necessary, but for the sake of further discussion create an environment
    variable for the top level of your working directory, $WORK.

 7) First, create the IOAPI library required for the models. See the IOAPI
    readme file included with this release.

       mkdir $M3LIB/ioapi_22   <<<<<<< install here

 8) Next create the stencil exchange library required for parallel processing
    and the no-op version for serial processing:

       cd $WORK/stenex

	<BEGIN DARWIN/XLF SPECIFIC>
		In bldit.se_noop ...
		1) comment out the if block that detects and requires linux
			<BEFORE>
			 if ($BLD_OS != 'Linux') then
			    echo "   $BLD_OS -> wrong bldit script for host\!"
			    exit 1
			    endif
			<AFTER>
			#if ($BLD_OS != 'Linux') then
			#   echo "   $BLD_OS -> wrong bldit script for host\!"
			#   exit 1
			#   endif
		2) replace compiler path to point to xlf (your xlf may be located somewhere else)
			<BEFORE>
			set FC = /usr/pgi/linux86/bin/pgf90
			<AFTER>
			set FC = /opt/ibmcmp/xlf/8.1/bin/xlf
		3) replace compiler options
			<BEFORE>
			set FSTD = "-Mfixed -Mextend -c"
			<AFTER>
			set FSTD = "-qfixed=132 -c"
		4) make sure the library that has been created gets ranlib'd
			<BEFORE>
			ar rv $Arc *.o
			chmod 444 $Arc
			<AFTER>
			ar rv $Arc *.o
 			ranlib $Arc
 			chmod 444 $Arc
	<END DARWIN/XLF SPECIFIC>

	<BEGIN DARWIN/XLF SPECIFIC>
		We have not tested the mpich parrallel code, so we ignore bldit.se.pgf and only build bldit.se_noop.
		Changes to bldit.se would be identical.
	<END DARWIN/XLF SPECIFIC>

       Execute (type) bldit.se.pgf        <ignored>
       Execute (type) bldit.se_noop.pgf

 9) For parallel CCTM operation create the parallel I/O and associated dynamic
    memory libraries:

	<BEGIN DARWIN/XLF SPECIFIC>
		again, not using parallel code, step 9 and move to step 10
	<END DARWIN/XLF SPECIFIC>

       cd $WORK/pario
       Execute (type) bldit.pario.pgf

       cd $WORK/dynmem
       Execute (type) bldit.dynmem.pgf

10) Create m3bld, the tool required to build all the other executables.

       cd $WORK/build

	<BEGIN DARWIN/XLF SPECIFIC>
		in bldit.m3bld...
		1) comment out the linux checker
			<BEFORE>
			 if ($BLD_OS != 'Linux') then
			    echo "   $BLD_OS -> wrong bldit script for host\!"
			    exit 1
			    endif
			<AFTER>
			# if ($BLD_OS != 'Linux') then
			#    echo "   $BLD_OS -> wrong bldit script for host\!"
			#    exit 1
			#    endif
	<END DARWIN/XLF SPECIFIC>

       execute (type) bldit.m3bld

    Note: Although m3bld is really a tool, we put it in with the "libraries."

	<BEGIN DARWIN/XLF PORT>
		running bldit.m3bld should have generated lots of compiler errors.
		now go back into bldit.m3bld...
		1) comment out the portion that removes source files, as well as the part that tells cvs to reload them
			<BEFORE>
			 set Bld = $BASE/BLD
			#unset echo
			 if ( ! -e "$Bld" ) then
			    mkdir $Bld
			    else
			    if ( ! -d "$Bld" ) then
			       echo "   *** target exists, but not a directory ***"
			       exit 1
			       else
			       echo "Deleting files in $Bld"
			       /bin/rm $Bld/*
			       endif
			    endif
			#set echo
			 cd $Bld

			# extract source files from cvs archive
			  cvs export -r $Rel -d $Bld includes 
			  cvs export -r $Rel -d $Bld m3bld 
			<AFTER>
			 set Bld = $BASE/BLD
			#unset echo
			 if ( ! -e "$Bld" ) then
			    mkdir $Bld
			    else
			    if ( ! -d "$Bld" ) then
			       echo "   *** target exists, but not a directory ***"
			       exit 1
			       else
			       #echo "Deleting files in $Bld"
			       #/bin/rm $Bld/*
			       endif
			    endif
			#set echo
			 cd $Bld

			# extract source files from cvs archive
			# cvs export -r $Rel -d $Bld includes 
			# cvs export -r $Rel -d $Bld m3bld

		2) now make sure all the code is compiled with AIX and _AIX defined
			<BEFORE>
			# set c compiler and flags
			 set CC = /usr/bin/gcc
			 set CFLAGS = " "
			<AFTER>
			# set c compiler and flags
			 set CC = /usr/bin/gcc
			 set CFLAGS = " -DAIX -D_AIX "

		3) now open BLD/sms.h and edit the definition of boolean_t to be guaranteed
		   by commenting out the #if block
			<BEFORE>
			#if defined(CRAY) || defined(__osf__) || _WIN32 || __unix__
			typedef unsigned char boolean_t;
			#define B_TRUE 1
			#define B_FALSE 0
			#endif  /* CRAY */
			<AFTER>
			// #if defined(CRAY) || defined(__osf__) || _WIN32 || __unix__
			typedef unsigned char boolean_t;
			#define B_TRUE 1
			#define B_FALSE 0
			// #endif  /* CRAY */

		4) open BLD/bld_parser.c and remove the inclusion of values.h (line 263); gcc doesn't
		   recognize it and the file builds fine without it.
			<BEFORE>
			#ifndef _WIN32
			#include <values.h>
			#endif
			<AFTER>
			#ifndef _WIN32
			//#include <values.h>
			#endif

		5) now run bldit.m3bld again

	<END DARWIN/XLF SPECIFIC>

11) Now create the model executables: JPROC is created and run only once for
    the tutorial; ICON and BCON need to be compiled and run separately for
    profile data (coarse grid) and for nest data (fine grid); CCTM is compiled
    only once. See the TUTORIAL_PROCEDURE readme file for details.

    Generally, you will need to get the MCIP2 code and run it to create met
    data from MM5 for CCTM. MCIP2 can be downloaded from the same site as this
    distribution package. And of course, you will need "model-ready" emissions
    data - presumably from SMOKE. See the SMOKE readme file included with this
    package. For this tutorial release we have provided the model-ready
    emissions and met data.

    Start with JPROC (cd to $WORK/jproc). Invoke "bldit.jproc.pgf". There
    will be a lot of text displayed to standard out (which you can capture of
    course, by redirecting to a file). The process should end with a JPROC
    executable, which is invoked in the second script, "run.jproc", producing
    output data files. These data files will be inserted into the path
    predefined in the run script, $M3DATA/jproc.
    Note: The "run.jproc" script is set up to produce daily J-value tables for
    the cb4_ae3_aq mechanism starting from 30 June to 14 July 1999. This works
    as long as you're not using TOMS data, in which case you would need to run
    one day at a time.
    Note: It's always a good idea to capture in a log file the text written
          to standard out when running these models. In each "run" script,
          near the top, is a suggested method. e.g. for JPROC,
          run.jproc >&! jproc.log &

	<BEGIN DARWIN/XLF SPECIFIC>
		in bldit.jproc.pgf...

		1) again , comment out linux detection.

		2) now change the fortran compiler path to xlf and fix the options
			<BEFORE>
			 set FC = /usr/pgi/linux86/bin/pgf90
			 set FP = $FC
			
			 set FSTD       = "-Mfixed -Mextend"
			 set F_FLAGS    = "${FSTD} -O2 -I."
			 set CPP_FLAGS  = ""
			 set C_FLAGS    = "-v -O2 -I."
			 set LINK_FLAGS = "-Bstatic"

			#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#
			
			 set LIB1 = "-L${M3LIB}/ioapi_22/${BLD_OS}2_x86pg -lioapi"
			 set LIB2 = "-L${M3LIB}/netCDF/${BLD_OS} -lnetcdf"
			 set LIBS = "$LIB1 $LIB2"

			<AFTER>
			 set FC = /opt/ibmcmp/xlf/8.1/bin/xlf
			 set FP = $FC
			
			 set FSTD       = "-qfixed=132"
			 set F_FLAGS    = "${FSTD} -O2 -I."
			 set CPP_FLAGS  = ""
			 set C_FLAGS    = "-v -O2 -I."
			 set LINK_FLAGS = ""

			#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#:#
			
			 set LIB1 = "-L${M3LIB}/ioapi_22/${BLD_OS} -lioapi"
			 set LIB2 = "-L${M3LIB}/netCDF/${BLD_OS} -lnetcdf"
			 set LIBS = "$LIB1 $LIB2"

		3) make sure the netcdf and ioapi libraries exist at...
			${M3LIB}/ioapi_22/Darwin/libioapi.a
			${M3LIB}/netCDF/Darwin/libnetcdf.a

		4) bldit.jproc should run error free. if it doesn't and you find
		   link errors like this... 
			/usr/bin/ld: Undefined symbols:
			_m3exit
			_envint
			...etc
		   then recompile libioapi and/or libnetcdf with xlf.
	<END DARWIN/XLF SPECIFIC>

 
12) Check the JPROC log file to ensure complete and correct execution.
    Then cd to $WORK/icon and follow the same procedure;

	<BEGIN DARWIN/XLF SPECIFIC>
		in bldit.icon.pgf...
		1) comment out linux test
		2) make sure this line is commented out for serial processing
		   #cp -p ${STENEX}/se_*.mod $MODLOC
		3) make sure that...
			FC = /opt/ibmcmp/xlf/8.1/bin/xlf
			FSTD = "-qfixed=132"
			F_FLAGS = "${FSTD} -O2 -I${MODLOC} -qmoddir=${MODLOC}"
			LINK_FLAGS = ""
			LIB1 = "-L${M3LIB}/ioapi_22/${BLD_OS} -lioapi"
			LIB3 = "-L${STENEX} -lsef90_noop"

		4) also, make sure that in lib/ioapi_22/Darwin/, there exists either a symlink to or
		   a copy of the actual fixed_src directory that can be found in the ioapi source tree.

		5) now change the preprocessor flags to work with xlf
			<BEFORE>
			 set STX = ( -DF90\
			             -DSUBST_MODULES=${Popt}_MODULES\
			             -DSUBST_DATA_COPY=${Popt}_DATA_COPY\
			             -DSUBST_BARRIER=${Popt}_BARRIER\
			             -DSUBST_SUBGRID_INDEX=${Popt}_SUBGRID_INDEX )
			<AFTER>
			 set STX = ( -WF,-DF90\
			             -WF,-DSUBST_MODULES=${Popt}_MODULES\
			             -WF,-DSUBST_DATA_COPY=${Popt}_DATA_COPY\
			             -WF,-DSUBST_BARRIER=${Popt}_BARRIER\
			             -WF,-DSUBST_SUBGRID_INDEX=${Popt}_SUBGRID_INDEX )
	<END DARWIN/XLF SPECIFIC>

    invoke "bldit.icon.pgf", followed by "run.icon >&! icon.log &". This will
    produce the first (profile) dataset for the first run of CCTM on the coarse
    domain. After CCTM finishes, you will need to generate a nest dataset for
    the fine domain. See the TUTORIAL_PROCEDURE readme file for details.

13) Follow this procedure for each of the model subdirectories after icon/
    (the order is not mandatory). If you are running through the tutorial,
    see the TUTORIAL_PROCEDURE readme file.

	<BEGIN DARWIN/XLF SPECIFIC>
		
		For CCTM, add the "-WF," to all "-D" defines.
		While jproc, icon and bcon have short runtimes, optimizing beyond -O2 is necessary
		for CCTM. We have tested optimization parameters and suggest -O5 -qstrict or
		-O3 -qstrict -qtune=auto -qarch=auto -qcache=auto for optimal performance on G4/G5 
		processors. Note that using xlf_r for dual-processor parallelization is not faster, 
		because the division of labor is serialized into the transport and chemistry steps.

		 set F_FLAGS    = "${FSTD} -O5 -qstrict -g -qmoddir ${MODLOC} -I${MODLOC}"
 		 set CPP_FLAGS  = ""
 		 set C_FLAGS    = "-v -g -O2 -I${Mpich}/include"
 		 set LINK_FLAGS = "-O5"
	<END DARWIN/XLF SPECIFIC>
	
14) Finishing with CCTM, you should have a complete collection of datasets,
    which you can compare with the distribution datasets in $M3DATA_REF.tar.
    Unless you modify the run scripts, the output data from all the models
    will reside in the following (automatically generated) paths:
       $M3DATA/
              jproc/
              icon/
              bcon/
              cctm/

15) Concerning parallel CCTM operation: We have tested the "bldit" script for
    both serial and parallel compilation. The source code is the same for both.
    Only some libraries are different as well as the run scripts. The "stenex"
    library for parallel is different than for serial; "pario" and "dynmem"
    are needed only for parallel. We ran successfully on a Scyld Beowulf
    Linux cluster, but this release was set up and tested for a "standard"
    MPICH linux cluster, requiring the addition of a C code that distributes
    the run time environment from the node that launches the run to the other
    participating nodes. Thanks to Bo Wang and Zion Wang of CERT-UC-Riverside,
    who developed and tested this code. Also, see the PARALLEL_NOTES readme.
    (Note: The initial concentrations pre-processor, ICON can also be executed
    in parallel, but we have not yet tested this for Linux clusters.)

16) Concerning non-parallel CCTM operation (to run the model in serial):

    In bldit.cctm.linux, deselect ParOpt (comment it out)

    Modify the run.cctm script as:
      9c9
      < # method: run.cctm >&! cctm_e2a.log &
      ---
      > # method: run.cctm >&! cctm_e1a.log &
      19,20c19,20
      <  set APPL     = e2a
      <  set CFG      = e2a
      ---
      >  set APPL     = e1a
      >  set CFG      = e1a
      24,27c24,27
      < #setenv NPCOL_NPROW "1 1"
      < #set NPROCS   = 1
      <  setenv NPCOL_NPROW "5 2"
      <  set NPROCS   = 10
      ---
      >  setenv NPCOL_NPROW "1 1"
      >  set NPROCS   = 1
      > #setenv NPCOL_NPROW "5 2"
      > #set NPROCS   = 10
      206c206
      < #time  $BASE/$EXEC
      ---
      >  time  $BASE/$EXEC
      208,211c208,211
      <  set MPIRUN = /share/linux/bin/mpich-ch_p4/bin/mpirun
      <  set TASKMAP = $BASE/machines10
      <  cat $TASKMAP
      <  time $MPIRUN -v -machinefile $TASKMAP -np $NPROCS $BASE/$EXEC
      ---
      > #set MPIRUN = /share/linux/bin/mpich-ch_p4/bin/mpirun
      > #set TASKMAP = $BASE/machines10
      > #cat $TASKMAP
      > #time $MPIRUN -v -machinefile $TASKMAP -np $NPROCS $BASE/$EXEC

    Note: You can change the default script by using the Unix "patch" utility.
          Cut the indented section listed above into a file, say "mod." Then
          type "patch run.cctm mod."

17) Other details:

 a. You can check output ioapi file headers (and data) using ncdump. This
    utility will be located in the same place as netcdf, mentioned in (4) above.

 b. You can run a model using the required inputs from the reference datasets,
    instead of your preprocessor outputs. You might want to do this to run just
    the CCTM, compiled with a different set of module options, for example.

 c. To run CCTM, ICON and BCON requires a GRIDDESC file that contains horizontal
    projection and grid domain definitions. The run scripts contain environment
    variables that point to this file that contains the user's horizontal grid
    definitions. The horizontal grid definition can be set to window from the
    met and emissions input files. However, the window must be a proper subset
    - not lie along any of the boundaries of the domain represented by the
    input files.
    Note: The domains represented by the met and emissions data must be the
    same.  Of course, you don't have to window, and the domain represented by
    the input files is a valid CCTM domain.
 
 d. Running CCTM for a windowed domain or a higher resolution nested domain
    from larger or coarser met and emissions datasets still requires creating
    initial and boundary data for the target domain using ICON and BCON.

