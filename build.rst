
my notes on building cmaq.

download was for Linux Intel.

Plan is to build on lrc, using intel SMF


** just cuz don't have cvs... and singularity does not bind path for /global/home/groups-sw
** have to move this to my own scratch dir.

*** intel ifort, openmpi mpifort (wrapper)
*** don't work well.  many complains during compilation
*** compile script is really pgi.  
*** try to see if there are other download, maybe gcc??


use scripts/ 
where build steps are listed.

Ling said to work in : /global/home/groups-sw/pc_adjoint/Tin_Ho


**refer to doc/README.txt [.rst for annotated version]**

export M3HOME=/global/home/groups-sw/pc_adjoint/Tin_Ho/CMAS4.5.1/rel
mkdir -p $M3HOME

export M3MODEL=$M3HOME/models
export M3LIB=$M3HOME/lib 
mkdir  $M3LIB
export M3DATA=$M3HOME/data

# export M3DATA_TGZ=???     ## in "distribution pkg", but only found it under CMAQ 4.7.1
# /global/home/groups-sw/pc_adjoint/CMAQ4.5/data # not sure what Ling has in there
export M3DATA_TGZ=~/gs/Downloads/CMAQ/DATA.CMAQv4.7.1.tar.gz

**Step 4**

cd $M3HOME    				# /global/home/groups-sw/pc_adjoint/Tin_Ho/CMAS4.5.1/rel
tar xfz $M3DATA_TGZ

          mkdir $M3LIB/
          mkdir $M3LIB/build/
          mkdir $M3LIB/ioapi_3/
          mkdir $M3LIB/netCDF/
          mkdir $M3LIB/pario/
          mkdir $M3LIB/stenex/


mkdir $M3LIB/netCDF/Linux
cd    $M3LIB/netCDF/Linux
ln -s /global/software/sl-7.x86_64/modules/intel/2018.1.163/netcdf/4.6.1-intel-p/lib/libnetcdf.a .
			# started with parallel version, but not sure if it would work... ~~

# module load netcdf/4.6.1-intel-p

module load netcdf/4.6.1-intel-p

					module av
					-s = serial version
					-p = parallel version
													    netcdf/4.4.1.1-intel-p
					hdf5/1.8.18-intel-p                 netcdf/4.4.1.1-intel-s
					hdf5/1.8.18-intel-s                 netcdf/4.6.1-intel-p
					hdf5/1.8.20-intel-p                 netcdf/4.6.1-intel-s
					hdf5/1.8.20-intel-s                 

		**>>** details on ioapi not done yet
		       may need this now, cuz stuck at step 7

**Step 5**

cd $M3HOME  # /global/home/groups-sw/pc_adjoint/Tin_Ho/CMAS4.5.1/rel
tar xfz ~/gs/Downloads/CMAQ/M3MODELS.CMAQv4.5.1.tar.gz


**Step 6**
tar xf ~tin/gs/Downloads/CMAQ/M3SCRIPTS.CMAQv4.5.1.tar.gz 

export M3HOME=/global/home/groups-sw/pc_adjoint/Tin_Ho/CMAS4.5.1/rel
export WORK=$M3HOME/scripts     # ie /global/home/groups-sw/pc_adjoint/Tin_Ho/CMAS4.5.1/rel/scripts


**Step 7**

mkdir $M3LIB/ioapi_3 						####### install here

cd $WORK/stenex 					# /global/home/groups-sw/pc_adjoint/Tin_Ho/CMAS4.5.1/rel/scripts/stenex

	cp -p bldit.se_noop.pgf bldit.se_noop.pgf.orig
  cp -p bldit.se.pgf bldit.se.pgf.orig


		vi bldit.se.pgf
		# change lines 46
		## set FC = /global/software/sl-7.x86_64/modules/langs/intel/2018.1.163/bin/fpp ## this is Fortran PreProcessor
		##set FC = /global/software/sl-7.x86_64/modules/langs/intel/2018.1.163/bin/ifort
		set FC = /global/software/sl-7.x86_64/modules/intel/2018.1.163/openmpi/2.0.2-intel/bin/mpifort
					# hint from https://proteusmaster.urcf.drexel.edu/urcfwiki/index.php/Compiling_CMAQ
		set F_FLAGS = "" # since don't know what's intel equiv of pgi options 


		export M3HOME=/global/home/groups-sw/pc_adjoint/Tin_Ho/CMAS4.5.1/rel
		export M3MODEL=$M3HOME/models
		export M3LIB=$M3HOME/lib 
    # cvs via singularity container in /global/scratch/tin/singularity-repo
		# need do run things like `setenv CVSROOT $M3MODEL/STENEX`
    # end up compiling cvs from souce (which req fixing s/getline/get\ line/ kind of patch)
		. ~/.bashrc
    module load netcdf/4.6.1-intel-p   # include intel/2018.
		    1) vim/7.4                4) intel/2018.1.163       7) openmpi/3.0.1-intel
			  2) emacs/25.1             5) mkl/2018.1.163         8) hdf5/1.8.20-intel-p
				3) git/2.11.1             6) openmpi/2.0.2-intel    9) netcdf/4.6.1-intel-p
		   10) /tools/cvs/1.11.23  ## cvs added 2019.0704 (in personal SMFdev)

	  # cp -p bldit.se.pgf ~tin/gs/tin-gh/cmaq/scripts/stenex/ 
    csh bldit.se.pgf 2>&1  | tee bldit.se.pgf.log 

			problem
				/global/home/groups-sw/pc_adjoint/Tin_Ho/CMAS4.5.1/rel/scripts/stenex/BLD

						[tin@viz BLD]$ /global/software/sl-7.x86_64/modules/langs/intel/2018.1.163/bin/ifort -O2 -I/share/linux/bin/mpich-ch_p4/include se_comm_info_ext.f

						/global/software/sl-7.x86_64/modules/langs/intel/2018.1.163/compilers_and_libraries_2018.1.163/linux/compiler/lib/intel64_lin/for_main.o: In function `main':
						for_main.c:(.text+0x2a): undefined reference to `MAIN__'
						#`rst food` 

	


~~~~


export CVSROOT=/home/tin/tin-gh/cmaq/models/BUILD 
	CVS create a CVSROOT for its DB, (think .git or RCS)
	CVS is like a glorified RCS, adding directory and centralized server to store the tree

MAC_OSX_README.txt has step detail with cvs


also need to see IOAPI.txt and the CVS_NETCDF


~~~~


xref:
* https://wiki.uiowa.edu/display/hpcdocs/CMAQ

* https://blog.chenzhang.org/post/gis/cmaq-installation/   for CMAQ 5.1 or so...
