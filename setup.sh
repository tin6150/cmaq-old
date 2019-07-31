#!/bin/bash

#### compile and install cmaq and its dependencies.
#### done as function, which is really section
#### run as:
#### ./setup.sh 2>&2 | tee setup.log

env_prep() {

	#BASEPATH=/opt
	export PATH=/opt/pgi/linux86-64/19.4/bin/:$PATH

	##export compiler=pgi
    #XX export CC=/opt/pgi/linux86-64/19.4/bin/pgcc CCFLAGS="-g"   FC=/opt/pgi/linux86-64/19.4/bin/pgf95 FCFLAGS="-g"
    export CC=/opt/pgi/linux86-64/19.4/bin/pgcc CCFLAGS="-g"   FC=/opt/pgi/linux86-64/19.4/bin/pgf90 FCFLAGS="-g"

    # export SNHOME=/Downloads                      # -or-
    # export SRCBASE=/Downloads                     # -or-
    #export SRCBASE=/local/home/tin/tin-gh    # as appropriate 
	export SRCBASE=$(pwd)
	export DSTBASE=/opt/CMAS4.5.1/rel

    export LD_LIBRARY_PATH=/opt/CMAS4.5.1/rel/lib/ioapi_3:$LD_LIBRARY_PATH
    export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
    export PATH=/usr/local/bin:$PATH

    export BIN=`uname -s``uname -r | cut -d. -f1`
}

setup_ioapi() {
        #BASEDIR=$SRCBASE/cmaq/ioapi/ioapi  # source dir
        BASEDIR=${SRCBASE}/ioapi/ioapi  # source dir
        #mkdir ${BASEDIR}/Linux4   #in git repo now
		mkdir $BASEDIR/$BIN  > /dev/null 2>&1			# BASEDIR := $(pwd)
        mkdir -p /opt/CMAS4.5.1/rel/lib/ioapi_3         # install destination?
        cd $BASEDIR     #cd cmaq/ioapi/ioapi
        cp -p Makefile.pgi_container Makefile       # some edit done, now using pgf95
        #HOME=/local/home/tin/tin-gh/cmaq/ioapi  BIN=Linux4  INSTDIR=/opt/CMAS4.5.1/rel/lib/ioapi_3   make
        #echo $?
        #echo "done with make"
        #HOME=/local/home/tin/tin-gh/cmaq/ioapi  BIN=Linux4  INSTDIR=/opt/CMAS4.5.1/rel/lib/ioapi_3 make install
        make HOME=${SRCBASE}/ioapi  BIN=Linux4  INSTDIR=/opt/CMAS4.5.1/rel/lib/ioapi_3  install  2>&1 | tee make.install.log
        # only 1 file: /opt/CMAS4.5.1/rel/lib/ioapi_3/libioapi.a

        #cd $HOME/tin-gh/cmaq/ioapi/bin
        # ln -s /opt/lib/libnetcdf.a .
        # ln ... libioapi.a

        cd ${SRCBASE}
}


#### cmaq m3tools #####
setup_m3tools() {
		## hmm... refer to build.rst have steps i no longer do... 
        ##defined above## SRCBASE=/local/home/tin/tin-gh    
        #cd $SRCBASE/cmaq/ioapi/m3tools # cd $HOME/tin-gh/cmaq/ioapi/m3tools
        #cd cmaq/ioapi/m3tools # cd $HOME/tin-gh/cmaq/ioapi/m3tools
        cd ${SRCBASE}/ioapi/m3tools 
        cp -p Makefile.pgi_container Makefile
		mkdir /opt/CMAS4.5.1/rel/bin
        #make         2>&1 | tee make.log
        #make install 2>&1 | tee make.install.log
        make HOME=${SRCBASE}/ioapi/m3tools  BIN=Linux4  INSTDIR=/opt/CMAS4.5.1/rel/bin  2>&1 | tee make.log
        #make INSTDIR=/opt/CMAS4.5.1/rel/bin  install 2>&1 | tee make.install.log
        make install /opt/CMAS4.5.1/rel/bin 2>&1 | tee make.install.log
        #HOME=${SRCBASE}/ioapi/m3tools  BIN=Linux4  INSTDIR=/opt/CMAS4.5.1/rel/bin   make install  2>&1 | tee make.install.log

        cd ${SRCBASE}
        #echo "last_line_of_post" >  container_build_done
        #echo "last_line_of_post" > /container_build_done
        # singularity build get interrupted and result in no img whatsoever if there are errors with these mkdir or make commands :(
        # building docker layers first would likely save time for development cycle.

}


setup_cmaq451() {
	# read doc/README.txt.rst again (build.rst was for intel on lrc).  this is for PGI on singularity container.
	#export DSTBASE=/opt/CMAS4.5.1/rel # done by env_prep fn above

	#export M3HOME=/global/home/groups-sw/pc_adjoint/Tin_Ho/CMAS4.5.1/rel
	#export WORK=$M3HOME/scripts     # ie /global/home/groups-sw/pc_adjoint/Tin_Ho/CMAS4.5.1/rel/scripts
	export M3HOME=${DSTBASE}		 # doc eg use /project/air5/sjr/CMAS4.5.1/rel, not sure if it is part of source tree :(
	export WORK=${DSTBASE}/scripts
	export M3MODEL=${M3HOME}/models
	export M3LIB=${M3HOME}/lib
	export M3LIB=${M3HOME}/data      # may not have this yet, may need to get from 4.7.  source: 58MB, output: 291MB
	#? export CVSROOT=${M3MODEL}/STENEX
	#export CVSROOT=/home/tin/tin-gh/cmaq/models/BUILD


	#export FC=mpifort   ## exist for pgi??
	export FC=mpirun_dbg.pgdbg  ## /opt/pgi/linux86-64-llvm/19.4/bin/mpirun_dbg.pgdbg 
	export F_FLAGS="" ## since don't know what's intel equiv of pgi options

	[[ -d $M3LIB         ]] || mkdir $M3LIB/
	[[ -d $M3LIB/build   ]] || mkdir $M3LIB/build/
	[[ -d $M3LIB/ioapi_3 ]] || mkdir $M3LIB/ioapi_3/
	[[ -d $M3LIB/netCDF  ]] || mkdir $M3LIB/netCDF/
	[[ -d $M3LIB/pario   ]] || mkdir $M3LIB/pario/
	[[ -d $M3LIB/stenex  ]] || mkdir $M3LIB/stenex/

	##### doc/README.txt.rst step 3 ####
	cd ${M3HOME} # same as ${DSTBASE}  eg /opt/CMAS4.5.1/rel
	wget 'https://drive.google.com/uc?export=download&id=0B4Gx-y00i4D0TWF4SHNtc29vZ2c' -O  m3data.tgz  # create data/ dir 
	tar xfz m3data.tgz

	##### doc/README.txt.rst step 5 ####
	cd ${M3HOME} # same as ${DSTBASE}  eg /opt/CMAS4.5.1/rel
	??get M3MODELS.CMAQv4.5.1.tar.gz
	tar xfz M3MODELS.CMAQv4.5.1.tar.gz # create models/ dir

	# looking at cmaq 4.7.1 download page (cached to cmaq.4.7.1.download.rst)
	# don't seems to find m3models tgz there either...

	# pfff... really think should start with current version of cmaq, which is well documented
	# learn, then only when needed, come back to this old version.
	# see https://www.epa.gov/cmaq/cmaq-models-0

	#cd ${WORK}/stenex # formerly # /global/home/groups-sw/pc_adjoint/Tin_Ho/CMAS4.5.1/rel/scripts/stenex
	cd ${M3LIB}/stenex # compile in source code tree for now, hopefully not a bad idea... ++
	csh  bldit.se.pgf 2>&1 |  tee bldit.se.pgf.log

}



main() {
	env_prep
	#setup_ioapi
	#setup_m3tools
	setup_cmaq451

}


# start of script
main
