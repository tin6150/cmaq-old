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


setup_cmaq() {
	echo tbd
}


main() {
	env_prep
	#setup_ioapi
	setup_m3tools
	#setup_cmaq

}


# start of script
main
