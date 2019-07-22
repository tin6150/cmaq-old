cmaq
====
personal notes on using cmaq, contain cached version of the source code (old version(s)).  Hopefully no edits needed.

original source is from
https://www.cmascenter.org/help/documentation.cfm?MODEL=cmaq&VERSION=4.5



branch 4.5.1 
============

will likely work mainly from this branch (2019.06)
as getting source cached in here and be portable
Did not yet create a branch 4.7.1, as was just poking that tar file to get a very few specific file.


Build plan
==========

* Need to have a single compiler with NetCDF and ioapi, etc
  If using SMF Intel compiler, seems to need to recompile NetCDF.

* Singularity: 2019.TBA: Trying to use a singularity container with PGI, Ryan already seeded with those libs.
  will start with interactive command rather than write .def cuz cmaq may need lot of interactions.
  jog down the commands that may eventually lead to a .def...
  without beagle or singularity-hub, no easy way to build the container.
  get bofh to work with singulairty... 


Singularity
-----------

:: 

    sudo /opt/singularity-2.5.2/bin/singularity build cmaq_b0721a.img pgi_netcdf.def
    sudo /opt/singularity-2.5.2/bin/singularity shell cmaq.img 
    ./singularity shell cmaq.img 
	# home dir is mapped, so source code for ioapi already avail in my use
	# .def file would need to do more things...
	# see the various rst files for commands.
	


TMP
~~~

pg* bin, 

pgaccelerror      pgcc18            pgdbg             pgf90-llvm_ex     pgi_license_tool  pgrep
pgaccelinfo       pgcc-llvm         pgdebug           pgf95             pgimport          pgroupd
pgacclnk          pgc-llvm          pgexplain.xml     pgfortran         pgipa             pgserv
pgappend          pgc++llvm         pgextract         pgfortran-llvm    pgnvd             pgsize
pgawk             pgc-llvm_ex       pgf90             pggpp1-llvm       pgnvvm            pgsmart
pgc++             pgcollect         pgf901-llvm       pggpp2ex-llvm     pgobjinfo         pgsupport
pgcc              pgcpuid           pgf902-llvm       pggpp2-llvm       pgprepro          pgunzip
pgcc11            pgcudainit        pgf90-llvm        pgicg             pgprof            pgzip



