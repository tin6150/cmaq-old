
*****************************************************************************
** this repo is being renamed to cmaq-old                                ****
** it contained an old version of cmaq before they used git as the repo  ****
** but the new git-based repo is also called cmaq, thus renaming this    ****
*****************************************************************************


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
  	

* Singularity: 2019.0721 : Trying to use a singularity container with PGI, Ryan already seeded with those libs.
  will start with interactive command rather than write .def cuz cmaq may need lot of interactions.
  commands (mostly) jogged into the pgi_netcdf.def, but may have missing steps.
  bofh now has singularity via SMFdev.
  cmaq.img is writable, with rebuild hdf5, libnetcdff compiled by pgi pgf95. [2019.0726, but ioapi isn't compiling]
  now rebuild ioapi...  see  ioapi/README.txt.rst

  using a new version of PGI_netcdf.def file, at least c1576fc
  cmaq.img is writable, with rebuild hdf5, libnetcdff compiled by pgi pgf90. [2019.0728]

* setup.sh to have scripted compilation for ioapi, m3tools. 2019.0730.
  focus is singularity container with PGI compiler, pgf90.  using cmaq.b0728c.img
  cmaq coming next, per instruction in doc/README.txt.rst
  edits will be checked into git


Singularity
-----------

:: 

	sudo /opt/singularity-2.5.2/bin/singularity build --writable cmaq_b072xx.img cmaq.def 2>&1  | tee singularity_build.log
	#tmp bulid stopped after erro.  even --notest doesn't produce img file for use from where build borked :(
	# 28b and 28c end up being the same, cuz forgot to remove the `exit 0`

	module load tools/singularity/2.5.2 # SMFdev for bofh
	singularity shell --writable cmaq.img 
	# ++ need to start interactive container so content are saved, not ephemeral... 
	# home dir is mapped, so source code for ioapi already avail in my use
	# .def file would need to do more things...
	# see the various rst files for commands.
	sudo ./singularity shell --writable -B /home/tin cmaq.img 
	# then `su  tin` as necessary (eg to compile cmaq)  ## dont use su - or prompt loose idea of inside singularity
	
	# if persistent overlay is avail (2.6 has), 
	# but has to deal with an extra .img file.  maybe after cmaq is build and and to preserve from future changes...
	# source (first) .img didn't need to be created as --writable if using persistent overlay
	# singularity image.create cmaq-overlay.img
	# sudo singularity shell --overlay cmaq-overlay.img shub_pgi_netcdf.img


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

mpi wrapper is ??


