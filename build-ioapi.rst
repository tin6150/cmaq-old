# vim: tabstop=4 noexpandtab

my notes on building ioapi, a lib needed by cmaq

2019.0704

**>> refer to doc/IOAPI.txt <<**


PLAN
----

download source, checkin to cmaq git repo
make
copy resulting binary to rel[ease] dir in /global/home/groups-sw/pc_adjoint/Tin_Ho/CMAS4.5.1/rel 

cmaq 4.5.1 said to use obsolete 3.0 ver [ca 2007.06]
https://www.cmascenter.org/ioapi/documentation/all_versions/html/AVAIL.html#v30

ioapi doc says ver 0.9 to 3.1 is upward compatible by concious effort.
so, using that (3.2 has a few compatibility problem).
https://www.cmascenter.org/ioapi/documentation/all_versions/html/AVAIL.html#v31



ISSUE
-----

1.
https://www.cmascenter.org/ioapi/documentation/all_versions/html/AVAIL.html#ncf4
says netCDF4 removed `CALL NC*()` 
ioapi 3.2 was re-coded to use `IERR=NF_*()`.  Since I am using SMF netCDF 4.6.1, i might run into issue.
CMAQ 4.5.1 says to use IOAPI 3.0.  Not sure if that old code will work with 3.2...

2.
linux memory model.
module show netcdf/4.4.1.1-intel-p says nothing of memory model.  so maybe was using default.
this means limit to 2GB of RAM.
If future has memory problem... 
Try `-mcmodel=large` with intel first.  if fails, use `-mcmodel=medium`


LOG
---

**^ tin viz.scs00 ~/gs/tin-gh/cmaq/ioapi ^**>  
vi Makefile
vi ioapi/Makefiile
vi m3tools/Makefile



