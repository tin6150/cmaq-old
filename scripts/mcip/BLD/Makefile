.SUFFIXES:
.SUFFIXES: .o .F .f

MODEL = mcip.exe
NETCDF = $(M3LIB)/netCDF

#...IBM
#FC     = /usr/bin/xlf90
#FFLAGS = -qfree -O3 -qstrict -bmaxstack:512000000 -bmaxdata:2000000000 -I$(NETCDF)/include
#FFLAGS = -qfree -O0 -g -qcheck -qextchk -qflttrap=ov:und:zero:inv:imp:en -qsigtrap -qinitauto=FF911299 -bmaxstack:512000000 -bmaxdata:2000000000 -I$(NETCDF)/include
#LIBS   = -L/work/wdx/tools/ioapi_3/AIX -lioapi \
#	 -L$(NETCDF)/lib -lnetcdf
#
#...Linux (PGF90)
FC     =  /afs/isis/pkg/pgi-52/linux86/5.2/bin/pgf90
#FFLAGS = -g -O0 -Ktrap=unf -Ktrap=denorm -Ktrap=inv -Ktrap=ovf -Ktrap=divz -Ktrap=fp -pc 32 -tp k8-32 -Mbounds -Mchkfpstk -Mchkptr -Kieee -Minform,inform -Mfree -byteswapio -I/share/linux/netcdf_3.5/include
FFLAGS = -O4 -fastsse -pc 32 -Mfree -byteswapio -I/afs/isis.unc.edu/depts/cep/emc/apps/netcdf/netcdf-3.6.0-p1/Linux2_pgf90/include
LIBS   = -L$(M3LIB)/ioapi_3/Linux2_x86pg -lioapi \
	 -L$(NETCDF)/Linux -lnetcdf
#
#...Linux (Intel)
#FC     = /share/linux/intel8.0/bin/ifort
#FFLAGS = -g -O0 -check all -C -pc32 -fpstkchk -traceback -FR -I/share/linux/netcdf_3.5/include
#FFLAGS = -O3 -fast -pc32 -FR -I/share/linux/netcdf_3.5/include
#LIBS   = -L/share/linux/ioapi30_ifc/lib -lioapi \
#         -L/share/linux/netcdf_3.5/lib -lnetcdf
#
#...Linux (Intel 64)
#FC	= /opt/intel/fce/9.0/bin/ifort
#FFLAGS	= -g -O0 -check all -C -pc32 -fpstkchk -traceback -FR -I/usr/local/include
#FFLAGS	= -O3 -FR -I/usr/local/include
#LIBS	= -L/opt/ioapi_3.0/Linux2_x86_64ifort -lioapi \
#	  -L/usr/local/lib -lnetcdf


DEFS   = 


MODULES =\
 const_mod.o \
 const_mete_mod.o \
 const_pbl_mod.o \
 coord_mod.o \
 date_pack_mod.o \
 date_time_mod.o \
 depvvars_mod.o \
 parms3_mod.o \
 fdesc3_mod.o \
 file_mod.o \
 iodecl3_mod.o \
 lradmdat_mod.o \
 groutcom_mod.o \
 mcipparm_mod.o \
 mcoutcom_mod.o \
 mdoutcom_mod.o \
 metinfo_mod.o \
 metvars_mod.o \
 vgrd_mod.o \
 wrf_netcdf_mod.o \
 xvars_mod.o
 
OBJS =\
 mcip.o \
 alloc_ctm.o \
 alloc_depv.o \
 alloc_met.o \
 alloc_x.o \
 bcldprc_ak.o \
 close_files.o \
 comheader.o \
 collapx.o \
 crs2dot.o \
 cvmgp.o \
 cvmgz.o \
 dealloc_ctm.o \
 dealloc_depv.o \
 dealloc_met.o \
 dealloc_x.o \
 dynflds.o \
 getgist.o \
 getluse.o \
 getmet.o \
 getpblht.o \
 getsdt.o \
 getversion.o \
 graceful_stop.o \
 gridgeometry.o \
 gridout.o \
 hlconst.o \
 init_io.o \
 init_depv.o \
 init_met.o \
 init_x.o \
 julian.o \
 layht.o \
 locate.o \
 m3dry.o \
 mdate2hdate.o \
 metcro.o \
 metdot.o \
 metgrid2ctm.o \
 metvars2ctm.o \
 pblpkg.o \
 pblpwr.o \
 pblsup.o \
 radmdry.o \
 rdmm5.o \
 rdmm5v2.o \
 rdmm5v3.o \
 rdwrfem.o \
 readnml.o \
 readter.o \
 refstate.o \
 resistcalc.o \
 setgriddefs.o \
 setup.o \
 setup_mm5v2.o \
 setup_mm5v3.o \
 setup_wrfem.o \
 sfcflux.o \
 sfclayer.o \
 slflux.o \
 solar.o \
 statflds.o \
 transm.o \
 vertarys.o \
 verthyd.o \
 vertnhy.o \
 vertnhy_wrf.o \
 vstamp.o \
 wind.o \
 wrgdesc.o \
 xxyy.o

all:
	@$(MAKE) $(MODULES)
	@$(MAKE) $(MODEL)

$(MODEL): $(OBJS)
	$(FC) -o $(MODEL) $(FFLAGS) $(OBJS) $(MODULES) $(LIBS)

$(OBJS): $(MODULES)

.f.o:
	$(FC) $(FFLAGS) $(INCLUDES) -c $<

.F.o:
	$(FC) $(FFLAGS) $(INCLUDES) -c $< $(DEFS)

clean:
	rm -f *.o *.mod *.il $(MODEL)
