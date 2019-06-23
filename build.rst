
my notes on building cmaq.

download was for Linux Intel.

Plan is to build on lrc, using intel SMF


use scripts/ 
where build steps are listed.

Ling said to work in : /global/home/groups-sw/pc_adjoint/Tin_Ho

start with doc/README.txt

export M3HOME=/global/home/groups-sw/pc_adjoint/Tin_Ho/CMAS4.5.1/rel
mkdir -p $M3HOME

export M3MODEL=$M3HOME/models
export M3LIB=$M3HOME/lib 
mkdir $M3LIB
export M3DATA=$M3HOME/data

export M3DATA_TGZ=???     ## where to get this??  use
/global/home/groups-sw/pc_adjoint/CMAQ4.5/data ??
export M3DATA_TGZ=

cd $M3HOME

tar 





also need to see IOAPI.txt and the CVS_NETCDF
