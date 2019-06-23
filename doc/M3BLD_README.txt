                  M3BLD_README  September, 2005

These are the options for the "Opt" variable used for controlling
m3bld in the CMAQ build scripts:

set Opt = compile_all  # force compile, even if object files are current
set Opt = clean_up     # remove all source files upon successful completion
set Opt = no_compile   # do everything except compile
set Opt = no_link      # do everything except link
set Opt = one_step     # compile and link in one step
set Opt = parse_only   # checks config file syntax
set Opt = show_only    # show requested commands but doesn't execute them
set Opt = verbose      # show requested commands as they are executed
set MakeOpt            # builds a Makefile to make the model, uncomment to invoke 
