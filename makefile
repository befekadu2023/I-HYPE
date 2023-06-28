# Call options: comp=ifort or gfortran config=debug or release
comp_ifort = ifort
comp_gfortran = gfortran
config_debug = debug
config_release = release

# Set compiler and switches; gfortran release is default
f90comp = $(comp_$(comp))
f90config = $(config_$(config))
ifeq ($(f90comp),)
	f90comp = $(comp_gfortran)
endif
ifeq ($(f90comp),$(comp_gfortran))
	switch = -O2 -fbacktrace -ffree-line-length-0 -Wall -fno-automatic -cpp
	ifeq ($(f90config),$(config_debug))
		switch = -O0 -fbacktrace -ffree-line-length-0 -Wall -fno-automatic -cpp
	endif
endif
ifeq ($(f90comp),$(comp_ifort))
	switch = -auto-scalar -heap-arrays 1024 -fpp
	ifeq ($(f90config),$(config_debug))
		switch = -warn unused -warn uncalled -warn interfaces -check bounds -traceback -O0 -auto-scalar -heap-arrays 1024 -fpp
	endif
endif

objects = gear_GlobVARs.o t_dgls.o uawp.o fgauss.o gear.o libdate.o hypevar.o modvar.o worvar.o general_wc.o general_func.o convert.o time.o compout.o hypetypes.o t_proc.o readwrite.o hype_indata.o hype_tests.o atm_proc.o irrigation.o hype_wbout.o npc_soil_proc.o soil_proc.o regional_groundwater.o sw_proc.o npc_sw_proc.o soilmodel0.o glacier_soilmodel.o soilmodel4.o model_hype.o data.o statedata.o optim.o main.o
modfiles = gear_GlobVARs.mod t_dgls.mod uawp.mod fgauss.mod gear_implicit.mod libdate.mod hypevariables.mod modvar.mod worldvar.mod general_water_concentration.mod general_functions.mod convert.mod timeroutines.mod compout.mod statetype_module.mod tracer_processes.mod readwrite_routines.mod hype_indata.mod hype_test_routines.mod atmospheric_processes.mod irrigation_module.mod hype_waterbalance.mod npc_soil_processes.mod soil_processes.mod regional_groundwater_module.mod surfacewater_processes.mod npc_surfacewater_processes.mod soilmodel_default.mod glacier_soilmodel.mod floodplain_soilmodel.mod modelmodule.mod datamodule.mod state_datamodule.mod optimization.mod

# Makefile
hype:	$(objects)
	$(f90comp) -o hype $(switch) $(objects)

# All .o files are made from corresponding .f90 files
%.o:	%.f90
	$(f90comp) -c $(switch) $<
%.o:	%.F90
	$(f90comp) -c $(switch) $<
%.mod:	%.f90
	$(f90comp) -c $(switch) $<
%.mod:	%.F90
	$(f90comp) -c $(switch) $<

# Dependencies
modvar.o       : libdate.mod
convert.o      : libdate.mod
t_dgls.o       : gear_GlobVARs.mod modvar.mod hypevariables.mod
uawp.o         : gear_GlobVARs.mod t_dgls.mod
gear.o         : gear_GlobVARs.mod uawp.mod t_dgls.mod fgauss.mod modvar.mod hypevariables.mod
atm_proc.o     : hypevariables.mod modvar.mod hype_indata.mod
irrigation.o   : hypevariables.mod modvar.mod statetype_module.mod
t_proc.o       : hypevariables.mod modvar.mod statetype_module.mod general_water_concentration.mod general_functions.mod
npc_soil_proc.o: hypevariables.mod modvar.mod statetype_module.mod general_water_concentration.mod general_functions.mod
soil_proc.o    : gear_implicit.mod uawp.mod t_dgls.mod fgauss.mod hypevariables.mod modvar.mod statetype_module.mod general_water_concentration.mod general_functions.mod npc_soil_processes.mod atmospheric_processes.mod hype_indata.mod
npc_sw_proc.o  : hypevariables.mod modvar.mod statetype_module.mod general_water_concentration.mod general_functions.mod
sw_proc.o      : hypevariables.mod modvar.mod statetype_module.mod general_water_concentration.mod general_functions.mod soil_processes.mod
soilmodel0.o   : hypevariables.mod modvar.mod statetype_module.mod general_water_concentration.mod general_functions.mod npc_soil_processes.mod soil_processes.mod atmospheric_processes.mod regional_groundwater_module.mod irrigation_module.mod tracer_processes.mod
glacier_soilmodel.o : hypevariables.mod modvar.mod statetype_module.mod general_water_concentration.mod npc_soil_processes.mod soil_processes.mod atmospheric_processes.mod regional_groundwater_module.mod tracer_processes.mod
soilmodel4.o   : hypevariables.mod modvar.mod statetype_module.mod general_water_concentration.mod general_functions.mod npc_soil_processes.mod soil_processes.mod atmospheric_processes.mod regional_groundwater_module.mod irrigation_module.mod tracer_processes.mod
regional_groundwater.o : hypevariables.mod modvar.mod statetype_module.mod general_water_concentration.mod npc_soil_processes.mod 
model_hype.o   : libdate.mod hypevariables.mod modvar.mod statetype_module.mod general_water_concentration.mod glacier_soilmodel.mod soilmodel_default.mod soil_processes.mod npc_soil_processes.mod surfacewater_processes.mod npc_surfacewater_processes.mod irrigation_module.mod regional_groundwater_module.mod hype_waterbalance.mod hype_indata.mod atmospheric_processes.mod tracer_processes.mod
compout.o      : libdate.mod modvar.mod worldvar.mod timeroutines.mod convert.mod
worldvar.o     : libdate.mod modvar.mod
readwrite.o    : libdate.mod worldvar.mod convert.mod compout.mod
hype_wbout.o   : libdate.mod modvar.mod worldvar.mod readwrite_routines.mod
hype_indata.o  : libdate.mod modvar.mod worldvar.mod readwrite_routines.mod
hype_tests.o   : libdate.mod hypevariables.mod modvar.mod worldvar.mod readwrite_routines.mod
timeroutines.o : libdate.mod worldvar.mod modvar.mod
data.o         : libdate.mod modvar.mod worldvar.mod convert.mod timeroutines.mod readwrite_routines.mod compout.mod modelmodule.mod 
optim.o        : libdate.mod modvar.mod worldvar.mod statetype_module.mod timeroutines.mod modelmodule.mod compout.mod datamodule.mod state_datamodule.mod
statedata.o    : libdate.mod modvar.mod worldvar.mod statetype_module.mod modelmodule.mod readwrite_routines.mod
main.o         : libdate.mod modvar.mod worldvar.mod statetype_module.mod timeroutines.mod readwrite_routines.mod hype_test_routines.mod modelmodule.mod compout.mod hype_test_routines.mod datamodule.mod optimization.mod state_datamodule.mod


.PHONY : clean
clean:	
	rm -f $(objects)
	rm -f $(modfiles)
