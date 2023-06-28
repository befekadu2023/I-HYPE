!> \file hype_tests.f90
!> Contains module hype_test_routines.

!>Module for writing test cases to a log file
MODULE HYPE_TEST_ROUTINES

  !Copyright 2017-2018 SMHI
  !
  !This file is part of HYPE.
  !
  !HYPE is free software: you can redistribute it and/or modify it under
  !the terms of the Lesser GNU General Public License as published by
  !the Free Software Foundation, either version 3 of the License, or (at
  !your option) any later version. HYPE is distributed in the hope that
  !it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  !warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See
  !the Lesser GNU General Public License for more details. You should
  !have received a copy of the Lesser GNU General Public License along
  !with HYPE. If not, see <http://www.gnu.org/licenses/>.
  !----------------------------------------------------------------------------
  USE LIBDATE
  USE READWRITE_ROUTINES, ONLY : read_parameterline
  USE HYPEVARIABLES, ONLY : n_max_par

  IMPLICIT NONE
  
  SAVE
  PRIVATE
  
  !Parameter used as index for parameter,input and forcing
  INTEGER, PARAMETER :: p_parameter  = 1  !
  INTEGER, PARAMETER :: p_input      = 2  !
  INTEGER, PARAMETER :: p_forcing    = 3  !
  INTEGER, PARAMETER :: p_generic    = 4  !
  
  !Parameter used as index for the specific test processes
  INTEGER, PARAMETER :: t_temperature_precipition  = 1  !
  INTEGER, PARAMETER :: t_evaporation              = 2  !
  INTEGER, PARAMETER :: t_soil_water               = 3  !
  INTEGER, PARAMETER :: t_snow_routines            = 4  !
  INTEGER, PARAMETER :: t_rivers                   = 5  !
  INTEGER, PARAMETER :: t_lakes                    = 6  !
  INTEGER, PARAMETER :: t_floodplains              = 7  !
  INTEGER, PARAMETER :: t_glaciers                 = 8  !
  INTEGER, PARAMETER :: t_info_generic             = 9  !
  INTEGER, PARAMETER :: t_geodata_generic          = 10 !
  INTEGER, PARAMETER :: t_geoclass_generic         = 11 !
  INTEGER, PARAMETER :: t_generic                  = 12 !
  INTEGER, PARAMETER :: t_veg_soil_surface         = 13 !
  ! This is the last number of processes
  INTEGER, PARAMETER :: nof_test_processes         = 13

  !Parameter used as index for test case names of input and forcing tests
  INTEGER, PARAMETER :: a_tobs              = n_max_par + 1  !
  INTEGER, PARAMETER :: a_tminobs           = n_max_par + 2  !
  INTEGER, PARAMETER :: a_tmaxobs           = n_max_par + 3  !
  INTEGER, PARAMETER :: a_swobs             = n_max_par + 4  !
  INTEGER, PARAMETER :: a_rhobs             = n_max_par + 5  !
  INTEGER, PARAMETER :: a_uobs              = n_max_par + 6  !
  INTEGER, PARAMETER :: a_sfobs             = n_max_par + 7  !
  INTEGER, PARAMETER :: a_elevation         = n_max_par + 8  !
  INTEGER, PARAMETER :: a_latitude          = n_max_par + 9  !
  INTEGER, PARAMETER :: a_area              = n_max_par + 10 !
  INTEGER, PARAMETER :: a_ldfastlake        = n_max_par + 11 !
  INTEGER, PARAMETER :: a_grwtolake         = n_max_par + 12 !
  INTEGER, PARAMETER :: a_modeloptionrange  = n_max_par + 13 !
  INTEGER, PARAMETER :: a_snalbrange        = n_max_par + 14 !
  INTEGER, PARAMETER :: a_sumwcfc           = n_max_par + 15 !
  INTEGER, PARAMETER :: a_sumwcwp           = n_max_par + 16 !
  INTEGER, PARAMETER :: a_sumwcep           = n_max_par + 17 !
  INTEGER, PARAMETER :: a_AquiferData       = n_max_par + 18 !
  INTEGER, PARAMETER :: a_CropData          = n_max_par + 19 !
  INTEGER, PARAMETER :: a_FloodData         = n_max_par + 20 !
  INTEGER, PARAMETER :: a_i_t2_lt_zero      = n_max_par + 21 !
  INTEGER, PARAMETER :: a_sum_macratesrrate = n_max_par + 22 !
  INTEGER, PARAMETER :: a_latitude_defined  = n_max_par + 23 !
  INTEGER, PARAMETER :: a_gddsow            = n_max_par + 24 !
  INTEGER, PARAMETER :: a_daylenth          = n_max_par + 25 !
  INTEGER, PARAMETER :: a_firstday          = n_max_par + 26 !
  INTEGER, PARAMETER :: a_percdelay         = n_max_par + 27 !
  INTEGER, PARAMETER :: a_aqarea            = n_max_par + 28 !
  INTEGER, PARAMETER :: a_porosity          = n_max_par + 29 !
  INTEGER, PARAMETER :: a_retrate           = n_max_par + 30 !
  INTEGER, PARAMETER :: a_inivol            = n_max_par + 31 !
  INTEGER, PARAMETER :: a_basedepth         = n_max_par + 32 !
  INTEGER, PARAMETER :: a_parregion2        = n_max_par + 33 !
  INTEGER, PARAMETER :: a_recievefrac       = n_max_par + 34 !
  INTEGER, PARAMETER :: a_rrcscorr          = n_max_par + 35 !
  INTEGER, PARAMETER :: a_eroindex          = n_max_par + 36 !
  INTEGER, PARAMETER :: a_slope             = n_max_par + 37 !
  INTEGER, PARAMETER :: a_noftimestepts     = n_max_par + 38 !
  INTEGER, PARAMETER :: a_bdaterange        = n_max_par + 39 !
  INTEGER, PARAMETER :: a_edaterange        = n_max_par + 40 !
  INTEGER, PARAMETER :: a_cdaterange        = n_max_par + 41 !
  INTEGER, PARAMETER :: a_timemapduplicates = n_max_par + 42 !
  INTEGER, PARAMETER :: a_basinarea         = n_max_par + 43 !
  INTEGER, PARAMETER :: a_basinslcfrac      = n_max_par + 44 !
  INTEGER, PARAMETER :: a_subidrange        = n_max_par + 45 !
  INTEGER, PARAMETER :: a_basinslope        = n_max_par + 46 !
  INTEGER, PARAMETER :: a_basinrivlen       = n_max_par + 47 !
  INTEGER, PARAMETER :: a_basinpospart      = n_max_par + 48 !
  INTEGER, PARAMETER :: a_basinpartperc     = n_max_par + 49 !
  INTEGER, PARAMETER :: a_basinodepth       = n_max_par + 50 !
  INTEGER, PARAMETER :: a_basinidepth       = n_max_par + 51 !
  INTEGER, PARAMETER :: a_slcforolake       = n_max_par + 52 !
  INTEGER, PARAMETER :: a_abstrwitholake    = n_max_par + 53 !
  INTEGER, PARAMETER :: a_linkmain          = n_max_par + 54 !
  INTEGER, PARAMETER :: a_linksecondary     = n_max_par + 55 !
  INTEGER, PARAMETER :: a_nclass            = n_max_par + 56 !
  INTEGER, PARAMETER :: a_classdataluse     = n_max_par + 57 !
  INTEGER, PARAMETER :: a_classdatasoil     = n_max_par + 58 !
  INTEGER, PARAMETER :: a_multiple_classes  = n_max_par + 59 !
  INTEGER, PARAMETER :: a_soildepth         = n_max_par + 60 !
  INTEGER, PARAMETER :: a_inc_soildepth     = n_max_par + 61 !
  ! This is the maximum modparid + extra for test of generic, input and forcing
  INTEGER, PARAMETER :: max_testing         = n_max_par + 61 !
  !This is the maximum of values that can be tested for one hydrological process
  INTEGER, PARAMETER :: max_testing_values  = 64

  !Test names for input and forcing and generic tests
  CHARACTER(LEN=32) :: testing_names(max_testing-n_max_par)
  PARAMETER(testing_names=[CHARACTER(LEN=32) :: 'tobs', 'tminobs', 'tmaxobs', 'swobs', &
      'rhobs', 'uobs', 'sfobs', 'elevation', 'latitude', 'area', 'ldfastlake', 'grwtolake', &
      'modeloptionrange', 'snalbrange', 'sumwcfc', 'sumwcwp', 'sumwcep', 'AquiferData.txt', &
      'CropData.txt', 'FloodData.txt', 'i_t2_lt_zero', 'summacratesrrate', 'lat_defined', 'gddsow', &
      'daylenth', 'firstday', 'percdelay', 'aqarea', 'porosity', 'retrate', 'inivol', &
      'basedepth', 'parregion2', 'recievefrac', 'rrcscorr', 'eroindex', 'slope', 'noftimestepts', &
      'bdaterange', 'edaterange', 'cdaterange', 'timemapduplicates', 'basinarea', 'basinslcfrac', &
      'subidrange', 'basinslope', 'basinrivlen', 'basinpospart', 'basinpartperc', 'basinodepth', &
      'basinidepth', 'slcforolake', 'abstrwitholake', 'linkmain', 'linksecondary', 'nclass', &
      'classdataluse', 'classdatasoil', 'multiple_classes', 'soildepth', 'inc_soildepth'])
  
  !Parameter used for specific tolerance tests
  INTEGER, PARAMETER :: eq_zero                      = 1   !
  INTEGER, PARAMETER :: ne_zero                      = 2   !
  INTEGER, PARAMETER :: gt_zero                      = 3   !
  INTEGER, PARAMETER :: lt_zero                      = 4   !
  INTEGER, PARAMETER :: ge_zero                      = 5   !
  INTEGER, PARAMETER :: le_zero                      = 6   !
  INTEGER, PARAMETER :: gt_zero_and_lt_one           = 7   !
  INTEGER, PARAMETER :: ge_zero_and_lt_one           = 8   !
  INTEGER, PARAMETER :: gt_zero_and_le_one           = 9   !
  INTEGER, PARAMETER :: ge_zero_and_le_one           = 10  !
  INTEGER, PARAMETER :: gt_minus_one_and_lt_plus_one = 11  !
  INTEGER, PARAMETER :: gt_minus_one                 = 12  !
  INTEGER, PARAMETER :: ge_minus_one                 = 13  !
  INTEGER, PARAMETER :: plus_minus_90                = 14  !
  INTEGER, PARAMETER :: plus_minus_180               = 15  !
  INTEGER, PARAMETER :: one_plus_minus_5percent      = 16  !
  ! This is the last number of tolerances
  INTEGER, PARAMETER :: nof_test_tolerances          = 16

  CHARACTER(LEN=32) :: testing_tolerances(nof_test_tolerances)
  PARAMETER(testing_tolerances=[CHARACTER(LEN=32) :: 'eq_zero', 'ne_zero', 'gt_zero', 'lt_zero', &
      'ge_zero', 'le_zero', 'gt_zero_and_lt_one', 'ge_zero_and_lt_one', 'gt_zero_and_le_one', 'ge_zero_and_le_one', &
      'gt_minus_one_and_lt_plus_one', 'gt_minus_one', 'ge_minus_one', 'plus_minus_90', 'plus_minus_180', &
      'one_plus_minus_5percent'])
  
  !Type declarations
  
  !brief Type for holding data about test cases with test information and error messages
  TYPE TESTCASETYPE
    LOGICAL           :: tested     = .FALSE.  !<
    LOGICAL           :: passed                !<
    INTEGER           :: kind_of_test          !<
    INTEGER           :: tolerance             !<
    INTEGER           :: status                !<
    INTEGER           :: modpar_index          !<
    CHARACTER(LEN=32) :: name                  !<
    CHARACTER(LEN=64) :: errstring             !<
  END TYPE TESTCASETYPE

  TYPE TESTPROCESSTYPE
    INTEGER           :: test_id      = -1       !<
    LOGICAL           :: test_passed  = .FALSE.  !<
  END TYPE TESTPROCESSTYPE

  TYPE(TESTCASETYPE) :: all_test_cases(max_testing)
  TYPE(TESTPROCESSTYPE), DIMENSION(max_testing_values,nof_test_processes) :: tests_c2_process
  
  INTEGER :: funit_used  = 0       ! Use stderr if not specified
  INTEGER :: printout_level = 1    ! Printout-debug-level e.g. 1=normal,2=extended,3=extended-extra
  LOGICAL :: error_halt = .TRUE.   ! Halt simulation on error TRUE/FALSE
  LOGICAL :: force_test  = .FALSE. ! Force a test regardless if tested before

  PUBLIC :: run_hype_setup_tests,run_hype_observation_tests, &
      run_hype_tests,run_hype_finalize_tests
  
  INTERFACE data_is_within_tolerance
    MODULE PROCEDURE int_data_is_within_tolerance,real_data_is_within_tolerance
  END INTERFACE
  
  INTERFACE data_differs_from_value
    MODULE PROCEDURE int_data_differs_from_value,real_data_differs_from_value
  END INTERFACE
       
  CONTAINS
    
  !>Runs setup tests
  !----------------------------------------------------------------------------
  SUBROUTINE run_hype_setup_tests(funit,plevel,force)
  
    !Argument declarations
    INTEGER, OPTIONAL, INTENT(IN) :: funit  ! Used funit
    INTEGER, OPTIONAL, INTENT(IN) :: plevel ! Used level for debug printouts
    LOGICAL, OPTIONAL, INTENT(IN) :: force  ! Testing forced, even if already tested

    !Local variables

    error_halt = .TRUE.

    IF (PRESENT(funit)) THEN
      funit_used = funit
    END IF

    IF (PRESENT(plevel)) THEN
      printout_level = plevel
      IF (printout_level > 3) THEN
        printout_level = printout_level + 1
        error_halt = .FALSE.
      END IF
      IF (printout_level > 0) THEN
        printout_level = MOD(printout_level,4)
      END IF
    END IF

    IF (PRESENT(force)) THEN
      force_test = force
    END IF
  
    RETURN

  END SUBROUTINE run_hype_setup_tests
  
  !>Runs finalize tests 
  !----------------------------------------------------------------------------
  SUBROUTINE run_hype_finalize_tests()

    !Argument declarations

    !Local variables
    
    IF ((funit_used .NE. 0) .AND. (funit_used .NE. 6)) THEN
        CLOSE(funit_used)
    END IF

    CALL clear_tests()

    RETURN

  END SUBROUTINE run_hype_finalize_tests

  !>Runs tests for hype and prints the result in specified funit
  !----------------------------------------------------------------------------
  SUBROUTINE run_hype_observation_tests(status)
  
    USE WORLDVAR, ONLY : checkdata,checkindata

    !Argument declarations
    INTEGER, INTENT(OUT)           :: status  !<error number

    !Local variables
    
    WRITE(funit_used,*) 'checkindata', checkindata

    WRITE(funit_used,*,ERR=300) '---------------- Check observations --------------'
    IF(checkdata(1,1)) CALL checkindata_part1(funit_used)
    IF(checkdata(2,1)) CALL checkindata_part2(funit_used)
    CALL checkindata_stop(funit_used,status)
    WRITE(funit_used,*)         '---------------------------------------------------'
    
    IF (.NOT. error_halt) THEN
      status = 0
    END IF

    RETURN
	
	!Error handling
300 WRITE(0,*) 'Error: writing to hype tests funit',funit_used
    status = 1
    RETURN

  END SUBROUTINE run_hype_observation_tests

  !>Runs tests for hype and prints the result in specified funit
  !----------------------------------------------------------------------------
  SUBROUTINE run_hype_tests(status)

    !Argument declarations
    INTEGER, INTENT(OUT)  :: status  !<error number

    !Local variables
    
    IF (printout_level <= 0) THEN
      WRITE(funit_used,*,ERR=301) 'No indata checks will be performed; printout_level',printout_level
      status = 0
      RETURN
    END IF

    WRITE(funit_used,*,ERR=301) '------------------ GENERIC ------------------------'
    CALL check_info_generic(status)
    CALL check_geodata_generic(status)
    CALL check_geoclass_generic(status)
    CALL check_generic(status)

    WRITE(funit_used,*) '------------------ WATER --------------------------'
    
    WRITE(funit_used,*) '-------------- Processes above ground -------------'
    WRITE(funit_used,*) '-- Temperature and precipitation --'
    
    WRITE(funit_used,*) '-- Evaporation --'
    CALL check_evaporation(status)
    
    WRITE(funit_used,*) '-- Atmospheric deposition of nitrogen and phosphorus --'
	
    WRITE(funit_used,*) '-------------- Land routines ----------------------'
    WRITE(funit_used,*) '-- Soil water --'
    CALL check_soil_water(status)
    WRITE(funit_used,*) '-- Snow routines --'
    CALL check_snow_routines(status)
    WRITE(funit_used,*) '-- Glaciers --'
    CALL check_glaciers(status)
    
    WRITE(funit_used,*) '-------------- Rivers and lakes -------------------'
    WRITE(funit_used,*) '-- Rivers --'
    CALL check_rivers(status)
    WRITE(funit_used,*) '-- Lakes --'
    CALL check_lakes(status)
    WRITE(funit_used,*) '-- Floodplains --'
    CALL check_floodplains(status)
    WRITE(funit_used,*) '-- Bifurcations --'
    
    WRITE(funit_used,*) '------------------ SUBSTANCES ---------------------'
    
    WRITE(funit_used,*) '---- Nitrogen and phosphorus in land routines -----'
    WRITE(funit_used,*) '-- Nutrient Sources --'
    WRITE(funit_used,*) '-- Vegetation and soil surface processes --'
    CALL check_veg_soil_surface(status)
    WRITE(funit_used,*) '-- Nutrient soil processes --'
    
    WRITE(funit_used,*) '- Nitrogen and phosphorus processes in rivers and lakes -'
    WRITE(funit_used,*) '-- Denitrification --'
    WRITE(funit_used,*) '-- Primary production and mineralization --'
    WRITE(funit_used,*) '-- Sedimentation/Resuspension --'
    WRITE(funit_used,*) '-- Internal load --'
    
    WRITE(funit_used,*) '-------------------- STATUS -----------------------'
    WRITE(funit_used,*) '-- Total --', status

    IF (.NOT. error_halt) THEN
        status = 0
    END IF
    
    RETURN
	
	!Error handling
301 WRITE(0,*) 'Error: writing to hype tests funit', funit_used
    status = 1
    RETURN

  END SUBROUTINE run_hype_tests

  !>Generic info tests
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_info_generic(status)

    USE WORLDVAR, ONLY: ndt,bdate,sdate,outstartdate,output,noutput

    USE MODVAR, ONLY : outvarid

    !Argument declaration
    INTEGER, INTENT(OUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret
    INTEGER s,io,ivar,ivar2

    status = 0
    !Check simulation period
    ret = ndt > 0
    CALL add_generic_result(ret,0,a_noftimestepts,t_info_generic,'nof timesteps')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF
    ret = bdate%day >= 1 .AND. bdate%day <= 31
    ret = ret .AND. (bdate%month >= 1 .AND. bdate%month <= 12)
    CALL add_generic_result(ret,0,a_bdaterange,t_info_generic,'bdate range')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF
    ret = sdate%day >= 1 .AND. sdate%day <= 31
    ret = ret .AND. (sdate%month >= 1 .OR. sdate%month <= 12)
    CALL add_generic_result(ret,0,a_edaterange,t_info_generic,'edate range')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF
    ret = outstartdate%day >= 1 .AND. outstartdate%day <= 31
    ret = ret .AND. (outstartdate%month >= 1 .OR. outstartdate%month <= 12)
    CALL add_generic_result(ret,0,a_cdaterange,t_info_generic,'cdate range')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    !Check timeoutput and mapoutput variables for duplicates
    ret = .TRUE.
    s = 0
    DO io = 1, noutput
      IF(output(io)%fileformat==2 .OR. output(io)%fileformat==3)THEN
        DO ivar = 1,output(io)%nvar
          DO ivar2 = ivar+1,output(io)%nvar
            IF(outvarid(output(io)%variable(ivar)%idindex)%shortname==outvarid(output(io)%variable(ivar2)%idindex)%shortname &
                 .AND. output(io)%variable(ivar)%areaagg==output(io)%variable(ivar2)%areaagg)THEN
              ret = .FALSE.
              s = s + 1
            ENDIF
          ENDDO
        ENDDO
      ENDIF
    ENDDO

    CALL add_generic_result(ret,s,a_timemapduplicates,t_info_generic,'no duplicated time/mapoutputs')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    ret = status == 0
    CALL print_testcase_logical('info',ret,suffix='')

    CALL print_process_testcase(t_info_generic)

  END SUBROUTINE check_info_generic

  !>Generic GeoData tests
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_geodata_generic(status)

    USE HYPEVARIABLES, ONLY : n_gldepo

    USE MODVAR, ONLY : basin,classbasin,max_subid,slc_olake,slc_ilake, &
                       load,nsub_basemodel,pathsubid,branchsubid

    !Argument declaration
    INTEGER, INTENT(OUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret
    INTEGER s,ksource,kbranch,bdim,i,k

    status = 0
    ret = (MINVAL(basin(:)%area) >= 0.0) .AND. (MAXVAL(basin(:)%area) > 0.0)
    CALL add_generic_result(ret,0,a_basinarea,t_geodata_generic,'some basin areas lt zero')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    !Check of slc-fraction not zero if basin area is zero!
    ret = ALL(.NOT. ((basin(:)%area <= 0.0) .AND. (SUM(classbasin(:,:)%part, DIM=2) /= 0.0)))
    CALL add_generic_result(ret,0,a_basinslcfrac,t_geodata_generic,'basin slc fraction')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    ret = (MINVAL(basin(:)%subid) > 0) .AND. (MAXVAL(basin(:)%subid) < max_subid)
    CALL add_generic_result(ret,0,a_subidrange,t_geodata_generic,'subid range')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    ret = MINVAL(basin(:)%slope) >= 0
    CALL add_generic_result(ret,0,a_basinslope,t_geodata_generic,'basin slope')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    !TODO: Add the check of lake region for NPC simulation

    ret = MINVAL(basin(:)%rivlen(2)) >= 0
    CALL add_generic_result(ret,0,a_basinrivlen,t_geodata_generic,'basin rivlen')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    CALL check_input(ret,SUM(classbasin(:,:)%part, DIM=2),one_plus_minus_5percent,a_basinpartperc,t_geodata_generic)
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    ret = MINVAL(SUM(classbasin(:,:)%part, DIM=2)) >= 0.0
    CALL add_generic_result(ret,0,a_basinpospart,t_geodata_generic,'basin positive part')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    !Check olake ilake depths
    ret = ALL(.NOT.((classbasin(:,slc_olake)%part > 0.0) .AND. (basin(:)%lakedepth(2) <= 0.0)))
    CALL add_generic_result(ret,0,a_basinodepth,t_geodata_generic,'basin olake depth')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF
    ret = ALL(.NOT.((classbasin(:,slc_ilake)%part > 0.0) .AND. (basin(:)%lakedepth(1) <= 0.0)))
    CALL add_generic_result(ret,0,a_basinidepth,t_geodata_generic,'basin ilake depth')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    !Check abstractions
    ret = ALL(.NOT. (((load(:)%abstrvol > 0) .AND. (load(:)%abstrind == 2)) .AND. (slc_olake == 0)))
    CALL add_generic_result(ret,0,a_slcforolake,t_geodata_generic,'slc for olake')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF
    ret = ALL(.NOT. (((load(:)%abstrvol > 0) .AND. (load(:)%abstrind == 2)) .AND. (classbasin(:,slc_olake)%part <= 0.0)))
    CALL add_generic_result(ret,0,a_abstrwitholake,t_geodata_generic,'abstration with olake')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    !Check linkage and subbasin order
    IF(ALLOCATED(pathsubid)) THEN
      s = 0
      DO i = 1,nsub_basemodel
        DO k = i-1,1,-1
          IF(basin(k)%subid==pathsubid(i)%main) THEN
            s = s + 1
          END IF
        END DO
      END DO
      ret = (s == 0)
      CALL add_generic_result(ret,s,a_linkmain,t_geodata_generic,'linking main branch')
      IF (.NOT. ret) THEN
          status = status + 1
      END IF
    END IF

    IF(ALLOCATED(branchsubid)) THEN
      s = 0
      bdim=SIZE(branchsubid(:)%source)
      DO i = 1,bdim
        ksource = 0
        kbranch = 0
        DO k = 1,nsub_basemodel
          IF(branchsubid(i)%source==basin(k)%subid)  ksource = k
          IF(branchsubid(i)%branch==basin(k)%subid)  kbranch = k
        END DO
        IF(kbranch/=0) THEN
          IF(kbranch<ksource) THEN
            s = s + 1
          END IF
        END IF
      END DO
      ret = (s == 0)
      CALL add_generic_result(ret,s,a_linksecondary,t_geodata_generic,'linking secondary branch')
      IF (.NOT. ret) THEN
        status = status + 1
      END IF
    END IF

    ret = (status == 0)
    CALL print_testcase_logical('geodata',ret,suffix='')

    CALL print_process_testcase(t_geodata_generic)

  END SUBROUTINE check_geodata_generic

  !>Generic GeoClass tests
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_geoclass_generic(status)

    USE MODVAR, ONLY : nclass,classdata,classmodel,maxsoillayers

    !Argument declaration
    INTEGER, INTENT(OUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret,ret2
    INTEGER i,help1,help2,help3,help4,help5

    status = 0
    ret = (nclass > 0)
    CALL add_generic_result(ret,0,a_nclass,t_geoclass_generic,'nclass gt zero')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    ret = .TRUE.
    CALL check_input(ret,REAL(classdata(:)%luse),gt_zero,a_classdataluse,t_geoclass_generic)
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    ret = .TRUE.
    CALL check_input(ret,REAL(classdata(:)%soil),gt_zero,a_classdatasoil,t_geoclass_generic)
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    !Check if multiple classes of the same type
    ret = (nclass > 0)
    CALL add_generic_result(ret,0,a_nclass,t_geoclass_generic,'nclass gt zero')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    IF(ALLOCATED(classmodel)) THEN
      help1=0;help2=0;help3=0;help4=0;help5=0
      DO i=1,nclass
        IF(classmodel(i)==1) THEN
          help1 = help1 + 1
        ELSE IF(classmodel(i)==2) THEN
          help2 = help2 + 1
        ELSE IF(classmodel(i)==3) THEN
          help3 = help3 + 1
        ELSE IF(classmodel(i)==11) THEN
          help4 = help4 + 1
        ELSE IF(classmodel(i)==12) THEN
          help5 = help5 + 1
        END IF
      END DO
      ret = (help1 <= 1 .AND. help2 <= 1 .AND. help3 <= 1 .AND. help4 <= 1 .AND. help5 <= 1)
      CALL add_generic_result(ret,0,a_multiple_classes,t_geoclass_generic,'no multiple classes, ilake,olake,mriver,lriver,glaicier')
      IF (.NOT. ret) THEN
          status = status + 1
      END IF
    END IF

    !Check soildepth
    ret = .TRUE.
    ret2 = .TRUE.
    DO i=1,maxsoillayers
        CALL check_input(ret,classdata(:)%soildepth(i),gt_zero,a_soildepth,t_geoclass_generic,force=.TRUE.)
        IF (.NOT. ret) THEN
            status = status + 1
        END IF
        IF (i<maxsoillayers) THEN
            !Check that each soillayer as an increasing depth
            ret2 = (MINVAL(classdata(:)%soildepth(i+1)-classdata(:)%soildepth(i)) >= 0);
            CALL add_generic_result(ret2,0,a_inc_soildepth,t_geoclass_generic,'increasing soildepth')
        END IF
    END DO

    ret = (status == 0)
    CALL print_testcase_logical('geoclass',ret,suffix='')

    CALL print_process_testcase(t_geoclass_generic)

  END SUBROUTINE check_geoclass_generic

  !>Generic tests
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_generic(status)

    USE HYPEVARIABLES, ONLY : n_snalbmin,n_snalbmax, &
                              n_wcfc,n_wcfc1,n_wcfc2,n_wcfc3, &
                              n_wcwp,n_wcwp1,n_wcwp2,n_wcwp3, &
                              n_wcep,n_wcep1,n_wcep2,n_wcep3

    USE MODVAR, ONLY : num_modelprocess,modeloption,maxsoillayers, &
                       maxmodelsinoption,soilpar,modparid,nsoil

    !Argument declaration
    INTEGER, INTENT(OUT)  :: status  !<Return status

    !Local variables
    INTEGER i,j,s(maxsoillayers)
    LOGICAL ret,r(maxsoillayers)
    REAL :: val_wcfc(maxsoillayers,nsoil)
    REAL :: val_wcwp(maxsoillayers,nsoil)
    REAL :: val_wcep(maxsoillayers,nsoil)
    REAL :: sum_val

    !Check modeloptions within range
    ret = .TRUE.
    status = 0
    DO i = 1, num_modelprocess
        IF (modeloption(i) .GT. maxmodelsinoption(i) .OR. modeloption(i) .LT. 0) THEN
            status = status + 1
            ret = .FALSE.
        END IF
    END DO
    CALL add_generic_result(ret,status,a_modeloptionrange,t_generic,'modeloption range')
    CALL print_testcase_logical('modeloption range',ret,suffix='')

    !Check snalbmin and snalbmax range
    ret = .TRUE.
    IF (n_snalbmin < n_snalbmax) THEN
        CALL check_param(ret,ge_zero_and_le_one,n_snalbmin,t_generic)
        CALL check_param(ret,ge_zero_and_le_one,n_snalbmax,t_generic)
    ELSE
        ret = .FALSE.
        status = status + 1
    END IF
    CALL add_generic_result(ret,0,a_snalbrange,t_generic,'snalbminmax range')
    CALL print_testcase_logical('snalbminmax range',ret,suffix='')

    !Check wcfc,wcwp and wcep
    ret = .TRUE.
    CALL check_param(ret,ge_zero_and_lt_one,n_wcfc,t_generic)
    CALL check_param(ret,ge_zero_and_lt_one,n_wcwp,t_generic)
    CALL check_param(ret,ge_zero_and_lt_one,n_wcep,t_generic)

    CALL check_param(ret,ge_zero_and_lt_one,n_wcfc1,t_generic)
    CALL check_param(ret,ge_zero_and_lt_one,n_wcfc2,t_generic)
    CALL check_param(ret,ge_zero_and_lt_one,n_wcfc3,t_generic)
    CALL check_param(ret,ge_zero_and_lt_one,n_wcwp1,t_generic)
    CALL check_param(ret,ge_zero_and_lt_one,n_wcwp2,t_generic)
    CALL check_param(ret,ge_zero_and_lt_one,n_wcwp3,t_generic)
    CALL check_param(ret,ge_zero_and_lt_one,n_wcep1,t_generic)
    CALL check_param(ret,ge_zero_and_lt_one,n_wcep2,t_generic)
    CALL check_param(ret,ge_zero_and_lt_one,n_wcep3,t_generic)

    !Check sums for wcfc,wcwp and wcep
    val_wcfc(1,:) = soilpar(modparid(n_wcfc1)%parno,:)
    val_wcfc(2,:) = soilpar(modparid(n_wcfc2)%parno,:)
    val_wcfc(3,:) = soilpar(modparid(n_wcfc3)%parno,:)
    val_wcwp(1,:) = soilpar(modparid(n_wcwp1)%parno,:)
    val_wcwp(2,:) = soilpar(modparid(n_wcwp2)%parno,:)
    val_wcwp(3,:) = soilpar(modparid(n_wcwp3)%parno,:)
    val_wcep(1,:) = soilpar(modparid(n_wcep1)%parno,:)
    val_wcep(2,:) = soilpar(modparid(n_wcep2)%parno,:)
    val_wcep(3,:) = soilpar(modparid(n_wcep3)%parno,:)

    !Initialize result and status
    DO j = 1, maxsoillayers
        r(j) = .TRUE.
        s(j) = 0
    END DO

    DO i = 1, nsoil
        DO j = 1, maxsoillayers
            !wcfc
            IF (val_wcfc(j,i) <= 0) THEN
                val_wcfc(j,i) = soilpar(modparid(n_wcfc)%parno,i)
            END IF
            !wcwp
            IF (val_wcwp(j,i) <= 0) THEN
                val_wcwp(j,i) = soilpar(modparid(n_wcwp)%parno,i)
            END IF
            !wcep
            IF (val_wcep(j,i) <= 0) THEN
                val_wcep(j,i) = soilpar(modparid(n_wcep)%parno,i)
            END IF
            !here are the sum checks
            sum_val = val_wcfc(j,i) + val_wcwp(j,i) + val_wcep(j,i)
            IF (sum_val <= 0 .OR. sum_val > 1) THEN
                r(j) = .FALSE.
                s(j) = s(j) + 1
            END IF
        END DO
    END DO

    CALL add_generic_result(r(1),s(1),a_sumwcfc,t_generic,'wcfc sum')
    CALL add_generic_result(r(2),s(2),a_sumwcwp,t_generic,'wcwp sum')
    CALL add_generic_result(r(3),s(3),a_sumwcep,t_generic,'wcep sum')
    CALL print_testcase_logical('wcfc sum',r(1),suffix='')
    CALL print_testcase_logical('wcwp sum',r(2),suffix='')
    CALL print_testcase_logical('wcep sum',r(3),suffix='')

    status = status + s(1) + s(2) + s(3)

    CALL print_process_testcase(t_generic)

  END SUBROUTINE check_generic

  !>Check and test lakes options
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_lakes(status)

    USE MODVAR, ONLY : modeloption,p_swtemperature, &
                       swtemperaturemodelnames

    !Argument declaration
    INTEGER, INTENT(OUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret

    status = 0
    ret = valid_swtemperature_options(modeloption(p_swtemperature))
    CALL print_testcase_logical(swtemperaturemodelnames(modeloption(p_swtemperature)),ret,prefix='swtemperaturemodel: ')
    IF (.NOT. ret) THEN
        status = 1
    END IF

    CALL print_process_testcase(t_lakes)

  END SUBROUTINE check_lakes

  !>Check and test specified swtemperature model for valid options
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_swtemperature_options(current_swtemperaturemodel)

    USE HYPEVARIABLES, ONLY : n_laketemp
    USE MODVAR, ONLY : i_t2

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_swtemperaturemodel  !<swtemperaturemodel to be tested for valid options

    !Local variables
    LOGICAL result

    valid_swtemperature_options = .TRUE.
    SELECT CASE(current_swtemperaturemodel)
      CASE(0)
        !laketemp          >= 0.0               --> OK
        CALL check_param(valid_swtemperature_options,ge_zero,n_laketemp,t_lakes)
      CASE(1)
        !i_t2>0            TRUE                 --> OK
        result = (i_t2 > 0)
        CALL add_generic_result(result,0,a_i_t2_lt_zero,t_lakes,'i_t2 larger than zero')
        valid_swtemperature_options = valid_swtemperature_options .AND. result
    END SELECT

  END FUNCTION valid_swtemperature_options

  !>Check and veg and soil surface processes
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_veg_soil_surface(status)

    USE MODVAR, ONLY : modeloption,p_growthstart,growthstartmodelnames, &
                       p_erosion,erosionmodelnames,conduct

    !Argument declaration
    INTEGER, INTENT(OUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret

    status = 0
    IF (conduct%simN .AND. conduct%simP) THEN
        ret = valid_growthstart_options(modeloption(p_growthstart))
        CALL print_testcase_logical(growthstartmodelnames(modeloption(p_growthstart)),ret,prefix='growthstartmodel: ')
        IF (.NOT. ret) THEN
            status = status + 1
        END IF
    END IF

    IF (conduct%simP .OR. conduct%simS) THEN
        ret = valid_erosion_options(modeloption(p_erosion))
        CALL print_testcase_logical(erosionmodelnames(modeloption(p_erosion)),ret,prefix='erosionmodel: ')
        IF (.NOT. ret) THEN
            status = status + 1
        END IF
    END IF

    CALL print_process_testcase(t_veg_soil_surface)

  END SUBROUTINE check_veg_soil_surface

  !>Check and test specified growthstart model for valid options
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_growthstart_options(current_growthstartmodel)

    USE MODVAR, ONLY : basin,cropdata

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_growthstartmodel  !<growthstartmodel to be tested for valid options

    !Local variables
    LOGICAL r

    valid_growthstart_options = .TRUE.
    SELECT CASE(current_growthstartmodel)
      CASE(0)
        !CropData.txt  EXIST = TRUE   --> OK
        !gddsow        > 0.0          --> OK
        !daylength     > 0.0          --> OK
        !firstday      > 0.0          --> OK
        CALL check_file_existence(valid_growthstart_options,'CropData.txt',a_CropData,t_veg_soil_surface)
        IF (ALLOCATED(cropdata)) THEN
          CALL check_input(valid_growthstart_options,cropdata(:)%gddsow,gt_zero,a_gddsow,t_veg_soil_surface)
          CALL check_input(valid_growthstart_options,cropdata(:)%daylength,gt_zero,a_daylenth,t_veg_soil_surface)
          CALL check_input(valid_growthstart_options,REAL(cropdata(:)%firstday),gt_zero,a_firstday,t_veg_soil_surface)
        END IF
      CASE(1)
    END SELECT

    !Latitude  >= -90.0 AND <= 90.0         --> OK
    !Latitude  MIN() != 0.0 OR MAX() != 0.0 --> OK
    CALL check_input(valid_growthstart_options,basin(:)%latitude,plus_minus_90,a_latitude,t_veg_soil_surface)
    r = (MINVAL(basin(:)%latitude) /= 0.0) .OR. (MAXVAL(basin(:)%latitude) /= 0.0)
    CALL add_generic_result(r,0,a_latitude_defined,t_veg_soil_surface,'latitude defined')
    valid_growthstart_options = valid_growthstart_options .AND. r

  END FUNCTION valid_growthstart_options

  !>Check and test specified erosion model for valid options
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_erosion_options(current_erosionmodel)

    USE HYPEVARIABLES, ONLY : n_soilcoh,n_soilerod,n_sreroexp, &
                              n_erodluse,n_erodsoil,n_erodslope,n_erodexp,n_erodindex, &
                              n_filtPbuf,n_filtPinner,n_filtPother,n_macfilt, &
                              n_incorr,n_oncorr,n_phoscorr,n_pprelmax,n_pprelexp
    USE MODVAR, ONLY : basin

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_erosionmodel  !<erosionmodel to be tested for valid options

    !Local variables

    valid_erosion_options = .TRUE.
    SELECT CASE(current_erosionmodel)
      CASE(0)
        !soilcoh          >  0.0         --> OK
        !soilerod         >  0.0         --> OK
        !sreroexp         >= 0.0         --> OK
        CALL check_param(valid_erosion_options,gt_zero,n_soilcoh,t_veg_soil_surface)
        CALL check_param(valid_erosion_options,gt_zero,n_soilerod,t_veg_soil_surface)
        CALL check_param(valid_erosion_options,ge_zero,n_sreroexp,t_veg_soil_surface)
        !incorr           >= 0.0         --> OK
        !oncorr           >= 0.0         --> OK
        !phoscorr         >= 0.0         --> OK
        CALL check_param(valid_erosion_options,ge_zero,n_incorr,t_veg_soil_surface)
        CALL check_param(valid_erosion_options,ge_zero,n_oncorr,t_veg_soil_surface)
        CALL check_param(valid_erosion_options,ge_zero,n_phoscorr,t_veg_soil_surface)
      CASE(1)
        !erodluse           >= 0.0         --> OK
        !erodsoil           >= 0.0         --> OK
        !erodslope          >= 0.0         --> OK
        !erodexp            >  0.0         --> OK
        !erodindex          >= 0.0         --> OK
        CALL check_param(valid_erosion_options,ge_zero,n_erodluse,t_veg_soil_surface)
        CALL check_param(valid_erosion_options,ge_zero,n_erodsoil,t_veg_soil_surface)
        CALL check_param(valid_erosion_options,ge_zero,n_erodslope,t_veg_soil_surface)
        CALL check_param(valid_erosion_options,gt_zero,n_erodexp,t_veg_soil_surface)
        CALL check_param(valid_erosion_options,ge_zero,n_erodindex,t_veg_soil_surface)
        !TODO: Necessary to test inputs below?
        !basin(:)%eroindex  >= 0.0         --> OK
        !basin(:)%slope     >= 0.0         --> OK
        CALL check_input(valid_erosion_options,basin(:)%eroindex,ge_zero,a_eroindex,t_veg_soil_surface)
        CALL check_input(valid_erosion_options,basin(:)%slope,ge_zero,a_slope,t_veg_soil_surface)
    END SELECT

    CALL check_param(valid_erosion_options,ge_zero,n_filtPbuf,t_veg_soil_surface)
    CALL check_param(valid_erosion_options,ge_zero,n_filtPinner,t_veg_soil_surface)
    CALL check_param(valid_erosion_options,ge_zero,n_filtPother,t_veg_soil_surface)
    CALL check_param(valid_erosion_options,ge_zero,n_macfilt,t_veg_soil_surface)
    !pprelmax         >  0.0         --> OK
    !pprelexp         >= 0.0         --> OK
    CALL check_param(valid_erosion_options,gt_zero,n_pprelmax,t_veg_soil_surface)
    CALL check_param(valid_erosion_options,ge_zero,n_pprelexp,t_veg_soil_surface)

  END FUNCTION valid_erosion_options

  !>Check and test soil water options
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_soil_water(status)

    USE MODVAR, ONLY : modeloption,p_deepgroundwater,p_infiltration, &
                       deepgroundwatermodelnames,infiltrationmodelnames

    !Argument declaration
    INTEGER, INTENT(OUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret

    status = 0
    ret = valid_deepgroundwater_options(modeloption(p_deepgroundwater))
    CALL print_testcase_logical(deepgroundwatermodelnames(modeloption(p_deepgroundwater)),ret,prefix='deepgroundwatermodel: ')
    IF (.NOT. ret) THEN
        status = 1
    END IF
    ret = valid_infiltration_options(modeloption(p_infiltration))
    CALL print_testcase_logical(infiltrationmodelnames(modeloption(p_infiltration)),ret,prefix='infiltrationmodel: ')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    CALL print_process_testcase(t_soil_water)

  END SUBROUTINE check_soil_water

  !>Check and test specified infiltration model for valid options
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_infiltration_options(current_infiltrationmodel)

    USE HYPEVARIABLES, ONLY : n_mactrsm,n_mactrinf,n_srrate, &
                              n_macrate,n_bfroznsoil,n_ponatm
    USE MODVAR, ONLY : soilpar,modparid,nsoil,conduct

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_infiltrationmodel  !<infiltrationmodel to be tested for valid options

    !Local variables
    LOGICAL r
    INTEGER i,s
    REAL sum_val

    valid_infiltration_options = .TRUE.
    !mactrsm   >= 0.0          --> OK
    !mactrinf  >= 0.0          --> OK
    !srrate    >= 0.0, <= 1.0  --> OK
    !macrate   >= 0.0, <= 1.0  --> OK
    CALL check_param(valid_infiltration_options,ge_zero,n_mactrsm,t_soil_water)
    CALL check_param(valid_infiltration_options,ge_zero,n_mactrinf,t_soil_water)
    CALL check_param(valid_infiltration_options,ge_zero_and_le_one,n_srrate,t_soil_water)
    CALL check_param(valid_infiltration_options,ge_zero_and_le_one,n_macrate,t_soil_water)

    SELECT CASE(current_infiltrationmodel)
      CASE(0)
        !sum macratesrrate >= 0, <= 1 --> OK
        r = .TRUE.
        s = 0
        DO i = 1, nsoil
           sum_val = soilpar(modparid(n_macrate)%parno,i) + soilpar(modparid(n_srrate)%parno,i)
           IF (sum_val < 0 .OR. sum_val > 1) THEN
               r = .FALSE.
               s = s + 1
           END IF
        END DO
        CALL add_generic_result(r,s,a_sum_macratesrrate,t_soil_water,'macrate srrate sum')
      CASE(1)
        !sum macratesrrate > 0, <= 1  --> OK
        !bfroznsoil        >= 0.0     --> OK
        r = .TRUE.
        s = 0
        DO i = 1, nsoil
           sum_val = soilpar(modparid(n_macrate)%parno,i) + soilpar(modparid(n_srrate)%parno,i)
           IF (sum_val <= 0 .OR. sum_val > 1) THEN
               r = .FALSE.
               s = s + 1
           END IF
        END DO
        CALL add_generic_result(r,s,a_sum_macratesrrate,t_soil_water,'macrate srrate sum')
        CALL check_param(valid_infiltration_options,ge_zero,n_bfroznsoil,t_soil_water)
    END SELECT

    IF (conduct%simN) THEN
      !ponatm  >= 0.0, <= 1.0  --> OK
      CALL check_param(valid_infiltration_options,ge_zero_and_le_one,n_ponatm,t_soil_water)
    END IF

  END FUNCTION valid_infiltration_options

  !>Check and test specified deepgroundwater model for valid options
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_deepgroundwater_options(current_deepgroundwatermodel)

    USE HYPEVARIABLES, ONLY : n_rcgrw,n_aqpercorr,n_aqdelcorr,n_aqretcorr, &
                              n_rcgrwst,n_denitaq,n_hsatINsoil
    USE MODVAR, ONLY : aquifer,path,conduct

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_deepgroundwatermodel  !<deepgroundwatermodel to be tested for valid options

    !Local variables

    valid_deepgroundwater_options = .TRUE.
    SELECT CASE(current_deepgroundwatermodel)
      CASE(1)
      CASE(2)
        !aqdelcorr        >  -1.0        --> OK
        !aqretcorr        >= -1.0        --> OK
        !AquiferData.txt  EXIST = TRUE   --> OK
        CALL check_param(valid_deepgroundwater_options,gt_minus_one,n_aqdelcorr,t_soil_water)
        CALL check_param(valid_deepgroundwater_options,ge_minus_one,n_aqretcorr,t_soil_water)
        CALL check_file_existence(valid_deepgroundwater_options,'AquiferData.txt',a_AquiferData,t_soil_water)
        IF (conduct%simN) THEN
          !denitaq        >= 0.0         --> OK
          !hsatINsoil     >= 0.0         --> OK
          CALL check_param(valid_deepgroundwater_options,ge_zero,n_denitaq,t_soil_water)
          CALL check_param(valid_deepgroundwater_options,ge_zero,n_hsatINsoil,t_soil_water)
        END IF
      CASE DEFAULT
        RETURN
    END SELECT

    !rcgrw          >= 0.0               --> OK
    !rcgrwst        >= 0.0               --> OK
    !aqpercorr      >= -1.0              --> OK
    !grwtolake      >= 0.0, <= 1.0       --> OK
    CALL check_param(valid_deepgroundwater_options,ge_zero,n_rcgrw,t_soil_water)
    CALL check_param(valid_deepgroundwater_options,ge_zero,n_rcgrwst,t_soil_water)
    CALL check_param(valid_deepgroundwater_options,ge_minus_one,n_aqpercorr,t_soil_water)
    CALL check_input(valid_deepgroundwater_options,path(:)%grwtolake,ge_zero_and_le_one,a_grwtolake,t_soil_water)

    IF (ALLOCATED(aquifer)) THEN
        CALL check_input(valid_deepgroundwater_options,aquifer(:)%percdelay,ge_zero,a_percdelay,t_soil_water)
        CALL check_input(valid_deepgroundwater_options,aquifer(:)%area,gt_zero,a_aqarea,t_soil_water)
        CALL check_input(valid_deepgroundwater_options,aquifer(:)%porosity,gt_zero_and_lt_one,a_porosity,t_soil_water)
        CALL check_input(valid_deepgroundwater_options,aquifer(:)%retrate,ge_zero_and_le_one,a_retrate,t_soil_water)
        CALL check_input(valid_deepgroundwater_options,aquifer(:)%inivol,ge_zero,a_inivol,t_soil_water)
        CALL check_input(valid_deepgroundwater_options,aquifer(:)%basedepth,lt_zero,a_basedepth,t_soil_water)
        CALL check_input(valid_deepgroundwater_options,REAL(aquifer(:)%parregion(2)),gt_zero,a_parregion2,t_soil_water)
    END IF
    IF (ALLOCATED(path)) THEN
        CALL check_input(valid_deepgroundwater_options,path(:)%recievefraction,ge_zero_and_le_one,a_recievefrac,t_soil_water)
    END IF

  END FUNCTION valid_deepgroundwater_options

  !>Check and test floodplains options
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_floodplains(status)

    USE MODVAR, ONLY : modeloption,p_floodplain, &
                       floodplainmodelnames

    !Argument declaration
    INTEGER, INTENT(OUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret

    status = 0
    ret = valid_floodplain_options(modeloption(p_floodplain))
    CALL print_testcase_logical(floodplainmodelnames(modeloption(p_floodplain)),ret,prefix='floodplainmodel: ')
    IF (.NOT. ret) THEN
        status = 1
    END IF

    CALL print_process_testcase(t_floodplains)

  END SUBROUTINE check_floodplains

  !>Check and test specified floodplain model for valid options
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_floodplain_options(current_floodplainmodel)

    USE HYPEVARIABLES, ONLY : basinrrcscorr,n_mactrinf,n_T1evap,n_ttmp,n_cmlt,n_srrcs
    USE MODVAR, ONLY : numsubstances,i_t1

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_floodplainmodel  !<floodplainmodel to be tested for valid options

    !Local variables

    valid_floodplain_options = .TRUE.
    SELECT CASE(current_floodplainmodel)
      CASE(1)
      CASE(2)
        !mactrinf       >= 0.0           --> OK
        !cmlt           >  0.0           --> OK
        !srrcs          >= 0.0           --> OK
        !basinrrcscorr  >= 0.0           --> OK
        CALL check_param(valid_floodplain_options,ge_zero,n_mactrinf,t_floodplains)
        CALL check_param(valid_floodplain_options,gt_zero,n_cmlt,t_floodplains)
        CALL check_param(valid_floodplain_options,ge_zero,n_srrcs,t_floodplains)
        !TODO: correct to test basinrrcscorr?
        IF (ALLOCATED(basinrrcscorr)) THEN
          CALL check_input(valid_floodplain_options,basinrrcscorr(:),ge_zero,a_rrcscorr,t_floodplains)
        END IF
      CASE DEFAULT
        RETURN
    END SELECT

    !FloodData.txt  EXIST = TRUE   --> OK
    !ttmp           >= 0.0         --> OK
    CALL check_file_existence(valid_floodplain_options,'FloodData.txt',a_FloodData,t_floodplains)
    CALL check_param(valid_floodplain_options,ge_zero,n_ttmp,t_floodplains)
    IF (i_t1 > 0 .AND. numsubstances > 0) THEN
      ! T1evap  >= 0.0, <= 1.0   --> OK
      CALL check_param(valid_floodplain_options,ge_zero_and_le_one,n_T1evap,t_floodplains)
    END IF

    !TODO: Should m_optonoff and m_opt1-8 be tested?

  END FUNCTION valid_floodplain_options

  !>Check and test river options
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_rivers(status)

    USE MODVAR, ONLY : modeloption,p_lakeriverice, &
                       lakerivericemodelnames

    !Argument declaration
    INTEGER, INTENT(OUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret

    status = 0
    ret = valid_lakeriverice_options(modeloption(p_lakeriverice))
    CALL print_testcase_logical(lakerivericemodelnames(modeloption(p_lakeriverice)),ret,prefix='lakerivericemodel: ')
    IF (.NOT. ret) THEN
        status = 1
    END IF

    CALL print_process_testcase(t_rivers)

  END SUBROUTINE check_rivers

  !>Check and test specified lakeriverice model for valid options
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_lakeriverice_options(current_lakerivericemodel)

    USE HYPEVARIABLES, ONLY : n_t2trlake,n_t2trriver, &
                              n_tcflake,n_scflake, &
                              n_tcfriver,n_scfriver,n_limt2exch, &
                              n_stbcorr1,n_stbcorr2,n_stbcorr3, &
                              n_ccflake,n_lcflake, &
                              n_ccfriver,n_lcfriver, &
                              n_licetf, n_ricetf, m_ldfastlake

    USE MODVAR, ONLY : lakedatapar

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_lakerivericemodel  !<lakerivericemodel to be tested for valid options

    !Local variables
    LOGICAL tmp_result(2)

    tmp_result = [.TRUE.,.TRUE.]

    valid_lakeriverice_options = .TRUE.
    SELECT CASE(current_lakerivericemodel)
      CASE(1) !the simple air-water temperature exchange model (Johan/David), with
              !modifications for fractional ice cover, and calculation of fractional freezup area
        !t2trlake      >  0.0     --> OK
        !t2trriver     >  0.0     --> OK
        CALL check_param(valid_lakeriverice_options,gt_zero,n_t2trlake,t_rivers)
        CALL check_param(valid_lakeriverice_options,gt_zero,n_t2trriver,t_rivers)
      CASE(2) !new model based on Piccolroaz et al 2013, with modifications for fractional
              !ice cover, and calculation of fractional freezup area
        !tcflake       >  0.0     --> OK
        !scflake       >  0.0     --> OK
        !tcfriver      >  0.0     --> OK
        !scfriver      >  0.0     --> OK
        !stbcorr1      >= 0.0     --> OK
        !stbcorr2      >= 0.0     --> OK
        !stbcorr3      >= 0.0     --> OK
        !lake
        CALL check_param(tmp_result(1),gt_zero,n_tcflake,t_rivers)
        CALL check_param(tmp_result(2),gt_zero,n_scflake,t_rivers)
        IF ((.NOT. tmp_result(1)) .AND. (.NOT. tmp_result(2))) THEN
            valid_lakeriverice_options = .FALSE.
        ELSE IF (.NOT. tmp_result(1)) THEN
            CALL uncheck(n_tcflake,t_rivers)
        ELSE IF (.NOT. tmp_result(2)) THEN
            CALL uncheck(n_scflake,t_rivers)
            !ccflake      >  0.0     --> OK
            !lcflake      >  0.0     --> OK
            CALL check_param(valid_lakeriverice_options,gt_zero,n_ccflake,t_rivers)
            CALL check_param(valid_lakeriverice_options,gt_zero,n_lcflake,t_rivers)
        END IF
        !river
        CALL check_param(tmp_result(1),gt_zero,n_tcfriver,t_rivers)
        CALL check_param(tmp_result(2),gt_zero,n_scfriver,t_rivers)
        IF ((.NOT. tmp_result(1)) .AND. (.NOT. tmp_result(2))) THEN
            valid_lakeriverice_options = .FALSE.
        ELSE IF (.NOT. tmp_result(1)) THEN
            CALL uncheck(n_tcfriver,t_rivers)
        ELSE IF (.NOT. tmp_result(2)) THEN
            CALL uncheck(n_scfriver,t_rivers)
            !ccfriver      >  0.0     --> OK
            !lcfriver      >  0.0     --> OK
            CALL check_param(valid_lakeriverice_options,gt_zero,n_ccfriver,t_rivers)
            CALL check_param(valid_lakeriverice_options,gt_zero,n_lcfriver,t_rivers)
        END IF
        CALL check_param(valid_lakeriverice_options,ge_zero,n_limt2exch,t_rivers)
        CALL check_param(valid_lakeriverice_options,ge_zero,n_stbcorr1,t_rivers)
        CALL check_param(valid_lakeriverice_options,ge_zero,n_stbcorr2,t_rivers)
        CALL check_param(valid_lakeriverice_options,ge_zero,n_stbcorr3,t_rivers)
    END SELECT

    !licetf        >= 0.0     --> OK
    !ricetf        >= 0.0     --> OK
    CALL check_param(valid_lakeriverice_options,ge_zero,n_licetf,t_rivers)
    CALL check_param(valid_lakeriverice_options,ge_zero,n_ricetf,t_rivers)
    !ldfastlake    >= 0.0     --> OK
    CALL check_input(valid_lakeriverice_options,lakedatapar(:,m_ldfastlake),ge_zero,a_ldfastlake,t_rivers)

  END FUNCTION valid_lakeriverice_options

  !>Check and test snow routine options
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_snow_routines(status)
  
    USE MODVAR, ONLY : modeloption,p_snowfall,p_snowmelt,p_snowevap,p_snowdensity, &
                       snowfallmodelnames,snowmeltmodelnames,snowevapmodelnames, &
                       snowdensitymodelnames

	!Argument declaration
    INTEGER, INTENT(OUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret
    
    status = 0
    ret = valid_snowfall_options(modeloption(p_snowfall))
    CALL print_testcase_logical(snowfallmodelnames(modeloption(p_snowfall)),ret,prefix='snowfallmodel: ')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF
    
    ret = valid_snowmelt_options(modeloption(p_snowmelt))
    CALL print_testcase_logical(snowmeltmodelnames(modeloption(p_snowmelt)),ret,prefix='snowmeltmodel: ')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF
    
    ret = valid_snowevap_options(modeloption(p_snowevap))
    CALL print_testcase_logical(snowevapmodelnames(modeloption(p_snowevap)),ret,prefix='snowevapmodel: ')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    ret = valid_snowdensity_options(modeloption(p_snowdensity))
    CALL print_testcase_logical(snowdensitymodelnames(modeloption(p_snowdensity)),ret,prefix='snowdensitymodel: ')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    CALL print_process_testcase(t_snow_routines)

  END SUBROUTINE check_snow_routines
  
  !>Check and test specified snow fall model for valid options
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_snowfall_options(current_snowfallmodel)

    USE WORLDVAR, ONLY : i_sfobs
  
    USE HYPEVARIABLES, ONLY : n_ttpi

	!Argument declaration
    INTEGER, INTENT(IN)  :: current_snowfallmodel  !<snowfallmodel to be tested for valid options

    !Local variables
    
    valid_snowfall_options = .TRUE.
    SELECT CASE(current_snowfallmodel)
      CASE(0) !Original model based on threshold temperatures
        !ttpi      >= 0.0               --> OK
        CALL check_param(valid_snowfall_options,ge_zero,n_ttpi,t_snow_routines)
      CASE(1) !Fraction of precipitation as snowfall is given in input data
        !sfobs
        CALL check_forcing(valid_snowfall_options,i_sfobs,a_sfobs,t_snow_routines)
      CASE DEFAULT
        valid_snowfall_options = .FALSE.
    END SELECT
    
  END FUNCTION valid_snowfall_options
  
  !>Check and test specified snow melt model for valid options
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_snowmelt_options(current_snowmeltmodel)

    USE WORLDVAR, ONLY : i_swobs,i_tminobs,i_tmaxobs
  
    USE HYPEVARIABLES, ONLY : n_cmlt,n_fsceff, &
                              n_snalbmin,n_snalbmax, &
                              n_snalbkexp,n_cmrad,n_cmrefr, &
                              n_krs

    USE MODVAR, ONLY : basin

	!Argument declaration
    INTEGER, INTENT(IN)  :: current_snowmeltmodel  !<snowmeltmodel to be tested for valid options

    !Local variables
    LOGICAL tmp_logical;
    
    !cmlt      >  0.0               --> OK
    !fsceff    >= 0.0, <= 1.0       --> OK
    CALL check_param(valid_snowmelt_options,gt_zero,n_cmlt,t_snow_routines)
    CALL check_param(valid_snowmelt_options,ge_zero_and_le_one,n_fsceff,t_snow_routines)
    
    valid_snowmelt_options = .TRUE.
    SELECT CASE(current_snowmeltmodel)
      CASE(0,1) !Original temperature index model, calculated with or without snowcover scaling
      CASE(2) !Temperature and radiation index model, with/without snowcover scaling and refreezing
        !snalbmin    >= 0.0, <= 1.0       --> OK
        !snalbmax    >= 0.0, <= 1.0       --> OK
        !snalbkexp   >  0.0               --> OK
        !cmrad       >  0.0               --> OK
        !cmrefr      >= 0.0               --> OK
        CALL check_param(valid_snowmelt_options,ge_zero_and_le_one,n_snalbmin,t_snow_routines)
        CALL check_param(valid_snowmelt_options,ge_zero_and_le_one,n_snalbmax,t_snow_routines)
        CALL check_param(valid_snowmelt_options,gt_zero,n_snalbkexp,t_snow_routines)
        CALL check_param(valid_snowmelt_options,gt_zero,n_cmrad,t_snow_routines)
        CALL check_param(valid_snowmelt_options,ge_zero,n_cmrefr,t_snow_routines)
        !swobs or (TMINobs and TMAXobs) or (krs and elevation)
        tmp_logical = .TRUE.
        CALL check_forcing(tmp_logical,i_swobs,a_swobs,t_snow_routines)
        IF (.NOT. tmp_logical) THEN
          tmp_logical = .TRUE.
          CALL uncheck(a_swobs,t_snow_routines)
          CALL check_forcing(tmp_logical,i_tminobs,a_tminobs,t_snow_routines)
          CALL check_forcing(tmp_logical,i_tmaxobs,a_tmaxobs,t_snow_routines)
          IF (.NOT. tmp_logical) THEN
            tmp_logical = .TRUE.
            CALL uncheck(a_tminobs,t_snow_routines)
            CALL uncheck(a_tmaxobs,t_snow_routines)
            CALL check_param(tmp_logical,ne_zero,n_krs,t_snow_routines)
            CALL check_input(tmp_logical,basin(:)%elev,ge_zero,a_elevation,t_snow_routines)
          END IF
        END IF
        valid_snowmelt_options = valid_snowmelt_options .AND. tmp_logical
      CASE DEFAULT
        valid_snowmelt_options = .FALSE.
    END SELECT
    
  END FUNCTION valid_snowmelt_options

  !>Check and test specified snow evaporation model for valid options
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_snowevap_options(current_snowevapmodel)

    USE HYPEVARIABLES, ONLY : n_fepotsnow

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_snowevapmodel  !<snowevapmodel to be tested for valid options

    !Local variables

    valid_snowevap_options = .TRUE.
    SELECT CASE(current_snowevapmodel)
      CASE(0) !off
        !fepotsnow    ==  0.0               --> OK
        CALL check_param(valid_snowevap_options,eq_zero,n_fepotsnow,t_snow_routines)
      CASE(1) !epotsnow = epot * fepotsnow (landuse dependent)
        !fepotsnow    >   0.0               --> OK
        CALL check_param(valid_snowevap_options,gt_zero,n_fepotsnow,t_snow_routines)
      CASE DEFAULT
        valid_snowevap_options = .FALSE.
    END SELECT

  END FUNCTION valid_snowevap_options

  !>Check and test specified snow density model for valid options
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_snowdensity_options(current_snowdensitymodel)

    USE MODVAR, ONLY : modeloption,p_lakeriverice
    USE HYPEVARIABLES, ONLY : n_sndens0,n_dsndens,n_ricesndens,n_licesndens, &
                              n_sdnsrate,n_sdnsradd,n_sdnsmax

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_snowdensitymodel  !<snowdensitymodel to be tested for valid options

    !Local variables

    valid_snowdensity_options = .TRUE.
    !sndens0       !=  0.0               --> OK
    CALL check_param(valid_snowdensity_options,ne_zero,n_sndens0,t_snow_routines);
    SELECT CASE(current_snowdensitymodel)
      CASE(0) !depending on age of snow, snowdens0 and snowdensdt
        !dsndens       >   0.0               --> OK
        !ricesndens    !=  0.0               --> OK
        !licesndens    !=  0.0               --> OK
        CALL check_param(valid_snowdensity_options,gt_zero,n_dsndens,t_snow_routines);
        IF (modeloption(p_lakeriverice) == 1) THEN
          CALL check_param(valid_snowdensity_options,ne_zero,n_ricesndens,t_snow_routines);
          CALL check_param(valid_snowdensity_options,ne_zero,n_licesndens,t_snow_routines);
        END IF
      CASE(1) !depending on compactation factor
        !sdnsrate   >   0.0               --> OK
        !sdnsradd   >=  0.0               --> OK
        !sdnsmax    >   0.0               --> OK
        CALL check_param(valid_snowdensity_options,gt_zero,n_sdnsrate,t_snow_routines);
        CALL check_param(valid_snowdensity_options,ge_zero,n_sdnsradd,t_snow_routines);
        CALL check_param(valid_snowdensity_options,gt_zero,n_sdnsmax,t_snow_routines);
      CASE DEFAULT
        valid_snowdensity_options = .FALSE.
    END SELECT

  END FUNCTION valid_snowdensity_options


  !>Check and test glacier options
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_glaciers(status)

    USE MODVAR, ONLY : modeloption,p_glacierini,glacierinimodelnames

    !Argument declaration
    INTEGER, INTENT(OUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret

    status = 0
    ret = valid_glacierini_options(modeloption(p_glacierini))
    CALL print_testcase_logical(glacierinimodelnames(modeloption(p_glacierini)),ret,prefix='glacierinimodel: ')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    CALL print_process_testcase(t_glaciers)

  END SUBROUTINE check_glaciers

  !>Check and test specified glacierini model for valid options
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_glacierini_options(current_glacierinimodel)

    USE MODVAR, ONLY : conduct
    USE HYPEVARIABLES, ONLY : n_glacdens,n_glacvcoef,n_glacvexp, &
                              n_glacvcoef1,n_glacvexp1

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_glacierinimodel  !<glacierinimodel to be tested for valid options

    !Local variables

    valid_glacierini_options = .TRUE.

    !glacdens      >  0.0      --> OK
    !glacvcoef     >  0.0      --> OK
    !glacvexp      >  0.0      --> OK
    !glacvcoef1    >  0.0      --> OK
    !glacvexp1     >  0.0      --> OK
    IF (conduct%glacier) THEN
      CALL check_param(valid_glacierini_options,gt_zero,n_glacdens,t_glaciers);
      CALL check_param(valid_glacierini_options,gt_zero,n_glacvcoef,t_glaciers);
      CALL check_param(valid_glacierini_options,gt_zero,n_glacvexp,t_glaciers);
      CALL check_param(valid_glacierini_options,gt_zero,n_glacvcoef1,t_glaciers);
      CALL check_param(valid_glacierini_options,gt_zero,n_glacvexp1,t_glaciers);
    END IF

    SELECT CASE(current_glacierinimodel)
      CASE(0) !Glacier init as usual via SLC+parameters or stated save
        continue
      CASE(1) !init from SLC+parameters overrides state_save
        continue
      CASE DEFAULT
        valid_glacierini_options = .FALSE.
    END SELECT

  END FUNCTION valid_glacierini_options

  !>Check and test evaporation options
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_evaporation(status)
    
    USE MODVAR, ONLY : p_petmodel,petmodelnames,maxmodelsinoption

	!Argument declaration
    INTEGER, INTENT(OUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret
    INTEGER i, num_petmodels_used
    INTEGER petmodels_used(maxmodelsinoption(p_petmodel))
    
    num_petmodels_used = find_petmodels_used(petmodels_used, maxmodelsinoption(p_petmodel))
    CALL log_petmodels_used(funit_used,petmodels_used,num_petmodels_used)
    
    status = num_petmodels_used
    DO i = 1, num_petmodels_used
      ret = valid_petmodel_options(petmodels_used(i))
      CALL print_testcase_logical(petmodelnames(petmodels_used(i)),ret,prefix='petmodel: ')
      status = status - 1
    END DO
    
    ret = check_actual_soil_evapotranspiration()
    CALL print_testcase_logical('actual_soil_evapotranspiration',ret)

    CALL print_process_testcase(t_evaporation)

  END SUBROUTINE check_evaporation

  !>Check and test specified petmodel for valid options
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_petmodel_options(current_petmodel)

    USE WORLDVAR, ONLY : i_tobs,i_rhobs, &
                         i_swobs,i_uobs, &
                         i_tminobs,i_tmaxobs
    USE HYPEVARIABLES, ONLY : n_ttmp,n_cevp, &
                              n_kc,n_krs,n_jhtadd,n_jhtscale,n_alfapt, &
                              n_alb,n_roughness,n_zpdh,n_zwind,n_zwish,n_mwind
    USE MODVAR, ONLY : basin

	!Argument declaration
    INTEGER, INTENT(IN)  :: current_petmodel  !<petmodel to be tested for valid options

    !Local variables
    LOGICAL r,tmp_result(3)
    
    tmp_result = [.TRUE.,.TRUE.,.TRUE.]
    valid_petmodel_options = .TRUE.
    SELECT CASE(current_petmodel)
      CASE(0) !HYPE original model (with Xobs replacement, if available)
        !ttmp      >= 0.0               --> OK
        !cevp      >  0.0               --> OK
        CALL check_param(valid_petmodel_options,ge_zero,n_ttmp,t_evaporation)
        CALL check_param(valid_petmodel_options,gt_zero,n_cevp,t_evaporation)
        !tobs
        CALL check_forcing(valid_petmodel_options,i_tobs,a_tobs,t_evaporation)
      CASE(1) !HYPE original model (without Xobs replacement)
        !ttmp      >= 0.0               --> OK
        !cevp      >  0.0               --> OK
        CALL check_param(valid_petmodel_options,ge_zero,n_ttmp,t_evaporation)
        CALL check_param(valid_petmodel_options,gt_zero,n_cevp,t_evaporation)
        !tobs
        CALL check_forcing(valid_petmodel_options,i_tobs,a_tobs,t_evaporation)
      CASE(2) !Modified Jensen-Haise/McGuinness following Oudin et al (2005)
        !kc        >  0.0               --> OK
        !jhtadd    != 0                 --> OK
        !jhtscale  != 0                 --> OK
        CALL check_param(valid_petmodel_options,gt_zero,n_kc(2),t_evaporation)
        CALL check_param(valid_petmodel_options,ne_zero,n_jhtadd,t_evaporation)
        CALL check_param(valid_petmodel_options,ne_zero,n_jhtscale,t_evaporation)
        !Latitude  >= -90.0 AND <= 90.0         --> OK
        !Latitude  MIN() != 0.0 OR MAX() != 0.0 --> OK
        CALL check_input(valid_petmodel_options,basin(:)%latitude,plus_minus_90,a_latitude,t_evaporation)
        r = (MINVAL(basin(:)%latitude) /= 0.0) .OR. (MAXVAL(basin(:)%latitude) /= 0.0)
        CALL add_generic_result(r,0,a_latitude_defined,t_evaporation,'latitude defined')
        valid_petmodel_options = valid_petmodel_options .AND. r
        !tobs
        CALL check_forcing(valid_petmodel_options,i_tobs,a_tobs,t_evaporation)
      CASE(3) !Hargreaves-Samani (known to overpredict in humid areas)
        !kc        >  0.0               --> OK
        !krs       != 0                 --> OK
        !Elevation >= 0.0               --> OK
        CALL check_param(valid_petmodel_options,gt_zero,n_kc(3),t_evaporation)
        CALL check_param(valid_petmodel_options,ne_zero,n_krs,t_evaporation)
        CALL check_input(valid_petmodel_options,basin(:)%elev,ge_zero,a_elevation,t_evaporation)
        !Latitude  >= -90.0 AND <= 90.0         --> OK
        !Latitude  MIN() != 0.0 OR MAX() != 0.0 --> OK
        CALL check_input(valid_petmodel_options,basin(:)%latitude,plus_minus_90,a_latitude,t_evaporation)
        r = (MINVAL(basin(:)%latitude) /= 0.0) .OR. (MAXVAL(basin(:)%latitude) /= 0.0)
        CALL add_generic_result(r,0,a_latitude_defined,t_evaporation,'latitude defined')
        valid_petmodel_options = valid_petmodel_options .AND. r
        !tobs
        CALL check_forcing(valid_petmodel_options,i_tobs,a_tobs,t_evaporation)
        !Depending on forcing data, different cases are ok
        !swobs OR (tminobs AND tmaxobs)
        CALL check_forcing(tmp_result(1),i_swobs,a_swobs,t_evaporation)
        CALL check_forcing(tmp_result(2),i_tminobs,a_tminobs,t_evaporation)
        CALL check_forcing(tmp_result(3),i_tmaxobs,a_tmaxobs,t_evaporation)
        IF (tmp_result(1)) THEN
          CALL uncheck(a_tminobs,t_evaporation)
          CALL uncheck(a_tmaxobs,t_evaporation)
          valid_petmodel_options = valid_petmodel_options .AND. .TRUE.
        ELSE IF (tmp_result(2) .AND. tmp_result(3)) THEN
          CALL uncheck(a_swobs,t_evaporation)
          valid_petmodel_options = valid_petmodel_options .AND. .TRUE.
        ELSE
          valid_petmodel_options = .FALSE.
        END IF
      CASE(4) !Priestly Taylor (known to underpredict in arid and semi-arid areas)
        !kc        >  0.0               --> OK
        !alb       >  0.0               --> OK
        !alfapt    != 0                 --> OK
        !Elevation >= 0.0               --> OK
        CALL check_param(valid_petmodel_options,gt_zero,n_kc(4),t_evaporation)
        CALL check_param(valid_petmodel_options,gt_zero,n_alb,t_evaporation)
        CALL check_param(valid_petmodel_options,ne_zero,n_alfapt,t_evaporation)
        CALL check_input(valid_petmodel_options,basin(:)%elev,ge_zero,a_elevation,t_evaporation)
        !Latitude  >= -90.0 AND <= 90.0         --> OK
        !Latitude  MIN() != 0.0 OR MAX() != 0.0 --> OK
        CALL check_input(valid_petmodel_options,basin(:)%latitude,plus_minus_90,a_latitude,t_evaporation)
        r = (MINVAL(basin(:)%latitude) /= 0.0) .OR. (MAXVAL(basin(:)%latitude) /= 0.0)
        CALL add_generic_result(r,0,a_latitude_defined,t_evaporation,'latitude defined')
        valid_petmodel_options = valid_petmodel_options .AND. r
        !tobs, tminobs, tmaxobs
        CALL check_forcing(valid_petmodel_options,i_tobs,a_tobs,t_evaporation)
        CALL check_forcing(valid_petmodel_options,i_tminobs,a_tminobs,t_evaporation)
        CALL check_forcing(valid_petmodel_options,i_tmaxobs,a_tmaxobs,t_evaporation)
        !Depending on forcing data, different cases are ok
        !(rhobs AND swobs) OR krs != 0
        CALL check_forcing(tmp_result(1),i_rhobs,a_rhobs,t_evaporation)
        CALL check_forcing(tmp_result(2),i_swobs,a_swobs,t_evaporation)
        IF (tmp_result(1) .AND. tmp_result(2)) THEN
          valid_petmodel_options = valid_petmodel_options .AND. .TRUE.
        ELSE
          CALL uncheck(a_rhobs,t_evaporation)
          CALL uncheck(a_swobs,t_evaporation)
          CALL check_param(valid_petmodel_options,ne_zero,n_krs,t_evaporation)
        END IF
      CASE(5) !FAO Penman Monteith reference crop evapotranspiration
        !kc        >  0.0               --> OK
        !alb       >  0.0               --> OK
        !Elevation >= 0.0               --> OK
        CALL check_param(valid_petmodel_options,gt_zero,n_kc(5),t_evaporation)
        CALL check_param(valid_petmodel_options,gt_zero,n_alb,t_evaporation)
        CALL check_input(valid_petmodel_options,basin(:)%elev,ge_zero,a_elevation,t_evaporation)
        !Latitude  >= -90.0 AND <= 90.0         --> OK
        !Latitude  MIN() != 0.0 OR MAX() != 0.0 --> OK
        CALL check_input(valid_petmodel_options,basin(:)%latitude,plus_minus_90,a_latitude,t_evaporation)
        r = (MINVAL(basin(:)%latitude) /= 0.0) .OR. (MAXVAL(basin(:)%latitude) /= 0.0)
        CALL add_generic_result(r,0,a_latitude_defined,t_evaporation,'latitude defined')
        valid_petmodel_options = valid_petmodel_options .AND. r
        !tobs, tminobs, tmaxobs
        CALL check_forcing(valid_petmodel_options,i_tobs,a_tobs,t_evaporation)
        CALL check_forcing(valid_petmodel_options,i_tminobs,a_tminobs,t_evaporation)
        CALL check_forcing(valid_petmodel_options,i_tmaxobs,a_tmaxobs,t_evaporation)
        !Depending on forcing data, different cases are ok
        !(rhobs AND swobs) OR krs  != 0
        CALL check_forcing(tmp_result(1),i_rhobs,a_rhobs,t_evaporation)
        CALL check_forcing(tmp_result(2),i_swobs,a_swobs,t_evaporation)
        IF (tmp_result(1) .AND. tmp_result(2)) THEN
          valid_petmodel_options = valid_petmodel_options .AND. .TRUE.
        ELSE
          CALL uncheck(a_rhobs,t_evaporation)
          CALL uncheck(a_swobs,t_evaporation)
          CALL check_param(valid_petmodel_options,ne_zero,n_krs,t_evaporation)
        END IF
        !(uobs AND (roughness > 0,zwind > 0,zwish > 0)) OR mwind > 0
        !zphd could be zero, therefore not tested
        CALL check_forcing(tmp_result(3),i_uobs,a_uobs,t_evaporation)
        IF (tmp_result(3)) THEN
          CALL check_param(valid_petmodel_options,gt_zero,n_roughness,t_evaporation)
          CALL check_param(valid_petmodel_options,gt_zero,n_zwind,t_evaporation)
          CALL check_param(valid_petmodel_options,gt_zero,n_zwish,t_evaporation)
        ELSE
          CALL uncheck(a_uobs,t_evaporation)
          CALL check_param(valid_petmodel_options,gt_zero,n_mwind,t_evaporation)
        END IF
      CASE DEFAULT
        valid_petmodel_options = .FALSE.
    END SELECT
    
  END FUNCTION valid_petmodel_options
  
  !>Check and test actual soil evapotranspiration
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION check_actual_soil_evapotranspiration()

    USE HYPEVARIABLES, ONLY : n_lp, n_ttrig,n_tredA,n_tredB,n_T1evap

    !Argument declaration

    !Local variables
    LOGICAL tmp_logical

    tmp_logical = .TRUE.

    !tredA      >  0.0               --> OK
    CALL check_param(tmp_logical,gt_zero,n_tredA,t_evaporation)
    IF (tmp_logical .EQV. .TRUE.) THEN
        !ttrig      >= 0.0           --> OK
        !tredB      >= 0.0           --> OK
        CALL check_param(tmp_logical,ge_zero,n_ttrig,t_evaporation)
        CALL check_param(tmp_logical,ge_zero,n_tredB,t_evaporation)
    ELSE
        CALL uncheck(n_tredA,t_evaporation)
        tmp_logical = .TRUE.
    END IF

    !lp         >= 0.0, <= 1.0          --> OK
    !T1evap     >= 0.0, <= 1.0          --> OK
    CALL check_param(tmp_logical,ge_zero_and_le_one,n_lp,t_evaporation)
    CALL check_param(tmp_logical,ge_zero_and_le_one,n_T1evap,t_evaporation)

    check_actual_soil_evapotranspiration = tmp_logical

  END FUNCTION check_actual_soil_evapotranspiration

  !>Add process tests
  !>
  !--------------------------------------------------------------------
  SUBROUTINE add_tests(test_id, test_process, test_logical)

    !Argument declaration
    INTEGER, INTENT(IN) :: test_id       !<
    INTEGER, INTENT(IN) :: test_process  !<
    LOGICAL, INTENT(IN) :: test_logical  !<
    !Local variables
    INTEGER i

    DO i = 1, max_testing_values
        IF ((tests_c2_process(i,test_process)%test_id .EQ. -1) .OR. &
            (tests_c2_process(i,test_process)%test_id .EQ. test_id)) THEN
            tests_c2_process(i,test_process)%test_id = test_id
            tests_c2_process(i,test_process)%test_passed = test_logical
            RETURN
        END IF
    END DO

    WRITE(0,*) 'ERROR: Size exceeded, could not add the test', test_id, ', test will not be counted for!'

  END SUBROUTINE add_tests

  !>Clear process tests
  !>
  !--------------------------------------------------------------------
  SUBROUTINE clear_tests(test_process)

    !Argument declaration
    INTEGER, OPTIONAL, INTENT(IN) :: test_process  !< Clear tests for one process if specified
    !Local variables
    INTEGER i,j

    IF (PRESENT(test_process)) THEN
        DO j = 1, max_testing_values
            tests_c2_process(j,test_process)%test_id = -1
            tests_c2_process(j,test_process)%test_passed = .FALSE.
        END DO
        RETURN
    END IF

    DO i = 1, nof_test_processes
        DO j = 1, max_testing_values
            tests_c2_process(j,i)%test_id = -1
            tests_c2_process(j,i)%test_passed = .FALSE.
        END DO
    END DO

  END SUBROUTINE clear_tests

  !>Check parameter
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_param(io_result,tolerance,test_param,test_process)

    USE MODVAR, ONLY : modparid,m_lpar,m_spar,m_gpar, &
                       genpar,landpar,soilpar

    !Argument declaration
    LOGICAL, INTENT(INOUT) :: io_result  !<Input and Output logical for "AND" result
    INTEGER, INTENT(IN) :: tolerance     !<The tolerance to test against
    INTEGER, INTENT(IN) :: test_param    !<The test parameter specifying test case
    INTEGER, INTENT(IN) :: test_process  !<Process assigned to this test case

    !Local variables
    REAL, DIMENSION(:), POINTER :: ptr
    REAL, TARGET :: v(1)
    INTEGER status

    SELECT CASE(modparid(test_param)%deptype)
    CASE(m_lpar)
      ptr => landpar(modparid(test_param)%parno,:)
    CASE(m_spar)
      ptr => soilpar(modparid(test_param)%parno,:)
    CASE(m_gpar)
      v(1) = genpar(modparid(test_param)%parno)
      ptr => v
    CASE DEFAULT
      io_result = .FALSE.
      RETURN
    END SELECT

    IF ((.NOT. all_test_cases(test_param)%tested) .OR. (force_test)) THEN
      all_test_cases(test_param)%passed = real_check_tolerance(ptr,tolerance,status)
      all_test_cases(test_param)%tested = .TRUE.
      all_test_cases(test_param)%kind_of_test = p_parameter
      all_test_cases(test_param)%tolerance = tolerance
      all_test_cases(test_param)%status = status
      all_test_cases(test_param)%modpar_index = test_param
      all_test_cases(test_param)%name = trim(modparid(test_param)%shortname)
    END IF

    !Construct the error string
    IF (.NOT. all_test_cases(test_param)%passed) THEN
      WRITE(all_test_cases(test_param)%errstring,*,ERR=400) trim(all_test_cases(test_param)%name),' tolerance ', &
          all_test_cases(test_param)%tolerance,' error!'
    END IF

    io_result = io_result .AND. all_test_cases(test_param)%passed
    CALL add_tests(test_param, test_process, all_test_cases(test_param)%passed)

    RETURN

    !Error handling
400 WRITE(0,*) 'Test case error (',trim(all_test_cases(test_param)%name),'), writing error string!'

  END SUBROUTINE check_param
  
  !>Print name of a test and print passed or failed
  !>
  !--------------------------------------------------------------------
  SUBROUTINE print_testcase_logical(name_string,return_logical,prefix,suffix)
  
    !Argument declaration
    CHARACTER(LEN=*), INTENT(IN) :: name_string  !<The testcase name
    LOGICAL, INTENT(IN) :: return_logical        !<The testcase returned logical
    CHARACTER(LEN=*), OPTIONAL :: prefix         !<Specify a prefix string if wanted"
    CHARACTER(LEN=*), OPTIONAL :: suffix         !<Specify a suffix string otherwise it becomes "with parameter, ..."
    
    !Local variables
    CHARACTER(LEN=128) :: text

    text = TRIM(name_string)
    IF (PRESENT(prefix)) THEN
      text = TRIM(prefix) // TRIM(name_string)
    END IF

    IF (PRESENT(suffix)) THEN
      text = TRIM(text) // TRIM(suffix)
    ELSE
      text = TRIM(text) // TRIM(' with parameter, input and forcing data')
    END IF
    
    IF (return_logical) THEN
      WRITE(funit_used,*) '[Passed] ', TRIM(text)
    ELSE
      WRITE(funit_used,*) '[Failed] ', TRIM(text)
    END IF
    
  END SUBROUTINE print_testcase_logical
  
  !>Print parameters,input and forcing for a test process
  !>Display failures if any
  !>
  !--------------------------------------------------------------------
  SUBROUTINE print_process_testcase(process)

    !Argument declaration
    INTEGER, INTENT(IN) :: process  !<The process

    !Local variables
    INTEGER t,i,failures
    CHARACTER(LEN=32) tolerance_name

    !Print out more information about the tests
    IF (printout_level > 1) THEN
        failures = 0
        DO i = 1, SIZE(tests_c2_process(:,process))
            IF (tests_c2_process(i,process)%test_id .GT. 0) THEN
                t = all_test_cases(tests_c2_process(i,process)%test_id)%tolerance
                IF (t > 0 .AND. t <= nof_test_tolerances) THEN
                    tolerance_name = testing_tolerances(t)
                ELSE
                    tolerance_name = '-'
                END IF
                WRITE(funit_used,*) tests_c2_process(i,process)%test_id, all_test_cases(tests_c2_process(i,process)%test_id)%name, &
                ' passed:',tests_c2_process(i,process)%test_passed,TRIM(tolerance_name)
                IF (.NOT. tests_c2_process(i,process)%test_passed) THEN
                    failures = failures + 1;
                END IF
            END IF
        END DO
        IF (failures .GT. 0) THEN
           WRITE(funit_used,*) 'Tested with number of failures: ',failures
        END IF
    END IF

    !Extended the printouts further
    IF (printout_level > 2) THEN
        DO i = 1, SIZE(tests_c2_process(:,process))
            IF (tests_c2_process(i,process)%test_id .GT. 0) THEN
                CALL print_extended(tests_c2_process(i,process)%test_id)
            END IF
        END DO
    END IF

  END SUBROUTINE print_process_testcase

  !>Print extended
  !>
  !--------------------------------------------------------------------
  SUBROUTINE print_extended(test_id)

    !Argument declaration
    INTEGER, INTENT(IN) :: test_id  !<The test id

    !Local variables

    SELECT CASE(all_test_cases(test_id)%kind_of_test)
    CASE(p_parameter)
      CALL print_param(all_test_cases(test_id)%modpar_index)
    CASE(p_input)
      CALL print_input(all_test_cases(test_id)%name)
    CASE(p_forcing)
      CALL print_forcing(all_test_cases(test_id)%name)
    CASE(p_generic)
      CALL print_generic(all_test_cases(test_id)%name)
    CASE DEFAULT
      RETURN
    END SELECT

  END SUBROUTINE print_extended
  
  !>Print parameter
  !>
  !--------------------------------------------------------------------
  SUBROUTINE print_param(modpar_index)
  
    USE MODVAR, ONLY : modparid,m_lpar,m_spar,m_gpar,m_rpar, &
                       genpar,landpar,soilpar,regpar
    
    !Argument declaration
    INTEGER, INTENT(IN) :: modpar_index  !<The parameter modpar index
    
    !Local variables
    REAL, DIMENSION(:), POINTER :: ptr
    REAL, TARGET :: v(1)
    
    SELECT CASE(modparid(modpar_index)%deptype)
    CASE(m_lpar)
      ptr => landpar(modparid(modpar_index)%parno,:)
    CASE(m_spar)
      ptr => soilpar(modparid(modpar_index)%parno,:)
    CASE(m_rpar)
      ptr => regpar(modparid(modpar_index)%parno,:)
    CASE(m_gpar)
      v(1) = genpar(modparid(modpar_index)%parno)
      ptr => v
    CASE DEFAULT
      RETURN
    END SELECT
    
    WRITE(funit_used,*) trim(modparid(modpar_index)%shortname), ':', ptr
    
  END SUBROUTINE print_param
  
  !>Check input
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_input(io_result,vector,tolerance,test_param,test_process,force)

    !Argument declaration
    LOGICAL, INTENT(INOUT) :: io_result       !<Input and Output logical for "AND" result
    REAL, DIMENSION(:), INTENT(IN) :: vector  !<The data vector to be tested
    INTEGER, INTENT(IN) :: tolerance          !<The tolerance to test against
    INTEGER, INTENT(IN) :: test_param         !<The test parameter specifying test case
    INTEGER, INTENT(IN) :: test_process       !<Process assigned to this test case
    LOGICAL, OPTIONAL, INTENT(IN) :: force    !<force the test eventhough already tested

    !Local variables
    INTEGER status
    LOGICAL test_forced

    test_forced = force_test
    IF (PRESENT(force)) THEN
      test_forced = force_test
    END IF

    IF ((test_param .LE. n_max_par) .OR. (test_param .GT. max_testing)) THEN
      WRITE(*,*) 'ERROR, test input parameter could not be tested!', test_param
      io_result = .FALSE.
      RETURN
    END IF

    IF ((.NOT. all_test_cases(test_param)%tested) .OR. (test_forced)) THEN
      all_test_cases(test_param)%passed = real_check_tolerance(vector,tolerance,status)
      all_test_cases(test_param)%tested = .TRUE.
      all_test_cases(test_param)%kind_of_test = p_input
      all_test_cases(test_param)%tolerance = tolerance
      all_test_cases(test_param)%status = status
      all_test_cases(test_param)%name = testing_names(test_param-n_max_par)
    END IF

    io_result = io_result .AND. all_test_cases(test_param)%passed
    CALL add_tests(test_param, test_process, all_test_cases(test_param)%passed)

  END SUBROUTINE check_input
  
  !>Print input
  !>
  !--------------------------------------------------------------------
  SUBROUTINE print_input(name)
    
    !Argument declaration
    CHARACTER(LEN=*), INTENT(IN) :: name      !<The name
    
    WRITE(funit_used,*) trim(name), ': incorporated'
    
  END SUBROUTINE print_input
  
  !>Check forcing
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_forcing(io_result,force_param,test_param,test_process)

    USE WORLDVAR, ONLY : forcingdata

    !Argument declaration
    LOGICAL, INTENT(INOUT) :: io_result  !<Input and Output logical for "AND" result
    INTEGER, INTENT(IN) :: force_param   !<The forcing parameter to test
    INTEGER, INTENT(IN) :: test_param    !<The test parameter specifying test case
    INTEGER, INTENT(IN) :: test_process  !<Process assigned to this test case

    !Local variables

    IF ((test_param .LE. n_max_par) .OR. (test_param .GT. max_testing)) THEN
      WRITE(*,*) 'ERROR, test forcing parameter could not be tested!', test_param
      io_result = .FALSE.
      RETURN
    END IF

    IF ((.NOT. all_test_cases(test_param)%tested) .OR. (force_test)) THEN
      all_test_cases(test_param)%passed = forcingdata(force_param)%readfile
      all_test_cases(test_param)%tested = .TRUE.
      all_test_cases(test_param)%kind_of_test = p_forcing
      all_test_cases(test_param)%tolerance = 0
      all_test_cases(test_param)%status = 0
      all_test_cases(test_param)%name = testing_names(test_param-n_max_par)
    END IF

    io_result = io_result .AND. all_test_cases(test_param)%passed
    CALL add_tests(test_param, test_process, all_test_cases(test_param)%passed)

  END SUBROUTINE check_forcing
  
  !>Print forcing
  !>
  !--------------------------------------------------------------------
  SUBROUTINE print_forcing(name)
    
    !Argument declaration
    CHARACTER(LEN=*), INTENT(IN) :: name      !<The name
    
    WRITE(funit_used,*) trim(name), ': incorporated'
    
  END SUBROUTINE print_forcing

  !>Check file existence
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_file_existence(io_result,filename,test_param,test_process)

    USE WORLDVAR, ONLY : modeldir

    !Argument declaration
    LOGICAL, INTENT(INOUT)       :: io_result     !<Input and Output logical for "AND" result
    CHARACTER(LEN=*), INTENT(IN) :: filename      !<The filename to test
    INTEGER, INTENT(IN)          :: test_param    !<The test parameter specifying test case
    INTEGER, INTENT(IN)          :: test_process  !<Process assigned to this test case

    !Local variables
    LOGICAL result

    INQUIRE(FILE=TRIM(modeldir)//TRIM(filename),EXIST=result)
    CALL add_generic_result(result,0,test_param,test_process,TRIM(filename)//' exist')
    io_result = io_result .AND. result

  END SUBROUTINE check_file_existence

  !>Add generic result
  !>
  !--------------------------------------------------------------------
  SUBROUTINE add_generic_result(result,status,test_param,test_process,name)

    !Argument declaration
    LOGICAL, INTENT(IN) :: result        !<Input logical for result
    INTEGER, INTENT(IN) :: status        !<The status of the test
    INTEGER, INTENT(IN) :: test_param    !<The test parameter specifying test case
    INTEGER, INTENT(IN) :: test_process  !<Process assigned to this test case
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: name !<Name to describe the generic test to show when printed

    !Local variables

    IF ((test_param .LE. n_max_par) .OR. (test_param .GT. max_testing)) THEN
      WRITE(*,*) 'ERROR, generic parameter could not be added to test!', test_param
      RETURN
    END IF

    all_test_cases(test_param)%passed = result
    all_test_cases(test_param)%tested = .TRUE.
    all_test_cases(test_param)%kind_of_test = p_generic
    all_test_cases(test_param)%tolerance = 0
    all_test_cases(test_param)%status = status

    IF (PRESENT(name)) THEN
      all_test_cases(test_param)%name = trim(name)
    ELSE
      all_test_cases(test_param)%name = '?'
    END IF

    CALL add_tests(test_param, test_process, all_test_cases(test_param)%passed)

  END SUBROUTINE add_generic_result

  !>Print generic
  !>
  !--------------------------------------------------------------------
  SUBROUTINE print_generic(name)

    !Argument declaration
    CHARACTER(LEN=*), INTENT(IN) :: name      !<The name

    !WRITE(funit_used,*) trim(name), ': used'

  END SUBROUTINE print_generic

  !>Uncheck
  !>
  !--------------------------------------------------------------------
  SUBROUTINE uncheck(test_param,test_process)

    !Argument declaration
    INTEGER, INTENT(IN) :: test_param    !<The test parameter specifying test case
    INTEGER, INTENT(IN) :: test_process  !<Process assigned to this test case

    !Local variables
    INTEGER :: i,remove_index,substitute_index

    remove_index = -1
    substitute_index = 0
    LOOP : DO i = 1, max_testing_values
        IF (tests_c2_process(i,test_process)%test_id .EQ. test_param) THEN
            remove_index = i
        ELSE IF (tests_c2_process(i,test_process)%test_id .GT. 0) THEN
            substitute_index = i
        ELSE
            EXIT LOOP
        END IF
    END DO LOOP

    IF (remove_index .GT. 0) THEN
        tests_c2_process(remove_index,test_process)%test_id = -1
        tests_c2_process(remove_index,test_process)%test_passed = .FALSE.
        IF ((substitute_index .GT. 0) .AND. ((substitute_index .GT. remove_index))) THEN
            tests_c2_process(remove_index,test_process)%test_id = tests_c2_process(substitute_index,test_process)%test_id
            tests_c2_process(remove_index,test_process)%test_passed = tests_c2_process(substitute_index,test_process)%test_passed
            tests_c2_process(substitute_index,test_process)%test_id = -1
            tests_c2_process(substitute_index,test_process)%test_passed = .FALSE.
        END IF
    END IF

  END SUBROUTINE uncheck

  !>Check tolerance real
  !>Real
  !--------------------------------------------------------------------
  LOGICAL FUNCTION real_check_tolerance(vector,tolerance,status)
  
    !Argument declaration
    REAL, DIMENSION(:), INTENT(IN) :: vector     !<The vector to be tested
    INTEGER, INTENT(IN)            :: tolerance  !<The tolerance parameter
    INTEGER, INTENT(OUT)           :: status     !<The returned status

    !Local variables
    LOGICAL retval,needed_retval
    INTEGER needed_status
    REAL aimed_value,tolerance_min,tolerance_max
    
    SELECT CASE(tolerance)
    CASE(eq_zero)
      aimed_value = 0.0
      tolerance_min = 0.0
      tolerance_max = 0.0
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(ne_zero)
      aimed_value = 0.0
      tolerance_min = 0.0
      tolerance_max = 0.0
      needed_retval = .FALSE.
      needed_status = 0
    CASE(gt_zero)
      aimed_value = 0.0
      tolerance_min = TINY(0.0)
      tolerance_max = HUGE(0.0)
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(lt_zero)
      aimed_value = 0.0
      tolerance_min = -HUGE(0.0)
      tolerance_max = -TINY(0.0)
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(ge_zero)
      aimed_value = 0.0
      tolerance_min = 0.0
      tolerance_max = HUGE(0.0)
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(le_zero)
      aimed_value = 0.0
      tolerance_min = -HUGE(0.0)
      tolerance_max = 0.0
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(gt_zero_and_lt_one)
      aimed_value = 0.0
      tolerance_min = TINY(0.0)
      tolerance_max = 1.0-TINY(0.0)
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(ge_zero_and_lt_one)
      aimed_value = 0.0
      tolerance_min = 0.0
      tolerance_max = 1.0-TINY(0.0)
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(gt_zero_and_le_one)
      aimed_value = 0.0
      tolerance_min = TINY(0.0)
      tolerance_max = 1.0
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(ge_zero_and_le_one)
      aimed_value = 0.0
      tolerance_min = 0.0
      tolerance_max = 1.0
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(gt_minus_one_and_lt_plus_one)
      aimed_value = 0.0
      tolerance_min = -1.0+TINY(0.0)
      tolerance_max = 1.0-TINY(0.0)
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(gt_minus_one)
      aimed_value = 0.0
      tolerance_min = -1.0+TINY(0.0)
      tolerance_max = HUGE(0.0)
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(ge_minus_one)
      aimed_value = 0.0
      tolerance_min = -1.0
      tolerance_max = HUGE(0.0)
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(plus_minus_90)
      aimed_value = 0.0
      tolerance_min = -90.0
      tolerance_max = 90.0
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(plus_minus_180)
      aimed_value = 0.0
      tolerance_min = -180.0
      tolerance_max = 180.0
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(one_plus_minus_5percent)
      aimed_value = 1.0
      tolerance_min = -0.05
      tolerance_max = 0.05
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE DEFAULT
      !Warning, return false as default
      real_check_tolerance = .FALSE.
      RETURN
    END SELECT
    
    retval = data_is_within_tolerance(vector,aimed_value,tolerance_min,tolerance_max,status)
    IF ((retval .EQV. needed_retval) .AND. (status .EQ. needed_status)) THEN
      real_check_tolerance = .TRUE.
    ELSE
      real_check_tolerance = .FALSE.
    END IF
  
  END FUNCTION real_check_tolerance
  
  
  !>Check that data differs from value
  !>Integer
  !--------------------------------------------------------------------
  LOGICAL FUNCTION int_data_differs_from_value(vector,value)

	!Argument declaration
    INTEGER, DIMENSION(:), INTENT(IN)  :: vector  !<Vector that is being verified
    INTEGER, INTENT(IN)                :: value   !<Value specifying a NOT wanted value
    
    !Local variables
    LOGICAL ret
    INTEGER num
    
    ret = data_is_within_tolerance(vector,value,0,0,num)
    IF ((.NOT. ret) .AND. (num .EQ. 0)) THEN
      int_data_differs_from_value = .TRUE.
    ELSE
      int_data_differs_from_value = .FALSE.
    END IF

  END FUNCTION int_data_differs_from_value
  
  !>Check that data differs from value
  !>Real
  !--------------------------------------------------------------------
  LOGICAL FUNCTION real_data_differs_from_value(vector,value)

	!Argument declaration
    REAL, DIMENSION(:), INTENT(IN)  :: vector  !<Vector that is being verified
    REAL, INTENT(IN)                :: value   !<Value specifying a NOT wanted value
    
    !Local variables
    LOGICAL ret
    INTEGER num
    
    ret = data_is_within_tolerance(vector,value,0.0,0.0,num)
    IF ((.NOT. ret) .AND. (num .EQ. 0)) THEN
      real_data_differs_from_value = .TRUE.
    ELSE
      real_data_differs_from_value = .FALSE.
    END IF

  END FUNCTION real_data_differs_from_value
  
  !>Check that data is within tolerance
  !>Integer
  !--------------------------------------------------------------------
  LOGICAL FUNCTION int_data_is_within_tolerance(vector,aimed_value,tolerance_min,tolerance_max,num_tolerance)

	!Argument declaration
    INTEGER, DIMENSION(:), INTENT(IN)     :: vector  !<Data vector that is being checked
    INTEGER, INTENT(IN)     :: aimed_value    !<Value specifying a wanted value
    INTEGER, INTENT(IN)     :: tolerance_min  !<Value specifying how far below wanted value is ok 
    INTEGER, INTENT(IN)     :: tolerance_max  !<Value specifying how far above wanted value is ok
    INTEGER, INTENT(OUT)    :: num_tolerance  !<Value specifying how many values in vector is within tolerance
    
    !Local variables
    INTEGER k
    
    num_tolerance = 0
    DO k = 1, SIZE(vector)
      IF ((vector(k)-aimed_value) .GE. tolerance_min .AND. &
          (vector(k)-aimed_value) .LE. tolerance_max) THEN
        num_tolerance = num_tolerance + 1
      END IF
    END DO
    
    IF (SIZE(vector) .EQ. num_tolerance) THEN
      int_data_is_within_tolerance = .TRUE.
    ELSE
      int_data_is_within_tolerance = .FALSE.
    END IF

  END FUNCTION int_data_is_within_tolerance

  !>Check that data is within tolerance
  !>Real
  !--------------------------------------------------------------------
  LOGICAL FUNCTION real_data_is_within_tolerance(vector,aimed_value,tolerance_min,tolerance_max,num_tolerance)

	!Argument declaration
    REAL, DIMENSION(:), INTENT(IN)     :: vector  !<Data vector that is being checked
    REAL, INTENT(IN)      :: aimed_value    !<Value specifying a wanted value
    REAL, INTENT(IN)      :: tolerance_min  !<Value specifying how far below wanted value is ok 
    REAL, INTENT(IN)      :: tolerance_max  !<Value specifying how far above wanted value is ok
    INTEGER, INTENT(OUT)  :: num_tolerance  !<Value specifying how many values in vector is within tolerance
    
    !Local variables
    INTEGER k
    
    num_tolerance = 0
    DO k = 1, SIZE(vector)
      IF ((vector(k)-aimed_value) .GE. tolerance_min .AND. &
          (vector(k)-aimed_value) .LE. tolerance_max) THEN
        num_tolerance = num_tolerance + 1
      END IF
    END DO
    
    IF (SIZE(vector) .EQ. num_tolerance) THEN
      real_data_is_within_tolerance = .TRUE.
    ELSE
      real_data_is_within_tolerance = .FALSE.
    END IF

  END FUNCTION real_data_is_within_tolerance

  !>find petmodels used
  !--------------------------------------------------------------------
  INTEGER FUNCTION find_petmodels_used(petmodels_used, petmodels_used_size)
  
    USE MODVAR, ONLY : maxmodelsinoption, &
                       petmodel,          &
                       modeloption,       &
                       p_petmodel

	!Argument declaration
    INTEGER, DIMENSION(:), INTENT(OUT)  :: petmodels_used       !<will hold models found in basins petmodel array
    INTEGER, INTENT(IN)                 :: petmodels_used_size  !<size of the petmodels_used array
    
    !Local variables
    INTEGER i, j
    
    find_petmodels_used = 0
    IF (petmodels_used_size .LE. 0 .OR. petmodels_used_size .GT. maxmodelsinoption(p_petmodel)) THEN
      find_petmodels_used = -1
    ELSE IF (modeloption(p_petmodel) .GT. 0) THEN
      find_petmodels_used = 1
      petmodels_used(find_petmodels_used) = modeloption(p_petmodel)
    ELSE IF (ALLOCATED(petmodel)) THEN
      find_petmodels_used = 1;
      petmodels_used(find_petmodels_used) = petmodel(1)
      DO i = 1, SIZE(petmodel)
        INNER_LOOP : DO j = 1, find_petmodels_used
          IF (petmodels_used(j) .EQ. petmodel(i)) THEN
            !break inner loop if found in petmodels_used
            EXIT INNER_LOOP
          ELSE IF (petmodels_used(j) .NE. petmodel(i) .AND. j .EQ. find_petmodels_used) THEN
            !if j is last element and petmodel index is not found there, add it.
            !be sure though that we do not exceed petmodels_used array to do so.
            IF (find_petmodels_used .LE. maxmodelsinoption(p_petmodel)) THEN
              find_petmodels_used = find_petmodels_used + 1
              petmodels_used(find_petmodels_used) = petmodel(i)
            END IF
            EXIT INNER_LOOP
          END IF
        END DO INNER_LOOP
      END DO
    END IF
  
  END FUNCTION find_petmodels_used

  !>Log_petmodels
  !--------------------------------------------------------------------
  SUBROUTINE log_petmodels_used(funit,petmodels_used,num_petmodels_used)
  
    USE MODVAR, ONLY : maxmodelsinoption, &
                       petmodelnames,     &
                       modeloptionname,   &
                       p_petmodel

	!Argument declaration
    INTEGER, INTENT(IN)                :: funit               !<File unit of log-file
    INTEGER, DIMENSION(:), INTENT(IN)  :: petmodels_used      !<will hold petmodels used
    INTEGER, INTENT(IN)                :: num_petmodels_used  !<number of used petmodels
    
    !Local variables
    INTEGER i

    !log all PETmodels used
    IF (SIZE(petmodels_used) .GE. num_petmodels_used) THEN
      WRITE(funit, '(A,A2)', ADVANCE='NO') TRIM(modeloptionname(p_petmodel)), ': '
      DO i = 1, num_petmodels_used
        IF (petmodels_used(i)+1 .LE. maxmodelsinoption(p_petmodel)) THEN
          IF (i .GT. 1) THEN
            WRITE(funit, '(A2)', ADVANCE='NO') ', '
          END IF
          WRITE(funit, '(A1,I1,A1,A)', ADVANCE='NO') '(', petmodels_used(i), ')', TRIM(petmodelnames(petmodels_used(i)))
        END IF
      END DO
      WRITE(funit, *)
    END IF
  
  END SUBROUTINE log_petmodels_used
  
  !>Check input data from info, GeoData, BranchData, GeoClass, par and
  !>CropData files
  !--------------------------------------------------------------------
  SUBROUTINE checkindata_part1(funit)

    USE WORLDVAR, ONLY : bdate,ndt,   &
                         sdate,       &
                         checkdata,   &
                         modeldir,    &
                         fileunit_temp, &
                         comment_str, &
                         vegtypereset, &
                         output,noutput
    USE MODVAR, ONLY : basin,         &
                       classbasin,    &
                       classdata,     &
                       cropdata,      &
                       classmodel,    &
                       slc_olake,     &
                       load,          &
                       pathsubid,     &
                       branchsubid,   &
                       max_subid, &
                       petmodel, &
                       soildepth,     &
                       nregions,      &
                       nclass,        &
                       nluse,nsoil,   &
                       conduct, &
                       nsub_basemodel, &
                       outvarid, &
                       modparid, &
                       modeloption, &
                       max_par, &
                       regiondivision, &
                       m_gpar,m_bpar, &
                       m_spar,m_lpar, &
                       m_rpar, &
                       m_ldpar,m_mpar, &
                       p_growthstart, &
                       p_erosion, &
                       p_petmodel

	!Argument declaration
    INTEGER, INTENT(IN)    :: funit    !<File unit of log-file

    !Local variables
    REAL    basinarea
    REAL    classtest
    REAL    basintest
    INTEGER subidproc,classproc
    INTEGER i,k,bdim,ksource,kbranch
    INTEGER j,io,ivar,ivar2
    INTEGER help1,help2,help3,help4,help5
    INTEGER dim
    INTEGER dimcheck                !current parameter supposed size
    INTEGER ncomment                !number of lines with comments
    INTEGER nline                   !line number in file
    INTEGER nvalues                 !number of values read from line
    INTEGER varindex                !parameter index in array
    LOGICAL found
    CHARACTER (LEN=220) filename
    CHARACTER (LEN=10) varstr       !string with variable name
    CHARACTER(LEN=18000) line
    REAL,ALLOCATABLE :: values(:)               !values of parameter read from file

    WRITE(funit,*)
    WRITE(funit,*) 'Check indata part one'
    WRITE(funit,*) '---------------------'
    
    !Check simulation period
    WRITE(funit,*) 'Checking info.txt...'
    IF(ndt<=0) THEN
      WRITE(funit,*) 'Number of timesteps less than one. Check bdate and edate in info.txt'
      checkdata(1,2) = .TRUE.
    ENDIF
    IF(bdate%day<1.OR.bdate%day>31) WRITE(funit,*) 'Check bdate day in info.txt'
    IF(bdate%month<1.OR.bdate%month>12) WRITE(funit,*) 'Check bdate month in info.txt'
    IF(sdate%day<1.OR.sdate%day>31) WRITE(funit,*) 'Check edate day in info.txt'
    IF(sdate%month<1.OR.sdate%month>12) WRITE(funit,*) 'Check edate month in info.txt'
    DO io = 1, noutput
      IF(output(io)%fileformat==2 .OR. output(io)%fileformat==3)THEN
        DO ivar = 1,output(io)%nvar
          DO ivar2 = ivar+1,output(io)%nvar
            IF(outvarid(output(io)%variable(ivar)%idindex)%shortname==outvarid(output(io)%variable(ivar2)%idindex)%shortname &
                 .AND. output(io)%variable(ivar)%areaagg==output(io)%variable(ivar2)%areaagg)THEN
              WRITE(funit,*) 'Error: Same variable multiple times in output. Variable: '//TRIM(outvarid(output(io)%variable(ivar)%idindex)%shortname)
              WRITE(funit,*) 'Check timeoutput and/or mapoutput.'
              checkdata(1,2) = .TRUE.
            ENDIF
          ENDDO
        ENDDO
      ENDIF
    ENDDO
    
    !Check GeoData
    WRITE(funit,*) 'Checking GeoData.txt...'
    basinarea = SUM(basin(:)%area)
    IF(basinarea<=0.)THEN 
      WRITE(funit,*) 'Error: No area of catchment. Check GeoData.txt.'
      checkdata(1,2) = .TRUE.
    ENDIF
    basinarea = MINVAL(basin(:)%area)
    IF(basinarea<=0.)THEN
      DO i = 1,nsub_basemodel
        IF(basin(i)%area<=0.)THEN
          WRITE(funit,*) 'Subbasin',basin(i)%subid,'has area',basin(i)%area,'.'
          IF(SUM(classbasin(i,:)%part)/=0.)THEN
            WRITE(funit,*) 'and its sum of slc-fraction is not zero (Error).'
            checkdata(1,2) = .TRUE.
          ENDIF
        ENDIF  
      ENDDO
    ENDIF
    subidproc = MINVAL(basin(:)%subid)
    IF(subidproc==0)THEN
      WRITE(funit,*) 'Error: Some subid are zero. Check GeoData.txt.'
      checkdata(1,2)=.TRUE.
    ENDIF
    subidproc = MAXVAL(basin(:)%subid)
    IF(subidproc>max_subid)THEN
      WRITE(funit,*) 'Error: Subid is too large. Maximum ',max_subid,' may be used. Check GeoData.txt.'
      checkdata(1,2)=.TRUE.
    ENDIF
    basintest = MINVAL(basin(:)%slope)
    IF(basintest<0)THEN
      WRITE(funit,*) 'Error: Some slope is less than zero. Check GeoData.txt.'
      checkdata(1,2)=.TRUE.
    ENDIF
    IF(conduct%simN.OR.conduct%simP.OR.conduct%simC) THEN
      IF(nregions(6)<=0)THEN
        WRITE(funit,*) 'Error: Max value of lake regions is zero. Lake regions are necessary for'
        WRITE(funit,*) 'NPC simulation. Check GeoData.txt.'
        checkdata(1,2)=.TRUE.
      ELSE
        subidproc = MINVAL(basin(:)%parregion(6))
        IF(subidproc<=0)THEN
          WRITE(funit,*) 'Error: Some lake regions are not set. Lake regions are necessary for NPC simulation. Check GeoData.txt.'
          checkdata(1,2)=.TRUE.
        ENDIF
      ENDIF
    ENDIF  
    IF(MAXVAL(nregions)<=0)THEN
      WRITE(funit,*) 'Max value of parameter regions is zero. Check GeoData.txt.'
    ENDIF
    IF(conduct%simN.OR.conduct%simP) THEN
      IF(nregions(3)<=0)THEN
        WRITE(funit,*) 'Max value of water quality parameter regions is zero. Check GeoData.txt.'
      ENDIF
      subidproc = MINVAL(basin(:)%parregion(3))
      IF(subidproc<=0) WRITE(funit,*) 'Some water quality parameter regions are not set. Check GeoData.txt.'
    ENDIF
    subidproc = MINVAL(basin(:)%parregion(1))
    IF(subidproc<=0) WRITE(funit,*) 'Some parameter regions are not set. Check GeoData.txt.'
    subidproc = MINVAL(basin(:)%region)
    IF(subidproc<=0)THEN
      WRITE(funit,*) 'Some regions are not set. Regions are necessary for NP simulation'
      WRITE(funit,*) '(and irrigation). Check GeoData.txt.'
    ENDIF
    basinarea=SUM(basin(:)%rivlen(2))
    IF(basinarea<=0) WRITE(funit,*) 'No main rivers? Check GeoData.txt (rivlen).'
    basinarea=MINVAL(basin(:)%rivlen(2))
    IF(basinarea<0) WRITE(funit,*) 'Negative length of some rivers. That is not allowed. Check GeoData.txt'
    !Check slc-fractions
    DO i = 1,nsub_basemodel
      IF(SUM(classbasin(i,:)%part)<0.99 .OR. SUM(classbasin(i,:)%part)>1.01)THEN
        WRITE(funit,'(A9,I7,A51)') 'Subbasin ',basin(i)%subid,' has sum of class-fractions deviating more than 1%.'
      ELSEIF(SUM(classbasin(i,:)%part)<0.9999 .OR. SUM(classbasin(i,:)%part)>1.0001)THEN
        WRITE(funit,'(A9,I7,A54)') 'Subbasin ',basin(i)%subid,' has sum of class-fractions deviating more than 0.01%.'
      ENDIF
      IF(MINVAL(classbasin(i,:)%part)<0.)THEN
        WRITE(funit,'(A9,I7,A36)') 'Subbasin ',basin(i)%subid,' has class-fractions less than zero.'
        WRITE(funit,'(A38)') 'This is not allowed. Check GeoData.txt'
        checkdata(1,2) = .TRUE.
      ENDIF
    ENDDO
    !Check olake depth
    IF(slc_olake>0)THEN
      DO i = 1,nsub_basemodel
        IF(classbasin(i,slc_olake)%part>0. .AND. basin(i)%lakedepth(2)<=0.)THEN
          WRITE(funit,'(A9,I7,A65)') 'Subbasin ',basin(i)%subid,' has olake with depth zero. This is not allowed, model may crash.'
          WRITE(funit,*) 'Check GeoData.txt, LakeData.txt and/or DamData.txt'
          WRITE(funit,*) 'If lakedepth set in par.txt (gldepo>0) then ignore this warning.'
        ENDIF
      ENDDO
    ENDIF
    !Check abstractions
    DO i = 1,nsub_basemodel
      IF(load(i)%abstrvol>0)THEN
        IF(load(i)%abstrind==0)THEN 
          WRITE(funit,*) 'Warning: Abstraction of water without defined source for subbasin ',basin(i)%subid
        ELSEIF(load(i)%abstrind==1)THEN 
          IF(basin(i)%rivlen(2)==0.)THEN
            WRITE(funit,*) 'Warning: Abstraction of water from main river of length 0 for subbasin ',basin(i)%subid
          ENDIF
        ELSEIF(load(i)%abstrind==2)THEN 
          IF(slc_olake==0)THEN
            WRITE(funit,*) 'Abstraction of water from outlet lake for subbasin ',basin(i)%subid, 'but no slc-class for olake defined.'
            checkdata(1,2) = .TRUE.
          ELSEIF(classbasin(i,slc_olake)%part==0.)THEN
            WRITE(funit,*) 'Abstraction of water from outlet lake for subbasin',basin(i)%subid, 'without outlet lake.'
            checkdata(1,2) = .TRUE.
          ENDIF
        ENDIF
      ENDIF
    ENDDO
    !Check linkage and subbasin order
    WRITE(funit,*) 'Checking subbasin linkage...'
    DO i = 1,nsub_basemodel
      DO k = i-1,1,-1
        IF(basin(k)%subid==pathsubid(i)%main)THEN
          WRITE(funit,*) 'Linking error: main branch (',pathsubid(i)%main,') of subbasin',basin(i)%subid,'comes before subbasin in GeoData.txt.'
          checkdata(1,2) = .TRUE.
        ENDIF
      ENDDO
      found = .FALSE.
      IF(pathsubid(i)%main<=0) found = .TRUE.
      IF(.NOT.found)THEN
        DO k = i+1,nsub_basemodel
          IF(pathsubid(i)%main==basin(k)%subid)THEN
            found=.TRUE.
            EXIT
          ENDIF  
        ENDDO
      ENDIF
      IF(.NOT.found) WRITE(funit,*) 'Main branch (',pathsubid(i)%main,') of subbasin',basin(i)%subid,' not found. Check GeoData.txt'
    ENDDO
    IF(ALLOCATED(branchsubid))THEN
      bdim=SIZE(branchsubid(:)%source)
      DO i = 1,bdim
        ksource = 0
        kbranch = 0
        DO k = 1,nsub_basemodel
          IF(branchsubid(i)%source==basin(k)%subid)  ksource = k
          IF(branchsubid(i)%branch==basin(k)%subid)  kbranch = k
        ENDDO
        IF(ksource==0) WRITE(funit,*) 'Branch from subid',branchsubid(i)%source,' not in model. Check BranchData.txt.'
        IF(kbranch==0)THEN
          IF(branchsubid(i)%branch>0)  WRITE(funit,*) 'Branch (',branchsubid(i)%branch,') of subbasin',branchsubid(i)%source,' not found. Check BranchData.txt.'
        ELSE
          IF(kbranch<ksource)THEN
            WRITE(funit,*) 'Linking error: Branch (',branchsubid(i)%branch,') of subbasin'
            WRITE(funit,*) branchsubid(i)%source,' lies upstream subbasin. Check BranchData.txt.'
            checkdata(1,2) = .TRUE.
          ENDIF
        ENDIF  
      ENDDO  
    ENDIF
    IF(modeloption(p_growthstart)==1)THEN
      basinarea=SUM(basin(:)%latitude)
      IF(basinarea<=0) WRITE(funit,*) 'Latitude is needed for using growth start model 1. No latitude given. Check GeoData.txt (latitude)?'
    ENDIF
    IF(ALLOCATED(petmodel))THEN
      classproc = MINVAL(petmodel)
      IF(classproc<0)THEN
        WRITE(funit,*) 'Error: Illegal value of petmodel in GeoData.txt. Must be 0-5.'
        checkdata(1,2) = .TRUE.
      ENDIF
      classproc = MAXVAL(petmodel)
      IF(classproc>5)THEN
        WRITE(funit,*) 'Error: Illegal value of petmodel in GeoData.txt. Must be 0-5.'
        checkdata(1,2) = .TRUE.
      ENDIF
    ELSEIF(modeloption(p_petmodel)<0.OR.modeloption(p_petmodel)>5)THEN
      WRITE(funit,*) 'Error: Modeloption petmodel must be 0-5. Check info.txt'
      checkdata(1,2) = .TRUE.
    ENDIF
    
    !Check GeoClass
    WRITE(funit,*) 'Checking GeoClass.txt...'
    IF(nclass<=0)THEN
      WRITE(funit,*) 'Error: Zero classes. Check GeoClass.txt'
      checkdata(1,2) = .TRUE.
    ENDIF
    classproc=MINVAL(classdata(:)%luse)
    IF(classproc<=0)THEN
      WRITE(funit,*) 'Error: No land use code given. Check GeoClass.txt'
      checkdata(1,2) = .TRUE.
    ENDIF
    classproc=MINVAL(classdata(:)%soil)
    IF(classproc<=0)THEN
      WRITE(funit,*) 'Error: No soil type code given. Check GeoClass.txt'
      checkdata(1,2) = .TRUE.
    ENDIF
    help1=0;help2=0;help3=0;help4=0;help5=0
    DO i=1,nclass
      IF(classmodel(i)==0)THEN
      ELSEIF(classmodel(i)==1)THEN
        help1 = help1 + 1
      ELSEIF(classmodel(i)==2)THEN
        help2 = help2 + 1
      ELSEIF(classmodel(i)==3)THEN
        help3 = help3 + 1
      ELSEIF(classmodel(i)==11)THEN
        help4 = help4 + 1
      ELSEIF(classmodel(i)==12)THEN
        help5 = help5 + 1
      ELSE
        WRITE(funit,*) 'Unknown information in special class column for slc-class',i,'. Check GeoClass.txt.'
      ENDIF
    ENDDO
    IF(help1>1)THEN
      WRITE(funit,*) 'ERROR: More than one olake class. Check GeoClass.txt'
      checkdata(1,2) = .TRUE.
    ENDIF
    IF(help2>1)THEN
      WRITE(funit,*) 'ERROR: More than one ilake class. Check GeoClass.txt'
      checkdata(1,2) = .TRUE.
    ENDIF
    IF(help3>1)THEN
      WRITE(funit,*) 'ERROR: More than one glacier class. Check GeoClass.txt'
      checkdata(1,2) = .TRUE.
    ENDIF
    IF(help4>1)THEN
      WRITE(funit,*) 'ERROR: More than one main river class. Check GeoClass.txt'
      checkdata(1,2) = .TRUE.
    ENDIF
    IF(help5>1)THEN
      WRITE(funit,*) 'ERROR: More than one local river class. Check GeoClass.txt'
      checkdata(1,2) = .TRUE.
    ENDIF
    classtest = MINVAL(soildepth(1,:))
    IF(classtest<=0.)THEN
      WRITE(funit,*) 'Zero soil depths found for some class. Check GeoClass.txt'
    ENDIF
    classtest=MAXVAL(soildepth(:,:))
    IF(classtest<=0.)THEN
      WRITE(funit,*) 'Error: No soil depths found. Check GeoClass.txt'
      checkdata(1,2) = .TRUE.
    ENDIF
    DO i = 1,nclass
      IF(vegtypereset)THEN
        WRITE(funit,*) 'Error: vegetation type not given in GeoClass.txt for slc-class vegtype=1 (open) is used (when no indatacheck is off)'
        checkdata(1,2) = .TRUE.
      ENDIF
      IF(MAXVAL(soildepth(:,i))<classdata(i)%streamdepth)THEN
        WRITE(funit,*) 'Warning: Stream depth below deepest soillayer for class:',i
      ENDIF
    ENDDO

    !Check parameters
    WRITE(funit,*) 'Checking par.txt...'
    filename=TRIM(modeldir)//'par.txt'
    OPEN(UNIT = fileunit_temp,FILE = filename, STATUS = 'old', ACTION='read')
    !Initiate some variables
    dim = MAX(1,nsub_basemodel,nsoil,nluse,MAXVAL(nregions),12)
    IF(.NOT.ALLOCATED(values)) ALLOCATE(values(dim))
    !Read and check heading
    READ(fileunit_temp,'(a)',END=100) line
    READ(line,*,ERR=10) varstr
    varindex = 0
    DO j = 1,max_par
      IF(varstr==modparid(j)%shortname)THEN
        IF(modparid(j)%deptype/=m_ldpar)THEN 
          varindex = j
          EXIT
        ENDIF
      ENDIF
    ENDDO
    IF(varindex>0) WRITE(funit,*) 'First line (heading) has parameter name. This parameter will not be used. Ok?'
10  CONTINUE
    !Read and check rest of parameters and their values    
    nline = 1
    ncomment = 0
    DO 
      nline = nline + 1
      READ(fileunit_temp,'(a)',END=100) line
      IF(line(1:2)==comment_str)THEN
        ncomment = ncomment + 1
        CYCLE
      ENDIF
      CALL read_parameterline(line,dim,varstr,values,nvalues)    !read one line with parameter values
      IF(varstr=='          ')THEN
        WRITE(funit,*) 'No parameter name found on line',nline,'. End of par.txt file?'
        EXIT    !end of file
      ENDIF
      varindex = 0
      DO j = 1,max_par      !Find corresponding varindex
        IF(varstr==modparid(j)%shortname)THEN
          IF(modparid(j)%deptype/=m_ldpar)THEN 
            varindex = j
            EXIT
          ENDIF
        ENDIF
      ENDDO
      IF(varindex==0)THEN
        WRITE(funit,*) 'Unknown variable (',TRIM(varstr),') on line',nline,'.'
      ELSE
        !Find and check variable dimension
        dimcheck = 0
        SELECT CASE(modparid(varindex)%deptype)
        CASE(m_gpar)
          dimcheck = 1  
        CASE(m_bpar)
          dimcheck = nsub_basemodel 
        CASE(m_spar)
          dimcheck = nsoil
        CASE(m_lpar)
          dimcheck = nluse
        CASE(m_rpar)
          dimcheck = nregions(regiondivision(modparid(varindex)%parno))
        CASE(m_mpar)
          dimcheck = 12
        END SELECT
        IF(dimcheck /= nvalues)THEN
          IF(modparid(varindex)%deptype==m_rpar .AND. dimcheck<nvalues)THEN
            WRITE(funit,*) 'To many values in parameter file for parameter ',TRIM(varstr),'.'
            WRITE(funit,*) 'This parameter is parameter region dependent, and extra values will be skipped.'
          ELSE
            WRITE(funit,*) 'Error: wrong number of values in parameter file for parameter ',TRIM(varstr),'.'
            checkdata(1,2) = .TRUE.
          ENDIF
        ENDIF
      ENDIF
    ENDDO
    
100 CONTINUE
    WRITE(funit,*) 'Found',ncomment,' lines with comments in par.txt.'
    CLOSE(fileunit_temp)
    IF(ALLOCATED(values)) DEALLOCATE(values)

    !Check CropData.txt
    WRITE(funit,*) 'Checking CropData.txt...'
    IF(modeloption(p_growthstart)==1)THEN
      filename=TRIM(modeldir)//'CropData.txt'
      INQUIRE(FILE = filename, EXIST= found)
      IF(.NOT.found)THEN
        WRITE(funit,*) 'CropData.txt necessary for using growth start model 1. File not found.'
        checkdata(1,2) = .TRUE.
      ELSE
        basinarea=SUM(cropdata(:)%gddsow)
        IF(basinarea<=0) WRITE(funit,*) 'Four columns needed for using growth start model 1, but gddsow not found. Check CropData.txt.'
        basinarea=MINVAL(cropdata(:)%gddsow)
        IF(basinarea<=0) WRITE(funit,*) 'Negative or zero gddsow given. Check CropData.txt'
      ENDIF
    ENDIF
    IF(modeloption(p_erosion)==0)THEN
      filename=TRIM(modeldir)//'CropData.txt'
      INQUIRE(FILE = filename, EXIST= found)
      IF(.NOT.found)THEN
        WRITE(funit,*) 'CropData.txt necessary for using erosion model 0 (default). Erosion model is used for phosphorus and sediment simulations. File not found.'
        checkdata(1,2) = .TRUE.
      ENDIF
    ENDIF

    WRITE(funit,*) '---------------------'

  END SUBROUTINE checkindata_part1

  !>Check forcing data and other observations
  !------------------------------------------------------------
  SUBROUTINE checkindata_part2(funit) 

    USE WORLDVAR, ONLY : modeldir, &
                         fileunit_temp, &
                         filename_Qobs, &
                         maxcharpath, &
                         get_seq_filename, &
                         forcingdata, &
                         max_forcingdata, &
                         i_pobs,i_tobs, &
                         i_rhobs,i_sfobs, &
                         i_swobs,i_uobs, &
                         bdate,sdate, &
                         checkdata, &
                         i_str,i_real,i_intg
    USE MODVAR, ONLY : basin, &
                       tobselevation, &
                       nsub_basemodel  
  USE READWRITE_ROUTINES, ONLY : check_obs_timeperiod, &
                                 check_file_station_id_order, &
                                 check_q_stn

	!Argument declaration
    INTEGER, INTENT(IN)    :: funit    !<File unit of log-file
    
    !Local parameters

    !Local variables
    CHARACTER(LEN=16) filename
    CHARACTER(LEN=maxcharpath) filepath
    TYPE(DateType) :: fbdate,fedate
    REAL    forcproc
    INTEGER i,iforc
    INTEGER forcid
    INTEGER status
    INTEGER obsindex(nsub_basemodel)         !Index of observation stations
    INTEGER nobscol                !Number of columns with observations in file
    INTEGER numneg(nsub_basemodel)
    LOGICAL fileexist,notimefound
    INTEGER,ALLOCATABLE :: xi(:,:)           !Integer data read from file
    REAL,ALLOCATABLE    :: xr(:,:)           !Real data read from file

    WRITE(funit,*)
    WRITE(funit,*) 'Check indata part two'
    WRITE(funit,*) '---------------------'
    
    !Checking ForcKey and coupling between subbasin and observations
    WRITE(funit,*) 'Checking ForcKey.txt...'
    IF(ALLOCATED(tobselevation))THEN
      forcproc = MINVAL(tobselevation)
      IF(forcproc<0.) WRITE(funit,*) 'Elevation of forcing data (temperature) is below zero. Check ForcKey.txt.'
    ENDIF
    DO iforc = 1, max_forcingdata
      IF(forcingdata(iforc)%readfile)THEN
        forcid = MINVAL(forcingdata(iforc)%stationid)
        IF(forcid<=0)THEN
          WRITE(funit,*) 'Identification number of '//forcingdata(iforc)%filename//' forcing is zero for some subbasin. Check ForcKey.txt'
          checkdata(2,2) = .TRUE.
        ENDIF
      ENDIF
    ENDDO
    
    !Checking Pobs, Tobs, TMINobs, TMAXobs, RHobs, SFobs, SWobs, Uobs
    DO iforc = 1,max_forcingdata
      IF(forcingdata(iforc)%readfile)THEN
        WRITE(funit,*) 'Checking '//TRIM(forcingdata(iforc)%filename)//'.txt...'
        filename = forcingdata(iforc)%filename
        CALL get_seq_filename(filename)
        filepath = TRIM(modeldir)//TRIM(filename)
        !Check that file exist
        INQUIRE(FILE=TRIM(filepath),EXIST=fileexist)
        IF(.NOT.fileexist)THEN
          WRITE(funit,*) 'Error: Data file is missing, while setting is set to read it.'
          WRITE(funit,*) 'Error: ', TRIM(filepath)
          checkdata(2,2)=.TRUE.
        ELSE
          CALL check_file_station_id_order(fileunit_temp,filepath,nsub_basemodel,forcingdata(iforc)%stationid,obsindex,nobscol,status)
          IF(status/=0)THEN
            WRITE(funit,*) 'Error: Missmatch in observations between model and '//TRIM(forcingdata(iforc)%filename)//'.txt. Check ForcKey and '//TRIM(forcingdata(iforc)%filename)//'.txt.'
            checkdata(2,2)=.TRUE.
          ENDIF 
          CALL check_obs_timeperiod(fileunit_temp,filepath,1,bdate,   &
               sdate,fbdate,fedate,notimefound,status)
          IF(status.NE.0)THEN
            WRITE(funit,*) 'Error: Forcing data has problems with time period. Check '//TRIM(forcingdata(iforc)%filename)//'.txt.'
            checkdata(2,2)=.TRUE.
          ENDIF
          IF(iforc==i_pobs.OR.iforc==i_rhobs.OR.iforc==i_sfobs.OR.iforc==i_swobs.OR.iforc==i_uobs)THEN
            CALL check_data_positive(filepath,notimefound,nobscol,nsub_basemodel,obsindex,0,numneg,status)
            IF(status==1)THEN
              WRITE(funit,*) 'Negative forcing data exist. Check '//TRIM(forcingdata(iforc)%filename)//'.txt.'
              IF(SUM(numneg)>0)THEN 
                checkdata(2,2)=.TRUE. 
                WRITE(funit,*) 'The following subbasins have negative values:'
                DO i = 1,nsub_basemodel
                  IF(numneg(i)>0) WRITE(funit,*) 'subid:',basin(i)%subid,'obsid:',forcingdata(iforc)%stationid(i)
                ENDDO
              ENDIF  
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDDO

    !Checking Qobs
    WRITE(funit,*) 'Checking Qobs.txt...'
    filepath = TRIM(modeldir)//filename_Qobs
    INQUIRE(FILE=filepath,EXIST=fileexist)
    IF(fileexist)THEN
      CALL check_q_stn(fileunit_temp,filepath,nsub_basemodel,   &
                       basin(:)%subid,nobscol,obsindex,status)
      IF(status==0)THEN
        CALL check_obs_timeperiod(fileunit_temp,filepath,1,   &
            bdate,sdate,fbdate,fedate,notimefound,status)
        IF(status==1)THEN
          WRITE(funit,*) 'Error: Discharge data has problems with time period. Check Qobs.txt.'
          checkdata(2,2)=.TRUE.
        ENDIF
        CALL check_data_positive(filepath,notimefound,nobscol,nsub_basemodel,obsindex,1,numneg,status)
        IF(status==1)THEN
          WRITE(funit,*) 'Negative discharge data exist. Check Qobs.txt.'
          IF(SUM(numneg)>0)THEN 
            WRITE(funit,*) 'The following subbasins have negative discharge:'
            DO i = 1,nsub_basemodel
              IF(numneg(i)>0) WRITE(funit,*) 'subid:',basin(i)%subid
            ENDDO
          ENDIF  
        ENDIF
      ENDIF
    ENDIF

    IF(ALLOCATED(xi)) DEALLOCATE(xi)
    IF(ALLOCATED(xr)) DEALLOCATE(xr)

    WRITE(funit,*) '---------------------'
    RETURN

  END SUBROUTINE checkindata_part2
  
  !>Check for negative data in file maybe also for missing values (Pobs.txt, Qobs.txt)
  !------------------------------------------------------------
  SUBROUTINE check_data_positive(filepath,notimefound,ncols,ns,oindex,miss,numneg,negfound)

    USE WORLDVAR, ONLY : readmatlab,      &
                         fileunit_temp   
    USE MODVAR, ONLY : missing_value

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: filepath  !<File
    LOGICAL, INTENT(IN)  :: notimefound  !<Flag for time format: date or date and time
    INTEGER, INTENT(IN)  :: ncols        !<Number of data columns in file
    INTEGER, INTENT(IN)  :: ns           !<Number of subbasins, basemodel
    INTEGER, INTENT(IN)  :: oindex(ns)   !<Index for columns used in model set-up
    INTEGER, INTENT(IN)  :: miss         !<Flag for counting missing values, 0=count, 1=ignore
    INTEGER, INTENT(OUT) :: numneg(ns)   !<Number of negative values per subbasin
    INTEGER, INTENT(OUT) :: negfound     !<Code for found negative data
    
    !Local variables
    INTEGER timeform
    INTEGER i
    INTEGER :: neg(ncols)
    REAL :: y(ncols)                 !Data (one time step)
    CHARACTER(LEN=16)  d2,d3         !Date yyyy-mm-dd[ hh:mm]

    IF(notimefound)THEN
      timeform = 0
    ELSE
      timeform = 1
    ENDIF

    !Load precipitation forcing data
    OPEN(UNIT = fileunit_temp,FILE = filepath, STATUS = 'old', ACTION='read')
    READ(fileunit_temp,*)  !Skip heading
    neg = 0
    numneg = 0
    DO
      y = missing_value
      IF(readmatlab)THEN
        READ(fileunit_temp,*,END=900) d2,y
      ELSEIF(timeform==0)THEN
        READ(fileunit_temp,*,END=900) d2,y
      ELSEIF(timeform==1)THEN
        READ(fileunit_temp,*,END=900) d2,d3,y    
      ENDIF
      DO i = 1,ncols
        IF(miss==0)THEN
          IF(y(i)<0.) neg(i) = neg(i) + 1
        ELSEIF(miss==1)THEN
          IF(y(i)<0..AND.y(i)/=missing_value) neg(i) = neg(i) + 1
        ENDIF
      ENDDO  
    ENDDO
900 IF(SUM(neg)==0.)THEN
      negfound = 0    !no negative data in file
    ELSE
      negfound = 1
      DO i = 1,ns
        IF(oindex(i)>0)THEN
          numneg(i) = neg(oindex(i))
        ENDIF
      ENDDO
    ENDIF

  END SUBROUTINE check_data_positive

  !>Stops simulation if checkindata routine asks for it
  !------------------------------------------------------------
  SUBROUTINE checkindata_stop(funit,status) 

    USE WORLDVAR, ONLY : checkdata
    
    !Argument declaration
    INTEGER, INTENT(IN)    :: funit    !<File unit of log-file
    INTEGER, INTENT(OUT)   :: status   !<error number

    IF(checkdata(1,2).OR.checkdata(2,2))THEN
      WRITE(funit,*)
      WRITE(funit,*) 'Simulation stopped'
      WRITE(funit,*) 'Serious errors in indata found'
      status = 1
      RETURN
    ENDIF
    status = 0
    
  END SUBROUTINE checkindata_stop

END MODULE HYPE_TEST_ROUTINES
