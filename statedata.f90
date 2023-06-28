!> \file statedata.f90
!> Contains module state_datamodule.

!> \brief Load and save model states.
!>
!> Procedures for loading and saving initial states from file. Also 
!> processing them for submodel.
MODULE STATE_DATAMODULE
  !Copyright 2014-2018 SMHI
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
  !
  !--------------------------------------------------------------------
  USE STATETYPE_MODULE
  USE LibDate
  !Subroutines also uses modules convert, modelmodule, modvar, readwrite_routines and worldvar.
  
  IMPLICIT NONE
  PRIVATE
! Private procedures
! -------------------
! divide_large_array  
! write_state_check
! read_and_perform_state_check
  PUBLIC :: initiate_state_for_submodel,&
            load_saved_state,&
            finalize_outstate

CONTAINS
  

  !>Initiate state variables for submodel simulation
  !----------------------------------------------------------------------
  SUBROUTINE initiate_state_for_submodel(dir,indexarray,stateinput,   &
                                         frozenstate,soilstate,aquiferstate,riverstate,lakestate,miscstate) 

    USE MODELMODULE, ONLY : initiate_model,   &
                            initiate_model_state
    USE MODVAR, ONLY : nsub,                     &
                       nsub_basemodel,           &
                       conduct, &
                       statesize

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir      !<file directory
    INTEGER, INTENT(IN)  :: indexarray(nsub) !<index for basemodel
    LOGICAL, INTENT(IN)  :: stateinput       !<code for reading state
    TYPE(snowicestatetype),INTENT(INOUT) :: frozenstate   !<Snow and ice states
    TYPE(soilstatetype),INTENT(INOUT)  :: soilstate   !<Soil states
    TYPE(aquiferstatetype),INTENT(INOUT) :: aquiferstate  !<Aquifer states
    TYPE(riverstatetype),INTENT(INOUT) :: riverstate  !<River states
    TYPE(lakestatetype),INTENT(INOUT)  :: lakestate   !<Lake states
    TYPE(miscstatetype),INTENT(INOUT)  :: miscstate   !<Misc states

    !Local variables
    TYPE(snowicestatetype) :: frozenstate2   !Temporary snow and ice states
    TYPE(soilstatetype)    :: soilstate2   !Temporary soil states
    TYPE(aquiferstatetype) :: aquiferstate2  !<Aquifer states
    TYPE(riverstatetype)   :: riverstate2  !Temporary river states
    TYPE(lakestatetype)    :: lakestate2   !Temporary lake states
    TYPE(miscstatetype)    :: miscstate2   !Temporary misc states

    !>/b Algoritm /n
    CALL deallocate_model_states(frozenstate,soilstate,aquiferstate,riverstate,lakestate,miscstate)
    !>If statefiles exist: read and store states temporary
    IF(stateinput)THEN
      CALL allocate_model_states(nsub_basemodel,statesize,conduct, &
                               frozenstate2,soilstate2,aquiferstate2,riverstate2,lakestate2,miscstate2) 
      CALL load_saved_state(dir,nsub_basemodel,frozenstate2,soilstate2,aquiferstate2,riverstate2,lakestate2,miscstate2)
    ENDIF
    !>Reallocate state variables to submodel size
    CALL allocate_model_states(nsub,statesize,conduct, &
                               frozenstate,soilstate,aquiferstate,riverstate,lakestate,miscstate)
    !>If statefiles exist: Initiate state variables from those and deallocate temporary storage
    IF(stateinput)THEN
      CALL initiate_modelstate_submodel(nsub,statesize,conduct,indexarray,   &
                                        frozenstate,frozenstate2,soilstate,soilstate2, &
                                        aquiferstate,aquiferstate2,riverstate,riverstate2, &
                                        lakestate,lakestate2,miscstate,miscstate2)
      CALL deallocate_model_states(frozenstate2,soilstate2,aquiferstate2,riverstate2,lakestate2,miscstate2)
    !>Else: Initiate state variables with default values
    ELSE
      CALL initiate_model_state(frozenstate,soilstate,aquiferstate,riverstate,lakestate,miscstate)
    ENDIF
    !>Initiate other model variables and parameters
    CALL initiate_model(frozenstate,soilstate,aquiferstate,riverstate,lakestate,miscstate)  

  END SUBROUTINE initiate_state_for_submodel

  !>Load starting state from file and initiate state variables
  !------------------------------------------------------------------
  SUBROUTINE load_saved_state(dir,ns,frozenstate,soilstate,aquiferstate,riverstate,lakestate,miscstate)

    USE MODVAR, ONLY : seconds_per_timestep, &
                       conduct, &
                       statesize, &
                       STATECONFIGURATIONTYPE
    USE WORLDVAR, ONLY : fileunit_temp, &   
                         bdate
    USE READWRITE_ROUTINES, ONLY : read_array_from_file
 
    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir   !<file directory
    INTEGER, INTENT(IN)  :: ns            !<number of subbasins
    TYPE(snowicestatetype),INTENT(INOUT) :: frozenstate !<Snow and ice states
    TYPE(soilstatetype),INTENT(INOUT)    :: soilstate   !<Soil states
    TYPE(aquiferstatetype),INTENT(INOUT) :: aquiferstate   !<Aquifer states
    TYPE(riverstatetype),INTENT(INOUT)   :: riverstate  !<River states
    TYPE(lakestatetype),INTENT(INOUT)    :: lakestate   !<Lake states
    TYPE(miscstatetype),INTENT(INOUT)    :: miscstate   !<Misc states
    
    !Local variables
    INTEGER ffunit               
    INTEGER ios
    INTEGER dim,idim
    INTEGER ipiece,npiece
    INTEGER checkstatus,readsubst
    TYPE(STATECONFIGURATIONTYPE) :: statefileconfig
    INTEGER, ALLOCATABLE :: sectionlimits(:,:)
    REAL, ALLOCATABLE :: array(:)
    CHARACTER(LEN=28) filename   
    CHARACTER(LEN=16) bdatestr  
     
    !Local parameters
    INTEGER, PARAMETER :: seconds_per_day  = 86400 

    !Beginning of subroutine
    IF(seconds_per_timestep==seconds_per_day)THEN
      CALL format_date(bdate,'yyyymmdd',bdatestr)
    ELSE
      CALL format_date(bdate,'yyyymmddHHMM',bdatestr)
    ENDIF
    filename = 'state_save'//TRIM(ADJUSTL(bdatestr))//'.txt'                
    ffunit = fileunit_temp
    OPEN(FILE=TRIM(dir)//TRIM(filename),UNIT=ffunit,STATUS='old',FORM='formatted',IOSTAT=ios,ACTION='read')
    IF(ios/=0) THEN
      WRITE(6,*) 'ERROR: Statefile ', filename, ' not found'
      STOP 1
    ENDIF

    CALL read_and_perform_state_check(ffunit,checkstatus,readsubst,statefileconfig)

    CALL get_frozenstate_variables_arraysize(ns,readsubst,statesize,statefileconfig,dim)
    IF(dim>0)THEN
      CALL divide_large_array(dim,npiece,sectionlimits)
      DO ipiece = 1,npiece
        idim = sectionlimits(2,ipiece)-sectionlimits(1,ipiece)+1
        ALLOCATE(array(idim))
        CALL read_array_from_file(ffunit,100,idim,array)
        CALL set_frozenstate_variables_from_array(ns,readsubst, &
                  statesize,conduct,frozenstate, &
                  sectionlimits(1,ipiece),sectionlimits(2,ipiece),idim,array)
        DEALLOCATE(array)
      ENDDO
      DEALLOCATE(sectionlimits)
    ENDIF

    CALL get_soilstate_variables_arraysize(ns,readsubst,statesize,statefileconfig,dim)
    IF(dim>0)THEN
      CALL divide_large_array(dim,npiece,sectionlimits)
      DO ipiece = 1,npiece
        idim = sectionlimits(2,ipiece)-sectionlimits(1,ipiece)+1
        ALLOCATE(array(idim))
        CALL read_array_from_file(ffunit,100,idim,array)
        CALL set_soilstate_variables_from_array(ns,readsubst,statesize,conduct,soilstate, &
                  sectionlimits(1,ipiece),sectionlimits(2,ipiece),idim,array)
        DEALLOCATE(array)
      ENDDO
      DEALLOCATE(sectionlimits)
    ENDIF

    CALL get_aquiferstate_variables_arraysize(readsubst,statesize,dim)
    IF(dim>0)THEN
      CALL divide_large_array(dim,npiece,sectionlimits)
      DO ipiece = 1,npiece
        idim = sectionlimits(2,ipiece)-sectionlimits(1,ipiece)+1
        ALLOCATE(array(idim))
        CALL read_array_from_file(ffunit,100,idim,array)
        CALL set_aquiferstate_variables_from_array(readsubst,statesize,aquiferstate, &
                  sectionlimits(1,ipiece),sectionlimits(2,ipiece),idim,array)
        DEALLOCATE(array)
      ENDDO
      DEALLOCATE(sectionlimits)
    ENDIF

    CALL get_riverstate_variables_arraysize(ns,readsubst,statesize,  &
              statefileconfig,dim)
    IF(dim>0)THEN
      CALL divide_large_array(dim,npiece,sectionlimits)
      DO ipiece = 1,npiece
        idim = sectionlimits(2,ipiece)-sectionlimits(1,ipiece)+1
        ALLOCATE(array(idim))
        CALL read_array_from_file(ffunit,100,idim,array)
        CALL set_riverstate_variables_from_array(ns,readsubst,statesize,conduct, &
                  statefileconfig,riverstate,sectionlimits(1,ipiece),sectionlimits(2,ipiece),idim,array)
        DEALLOCATE(array)
      ENDDO
      DEALLOCATE(sectionlimits)
    ENDIF

    CALL get_lakestate_variables_arraysize(ns,readsubst,statesize,statefileconfig,dim)
    IF(dim>0)THEN
      CALL divide_large_array(dim,npiece,sectionlimits)
      DO ipiece = 1,npiece
        idim = sectionlimits(2,ipiece)-sectionlimits(1,ipiece)+1
        ALLOCATE(array(idim))
        CALL read_array_from_file(ffunit,100,idim,array)
        CALL set_lakestate_variables_from_array(ns,readsubst,statesize, &
                  statefileconfig,lakestate,sectionlimits(1,ipiece),sectionlimits(2,ipiece),idim,array)
        DEALLOCATE(array)
      ENDDO
      DEALLOCATE(sectionlimits)
    ENDIF

    CALL get_miscstate_variables_arraysize(ns,statesize,statefileconfig,dim)
    IF(dim>0)THEN
      CALL divide_large_array(dim,npiece,sectionlimits)
      DO ipiece = 1,npiece
        idim = sectionlimits(2,ipiece)-sectionlimits(1,ipiece)+1
        ALLOCATE(array(idim))
        CALL read_array_from_file(ffunit,100,idim,array)
        CALL set_miscstate_variables_from_array(ns,readsubst,statesize, &
                  statefileconfig,conduct,miscstate, &
                  sectionlimits(1,ipiece),sectionlimits(2,ipiece),idim,array)
        DEALLOCATE(array)
      ENDDO
      DEALLOCATE(sectionlimits)
    ENDIF

    CLOSE(ffunit)
    WRITE(6,*) 'File read: ', TRIM(dir)//TRIM(filename)


  END SUBROUTINE load_saved_state

  !>Saves state values for later use as starting state
  !---------------------------------------------------
  SUBROUTINE finalize_outstate(dir,ns,stateoutdate,frozenstate,soilstate,aquiferstate,riverstate,lakestate,miscstate) 

    USE MODVAR, ONLY : seconds_per_timestep,    &
                       conduct,  &
                       statesize
    USE WORLDVAR, ONLY : fileunit_temp
    USE READWRITE_ROUTINES, ONLY : write_array_to_file

    !Argument declaration
    CHARACTER(LEN=*), INTENT(IN) :: dir            !<file directory
    INTEGER, INTENT(IN)  :: ns            !<number of subbasins
    TYPE(DateType), INTENT(IN) :: stateoutdate     !<date for writing state           
    TYPE(snowicestatetype),INTENT(INOUT) :: frozenstate !<Snow and ice states
    TYPE(soilstatetype),INTENT(INOUT)    :: soilstate   !<Soil states
    TYPE(aquiferstatetype),INTENT(INOUT) :: aquiferstate   !<Aquifer states
    TYPE(riverstatetype),INTENT(INOUT)   :: riverstate  !<River states
    TYPE(lakestatetype),INTENT(INOUT)    :: lakestate   !<Lake states
    TYPE(miscstatetype),INTENT(INOUT)    :: miscstate   !<Misc states
    
    !Local variables
    INTEGER ipiece,npiece
    INTEGER ffunit
    INTEGER dim,idim
    INTEGER,ALLOCATABLE :: sectionlimits(:,:)
    REAL,ALLOCATABLE :: array(:)
    CHARACTER(LEN=16) stateoutdatestr
    CHARACTER(LEN=28) filename  
    
    !Local parameters
    INTEGER, PARAMETER :: seconds_per_day  = 86400 

    !Save STATETYPE_MODULE state variables 
    ffunit = fileunit_temp
    IF(seconds_per_timestep==seconds_per_day)THEN
      CALL format_date(stateoutdate,'yyyymmdd',stateoutdatestr)
    ELSE
      CALL format_date(stateoutdate,'yyyymmddHHMM',stateoutdatestr)
    ENDIF
    filename = 'state_save'//TRIM(ADJUSTL(stateoutdatestr))//'.txt'
    OPEN(FILE=TRIM(dir)//TRIM(filename),UNIT=ffunit,STATUS='unknown',FORM='formatted',ACTION='write')
    CALL write_state_check(ffunit)

    CALL get_frozenstate_variables_arraysize(ns,statesize%substance,statesize,conduct,dim)
    IF(dim>0)THEN
      CALL divide_large_array(dim,npiece,sectionlimits)
      DO ipiece = 1,npiece
        idim = sectionlimits(2,ipiece)-sectionlimits(1,ipiece)+1
        ALLOCATE(array(idim))
        CALL set_frozenstate_variables_to_array(ns,statesize,conduct,frozenstate,  &
                  sectionlimits(1,ipiece),sectionlimits(2,ipiece),idim,array)
        CALL write_array_to_file(ffunit,100,idim,array)
        DEALLOCATE(array)
      ENDDO
      DEALLOCATE(sectionlimits)
    ENDIF

    CALL get_soilstate_variables_arraysize(ns,statesize%substance,statesize,conduct,dim)
    IF(dim>0)THEN
      CALL divide_large_array(dim,npiece,sectionlimits)
      DO ipiece = 1,npiece
        idim = sectionlimits(2,ipiece)-sectionlimits(1,ipiece)+1
        ALLOCATE(array(idim))
        CALL set_soilstate_variables_to_array(ns,statesize,conduct,soilstate, &
                  sectionlimits(1,ipiece),sectionlimits(2,ipiece),idim,array)
        CALL write_array_to_file(ffunit,100,idim,array)
        DEALLOCATE(array)
      ENDDO
      DEALLOCATE(sectionlimits)
    ENDIF

    CALL get_aquiferstate_variables_arraysize(statesize%substance,statesize,dim)
    IF(dim>0)THEN
      CALL divide_large_array(dim,npiece,sectionlimits)
      DO ipiece = 1,npiece
        idim = sectionlimits(2,ipiece)-sectionlimits(1,ipiece)+1
        ALLOCATE(array(idim))
        CALL set_aquiferstate_variables_to_array(statesize,aquiferstate,  &
                  sectionlimits(1,ipiece),sectionlimits(2,ipiece),idim,array)
        CALL write_array_to_file(ffunit,100,idim,array)
        DEALLOCATE(array)
      ENDDO
      DEALLOCATE(sectionlimits)
    ENDIF

    CALL get_riverstate_variables_arraysize(ns,statesize%substance,statesize,conduct,dim)
    IF(dim>0)THEN
      CALL divide_large_array(dim,npiece,sectionlimits)
      DO ipiece = 1,npiece
        idim = sectionlimits(2,ipiece)-sectionlimits(1,ipiece)+1
        ALLOCATE(array(idim))
        CALL set_riverstate_variables_to_array(ns,statesize,conduct,riverstate,  &
                  sectionlimits(1,ipiece),sectionlimits(2,ipiece),idim,array)
        CALL write_array_to_file(ffunit,100,idim,array)
        DEALLOCATE(array)
      ENDDO
      DEALLOCATE(sectionlimits)
    ENDIF

    CALL get_lakestate_variables_arraysize(ns,statesize%substance,statesize,conduct,dim)
    IF(dim>0)THEN
      CALL divide_large_array(dim,npiece,sectionlimits)
      DO ipiece = 1,npiece
        idim = sectionlimits(2,ipiece)-sectionlimits(1,ipiece)+1
        ALLOCATE(array(idim))
        CALL set_lakestate_variables_to_array(ns,statesize,conduct,lakestate, &
                  sectionlimits(1,ipiece),sectionlimits(2,ipiece),idim,array)
        CALL write_array_to_file(ffunit,100,idim,array)
        DEALLOCATE(array)
      ENDDO
      DEALLOCATE(sectionlimits)
    ENDIF

    CALL get_miscstate_variables_arraysize(ns,statesize,conduct,dim)
    IF(dim>0)THEN
      CALL divide_large_array(dim,npiece,sectionlimits)
      DO ipiece = 1,npiece
        idim = sectionlimits(2,ipiece)-sectionlimits(1,ipiece)+1
        ALLOCATE(array(idim))
        CALL set_miscstate_variables_to_array(ns,statesize,  &
                  conduct,miscstate,sectionlimits(1,ipiece), &
                  sectionlimits(2,ipiece),idim,array)
        CALL write_array_to_file(ffunit,100,idim,array)
        DEALLOCATE(array)
      ENDDO
      DEALLOCATE(sectionlimits)
    ENDIF

    CLOSE(ffunit)

  END SUBROUTINE finalize_outstate

  !--------------------------------------------------------------------
  !> Calculates appropriate size sections of large array
  !--------------------------------------------------------------------
  SUBROUTINE divide_large_array(dim,npiece,array)

  !Argument declarations
  INTEGER,INTENT(IN) :: dim
  INTEGER,INTENT(OUT) :: npiece
  INTEGER,ALLOCATABLE,INTENT(OUT) :: array(:,:)
  
  !Local varaibles
  INTEGER i
  INTEGER,PARAMETER :: maxchunksize = 12500000  !corresponds to real array of 50MB
  
  npiece = dim/maxchunksize
  IF(dim-maxchunksize*npiece>0) npiece = npiece + 1
  ALLOCATE(array(2,npiece))
  DO i = 1,npiece
    array(1,i) = 1 + (i-1)*maxchunksize
    array(2,i) = i*maxchunksize
  ENDDO
  array(2,npiece) = dim
  
  END SUBROUTINE divide_large_array
  
  !--------------------------------------------------------------------
  !> Saves values for later use as check if starting state is appropriate
  !--------------------------------------------------------------------
  SUBROUTINE write_state_check(ffunitloc) 

    USE MODVAR, ONLY : i_in,i_sp,i_t1,i_t2,i_oc,i_ss,i_ae, &
                       nsub, &
                       conduct, &
                       statesize, &
                       modeloption,p_lakeriverice
    USE CONVERT, ONLY : logical_convert_to_integer
         
    !Argument declarations
    INTEGER, INTENT(IN) :: ffunitloc   !<File unit
    
    !Local variables
    INTEGER log2intvar1,log2intvar2,log2intvar3,log2intvar4,log2intvar5
    INTEGER log2intvar6,log2intvar8,log2intvar9,log2intvar10,log2intvar11
    INTEGER log2intvar12, log2intvar13

    !Transform logical variables to integer
    log2intvar1 = logical_convert_to_integer(conduct%simN)
    log2intvar2 = logical_convert_to_integer(conduct%simP)
    log2intvar3 = logical_convert_to_integer(conduct%simC)
    log2intvar11 = logical_convert_to_integer(conduct%simS)
    log2intvar4 = logical_convert_to_integer(conduct%riverwetland)
    log2intvar5 = logical_convert_to_integer(conduct%irrigation)
    log2intvar6 = logical_convert_to_integer(conduct%glacier)
    log2intvar8 = logical_convert_to_integer(conduct%arupdating)
    log2intvar9 = logical_convert_to_integer(conduct%growthdegreeday)
    log2intvar10 = logical_convert_to_integer(conduct%icelens)
    log2intvar12 = logical_convert_to_integer(conduct%watertransfer)
    log2intvar13 = logical_convert_to_integer(conduct%floodplain)
    
    !Checkwrite for substances, number of subbasins and slc-classes
    WRITE(ffunitloc,'(8I3,I7,17I5)') statesize%substance, i_in,i_sp,i_t1, &
            i_t2,i_oc,i_ss,i_ae,nsub,statesize%slcclass,statesize%soillayer, &
            statesize%riverqueue,statesize%timestep,log2intvar1, &
            log2intvar2,log2intvar3,log2intvar11,log2intvar4,log2intvar5,log2intvar6, &
            modeloption(p_lakeriverice),log2intvar8,log2intvar9,log2intvar10,log2intvar12, &
            log2intvar13

  END SUBROUTINE write_state_check

  !> Check if starting state is appropriate
  !------------------------------------------------------------
  SUBROUTINE read_and_perform_state_check(ffunitloc,status,nsubst,config)

    USE MODVAR, ONLY : i_in,i_sp,i_t1,i_t2,i_oc,i_ss,i_ae, &
                       nsub_basemodel, &
                       STATECONFIGURATIONTYPE, &
                       conduct, &
                       statesize, &
                       modeloption,p_lakeriverice
    USE CONVERT, ONLY : logical_convert_to_integer, &
                        integer_convert_to_logical

    !Argument declarations
    INTEGER, INTENT(IN)  :: ffunitloc !<File unit
    INTEGER, INTENT(OUT) :: status    !<status of check
    INTEGER, INTENT(OUT) :: nsubst    !<number of substances in file
    TYPE(STATECONFIGURATIONTYPE), INTENT(OUT) :: config !<state file configuration
    
    !Local variables
    INTEGER :: statecheck_file(26)  !Checkrow from saved state file
    INTEGER :: statecheck_sim(26)   !Checkrow from current model simulation

    !>\b Algorithm \n
    !>Get model set-up for statefile and current model
    READ(ffunitloc, *,ERR=200) statecheck_file
    statecheck_sim(1) = statesize%substance
    statecheck_sim(2) = i_in
    statecheck_sim(3) = i_sp
    statecheck_sim(4) = i_t1
    statecheck_sim(5) = i_t2
    statecheck_sim(6) = i_oc
    statecheck_sim(7) = i_ss
    statecheck_sim(8) = i_ae
    statecheck_sim(9) = nsub_basemodel
    statecheck_sim(10) = statesize%slcclass
    statecheck_sim(11) = statesize%soillayer
    statecheck_sim(12) = statesize%riverqueue
    statecheck_sim(13) = statesize%timestep
    statecheck_sim(14) = logical_convert_to_integer(conduct%simN)
    statecheck_sim(15) = logical_convert_to_integer(conduct%simP)
    statecheck_sim(16) = logical_convert_to_integer(conduct%simC)
    statecheck_sim(17) = logical_convert_to_integer(conduct%simS)
    statecheck_sim(18) = logical_convert_to_integer(conduct%riverwetland)
    statecheck_sim(19) = logical_convert_to_integer(conduct%irrigation)
    statecheck_sim(20) = logical_convert_to_integer(conduct%glacier)
    statecheck_sim(21) = modeloption(p_lakeriverice)
    statecheck_sim(22) = logical_convert_to_integer(conduct%arupdating)
    statecheck_sim(23) = logical_convert_to_integer(conduct%growthdegreeday)
    statecheck_sim(24) = logical_convert_to_integer(conduct%icelens)
    statecheck_sim(25) = logical_convert_to_integer(conduct%watertransfer)
    statecheck_sim(26) = logical_convert_to_integer(conduct%floodplain)
    
    !>Set output variables
    nsubst = statecheck_file(1)
    config%simT1 = statecheck_file(4)>0
    config%simT2 = statecheck_file(5)>0
    config%simN = integer_convert_to_logical(statecheck_file(14))
    config%simP = integer_convert_to_logical(statecheck_file(15))
    config%simC = integer_convert_to_logical(statecheck_file(16))
    config%simS = integer_convert_to_logical(statecheck_file(17))
    config%arupdating = integer_convert_to_logical(statecheck_file(22))
    config%floodplain = integer_convert_to_logical(statecheck_file(26))
    config%glacier = integer_convert_to_logical(statecheck_file(20))
    config%growthdegreeday = integer_convert_to_logical(statecheck_file(23))
    config%icelens = integer_convert_to_logical(statecheck_file(24))
    config%irrigation = integer_convert_to_logical(statecheck_file(19))
    config%lakeriverice = (statecheck_file(21)==1).OR.(statecheck_file(21)==2)
    config%riverwetland = integer_convert_to_logical(statecheck_file(18))
    config%watertransfer = integer_convert_to_logical(statecheck_file(25))
    
    !>Compare model set-up with statefile set-up
    status = 2    !missmatch
    IF(ALL(statecheck_sim==statecheck_file,1)) THEN
      !identical model simulation set-up
      status = 0
    ELSEIF(statecheck_sim(22)/=statecheck_file(22))THEN
      !AR-updating in one of the model set-ups only
      statecheck_file(22)=statecheck_sim(22)
      IF(ALL(statecheck_sim==statecheck_file,1))   &
      !similar model simulation set-up
      status = 1
    ELSEIF(statecheck_sim(1)==0)THEN
      !no substances modelled
      IF(statecheck_file(9)==statecheck_sim(9).AND.  &
         statecheck_file(10)==statecheck_sim(10).AND. &
         statecheck_file(11)==statecheck_sim(11).AND. &
         statecheck_file(12)==statecheck_sim(12).AND. &
         statecheck_file(13)==statecheck_sim(13).AND. &
         statecheck_file(18)==statecheck_sim(18).AND. &
         statecheck_file(19)==statecheck_sim(19).AND. &
         statecheck_file(20)==statecheck_sim(20).AND. &
         statecheck_file(21)==statecheck_sim(21).AND. &
         statecheck_file(24)==statecheck_sim(24).AND. &
         statecheck_file(25)==statecheck_sim(25).AND. &
         statecheck_file(26)==statecheck_sim(26))  &
      !similar model simulation set-up, but without substance simulation
      status = 1
    ENDIF
    IF(status==2)THEN
      WRITE(6,*) ' '
      WRITE(6,*) 'ERROR: State file not compatible with model set-up and simulation'
      STOP 1
    ENDIF
    RETURN
    
200 WRITE(6,*) 'ERROR: reading first line of state-file'
    STOP 1
    RETURN
    
  END SUBROUTINE read_and_perform_state_check


END MODULE STATE_DATAMODULE
