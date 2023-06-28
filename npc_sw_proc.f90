!> \file npc_sw_proc.f90
!> Contains module npc_surfacewater_processes.

!>Nitrogen, phosphorus and organic carbon processes in surface water in HYPE
MODULE NPC_SURFACEWATER_PROCESSES

  !Copyright 2012-2018 SMHI
  !
  !This file is part of HYPE.
  !HYPE is free software: you can redistribute it and/or modify it under the terms of the Lesser GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
  !HYPE is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the Lesser GNU General Public License for more details.
  !You should have received a copy of the Lesser GNU General Public License along with HYPE. If not, see <http://www.gnu.org/licenses/>.

  !Used modules
  USE STATETYPE_MODULE, ONLY : riverstatetype,lakestatetype
  USE GENERAL_WATER_CONCENTRATION
  USE GENERAL_FUNCTIONS
  !Subroutines also uses modvar and hypevariables
  IMPLICIT NONE
  PRIVATE
  !----------------------------------------------
  ! Private procedures 
  !----------------------------------------------
  ! denitrification_water 
  ! production_mineralisation
  ! lake_sedimentation
  ! sedimentation_resuspension
  ! calculate_lake_tpmean
  ! calculate_river_tpmean
  ! internal_lake_load 
  ! oc_production_mineralisation
  ! calculate_wetland_np 
  !----------------------------------------------
  PUBLIC :: initiate_river_npc_state, &
       initiate_lake_npc_state, &
       add_deposition_to_river_as_load, &
       add_deposition_to_lake_as_load, &
       np_processes_in_river, &
       np_processes_in_lake, &
       oc_processes_in_river, &
       oc_processes_in_lake, &
       add_diffuse_source_to_local_river, &
       add_point_sources_to_main_river, &
       calculate_river_wetland, &
       set_lake_slowwater_maxvolume
CONTAINS

  !>\brief Initiation river variables for nutrients and organic
  !>carbon simulations. Concentration (mg/L)
  !>
  !>\b Consequences Module hypevariables variable Qmax, Q2max,
  !>iQmax, iQ2max may be allocated and set.
  !-----------------------------------------------------------------
  SUBROUTINE initiate_river_npc_state(initN,initP,initC,initS,initT1,riverstate)

    USE HYPEVARIABLES, ONLY : Qmax,       &   !OUT
                              Q2max,      &   !OUT
                              iQmax,      &   !OUT
                              iQ2max,     &   !OUT
                              m_iniT2,    &
                              m_tpmean,   &
                              m_ldTPmean
    USE MODVAR,      ONLY : nsub, &
                            basin, &
                            regpar, &
                            regiondivision, &
                            lakedatapar, &
                            lakedataparindex

    !Argument declarations
    LOGICAL, INTENT(IN) :: initN  !<flag for initiation of nitrogen model
    LOGICAL, INTENT(IN) :: initP  !<flag for initiation of phosphorus model
    LOGICAL, INTENT(IN) :: initC  !<flag for initiation of organic carbon model
    LOGICAL, INTENT(IN) :: initS  !<flag for simulation of sediment model
    LOGICAL, INTENT(IN) :: initT1 !<flag for initiation of tracer model T1
    TYPE(riverstatetype),INTENT(INOUT)   :: riverstate  !<River states

    !Local variables
    INTEGER isb

    !>\b Algorithm \n
    !>Allocate and initialize river sediment variables
    IF(initN.OR.initP.OR.initS.OR.initT1)THEN !For calculation of bankful flow and Qmean
      IF(.NOT.ALLOCATED(Qmax))   ALLOCATE(Qmax(2,nsub))
      IF(.NOT.ALLOCATED(Q2max))  ALLOCATE(Q2max(2,nsub))
      IF(.NOT.ALLOCATED(iQmax))  ALLOCATE(iQmax(2,nsub))
      IF(.NOT.ALLOCATED(iQ2max)) ALLOCATE(iQ2max(2,nsub))
      riverstate%Q365 = 0.0001
      riverstate%Qdayacc = 0.000
      Qmax = 0.0001; Q2max = 0.0001
      iQmax = 365; iQ2max = 364
    ENDIF

    !>Set TPmean-variable if phosphorus is not calculated by HYPE
    IF((initN.OR.initC.OR.initS).AND.(.NOT.initP))THEN
      DO isb = 1,nsub
        IF(basin(isb)%parregion(regiondivision(m_tpmean))>0) riverstate%TPmean(:,isb) = regpar(m_tpmean,basin(isb)%parregion(regiondivision(m_tpmean)))
        IF(ALLOCATED(lakedatapar)) riverstate%TPmean(1,isb) = lakedatapar(lakedataparindex(isb,1),m_ldtpmean)
        IF(ALLOCATED(lakedatapar)) riverstate%TPmean(2,isb) = lakedatapar(lakedataparindex(isb,2),m_ldtpmean)
      ENDDO
    ENDIF

  END SUBROUTINE initiate_river_npc_state

  !>Initiation lake for nutrients and organic carbon. Concentration in 
  !(mg/L)
  !!
  !>\b Consequences Module hypevariables variable slowlakeini
  !> may be allocated and set.
  !-----------------------------------------------------------------
  SUBROUTINE initiate_lake_npc_state(inislow,initN,initP,initC,initS,initilake,initolake,lakestate)

    USE HYPEVARIABLES, ONLY : slowlakeini,  &   !OUT
                              m_tpmean,     &
                              m_tnmean,     &
                              m_tocmean,    &
                              m_ldtpmean,   &
                              m_ldtnmean,   &
                              m_ldtocmean,  &
                              m_lddeeplake, &
                              m_iniT2
    USE MODVAR, ONLY : basin, &
                       nsub, &
                       genpar, &
                       regpar, &
                       regiondivision, &
                       lakedatapar, &
                       lakedataparindex, &
                       i_in,i_on, &
                       i_pp,i_oc,i_t2

    !Argument declarations
    LOGICAL, INTENT(IN) :: inislow   !<flag for initiation of slowlake (numsubstances>0)
    LOGICAL, INTENT(IN) :: initN     !<flag for initiation of nitrogen model
    LOGICAL, INTENT(IN) :: initP     !<flag for initiation of phosphorus model
    LOGICAL, INTENT(IN) :: initC     !<flag for initiation of organic carbon model
    LOGICAL, INTENT(IN) :: initS     !<flag for simulation of sediment model
    LOGICAL, INTENT(IN) :: initilake !<flag for ilake class existance
    LOGICAL, INTENT(IN) :: initolake !<flag for olake class existance
    TYPE(lakestatetype),INTENT(INOUT)    :: lakestate   !<Lake states
    
    !Local variables
    INTEGER j,isb     !loop-variables (class,subbasin)
    REAL, ALLOCATABLE :: inidepth(:,:)

    !Allocate variables for lake partitioning in fastflow (lakewi) and slow lake parts.    
    IF(inislow)THEN
      CALL set_lake_slowwater_maxvolume(nsub,basin%lakedepth(1),basin(:)%lakedepth(2), &
                   lakedataparindex,lakedatapar(:,m_lddeeplake),initilake,initolake)
      ALLOCATE(inidepth(2,nsub))   

      !Initiate default lake volumes (mm)     
      inidepth = lakestate%water     ! water is earlier initiated as whole lake wolume
      !Redistribute initial lake volume by lake partitioning
      IF(initilake)THEN
        DO isb = 1,nsub
          IF(inidepth(1,isb)>slowlakeini(1,isb))THEN
            lakestate%slowwater(1,isb) = slowlakeini(1,isb)
            lakestate%water(1,isb) = inidepth(1,isb) - lakestate%slowwater(1,isb)
          ELSEIF(inidepth(1,isb)<=slowlakeini(1,isb))THEN
            lakestate%slowwater(1,isb) = inidepth(1,isb)
            lakestate%water(1,isb) = 0
          ENDIF
        ENDDO
      ENDIF
      IF(initolake)THEN
        DO isb = 1,nsub
          IF(inidepth(2,isb)>slowlakeini(2,isb))THEN
            lakestate%slowwater(2,isb) = slowlakeini(2,isb)
            lakestate%water(2,isb) = inidepth(2,isb) - lakestate%slowwater(2,isb)
          ELSEIF(inidepth(2,isb)<=slowlakeini(2,isb))THEN
            lakestate%slowwater(2,isb) = inidepth(2,isb)
            lakestate%water(2,isb) = 0
          ENDIF
        ENDDO
      ENDIF
      DEALLOCATE(inidepth)
    ENDIF

    !Initialize lake concentration (mg/L)
    IF(initN.OR.initP)THEN
      DO isb = 1,nsub
        IF(initP)THEN
          IF(ALLOCATED(lakedatapar))THEN
            lakestate%conc(i_pp,:,isb) = lakedatapar(lakedataparindex(isb,:),m_ldtpmean)
          ELSEIF(basin(isb)%parregion(regiondivision(m_tpmean))>0)THEN
            lakestate%conc(i_pp,:,isb) = regpar(m_tpmean,basin(isb)%parregion(regiondivision(m_tpmean)))
          ENDIF  
        ENDIF
        IF(initN)THEN
          IF(ALLOCATED(lakedatapar))THEN
            lakestate%conc(i_on,:,isb) = lakedatapar(lakedataparindex(isb,:),m_ldtnmean)*0.5
            lakestate%conc(i_in,:,isb) = lakedatapar(lakedataparindex(isb,:),m_ldtnmean)*0.5
          ELSEIF(basin(isb)%parregion(regiondivision(m_tnmean))>0)THEN
            lakestate%conc(i_on,:,isb) = regpar(m_tnmean,basin(isb)%parregion(regiondivision(m_tnmean)))*0.5
            lakestate%conc(i_in,:,isb) = regpar(m_tnmean,basin(isb)%parregion(regiondivision(m_tnmean)))*0.5
          ENDIF
        ENDIF
      ENDDO
    ENDIF

    IF(initC)THEN
      DO isb = 1,nsub
        IF(ALLOCATED(lakedatapar))THEN 
          lakestate%conc(i_oc,:,isb) = lakedatapar(lakedataparindex(isb,:),m_ldtocmean)
        ELSEIF(basin(isb)%parregion(regiondivision(m_tocmean))>0)THEN
          lakestate%conc(i_oc,:,isb) = regpar(m_tocmean,basin(isb)%parregion(regiondivision(m_tocmean)))
        ENDIF
      ENDDO
    ENDIF

    IF(i_t2>0)THEN
      DO isb = 1,nsub
        lakestate%conc(i_t2,:,isb) = genpar(m_iniT2)
      ENDDO
      lakestate%lowertemp(:,:) = genpar(m_iniT2)
      lakestate%uppertemp(:,:) = genpar(m_iniT2)
    ENDIF
    
    IF(inislow)THEN
      DO isb = 1,nsub
        DO j = 1,2
          lakestate%concslow(:,j,isb) = lakestate%conc(:,j,isb)
        ENDDO
      ENDDO
    ENDIF

    !Set TPmean-variable if phosphorus is not calculated by HYPE
    IF((initN.OR.initC.OR.initS).AND..NOT.initP)THEN
      DO isb = 1,nsub
        IF(ALLOCATED(lakedatapar))THEN
          lakestate%TPmean(1,isb) = lakedatapar(lakedataparindex(isb,1),m_ldtpmean)
          lakestate%TPmean(2,isb) = lakedatapar(lakedataparindex(isb,2),m_ldtpmean)
        ELSEIF(basin(isb)%parregion(regiondivision(m_tpmean))>0)THEN
          lakestate%TPmean(:,isb) = regpar(m_tpmean,basin(isb)%parregion(regiondivision(m_tpmean)))
        ENDIF  
      ENDDO
    ENDIF

  END SUBROUTINE initiate_lake_npc_state

  !>Calculate the maximum slowwater volume of lakes. Used for simulation of nutrients and organic carbon. 
  !!The variable is used for lake partitioning in fastflow and slow lake parts.    
  !>
  !>\b Consequences Module hypevariables variable slowlakeini may be set.
  !>
  !>\b Reference ModelDescription Chapter Rivers and lakes (Basic assumptions)
  !-----------------------------------------------------------------
  SUBROUTINE set_lake_slowwater_maxvolume(n,ildepth,oldepth,ldparindex,ldpar,initilake,initolake)

    USE HYPEVARIABLES, ONLY : slowlakeini  !OUT

    !Argument declarations
    INTEGER, INTENT(IN) :: n          !<number of subbasins
    REAL, INTENT(IN)    :: ildepth(n) !<ilake depth
    REAL, INTENT(IN)    :: oldepth(n) !<olake depth
    INTEGER, INTENT(IN) :: ldparindex(n,2)  !<lakedataparindex
    REAL, INTENT(IN)    :: ldpar(:)   !<deeplake parameter (lakedatapar(:,m_lddeeplake))
    LOGICAL, INTENT(IN) :: initilake  !<flag for ilake class existance
    LOGICAL, INTENT(IN) :: initolake  !<flag for olake class existance
    
    !Local variables
    INTEGER isb         !loop-variable
    REAL    deeplake    !model parameter deeplake
    REAL    inidepth(n)
    
    !Allocate variable for maximum slow lake part
    IF(.NOT. ALLOCATED(slowlakeini))  ALLOCATE(slowlakeini(2,n))
    slowlakeini = 0.    !mm
    !Set variable for maximum slow lake part for ilake to ilake depth or adjusted by parameter deeplake.
    IF(initilake)THEN
      inidepth = ildepth*1000.
      slowlakeini(1,:) = inidepth   !default
      DO isb = 1,n
        deeplake = ldpar(ldparindex(isb,1))
        IF(deeplake>0) slowlakeini(1,isb) = deeplake * inidepth(isb)           !ilake: slowlake target water stage (mm)
      ENDDO
    ENDIF
    !Set variable for maximum slow lake part for olake to olake depth or adjusted by parameter deeplake.
    IF(initolake)THEN
      inidepth = oldepth * 1000.
      slowlakeini(2,:) = inidepth   !default
      DO isb = 1,n
        deeplake = ldpar(ldparindex(isb,2))
        IF(deeplake>0) slowlakeini(2,isb) = deeplake * inidepth(isb)           !olake: slowlake target water stage (mm)
      ENDDO
    ENDIF

  END SUBROUTINE set_lake_slowwater_maxvolume
  
  !>\brief Calculate atmospheric deposition of N and P and add it
  !>to lakewater as load
  !>
  !>\b Reference ModelDescription Chapter Processes above ground (Atmospheric deposition of nitrogen and phosphorus)
  !----------------------------------------------------------------------------
  SUBROUTINE add_deposition_to_lake_as_load(i,pooltype,veg,month,areaij,wetsp,drypp,dep,sourcewet,sourcedry,lakestate)

    USE MODVAR, ONLY : simulate, &
                       numsubstances, &
                       i_in,i_pp,i_sp, &
                       depositiontype

    !Argument declarations
    INTEGER, INTENT(IN) :: i                          !<index of subbasin
    INTEGER, INTENT(IN) :: pooltype                   !<laketype: 1=ilake, 2=olake
    INTEGER, INTENT(IN) :: veg                        !<vegetation index
    INTEGER, INTENT(IN) :: month                      !<current month
    REAL, INTENT(IN)    :: areaij                     !<classarea (km2)
    REAL, INTENT(IN)    :: wetsp                      !<wet deposition SP (kg/km2/timestep)
    REAL, INTENT(IN)    :: drypp                      !<dry deposition PP (kg/km2/timestep)
    TYPE(DEPOSITIONTYPE), INTENT(IN) :: dep           !<tot deposition (kg/km2/timestep) [added as dry deposition]
    REAL, INTENT(OUT)   :: sourcewet(numsubstances)   !<wet deposition (kg/timestep)
    REAL, INTENT(OUT)   :: sourcedry(numsubstances)   !<dry deposition (kg/timestep)    
    TYPE(lakestatetype),INTENT(INOUT) :: lakestate    !<Lake state

    !Local variables
    REAL :: sourcedd(numsubstances)
    REAL :: sourcewd(numsubstances)    

    sourcedry = 0.
    sourcewet = 0.
    IF(i_in==0.AND.i_pp==0.AND.i_sp==0)RETURN

    !Prepare deposition
    sourcedd = 0.
    sourcewd = 0.
    IF(ALLOCATED(dep%indryload).AND.simulate%substance(i_in) .AND. veg>0) sourcedd(i_in) = dep%indryload(i,veg)
    IF(simulate%substance(i_pp)) sourcedd(i_pp) = drypp
    IF(ALLOCATED(dep%inloadwater).AND.simulate%substance(i_in)) sourcedd(i_in) = dep%inloadwater(i,month)
    IF(simulate%substance(i_sp)) sourcewd(i_sp) = wetsp

    !Add deposition of Inorg-N and PartP to lake
    IF(lakestate%water(pooltype,i)>0)THEN
      IF(SUM(sourcedd)>0.) CALL add_source_to_water(lakestate%water(pooltype,i),numsubstances,lakestate%conc(:,pooltype,i),sourcedd)
      IF(SUM(sourcewd)>0.) CALL add_source_to_water(lakestate%water(pooltype,i),numsubstances,lakestate%conc(:,pooltype,i),sourcewd)
    ELSE
      IF(SUM(sourcedd)>0.) CALL add_source_to_water(lakestate%slowwater(pooltype,i),numsubstances,lakestate%concslow(:,pooltype,i),sourcedd)
      IF(SUM(sourcewd)>0.) CALL add_source_to_water(lakestate%slowwater(pooltype,i),numsubstances,lakestate%concslow(:,pooltype,i),sourcewd)
    ENDIF

    !Calculate atmospheric deposition loads (kg/timestep)
    sourcedry = sourcedd*areaij
    sourcewet = sourcewd*areaij

    RETURN
    
  END SUBROUTINE add_deposition_to_lake_as_load
  
  !>\brief Calculate atmospheric deposition of N and P and add it
  !>to river water components as load
  !>
  !\b Reference ModelDescription Chapter Processes above ground (Atmospheric 
  !>deposition of nitrogen and phosphorus)
  !----------------------------------------------------------------------------
  SUBROUTINE add_deposition_to_river_as_load(i,pooltype,veg,month,areaij,wetsp,drypp,dep,sourcewet,sourcedry,riverstate)

    USE MODVAR, ONLY : simulate, &
                       numsubstances, &
                       i_in,i_pp,i_sp, &
                       depositiontype

    USE HYPEVARIABLES, ONLY : ttpart,ttstep

    !Argument declarations
    INTEGER, INTENT(IN) :: i                          !<index of subbasin
    INTEGER, INTENT(IN) :: pooltype                   !<rivertype: 1=lriver, 2=mriver
    INTEGER, INTENT(IN) :: veg                        !<vegetation index
    INTEGER, INTENT(IN) :: month                      !<current month
    REAL, INTENT(IN)    :: areaij                     !<classarea (km2)
    REAL, INTENT(IN)    :: wetsp                      !<wet deposition SP (kg/km2/timestep)
    REAL, INTENT(IN)    :: drypp                      !<dry deposition PP (kg/km2/timestep)
    TYPE(DEPOSITIONTYPE), INTENT(IN) :: dep           !<tot deposition (kg/km2/timestep) [added as dry deposition]
    REAL, INTENT(OUT)   :: sourcewet(numsubstances)   !< wet deposition (kg/timestep)
    REAL, INTENT(OUT)   :: sourcedry(numsubstances)   !< dry deposition (kg/timestep)    
    TYPE(riverstatetype),INTENT(INOUT) :: riverstate  !<River state

    !Local variables
    INTEGER l
    REAL sourcedd(numsubstances)
    REAL sourcewd(numsubstances)    
    REAL totvol
    REAL deposition(numsubstances)
    REAL fractionw,fractionpart,notused   !fractions of volume in river compartments
    REAL,ALLOCATABLE :: fractionqueue(:)  !fractions of volume in river compartments

    !>\b Algorithm \n
    sourcedry = 0.
    sourcewet = 0.
    IF(i_in==0.AND.i_pp==0.AND.i_sp==0)RETURN

    !>Prepare deposition
    IF(ALLOCATED(dep%indryload).AND.simulate%substance(i_in) .AND. veg>0) sourcedry(i_in) = dep%indryload(i,veg)*areaij   !kg/timestep 
    IF(simulate%substance(i_pp)) sourcedry(i_pp) = drypp*areaij
    IF(ALLOCATED(dep%inloadwater).AND.simulate%substance(i_in)) sourcedry(i_in) = dep%inloadwater(i,month)*areaij
    IF(simulate%substance(i_sp)) sourcewet(i_sp) = wetsp*areaij
    sourcedd = sourcedry*1000.   !g=mg/L*m3
    sourcewd = sourcewet*1000.   !g=mg/L*m3
    
    IF(SUM(sourcedd)>0. .OR. SUM(sourcewd)>0.)THEN
      !>Calculate fractions to be added to river water compartments
      ALLOCATE(fractionqueue(ttstep(pooltype,i)))
      CALL calculate_water_fractions(ttstep(pooltype,i),ttpart(pooltype,i),riverstate%water(pooltype,i),0.,riverstate%qqueue(1:ttstep(pooltype,i)+1,pooltype,i),totvol,fractionw,notused,fractionqueue,fractionpart)
      IF(totvol<=0)THEN
        IF(ALLOCATED(fractionqueue)) DEALLOCATE(fractionqueue)
        sourcedry = 0.
        sourcewet = 0.        
        RETURN
      ENDIF

      !>Add deposition of Inorg-N and PartP/SoluteP to river watercourse compartments
      IF(fractionw>0)THEN
        deposition = fractionw*(sourcedd + sourcewd)
        CALL add_source_to_water(riverstate%water(pooltype,i),numsubstances,riverstate%conc(:,pooltype,i),deposition)
      ENDIF
      DO l = 1,ttstep(pooltype,i)
        IF(fractionqueue(l)>0)THEN
          deposition = fractionqueue(l)*(sourcedd + sourcewd)
          CALL add_source_to_water(riverstate%qqueue(l,pooltype,i),numsubstances,riverstate%cqueue(:,l,pooltype,i),deposition)
        ENDIF
      ENDDO
      IF(fractionpart>0)THEN
        l = ttstep(pooltype,i) + 1
        deposition = fractionpart*(sourcedd + sourcewd)
        CALL add_source_to_water(riverstate%qqueue(l,pooltype,i),numsubstances,riverstate%cqueue(:,l,pooltype,i),deposition)
      ENDIF
    ENDIF
    IF(ALLOCATED(fractionqueue)) DEALLOCATE(fractionqueue)

  END SUBROUTINE add_deposition_to_river_as_load  
  
  !>\brief Calculate nutrient and sediment processes in river 
  !> This include denitrification, mineralisation, primary production,
  !!sedimentation, resuspension, exchange with sediment
  !>
  !>\b Reference ModelDescription Chapter Nitrogen and phosphorus processes in rivers and lakes (Common things in lakes and river,
  !> Denitrification, Primary production and mineralization, and Sedimentation/Resuspension)  
  !---------------------------------------------------------------------------
  SUBROUTINE np_processes_in_river(i,itype,area,depth,transq,Qbank,denpar, &
                                   denparl,hsatINpar,prodNpar,prodPpar,hsatTPpar, &
                                   sedexppar,limpppar,riverstate)   

    USE MODVAR, ONLY : conduct, &
                       simulate, &
                       i_in,i_sp

    !Argument declarations
    INTEGER, INTENT(IN) :: i         !<index of current subbasin
    INTEGER, INTENT(IN) :: itype     !<river type (local or main)
    REAL, INTENT(IN)    :: area      !<river area (m2)
    REAL, INTENT(IN)    :: depth     !<river depth (m)   
    REAL, INTENT(IN)    :: transq    !<flow out of translation box chain (m3/s)
    REAL, INTENT(IN)    :: qbank     !<bank full river flow
    REAL, INTENT(IN)    :: denpar    !<model parameter denitrification rate (kg/m2/day)
    REAL, INTENT(IN)    :: denparl   !<model parameter denitrification rate, local river (kg/m2/day)
    REAL, INTENT(IN)    :: hsatINpar !<model parameter half saturation IN (mg/L)
    REAL, INTENT(IN)    :: prodNpar  !<model parameter production ON 
    REAL, INTENT(IN)    :: prodPpar  !<model parameter production PP
    REAL, INTENT(IN)    :: hsatTPpar !<model parameter half saturation TP (mg/L)
    REAL, INTENT(IN)    :: sedexppar !<sedimentation/resuspension parameter (mg/L)
    REAL, INTENT(IN)    :: limpppar  !<limitation of sedimentation parameter (mg/L)
    TYPE(riverstatetype),INTENT(INOUT) :: riverstate  !<River states

    !Local parameters
    INTEGER, PARAMETER :: systemtype = 2    !river system

    !Calculate the nutrient processes
    IF(area>0)THEN
      IF(i_sp>0) CALL calculate_river_tpmean(i,itype,riverstate)
      IF(simulate%substance(i_in)) THEN
        IF(itype==1)THEN
          CALL denitrification_water(i,itype,systemtype,area,denparl,hsatINpar,RIVERSTATE=riverstate)  !denitrification in local rivers
        ELSE
          CALL denitrification_water(i,itype,systemtype,area,denpar,hsatINpar,RIVERSTATE=riverstate)  !denitrification in rivers
        ENDIF
      ENDIF
      CALL production_mineralisation(i,itype,systemtype,area,prodNpar,prodPpar,hsatTPpar,limpppar,RIVERSTATE=riverstate,DEPTH=depth) !mineraliation and primary production in rivers  
      IF(conduct%simP.OR.conduct%simS) CALL sedimentation_resuspension(i,itype,area,sedexppar,transq,Qbank,depth,riverstate) !sedimentation and resuspension of part P in rivers  
       !    No sediment SRP exchange. The concentration smoothed with deadvolume
    ENDIF

  END SUBROUTINE np_processes_in_river


  !>\brief Calculate nutrient and sediment processes in lake: 
  !!denitrification, mineralisation, primary production,
  !!sedimentation, internal load
  !>
  !>\b Reference ModelDescription Chapter Nitrogen and phosphorus processes in rivers and lakes (Common things in lakes and river,
  !> Denitrification, Primary production and mineralization, Sedimentation/Resuspension and Internal load)  
  !------------------------------------------------------------------
  SUBROUTINE np_processes_in_lake(i,itype,area,denpar,hsatINpar,prodNpar, &
                                  prodPpar,hsatTPpar,sedonpar,sedpppar,sedsspar,sedaepar,  &
                                  limonpar,limpppar,limsspar,lakestate)

    USE MODVAR, ONLY : conduct, &
                       i_on,i_pp,i_ss,i_ae

    !Argument declarations
    INTEGER, INTENT(IN) :: i          !<index of subbasin
    INTEGER, INTENT(IN) :: itype      !<lake type (ilake or olake)
    REAL, INTENT(IN)    :: area       !<lake area (m2)
    REAL, INTENT(IN)    :: denpar     !<model parameter denitrification rate (kg/m2/day)
    REAL, INTENT(IN)    :: hsatINpar  !<model parameter half saturation IN (mg/L)
    REAL, INTENT(IN)    :: prodNpar   !<model parameter production ON 
    REAL, INTENT(IN)    :: prodPpar   !<model parameter production PP 
    REAL, INTENT(IN)    :: sedonpar   !<ON sedimentation rate  (lakes)
    REAL, INTENT(IN)    :: sedpppar   !<PP sedimentation rate  (lakes)
    REAL, INTENT(IN)    :: sedsspar   !<SS sedimentation rate  (lakes)
    REAL, INTENT(IN)    :: sedaepar   !<AE sedimentation rate  (lakes)
    REAL, INTENT(IN)    :: hsatTPpar  !<model parameter half saturation TP (mg/L)
    REAL, INTENT(IN)    :: limonpar   !<limitation of sedimentation ON parameter (mg/L)
    REAL, INTENT(IN)    :: limpppar   !<limitation of sedimentation PP parameter (mg/L)
    REAL, INTENT(IN)    :: limsspar   !<limitation of sedimentation SS parameter (mg/L)
    TYPE(lakestatetype),INTENT(INOUT) :: lakestate  !<Lake state
    
    !Local parameters
    INTEGER, PARAMETER :: systemtype = 1    !lake

    !Calculate the nutrient processes
    IF(conduct%simP) CALL calculate_lake_tpmean(i,itype,lakestate)
    IF(conduct%simN)THEN
      CALL denitrification_water(i,itype,systemtype,area,denpar,hsatINpar,LAKESTATE=lakestate) !denitrification in lakes
    ENDIF
    CALL production_mineralisation(i,itype,systemtype,area,prodNpar,prodPpar,hsatTPpar,limpppar,LAKESTATE=lakestate)  !primary production and mineralisation in lakes
    IF(conduct%simN) CALL lake_sedimentation(i,i_on,itype,sedonpar,limonpar,lakestate)
    IF(conduct%simP) CALL lake_sedimentation(i,i_pp,itype,sedpppar,limpppar,lakestate)
    IF(conduct%simS)THEN
      CALL lake_sedimentation(i,i_ss,itype,sedsspar,limsspar,lakestate)
      CALL lake_sedimentation(i,i_ae,itype,sedaepar,0.,lakestate)
    ENDIF
    IF(conduct%simP) CALL internal_lake_load(i,itype,systemtype,area,lakestate)  !internal load of phosphorus

  END SUBROUTINE np_processes_in_lake

  !>\brief Calculates the denitrification in river and lakes
  !!Lake processes in slow turnover lake part. 
  !>
  !>\b Reference ModelDescription Chapter Nitrogen and phosphorus processes in rivers and lakes (Denitrification)
  !-----------------------------------------------------------------------
  SUBROUTINE denitrification_water(i,watertype,systemtype,area,denpar,halfsatINwater,riverstate,lakestate)

    USE MODVAR, ONLY : i_in
    USE HYPEVARIABLES, ONLY : maxdenitriwater   

    !Argument declarations
    INTEGER, INTENT(IN) :: i          !<index of current subbasin
    INTEGER, INTENT(IN) :: watertype  !<Lake or river type (1=local, 2=main/outlet)
    INTEGER, INTENT(IN) :: systemtype !<aquatic system type (1=lake, 2=river)
    REAL, INTENT(IN)    :: area       !<lake surface area/river bottom area (m2)
    REAL, INTENT(IN)    :: denpar     !<model parameter denitrification rate (kg/m2/day)
    REAL, INTENT(IN)    :: halfsatINwater  !<model parameter half saturation IN (mg/L)
    TYPE(riverstatetype),INTENT(INOUT),OPTIONAL :: riverstate  !<River states
    TYPE(lakestatetype),INTENT(INOUT),OPTIONAL  :: lakestate   !<Lake states

    !Local variables
    REAL, DIMENSION(1) :: denitri_water, inorganicNpool
    REAL tmpfcn, concfcn, watertemp, waterconc,vol
    
    !Local parameters
    INTEGER, PARAMETER :: pooldim = 1

    !Initial pools and values
    IF(systemtype==1) THEN   !lakes
      vol = lakestate%slowwater(watertype,i) * area * 1.0E-6    !0.001 m3
      waterconc = lakestate%concslow(i_in,watertype,i)          !mg/L
      inorganicNpool = vol * waterconc                          !kg
      watertemp = lakestate%temp(watertype,i)
    ELSE                     !rivers
      vol = riverstate%water(watertype,i) * 1.0E-3
      waterconc = riverstate%conc(i_in,watertype,i)
      inorganicNpool = vol * waterconc     !kg      
      watertemp = riverstate%temp(watertype,i)
    ENDIF

    !Temperature and concentration dependence factor
    tmpfcn  = tempfactor(watertemp)
    concfcn = halfsatconcfactor(waterconc,halfsatINwater)

    !Denitrification    
    denitri_water = denpar * area * concfcn * tmpfcn   !kg  
    denitri_water = MIN(maxdenitriwater*inorganicNpool, denitri_water)    !max 50% kan be denitrified
    CALL retention_pool(pooldim, inorganicNPool, denitri_water)
    IF(systemtype==1) THEN   !lakes
      CALL new_concentration(inorganicNpool(1),vol,lakestate%concslow(i_in,watertype,i))
    ELSE                     !rivers
      IF(riverstate%water(watertype,i) > 0.) THEN
        CALL new_concentration(inorganicNpool(1),vol,riverstate%conc(i_in,watertype,i))
      ENDIF
    ENDIF

  END SUBROUTINE denitrification_water

  !>\brief Calculates transformation between IN/ON and SRP/PP in water.
  !!Also simulated algae production (=ON-production) for sediment simulation.
  !!Simulating the combined processes of primary production and
  !!mineralisation. Lake process in slow turnover lake part. 
  !>
  !>\b Reference ModelDescription Chapter Nitrogen and phosphorus processes 
  !>in rivers and lakes (Primary production and mineralization)  
  !-----------------------------------------------------------------------
  SUBROUTINE production_mineralisation(i,watertype,systemtype,area, &
                                       prodNpar,prodPpar,halfsatTPwater,  &
                                       limpppar,riverstate,lakestate,depth)

    USE MODVAR, ONLY : conduct, &
                       i_in,i_on,i_sp,i_pp,i_ae
    USE HYPEVARIABLES, ONLY : maxprodwater,   &
                              maxdegradwater, &
                              NPratio

    !Argument declarations
    INTEGER, INTENT(IN) :: i          !<index of current subbasin
    INTEGER, INTENT(IN) :: watertype  !<Lake or river type (1=local, 2=main/outlet)
    INTEGER, INTENT(IN) :: systemtype !<aquatic system type (1=lake, 2=river)
    REAL, INTENT(IN)    :: area       !<lake surface area/ river bottom area (m2)
    REAL, INTENT(IN)    :: prodNpar   !<model parameter production rate ON in water
    REAL, INTENT(IN)    :: prodPpar   !<model parameter production rate PP in water
    REAL, INTENT(IN)    :: halfsatTPwater !<model parameter half saturation TP (mg/L)
    REAL, INTENT(IN)    :: limpppar   !<limitation of sedimentation parameter (mg/L)
    TYPE(riverstatetype),INTENT(INOUT),OPTIONAL :: riverstate !<River states
    TYPE(lakestatetype),INTENT(INOUT),OPTIONAL  :: lakestate  !<Lake states
    REAL, INTENT(IN), OPTIONAL :: depth      !<river depth (m) 

    !Local variables
    REAL, DIMENSION(1) :: ONpool, INpool, SRPpool,PPpool,AEpool
    REAL, DIMENSION(1) :: minprodN, minprodP,minprodAE
    REAL watertemp,waterTPmean,temp10,temp20
    REAL tmpfcn, tmpfcn1, tmpfcn2, TPfcn
    REAL vol
    REAL waterdepth     !m

    !Local parameters
    INTEGER, PARAMETER :: pooldim = 1

    IF(.NOT.(conduct%simN.OR.conduct%simP.OR.conduct%simS)) RETURN

    !Pools of nutrients in the water, water temperature and fraction of depth of water volume that is active
    IF (systemtype==1)THEN   !lakes
      vol = lakestate%slowwater(watertype,i) * area / 1.0E6
      IF(conduct%simN)THEN 
        INpool = vol * lakestate%concslow(i_in,watertype,i)   !kg
        ONpool = vol * lakestate%concslow(i_on,watertype,i)   !kg
      ENDIF
      IF(conduct%simP)THEN
        SRPpool = vol * lakestate%concslow(i_sp,watertype,i) !kg
        PPpool  = vol * lakestate%concslow(i_pp,watertype,i)  !kg
      ENDIF
      IF(conduct%simS) AEpool = vol * lakestate%concslow(i_ae,watertype,i) !kg N
    ELSE                     !rivers
      vol = riverstate%water(watertype,i) / 1.0E3
      IF(conduct%simN)THEN
        INpool = vol * riverstate%conc(i_in,watertype,i) !kg
        ONpool = vol * riverstate%conc(i_on,watertype,i) !kg
      ENDIF
      IF(conduct%simP)THEN
        SRPpool = vol * riverstate%conc(i_sp,watertype,i)    !kg    
        PPpool  = vol * riverstate%conc(i_pp,watertype,i)    !kg
      ENDIF
      IF(conduct%simS) AEpool = vol * riverstate%conc(i_ae,watertype,i)    !kg N 
    ENDIF

    !Set help variables
    IF (systemtype==1) THEN   !lakes
      watertemp = lakestate%temp(watertype,i)
      waterdepth = lakestate%slowwater(watertype,i) * 1.E-3
      waterTPmean = lakestate%TPmean(watertype,i)
      temp10 = lakestate%temp10(watertype,i)
      temp20 = lakestate%temp20(watertype,i)
    ELSE                     !rivers
      watertemp = riverstate%temp(watertype,i)  
      waterdepth = depth
      waterTPmean = riverstate%TPmean(watertype,i)
      temp10 = riverstate%temp10(watertype,i)
      temp20 = riverstate%temp20(watertype,i)
    ENDIF

    IF(watertemp > 0.)THEN
      !Total phosphorus concentration dependent factor
      TPfcn = halfsatconcfactor(MAX(waterTPmean-limpppar,0.),halfsatTPwater)

      !Temperature dependent factor
      tmpfcn1 = watertemp / 20.    
      tmpfcn2 = (temp10 - temp20) / 5.
      tmpfcn = tmpfcn1*tmpfcn2

      !Production/mineralisation
      IF(conduct%simN.OR.conduct%simS)THEN
        minprodN = prodNpar * TPfcn * tmpfcn * waterdepth * area  !kg
        IF(conduct%simN)THEN
          IF(minprodN(1) > 0.) THEN  !production        
            minprodN = MIN(maxprodwater * INpool, minprodN)
          ELSE                       !mineralisation
            minprodN = MAX(-maxdegradwater * ONpool, minprodN)
          ENDIF
          CALL retention_pool(pooldim,INpool,minprodN)   !minprodN may be negative
          CALL production_pool(pooldim,ONpool,minprodN)
        ENDIF
        IF(conduct%simS)THEN
          minprodAE = MAX(-AEpool, minprodN)  !AE can be produced from zero, thus no max depending on pool.
          CALL production_pool(pooldim,AEpool,minprodAE) !AEpool is a part of ON pool
        ENDIF
      ENDIF
      IF(conduct%simP)THEN
        minprodP = prodPpar * NPratio * TPfcn * tmpfcn * waterdepth * area  !kg  
        IF(minprodP(1) > 0.) THEN  !production        
          minprodP = MIN(maxprodwater * SRPpool,minprodP)
        ELSE                       !mineralisation
          minprodP = MAX(-maxdegradwater * PPpool,minprodP)
        ENDIF
        CALL retention_pool(pooldim,SRPpool,minprodP)    !minprodP may be negative
        CALL production_pool(pooldim,PPpool,minprodP)
      ENDIF

      !New concentration due to changes in pools
      IF(systemtype==1) THEN            !lakes
        IF(conduct%simN) CALL new_concentration(INpool(1),vol,lakestate%concslow(i_in,watertype,i))
        IF(conduct%simN) CALL new_concentration(ONpool(1),vol,lakestate%concslow(i_on,watertype,i))
        IF(conduct%simP) CALL new_concentration(SRPpool(1),vol,lakestate%concslow(i_sp,watertype,i))
        IF(conduct%simP) CALL new_concentration(PPpool(1),vol,lakestate%concslow(i_pp,watertype,i))
        IF(conduct%simS) CALL new_concentration(AEpool(1),vol,lakestate%concslow(i_ae,watertype,i))
      ELSE                              !rivers
        IF(riverstate%water(watertype,i) > 0.) THEN
          IF(conduct%simN) CALL new_concentration(INpool(1),vol,riverstate%conc(i_in,watertype,i))
          IF(conduct%simN) CALL new_concentration(ONpool(1),vol,riverstate%conc(i_on,watertype,i))
          IF(conduct%simP) CALL new_concentration(SRPpool(1),vol,riverstate%conc(i_sp,watertype,i))
          IF(conduct%simP) CALL new_concentration(PPpool(1),vol,riverstate%conc(i_pp,watertype,i))
          IF(conduct%simS) CALL new_concentration(AEpool(1),vol,riverstate%conc(i_ae,watertype,i))          
        ENDIF
      ENDIF
    ENDIF

  END SUBROUTINE production_mineralisation

  !>\brief Calculate sedimentation of substance in lake
  !!Only substance in in slow turnover lake part sediments.
  !>
  !>\b Reference ModelDescription Chapter Nitrogen and phosphorus processes in 
  !>rivers and lakes (Sedimentation/Resuspension) and Organic carbon (River and lakes - Sedimentation)
  !--------------------------------------------------------------------------
  SUBROUTINE lake_sedimentation(i,substance,watertype,sedrate,limsedpar,lakestate)

    !Argument declarations
    INTEGER, INTENT(IN) :: i          !<current index of subbasin
    INTEGER, INTENT(IN) :: substance  !<current index of substance (PP,ON,SS,AE)
    INTEGER, INTENT(IN) :: watertype  !<Lake type (1=local, 2=outlet)
    REAL, INTENT(IN)    :: sedrate    !<sedimentation rate  (lakes) (m/ts)
    REAL, INTENT(IN)    :: limsedpar  !<concentration limit for sedimentation (mg/L)
    TYPE(lakestatetype),INTENT(INOUT) :: lakestate  !<Lake state
    
    !Local variables
    REAL, DIMENSION(1) :: pool     !substance pool in water per area (g/m2) ((kg))
    REAL, DIMENSION(1) :: delta    !sedimentation (g/m2/ts)
    REAL depth                     !lake average depth (m)

    !>/b Algoritm
    !>Calculate water volume, amount of substance in water (pool) and the sedimentation
    depth = lakestate%slowwater(watertype,i)  * 1.0E-3        !m
    pool = depth * lakestate%concslow(substance,watertype,i) !mg/l*m
    delta = sedrate * MAX(lakestate%concslow(substance,watertype,i)-limsedpar,0.) !m/ts*mg/l

    !>Remove sedimentation from the pool
    CALL retention_pool(1,pool,delta)

    !>Calculate the new concentration in the water due to the change in the pool
    CALL new_concentration(pool(1),depth,lakestate%concslow(substance,watertype,i))

  END SUBROUTINE lake_sedimentation

  !>\brief Calculate sedimentation and resuspension of PP and SS in rivers.
  !>
  !>\b Reference ModelDescription Chapter Nitrogen and phosphorus processes in rivers and lakes (Sedimentation/Resuspension)
  !-----------------------------------------------------------------
  SUBROUTINE sedimentation_resuspension(i,watertype,area,sedexppar,riverq,qbank,depth,riverstate)

    USE MODVAR, ONLY : simulate,i_pp,i_ss

    !Argument declaration
    INTEGER, INTENT(IN) :: i          !<index of current subbasin
    INTEGER, INTENT(IN) :: watertype  !<river type (1=local, 2=main)
    REAL, INTENT(IN)    :: area       !<river surface area (m2)
    REAL, INTENT(IN)    :: sedexppar  !<sedimentation/resuspension parameter
    REAL, INTENT(IN)    :: riverq     !<river discharge (m3/s)
    REAL, INTENT(IN)    :: qbank      !<bank full flow (m3/s)
    REAL, INTENT(IN)    :: depth      !<river depth (m) 
    TYPE(riverstatetype),INTENT(INOUT) :: riverstate  !<River states

    !Local variables
    REAL, DIMENSION(1) :: PPpool,SSpool    !pools in water (kg)
    REAL, DIMENSION(1) :: transfer         !changes (kg/d)
    REAL, DIMENSION(1) :: tempsed,tempsed2 !temparary variables for river sediment pools (kg)
    REAL sedresp, help, qbankcorr

    !>/b Algoritm
    !>Initial check if calculation is to be made
    IF(sedexppar == 0) RETURN
    IF(area>0. .AND. qbank>0.) THEN

      !>Assess pool of PP and SS in water and sediment
      IF(simulate%substance(i_pp))THEN
        tempsed(1) = riverstate%Psed(watertype,i)
        PPpool = (riverstate%water(watertype,i) * riverstate%conc(i_pp,watertype,i))* 1.0E-3 !kg
      ENDIF
      IF(simulate%substance(i_ss))THEN
        tempsed2(1) = riverstate%Ssed(watertype,i)
        SSpool = (riverstate%water(watertype,i) * riverstate%conc(i_ss,watertype,i))* 1.0E-3 !kg
      ENDIF

      !>Calculate sedimentation and resuspension factor
      qbankcorr = 0.7*qbank
      help = 0.
      IF(qbankcorr-riverq>0.) help = help + ((qbankcorr-riverq)/qbankcorr)**sedexppar   !sedimentation at low flow
      IF(riverq>0) help = help - (riverq/qbankcorr)**sedexppar  !resuspension at all flows
      sedresp = MAX(-1., MIN(1.,help))
      
      !>Transfer PP and SS between sediment and water pools
      IF(sedresp>0.)THEN  !sedimentation
        IF(simulate%substance(i_pp))THEN
          transfer = sedresp * (riverstate%conc(i_pp,watertype,i) * MIN(riverstate%water(watertype,i),area * depth)) / 1.0E3  
          CALL retention_pool(1,PPpool,transfer)           !transfer may change
          CALL production_pool(1,tempsed,transfer)
        ENDIF
        IF(simulate%substance(i_ss))THEN
          transfer = sedresp * (riverstate%conc(i_ss,watertype,i) * MIN(riverstate%water(watertype,i),area * depth)) / 1.0E3  
          CALL retention_pool(1,SSpool,transfer)           !transfer may change
          CALL production_pool(1,tempsed2,transfer)
        ENDIF
      ELSE                !resuspension
        IF(simulate%substance(i_pp))THEN
          transfer = - sedresp * tempsed 
          CALL retention_pool(1,tempsed,transfer)          !transfer may change
          CALL production_pool(1,PPpool,transfer)
        ENDIF
        IF(simulate%substance(i_ss))THEN
          transfer = - sedresp * tempsed2
          CALL retention_pool(1,tempsed2,transfer)         !transfer may change
          CALL production_pool(1,SSpool,transfer)
        ENDIF
      ENDIF

      !>Update state variables
      IF(simulate%substance(i_pp))THEN
        riverstate%Psed(watertype,i) = tempsed(1)
        CALL new_concentration(PPpool(1),riverstate%water(watertype,i)*1.0E-3,riverstate%conc(i_pp,watertype,i))
      ENDIF
      IF(simulate%substance(i_ss))THEN
        riverstate%Ssed(watertype,i) = tempsed2(1)
        CALL new_concentration(SSpool(1),riverstate%water(watertype,i)*1.0E-3,riverstate%conc(i_ss,watertype,i))
      ENDIF
    ENDIF

  END SUBROUTINE sedimentation_resuspension

  !>\brief Calculates straight 365-day running average mean of TP
  !>concentration in lake
  !>
  !>\b Reference ModelDescription Chapter Nitrogen and phosphorus processes in rivers and lakes (Primary production 
  !> and mineralization) and Organic carbon (River and Lakes - Primary production and mineralization)
  !-----------------------------------------------------------------------
  SUBROUTINE calculate_lake_tpmean(i,watertype,lakestate)

    USE MODVAR, ONLY : i_sp,i_pp

    !Argument declarations
    INTEGER, INTENT(IN) :: i         !<index of current subbasin
    INTEGER, INTENT(IN) :: watertype !<Lake type (1=local, 2=outlet)
    TYPE(lakestatetype),INTENT(INOUT) :: lakestate  !<Lake state
    
    !Local variables
    REAL meanconc

    IF(lakestate%water(watertype,i)+lakestate%slowwater(watertype,i)>0.)THEN
      meanconc = (lakestate%water(watertype,i)*(lakestate%conc(i_sp,watertype,i)+lakestate%conc(i_pp,watertype,i)) +           & 
           lakestate%slowwater(watertype,i)*(lakestate%concslow(i_sp,watertype,i) + lakestate%concslow(i_pp,watertype,i)))/  &
           (lakestate%water(watertype,i)+lakestate%slowwater(watertype,i))
      lakestate%TPmean(watertype,i) = lakestate%TPmean(watertype,i) + (meanconc - lakestate%TPmean(watertype,i))/365. 
    ENDIF

  END SUBROUTINE calculate_lake_tpmean

  !>\brief Calculates straight 365-day running average mean of TP
  !>concentration in river
  !>
  !>\b Reference ModelDescription Chapter  Nitrogen and phosphorus processes in rivers and lakes (Primary production 
  !> and mineralization) and Organic carbon (River and Lakes - Primary production and mineralization)
  !-------------------------------------------------------------------
  SUBROUTINE calculate_river_tpmean(i,watertype,riverstate)

    USE MODVAR, ONLY : i_sp,i_pp

    !Argument declarations
    INTEGER, INTENT(IN) :: i         !<index of current subbasin
    INTEGER, INTENT(IN) :: watertype !<River type (1=local, 2=main)
    TYPE(riverstatetype),INTENT(INOUT) :: riverstate  !<River states

    riverstate%TPmean(watertype,i) = riverstate%TPmean(watertype,i) + (riverstate%conc(i_sp,watertype,i) &
         + riverstate%conc(i_pp,watertype,i) - riverstate%TPmean(watertype,i))/365. 

  END SUBROUTINE calculate_river_tpmean

  !>\brief Calculates and add internal load of phosphorus for lakes
  !!Lake processes in slow turnover lake part
  !>
  !>\b Reference ModelDescription Chapter Nitrogen and phosphorus processes in rivers and lakes (Internal load)
  !-------------------------------------------------------------
  SUBROUTINE internal_lake_load(i,watertype,systemtype,area,lakestate)

    USE MODVAR, ONLY : lakeindex,   &
                       lakedatapar, &
                       lakedataparindex,  &
                       i_sp,i_pp,   &
                       numsubstances
    USE HYPEVARIABLES, ONLY : m_ldprodpp,  &
                              m_ldprodsp       

    !Argument declarations
    INTEGER, INTENT(IN) :: i           !<index of current subbasin
    INTEGER, INTENT(IN) :: watertype   !<Lake or river type (1=local, 2=main/outlet)
    INTEGER, INTENT(IN) :: systemtype  !<aquatic system type (1=lake, 2=river)
    REAL, INTENT(IN)    :: area        !<lake surface area/ river bottom area (m2)
    TYPE(lakestatetype),INTENT(INOUT) :: lakestate  !<Lake state

    !Local variables
    REAL prodPP, prodSP
    REAL tmpfcn, TPfcn
    REAL vol
    REAL pppar,sppar
    REAL :: sourceP(numsubstances)
    !Local parameters
    INTEGER, PARAMETER :: pooldim = 1

    !>\b Algorithm \n
    !>Check if internal phosphorus load is to be calculated
    IF(systemtype==2) RETURN   !river
    IF(watertype==1) RETURN    !local
    IF(.NOT.ALLOCATED(lakeindex)) RETURN  !no special lakes
    pppar = lakedatapar(lakedataparindex(i,watertype),m_ldprodpp)
    sppar = lakedatapar(lakedataparindex(i,watertype),m_ldprodsp)
    IF(pppar==0 .AND. sppar==0) RETURN
    sourceP = 0.

    !>Calculate pool of P, and concentration and temperature dependent factors
    TPfcn = 0.1
    tmpfcn = 0.86**(ABS(lakestate%temp(watertype,i)-15.))   !laketemp=T20 for olake

    !> Calculate internal load of phosphorus
    prodPP = pppar * TPfcn * tmpfcn * area / 1000.  !kg/d
    prodSP = sppar * TPfcn * tmpfcn * area / 1000.  !kg/d
    sourceP(i_pp) = prodPP
    sourceP(i_sp) = prodSP

    !>Add internal load of phosphorus to lake water
    vol = lakestate%slowwater(watertype,i) * area / 1.0E6
    CALL add_source_to_water(vol,numsubstances,lakestate%concslow(:,watertype,i),sourceP)

  END SUBROUTINE internal_lake_load

  !>Calculate organic carbon processes in river; mineralisation,
  !>primary production
  !>
  !>\b Reference ModelDescription Chapter Organic carbon (River and Lakes - Primary production and mineralization)
  !----------------------------------------------------------------
  SUBROUTINE oc_processes_in_river(i,watertype,area,depth,prodpar,hsatTPpar,limpppar,riverstate)   

    USE MODVAR, ONLY : i_oc

    !Argument declarations
    INTEGER, INTENT(IN) :: i          !<index of current subbasin
    INTEGER, INTENT(IN) :: watertype  !<river type (local or main)
    REAL, INTENT(IN)    :: area       !<river area (m2)
    REAL, INTENT(IN)    :: depth      !<river depth (m)   
    REAL, INTENT(IN)    :: prodpar    !<model parameter production OC 
    REAL, INTENT(IN)    :: hsatTPpar  !<model parameter half saturation TP (mg/L)
    REAL, INTENT(IN)    :: limpppar   !<limitation of sedimentation parameter (mg/L)
    TYPE(riverstatetype),INTENT(INOUT) :: riverstate  !<River states
    
    !Local parameters
    INTEGER, PARAMETER :: systemtype = 2    !river system

    IF(i_oc==0)RETURN

    !Calculate the organic carbon processes
    IF(area>0)THEN
      CALL oc_production_mineralisation(systemtype,area,prodpar,hsatTPpar,limpppar,       &
              riverstate%water(watertype,i),riverstate%conc(i_oc,watertype,i),  &
              riverstate%temp(watertype,i),riverstate%TPmean(watertype,i),      &
              riverstate%temp10(watertype,i),riverstate%temp20(watertype,i),depth) 
    ENDIF

  END SUBROUTINE oc_processes_in_river

  !>\brief Calculate organic carbon processes in lake 
  !!Mineralisation, primary production, sedimentation
  !>
  !>\b Reference ModelDescription Chapter Organic carbon (River and Lakes)
  !------------------------------------------------------------------
  SUBROUTINE oc_processes_in_lake(i,watertype,area,prodpar,hsatTPpar,limpppar,sedocpar,lakestate)

    USE MODVAR, ONLY : i_oc

    !Argument declarations
    INTEGER, INTENT(IN) :: i                        !<current index of subbasin
    INTEGER, INTENT(IN) :: watertype                !<lake type (ilake or olake)
    REAL, INTENT(IN)    :: area                     !<lake area (m2)
    REAL, INTENT(IN)    :: prodpar                  !<model parameter production OC
    REAL, INTENT(IN)    :: hsatTPpar                !<model parameter half saturation TP (mg/L)
    REAL, INTENT(IN)    :: limpppar                 !<limitation of sedimentation parameter (mg/L)
    REAL, INTENT(IN)    :: sedocpar                 !<OC sedimentation rate  (lakes)
    TYPE(lakestatetype),INTENT(INOUT) :: lakestate  !<Lake state
    
    !Local parameters
    INTEGER, PARAMETER :: systemtype = 1    !lake

    IF(i_oc==0) RETURN

    !Calculate the nutrient processes
    CALL oc_production_mineralisation(systemtype,area,prodpar,hsatTPpar,limpppar,   &
            lakestate%slowwater(watertype,i),lakestate%concslow(i_oc,watertype,i),  &
            lakestate%temp(watertype,i),lakestate%TPmean(watertype,i),              &
            lakestate%temp10(watertype,i),lakestate%temp20(watertype,i))
    CALL lake_sedimentation(i,i_oc,watertype,sedocpar,0.,lakestate)


  END SUBROUTINE oc_processes_in_lake

  !>\brief Calculates transformation between OC/DIC in water 
  !!Simulating the combined processes of primary production and
  !!mineralisation.
  !>
  !>\b Reference ModelDescription Organic carbon (River and lakes - Primary production 
  !> and mineralization)
  !----------------------------------------------------------------
  SUBROUTINE oc_production_mineralisation(systemtype,area,prodpar,halfsatTPwater, &
                             limpppar,water,conc,watertemp,waterTPmean,temp10,temp20,depth)

    USE HYPEVARIABLES, ONLY : maxdegradwater, &
                              NCratio

    !Argument declarations
    INTEGER, INTENT(IN)        :: systemtype  !<aquatic system type (1=lake, 2=river)
    REAL, INTENT(IN)           :: area        !<lake surface area/ river bottom area (m2)
    REAL, INTENT(IN)           :: prodpar     !<model parameter production rate OC in water
    REAL, INTENT(IN)           :: halfsatTPwater !<model parameter half saturation TP (mg/L)
    REAL, INTENT(IN)           :: limpppar    !<limitation of sedimentation parameter (mg/L)
    REAL, INTENT(IN)           :: water       !<river or lake water (mm or m3)
    REAL, INTENT(INOUT)        :: conc        !<OC concentration of river or lake
    REAL, INTENT(IN)           :: watertemp   !<water temperature
    REAL, INTENT(IN)           :: waterTPmean !<water TP mean
    REAL, INTENT(IN)           :: temp10      !<10-day water temperature
    REAL, INTENT(IN)           :: temp20      !<20-day water temperature
    REAL, INTENT(IN), OPTIONAL :: depth       !<river depth (m) 
    
    !Local variables
    REAL, DIMENSION(1) :: OCpool, minprodN, minprodC,minC,prodC
    REAL tmpfcn, tmpfcn1, tmpfcn2, TPfcn
    REAL vol
    REAL waterdepth !(m)
    
    !Local parameter
    INTEGER, PARAMETER :: pooldim = 1

    !>\b Algorithm \n
    !>Calculate pools of organic carbon in the water, water temperature 
    !>and fraction of depth of water volume that is active
    IF(systemtype==1) THEN   !lakes
      OCpool = (water * area * conc) /1.0E6  !kg
      waterdepth = water/1000.
    ELSE                     !rivers
      OCpool = (water * conc)/ 1.0E3 !kg
      waterdepth=depth
    ENDIF

    !>Calculate dependency factors (Tot-P and temperature)
    TPfcn = halfsatconcfactor(waterTPmean-limpppar,halfsatTPwater)
    IF(watertemp >= 0.) THEN
      tmpfcn1 = watertemp / 20.    
    ELSE 
      tmpfcn1 = 0.
    ENDIF
    tmpfcn2 = (temp10 - temp20) / 5.
    tmpfcn = tmpfcn1*tmpfcn2

    !>Calculate production/mineralisation of organic carbon
    minprodN = 0.
    IF(watertemp > 0. ) THEN 
      minprodN = prodpar * TPfcn * tmpfcn * waterdepth * area  !kg  
      IF(minprodN(1) > 0.) THEN  !production        
        minprodC = minprodN * NCratio
      ELSE                       !mineralisation
        minprodC = MAX(-maxdegradwater * OCpool, minprodN * NCratio)
      ENDIF
    ENDIF
    minC = -minprodC
    prodC = minprodC
    IF(minprodC(1)>0.) CALL production_pool(pooldim,OCpool,prodC)
    IF(minprodC(1)<0.) CALL retention_pool(pooldim,OCpool,minC)

    !>Set new concentration due to changes in pools
    IF(systemtype==1) THEN            !lakes
      vol = water * area / 1.0E6
      CALL new_concentration(OCpool(1),vol,conc)
    ELSE                                 !rivers
      IF(water > 0.) THEN
        vol = water / 1.0E3
        CALL new_concentration(OCpool(1),vol,conc)
      ENDIF
    ENDIF

  END SUBROUTINE oc_production_mineralisation

  !>Add load from local diffuse sources to local river inflow
  !>
  !> \b Reference ModelDescription Chapter Nitrogen and phosphorus in land 
  !!routines (Nutrient sources - Rural household diffuse source)
  !-----------------------------------------------------------------
  SUBROUTINE add_diffuse_source_to_local_river(i,qin,cin,source,addedflow)

    USE MODVAR, ONLY : i_in,i_on,i_sp,i_pp, &
                       load,                &
                       genpar,              &
                       numsubstances,       &
                       seconds_per_timestep
    USE HYPEVARIABLES, ONLY : m_locsoil

    !Argument declarations
    INTEGER, INTENT(IN) :: i                      !<index of subbasin
    REAL, INTENT(INOUT) :: qin                    !<flow in local river (m3/s)
    REAL, INTENT(INOUT) :: cin(numsubstances)     !<concentration of flow into local river (mg/L)
    REAL, INTENT(OUT)   :: source(numsubstances)  !<local source added to local river (kg/timestep)
    REAL, INTENT(OUT)   :: addedflow              !<added flow (m3/timestep)
    
    !Local variables
    REAL qhelp
    REAL qadd
    REAL cadd(numsubstances)

    !Initiation
    source = 0.
    cadd = 0.

    !> \b Algorithm \n
    !>Calculate diffuse source from rural households to local river
    addedflow = (1. - genpar(m_locsoil)) * load(i)%volloc   !m3/ts
    qadd = addedflow / seconds_per_timestep   !m3/s !fel vid korta tidsteg!
    IF(qadd>0)THEN
      qhelp = qadd * seconds_per_timestep * 1.E-3   !1000m3/timestep
      cadd = load(i)%locconc
      IF(i_in>0)THEN 
        source(i_in) = cadd(i_in) * qhelp    !Diffuse load, ruralB, kg/timestep
        source(i_on) = cadd(i_on) * qhelp    !Diffuse load, ruralB, kg/timestep
      ENDIF
      IF(i_sp>0)THEN 
        source(i_sp) = cadd(i_sp) * qhelp    !Diffuse load, ruralB, kg/timestep
        source(i_pp) = cadd(i_pp) * qhelp    !Diffuse load, ruralB, kg/timestep
      ENDIF

      !>Add diffuse source to inflow to local river flow
      IF(qin>0)THEN
        cin = (qin * cin + qadd * cadd)/(qin + qadd)
        qin = qin + qadd
      ELSE
        qin = qadd
        cin = cadd
      ENDIF
    ENDIF

  END SUBROUTINE add_diffuse_source_to_local_river

  !>Add load from point sources to main river inflow
  !>
  !> \b Reference ModelDescription Chapter Water management (Point sources)
  !-----------------------------------------------------------------
  SUBROUTINE add_point_sources_to_main_river(i,qin,cin,source,addedflow)

    USE MODVAR, ONLY : simulate, &
                       max_pstype,          &
                       load,                &
                       numsubstances,       &
                       seconds_per_timestep

    !Argument declarations
    INTEGER, INTENT(IN) :: i                        !<index of subbasin
    REAL, INTENT(INOUT) :: qin                      !<flow into main river (m3/s)
    REAL, INTENT(INOUT) :: cin(numsubstances)       !<concentration of flow into main river (mg/L)
    REAL, INTENT(OUT)   :: source(numsubstances,max_pstype)  !<point sources added to main river (kg/timestep)
    REAL, INTENT(OUT)   :: addedflow                !<added flow (m3/timestep)
    
    !Local variables
    INTEGER k,j
    REAL divvolps
    REAL qadd
    REAL cadd(numsubstances)

    !Initiation
    source = 0.
    cadd = 0.
    qadd = 0.

    !Calculate source to be added to river
    DO k = 1,max_pstype
      qadd = qadd + load(i)%psvol(k)   !m3/s
    ENDDO
    addedflow = qadd * seconds_per_timestep
    IF(qadd>0)THEN
      divvolps = 1000./qadd/seconds_per_timestep                    !kg/ts,m3/s->mg/L
      DO k = 1,max_pstype
        DO j = 1,numsubstances
          IF(simulate%substance(j))THEN 
            cadd(j) = cadd(j) + load(i)%psload(k,j)
            source(j,k) = load(i)%psload(k,j)        !Point source k, substance j
          ENDIF
        ENDDO
      ENDDO
      cadd(:) = cadd(:) * divvolps    !mg/L

      !Add source to river      
      IF(qin>0)THEN
        cin = (qin * cin + qadd * cadd)/(qin + qadd)
        qin = qin + qadd
      ELSE
        qin = qadd
        cin = cadd
      ENDIF
    ENDIF

  END SUBROUTINE add_point_sources_to_main_river

  !>Calculate effect of river wetland constructed for nutrient removal
  !>
  !>\b Reference ModelDescription Chapter Water management (Constructed wetlands)  
  !-----------------------------------------------------------------
  SUBROUTINE calculate_river_wetland(i,itype,n,temp5,temp30,qin,cin,cwetland)

    USE MODVAR, ONLY : wetland,     &
                       seconds_per_timestep

    !Argument declarations
    INTEGER, INTENT(IN) :: i           !<index of subbasin
    INTEGER, INTENT(IN) :: itype       !<index of river type (local or main)
    INTEGER, INTENT(IN) :: n           !<number of substances
    REAL, INTENT(IN)    :: temp5       !<temperature (5-day-mean) (degree Celsius)
    REAL, INTENT(IN)    :: temp30      !<temperature (30-day-mean) (degree Celsius)
    REAL, INTENT(IN)    :: qin         !<flow into/out of river wetland (m3/s)
    REAL, INTENT(INOUT) :: cin(n)      !<concentration of flow into/out of river wetland (mg/L)
    REAL, INTENT(INOUT) :: cwetland(n) !<concentration of river wetland (mg/L)
    
    !Local variables
    REAL wetlandvol     !m3 (constant)
    REAL wetlandinflow  !m3/timestep

    !Start of calculations
    IF(wetland(i,itype)%area==0) RETURN   !no wetland

    wetlandvol = wetland(i,itype)%area * wetland(i,itype)%depth   !m3
    wetlandinflow = qin * wetland(i,itype)%part * seconds_per_timestep     !m3/timestep
    CALL calculate_wetland_np(n,wetlandinflow,cin,wetland(i,itype)%area,wetlandvol,cwetland,temp5,temp30)
    IF(qin>0) cin = cin * (1. - wetland(i,itype)%part) + cwetland * wetland(i,itype)%part           !New concentration

  END SUBROUTINE calculate_river_wetland

  !>\brief Calculate nutrient processes in river wetland. 
  !!Retention is limited to 99.9% of the pool.
  !>
  !>\b Reference ModelDescription Chapter Water management (Constructed wetlands)  
  !------------------------------------------------------------------------
  SUBROUTINE calculate_wetland_np(n,qin,cin,area,vol,cvol,temp5,temp30)

    USE MODVAR, ONLY : i_in,i_sp,i_pp,  &
                       conduct

    !Argument declarations
    INTEGER, INTENT(IN) :: n       !<number of substances
    REAL, INTENT(IN)    :: qin     !<flow into wetland (m3/d)
    REAL, INTENT(IN)    :: cin(n)  !<concentration of river flow (mg/l) (before and after wetland processes
    REAL, INTENT(IN)    :: area    !<area of wetland (m2)
    REAL, INTENT(IN)    :: vol     !<volume of wetland (m3)
    REAL, INTENT(INOUT) :: cvol(n) !<concentration of wetland volume (mg/l) (before and after wetland processes
    REAL, INTENT(IN)    :: temp5   !<temperature (5-day-mean) (degree Celsius)
    REAL, INTENT(IN)    :: temp30  !<temperature (30-day-mean) (degree Celsius)
    
    !Local variables
    REAL wetlandnutrient(n), wetlandconc(n)
    REAL retention(n)
    REAL retention_tp, production_tp
    REAL wetland_tp,srpfrac
    
    !Local parameters
    REAL, PARAMETER :: teta = 1.2
    REAL, PARAMETER :: tkoeff = 20.   !temperature coefficient (degree Celsius)
    REAL, PARAMETER :: inpar = 2.3    !model parameter for inorganic nitrogen retention (mm/d/degree Celsius)
    REAL, PARAMETER :: sedpar = 0.09  !model parameter for phosphorus sedimentation (m/d)
    REAL, PARAMETER :: uptpar = 0.1   !model parameter for phosphorus uptake (m/d)

    !Calculate the nutrient processes
    wetlandnutrient = vol*cvol+qin*cin         !g
    wetlandconc = wetlandnutrient /(vol+qin)   !mg/l
    retention = 0.
    IF(conduct%simN)THEN
      IF(temp5>0) retention(i_in) = inpar * wetlandconc(i_in) * area * temp5 * 1.E-3         !g/d denitrification
      IF(retention(i_in)<0) retention(i_in) = 0.
      IF(retention(i_in)>0.999*wetlandnutrient(i_in)) retention(i_in) = 0.999 * wetlandnutrient(i_in)
    ENDIF
    IF(conduct%simP)THEN
      retention_tp = sedpar * (wetlandconc(i_pp) + wetlandconc(i_sp)) * area                 !g/d sedimentation
      IF(retention_tp<0) retention_tp = 0.
      production_tp = uptpar * (cin(i_pp) + cin(i_sp)) * (teta ** (temp30 - tkoeff)) * area  !g/d uptake
      IF(production_tp<0) production_tp = 0.
      wetland_tp = wetlandnutrient(i_pp) + wetlandnutrient(i_sp)    !g
      IF(retention_tp - production_tp < 0.999 * wetland_tp)THEN
        srpfrac = wetlandnutrient(i_sp) / wetland_tp
        retention(i_sp) = srpfrac * (retention_tp - production_tp)
        retention(i_pp) = (1.-srpfrac) * (retention_tp - production_tp)
      ELSE
        retention_tp = 0.999 * wetland_tp
        IF(wetland_tp>0)THEN
          srpfrac = wetlandnutrient(i_sp)/wetland_tp
        ELSE
          srpfrac = 0.
        ENDIF
        retention(i_sp) = srpfrac * retention_tp
        retention(i_pp) = (1.-srpfrac) * retention_tp
      ENDIF
    ENDIF
    cvol = (wetlandnutrient - retention)/(vol+qin)    !New concentration of wetland volume

  END SUBROUTINE calculate_wetland_np


END MODULE NPC_SURFACEWATER_PROCESSES
