	module mdl_bin5_DP
* ----------------------------------------------------------------------
      INteger cpus, TotalCPUS
	integer  i_sh_col(8), i_sh_row(8), i_eros, i_celle
	integer  N_ultimo, N_allagamenti, Nmax_step, Num_sorgenti, i_iniz
	integer  iiij
	integer  i_flag, N_file_output, no_rows, no_columns
	integer  max_celsorg, max_celstr, N_celle_contorno_DEM
	integer  N_stati, N_max_striscia, i_mini
	integer  N_step_entrain, j_finEntrain, i_coordQ, i_coordt
	integer  iiiiii, ii_iniz, i_file_out, i_check_massa_max, i_file77
      integer  intFileERR
	logical  boolFileERR
	
	integer N_sezioni_interne, N_internal_output_file, N_internal_DT

       integer i_maxValle  ! 11/7/2017

	integer Num_celle_routing, Num_celle_bacino
	
	integer j_entr, i_suoli, N_suoli, i_cont_suoli, i_DQ
	integer j_vel_max, j_vel_stramaz_max, i_celle2, i_rout
	
	integer j_TS, i_retint, i_retint2
	
	integer  N_strutture, N_celle_strutt_contigue   ! 23/9/2015

      integer N_Pile, j_pile   ! 25/12/2018

      integer  i_entrato  ! 1/8/2017

      integer  N_ponti  ! 5/12/2019
      integer  N_celle_ponti_max    ! 5/12/2019
	
      integer, allocatable :: N_celle_ponti(:)  ! 5/12/2019
      integer, allocatable :: ic_ponti(:,:) ! 5/12/2019
      integer, allocatable :: ir_ponti(:,:) ! 5/12/2019

	integer, allocatable ::  Nsorg1(:)
	integer, allocatable ::  Nstr1(:)
	integer, allocatable ::  ic_sorg1(:,:)
	integer, allocatable ::  ir_sorg1(:,:)
      integer, allocatable ::  ic_d(:,:,:)
	integer, allocatable ::  ir_d(:,:,:)
      integer, allocatable ::  ic_str1(:,:)
	integer, allocatable ::  ir_str1(:,:)
      integer, allocatable ::  ic_s(:,:,:)
	integer, allocatable ::  ir_s(:,:,:)
	integer, allocatable ::  k(:,:)
	integer, allocatable ::  kk(:,:)
	integer, allocatable ::  ic_eros(:)
	integer, allocatable ::  ir_eros(:)

	integer, allocatable ::  idf(:)
	integer, allocatable ::  j_fin(:)
	integer, allocatable ::  j_fine(:)
	integer, allocatable ::  N_step_input(:)
	integer, allocatable ::  ic_bc(:)
	integer, allocatable ::  ir_bc(:)
	integer, allocatable ::  ic_bc_f(:)
	integer, allocatable ::  ir_bc_f(:)
	integer, allocatable ::  Nsorg2(:)
	integer, allocatable ::  ic_sorg2(:,:)
	integer, allocatable ::  ir_sorg2(:,:)
	integer, allocatable ::  i_tmin(:)
	integer, allocatable ::  j_dir(:,:,:)
	integer, allocatable ::  j_vel(:,:,:)   ! 20 maggio 2015
	
	
	integer, allocatable :: N_celle_sez_intern(:)
      integer, allocatable ::  ic_intern(:,:)
	integer, allocatable ::  ir_intern(:,:)

	integer, allocatable ::  ic_routing(:)
	integer, allocatable ::  ir_routing(:)
      integer, allocatable ::  ic_sol(:) !BARBINI
	integer, allocatable ::  ir_sol(:) !BARBINI
	
	integer, allocatable ::  i_file_sforzoPlatea(:)
      
	
	integer, allocatable ::  ic_strutt(:)   ! 23/9/2015
	integer, allocatable ::  ir_strutt(:)   ! 23/9/2015


      integer, allocatable ::  N_celle_sez_internValle(:)  ! 11 lug 2017
      integer, allocatable ::  ic_internValle(:,:)         ! 11 lug 2017
	integer, allocatable ::  ir_internValle(:,:)         ! 11 lug 2017

      integer IC_celmax, IR_celmax   ! 7/9/2017

      integer i_file88  ! 25/10/2017

      integer  i_jj(8)  ! 29/4/2019
	


	real*8 Vtot, UU(8), QQQQQ
	real*8 V_dep_unif, V_dep_belang, V_eros_unif, V_eros_belang, V_DA, V_DB
	real*8 U_stramaz_max
	real*8 Limit_Angle, sin_Limit_Angle
	real*8 Vol_inf, dh_inf, sin_max, dh_infBel, Vol_infBel
	real*8 Coeff_Ang_Limit, PP, Sollecitazione_Verticale
      real*8 QM_U(8),QM_U_TOT, QM_E(8), QM_E_TOT, QM_TOT, QM(8), dens(8) !  29/4/2019
      real*8 F_entrante(8), F_uscente(8), F_totale(8), h_entrante    !  29/4/2019
      real*8 V_entrante, V_uscente, QM_entrante, QM_uscente     !  29/4/2019
      real*8 control_ele_ponti, control_file_ponti ! 5/12/2019

	real*8, allocatable :: val_sorg(:,:)
	!real, allocatable :: val_contorno(:,:)
      real*8, allocatable :: val(:,:)
	real*8, allocatable :: val_tempi(:,:)
      real*8, allocatable :: val_flag(:,:)
	real*8, allocatable :: ele(:,:)
	real*4, allocatable :: ele1(:,:)
      
      real*8, allocatable :: ele_b(:,:)
      real*8, allocatable :: SCS_error(:,:)
	real*8, allocatable :: h_tot(:,:)
	real*8, allocatable :: h_tot2(:,:)
	real*8, allocatable :: sen_max(:,:)
	real*8, allocatable :: peso_max(:,:)
	!real, allocatable :: Q(:,:)
	real*8, allocatable :: h(:,:)
	real*8, allocatable :: dh(:,:)
      real*8, allocatable :: Ch(:,:)
	real*8, allocatable :: Erod(:,:)
	real*8, allocatable :: U_crit1(:,:)
	real*8, allocatable :: senteta_crit(:,:)
      real*8, allocatable :: dh_sed(:,:)
	real*8, allocatable :: Eros_tot(:,:)
	real*8, allocatable :: sen_teta(:,:,:)
	real*8, allocatable :: senteta(:,:,:)
	real*8, allocatable :: peso(:,:,:)
	real*8, allocatable :: peso_d(:,:,:)
	!real, allocatable :: h_affl_sorg(:)
	!real, allocatable :: h_defl_sorg(:)
	!real, allocatable :: h_defl_str(:)
	!real, allocatable :: h_affl_str(:)

	real*8, allocatable :: dh_entrata_unif(:,:)
	real*8, allocatable :: dh_entrata_Bel(:,:)

      !real, allocatable :: t(:)  15/01/2013
	real*8, allocatable :: t_fin(:)
	real*8, allocatable :: t_inizio(:)
      real*8, allocatable :: t_1d(:,:)
	real*8, allocatable :: Q_input(:,:)
	real*8, allocatable :: Q_input_tot(:,:)
	!real, allocatable :: Q_out(:,:)   ! 14/01/2013
	!real, allocatable :: V_fuori_uscito_DT(:)   ! 14/01/2013
	real*8, allocatable :: dh_contorno(:)
	real*8, allocatable :: Q_contorno(:)
	real*8, allocatable :: V_contorno(:,:)
	
	real*8, allocatable :: t_file(:)
	real*8, allocatable :: tempo_file(:)
	real*8, allocatable :: flag(:)

	real, allocatable :: h_finale(:,:)
	real, allocatable :: erosione_finale(:,:)
      real, allocatable :: ele_finale(:,:)
      real, allocatable :: Area_dep_eros_finale(:,:)
      real, allocatable :: h_tot_finale(:,:)  ! 26/1/2015
	real, allocatable :: conc_finale(:,:)    ! 26/1/2015
 	real, allocatable :: solid_tot_finale(:,:)
      
      real*8, allocatable :: min_ele(:,:)
	real*8, allocatable :: Area_dep_eros_ultimo_step(:,:)
	real*8, allocatable :: velocit(:,:)
	real*8, allocatable :: direz_vel(:,:)
	real*8, allocatable :: t_out_minuti(:)
	real*8, allocatable :: flag_int(:)

	
	real, allocatable :: h_finale2(:,:)  ! 7/2/2016
	real, allocatable :: erosione_finale2(:,:)  ! 7/2/2016
      real, allocatable :: ele_finale2(:,:)   ! 7/2/2016
	real, allocatable :: Area_dep_eros_finale2(:,:)  ! 7/2/2016
	real, allocatable :: h_tot_finale2(:,:)  ! 7/2/2016
	real, allocatable :: conc_finale2(:,:)    ! 7/2/2016
	
      
   !   real, allocatable :: V_entrain(:)    ! 14/01/2013
	real*8, allocatable :: V_entrained_tot(:)
	real*8, allocatable :: V_entrained_step(:)
	real*8, allocatable :: t_step_entrain(:)
	real*8, allocatable :: attivata(:)
	real*8, allocatable :: V_input_iniziale(:)
	real*8, allocatable :: V_input(:)
	real*8, allocatable :: attivata_new(:)
	real*8, allocatable :: InternalOutput(:,:)
      real*8, allocatable :: InternalOutputValle(:,:)   ! 11 lug 2017
      real*8, allocatable :: InternalOutputValleAux(:,:)   ! 11 lug 2017
	real*8, allocatable :: tempi_output(:)
	real*8, allocatable :: Q_out_interne(:)
	real*8, allocatable :: FreeSurf_interne_medio(:) 
	real*8, allocatable :: Conc_sezioni_interne(:)
	real*8, allocatable :: ele_interno_medio(:)
      real*8, allocatable :: ele_iniz_interno_medio(:)   ! 21/7/2017
      real*4, allocatable :: InletOutlet(:,:)
      real*8, allocatable :: Erod_Aux(:,:)
      real*8, allocatable :: Suolo(:,:)
      real*4, allocatable :: Suolo1(:,:)
      real*8, allocatable :: uso_suolo(:)
      real*8, allocatable :: scabr(:)
      real*8, allocatable :: ang_eros(:)
      real*8, allocatable :: vel_eros(:)
      real*8, allocatable :: Mobile(:)
      real*8, allocatable :: dh_uscita_sez(:,:)
      real*8, allocatable :: dh_uscita_solido_sez(:,:)
      real*8, allocatable :: dh_uscita_tot(:,:)
      real*8, allocatable :: dh_uscita_solido_tot(:,:)
      real*8, allocatable :: Q_uscita_interne(:)
      real*8, allocatable :: Q_uscita_solido_interne(:)
      real*8, allocatable :: Q_uscita_sez_interne(:)
      real*8, allocatable :: Q_uscita_solido_sez_interne(:)
      real*8, allocatable :: flow_depth_interne_medio(:)  ! 21/7/2017
      real*8, allocatable :: spessore_interne_medio(:)  ! 21/9/2017


      real*8, allocatable :: Q_out_sez(:)   ! 15/11/2017
	real*8, allocatable :: FreeSurf_sez(:)    ! 15/11/2017
	real*8, allocatable :: Conc_sez(:)    ! 15/11/2017
      real*8, allocatable :: Flow_depth_sez(:)   ! 15/11/2017
	real*8, allocatable :: Spessore_sez(:)    ! 15/11/2017
	real*8, allocatable :: Q_uscita_sez(:)    ! 15/11/2017
      real*8, allocatable :: Q_uscita_solido_sez(:)    ! 15/11/2017
      real*8, allocatable :: Q_uscitatot_sez(:)    ! 15/11/2017
      real*8, allocatable :: Q_uscita_solidotot_sez(:)    ! 15/11/2017
      real*8, allocatable :: Averaged_conc(:)    ! 15/11/2017
      real*8, allocatable :: Averaged_FreeSurf(:)    ! 15/11/2017
      real*8, allocatable :: Averaged_Spessore(:)    ! 15/11/2017


      
      real*8, allocatable :: T_INIZIAL(:)
      
      real*8, allocatable :: Solid_tot(:,:)
      
      real*8, allocatable :: h_pre(:,:)
      real*8, allocatable :: h_post(:,:)
      
      
       real*8, allocatable :: Q_entrata(:,:)  
       real*8, allocatable :: dh_entrata_sorg(:,:) 
       
       !real, allocatable :: direz_vel_max(:,:)
       real*8, allocatable :: conc_max(:,:)
       real*8, allocatable :: h_tot_max(:,:)
       real*8, allocatable :: hh_max(:,:)
       real*8, allocatable :: vel_max(:,:)
       real*8, allocatable :: spessore_max(:,:)
       
       real*8, allocatable :: h_sol(:)
       real*8, allocatable :: htot_sol(:)
       real*8, allocatable :: eros_sol(:)
       real*8, allocatable :: vel_sol(:)
       real*8, allocatable :: direz_vel_sol(:)
       real*8, allocatable :: conc_sol(:)  ! 18/9/2017
       
          real*8, allocatable :: ele_iniz(:,:)
          real*8, allocatable :: magnitudo(:,:)
          real*8, allocatable :: Erosion_fin(:,:)
             
          real*8, allocatable :: direz_max(:,:)
          
            real*8, allocatable :: Conc_input(:,:)
            real*8, allocatable :: Conc_input_iniziale(:)
            
            real*8, allocatable :: V_solido_sorg(:)
            real*8, allocatable :: V_entrato_sorg(:)
            
               real*8, allocatable :: dh_entrata_solido_sorg(:,:)    
               real*8, allocatable :: Conc(:,:)  
               real*8, allocatable :: h_solido(:,:) 
               real*8, allocatable :: VS_input(:)
               real*8, allocatable :: dh_entrata_solido(:,:)  
               real*8, allocatable :: dh_solido(:,:) 
               real*8, allocatable :: dh_solido_contorno(:)  
               
               real*8, allocatable :: Q_int_medio(:)
               real*8, allocatable :: Q_uscita_medio(:)
               real*8, allocatable :: Q_uscita_solido_medio(:)
               real*8, allocatable :: Q_uscita_sez_medio(:)
               real*8, allocatable :: Q_uscita_solido_sez_medio(:)
               real*8, allocatable :: Ele_medio(:)
               real*8, allocatable :: ele_iniz_medio(:)  ! 21/7/2017
               real*8, allocatable :: FRSURF_medio(:)
               real*8, allocatable :: Flow_depth_medio(:)    ! 21/7/2017
               real*8, allocatable :: Spessore_medio(:)    ! 21/9/2017
               real*8, allocatable :: Conc_med(:)
               real*8, allocatable :: T_intervallo_calcolo(:)
               real*8, allocatable :: T_intervallo_calcoloELE(:)  ! 21/7/2017
               real*8, allocatable :: C_star(:)
               real*8, allocatable :: C_fondo(:,:)
               real*8, allocatable :: tauMax(:,:)
               real*8, allocatable :: tauMax_x(:,:)
               real*8, allocatable :: tauMax_y(:,:)
               real*8, allocatable :: energia(:,:)
               real*8, allocatable :: densita(:,:)
               
               real*8, allocatable :: Vx(:,:)
               real*8, allocatable :: Vy(:,:)
               real*8, allocatable :: Vx_max(:,:)
               real*8, allocatable :: Vy_max(:,:)
               real*8, allocatable :: Vel_Cella(:,:)
               real*8, allocatable :: vel_cella_max(:,:)
               real*8, allocatable :: vel_sol_x(:)
               real*8, allocatable :: vel_sol_y(:)
               real*8, allocatable :: Pmax(:,:)
               real*8, allocatable :: SVmax(:,:)
               real*8, allocatable :: no_erod(:,:) 
               real*8, allocatable :: h_noerod(:,:) 
               real*8, allocatable :: cond_noerod(:)
               real*8, allocatable :: struttura(:)
               real*8, allocatable :: codice_struttura(:)
               real*8, allocatable :: ponte(:)  ! 5/12/2019
               real*8, allocatable :: quota_critica_ponte(:,:)  ! 5/12/2019
               real*4, allocatable :: file_ponti(:,:)  ! 5/12/2019
               real*4, allocatable :: ele_ponti(:,:)  ! 5/12/2019
               real*8, allocatable :: diff_ponti(:,:)  ! 5/12/2019
               real*8, allocatable :: codice_ponte(:)  ! 5/12/2019
               real*8, allocatable :: soletta(:)  ! 5/12/2019
               real*8, allocatable :: franco(:)  ! 5/12/2019
               real*8, allocatable :: ponte_ostruito(:)  ! 5/12/2019
               real*8, allocatable :: volume_ostruzione(:)  ! 5/12/2019
               real*8, allocatable :: file_strutture(:,:)  ! aggiunta del 21/09/2015
               real*8, allocatable :: file_strutturecontigue(:,:) ! aggiunta del 23/09/2015


*************************************** PILE 25/12/2018 *********************************

       real*8, allocatable :: pile(:)  ! 25/12/2018
       real*8, allocatable :: codice_pile(:)  ! 25/12/2018
       integer, allocatable ::  i_file_sforzoPila(:)   ! 25/12/2018
       integer, allocatable ::  ic_Pile(:)  ! 25/12/2018
       integer, allocatable ::  ir_Pile(:)  ! 25/12/2018
       integer, allocatable ::  i_Pile(:)  ! 25/12/2018
        real*8, allocatable :: Vx_pre(:)   !  29/4/2019

*****************************************************************************************
               
               
               
      real*8, allocatable :: Tx(:)
	real*8, allocatable :: Ty(:)
	real*8, allocatable :: P(:)
	real*8, allocatable :: Tx_max(:)
	real*8, allocatable :: Ty_max(:)
	real*8, allocatable :: P_max(:)
      real*8, allocatable :: SV_max(:)
	real*8, allocatable :: Valore_massimo_T(:)
	real*8, allocatable :: Valore_massimo_P(:)
      real*8, allocatable :: Valore_massimo_SV(:)
      real*8, allocatable :: SOLLECIT_VERT(:)

       real*8, allocatable :: VolumeEntrato(:)   ! 1/8/2017
       real*8, allocatable :: VolumeSolidoEntrato(:)   ! 1/8/2017

        real*8, allocatable :: U_crit2(:,:)   ! 13/9/2017
        real*8, allocatable :: senteta_crit2(:,:)    ! 13/9/2017

      real*8, allocatable :: ang_dep(:)  ! aggiunta 13/9/2013
      real*8, allocatable :: vel_dep(:)  ! aggiunta 13/9/20133

      !real, allocatable :: spessore(:,:)  ! 21/9/2017

      real*8, allocatable :: Vol_uscita(:) ! 25/10/2017
      real*8, allocatable :: Vol_uscita_solido(:) ! 25/10/2017   
      real*8, allocatable :: volume_sez(:) ! 25/10/2017    
      real*8, allocatable :: Q_uscita(:) ! 25/10/2017    
      real*8, allocatable :: Q_uscita_solido(:) ! 25/10/2017       
      real*8, allocatable :: Averaged_flow_depth(:) ! 25/10/2017    
      
      
      
   !    real, allocatable :: dh_x(:,:)   ! 12/7/2018
   !    real, allocatable :: dh_y(:,:)   ! 12/7/2018   

    !   real, allocatable :: dh_xE(:,:)   ! 12/7/2018
    !   real, allocatable :: dh_yE(:,:)   ! 12/7/2018  
    !   real, allocatable :: vel_cella_maxE(:,:) 
   !     real, allocatable :: Vel_CellaE(:,:)

        real*8, allocatable :: Vel8(:,:,:)   !  29/4/2019
        real*8, allocatable :: Deltah8(:,:,:)   !  29/4/2019
       
   !     real, allocatable :: VXX(:,:)   !  29/4/2019
   !     real, allocatable :: VYY(:,:)   !  29/4/2019
   !     real, allocatable :: VV(:,:)   !  29/4/2019

    !    real, allocatable :: VXX_max(:,:)   !  29/4/2019
    !    real, allocatable :: VYY_max(:,:)   !  29/4/2019
   !     real, allocatable :: VV_max(:,:)   !  29/4/2019

        
          

               
               
                               	
	
	
      real*8 pixsize, or_easting, or_northing, esterno, undefined
      real*8 dh_eros_tot, dh_eros(8), eros, Egashira, U, dh_neg, rapp
	real*8 control4, control5, control6, control7, lato_cella, sen_tetaj
	real*8 sen_tetatot, ang_deposito, senteta_deposito 
    	real*8 tempo_finale, control_eros, Vel_erosion, Ang_erosion
	real*8 Courant, h_asciutto, control1, Chezy, Erosion, total_eros
	real*8 h_erosione, Egash_eros, Egash_dep, DT1, a_DT, b_DT, c_DT, DT2
	real*8 Area_alluvionata, Area_erosa, Area_inondata

	real*8 h_routing, h_totale_routing, V_total_routing, h_totale2
	real*8 Cmedio_input, celle_attive, T_intervallo
	
	real*8  V_solido_fuori_uscito_totale, V_solido_fuoriuscito_DT
	
	real*8    V_entrato_DT, V_solid_input_DT



c      erosione/deposito per differenze di altezza

      real*8 D_ele, ang_tot(8), senteta_superf, dh_eros_Belangier, V_step
	real*8 V_dep_step_A, V_eros_step_A, V_dep_step_B, V_eros_step_B


	real*8 Q(8), somma_dh, diff_dh, coeff_dh, U_stramaz(8), Qmaxx
	real*8 Q_CONTORNO_TOTALE, ttttt, Qmax_contorno, Qmax_input

	real*8  t_step_entrainDT, V_eros_prima, DT_entrain, tt_max
	real*8  t_minimo, Qseconda, h_defluit, sum_attivata, V_inp, V1, V2
	real*8  VOLUME_ENTRATO_IDROGRAMMI
	
	real*8 Control_Output, Output_control, Output_massimo
	real*8 Output, Intern_Output,  DT_Internal_Output, rapp_tempi 
	real*8 Check_massa, Check_massa_max, t_minuti, control8
	real*8 Control_InletOutlet1, Control_InletOutlet2, Inlet_massimo
	real*8 h_previous
	real*8 V_eroso_prima, V_depositato_prima, V_fuori_uscito_DT         ! 14/01/2013 
	real*8 t, t1, t2, t_prima, t_dopo, t_check_massa_max, DT_OUTput_secondi
	real*8 Suolo_diff, T_INIZ, DT_OUTPUT_MINUTI, SCRAUSA
	
	real*8 Cmedio,Cstar,Cmax, Volume_solido_eroso, Volume_solido_depositato
	real*8 Volume_solido, h_solid, Volume_solido_eroso_step
	real*8 Volume_solido_depositato_step, C_limite_deposito, V_solid_input
	real*8 total_solid, V_solid, volume_solido_intrappolato
	real*8 spessore, VS_inp, VS1, VS2,  VOLSOL_ENTRATO_IDROGRAMMI
	real*8 Check_VS_max, Check_VS, t_check_VS_max, C_input_routing
	real*8 differenza_solido, differ, dh_neg1, dh_neg2, trapped_solid_depth
	real*8 volume_solido_intrappolato_STEP, V_dep_step_C, tempo_scrittura
	real*8 tempo_scrittura_interfaccia, SUOLO_BIS(500), SUOLO_TRIS(500)
	real*8 differenza_liquido, deposito_solido, deposito_liquido
	real*8 Volume_solido_depositato_prima, Volume_solido_eroso_prima
	real*8 tempo_scrittura2   ! aggiunto il 2/3/2015
	real*8 Conc_fuoriuscito   ! aggiunto il 13/6/2015
	
	real*8 quota_finale, cond_eros, sumDT    ! 30/4/2015
	
	real*8 rapp_no_erod, dh_neg_previous      ! 7/5/2015
	
	real*8 densita_max 
	real*8 PP_max, Eros_tot_previous, Solid_tot_previous
	real*8 controllo_inerodibilita, differ2, dif_dif
	real*8 avvertenza_erosione_quota_inerodibile, spessore_erodibile
	
      real*8 SUOLO_FLT(500) !BERNARD
	
	real*8 celle_ele_variata  ! 21/07/2017

       real*8 cel1, cel1max, h1max   ! 7/9/2017

       real*8 Vel_dep_sup ! modifica 13/9/2017

       real*8 celle_attive_sez ! 25/10/2017

          
          real*8 cq  ! 7/10/2019
          
	



      character*5256 fileprj,PRJ
	character*5256 fileFormat, fileEle, fileCh, fileSorgente, fileBM
	character*5256 fileIdrogramma, fileErosione, fileVel_inf_eros
	character*5256 fileAng_inf_eros, fileTempi_Allag, fileControllo 
	character*5256 fileComandi, fileHeader, fileEleNuovo, fileBC
	character*55256 fileVC, fileLog, file_finale, fileERR, file_finale_1
	character*5256 file_Internal_Outputs, file_Inlet_Outlet, file_hErod
	character*5256 fileLandChar, fileLandUse, fileSOL1, fileSOL2, fileSOL3
	character*5256 fileSOL4, fileSOL5,fileCstar, fileErodibilita 
	character*5256 fileSforzoPlatea
	character*5256 fileStrutture ! aggiunta del 21/09/2015
      character*5256 file_Internal_OutputsValle ! 11/7/2017
      character*5256 fileVel_sup_dep, fileAng_sup_dep   ! 13/9/2017
      character*5256 fileSOL6   ! 13/9/2017
      character*5256 fileElePonti, fileTxtPonti ! 5/12/2019

	character*5256, allocatable :: filename_flowdepth(:)
	character*5256, allocatable :: filename_freesurf(:)
	character*5256, allocatable :: filename_erosiondepth(:)
	character*5256, allocatable :: filename_velmax(:)
	character*5256, allocatable :: filename_veldir(:)
	character*5256, allocatable :: filename_DEM(:)
	character*5256, allocatable :: filename_conc(:)
	character*5256, allocatable :: filename_VelCella(:)
      character*5256, allocatable :: filename_velocit_uscente(:)
	character*5256, allocatable :: filename_direz_vel_uscente(:)
      character*5256, allocatable :: filename_Vx(:)
      character*5256, allocatable :: filename_Vy(:)
      
	
	
	character*120 intestazione_uso_suolo
      character*50 pippone

	character*256 text
      character*25  tipofile
      character*10  byteorder
      character*5   estensione

	character*12  asseq
      character*8  asset
	character*50 asse1

	character*16 pippo_char
	character*3 Sorgente

       Type sezioneInterna
        !integer i
        integer ic
        integer ir
      end type sezioneInterna

      Type sezioniInt
        Type(sezioneInterna), allocatable :: seqCell(:)
        integer direzioni(4) ![Nord, Sud, Ovest, Est]
        integer index
      end type sezioniInt
      
      Type(sezioniInt), allocatable :: sezioniInterne (:)



      
      data     undefined / -8888.0 /, esterno / -9999.0 /
* ----------------------------------------------------------------------
      end module 