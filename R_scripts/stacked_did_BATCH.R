library(dplyr)
library(tidyr)
library(fixest)
# library(stargazer)
library(modelsummary)
library(stringr)
library(ggplot2)
library(reshape2)
library(car)
# library(did)
# source("R_scripts/helper_functions.R")
source(file.path(getwd(),"R_scripts","feols_utils.R"))
source(file.path(getwd(),"R_scripts","stacked_did.R"))

main = function(y_var = "log_price_PSM",
                property_type= "Condominium/Apartment",
                cluster_vars = c("SUBZONE_N"),
                fe_vars = c("SUBZONE_N","month_year"),
                period_duration = c(-18,12),
                model_names = c("potentialcontamination_removed","contaminated_removed"),
                control_vars = c("Type_of_Sale","Building_Age","Floor_level",
                                  "distance_to_CBD","distance_to_MRT"),
                buffer_sub_dir = "test"
                ){
  
  start.time <- Sys.time()
  
  #-----------------import supporting files----------------------
  # import distance to CBD (to be added as additional arguments to "import_files" function)
  distance_to_CBD_df <- read.csv("Data/Priv_Res_distance_to_CBD.csv")
  distance_to_MRT_df <- read.csv("Data/Priv_Res_distance_to_MRT.csv")
  adaptation_df <- read.csv(file.path("Exported_Data/adaptation","Property_ID_MLProbit_20260423.csv"))
  
  #-----import file path--------------------
  small_radius <- c(seq(from=50, to=350, by = 20),
                    seq(from=200, to = 340, by= 20))
  big_radius <- round(sqrt(2)*small_radius,-1)
  
  # dir
  buffer_dir <- file.path(getwd(),"Exported_Data","stacked_did")
  
  # create directory if it doesnt/alr exist
  dir.create(file.path(buffer_dir, buffer_sub_dir), showWarnings = FALSE)
  dir.create(file.path(getwd(),"Exported_Data","exported_figures", buffer_sub_dir), showWarnings = FALSE)
  
  #---import csvs---
  fp_list <- list.files(path=buffer_dir, pattern="\\.csv$")
  fp_names <- str_extract(fp_list, "(dsmall).+?(?=_network)")
  fp_list <- file.path(buffer_dir, fp_list)
  # fp_list <- file.path(buffer_dir,sprintf("PrivRes_20260403_%s_networkDepthNone.csv", fp_names))
  names(fp_list) <- fp_names
  # fp_list
  # # import files
  buffer_df_list <- import_files(fp_list,distance_to_CBD_df,distance_to_MRT_df, adaptation_df)
  
  
  #--- create other variables------
  buffer_df_list <- lapply(buffer_df_list, function(df){
    df%>%
      # filter property type
      filter(Property_Type == property_type)%>%
      # log distance to MRT (originally in m)
      # log distance to CBD (originally in km)
      mutate(log_distance_to_MRT = log(distance_to_MRT),
             log_distance_to_CBD = log(distance_to_CBD),
             Building_Age1 = ifelse(Building_Age<0,0,Building_Age),
             log_Building_Age1 = log1p(Building_Age1),
             distance_to_MRT1 = distance_to_MRT/1000,
             log_distance_to_MRT1 = log(distance_to_MRT1)
             )%>%
      
      # create dummy variables for adaptation
      mutate(across(c(treat_adaptation_prob, treat_adaptation_prob_logit), function(x){
        as.integer(ifelse(x>quantile(x, probs=0.25),1,0)) #quantile(x, probs=0.25)
      }, .names= "D_p25_{.col}"))%>%
      mutate(across(c(treat_adaptation_prob, treat_adaptation_prob_logit), function(x){
        as.integer(ifelse(x>quantile(x, probs=0.50),1,0)) #quantile(x, probs=0.25)
      }, .names= "D_p50_{.col}"))%>%
      mutate(across(c(treat_adaptation_prob, treat_adaptation_prob_logit), function(x){
        as.integer(ifelse(x>quantile(x, probs=0.75),1,0)) #quantile(x, probs=0.25)
      }, .names= "D_p75_{.col}"))%>%
      mutate(across(c(treat_adaptation_prob, treat_adaptation_prob_logit), function(x){
        as.integer(ifelse(x>mean(x),1,0)) 
      }, .names= "D_mean_{.col}"))%>%
      mutate(across(c(treat_adaptation_prob, treat_adaptation_prob_logit), function(x){
        (x-mean(x))/sd(x) 
      }, .names= "norm_{.col}"))
  })
  
  
  #---plot data distribution----
  # buffer_df_list$dsmall200dbig280%>%
  #   select(where(is.numeric))%>%
  #   select(-c(Postal_Sector,Postal_Code,Postal_District,X,Property_ID,contaminated_rows,potential_contamination,
  #             period_sale, period_flood))%>%
  #   melt()%>%
  #   ggplot(aes(x=value)) +
  #   geom_density(fill="#69b3a2") +
  #   facet_wrap(vars(variable), scales="free") +
  #   labs(x="Value") +
  #   theme_bw()
  # 
  # ggsave(filename = file.path(getwd(),"Exported_Data","exported_figures",
  #                             sprintf("stackedDID_dataDist_%s.svg",buffer_sub_dir))
  #   ,width = 9, height = 5, units = "in")
  
  #---- 1 stacked DID----------
  lapply(model_names, function(model_name){
    
    local_DID_df_list <- lapply(seq_along(buffer_df_list), function(i){
      
      
      # small radius, big radius (treatment, control group)
      name_df <- names(buffer_df_list)[i]
      small_radius <- strsplit(name_df,split="dsmall|dbig")[[1]][2]
      big_radius <- strsplit(name_df,split="dsmall|dbig")[[1]][3]
      
      # filter property type so we focus analysis on different panels
      local_DID_df <- buffer_df_list[[i]]
      
      if (!is.na(property_type)){
        local_DID_df <- local_DID_df%>%
          filter(Property_Type == property_type)
      }
      
      
      if (model_name =="contaminated_removed"){
        local_DID_df <- local_DID_df%>%
          filter(contaminated_rows==0)
        
      } else if (model_name =="potentialcontamination_removed"){
        local_DID_df <- local_DID_df%>%
          filter(potential_contamination==0)
      }
      
      # filter local_DID_df based on period_D, only include period_D from -18 to 12
      local_DID_df <- local_DID_df%>%
        filter((period_D>=period_duration[1]) & (period_D<=period_duration[2]))
      
      treat_vars <- c("treat")
      post_vars <- c("post")
      # model formula
      model_property_formula <- feols_formula(y_var = y_var,
                                              control_vars=c(control_vars, treat_vars,post_vars),
                                              specified_interaction_vars=c(
                                                sprintf("%s * %s", treat_vars,post_vars)
                                              ),
                                              interaction_sep = "*",
                                              fe_vars = fe_vars,
                                              cluster_vars = cluster_vars)
      
      # print(model_property_formula)
      
      # fit TWFE model
      model_property <- feols(
        model_property_formula$formula,
        cluster = model_property_formula$cluster,
        data = local_DID_df
      )
      
      summary(model_property)
      results_df <- get_model_results(model_property, model_name = sprintf("Treat%s_Control%s",small_radius, big_radius))
      
      results_df
    })
    
    local_DID_df_list <- as.data.frame(do.call(rbind, local_DID_df_list))
    # export data
    write.csv(local_DID_df_list,file.path(buffer_dir,buffer_sub_dir,
                                          sprintf("TWFE_%s.csv",model_name)
    ),
    row.names = FALSE)
    local_DID_df_list
  })
  
  ### 1.1.1 Stacked DID - plot robustness

  sapply(model_names, function(x){
    plot_local_DID_robustness(fp = file.path(buffer_dir,buffer_sub_dir,
                                             sprintf("TWFE_%s.csv",x)
    ),
    
    save_fp=file.path(getwd(),"Exported_Data","exported_figures",buffer_sub_dir,
                      sprintf("TWFE_%s.svg",x)
    ),
    
    filter_regex="^treat|^post",
    significance_regex = "\\*+|\\."
    )
  })

  
  ## 2.2 Event study - 1 month
  
  # - base period (determined by the last number): 
  #   -"Dt_min2_min1": base period is -1
  # - "Dt_min1_0": base period is 0
  
  # --------------2.1 Event study - 1 month---------------------------
  sapply(c(TRUE, FALSE), function(include_Dt){
    # sapply(c(-1,0), function(BASE_PERIOD){
    sapply(c("Dt_min2_min1","Dt_min1_0"), function(BASE_PERIOD){
      lapply(model_names, function(model_name){
        local_DID_df_list <- lapply(seq_along(buffer_df_list), function(i){
          
          # small radius, big radius (treatment, control group)
          name_df <- names(buffer_df_list)[i]
          small_radius <- strsplit(name_df,split="dsmall|dbig")[[1]][2]
          big_radius <- strsplit(name_df,split="dsmall|dbig")[[1]][3]
          # filter contaminated rows
          df <- buffer_df_list[[i]]
          
          if (!is.na(property_type)){
            df <- df%>%
              filter(Property_Type == property_type)
          }
          
          
          if (model_name =="contaminated_removed"){
            df <- df%>%
              filter(contaminated_rows==0)
            
          } else if (model_name =="potentialcontamination_removed"){
            df <- df%>%
              filter(potential_contamination==0)
          }

          model_property <- get_event_study_df1(df, period_D_list = seq(from=period_duration[1]-1,to=period_duration[2],by=1), 
                                                control_vars = control_vars,
                                                base_period=BASE_PERIOD, include_Dt=include_Dt,
                                                Dt_to_days=FALSE,include.lowest=FALSE)
          
          # print(model_property)
          # get model results
          results_df <- get_model_results(model_property, model_name = sprintf("Treat%s_Control%s",small_radius, big_radius))
          results_df
        })
        #--------------concat results-----------------------------------------
        local_DID_df_list <- as.data.frame(do.call(rbind, local_DID_df_list))
        # export data
        write.csv(local_DID_df_list,
                  file.path(buffer_dir,buffer_sub_dir,
                            sprintf("EventStudy_%s_BP%s_Dt%s.csv",
                                    model_name,BASE_PERIOD,include_Dt)
                  ),
                  row.names = FALSE)
        # local_DID_df_list
        print(file.path(buffer_dir,buffer_sub_dir,
                        sprintf("EventStudy_%s_BP%s_Dt%s.csv",
                                model_name,BASE_PERIOD,include_Dt)
        ))
      })
    })
  })
  
  ### 2.2.2 Plot event study (estimates) - 1 month
  period_D_list <- as.character(seq(from=period_duration[1]-1,to=period_duration[2],by=1))
  period_D_list <- unlist(sapply(c(1:length(period_D_list)-1),function(x){
    sprintf("treat x Dt_%s_%s", sub("-","min",period_D_list[x]), 
            sub("-","min",period_D_list[x+1]))
  }))
  
  sapply(c(TRUE, FALSE), function(include_Dt){
    sapply(c("Dt_min2_min1","Dt_min1_0"), function(BASE_PERIOD){
      sapply(model_names, function(x){
        
        try(
          plot_event_study_estimates1(fp = file.path(buffer_dir,buffer_sub_dir,
                                                     sprintf("EventStudy_%s_BP%s_Dt%s.csv",
                                                             x,BASE_PERIOD,include_Dt)
          ),
          
          save_fp=file.path(getwd(),"Exported_Data","exported_figures",buffer_sub_dir,
                            sprintf("EventStudyEstimate_%s_BP%s_Dt%s.svg",
                                    x,BASE_PERIOD,include_Dt)
          ),
          base_period=sprintf("treat x %s",BASE_PERIOD),
          period_D_list = period_D_list
          )
        )
        
      })
    })
  })
  
  ## 2.3 Event Study - 2 months aggregated
  
  # - base period "Dt_min2_0": -2 to 0 is the base period
  # --------------2.2 Event Study - 2 months aggregated---------------------------
  sapply(c(TRUE, FALSE), function(include_Dt){
    sapply(c("Dt_min2_0"), function(BASE_PERIOD){
      lapply(model_names, function(model_name){
        local_DID_df_list <- lapply(seq_along(buffer_df_list), function(i){
          
          # small radius, big radius (treatment, control group)
          name_df <- names(buffer_df_list)[i]
          small_radius <- strsplit(name_df,split="dsmall|dbig")[[1]][2]
          big_radius <- strsplit(name_df,split="dsmall|dbig")[[1]][3]
          # filter contaminated rows
          df <- buffer_df_list[[i]]
          
          if (!is.na(property_type)){
            df <- df%>%
              filter(Property_Type == property_type)
          }
          
          if (model_name =="contaminated_removed"){
            df <- df%>%
              filter(contaminated_rows==0)
            
          } else if (model_name =="potentialcontamination_removed"){
            df <- df%>%
              filter(potential_contamination==0)
          }
          # apply event study function
          model_property <- get_event_study_df1(df, period_D_list = seq(from=period_duration[1],to=period_duration[2],by=2),
                                                #seq(from=-12,to=6,by=2),#seq(from=-18,to=12,by=2)
                                                control_vars = control_vars,
                                                base_period=BASE_PERIOD, include_Dt=include_Dt,
                                                Dt_to_days=FALSE,include.lowest=TRUE)
          
          # print(model_property)
          # get model results
          results_df <- get_model_results(model_property, model_name = sprintf("Treat%s_Control%s",small_radius, big_radius))
          results_df
        })
        #--------------concat results-----------------------------------------
        local_DID_df_list <- as.data.frame(do.call(rbind, local_DID_df_list))
        # export data
        write.csv(local_DID_df_list,
                  file.path(buffer_dir,buffer_sub_dir,
                            sprintf("EventStudy_%s_BP%s_Dt%s.csv",
                                    model_name,BASE_PERIOD,include_Dt)
                  ),
                  row.names = FALSE)
        # local_DID_df_list
        print(file.path(buffer_dir,buffer_sub_dir,
                        sprintf("EventStudy_%s_BP%s_Dt%s.csv",
                                model_name,BASE_PERIOD,include_Dt)
        ))
      })
    })
  })
  
  
  ### 2.3.1 Plot event study (estimates) - 2 months
  period_D_list <-as.character(seq(from=period_duration[1],to=period_duration[2],by=2))  #as.character(seq(from=-12,to=6,by=2))
  period_D_list <- unlist(sapply(c(1:length(period_D_list)-1),function(x){
    sprintf("treat x Dt_%s_%s", sub("-","min",period_D_list[x]), 
            sub("-","min",period_D_list[x+1]))
  }))
  
  sapply(c(TRUE, FALSE), function(include_Dt){
    sapply(c("Dt_min2_0"), function(BASE_PERIOD){
      sapply(model_names, function(x){
        try(
          plot_event_study_estimates1(fp = file.path(buffer_dir,buffer_sub_dir,
                                                     sprintf("EventStudy_%s_BP%s_Dt%s.csv",
                                                             x,BASE_PERIOD,include_Dt)
          ),
          
          save_fp=file.path(getwd(),"Exported_Data","exported_figures",buffer_sub_dir,
                            sprintf("EventStudyEstimate_%s_BP%s_Dt%s.svg",
                                    x,BASE_PERIOD,include_Dt)
          ),
          base_period=sprintf("treat x %s",BASE_PERIOD),
          period_D_list = period_D_list
          )
        )
        
      })
    })
  })
  
  ## 2.4 Event Study - 45 days aggregated
  
  # - base period "Dt_min45_0": -45 to 0 days is the base period
  # --------------2.3 Event Study - 45 days aggregated---------------------------
  sapply(c(TRUE, FALSE), function(include_Dt){
    sapply(c("Dt_min45_0"), function(BASE_PERIOD){
      lapply(model_names, function(model_name){
        local_DID_df_list <- lapply(seq_along(buffer_df_list), function(i){
          
          # small radius, big radius (treatment, control group)
          name_df <- names(buffer_df_list)[i]
          small_radius <- strsplit(name_df,split="dsmall|dbig")[[1]][2]
          big_radius <- strsplit(name_df,split="dsmall|dbig")[[1]][3]
          # filter contaminated rows
          df <- buffer_df_list[[i]]
          
          if (!is.na(property_type)){
            df <- df%>%
              filter(Property_Type == property_type)
          }
          
          if (model_name =="contaminated_removed"){
            df <- df%>%
              filter(contaminated_rows==0)
            
          } else if (model_name =="potentialcontamination_removed"){
            df <- df%>%
              filter(potential_contamination==0)
          }
          # apply event study function
          model_property <- get_event_study_df1(df, period_D_list = seq(from=period_duration[1]*30,
                                                                        to=period_duration[2]*30,by=45), #seq(from=-12*30,to=6*30,by=45),# seq(from=-18*30,to=12*30,by=45)
                                                control_vars = control_vars,
                                                base_period=BASE_PERIOD, include_Dt=include_Dt,
                                                Dt_to_days=TRUE,include.lowest=TRUE)
          
          
          # print(model_property)
          # get model results
          results_df <- get_model_results(model_property, model_name = sprintf("Treat%s_Control%s",small_radius, big_radius))
          results_df
        })
        #--------------concat results-----------------------------------------
        local_DID_df_list <- as.data.frame(do.call(rbind, local_DID_df_list))
        # export data
        write.csv(local_DID_df_list,
                  file.path(buffer_dir,buffer_sub_dir,
                            sprintf("EventStudy_%s_BP%s_Dt%s.csv",
                                    model_name,BASE_PERIOD,include_Dt)
                  ),
                  row.names = FALSE)
        # local_DID_df_list
        print(file.path(buffer_dir,buffer_sub_dir,
                        sprintf("EventStudy_%s_BP%s_Dt%s.csv",
                                model_name,BASE_PERIOD,include_Dt)
        ))
      })
    })
  })
  
  ### 2.4.1 Plot event study (estimates) - 45 days
  period_D_list <- as.character(seq(from=period_duration[1]*30,to=period_duration[2]*30,by=45))
  
  period_D_list <- unlist(sapply(c(1:length(period_D_list)-1),function(x){
    sprintf("treat x Dt_%s_%s", sub("-","min",period_D_list[x]), 
            sub("-","min",period_D_list[x+1]))
  }))
  
  sapply(c(TRUE, FALSE), function(include_Dt){
    sapply(c("Dt_min45_0"), function(BASE_PERIOD){
      sapply(model_names, function(x){
        try(
          plot_event_study_estimates1(fp = file.path(buffer_dir,buffer_sub_dir,
                                                     sprintf("EventStudy_%s_BP%s_Dt%s.csv",
                                                             x,BASE_PERIOD,include_Dt)
          ),
          
          save_fp=file.path(getwd(),"Exported_Data","exported_figures",buffer_sub_dir,
                            sprintf("EventStudyEstimate_%s_BP%s_Dt%s.svg",
                                    x,BASE_PERIOD,include_Dt)
          ),
          base_period=sprintf("treat x %s",BASE_PERIOD),
          period_D_list = period_D_list
          )
        )
        
      })
    })
  })
  
  # 3 Adaptation Heterogeneity via adaptation likelihood
  
  ## 3.1 Condominium/Apartment TWFE
  #--------------------4. Adaptation heterogeneity------------------------------------
  # het_vars <- "treat_adaptation_prob" # "treat_adaptation_prob_logit"
  het_vars_list <-c("norm_treat_adaptation_prob", "norm_treat_adaptation_prob_logit", 
                    "D_p25_treat_adaptation_prob","D_p25_treat_adaptation_prob_logit",
                    "D_p50_treat_adaptation_prob","D_p50_treat_adaptation_prob_logit",
                    "D_p75_treat_adaptation_prob","D_p75_treat_adaptation_prob_logit",
                    "D_mean_treat_adaptation_prob", "D_mean_treat_adaptation_prob_logit")
  
  
  lapply(het_vars_list, function(het_vars){
    lapply(model_names, function(model_name){
      
      local_DID_df_list <- lapply(seq_along(buffer_df_list), function(i){
        # control variables
        
        # control_vars <- c("Type_of_Sale","Area_.SQM.","Building_Age","Floor_level","is_ground_floor")
        
        # small radius, big radius (treatment, control group)
        name_df <- names(buffer_df_list)[i]
        small_radius <- strsplit(name_df,split="dsmall|dbig")[[1]][2]
        big_radius <- strsplit(name_df,split="dsmall|dbig")[[1]][3]
        
        # filter property type so we focus analysis on different panels
        local_DID_df <- buffer_df_list[[i]]
        
        if (!is.na(property_type)){
          local_DID_df <- local_DID_df%>%
            filter(Property_Type == property_type)
        }
        
        if (model_name =="contaminated_removed"){
          local_DID_df <- local_DID_df%>%
            filter(contaminated_rows==0)
          
        } else if (model_name =="potentialcontamination_removed"){
          local_DID_df <- local_DID_df%>%
            filter(potential_contamination==0)
        }
        
        # filter local_DID_df based on period_D, only include period_D from -18 to 12
        local_DID_df <- local_DID_df%>%
          filter((period_D>=period_duration[1]) & (period_D<=period_duration[2]))
        
        treat_vars <- c("treat")
        post_vars <- c("post")
        # model formula
        model_property_formula <- feols_formula(y_var = y_var,
                                                control_vars=c(control_vars),#, treat_vars,post_vars),
                                                specified_interaction_vars=c(
                                                  sprintf("%s * %s * %s", treat_vars,post_vars, het_vars)
                                                ),
                                                interaction_sep = "*",
                                                fe_vars = fe_vars,
                                                cluster_vars = cluster_vars)
        
        # print(model_property_formula)
        
        # fit TWFE model
        model_property <- feols(
          model_property_formula$formula,
          cluster = model_property_formula$cluster,
          data = local_DID_df
        )
        
        summary(model_property)
        results_df <- get_model_results(model_property, model_name = sprintf("Treat%s_Control%s",small_radius, big_radius))
        
        results_df
      })
      
      local_DID_df_list <- as.data.frame(do.call(rbind, local_DID_df_list))
      # export data
      write.csv(local_DID_df_list,file.path(buffer_dir,buffer_sub_dir,
                                            sprintf("%s_TWFE_%s.csv",het_vars,model_name)
      ),
      row.names = FALSE)
      local_DID_df_list
    })
  })
  
  ### 3.1.1 Plot adaptation DID (estimates)
  lapply(het_vars_list, function(het_vars){
    sapply(model_names, function(x){
      try(
        plot_local_DID_robustness(fp = file.path(buffer_dir,buffer_sub_dir,
                                                 sprintf("%s_TWFE_%s.csv",het_vars,x)
        ),
        
        save_fp=file.path(getwd(),"Exported_Data","exported_figures",buffer_sub_dir,
                          sprintf("%s_TWFE_%s.svg",het_vars,x)
        ),
        
        filter_regex="^treat\\sx\\spost.*",
        significance_regex = "\\*+|\\."
        )
      )
    })
  })
  
  end.time <- Sys.time()
  time.taken <- (end.time - start.time)/60
  print(sprintf("Execution time for %s: %.3f",buffer_sub_dir,time.taken))
  
}

# property_type= "Condominium/Apartment",
# cluster_vars = c("SUBZONE_N"),
# fe_vars = c("SUBZONE_N","month_year"),
# period_duration = c(-18,12),
# model_names = c("potentialcontamination_removed","contaminated_removed"),

# stacked_did_models <- list(
#   "flood_adapt0" = c("Type_of_Sale","Building_Age","Floor_level"),
#   "flood_adapt1" = c("Type_of_Sale","Building_Age","Floor_level",
#                      "distance_to_CBD","distance_to_MRT"),
#   "flood_adapt2" = c("Type_of_Sale","Building_Age","Floor_level",
#                      "distance_to_CBD","log_distance_to_MRT"),
#   "flood_adapt3" = c("Type_of_Sale","Building_Age","Floor_level",
#                      "log_distance_to_CBD","log_distance_to_MRT")
# )



# stacked_did_models <- list(
#   "flood_adapt4" = c("Type_of_Sale","Building_Age","Floor_level",
#                      "distance_to_CBD","distance_to_MRT1"),
#   
#   "flood_adapt5" = c("Type_of_Sale","Building_Age","Floor_level",
#                      "log_distance_to_CBD","log_distance_to_MRT1"),
#   
#   "flood_adapt6" = c("Type_of_Sale","Building_Age1","Floor_level",
#                      "distance_to_CBD","distance_to_MRT1"),
#   
#   "flood_adapt7" = c("Type_of_Sale","log_Building_Age1","Floor_level",
#                      "log_distance_to_CBD","log_distance_to_MRT1")
  # "flood_adapt8" = c("Type_of_Sale","log_Building_Age1","Floor_level")
  # "flood_adapt9" = c("Type_of_Sale","log_Building_Age1","Floor_level",
  #                    "log_distance_to_CBD","log_distance_to_MRT1")
  # "flood_adapt10" = c("Type_of_Sale","log_Building_Age1","Floor_level",
  #                    "log_distance_to_CBD","log_distance_to_MRT1")
# )

#---model specifications---

stacked_did_models <- list(
    "landed_flood_adapt1" = c("Type_of_Sale","log_Building_Age1",
                       "log_distance_to_CBD","log_distance_to_MRT1")
  )
  


#--- RUN BATCH----
lapply(seq_along(stacked_did_models), function(i){
  buffer_sub_dir <- names(stacked_did_models)[i]
  control_vars <- stacked_did_models[[i]]
  
  main(y_var = "log_price_PSM",
    # property_type= "Condominium/Apartment",
    # property_type= NA,
    property_type= "Landed",
    cluster_vars = c("PLN_AREA_N"),#c("SUBZONE_N"),"Postal_District","Postal_Sector","PLN_AREA_N"
    fe_vars = c("PLN_AREA_N","month_year"),#c("SUBZONE_N","month_year"),c("PLN_AREA_N","month_year")
    period_duration = c(-18,12),
    model_names = c("potentialcontamination_removed","contaminated_removed"),
    control_vars = control_vars,
    buffer_sub_dir = buffer_sub_dir
  )
})

# main(y_var = "log_price_PSM",
#   property_type= "Condominium/Apartment",
#   cluster_vars = c("SUBZONE_N"),
#   fe_vars = c("SUBZONE_N","month_year"),
#   period_duration = c(-18,12),
#   model_names = c("potentialcontamination_removed","contaminated_removed"),
#   control_vars = c("Type_of_Sale","Building_Age","Floor_level",
#                    "distance_to_CBD","distance_to_MRT"),
#   buffer_sub_dir = "flood_adapt0"
# )