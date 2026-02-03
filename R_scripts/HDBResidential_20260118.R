library(dplyr)
library(fixest)
# library(stargazer)
library(modelsummary)
library(tinytable)

# Import functions from script_a.R
source(file.path(getwd(),"R_scripts","fixest_helper_functions.R"))

# 
# # Initialise FE and cluster parameters
# 
# fe_vars <- c("Building_Name","month_year")
# cluster_vars <- c("SUBZONE_N")
# y_var <- "log_price_PSM" # or log_price
# # test flood exposure one at a time, not all together because then there would be multicollinearity
# hazard_var <- "within_6_months_post_flood" #"within_6_months_post_flood","within_12_months_post_flood","within_18_months_post_flood","weeks_since_flood"



main = function(flood_residential_fp,
                y_var="log_price_PSM", fe_vars=c("Building_Name","month_year"),
                hazard_var="within_6_months_post_flood", cluster_vars=c("SUBZONE_N"),
                save_dir = file.path(getwd(),"Exported_Data"),save_fp_suffix="1"){
  # Args:
  # flood_residential_fp (str): filepath to dataframe
  # y_var (str): name of the dependent variable
  # fe_vars (vector of str): fixed effects variables
  # hazard_var (str): hazard variable (treatment)
  # cluster_vars (vector of str): variable to cluster the errors
  # save_dir (str): name of directory to save the file in
  # save_fp_suffix (str): suffix to identify unique files
  
  # unique filename for saving results
  
  save_fp <- unlist(strsplit(basename(flood_residential_fp),"[.]"))
  save_fp <- paste(c(save_fp[1],save_fp_suffix),collapse="-")
  save_fp <- gsub("adaptation","adapt",save_fp)
  save_fp <- gsub("floodHDBResidential","HDBRes",save_fp)
  # save_fp <-paste(c(save_fp,paste(fe_vars,collapse="-"),y_var),collapse="_")
  # save_fp <- paste(save_fp,collapse)
  # save_fp
  
  
  # Import residential transaction price
  
  flood_residential_transaction <- read.csv(flood_residential_fp)
  flood_residential_transaction
  
  # columns to drop
  drop_columns <- c("Sale_Date",                                  # irrelevant to be control vars
                    "Transacted_Price_...",                       # irrelevant to be control vars
                    "year","month",                               # remove since year-month is the fixed effect
                    # "Project_Name",                             # remove the other potential spatial fixed effect
                    "Address","ADDRESS",                          # remove the other potential spatial fixed effect
                    "ROAD_NAME","unique_index",
                    # absorbed by strong project/building FEs 
                    "REGION_N","PLN_AREA_N",                        # absorbed by strong project/building FEs, use subzone for clustering residuals
                    # "DOWNTOWN_CORE_travel_time","TAMPINES_travel_time",      # absorbed by strong project/building FEs
                    # "WOODLANDS_travel_time","ANG_MO_KIO_travel_time",        # absorbed by strong project/building FEs
                    # "JURONG_EAST_travel_time","min_travel_time_work_region", # absorbed by strong project/building FEs
                    # "min_travel_time_work","sch_within_1km_car",             # absorbed by strong project/building FEs
                    # "sch_within_2km_car","sch_min_distance_car",             # absorbed by strong project/building FEs
                    # "sch_within_1km_walk","sch_within_2km_walk",             # absorbed by strong project/building FEs
                    # "betweeness_centrality","closeness_centrality",          # absorbed by strong project/building FEs      
                    # "sch_min_distance_walk","malls_within_400m",             # absorbed by strong project/building FEs
                    # "parks_within_400m",                                     # absorbed by strong project/building FEs
                    "DEM","drainage_density_.km_km2."                        # can be removed since it is absorbed by month-year FE
  ) 
  
  if (fe_vars[1] == "Building_Name") {
    drop_columns <- c(drop_columns, "Project_Name")
  } else {
    drop_columns <- c(drop_columns, "Building_Name")
  }
  
  # transaction_df
  
  if (y_var == "log_price_PSM") {
    transaction_df <- flood_residential_transaction%>%
      # y-dependent variable
      mutate(log_price_PSM = log(Transacted_Price_.../Area_.SQM.))
    
  } else {
    
    transaction_df <- flood_residential_transaction%>%
      # y-dependent variable
      mutate(log_price = log(Transacted_Price_...))
    
  }
  
  transaction_df <- transaction_df%>%
    # convert to factors so fixest will create dummy variables
    mutate_at(c("Property_Type","Floor_level","flat_model",
                "PLN_AREA_N","SUBZONE_N","REGION_N",
                # "min_travel_time_work_region",
                "stn_lines","month_year",
                "work_categories","drainage_period"),as.factor)%>%
    # convert to boolean
    mutate_at(c("within_6_months_post_flood","within_12_months_post_flood","within_18_months_post_flood",
                # "parks_within_400m",
                "is_ground_floor","prone_to_high_tide","within_flooding_hotspot"),as.logical)%>%
    # drop columns
    select(-drop_columns)
  
  
  names(transaction_df)
  
  # Hedonic models
  
  ## Building attributes only
  
  model_property_formula <- feols_formula(y_var = y_var,
                                          control_vars=setdiff(names(transaction_df)[c(1:13)],
                                                               c("betweeness_centrality","closeness_centrality")),
                                          interaction_vars = NA,
                                          fe_vars = fe_vars,
                                          cluster_vars = cluster_vars
  )
  # model_property_formula
  
  # model where fixed effects are project name and month_year
  model_property <- feols(
    model_property_formula$formula,
    cluster = model_property_formula$cluster,
    data = transaction_df
  )
  # summary(model_property)
  # # display model outputs
  # etable(model_property)
  
  ## 0: Baseline model - flood hotspot
  
  ### 0A: Effect of flood on prices on private apartment located in flood-spot regions versus non-flood-spot regions
  
  model_0a_formula <- feols_formula(y_var = y_var,
                                    control_vars=setdiff(names(transaction_df),
                                                         c("stn_lines","Property_Type",
                                                           "betweeness_centrality","closeness_centrality",
                                                           "within_6_months_post_flood","within_12_months_post_flood",
                                                           "within_18_months_post_flood","weeks_since_flood",
                                                           "within_flooding_hotspot","prone_to_high_tide",
                                                           "work_categories","drainage_period")),
                                    interaction_vars =  interaction_combinations(
                                      hazards = c(hazard_var),
                                      vulnerability = c("within_flooding_hotspot")
                                    ),
                                    interaction_sep = "*",
                                    fe_vars = fe_vars,
                                    cluster_vars = cluster_vars
  )
  # model_0a_formula
  
  # model where fixed effects are project name and month_year
  model_0a <- feols(
    model_0a_formula$formula,
    cluster = model_0a_formula$cluster,
    data = transaction_df
  )
  # summary(model_0a)
  # # display model outputs
  # etable(model_0a)
  
  
  
  ## 1: Baseline model - flood occurrence
  
  ### 1A: Effect of flood on prices on private apartment's ground floor units
  
  model_1a_formula <- feols_formula(y_var = y_var,
                                    control_vars=setdiff(names(transaction_df),
                                                         c("stn_lines","Property_Type",
                                                           "betweeness_centrality","closeness_centrality",
                                                           "within_6_months_post_flood","within_12_months_post_flood",
                                                           "within_18_months_post_flood","weeks_since_flood",
                                                           "within_flooding_hotspot","prone_to_high_tide",
                                                           "work_categories","drainage_period")),
                                    interaction_vars =  interaction_combinations(
                                      hazards = c(hazard_var),
                                      vulnerability = c("is_ground_floor")
                                    ),
                                    interaction_sep = "*",
                                    fe_vars = fe_vars,
                                    cluster_vars = cluster_vars
  )
  # model_1a_formula
  
  # model where fixed effects are project name and month_year
  model_1a <- feols(
    model_1a_formula$formula,
    cluster = model_1a_formula$cluster,
    data = transaction_df
  )
  # summary(model_1a)
  # # display model outputs
  # etable(model_1a)
  
  
  ## 2: Heterogeneity by floor levels for tide-prone vs non-tide prone private apartment
  
  # - Effect if flood on prices within tide-prone vs non-tide-prone private apartment by floor levels
  # - TODO: Conduct robustness check by varying to within_12/18_months_post_flood
  # - Within-building vertical sorting
  
  ### 2A: Continuous floor levels
  
  
  model_2a_formula <- feols_formula(y_var = y_var,
                                    control_vars=setdiff(names(transaction_df),
                                                         c("stn_lines","Property_Type",
                                                           "betweeness_centrality","closeness_centrality",
                                                           "within_6_months_post_flood","within_12_months_post_flood",
                                                           "within_18_months_post_flood","weeks_since_flood",
                                                           "prone_to_high_tide",
                                                           "work_categories","drainage_period")),
                                    interaction_vars = interaction_combinations(
                                      hazard=c(hazard_var),
                                      vulnerability=c("Floor_level")),
                                    interaction_sep = "*",
                                    specified_interaction_vars = c(sprintf("%s : prone_to_high_tide", hazard_var),
                                                                   sprintf("%s : prone_to_high_tide : Floor_level", hazard_var)),
                                    fe_vars = fe_vars,
                                    cluster_vars = cluster_vars
  )
  # model_2a_formula
  
  # model where fixed effects are project name and month_year
  model_2a <- feols(
    model_2a_formula$formula,
    cluster = model_2a_formula$cluster,
    data = transaction_df
  )
  # summary(model_2a)
  # # display model outputs
  # etable(model_2a)
  
  ### 2B: Floor ordinal categories (for better interpretability)
  
  model_2b_formula <- feols_formula(y_var = y_var,
                                    control_vars=setdiff(names(transaction_df),
                                                         c("stn_lines","Property_Type",
                                                           "betweeness_centrality","closeness_centrality",
                                                           "within_6_months_post_flood","within_12_months_post_flood",
                                                           "within_18_months_post_flood","weeks_since_flood",
                                                           "prone_to_high_tide",
                                                           "work_categories","drainage_period")),
                                    interaction_vars = interaction_combinations(
                                      hazard=c(hazard_var),
                                      vulnerability=c("Floor_level")),
                                    interaction_sep = "*",
                                    specified_interaction_vars = c(sprintf("%s : prone_to_high_tide",hazard_var),
                                                                   sprintf("%s : prone_to_high_tide : Floor_level",hazard_var)),
                                    fe_vars = fe_vars,
                                    cluster_vars = cluster_vars
  )
  # model_2b_formula
  
  # model where fixed effects are project name and month_year
  model_2b <- feols(
    model_2b_formula$formula,
    cluster = model_2b_formula$cluster,
    data = transaction_df
  )
  # summary(model_2b)
  # # display model outputs
  # etable(model_2b)
  
  ## 3: Heterogeneity by coastal properties (i.e. tide-prone vs non-tide prone) for ground floor units
  
  # - Effect of flood on prices across ground-floor units by tide-proneness. 
  # - I.e. Among ground floor units only, does flood exposure affect prices differently in tide-prone vs non-tide prone projects/building
  # - This allows us to isolate pure coastal backflow/drainage constraints
  # - TODO: Conduct robustness check by varying to within_12/18_months_post_flood
  # - Horizontal vulnerability
  
  model_3_formula <- feols_formula(y_var = y_var,
                                   control_vars=setdiff(names(transaction_df),
                                                        c("stn_lines","Property_Type",
                                                          "is_ground_floor","Floor_level",
                                                          "betweeness_centrality","closeness_centrality",
                                                          "within_6_months_post_flood","within_12_months_post_flood",
                                                          "within_18_months_post_flood","weeks_since_flood",
                                                          "work_categories","drainage_period")),
                                   
                                   specified_interaction_vars = c(hazard_var,
                                                                  sprintf("%s : prone_to_high_tide",hazard_var)),
                                   fe_vars = fe_vars,
                                   cluster_vars = cluster_vars
  )
  # model_3_formula
  
  # model where fixed effects are project name and month_year
  model_3 <- feols(
    model_3_formula$formula,
    cluster = model_3_formula$cluster,
    data = transaction_df%>%
      filter(is_ground_floor == TRUE)
  )
  # summary(model_3)
  # # display model outputs
  # etable(model_3)
  
  ## 4: Heterogeneity by centrality
  
  # - Effect of flood on prices by centrality
  # - i.e. Do floods matter less or more in highly connected/accessible locations?
  
  model_4_formula <- feols_formula(y_var = y_var,
                                   control_vars=setdiff(names(transaction_df),
                                                        c("stn_lines","Property_Type",
                                                          "within_6_months_post_flood","within_12_months_post_flood",
                                                          "within_18_months_post_flood","weeks_since_flood",
                                                          "prone_to_high_tide",
                                                          "work_categories","drainage_period")),
                                   
                                   specified_interaction_vars = c(hazard_var,
                                                                  sprintf("%s : betweeness_centrality",hazard_var),
                                                                  sprintf("%s : closeness_centrality",hazard_var)),
                                   fe_vars = fe_vars,
                                   cluster_vars = cluster_vars
  )
  # model_4_formula
  
  # model where fixed effects are project name and month_year
  model_4 <- feols(
    model_4_formula$formula,
    cluster = model_4_formula$cluster,
    data = transaction_df
  )
  # summary(model_4)
  # # display model outputs
  # etable(model_4)
  
  ## 5: Heterogeneity by flood adaptation measures
  
  ### 5.0A: How does adaptation moderate the effect of flood on prices on private apartment located in flood-spot regions versus non-flood-spot regions
  
  # - does that even make sense? because if it's in a flood prone area that means flood adaptation hasnt been implemented. The effect of flood adaptation may already be absorbed in the non-flood prone area already
  # - or it could be interpreted as exogenous impacts from flood adaptation works on flood prone area
  
  model_5a_formula <- feols_formula(y_var = y_var,
                                    control_vars=setdiff(names(transaction_df),
                                                         c("stn_lines","Property_Type",
                                                           "betweeness_centrality","closeness_centrality",
                                                           "within_6_months_post_flood","within_12_months_post_flood",
                                                           "within_18_months_post_flood","weeks_since_flood",
                                                           "prone_to_high_tide",
                                                           "work_categories","drainage_period")),
                                    
                                    specified_interaction_vars = c("within_flooding_hotspot",
                                                                   "work_categories : drainage_period",
                                                                   # vulnerability
                                                                   "within_flooding_hotspot : work_categories : drainage_period"
                                                                   # "is_ground_floor : work_categories : drainage_period"
                                    ),
                                    fe_vars = fe_vars,
                                    cluster_vars = cluster_vars
  )
  # model_5a_formula
  
  # model where fixed effects are project name and month_year
  model_5a <- feols(
    model_5a_formula$formula,
    cluster = model_5a_formula$cluster,
    data = transaction_df
  )
  # summary(model_5a)
  # # display model outputs
  # etable(model_5a)
  
  
  ### 5.1A: How does adaptation moderate the effect of flood on prices on private apartment's ground floor units
  
  model_5c_formula <- feols_formula(y_var = y_var,
                                    control_vars=setdiff(names(transaction_df),
                                                         c("stn_lines","Property_Type",
                                                           "betweeness_centrality","closeness_centrality",
                                                           "within_6_months_post_flood","within_12_months_post_flood",
                                                           "within_18_months_post_flood","weeks_since_flood",
                                                           "within_flooding_hotspot","prone_to_high_tide",
                                                           "is_ground_floor","Floor_level",
                                                           "work_categories","drainage_period")),
                                    
                                    specified_interaction_vars = c("is_ground_floor",
                                                                   "work_categories : drainage_period",
                                                                   sprintf("%s : is_ground_floor : work_categories : drainage_period", hazard_var)),
                                    fe_vars = fe_vars,
                                    cluster_vars = cluster_vars
  )
  # model_5c_formula
  
  # model where fixed effects are project name and month_year
  model_5c <- feols(
    model_5c_formula$formula,
    cluster = model_5c_formula$cluster,
    data = transaction_df
  )
  # summary(model_5c)
  # # display model outputs
  # etable(model_5c)
  
  
  
  ### 5.2A: How does adaptation moderate the effect of flood on prices by floor levels for tide-prone vs non-tide prone private apartment
  
  model_5e_formula <- feols_formula(y_var = y_var,
                                    control_vars=setdiff(names(transaction_df),
                                                         c("stn_lines","Property_Type",
                                                           "betweeness_centrality","closeness_centrality",
                                                           "prone_to_high_tide",
                                                           "within_flooding_hotspot",
                                                           "is_ground_floor","Floor_level",
                                                           "within_6_months_post_flood",
                                                           "within_12_months_post_flood", 
                                                           "within_18_months_post_flood",
                                                           "weeks_since_flood")),
                                    
                                    
                                    specified_interaction_vars = c("Floor_level",
                                                                   "Floor_level : prone_to_high_tide : work_categories : drainage_period"),
                                    fe_vars = fe_vars,
                                    cluster_vars = cluster_vars
  )
  # model_5e_formula
  
  # model where fixed effects are project name and month_year
  model_5e <- feols(
    model_5e_formula$formula,
    cluster = model_5e_formula$cluster,
    data = transaction_df
  )
  # summary(model_5e)
  # # display model outputs
  # etable(model_5e)
  
  ### 5.2B: How does adaptation moderate the effect of flood on prices by Floor categories for tide-prone vs non-tide prone private apartment
  
  model_5f_formula <- feols_formula(y_var = y_var,
                                    control_vars=setdiff(names(transaction_df),
                                                         c("stn_lines","Property_Type",
                                                           "betweeness_centrality","closeness_centrality",
                                                           "prone_to_high_tide",
                                                           "within_flooding_hotspot",
                                                           "is_ground_floor","Floor_level",
                                                           "within_6_months_post_flood",
                                                           "within_12_months_post_flood", 
                                                           "within_18_months_post_flood",
                                                           "weeks_since_flood")),
                                    
                                    
                                    specified_interaction_vars = c("Floor_level",
                                                                   "work_categories : drainage_period",
                                                                   "Floor_level : work_categories : drainage_period"),
                                    fe_vars = fe_vars,
                                    cluster_vars = cluster_vars
  )
  # model_5f_formula
  
  # model where fixed effects are project name and month_year
  model_5f <- feols(
    model_5f_formula$formula,
    cluster = model_5f_formula$cluster,
    data = transaction_df
  )
  # summary(model_5f)
  # # display model outputs
  # etable(model_5f)
  
  ### 5.3: How does adaptation moderate the effect of flood on prices by coastal properties (i.e. tide-prone vs non-tide prone) for ground floor units
  
  model_5g_formula <- feols_formula(y_var = y_var,
                                    control_vars=setdiff(names(transaction_df),
                                                         c("stn_lines","Property_Type",
                                                           "betweeness_centrality","closeness_centrality",
                                                           "prone_to_high_tide",
                                                           "within_flooding_hotspot",
                                                           "is_ground_floor","Floor_level",
                                                           "within_6_months_post_flood",
                                                           "within_12_months_post_flood", 
                                                           "within_18_months_post_flood",
                                                           "weeks_since_flood")),
                                    
                                    specified_interaction_vars = c("work_categories : drainage_period",
                                                                   "prone_to_high_tide : work_categories : drainage_period"),
                                    fe_vars = fe_vars,
                                    cluster_vars = cluster_vars
  )
  model_5g_formula
  
  # model where fixed effects are project name and month_year
  model_5g <- feols(
    model_5g_formula$formula,
    cluster = model_5g_formula$cluster,
    data = transaction_df%>%
      filter(is_ground_floor == TRUE)
  )
  # summary(model_5g)
  # # display model outputs
  # etable(model_5g)
  
  ### 5.4: How does adaptation moderate the effect of flood on prices by centrality
  
  model_5h_formula <- feols_formula(y_var = y_var,
                                    control_vars=setdiff(names(transaction_df),
                                                         c("stn_lines","Property_Type",
                                                           "betweeness_centrality","closeness_centrality",
                                                           "prone_to_high_tide",
                                                           "within_flooding_hotspot",
                                                           "is_ground_floor","Floor_level",
                                                           "within_6_months_post_flood",
                                                           "within_12_months_post_flood", 
                                                           "within_18_months_post_flood",
                                                           "weeks_since_flood")),
                                    
                                    specified_interaction_vars = c("work_categories : drainage_period",
                                                                   "betweeness_centrality : work_categories : drainage_period",
                                                                   "closeness_centrality : work_categories : drainage_period"),
                                    fe_vars = fe_vars,
                                    cluster_vars = cluster_vars
  )
  model_5g_formula
  
  # model where fixed effects are project name and month_year
  model_5h <- feols(
    model_5h_formula$formula,
    cluster = model_5h_formula$cluster,
    data = transaction_df
  )
  
  # Display model results side-by-side
  
  models_hazard_vul <- list(model_0a,model_1a,
                            model_2a,model_2b,
                            model_3,model_4)
  names(models_hazard_vul) <- paste0("(",c(1:length(models_hazard_vul)),")")
  
  # modelsummary(models_hazard_vul, stars = TRUE)
  # export data
  modelsummary(models_hazard_vul, stars = TRUE, gof_omit = "IC|F|Log|AIC|BIC", output = 
                 file.path(getwd(),"Exported_Data","model_summaries",sprintf("HazardVul_%s.docx",save_fp)))
  
  models_vul_adaptation <- list(model_5a,model_5c,
                                model_5e,model_5f,
                                model_5g,model_5h)
  
  names(models_vul_adaptation) <- paste0("(",c(1:length(models_vul_adaptation)),")")
  
  # modelsummary(models_vul_adaptation, stars = TRUE)
  # export data
  modelsummary(models_vul_adaptation, stars = TRUE, gof_omit = "IC|F|Log|AIC|BIC", 
               output = file.path(getwd(),"Exported_Data","model_summaries",sprintf("VulAdapt_%s.docx",save_fp)))
  
  sprintf("Saved files in model_summaries folder...: %s", 
          file.path(getwd(),"Exported_Data","model_summaries",sprintf("%s.docx",save_fp)))
  
}

# import filepath
save_dir <- file.path(getwd(),"Exported_Data")
# flood_residential_fp <- file.path(save_dir,"flood_residential_transaction_20260114-3.csv")
flood_residential_fp <- file.path(save_dir,"floodPrivateResidential_20260118_buffer500_networkDepth2_adaptationProjectName.csv")

# Initialise FE and cluster parameters

fe_vars <- c("Building_Name","month_year") #c("Building_Name","month_year") # or c("Project_Name","month_year")
cluster_vars <- c("SUBZONE_N")
y_var <-"log_price"  #"log_price_PSM"  or log_price
# test flood exposure one at a time, not all together because then there would be multicollinearity
hazard_var <- "within_6_months_post_flood" #"within_6_months_post_flood","within_12_months_post_flood","within_18_months_post_flood","weeks_since_flood"

# list all filepaths for private properties
flood_residential_fps <- list.files(save_dir,pattern="^floodHDBResidential")
flood_residential_fps

main(file.path(save_dir, flood_residential_fps[1]),
     y_var=y_var, fe_vars=fe_vars,
     hazard_var=hazard_var, cluster_vars=cluster_vars,
     save_dir = save_dir,save_fp_suffix="FE1H1")

# increment operator
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))

# batch process

counter = 0
FEs <- list('FE1'=c("Building_Name","month_year"), 'FE2'=c("Project_Name","month_year"))
hazards <- list("H1" ="within_6_months_post_flood","H2" ="within_12_months_post_flood",
                "H3" ="within_18_months_post_flood","H4" ="weeks_since_flood")
y_vars <- list("Y1"="log_price_PSM", "Y2"="log_price")
cluster_vars <- c("SUBZONE_N")

for (i in seq_along(flood_residential_fps)) {
  for (j in seq_along(FEs)) {
    for (k in seq_along(hazards)) {
      for (l in seq_along(y_vars)){
        
        flood_residential_fp <- file.path(save_dir,flood_residential_fps[i])
        
        FE_name <- names(FEs)[j]
        fe_vars <- FEs[[j]]
        
        hazard_name <- names(hazards)[k]
        hazard_var <- hazards[[k]]
        
        y_name <- names(y_vars)[l]
        y_var <- y_vars[[l]]
        # print(sprintf("%s: %s\n%s, %s", counter, flood_residential_fp, paste(fe_vars,collapse=""), hazard_var))
        print(sprintf("%s: %s\n%s, %s", counter, flood_residential_fp, FE_name, hazard_name))
        
        main(flood_residential_fp,
             y_var=y_var, fe_vars=fe_vars,
             hazard_var=hazard_var, cluster_vars=cluster_vars,
             save_dir = save_dir,save_fp_suffix=paste0(y_name,hazard_name,FE_name))
        
        counter %+=% 1
      }
      
    }
  }
  
}


# etable(model_5h)$
# modelsummary(models_hazard_vul, stars = TRUE)
