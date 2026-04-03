# helper functions

plot_local_DID_robustness = function(fp = NA, local_DID_df_list = NA, save_fp=NA){
  # fp (str): filepath to model_results e.g file.path(getwd(),"Exported_Data","flood_buffer_dist","processed_df",
  # sprintf("model_trimmed12months_localDID_%s_CLUSTER%s_FE%s.csv",y_var,cluster_vars[1],fe_vars[1]))
  # save_fp (str): save plot
  # import csv of model results
  if (!is.na(fp)){
    local_DID_df_list <- read.csv(fp)
  }
  
  # process data to filter the significant DID interaction (Treat x Post) variables
  plot_local_DID_df_list <- local_DID_df_list%>%
    filter(grepl("^TREAT.*\\sx\\sPOST.*", Vars))%>%
    filter(grepl("\\*+",Significance))%>%
    mutate(POST = str_pad(str_extract(Vars, "(?<=POST_).+?(?=_months_d)"),2,pad="0"),
           TREAT = str_extract(Robustness_test, "(?<=Treat).+?(?=_)"),
           CONTROL = str_extract(Robustness_test, "(?<=Control).+?$")
    )%>%
    mutate_at(vars(POST,TREAT,CONTROL,Estimate),as.numeric)%>%
    mutate(title_labeller = sprintf("POST %s months", str_pad(POST,2,pad="0")))%>%
    arrange(POST)#%>%
 
  
  # plot heatmap, where each subplot represents the POST duration
  plot_local_DID_df_list%>%
    ggplot(aes(x = TREAT, y = CONTROL, col = Estimate, label = Estimate)) +
    # geom_tile() +
    geom_point(aes(size=abs(Estimate))) + 
    facet_wrap(vars(title_labeller)) + # Facet by the grouping_var
    # scale_fill_gradient2(low="red", mid="white", high="blue",midpoint=0)+
    scale_color_gradient2(low="red", mid="white", high="blue",midpoint=0)+
    labs(x="Treatment buffer radius (m)",y="Control buffer radius (m)",
         color="Signed Estimate", size="Absolute Estimate") +
    scale_x_continuous(breaks = seq(50, 450, by = 100)) +
    scale_y_continuous(breaks = seq(500, 1000, by = 100))+
    # reduce all point size proportionally
    scale_size(range = c(0, 2))+
    theme_bw()
  
  if (!is.na(save_fp)) {
    # Save the last plot as an SVG
    ggsave(filename = save_fp,width = 7, height = 5, units = "in")
  }
  
  plot_local_DID_df_list
  
}

plot_floodProne_local_DID_robustness = function(fp = NA, local_DID_df_list = NA, save_fp=NA){
  # fp (str): filepath to model_results e.g file.path(getwd(),"Exported_Data","flood_buffer_dist","processed_df",
  # sprintf("model_trimmed12months_localDID_%s_CLUSTER%s_FE%s.csv",y_var,cluster_vars[1],fe_vars[1]))
  # save_fp (str): save plot
  # import csv of model results
  if (!is.na(fp)){
    local_DID_df_list <- read.csv(fp)
  }
  
  # process data to filter the significant DID interaction (Treat x Post) variables
  plot_local_DID_df_list <- local_DID_df_list%>%
    filter(grepl("^TREAT.*", Vars))%>%
    filter(grepl("\\*+",Significance))%>%
    mutate(TREAT = str_extract(Robustness_test, "(?<=Treat).+?(?=_)"),
           CONTROL = str_extract(Robustness_test, "(?<=Control).+?$")
    )%>%
    mutate_at(vars(TREAT,CONTROL,Estimate),as.numeric)
  
  # plot heatmap, where each subplot represents the POST duration
  plot_local_DID_df_list%>%
    ggplot(aes(x = TREAT, y = CONTROL, col = Estimate, label = Estimate)) +
    # geom_tile() +
    geom_point(aes(size=abs(Estimate))) +
    scale_color_gradient2(low="red", mid="white", high="blue",midpoint=0)+
    labs(x="Treatment buffer radius (m)",y="Control buffer radius (m)",
         color="Signed Estimate", size="Absolute Estimate") +
    scale_x_continuous(breaks = seq(50, 450, by = 50)) +
    scale_y_continuous(breaks = seq(500, 1000, by = 50))+
    # reduce all point size proportionally
    scale_size(range = c(0, 3))+
    theme_bw()
  
  if (!is.na(save_fp)) {
    # Save the last plot as an SVG
    ggsave(filename = save_fp,width = 5.5, height = 3.5, units = "in")
  }
  
  plot_local_DID_df_list
  
}



get_floodProne_local_DID_df = function(buffer_df, small_radius, big_radius,
                                       y_var, fe_vars, cluster_vars){
  # buffer_df (df): df with residential attributes and TREAT AND POST variables
  # small radius (int): between 50 to 1000
  # big radius (int): between 50 to 1000, but must be bigger than small radius
  
  property_att_columns <- c(y_var, "Type_of_Sale",
                            "Area_.SQM.","Building_Age","Floor_level","is_ground_floor",
                            "month_year",fe_vars[1],cluster_vars[1],"Property_Type"
  )
  
  treatment_buffer_col <- sprintf("TREAT_d%s",small_radius)
  control_buffer_col <- sprintf("TREAT_d%s",big_radius)
  
  
  # for the larger buffer radius, filtering values == True - this serves as the filter to identify all the control group properties
  # for the inner buffer radius, values that are True serve as the treatment group that are within the smaller radius, if they are True within the smaller radius, they must be True for the larger radius
  buffer_df <- buffer_df%>%
    # create additional cols for y-var and time FE
    mutate(log_price_PSM = log(Unit_Price_.._PSM.),
           log_price = log(Transacted_Price_...),
           month_year = paste(month, year, sep = "_"))%>%
    # select relevant columns
    select(c(property_att_columns,
             treatment_buffer_col, 
             control_buffer_col))
  
  buffer_df <- buffer_df%>%
    # convert to boolean
    mutate_at(c(names(buffer_df)[grepl("^TREAT",names(buffer_df))],"is_ground_floor"),as.logical)%>%
    mutate(across(where(is.character), as.factor))%>%
    # filter obs based on larger radius
    # filter({{control_buffer_col}} == "TRUE")
    filter((!!as.name(control_buffer_col)) == TRUE)
  
  
  buffer_df
}

get_local_DID_df = function(buffer_df, small_radius, big_radius, months_post_flood,
                            y_var, fe_vars, cluster_vars){
  # buffer_df (df): df with residential attributes and TREAT AND POST variables
  # small radius (int): between 50 to 1000
  # big radius (int): between 50 to 1000, but must be bigger than small radius
  # months_post_flood (vector of int): from 1 to 12
  
  property_att_columns <- c(y_var, "Type_of_Sale",
                            "Area_.SQM.","Building_Age","Floor_level","is_ground_floor",
                            "month_year",fe_vars[1],cluster_vars[1],"Property_Type"
  )
  
  treatment_buffer_col <- sprintf("TREAT_d%s",small_radius)
  control_buffer_col <- sprintf("TREAT_d%s",big_radius)
  # for the post treatment (POST* columns), filter the ones that correspond to the larger buffer radius
  months_post_flood_cols <- sprintf("POST_%s_months_d%s", months_post_flood,big_radius)
  
  
  # for the larger buffer radius, filtering values == True - this serves as the filter to identify all the control group properties
  # for the inner buffer radius, values that are True serve as the treatment group that are within the smaller radius, if they are True within the smaller radius, they must be True for the larger radius
  buffer_df <- buffer_df%>%
    # create additional cols for y-var and time FE
    mutate(log_price_PSM = log(Unit_Price_.._PSM.),
           log_price = log(Transacted_Price_...),
           month_year = paste(month, year, sep = "_"))%>%
    # select relevant columns
    select(c(property_att_columns,
             treatment_buffer_col, 
             control_buffer_col, 
             months_post_flood_cols))
  
  buffer_df <- buffer_df%>%
    # convert to boolean
    mutate_at(c(names(buffer_df)[grepl("^POST|^TREAT",names(buffer_df))],"is_ground_floor"),as.logical)%>%
    mutate(across(where(is.character), as.factor))%>%
    # filter obs based on larger radius
    # filter({{control_buffer_col}} == "TRUE")
    filter((!!as.name(control_buffer_col)) == TRUE)
  
  
  buffer_df
}

get_flood_only_df = function(fp, y_var, fe_vars, cluster_vars, boolean_columns){
  # fp (str): filepath to csv
  # y_var (str): dependent variable
  # fe_vars (vector of str): fixed effect variables
  # boolean_columns (vector of str): column names to convert into boolean columns
  fe <- fe_vars[1]
  buffer_time_period <- read.csv(fp)
  
  buffer_time_period <- buffer_time_period%>%
    # y-dependent variable
    mutate(log_price_PSM = log(Unit_Price_.._PSM.),
           log_price = log(Transacted_Price_...),
           month_year = paste(month, year, sep = "_")
    )%>%
    select(c(y_var, Type_of_Sale,
             Area_.SQM.,Building_Age,Floor_level,is_ground_floor,
             sprintf("within_%s_months_post_flood",months_post_flood),
             month_year,
             fe,cluster_vars[1],"Property_Type"
    ))%>%
    # convert to boolean
    mutate_at(c(boolean_columns,"is_ground_floor"),as.logical)%>%
    mutate(across(where(is.character), as.factor))
  
  buffer_time_period
}

get_transaction_df = function(fp, y_var, fe_vars,months_post_flood){
  # fp (str): filepath to csv
  # y_var (str): dependent variable
  # fe_vars (vector of str): fixed effect variables
  # months_post_flood (vector of int): observation panel (event study)
  
  transaction_master_df <- read.csv(fp)
  
  transaction_master_df <- transaction_master_df%>%
    # y-dependent variable
    mutate(log_price_PSM = log(Unit_Price_.._PSM.),
           log_price = log(Transacted_Price_...))%>%
    # convert to factors so fixest will create dummy variables
    mutate_at(c("Type_of_Sale","Type_of_Area","Property_Type","Tenure",
                "Postal_District","Postal_Sector",
                "time_since_flood",
                # "min_travel_time_work_region",
                "stn_lines",
                "work_categories","drainage_period",
                "Building_Name","Project_Name",
                "PLN_AREA_N","SUBZONE_N"),as.factor)%>%
    # convert to boolean
    mutate_at(c(sprintf("within_%s_months_post_flood",months_post_flood),
                "parks_within_400m","malls_within_400m",
                "sch_within_1km_car","sch_within_2km_car",
                "sch_within_1km_walk","sch_within_2km_walk",
                "is_ground_floor","prone_to_high_tide","within_flooding_hotspot"),as.logical)%>%
    # create month-year column as we want month_year to be the fixed effect
    mutate(month_year = as.factor(paste(month, year, sep = "_")))%>%
    # create boolean column for MRT stn
    mutate(near_mrt = ifelse(existing_stn_count>0, TRUE, FALSE))%>%
    # drop columns
    select(-c("Transacted_Price_...",                       # irrelevant to be control vars
              "Sale_Date",                                  # irrelevant to be control vars
              "Unit_Price_.._PSM.",                         # remove the other y-dependent var
              "year","month",                               # remove since year-month is the fixed effect
              "Address",                                    # remove the other potential spatial fixed effect
              "DEM",                               # absorbed by strong project/building FEs
              "drainage_density_.km_km2.",                              # can be removed since it is absorbed by month-year FE
              "stn_lines","upcoming_stn_count",
              "Completion_Date",                              # absorbed by strong project/building FEs 
              "Postal_Code","Postal_District","Postal_Sector",# absorbed by strong project/building FEs residuals
              "Type_of_Area",# absorbed by strong project/building FEs. Require Property Type for filtering
    )            
    )%>%
    # drop chr columns
    select(-where(is.character))
  
  if (fe_vars[1]=="Project_Name"){
    transaction_df<- transaction_master_df%>%
      select(-c("Building_Name","PLN_AREA_N","SUBZONE_N"))
  } else if (fe_vars[1]=="Building_Name"){
    transaction_df<- transaction_master_df%>%
      select(-c("Project_Name","PLN_AREA_N","SUBZONE_N"))
  } else if (fe_vars[1]=="SUBZONE_N"){
    transaction_df<- transaction_master_df%>%
      select(-c("Project_Name","Building_Name","PLN_AREA_N"))
  } else if (fe_vars[1]=="PLN_AREA_N"){
    transaction_df<- transaction_master_df%>%
      select(-c("Project_Name","Building_Name","SUBZONE_N"))
  }

  if (y_var=="log_price"){
    transaction_df<- transaction_df%>%
      select(-c("log_price_PSM"))
  } else if (y_var=="log_price_PSM"){
    transaction_df<- transaction_df%>%
      select(-c("log_price"))
  }
  transaction_df

}


