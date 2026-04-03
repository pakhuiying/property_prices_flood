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
  # filter larger buffer radius which correpsonds to all True
  treatment_buffer_col <- sprintf("BUFFER_d%s",small_radius)
  control_buffer_col <- sprintf("BUFFER_d%s",big_radius)
  control_period_flood <- names(buffer_df)[grepl(sprintf("d%s_period_flood.*",big_radius),names(buffer_df))]
  control_period_flood <- c(control_period_flood, sprintf("d%s_treated",big_radius))
  # select rows where it starts with d[small_radius]
  small_radius_vars <- names(buffer_df)[grepl(sprintf("^d%s",small_radius),names(buffer_df))]
  big_radius_vars <- names(buffer_df)[grepl(sprintf("^d%s",big_radius),names(buffer_df))]
  # # for the post treatment (POST* columns), filter the ones that correspond to the larger buffer radius
  # months_post_flood_cols <- sprintf("d%s_Dt_%s", big_radius,months_post_flood)
  # # pre treatment vars
  # months_pre_flood_cols <- sprintf("d%s_Dt_.%s", big_radius,months_post_flood)
  
  
  # for the larger buffer radius, filtering values == True - this serves as the filter to identify all the control group properties
  # for the inner buffer radius, values that are True serve as the treatment group that are within the smaller radius, if they are True within the smaller radius, they must be True for the larger radius
  buffer_df <- buffer_df%>%
    # create additional cols for y-var and time FE
    mutate(log_price_PSM = log(Unit_Price_.._PSM.),
           log_price = log(Transacted_Price_...),
           month_year = paste(month, year, sep = "_"))%>%
    # select relevant columns
    select(all_of(c(property_att_columns,
                    "period_sale",
             treatment_buffer_col, 
             control_buffer_col, 
             control_period_flood,
             small_radius_vars,
             big_radius_vars
             # months_post_flood_cols,
             # months_pre_flood_cols
    )))
  
  buffer_df <- buffer_df%>%
    # convert to boolean
    mutate_at(all_of(c(names(buffer_df)[grepl("^BUFFER|.*treated$",names(buffer_df))],"is_ground_floor")),as.logical)%>%
    mutate(across(where(is.logical), as.integer))%>%
    mutate(across(where(is.character), as.factor))%>%
    # filter obs based on larger radius
    # filter({{control_buffer_col}} == "TRUE")
    filter((!!as.name(control_buffer_col)) == 1)
  
  buffer_df
}

plot_local_DID_robustness = function(fp = NA, local_DID_df_list = NA, save_fp=NA, 
                                     filter_regex="^BUFFER|.*treated$", 
                                     significance_regex = "\\*+",
                                     fn_regex=function(x) x){
  # fp (str): filepath to model_results e.g file.path(getwd(),"Exported_Data","flood_buffer_dist","processed_df",
  # sprintf("model_trimmed12months_localDID_%s_CLUSTER%s_FE%s.csv",y_var,cluster_vars[1],fe_vars[1]))
  # save_fp (str): save plot
  # import csv of model results
  if (!is.na(fp)){
    local_DID_df_list <- read.csv(fp)
  }
  
  # process data to filter the significant DID interaction (Treat x Post) variables
  plot_local_DID_df_list <- local_DID_df_list%>%
    filter(grepl(filter_regex, Vars))%>%
    filter(grepl(significance_regex,Significance))%>%
    mutate(BUFFER_treat = str_extract(Robustness_test, "(?<=Treat).+?(?=_)"),
           BUFFER_control = str_extract(Robustness_test, "(?<=Control).+?$")
    )%>%
    mutate_at(vars(BUFFER_treat,BUFFER_control,Estimate),as.numeric)%>%
    mutate_at(vars(Vars), fn_regex)
    # mutate(title_labeller = ifelse(grepl("^BUFFER", Vars), "flood buffer", "flood treatment"))
  
  
  # plot heatmap, where each subplot represents the POST duration
  plot_local_DID_df_list%>%
    ggplot(aes(x = BUFFER_treat, y = BUFFER_control, col = Estimate, label = Estimate)) +
    # geom_tile() +
    geom_point(aes(size=abs(Estimate))) +
    facet_wrap(vars(Vars)) + # Facet by the grouping_var
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
  
  plot_local_DID_df_list%>%
    arrange(Vars)
  
}

plot_event_study_robustness = function(fp = NA, local_DID_df_list = NA, save_fp=NA,
                                       significance_regex = "\\*+"){
  # fp (str): filepath to model_results e.g file.path(getwd(),"Exported_Data","flood_buffer_dist","processed_df",
  # sprintf("model_trimmed12months_localDID_%s_CLUSTER%s_FE%s.csv",y_var,cluster_vars[1],fe_vars[1]))
  # save_fp (str): save plot
  # import csv of model results
  if (!is.na(fp)){
    local_DID_df_list <- read.csv(fp)
  }
  
  # process data to filter the significant DID interaction (Treat x Post) variables
  plot_local_DID_df_list <- local_DID_df_list%>%
    filter(grepl(".*Dt.*", Vars))%>%
    filter(grepl(significance_regex,Significance))%>%
    mutate(BUFFER_treat = str_extract(Robustness_test, "(?<=Treat).+?(?=_)"),
           BUFFER_control = str_extract(Robustness_test, "(?<=Control).+?$")
    )%>%
    mutate_at(vars(BUFFER_treat,BUFFER_control,Estimate),as.numeric)%>%
    mutate_at(vars(Vars), function(x){
      Dt <- str_extract(x, "(?<=Dt_).+?$")
      num <- str_extract(Dt,"\\d+")
      sign <- sub(".","-",sub("\\d+","",Dt))
      num_replace<-as.numeric(gsub("\\s+","",paste(sign,num,"")))
      num_replace
      # sprintf("Dt=%s",num_replace)
      # num_replace <- str_pad(num,2,pad="0")
      # sprintf("Dt=%s",paste(sign,num_replace))
    })
  
  
  # plot heatmap, where each subplot represents the POST duration
  plot_local_DID_df_list%>%
    ggplot(aes(x = BUFFER_treat, y = BUFFER_control, col = Estimate, label = Estimate)) +
    # geom_tile() +
    geom_point(aes(size=abs(Estimate))) +
    facet_wrap(vars(Vars)) + # Facet by the grouping_var
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
    ggsave(filename = save_fp,width = 9, height = 7, units = "in")
  }
  
  plot_local_DID_df_list
  
}

plot_event_study_estimates = function(fp = NA, local_DID_df_list = NA, save_fp=NA){
  # fp (str): filepath to model_results e.g file.path(getwd(),"Exported_Data","flood_buffer_dist","processed_df",
  # sprintf("model_trimmed12months_localDID_%s_CLUSTER%s_FE%s.csv",y_var,cluster_vars[1],fe_vars[1]))
  # save_fp (str): save plot
  # import csv of model results
  if (!is.na(fp)){
    local_DID_df_list <- read.csv(fp)
  }
  
  # process data to filter the significant DID interaction (Treat x Post) variables
  plot_local_DID_df_list <- local_DID_df_list%>%
    filter(grepl(".*Dt.*", Vars))%>%
    # filter(grepl("\\*+",Significance))%>%
    mutate(BUFFER_treat = str_extract(Robustness_test, "(?<=Treat).+?(?=_)"),
           BUFFER_control = str_extract(Robustness_test, "(?<=Control).+?$")
    )
  
  
  buffer_treat_control <- unique(plot_local_DID_df_list$Robustness_test)
  dropped_Dt <- data.frame(model_results="", Estimate=0,SE=0, Significance="",Robustness_test=buffer_treat_control)%>%
    mutate(BUFFER_treat = str_extract(Robustness_test, "(?<=Treat).+?(?=_)"),
           BUFFER_control = str_extract(Robustness_test, "(?<=Control).+?$"))%>%
    mutate(Vars = paste0("d",BUFFER_treat,"_Dt_.1",",d",BUFFER_treat,"_Dt_0"))%>%
    separate_rows(Vars,sep=",")
  # merge df
  plot_local_DID_df_list <- rbind(plot_local_DID_df_list, dropped_Dt)%>%
    mutate_at(vars(Vars), function(x){
      Dt <- str_extract(x, "(?<=Dt_).+?$")
      num <- str_extract(Dt,"\\d+")
      sign <- sub(".","-",sub("\\d+","",Dt))
      num_replace<-as.numeric(gsub("\\s+","",paste(sign,num,"")))
      num_replace
    })%>%
    arrange(Robustness_test, Vars)
  
  plot_local_DID_df_list%>%
    mutate_at(vars(BUFFER_treat,BUFFER_control), as.numeric)%>%
    ggplot(aes(x=Vars, y = Estimate)) +
    geom_line(linetype = "dashed") +
    geom_point() + 
    geom_vline(xintercept=0, linetype="dashed", color="blue") + #x-intercept at 0
    geom_hline(yintercept=0, linetype="dashed", color="blue") + #y-intercept at 0
    geom_errorbar(aes(ymin = (Estimate-1.96*SE), ymax = (Estimate+1.96*SE)), width = 0.2) +
    facet_grid(vars(BUFFER_control),vars(BUFFER_treat)) +
    labs(x="Dt")+
    theme_bw()
  
  if (!is.na(save_fp)) {
    # Save the last plot as an SVG
    ggsave(filename = save_fp,width = 15, height = 10, units = "in")
  }
}