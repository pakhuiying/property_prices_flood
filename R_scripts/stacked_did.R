import_files = function(fp_list,...){
  # ... represents other df to be merged with df, ensure that the df has the column Property_ID
  df_list <- lapply(fp_list, function(fp){
    
    factor_vars <- c("Type_of_Sale","SUBZONE_N","PLN_AREA_N","REGION_N","Property_Type")
    logical_vars <- c("is_ground_floor")
    df <- read.csv(fp)%>%
      mutate(log_price_PSM = log(Unit_Price_.._PSM.),
             log_price = log(Transacted_Price_...),
             log_Area_.SQM. = log(Area_.SQM.),
             month_year = paste(month, year, sep = "_"))%>%
      mutate_at(all_of(factor_vars),as.factor)%>%
      mutate_at(all_of(logical_vars), function(x) as.integer(as.logical(x)))%>%
      # previous vuilding age was computed wrongly, leading to weird data distribution
      mutate(Completion_Date1= ifelse((Completion_Date=="Uncompleted"|Completion_Date=="-"),year,Completion_Date))%>%
      # overwrite Building Age
      mutate(Building_Age = year - as.numeric(Completion_Date1))#%>%
      # log1p = log(x+1) because there is 0 in Building_Age
      # mutate(log_Building_Age = log1p(Building_Age))
    
    # merge dataframe with other arguments e.g. distance to CBD
    add_df_list <- list(df, ...)
    if (length(add_df_list)>1){
      df <- purrr::reduce(.x = add_df_list, merge, all.x=TRUE,by="Property_ID")
    }
    # df <- merge(df,distance_to_CBD_df,all.x=TRUE,by="Property_ID")
    df
  })
  # bind_rows(df_list, .id="id")
  df_list
}

get_event_study_df = function(buffer_df, period_D_list, control_vars, base_period=-1, include_Dt=TRUE){
  # period_D_list (vector): list of period D list to obtain
  # base_period (int): base period of event study, usually just before the event e.g. -1
  
  base_period <- sprintf("Dt%s",sub("-","min", base_period))
  
  # filter relevant periods
  buffer_df <- buffer_df%>%
    # filter((period_D >= min_period_D) & (period_D <= max_period_D))%>%
    filter(period_D  %in% period_D_list)%>%
    # create index to merge back dummy variables later
    mutate(index = row_number())
  
  # create dummy variables from period_D
  Dt_df <- buffer_df%>%
    dcast(index~period_D, fun.aggregate=length)
  
  names(Dt_df) <- c("index",sprintf("Dt%s",sub("-","min",
                                               names(Dt_df)[c(2:length(names(Dt_df)))]
  )
  ))
  # merge main df with dummy variables
  buffer_df <- merge(buffer_df, Dt_df,by=c("index"))
  # drop base period column but keep observations associated to it
  buffer_df <- buffer_df%>%
    select(-all_of(base_period))#%>%
  # drop potentially contaminated rows
  # filter(contaminated_rows==0)
  # filter(potential_contamination==0)
  
  # identify Dt vars
  Dt_vars <- names(buffer_df)[grepl("^Dt.*",names(buffer_df))]
  
  # control variables
  # control_vars <- c("Type_of_Sale","Area_.SQM.","Building_Age","Floor_level","is_ground_floor")
  
  treat_vars <- c("treat")
  
  # model formula
  specified_interaction_vars <- c(treat_vars, 
                                  # Dt_vars, # comment out this if u don't want to include isolated DT vars
                                  sprintf("%s: %s", treat_vars, Dt_vars))
  
  if (include_Dt){
    specified_interaction_vars <- c(specified_interaction_vars,Dt_vars)
  }
  
  model_property_formula <- feols_formula(y_var = y_var,
                                          control_vars=control_vars,
                                          specified_interaction_vars=specified_interaction_vars,
                                          interaction_sep = ":",
                                          fe_vars = fe_vars,
                                          cluster_vars = cluster_vars)
  
  print(model_property_formula)
  
  # fit TWFE model
  model_property <- feols(
    model_property_formula$formula,
    cluster = model_property_formula$cluster,
    data = buffer_df
  )
  
  model_property
}

get_event_study_df1 = function(buffer_df, period_D_list, control_vars, 
                               base_period=-1, include_Dt=TRUE, Dt_to_days=TRUE,include.lowest=TRUE){
  # period_D_list (vector): list of period D list to obtain
  # base_period (int): base period of event study, usually just before the event e.g. -1
  # include_Dt (bool): whether to add Dt variables as covariates
  # Dt_to_days (bool): convert difference between Sale_Date and Flood_Date to days
  # include.lowest (bool): whether to include the lowest value of period_D_list
  
  if (Dt_to_days){
    # calculate days between sale date and flood date
    buffer_df <- buffer_df%>%
      mutate_at(vars(Sale_Date, Flood_Date), as.Date)%>%
      mutate(period_D = as.numeric(difftime(Sale_Date,Flood_Date, units="days")))
  }
  
  # filter relevant periods
  buffer_df <- buffer_df%>%
    mutate(Dt = cut(period_D,breaks=period_D_list,include.lowest=include.lowest))%>%
    drop_na(Dt)%>% # drop rows where Dt has NA
    # mutate_at(vars(Dt), function(x) str_replace_all(str_extract(x, "(?<=\\().+?(?=\\])"),c(","="_","-"="min")))%>%
    mutate_at(vars(Dt), function(x) str_replace_all(str_extract(x, "(?<=[\\(\\[])[^\\]]+(?=\\])"),c(","="_","-"="min")))%>%
    # create index to merge back dummy variables later
    mutate(index = row_number())
  
  # create dummy variables from period_D
  Dt_df <- buffer_df%>%
    dcast(index~Dt, fun.aggregate=length)
  print("Frequency of Dt:")
  print(apply(Dt_df[,c(2:length(names(Dt_df)))],2,sum))
  
  names(Dt_df) <- c("index",sprintf("Dt_%s",names(Dt_df)[c(2:length(names(Dt_df)))]))
  print("Names of Dt:")
  print(names(Dt_df))
  
  # merge main df with dummy variables
  buffer_df <- merge(buffer_df, Dt_df,by=c("index"))
  # drop base period column but keep observations associated to it
  buffer_df <- buffer_df%>%
    select(-all_of(base_period))
  buffer_df
  
  # identify Dt vars
  Dt_vars <- names(buffer_df)[grepl("^Dt_.*",names(buffer_df))]
  
  treat_vars <- c("treat")
  
  # model formula
  specified_interaction_vars <- c(treat_vars,
                                  # Dt_vars, # comment out this if u don't want to include isolated DT vars
                                  sprintf("%s: %s", treat_vars, Dt_vars))
  
  if (include_Dt){
    specified_interaction_vars <- c(specified_interaction_vars,Dt_vars)
  }
  
  model_property_formula <- feols_formula(y_var = y_var,
                                          control_vars=control_vars,
                                          specified_interaction_vars=specified_interaction_vars,
                                          interaction_sep = ":",
                                          fe_vars = fe_vars,
                                          cluster_vars = cluster_vars)
  
  print(model_property_formula)
  
  # fit TWFE model
  model_property <- feols(
    model_property_formula$formula,
    cluster = model_property_formula$cluster,
    data = buffer_df
  )
  
  model_property
}

plot_local_DID_robustness = function(fp = NA, local_DID_df_list = NA, save_fp=NA, 
                                     filter_regex="^treat|^post", 
                                     significance_regex = "\\*+|\\.",
                                     fn_regex=function(x) x){
  # fp (str): filepath to model_results e.g file.path(getwd(),"Exported_Data","flood_buffer_dist","processed_df",
  # filter_regex: to filter covariates of results df
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
    # replace significance
    mutate_at(vars(Significance),function(x){
      x <- str_replace_all(x, c("\\."="p<0.1",
                                "^\\*$"="p<0.05",
                                "^\\*\\*$"="p<0.01",
                                "^\\*\\*\\*$"="p<0.001"
      ))
      factor(x, levels= c("p<0.1","p<0.05","p<0.01","p<0.001"))
    })%>%
    mutate_at(vars(BUFFER_treat,BUFFER_control,Estimate),as.numeric)%>%
    mutate_at(vars(Vars), fn_regex)
  
  
  # plot heatmap, where each subplot represents the POST duration
  plot_local_DID_df_list%>%
    ggplot(aes(x = BUFFER_treat, y = BUFFER_control, col = Estimate, label = Estimate)) +
    # geom_tile() +
    geom_point(aes(size=abs(Estimate), shape=Significance)) +
    facet_wrap(vars(Vars)) + # Facet by the grouping_var
    # scale_fill_gradient2(low="red", mid="white", high="blue",midpoint=0)+
    scale_color_gradient2(low="red", mid="white", high="blue",midpoint=0)+
    labs(x="Treatment buffer radius (m)",y="Control buffer radius (m)",
         color="Signed Estimate", size="Absolute Estimate") +
    # scale_x_continuous(breaks = seq(50, 350, by = 50)) +
    # scale_y_continuous(breaks = seq(40, 500, by = 50))+
    # reduce all point size proportionally
    scale_size(range = c(1, 2))+
    theme_bw()
  
  if (!is.na(save_fp)) {
    # Save the last plot as an SVG
    ggsave(filename = save_fp,width = 7, height = 5, units = "in")
  }
  
  plot_local_DID_df_list%>%
    arrange(Vars)
  
}

plot_event_study_robustness = function(fp = NA, local_DID_df_list = NA, save_fp=NA,
                                       significance_regex = "\\*+|\\.",
                                       period_D_list = seq(-24,12,1)){
  # fp (str): filepath to model_results e.g file.path(getwd(),"Exported_Data","flood_buffer_dist","processed_df",
  # sprintf("model_trimmed12months_localDID_%s_CLUSTER%s_FE%s.csv",y_var,cluster_vars[1],fe_vars[1]))
  # save_fp (str): save plot
  # import csv of model results
  if (!is.na(fp)){
    local_DID_df_list <- read.csv(fp)
  }
  
  # create factors so that facet_wrap will maintain plotting sequence instead of plotting by alphabetical order
  period_D_name_list <- sprintf("Dt%s",sub("-","min",period_D_list))
  period_D_name_list <- c(period_D_name_list, sprintf("treat x %s", period_D_name_list))
  
  # process data to filter the significant DID interaction (Treat x Post) variables
  plot_local_DID_df_list <- local_DID_df_list%>%
    filter(grepl(".*Dt.*", Vars))%>%
    filter(grepl(significance_regex,Significance))%>%
    mutate(BUFFER_treat = str_extract(Robustness_test, "(?<=Treat).+?(?=_)"),
           BUFFER_control = str_extract(Robustness_test, "(?<=Control).+?$")
    )%>%
    # replace significance
    mutate_at(vars(Significance),function(x){
      x <- str_replace_all(x, c("\\."="p<0.1",
                                "^\\*$"="p<0.05",
                                "^\\*\\*$"="p<0.01",
                                "^\\*\\*\\*$"="p<0.001"
      ))
      factor(x, levels= c("p<0.1","p<0.05","p<0.01","p<0.001"))
    })%>%
    mutate_at(vars(BUFFER_treat,BUFFER_control,Estimate),as.numeric)%>%
    mutate(categories = factor(Vars, levels=period_D_name_list))
  
  
  # plot heatmap, where each subplot represents the POST duration
  plot_local_DID_df_list%>%
    ggplot(aes(x = BUFFER_treat, y = BUFFER_control, col = Estimate, label = Estimate)) +
    # geom_tile() +
    geom_point(aes(size=abs(Estimate), shape=Significance)) +
    facet_wrap(~categories) + # Facet by the grouping_var
    # scale_fill_gradient2(low="red", mid="white", high="blue",midpoint=0)+
    scale_color_gradient2(low="red", mid="white", high="blue",midpoint=0)+
    labs(x="Treatment buffer radius (m)",y="Control buffer radius (m)",
         color="Signed Estimate", size="Absolute Estimate") +
    scale_x_continuous(breaks = seq(50, 350, by = 50)) +
    scale_y_continuous(breaks = seq(40, 500, by = 50))+
    # reduce all point size proportionally
    scale_size(range = c(1, 2))+
    theme_bw()
  
  if (!is.na(save_fp)) {
    # Save the last plot as an SVG
    ggsave(filename = save_fp,width = 10, height = 8, units = "in")
  }
  
  plot_local_DID_df_list
  
}

plot_event_study_estimates = function(fp = NA, local_DID_df_list = NA, save_fp=NA,
                                      period_D_list = seq(-24,12,1), base_period=-1){
  # fp (str): filepath to model_results e.g file.path(getwd(),"Exported_Data","flood_buffer_dist","processed_df",
  # sprintf("model_trimmed12months_localDID_%s_CLUSTER%s_FE%s.csv",y_var,cluster_vars[1],fe_vars[1]))
  # save_fp (str): save plot
  # period_D_list: vector of names that correspond to the names of Vars in model_results.Sorted in ascending order
  # import csv of model results
  if (!is.na(fp)){
    local_DID_df_list <- read.csv(fp)
  }
  # create factors so that facet_wrap will maintain plotting sequence instead of plotting by alphabetical order
  period_D_name_list <- sprintf("treat x Dt%s",sub("-","min",period_D_list))
  names(period_D_list) <- period_D_name_list
  
  # process data to filter the significant DID interaction (Treat x Post) variables
  plot_local_DID_df_list <- local_DID_df_list%>%
    filter(grepl("^treat.*Dt.*", Vars))%>%
    # replace significance
    mutate_at(vars(Significance),function(x){
      x <- str_replace_all(x, c("\\."="p<0.1",
                                "^\\*$"="p<0.05",
                                "^\\*\\*$"="p<0.01",
                                "^\\*\\*\\*$"="p<0.001"
      ))
      factor(x, levels= c("p<0.1","p<0.05","p<0.01","p<0.001"))
    })%>%
    mutate_at(vars(Vars), function(x) period_D_list[x])
  
  
  buffer_treat_control <- unique(plot_local_DID_df_list$Robustness_test)
  dropped_Dt <- data.frame(model_results="",Estimate=0,SE=0, Significance=NA,Robustness_test=buffer_treat_control)%>%
    mutate(Vars = base_period)
  # merge df
  plot_local_DID_df_list <- rbind(plot_local_DID_df_list, dropped_Dt)%>%
    # mutate(BUFFER_treat = as.integer(str_extract(Robustness_test, "(?<=Treat).+?(?=_)")),
    #        BUFFER_control = as.integer(str_extract(Robustness_test, "(?<=Control).+?$")))%>%
    arrange(Robustness_test, Vars)
  
  plot_local_DID_df_list%>%
    ggplot(aes(x=Vars, y = Estimate)) +
    geom_line(linetype = "dashed") +
    geom_point(aes(color=Significance)) +
    geom_vline(xintercept=base_period, linetype="dashed", color="blue") + #x-intercept at 0
    geom_hline(yintercept=0, linetype="dashed", color="blue") + #y-intercept at 0
    geom_errorbar(aes(
      ymin = (Estimate-1.96*SE), 
      ymax = (Estimate+1.96*SE),
      color=Significance
    ), width = 0.2) +
    facet_wrap(~Robustness_test) +
    labs(x="Dt")+
    theme_bw()
  
  if (!is.na(save_fp)) {
    # Save the last plot as an SVG
    ggsave(filename = save_fp,width = 10, height = 8, units = "in")
  }
  plot_local_DID_df_list
}

plot_event_study_estimates1 = function(fp = NA, local_DID_df_list = NA, save_fp=NA,
                                      period_D_list = seq(-24,12,1), base_period=-1){
  # fp (str): filepath to model_results e.g file.path(getwd(),"Exported_Data","flood_buffer_dist","processed_df",
  # sprintf("model_trimmed12months_localDID_%s_CLUSTER%s_FE%s.csv",y_var,cluster_vars[1],fe_vars[1]))
  # save_fp (str): save plot
  # period_D_list: vector of names that correspond to the names of Vars in model_results.Sorted in ascending order
  # import csv of model results
  if (!is.na(fp)){
    local_DID_df_list <- read.csv(fp)
  }
 
  
  # process data to filter the significant DID interaction (Treat x Post) variables
  plot_local_DID_df_list <- local_DID_df_list%>%
    filter(grepl("^treat.*Dt.*", Vars))%>%
    # replace significance
    mutate_at(vars(Significance),function(x){
      x <- str_replace_all(x, c("\\."="p<0.1",
                                "^\\*$"="p<0.05",
                                "^\\*\\*$"="p<0.01",
                                "^\\*\\*\\*$"="p<0.001"
      ))
      factor(x, levels= c("p<0.1","p<0.05","p<0.01","p<0.001"))
    })%>%
    mutate_at(vars(Vars), function(x) factor(x, levels=period_D_list))
  # mutate_at(vars(Vars), function(x) period_D_list[x])
  
  
  buffer_treat_control <- unique(plot_local_DID_df_list$Robustness_test)
  dropped_Dt <- data.frame(model_results="",Estimate=0,SE=0, Significance=NA,Robustness_test=buffer_treat_control)%>%
    mutate(Vars = base_period)
  # merge df
  plot_local_DID_df_list <- rbind(plot_local_DID_df_list, dropped_Dt)%>%
    arrange(Robustness_test, Vars)
  
  # x axis labels
  xaxis_labels <- str_replace_all(period_D_list, c("min"="-","treat x Dt_"="","_"=","))
  xaxis_labels <- sprintf(r"((%s])",xaxis_labels)
  names(xaxis_labels) <- period_D_list
  
  plot_local_DID_df_list%>%
    ggplot(aes(x=Vars, y = Estimate, group=1)) +
    geom_line() +
    geom_point(aes(color=Significance)) +
    geom_vline(xintercept=base_period, linetype="dashed", color="blue") + #x-intercept at 0
    geom_hline(yintercept=0, linetype="dashed", color="blue") + #y-intercept at 0
    geom_errorbar(aes(
      ymin = (Estimate-1.96*SE),
      ymax = (Estimate+1.96*SE),
      color=Significance
    ), width = 0.2) +
    facet_wrap(~Robustness_test) +
    labs(x="Dt")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
    scale_x_discrete(labels=xaxis_labels)
  
  if (!is.na(save_fp)) {
    # Save the last plot as an SVG
    ggsave(filename = save_fp,width = 10, height = 8, units = "in")
  }
  plot_local_DID_df_list
}