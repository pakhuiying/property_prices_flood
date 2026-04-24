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
    scale_x_continuous(breaks = seq(50, 350, by = 50)) +
    scale_y_continuous(breaks = seq(40, 500, by = 50))+
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
    scale_size(range = c(0, 2))+
    theme_bw()
  
  if (!is.na(save_fp)) {
    # Save the last plot as an SVG
    ggsave(filename = save_fp,width = 10, height = 8, units = "in")
  }
  
  plot_local_DID_df_list
  
}

plot_event_study_estimates = function(fp = NA, local_DID_df_list = NA, save_fp=NA,
                                      period_D_list = seq(-24,12,1)){
  # fp (str): filepath to model_results e.g file.path(getwd(),"Exported_Data","flood_buffer_dist","processed_df",
  # sprintf("model_trimmed12months_localDID_%s_CLUSTER%s_FE%s.csv",y_var,cluster_vars[1],fe_vars[1]))
  # save_fp (str): save plot
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
    mutate(Vars = -1)
  # merge df
  plot_local_DID_df_list <- rbind(plot_local_DID_df_list, dropped_Dt)%>%
    # mutate(BUFFER_treat = as.integer(str_extract(Robustness_test, "(?<=Treat).+?(?=_)")),
    #        BUFFER_control = as.integer(str_extract(Robustness_test, "(?<=Control).+?$")))%>%
    arrange(Robustness_test, Vars)
  
  plot_local_DID_df_list%>%
    ggplot(aes(x=Vars, y = Estimate)) +
    geom_line(linetype = "dashed") +
    geom_point(aes(color=Significance)) +
    geom_vline(xintercept=-1, linetype="dashed", color="blue") + #x-intercept at 0
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