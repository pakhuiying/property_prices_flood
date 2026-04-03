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
    mutate_at(vars(BUFFER_treat,BUFFER_control,Estimate),as.numeric)%>%
    mutate_at(vars(Vars), fn_regex)
  
  
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