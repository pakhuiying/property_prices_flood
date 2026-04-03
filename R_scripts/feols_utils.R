interaction_combinations = function(...){
  # ... accepts an arbitrary number of arguments (both named and unnamed)
  # Returns: find all combinations input vectors
  
  combi <- expand.grid(...)
  
  as.vector(lapply(seq_len(nrow(combi)), function(i) as.character(unlist(combi[i, ]))))
}

feols_formula = function(y_var, control_vars, fe_vars,cluster_vars,
                         interaction_vars=NA, 
                         specified_interaction_vars=NA,
                         interaction_sep="*"){
  # y_var (chr): y variable
  # control_vars (vector): control variables
  # interaction_vars (vector of vector): interaction variables
  # fe_vars (vector): fixed effect variables
  # specified_interaction_vars (vector of str): vector of specified interaction e.g. c(a*b, a:c, b:c)
  # interaction_sep (chr): separator e.g. "*" or ":"
  # "*" includes the main effects and the interaction
  # ":" includes the interactions ONLY,  If var is a factor, it creates interaction dummies for all levels
  # "I(x1*x2)": i() is a specialized function for interacting continuous variables with factors, optimized for event studies
  control_vars_OG <- control_vars
  
  if (!any(is.na(interaction_vars))){
    # flatten interaction list
    interaction_list <- unlist(interaction_vars)
    # ensure that same variables do not appear in control vars, otherwise the main effects are included
    control_vars <- setdiff(control_vars_OG, interaction_list)
  }
  
  if (!any(is.na(specified_interaction_vars))){
    # if specified_interaction_vars is not NA, overwrite the previous interaction vars
    specified_interaction_list <- sapply(specified_interaction_vars, function(x) {
      strsplit(x,"[:* ]+") # split string by delimiter e.g. :, and *
    })
    specified_interaction_list <- unlist(specified_interaction_list)
    # ensure that same variables do not appear in control vars, otherwise the main effects are included
    control_vars <- setdiff(control_vars, specified_interaction_list)
    # print(control_vars)
  }
  
  control_vars <- setdiff(control_vars, fe_vars)
  control_vars <- setdiff(control_vars,c(y_var))
  control_vars <- setdiff(control_vars, cluster_vars)
  
  # collapse interaction vars
  if (!any(is.na(interaction_vars))){
    # combine interaction terms
    itn_terms <- sapply(interaction_vars, function(x) {
      if (interaction_sep == "I"){
        sprintf("I(%s)",paste(x,collapse="*"))
      } else {
        paste(x,collapse=interaction_sep)
      }
    })
    # collapse interaction terms using +
    itn_terms <- paste(itn_terms, collapse=" + ")
  } 
  
  # collapse specified interaction vars
  if (!any(is.na(specified_interaction_vars))){
    # collapse interaction terms using +
    specified_itn_terms <- paste(specified_interaction_vars, collapse=" + ")
    
    # combine with the prev interaction vars if its not NA
    if (!any(is.na(interaction_vars))) {
      itn_terms <- paste(c(itn_terms, specified_itn_terms), collapse=" + ")
    } else { # if prev interaction vars is NA, then override
      itn_terms <- specified_itn_terms
    }
    
  }
  
  # collapse control vars
  control_terms <-paste(control_vars, collapse = " + ")
  
  if (!any(is.na(interaction_vars)) | !any(is.na(specified_interaction_vars))){
    # combine control and interaction terms
    control_terms <- paste(control_terms, itn_terms, sep=" + ")
  }
  
  # collapse fe terms
  fe_terms <- paste(fe_vars, collapse = " + ")
  
  # combine all terms
  formula <- as.formula(paste(y_var,"~", control_terms, "|", fe_terms))
  
  cluster_vars <- as.formula(paste("~",paste(cluster_vars, collapse = " + ")))
  
  # return as a vector
  list("formula"=formula,"cluster"=cluster_vars)
}

extract_model_results = function(model_results){
  # model_results: output from unclass(etable(model_property))
  # outputs: list of data
  SE <- str_extract(model_results, "(?<=\\().+?(?=\\))")
  
  significance <- str_extract_all(str_extract(model_results, 
                                              "(?<=\\d).+?(?=\\()"), "[.*]")
  
  significance <- sapply(significance, function(x){
    concat_signif <- paste(x,collapse="")
    # only remove the first instance of \\., because this comes from decimal places
    gsub("^\\.","",concat_signif)
  })
  
  estimate <- str_extract(model_results, ".+?(?=\\()")
  estimate <- gsub("(\\d)\\D+$", "\\1", estimate)
  
  list(Estimate=estimate, SE=SE, Significance=significance)
  
}

get_model_results = function(model_property, model_name){
  # model_property: output from feols
  model_property_df <- unclass(etable(model_property))
  # consolidate model results
  df <- data.frame(Vars = model_property_df[[1]], model_results = model_property_df$model_property,
                   extract_model_results(model_property_df$model_property))
  # add additional rows for edj R2
  df[nrow(df)+1,] <- c("Adjusted R2",fitstat(model_property, "ar2")[[1]], rep(NA, times=ncol(df)-2))
  # add column to identify which robustness test we are looking at - good for long format
  df$Robustness_test <- model_name
  df
}
