library(dplyr)

# feols_formula
# - To parse formulas, taking into account of control variables, fixed effects

## interaction_combinations

interaction_combinations = function(...){
  # ... accepts an arbitrary number of arguments (both named and unnamed)
  # Returns: find all combinations input vectors
  
  combi <- expand.grid(...)
  
  as.vector(lapply(seq_len(nrow(combi)), function(i) as.character(unlist(combi[i, ]))))
}

# test output
# interaction_combinations(c("within_6_months_post_flood","within_flooding_hotspot"),
#                          c("prone_to_high_tide"),
#                          c("adaption1","adaptation2"))

## formula parser

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
  # ":" includes the interactions ONLY
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
      paste(x,collapse=interaction_sep)
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

# test output
# feols_formula(y_var = "log_price",control_vars=names(transaction_df),
#               interaction_vars =  interaction_combinations(
#                 hazards = c("within_6_months_post_flood","within_flooding_hotspot"),
#                 vulnerability = c("prone_to_high_tide"),
#                 adaptation = c("adaptation1","adaptation2")
#               ),
#               interaction_sep = "*",
#               fe_vars = c("Project_Name","month_year"),
#               cluster_vars = c("SUBZONE_N"),
#               specified_interaction_vars = c("within_6_months_post_flood : closeness_centrality",
#                                              "within_12_months_post_flood : betweeness_centrality")
# )

# feols_formula(y_var = "log_price",control_vars=names(transaction_df),
#               specified_interaction_vars = c("within_6_months_post_flood: closeness_centrality",
#                                               "within_12_months_post_flood: betweeness_centrality"),
#               interaction_sep = "*",
#               fe_vars = c("Project_Name","month_year"),
#               cluster_vars = c("SUBZONE_N")
#               )
