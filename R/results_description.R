
# Mixed models result report -------------------------------------------------
#' report_result
#'
#' @param model 
#' @param variable_name 
#' @param has_the_word 
#' @param transform exp(beta)? TRUE/FALSE
#' @param df_method c("wald", "ml1") ...
#'
#' @return
#' @export
#'
#' @examples
report_result <- function(model, variable_name = "", has_the_word = "", transform = FALSE, df_method = "ml1") {
  
  # library(parameters)
  # DEBUG
  # model = model_asc_liking
  # variable_name = "coherence"

  
  
  if (exists("variable_name") == FALSE) variable_name = ""
  not_significant = ""
  DF_parameters_temp = parameters::parameters(model, df_method = df_method) %>% as_tibble()
  names_to_round = names(DF_parameters_temp %>% select(-Parameter, -p))
  
  beta_or_OR = "beta"
  
  # If it is a glmer model, exponentiate coefficients, as in sjPlot::tab_model()
  if (grepl("glmer", model@call[1])) {
    if (transform == TRUE) {
      beta_or_OR = "OR"
      DF_parameters_temp = DF_parameters_temp %>% mutate_at(c("Coefficient", "CI_low", "CI_high"), exp)
    }
  }
  
  # Round to 3 decimals
  DF_parameters = DF_parameters_temp %>% mutate_at(vars(all_of(names_to_round)), list(~ round(., 3)))
  
  # Get DF, calculate means per group
  DF_data = model@frame %>% as_tibble()
  names_factors_DF_data = names(which(sapply(DF_data, is.factor))) # Which variables are factors. 
  DV_var = as.character(model@call[[2]][[2]]) #names(model@frame[1])
  
  # If DV_var has label, use it for output
  if (is.null(sapply(model@frame, attr, "label")[DV_var])){
    DV_var_label = DV_var
  } else {
    DV_var_label = sapply(model@frame, attr, "label")[DV_var]
  }
  
  is_interaction = grepl(":", variable_name)
  
  if (variable_name != "") {
    if (is_interaction == TRUE) {
      # IV_var = stringr::word(variable_name, 1)  
      IV_var = gsub("*.(\\(.*))", "", variable_name)
      # IV_var2 = stringr::word(gsub(".*:(.*)", "\\1", variable_name), 1)
      IV_var2 = gsub("*.(\\(.*))", "", gsub(".*:(.*)", "\\1", variable_name))
      
      # CHECK variables are there
      if((IV_var %in% names(DF_data) & IV_var2 %in% names(DF_data)) == FALSE) message("At least one of the variables in 'variable_name' does not exist. Should be one of: \n- ", paste(DF_parameters %>% pull(Parameter), collapse = "\n- "))
      
    } else {
      # IV_var = stringr::word(variable_name, 1)
      IV_var = gsub("*.(\\(.*))", "", variable_name)
      IV_var2 = NULL
      
      # CHECK variables are there
      if(IV_var %in% names(DF_data) == FALSE) message("At least one of the variables in 'variable_name' does not exist. Should be one of: \n- ", paste(DF_parameters %>% pull(Parameter), collapse = "\n- "))
      
    }
    
  } else {
    IV_var = has_the_word
    IV_var2 = NULL
    
  }  
  
  
  
  # Summary DF --------------------------------------------------------------
  
  # * TODO ------------------------------------------------------
  
  # If DV_var is factor but IV_var is continuous, can do M (SD) for each level of DV_var
  # is.factor(DF_data %>% pull(DV_var))[1] | is.character(DF_data %>% pull(DV_var))[1]
  # is.numeric(DF_data %>% pull(IV_var))[1]
  
  # Also, if a variable is char (not factor), should be treated like a categorical???
  # YES, but give a warning
  
  # Make sure DF_counts or DF_means creation and output printing are consistent.
  
  
  # -----------------------------------------------------------
  
  # If DV_var is a factor, COUNT
  if (all_of(DV_var) %in% names_factors_DF_data) {
    
    if (is_interaction == TRUE) {
      
      DF_counts_raw = 
        DF_data %>%
        # count_(c(IV_var, IV_var2, DV_var)) %>% # Deprecated
        count(!!as.symbol(paste0(IV_var, "")), !!sym(paste0(IV_var2, "")), !!sym(paste0(DV_var, ""))) %>% #paste0(x, "") to convert NULL to char
        rename(VAR = 1,
               VAR2 = 2,
               N = n)   
      
      DF_counts = DF_counts_raw %>% 
        left_join(DF_counts_raw %>% group_by(VAR, VAR2) %>% summarise(SUM = sum(N), .groups = "drop"),  by = c("VAR", "VAR2")) %>% 
        mutate(VAR_RAW = VAR) %>%
        unite("VAR",  VAR:all_of(DV_var), remove = FALSE)
      
    } else {
      
      DF_counts_raw = 
        DF_data %>%
        # count_(c(IV_var, IV_var2, DV_var)) %>% # Deprecated
        count(!!as.symbol(paste0(IV_var, "")), !!sym(paste0(IV_var2, "")), !!sym(paste0(DV_var, ""))) %>% #paste0(x, "") to convert NULL to char
        rename(VAR = 1, 
               N = n)
      
      DF_counts = DF_counts_raw %>% 
        left_join(DF_counts_raw %>% group_by(VAR) %>% summarise(SUM = sum(N), .groups = "drop"),  by = "VAR") %>% 
        mutate(VAR_RAW = VAR) %>% 
        unite("VAR",  VAR:all_of(DV_var), remove = FALSE)
    }
    
    
    # If DV_var is NOT a factor, MEAN and SD
  } else {
    
    DF_means = DF_data %>% 
      group_by_at(vars(all_of(IV_var), all_of(IV_var2))) %>% 
      summarise(MEAN =  mean(!!sym(DV_var)),
                SD = sd(!!sym(DV_var)),
                .groups = "drop") %>% 
      ungroup() %>% 
      mutate_if(is.numeric, round, 3) %>% 
      rename(VAR = 1)
    
    if (!is.null(IV_var2)) DF_means = DF_means %>% unite("VAR",  VAR:all_of(IV_var2))
    
  }
  
  
  if(variable_name != "") {
    DF_variable = DF_parameters %>% filter(Parameter == variable_name)  
  } else if (has_the_word != "") {
    DF_variable = DF_parameters %>% filter(grepl(has_the_word, Parameter, ignore.case = TRUE))  
  } else{
    cat(red("REVIEW..."))  
  }
  
  if (DF_variable$p > 0.05) { not_significant = "not " }  
  
  
  # Print output ------------------------------------------------------------
  
  # * TODO: Should put outputs in list? ----------------------
  # cat("\n", silver(model@call[2] %>% as.character()), "\n")
  # cat("", yellow(paste0("The effect of ", underline(DF_variable$Parameter) ," was ", bold(not_significant) ,"significant (beta = ", DF_variable$Coefficient, ", 95% CI [", DF_variable$CI_low, ", ", DF_variable$CI_high, "], p = ", DF_variable$p, ") when predicting ", DV_var, "\n")))
  output_model = paste0("\n", crayon::silver(model@call[2] %>% as.character()), "\n")
  output_result = paste0("", crayon::yellow(paste0("The effect of ", crayon::underline(DF_variable$Parameter) ," was ", crayon::bold(not_significant) ,"significant (", beta_or_OR, " = ", DF_variable$Coefficient, ", 95% CI [", DF_variable$CI_low, ", ", DF_variable$CI_high, "], p = ", DF_variable$p, ") when predicting ", DV_var_label, "\n")))
  output_all = list(output_model, output_result)
  cat("", as.character(output_all))
  
  
  # If IV_var is not a factor
  if (!IV_var %in% names_factors_DF_data) {
    
    # If IV_var and DV_var are continuous
    if (exists("DF_means")) {
      DF_cor = cor(DF_data %>% select(!!DV_var, all_of(IV_var), all_of(IV_var2))) %>% as_tibble(rownames = "names")
      cat("", crayon::green(paste0(" -",  IV_var, " -> ", DV_var, " (r = ", DF_cor %>% filter(names == DV_var) %>% pull(IV_var), ")\n")))
      
      # If IV_var is a continuous variable, but DV_var is categorical
    } else {
      
      cat("", crayon::green(paste0(" -", IV_var, " is continuous variable and ", DV_var, " is categorical\n")))
      
    }
    
    
  } else if (all_of(DV_var) %in% names_factors_DF_data) {
    # If DV_var is a factor, we give counts
    # cat("", green((1:nrow(DF_counts) %>% map_chr( ~ paste0("-",  DF_counts$VAR[.x], " ", IV_var, "/", DV_var, " (", (DF_counts$N[.x]/ DF_counts$SUM[.x])*100, "%, N = ", DF_counts$N[.x], " out of ", DF_counts$SUM[.x],  ")\n")))))
    cat("", crayon::green((1:nrow(DF_counts) %>% map_chr( ~ paste0(" -",  IV_var, " [", DF_counts$VAR_RAW[.x], "]", if (!is.null(IV_var2)) paste0(":", IV_var2, " [", DF_counts %>% pull(VAR2) %>% .[.x], "]"), " -> ", DV_var, " [", DF_counts %>% pull(!!DV_var) %>% .[.x] , "] (", (DF_counts$N[.x] / DF_counts$SUM[.x]) * 100, "%, N = ", DF_counts$N[.x], " out of ", DF_counts$SUM[.x],  ")\n")))))
    
  } else if (IV_var %in% names_factors_DF_data) {
    # If IV_var is a factor, we show M (SD)
    cat("", crayon::green(1:nrow(DF_means) %>% map_chr( ~ paste0(" -",  IV_var, " [", DF_means$VAR[.x], "] (M = ", DF_means$MEAN[.x], ", SD = ", DF_means$SD[.x], ")\n"))))
    
  }  
  
}
