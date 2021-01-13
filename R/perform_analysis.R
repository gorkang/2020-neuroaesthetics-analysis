# The perform_analysis() function filters the data, performs the analysis, creates a plot and an HTML table with the results

perform_analysis <- function(DF_input, group_input, task_input, SD_filter = 4) {
  
  # DEBUG ---------------
  # DF_input = DF
  # group_input = "Design"
  # task_input = "Liking" 
  # SD_filter = 4

  

  # Filtering by RT ---------------------------------------------------------

  filter_MAX = 
    DF_input %>% 
    filter(group == group_input) %>% 
    filter(task == task_input) %>% 
    summarise(MEAN = mean(rt), SD = sd(rt), min_rt = min(rt), max_rt = max(rt)) %>% 
    mutate(MAX = MEAN + (SD_filter * SD)) %>% 
    pull(MAX)
  
  # filter_MIN = 
    # DF_input %>%
    # filter(group == group_input) %>%
    # filter(task == task_input) %>%
    # summarise(MEAN = mean(rt), SD = sd(rt), min_rt = min(rt), max_rt = max(rt)) %>%
    # mutate(MIN = MEAN - (SD_filter * SD)) %>%
    # pull(MIN)
  # -5.128429
  
  # Create variables for RT filtering message
  rows_condition_unfiltered = DF_input %>% 
    filter(group == group_input) %>% 
    filter(task == task_input) %>% 
    nrow()
  
  rows_condition_filtered = DF_input %>% 
    filter(group == group_input) %>% 
    filter(task == task_input) %>% 
    filter(rt < filter_MAX) %>% 
    # filter(rt > .5) %>%
    
    nrow()
  
  message_filtering = paste0("\nwe filtered out ", rows_condition_unfiltered - rows_condition_filtered, " out of ", rows_condition_unfiltered, 
                             " (", round((rows_condition_unfiltered - rows_condition_filtered) / rows_condition_unfiltered, 4) * 100, "%)",
                             " responses where reaction time was over mean + ", SD_filter, " SD (", round(filter_MAX, 2), "s)")
  message(message_filtering)
  


  # Final dataset for analysis ----------------------------------------------

  DF_analysis = DF_input %>% 
    filter(rt < filter_MAX) %>% 
    
    # filter(rt > .5) %>%
    
    filter(group == group_input) %>% 
    filter(task == task_input) %>% 
    mutate(response = fct_drop(response))

  
  # Reference levels --------------------------------------------------------
  
  if (task_input == "Liking") {
    DF_analysis <- within(DF_analysis, response <- relevel(response, ref = "Dislike"))  
  } else {
    DF_analysis <- within(DF_analysis, response <- relevel(response, ref = "Avoidance"))  
  }


  
  # CHECKS ------------------------------------------------------------------
  
  num_items_per_participant = DF_analysis %>% count(pp_code) %>% distinct(n)
  num_participants = DF_analysis %>% count(pp_code) %>% nrow(.)
  num_each_item_per_participant = DF_analysis %>% count(pp_code, image_id) %>% distinct(n)
  
  
  if (any(40 != num_items_per_participant)) message("WARNING: #items not 40 for some participants") # This happens with filtering
  if (num_each_item_per_participant != 1) message("ERROR: repeated items per participant")
  
  message(num_participants, " participants completed ", num_items_per_participant, " items each.")

  
  
  # Analysis ----------------------------------------------------------------

  # Assign label to response so it shows up in table
  attr(DF_analysis$response, "label") <- task_input
  
  contrasts(DF_analysis$task) = named.contr.sum(levels(DF_analysis$task))

  model <- glmer(response ~ coherence + hominess + fascination +
                    (1|participant_id) + (1|image_id),
                  family = binomial(link = logit), data = DF_analysis,
                  control = glmerControl(optimizer = "nloptwrap")) #, optCtrl = list(maxfun = 2e9)

  # model2 <<- afex::mixed(response ~ coherence + hominess + fascination + (1|participant_id) + (1|image_id),
  #                       family = binomial, data = DF_analysis, method = "LRT")
  # model2$data
  
  model@frame %>% 
    group_by(response) %>% 
    summarise(coh_m = mean(coherence),
              coh_sd = sd(coherence),
              hom_m = mean(hominess),
              hom_sd = sd(hominess),
              fas_m = mean(fascination),
              fas_sd = sd(fascination), .groups = "keep") %>%
    knitr::kable()
  
    
  # Checks
  model@frame %>% 
    select(response, coherence, hominess, fascination) %>% 
    gtsummary::tbl_summary(by = response) %>% 
    gtsummary::add_overall() %>% 
    gtsummary::add_n() %>% 
    gtsummary::as_gt() %>% 
    gt::gtsave(file = paste0("outputs/", group_input, "_", task_input, "_table.png"))
  
    
  
  # performance::check_heteroscedasticity(model)
  performance::check_collinearity(model)
  performance::check_autocorrelation(model)
  

    
  # Plot -------------------------------------------------------------------
  
  plot_output_temp = 
    DF_analysis %>% 
    pivot_longer(coherence:fascination) %>% 
    ggplot(aes(x = response, y = value, fill = response, color = response)) +
    geom_flat_violin(position = position_nudge(x = .25, y = 0), adjust = .75) +
    geom_jitter(width = .15, height = 0, size = 1, alpha = .75) +
    #note that here we need to set the x-variable to a numeric variable and bump it to get the boxplots to line up with the rainclouds.
    geom_boxplot(
      aes(x = as.numeric(response) + 0.2, y = value),
      outlier.shape = NA,
      alpha = 0.3,
      width = .05,
      colour = "BLACK") +
    xlab('') + 
    ylab('PCA') + 
    theme_minimal() +
    guides(fill = FALSE, colour = FALSE) +
    scale_x_discrete(expand = expansion(mult = 0.25)) +
    scale_fill_grey(start = .2, end = .6) +
    scale_color_grey(start = .2, end = .6) +
    theme(legend.position = "none",
          text = element_text(size = 16),
          plot.caption = element_text(size = 10, color = "darkgrey")) + 
    labs(title = paste(group_input, task_input), 
         caption = message_filtering) +
    facet_wrap( ~ name)
    
  plot_output <- add_annotations(plot_output_temp, model)
    
  # suppressMessages(ggsave(filename = paste0("outputs/", group_input, "_", task_input, "_rain.png"), plot = plot_output, width = 16, height = 8, dpi = 300))
    
    
  # Results analysis --------------------------------------------------------
    
  # Must go at the end of the function to save the output

  # With ml1 correction
    # parameters::p_value(model, method = "ml1") #?parameters::ci_ml1
    # Approximation of degrees of freedom based on a "m-l-1" heuristic as suggested by Elff et al. (2019).
    # Elff, M.; Heisig, J.P.; Schaeffer, M.; Shikano, S. (2019). Multilevel Analysis with Few Clusters: Improving Likelihood-based Methods to Provide Unbiased Estimates and Accurate Inference, British Journal of Political Science.
    
    # sjPlot::tab_model(model, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE, p.val = "ml1", title = paste(group_input, task_input))#,
    #                   file = paste0("outputs/", group_input, "_", task_input, ".html")) 
  
  
  # Save table to html
  # sjPlot::tab_model(model, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE, p.val = "ml1", title = paste(group_input, task_input), 
  #                   file = paste0("outputs/", group_input, "_", task_input, ".html")) 

  name_model = paste0("model_", group_input, "_", task_input)
  name_plot = paste0("plot_", group_input, "_", task_input)
  
  assign(name_model, model, envir = .GlobalEnv)
  assign(name_plot, plot_output, envir = .GlobalEnv)
  
  cat(crayon::green("\nModel and plot stored in ", name_model, " and ", name_plot, "\n\n"))
  
}
