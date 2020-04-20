# The perform_analysis() function filters the data, performs the analysis, creates a plot and an HTML table with the results

perform_analysis <- function(DF_input, group_input, task_input, SD_filter = 3) {
  
  # DEBUG ---------------
    # DF_input = DF
    # group_input = "Design"
    # task_input = "Liking"
    # SD_filter = SD_filter_NUM

  

  # Filtering by RT ---------------------------------------------------------

  filter_MAX = DF_input %>% 
    filter(group == group_input) %>% 
    filter(task == task_input) %>% 
    summarise(MEAN = mean(rt), SD = sd(rt)) %>% 
    mutate(MAX = MEAN + (SD_filter * SD)) %>% 
    pull(MAX)
  
  # Create variables for RT filtering message
  rows_condition_unfiltered = DF_input %>% 
    filter(group == group_input) %>% 
    filter(task == task_input) %>% nrow()
  
  rows_condition_filtered = DF_input %>% 
    filter(group == group_input) %>% 
    filter(task == task_input) %>% 
    filter(rt < filter_MAX) %>% nrow()
  
  message_filtering = paste0("\nFiltered out ", rows_condition_unfiltered - rows_condition_filtered, " out of ", rows_condition_unfiltered, 
                             " (", round((rows_condition_unfiltered - rows_condition_filtered) / rows_condition_unfiltered, 4) * 100, "%)",
                             " responses with RT over mean + ", SD_filter, " SD (", round(filter_MAX, 2), "s)")
  message(message_filtering)
  
  

  # Final dataset for analysis ----------------------------------------------

  DF_analysis = DF_input %>% 
    filter(rt < filter_MAX) %>% 
    filter(group == group_input) %>% 
    filter(task == task_input) %>% 
    mutate(resp = fct_drop(resp))


  
  # CHECKS ------------------------------------------------------------------
  
  num_items_per_participant = DF_analysis %>% count(pp_code) %>% distinct(n)
  num_participants = DF_analysis %>% count(pp_code) %>% nrow(.)
  num_each_item_per_participant = DF_analysis %>% count(pp_code, image_id) %>% distinct(n)
  
  
  if (any(40 != num_items_per_participant)) message("WARNING: #items not 40 for some participants") # This happens with filtering
  if (num_each_item_per_participant != 1) message("ERROR: repeated items per participant")
  
  message(num_participants, " participants completed ", num_items_per_participant, " items each.")

  
  
  # Analysis ----------------------------------------------------------------

  contrasts(DF_analysis$task) = named.contr.sum(levels(DF_analysis$task))

  model <<- glmer(resp ~ coherence + hominess + fascination + (1|ID) + (1|image_id), family = binomial(link = logit), data = DF_analysis,  
        control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e9)))

  
  # Checks

  # performance::check_heteroscedasticity(model)
  performance::check_collinearity(model)
  performance::check_autocorrelation(model)
  

    
  # Plot -------------------------------------------------------------------
  
  plot_output = DF_analysis %>% 
    pivot_longer(coherence:fascination) %>% 
    ggplot(aes(x = resp, y = value, fill = resp, color = resp)) +
    geom_flat_violin(position = position_nudge(x = .25, y = 0), adjust = .75) +
    geom_jitter(width = .15, height = 0, size = 1, alpha = .75) +
    #note that here we need to set the x-variable to a numeric variable and bump it to get the boxplots to line up with the rainclouds.
    geom_boxplot(
      aes(x = as.numeric(resp) + 0.2, y = value),
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
    
  plot_output  = add_annotations(plot_output, model)
    
  suppressMessages(ggsave(filename = paste0("outputs/", group_input, "_", task_input, "_rain.png"), plot = plot_output, width = 16, height = 8, dpi = 300))
    
    
  # Results analysis --------------------------------------------------------
    
  # Must go at the end of the function to save the output
  sjPlot::tab_model(model, show.r2 = TRUE, show.icc = FALSE, show.re.var = FALSE, title = paste(group_input, task_input), 
                    file = paste0("outputs/", group_input, "_", task_input, ".html")) 
    
}
