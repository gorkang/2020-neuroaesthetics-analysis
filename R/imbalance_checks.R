

# In ASC and NTD, some images are more likely to be used in Liking and others in Approach
DF_all %>%
  count(group, image_id, task) %>%
  pivot_wider(names_from = c(group, task), values_from = n) %>%
  arrange(NTD_Liking)

# This did not affect the nesting on the enclosure, curvature and height dimensions
DF_all %>%
  filter(group == "NTD") %>% 
  count(group, image_id, task, enclosure, curvature, height) %>%
  group_by(n) %>% 
  count(enclosure, curvature, height, name = "nn")

# Each participant saw exactly 40 images for each task
DF_all %>%
  distinct(participant_id, image_id, task, .keep_all = TRUE) %>% # CAREFUL! HERE WE ELIMINATE DUPLICATES!
  count(participant_id, task)

# Each participant saw 5 images of each type 2*2*2(curvature, height, enclosure) in each task: 5 * 8 = 40 * 2 = 80
DF_all %>% 
  filter(group == "ASC") %>% 
  group_by(participant_id) %>% 
  count(task, curvature, height, enclosure)


DF_all %>%
  filter(group == "ASC") %>% 
  count(group, image_id, task, enclosure, curvature, height) %>%
  group_by(n) %>% 
  count(enclosure, curvature, height, name = "nn")


# Function to test imbalance in the PCA values
test_imbalance <- function(DF_input, group_filter, task_filter) {
  DF_fun = DF_input %>% filter(group == group_filter) %>% group_by(group, image_id, task) %>%
    summarise(n = n(), coherence = mean(coherence), hominess = mean(hominess), fascination = mean(fascination), .groups = "drop")
  
  model1 = lm(coherence ~ n, DF_fun %>% filter(task == task_filter))
  model2 = lm(hominess ~ n, DF_fun %>% filter(task == task_filter))
  model3 = lm(fascination ~ n, DF_fun %>% filter(task == task_filter))
  
  parameters::p_value(model1) %>% mutate(group = group_filter, task = task_filter, dimension = "coherence") %>% 
    bind_rows(parameters::p_value(model2) %>% mutate(group = group_filter, task = task_filter, dimension = "hominess")) %>% 
    bind_rows(parameters::p_value(model3) %>% mutate(group = group_filter, task = task_filter, dimension = "fascination")) %>% 
    filter(Parameter == "n")
  
  # list(coherence = parameters::p_value(model1), hominess = parameters::p_value(model2), fascination = parameters::p_value(model3))
}

# Min p value = .752, mean = .887, median = .94
p_values_imbalance = test_imbalance(DF_all, group_filter = "ASC", task_filter = "Approach") %>% 
  bind_rows(test_imbalance(DF_all, group_filter = "ASC", task_filter = "Liking")) %>% 
  bind_rows(test_imbalance(DF_all, group_filter = "NTD", task_filter = "Approach")) %>% 
  bind_rows(test_imbalance(DF_all, group_filter = "NTD", task_filter = "Liking")) %>% 
  summarise(MEDIAN = median(p),MIN = min(p))

cat(crayon::yellow(paste0("\nImbalance checks: Median ", p_values_imbalance[1], " and min p-value ", p_values_imbalance[2], " for the PCA values for all the dimensions across groups.\n")))
