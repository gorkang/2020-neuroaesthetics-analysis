
# REMEMBER ----------------------------------------------------------------

# Be careful with pp_numb and pp_code as they repeat in different groups
# We create ID = pp_code_group

# We rename a couple misslabeled responses # Disilike Like' 
# We filter by RT see SD_filter_NUM parameter



# Libraries ---------------------------------------------------------------

options(mc.cores = parallel::detectCores() - 2)

library(dplyr)
library(forcats)
library(janitor)
library(lme4)
library(patchwork)
library(purrr)
library(tidyr)
library(readxl)
library(webshot)
library(gtsummary)

# The perform_analysis() function filters the data, performs the analysis, creates a plot and an HTML table with the results
source("R/perform_analysis.R")

# Includes rainclod helper fn, add annotations...
source("R/helper_functions.R")
source("R/results_description.R")


# Avoid scientific notation
options(scipen = 999)


# Read data ---------------------------------------------------------------

DF1_Like = readxl::read_excel("data/Study 1b_ASC and NTD_DATA Curvature Interior.xlsx", sheet = 1, guess_max = 5000) %>% janitor::clean_names() %>% 
  select(-x12) %>% mutate(pp_code = as.character(pp_code))
DF1_Approach = readxl::read_excel("data/Study 1b_ASC and NTD_DATA Curvature Interior.xlsx", sheet = 2, guess_max = 5000) %>% janitor::clean_names() %>% 
  mutate(pp_code = as.character(pp_code))

DF2_Like = readxl::read_excel("data/Study 2b_Designers_DATA Curvature_Interior.xlsx", sheet = 1, guess_max = 5000) %>% janitor::clean_names() %>% 
  mutate(pp_code = as.character(pp_code))
DF2_Approach = readxl::read_excel("data/Study 2b_Designers_DATA Curvature_Interior.xlsx", sheet = 2, guess_max = 5000) %>% janitor::clean_names() %>% 
  mutate(pp_code = as.character(pp_code))

# Combine data
DF_all_raw = DF1_Like %>% 
  bind_rows(DF1_Approach) %>% 
  bind_rows(DF2_Like) %>% 
  bind_rows(DF2_Approach) %>% 
  
  # Create unique participant ID
  mutate(ID = paste0(pp_code, "_", group),
         image_id = gsub("\\.jpg", "", image_id)) %>% 
  
  # Rename misslabeled responses and Design group
  mutate(resp = 
           case_when(
             resp == "Disike" ~ "Dislike",
             resp == "Like'" ~ "Like",
             TRUE ~ resp),
         group = 
           case_when(
             group %in% c("Master", "Triennial") ~ "Design",
             TRUE ~ group)) %>% 
  
  # Create task variable
  mutate(task = 
           as.factor(
           case_when(
             resp %in% c("Like", "Dislike") ~ "Liking",
             resp %in% c("Approach", "Avoidance") ~ "Approach",
             TRUE ~ NA_character_
           ))) %>% 
  rename(response = resp) %>% 
  
  # Remove unused columns
  select(-ceiling, -figure, -space, -pp_numb)
  

  

# Images PCA

# Alex 2020 
DF_PAC = readxl::read_excel("data/Psychology of Architecture PC Scores_Excel_AC_4.18.2020.xlsx") %>% 
  drop_na(filename) %>% 
  rename(image_id = filename) %>% 
  janitor::clean_names()


# Combine participants data with images data -----------------------------

DF_all = DF_all_raw %>% 
  left_join(DF_PAC, by = "image_id") %>% 
  rename(coherence = pc1_order,
         hominess = pc2_coziness, 
         fascination = pc3_arousal,
         response = response,
         participant_id = ID)


# CHECKS ------------------------------------------------------------------

CHECK_images_PCAs = DF_PAC %>% count(image_id) %>% distinct(n)
if (CHECK_images_PCAs %>% nrow() != 1 | CHECK_images_PCAs$n != 1) stop("At least some images have more than 1 entry")

CHECK_items_per_participant = DF_all %>% count(participant_id) %>% distinct(n)
if (CHECK_items_per_participant %>% nrow() != 1 | CHECK_items_per_participant$n != 80) stop("At least some participants don't have 80 items!")

CHECK_items_per_participant_and_task = DF_all %>% count(participant_id, task) %>% distinct(task, n)
if (CHECK_items_per_participant_and_task %>% nrow() != 2 | all(CHECK_items_per_participant_and_task$n != c(40, 40))) stop("At least some participants don't have 40 items per task")

CHECK_repetitions_per_item = DF_all %>% group_by(image_id) %>% count() %>% ungroup() %>% distinct(n)
if (CHECK_repetitions_per_item %>% nrow() != 1 | CHECK_repetitions_per_item$n != 60) stop("At least some items appear more than once per participant")



# Imbalance  --------------------------------------------------------------

source("R/imbalance_checks.R")


# Analyses ----------------------------------------------------------------

# RT Filtering criteria (mun of SD + mean)

SD_filter_NUM = 4

DF = DF_all %>% 
    mutate(response = as.factor(response), 
           image_id = as.factor(image_id))



## Autism spectrum participants --------------------------------------------

perform_analysis(DF_input = DF,
                 group_input = "ASC",
                 task_input = "Liking", 
                 SD_filter = SD_filter_NUM)

performance::check_collinearity(model_ASC_Liking)

summary(report::report(model_ASC_Liking))
report_result(model = model_ASC_Liking, variable_name = "coherence", df_method = "ml1")
report_result(model = model_ASC_Liking, variable_name = "hominess", df_method = "ml1")
report_result(model = model_ASC_Liking, variable_name = "fascination", df_method = "ml1")


perform_analysis(DF_input = DF,
                 group_input = "ASC",
                 task_input = "Approach", 
                 SD_filter = SD_filter_NUM)

performance::check_collinearity(model_ASC_Approach)

summary(report::report(model_ASC_Approach))
report_result(model = model_ASC_Approach, variable_name = "coherence", df_method = "ml1")
report_result(model = model_ASC_Approach, variable_name = "hominess", df_method = "ml1")
report_result(model = model_ASC_Approach, variable_name = "fascination", df_method = "ml1")




## Neurotypical controls ---------------------------------------------------


perform_analysis(DF_input = DF,
                 group_input = "NTD",
                 task_input = "Liking", 
                 SD_filter = SD_filter_NUM)

performance::check_collinearity(model_NTD_Liking)

summary(report::report(model_NTD_Liking))
report_result(model = model_NTD_Liking, variable_name = "coherence", df_method = "ml1")
report_result(model = model_NTD_Liking, variable_name = "hominess", df_method = "ml1")
report_result(model = model_NTD_Liking, variable_name = "fascination", df_method = "ml1")


perform_analysis(DF_input = DF,
                 group_input = "NTD",
                 task_input = "Approach", 
                 SD_filter = SD_filter_NUM)

performance::check_collinearity(model_NTD_Approach)

summary(report::report(model_NTD_Approach))
report_result(model = model_NTD_Approach, variable_name = "coherence", df_method = "ml1")
report_result(model = model_NTD_Approach, variable_name = "hominess", df_method = "ml1")
report_result(model = model_NTD_Approach, variable_name = "fascination", df_method = "ml1")


## Design quasi-experts ----------------------------------------------------

perform_analysis(DF_input = DF,
                 group_input = "Design",
                 task_input = "Liking", 
                 SD_filter = SD_filter_NUM)

performance::check_collinearity(model_Design_Liking)


summary(report::report(model_Design_Liking))
report_result(model = model_Design_Liking, variable_name = "coherence", df_method = "ml1")
report_result(model = model_Design_Liking, variable_name = "hominess", df_method = "ml1")
report_result(model = model_Design_Liking, variable_name = "fascination", df_method = "ml1")



perform_analysis(DF_input = DF,
                 group_input = "Design",
                 task_input = "Approach", 
                 SD_filter = SD_filter_NUM)

performance::check_collinearity(model_Design_Approach)

summary(report::report(model_Design_Approach))
report_result(model = model_Design_Approach, variable_name = "coherence", df_method = "ml1")
report_result(model = model_Design_Approach, variable_name = "hominess", df_method = "ml1")
report_result(model = model_Design_Approach, variable_name = "fascination", df_method = "ml1")






# Final tables ------------------------------------------------------------

sjPlot::tab_model(model_NTD_Liking, model_NTD_Approach, title = paste("NTD"), p.val = "ml1", file = paste0("outputs/ALL_NTD.html"),
                  # show.std = TRUE, # std Beta
                  transform = NULL,
                  show.est = TRUE, # OR
                  show.ci = 0.95,
                  col.order = c("est", "se", "ci", "std.est", "std.se", "ci", "std.ci",  "p"),
                  string.est = "beta (95% CI)",
                  string.std = "std.B", 
                  string.std_ci = "std.CI",
                  collapse.ci = TRUE,
                  linebreak = FALSE,
                  digits.p = 5,
                  show.p = TRUE,
                  show.r2 = TRUE, 
                  show.icc = FALSE, 
                  show.re.var = FALSE)

sjPlot::tab_model(model_Design_Liking, model_Design_Approach, p.val = "ml1", file = paste0("outputs/ALL_Design.html"),
                  # show.std = TRUE, # std Beta
                  transform = NULL,
                  show.est = TRUE, # OR
                  show.ci = 0.95,
                  col.order = c("est", "se", "ci", "std.est", "std.se", "ci", "std.ci",  "p"),
                  string.est = "beta (95% CI)",
                  string.std = "std.B", 
                  string.std_ci = "std.CI",
                  collapse.ci = TRUE,
                  linebreak = FALSE,
                  digits.p = 5,
                  show.p = TRUE,
                  show.r2 = TRUE, 
                  show.icc = FALSE, 
                  show.re.var = FALSE)

sjPlot::tab_model(model_ASC_Liking, model_ASC_Approach, p.val = "ml1", file = paste0("outputs/ALL_ASC.html"),
                  # show.std = TRUE, # std Beta
                  transform = NULL,
                  show.est = TRUE, # OR
                  show.ci = 0.95,
                  col.order = c("est", "se", "ci", "std.est", "std.se", "ci", "std.ci",  "p"),
                  string.est = "beta (95% CI)",
                  string.std = "std.B", 
                  string.std_ci = "std.CI",
                  collapse.ci = TRUE,
                  linebreak = FALSE,
                  digits.p = 5,
                  show.p = TRUE,
                  show.r2 = TRUE, 
                  show.icc = FALSE, 
                  show.re.var = FALSE)



# Final plots -------------------------------------------------------------

plot_ASC = plot_ASC_Liking + plot_ASC_Approach
suppressMessages(ggsave(filename = paste0("outputs/", "ALL_ASC_rain.png"), plot = plot_ASC, width = 18, height = 8, dpi = 300))


plot_NTD = plot_NTD_Liking + plot_NTD_Approach
suppressMessages(ggsave(filename = paste0("outputs/", "ALL_NTD_rain.png"), plot = plot_NTD, width = 18, height = 8, dpi = 300))

plot_Design = plot_Design_Liking + plot_Design_Approach
suppressMessages(ggsave(filename = paste0("outputs/", "ALL_Design_rain.png"), plot = plot_Design, width = 18, height = 8, dpi = 300))
