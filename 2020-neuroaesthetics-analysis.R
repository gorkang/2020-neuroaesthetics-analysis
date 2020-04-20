
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
library(purrr)
library(tidyr)
library(readxl)

# The perform_analysis() function filters the data, performs the analysis, creates a plot and an HTML table with the results
source("R/perform_analysis.R")

# Includes rainclod helper fn, add annotations...
source("R/helper_functions.R")


# Avoid scientific notation
options(scipen=999)


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
         fascination = pc3_arousal)


# CHECKS ------------------------------------------------------------------

CHECK_images_PCAs = DF_PAC %>% count(image_id) %>% distinct(n)
if (CHECK_images_PCAs %>% nrow() != 1 | CHECK_images_PCAs$n != 1) stop("At least some images have more than 1 entry")

CHECK_items_per_participant = DF_all %>% count(ID) %>% distinct(n)
if (CHECK_items_per_participant %>% nrow() != 1 | CHECK_items_per_participant$n != 80) stop("At least some participants don't have 80 items!")

CHECK_items_per_participant_and_task = DF_all %>% count(ID, task) %>% distinct(task, n)
if (CHECK_items_per_participant_and_task %>% nrow() != 2 | all(CHECK_items_per_participant_and_task$n != c(40, 40))) stop("At least some participants don't have 40 items per task")

CHECK_repetitions_per_item = DF_all %>% group_by(image_id) %>% count() %>% ungroup() %>% distinct(n)
if (CHECK_repetitions_per_item %>% nrow() != 1 | CHECK_repetitions_per_item$n != 60) stop("At least some items appear more than once per participant")

# # In ASC and NTD, some images are more likely to be used in Liking and others in Approach (it seems systematic)
# DF_all %>% 
#   group_by(group, image_id, task) %>% count() %>% 
#   pivot_wider(names_from = c(group, task), values_from = n) %>% 
#   arrange(NTD_Liking) 
# 
# # Although each participant saw exactly 40 images for each task
# DF_all %>% 
#   distinct(ID, image_id, task, .keep_all = TRUE) %>% # CAREFUL! HERE WE ELIMINATE DUPLICATES!
#   group_by(ID, task) %>% count() %>% ungroup()
# 
# DF_all %>% 
#   count(ID, image_id, task) 
# 
# DF_all %>% group_by(image_id, task) %>% count() 




# Analyses ----------------------------------------------------------------

# RT Filtering criteria (mun of SD + mean)

SD_filter_NUM = 4

DF = DF_all %>% 
    mutate(resp = as.factor(resp), 
           image_id = as.factor(image_id)) 


perform_analysis(DF_input = DF,
                 group_input = "Design",
                 task_input = "Liking", 
                 SD_filter = SD_filter_NUM)

perform_analysis(DF_input = DF,
                 group_input = "Design",
                 task_input = "Approach", 
                 SD_filter = SD_filter_NUM)

perform_analysis(DF_input = DF,
                 group_input = "ASC",
                 task_input = "Liking", 
                 SD_filter = SD_filter_NUM)

perform_analysis(DF_input = DF,
                 group_input = "ASC",
                 task_input = "Approach", 
                 SD_filter = SD_filter_NUM)

perform_analysis(DF_input = DF,
                 group_input = "NTD",
                 task_input = "Liking", 
                 SD_filter = SD_filter_NUM)

perform_analysis(DF_input = DF,
                 group_input = "NTD",
                 task_input = "Approach", 
                 SD_filter = SD_filter_NUM)
