##=======================================================================##
#                     Analysis of categorisation data
#
# Publication: The hippocampus supports interpolation during category abstraction
# Nature Communications       
#
# Script Author: Dr. Theo AJ Schaefer                                                
# Contact: theo.schaefer@uni-hamburg.de             
#    
##=======================================================================##


##=======================================================================##
##                                SETUP                                  ##
##=======================================================================##


# Load libraries
library(tidyverse)
library(rstatix)


# Paths
path_root = dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))
path_data = paste0(path_root,'/data/behavior/')  
# Create this data folder in the root directory and copy categorisation data 
if (!dir.exists(path_data)) dir.create(path_data, recursive=T)


#=============================================================================#
#---------------------- Categorisation training performance -------------------
#=============================================================================#


# Load data
df_categorisation_train = read_csv(paste0(path_data,'behavior-categorisation-train_data.csv'))


# First 5 blocks
df_first_blocks <- df_categorisation_train |> 
  group_by(participant) |> 
  mutate(maxblock = max(block, na.rm=T)) |> 
  filter(block <= 5) |> 
  mutate(phase = "First_Blocks")

# Last 5 blocks
df_last_blocks <- df_categorisation_train |>
  group_by(participant) |>
  mutate(maxblock = max(block, na.rm=T)) |>
  filter(block > maxblock - 5) |>
  mutate(phase = "Last_Blocks")

# Combine and aggregate dataset
df_train_agg <- bind_rows(df_first_blocks, df_last_blocks) |>
  group_by(participant, phase) |>
  summarise(
    accuracy = mean(accuracy, na.rm=T),
    error = (1 - accuracy),
    rt = mean(response_time, na.rm=T),
    .groups = "drop"
  )


# 1-sample t-test: last 5 blocks' accuracy against chance level (33 %)
df_train_agg |> filter(phase=='Last_Blocks') |> 
  t_test(accuracy ~ 1, mu=1/3, detailed=T)
# Cohen's D effect size
df_train_agg |> filter(phase=='Last_Blocks') |>
  cohens_d(accuracy ~ 1, mu=1/3)


# 2-sample paired t-test: Error between first and last 5 blocks
df_train_agg |> t_test(error ~ phase, paired=T, detailed=T)  # t-test
df_train_agg |> cohens_d(error ~ phase, paired=T) # effect size


# 2-sample paired t-test: Response time (rt)
df_train_agg |> t_test(rt ~ phase, paired=T, detailed=T)
df_train_agg |> cohens_d(rt ~ phase, paired=T)



#=============================================================================#
#-------------- Categorisation final test (transfer) performance --------------
#=============================================================================#


# Load data
df_categorisation_test = read_csv(paste0(path_data,'behavior-categorisation-transfer_data.csv'))


# Group data to participant level
df_test_agg = df_categorisation_test |> 
  group_by(participant) |> 
  summarise(accuracy = mean(accuracy, na.rm=T))


# 1-sample t-test against chance level (33 %)
df_test_agg |> t_test(accuracy ~ 1, mu=1/3, detailed=T) -> f

# Cohen's D effect size
df_test_agg |> cohens_d(accuracy ~ 1, mu=1/3) 






