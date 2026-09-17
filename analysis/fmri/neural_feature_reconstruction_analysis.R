##=======================================================================##
#       Analysis of fMRI feature decoding during feature viewing task
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
library(RColorBrewer)
library(svglite)

# Plot Settings
plot_size = 2.5
color_single = '#009999'
color_representation = c('#4d0026','#4d2700')  # exemplar, prototype
color_categories = c('#C88D0D','#6A0DAD','#808080')  # A, B, C

# Set individual ggplot theme
theme_individual <- function() {
  theme_classic() %+replace%
    theme(
      panel.background = element_rect(color=NA, fill="white"),
      axis.line = element_line(color="black"),
      axis.ticks = element_line(color="black"),
      axis.text = element_text(color="black", size=8),
      axis.title = element_text(color = "black", size=10),
      plot.title = element_text(size=8, face='bold', hjust=0),
      text = element_text(family="Arial")
    )
}
theme_set(theme_individual())

# Set default discrete color and fill scale
scale_colour_discrete <- function(...) {
  scale_colour_manual(..., values=color_representation)
}
scale_fill_discrete <- function(...) {
  scale_fill_manual(..., values=color_representation) 
}


#=============================================================================#
#------------------------------ Paths and data --------------------------------
#=============================================================================#

# Root
path_root = dirname(dirname(dirname(rstudioapi::getSourceEditorContext()$path)))

# Data
path_data = paste0(path_root,'/data/fmri/decoding_analysis/')  
# Create this data folder in the root directory
if (!dir.exists(path_data)) dir.create(path_data, recursive=T)
# Then copy the relevant csv files (see below) 


# Load decoding results data
df_decodingCV = read_csv(paste0(path_data,'neural-feature-reconstruction_data.csv'))


#=============================================================================#
#------------------------------ Analyses -------------------------------------
#=============================================================================#

# Minor value variations in decimal places vs. publication due to different 
# non-seeded permutation.


#--- Mean absolute error (MAE) between true and decoded values

#------ Head Dimension

# Compute permutation-based chance level (cl) for MAE
mae_cl = df_decodingCV |> 
  filter(dimension=='head') |> 
  pull(mae_perm) |> 
  mean() 
# 1-sample t-test against chance level
df_decodingCV |>  
  filter(dimension=='head') |>
  t_test(mae ~ 1, mu=mae_cl, detailed=T)
# Cohen's D effect size
df_decodingCV |>  
  filter(dimension=='head') |>
  cohens_d(mae ~ 1, mu=mae_cl)

#------ Stomach Dimension

# Compute permutation-based chance level (cl) for MAE
mae_cl = df_decodingCV |> 
  filter(dimension=='stomach') |> 
  pull(mae_perm) |> 
  mean() 
# 1-sample t-test against chance level
df_decodingCV |>  
  filter(dimension=='stomach') |>
  t_test(mae ~ 1, mu=mae_cl, detailed=T)
# Cohen's D effect size
df_decodingCV |>  
  filter(dimension=='stomach') |>
  cohens_d(mae ~ 1, mu=mae_cl)

#------ Dimensional differences (head vs stomach)

# 2-sample paired t-test between head and stomach dimension
df_decodingCV |> t_test(mae ~ dimension, paired=F, detailed=T)
# Cohen's D effect size
df_decodingCV |> cohens_d(mae ~ dimension, paired=F)



#--- Pearson correlation between true and decoded values


#------ Head Dimension

# Compute permutation-based chance level (cl) for Pearson R
r_cl = df_decodingCV |> 
  filter(dimension=='head') |> 
  pull(r_perm) |> 
  mean() 
# 1-sample t-test against chance level
df_decodingCV |> 
  filter(dimension=='head') |> 
  t_test(r ~ 1, mu=r_cl, detailed=T)
# Cohen's D effect size
df_decodingCV |> 
  filter(dimension=='head') |> 
  cohens_d(r ~ 1, mu=r_cl)

#------ Stomach Dimension

# Compute permutation-based chance level (cl) for Pearson R
r_cl = df_decodingCV |> 
  filter(dimension=='stomach') |> 
  pull(r_perm) |> 
  mean() 
# 1-sample t-test against chance level
df_decodingCV |> 
  filter(dimension=='stomach') |> 
  t_test(r ~ 1, mu=r_cl, detailed=T)
# Cohen's D effect size
df_decodingCV |> 
  filter(dimension=='stomach') |> 
  cohens_d(r ~ 1, mu=r_cl)

#------ Dimensional differences (head vs stomach)

# 2-sample paired t-test between head and stomach dimension
df_decodingCV |> t_test(r ~ dimension, paired=F, detailed=T)
# Cohen's D effect size
df_decodingCV |> cohens_d(r ~ dimension, paired=F)











