##=======================================================================##
#        Analysis of behavioral data of feature inference task
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
library(gghalves)
library(ggbeeswarm)
library(ggforce)
library(ggnewscale)
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
path_data = paste0(path_root,'/data/behavior/')  
# Create this data folder in the root directory
if (!dir.exists(path_data)) dir.create(path_data, recursive=T)
# Then copy the relevant csv files (see below) 

# Plots
path_plots = paste0(path_root,'/plots/behavior/') 
if (!dir.exists(path_plots)) dir.create(path_plots, recursive=T)


# Load behavioral inference data
df_beh_inf = read_csv(paste0(path_data,'behavior-inference_data.csv'))
# Load category structure coordinates for plotting
df_category_structure = read_csv(paste0(path_data,'category-structure_data.csv'))


# #=============================================================================#
# #--------------------------------- Load data ----------------------------------
# #=============================================================================#
# 
# 
# # Behavioral inference response data
# df_beh_inf = read_csv('/data/pt_02352/infpro_fmri/code/Repository/dataset_creation/data_behavior-inference.csv') 
# # df_behavior = read_csv('/data/pt_02352/infpro_fmri/data/derivatives/behavior/sub-all_task-inf_beh-performance.csv')
# # df_behavior = read_csv('/data/pt_02352/infpro_fmri/data/derivatives/behavior/sub-all_task-inf_beh-distances.csv') 
# # Category structure data frame
# df_category_structure = read_csv("/data/pt_02352/infpro_fmri/code/Repository/dataset_creation/data_category-structure.csv")
# 
# # Directory for plots 
# dir_plot = '/data/pt_02352/infpro_fmri/code/Repository/plots/'
# 

#=============================================================================#
#------------------------------ Analyses -------------------------------------
#=============================================================================#


# Aggregate data to participant level
df_beh_inf_agg = df_beh_inf |>  
  group_by(participant, representation) |> 
  summarise(proximity=-mean(distance)) |> 
  ungroup()

# Compare proximity of behavioral completion responses to exemplar vs prototype

# 2-sample paired t-test between representations (exemplar vs prototype):
df_beh_inf_agg |> t_test(proximity ~ representation, paired=T, alternative='greater') 

# Cohen's D effect size
df_beh_inf_agg |> cohens_d(proximity ~ representation, paired=T)  


#=============================================================================#
#------------------------------- Plotting -------------------------------------
#=============================================================================#


#-------------------------------- Figure 2a -----------------------------------#

# Prototype coordinates (for plotting elipse)
pt = list(A= c(4,7), B = c(7,4), C = NA)  # prototype coordinates

# Plot one example participant (dim1: head, dim2: stomach)
fig_2a = df_category_structure |>   
  ggplot(aes(x=dim1, y=dim2, color=unlist(category))) +
  geom_ellipse(aes(x0=pt$A[1], y0=pt$A[2], a=10*(.32), b=10*(.18), angle=pi/4), color=color_categories[1]) +
  geom_ellipse(aes(x0=pt$B[1], y0=pt$B[2], a=10*(.32), b=10*(.18), angle=pi/4), color=color_categories[2]) +
  geom_point(alpha=1, size=3) +  
  geom_point(data=df_beh_inf |> filter(participant==106), 
             aes(cue_val,resp_val), size=3, shape=4, alpha=0.7) +  # shape=4
  labs(x='Cue dimension (head)', y='Response dimension (stomach)')  +
  scale_x_continuous(breaks=seq(0,10,2)) + 
  scale_y_continuous(breaks=seq(0,10,2)) + 
  scale_color_manual(values=color_categories) +
  theme(legend.position='None')
fig_2a

# Save figure
ggsave(paste0(path_plots,'Figure_2a.svg'), width=plot_size, height=plot_size)


#-------------------------------- Figure 2b -----------------------------------#

# Aggregate data to participant level
df_beh_inf_agg = df_beh_inf |>  
  group_by(participant, representation) |> 
  summarise(proximity=-mean(distance)) |> 
  ungroup()

# Plot exemplar and prototype proximity values (individual + average)
# For stats, see L113
fig_2b = ggplot(data=df_beh_inf_agg, 
                aes(x=representation, y=proximity, 
                    fill=representation, color=representation)) +
  geom_half_violin(data=df_beh_inf_agg |> filter(representation=='exemplar'),
                   aes(x=representation, y=proximity,
                       fill=representation, color=representation),
                   position=position_nudge(-0.0), side=c("l"), 
                   color=NA, alpha=.2) +
  geom_half_violin(data=df_beh_inf_agg |> filter(representation=='prototype'),
                   aes(x=representation, y=proximity,
                       fill=representation, color=representation),
                   position=position_nudge(0.0), side=c("r"), 
                   color=NA, alpha=.2) +
  geom_line(aes(group=participant), color="grey", 
            position=position_nudge(c(.15,-.15))) +
  geom_point(position=position_nudge(c(.15,-.15)), size=1, alpha=.5) +
  stat_summary(fun=mean, geom="point", size=1,
               position=position_nudge(c(-0.0,0.0)), color=color_single) +
  stat_summary(fun.data=mean_se, geom="errorbar",
               position=position_nudge(c(-0.0,0.0)), color=color_single,
               width=0, size=1.5) +
  stat_summary(fun=mean, geom="line", aes(group=1),
               position=position_nudge(c(-0.0,0.0)), color=color_single, size=1) +
  labs(x='Representation', y='Mean 1D proximity to responses') +
  scale_x_discrete(labels=c('Exemplar', 'Prototype')) +
  lims(y=c(-2., -0.4)) +
  theme(legend.position="none")
fig_2b

# Save plot
ggsave(paste0(path_plots, 'Figure_2b.svg'), width=plot_size, height=plot_size)




