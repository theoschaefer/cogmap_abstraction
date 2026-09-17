##=======================================================================##
#               Analysis of fMRI decoding during feature inference task
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
path_data = paste0(path_root,'/data/fmri/decoding_analysis/')  
# Create this data folder in the root directory
if (!dir.exists(path_data)) dir.create(path_data, recursive=T)
# Then copy the relevant csv files (see below) 

# Plots
path_plots = paste0(path_root,'/plots/fmri/decoding_analysis/') 
if (!dir.exists(path_plots)) dir.create(path_plots, recursive=T)


# Load decoding results data
df_decoding = read_csv(paste0(path_data,'neural-feature-inference_data.csv'))


#=============================================================================#
#---------------------------- Analyses & Plots --------------------------------
#=============================================================================#

# Minor value variations in decimal places vs. publication due to different 
# non-seeded permutation.


#-------------------------- Neural prototype bias ----------------------------#


# Proximity of Prototype to decoded values (z-scored against permutation distribution) 
# 1-sample t-test against 0
df_decoding |> t_test(proximity_prototype ~ 0, alternative='greater')
# Cohen's D effect size
df_decoding |> cohens_d(proximity_prototype ~ 0)


# Proximity of Exemplar to decoded values (z-scored against permutation distribution) 
# 1-sample t-test against 0
df_decoding |> t_test(proximity_exemplar ~ 0, alternative='greater')
# Cohen's D effect size
df_decoding |> cohens_d(proximity_exemplar ~ 0)


# Prototype vs exemplar proximity. First convert to long format:
df_decoding_long = df_decoding |> 
  mutate(neural_pt_bias = proximity_prototype - proximity_exemplar) |>
  pivot_longer(cols=c('proximity_exemplar','proximity_prototype'),
               names_to='representation',
               values_to='proximity',
               names_pattern = 'proximity_(.*)')
# 2-sample paired t-test
df_decoding_long |> t_test(proximity ~ representation, paired=T, alternative='less')
# Cohen's D effect size
df_decoding_long |> cohens_d(proximity ~ representation, paired=T)



#--- Create Figure 3B (for stats, see L112)

fig_3b <- ggplot(data=df_decoding_long, 
                 aes(x=representation, y=proximity, 
                     fill=representation, color=representation)) +
  geom_half_violin(data=df_decoding_long |> filter(representation=='exemplar'),
                   aes(x=representation, y=proximity,
                       fill=representation, color=representation),
                   position=position_nudge(-0.0), side=c("l"), 
                   color=NA, alpha=.2) +
  geom_half_violin(data=df_decoding_long |> filter(representation=='prototype'),
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
  labs(x='Representation', y='Proximity to decoded responses') +
  scale_x_discrete(labels=c('Exemplar', 'Prototype')) +
  theme(legend.position="none")
fig_3b

# Save plot
ggsave(paste0(path_plots,'Figure_3b.svg'), width=plot_size, height=plot_size)



#----------------- Correlation with behavioral bias -------------------------#


# Spearman Rho between neural prototype bias and behavioral prototype bias
df_decoding |> cor_test(neural_prototype_bias, 
                        behavioral_prototype_bias,
                        method='spearman', 
                        alternative='greater')


# Create Figure 3c

# Function to get correlation value and p-value for plot
get_cor_output = function(x, y, measure) {
  # Correlation test
  cor_test <- cor.test(x, y, method=measure)
  
  # Correlation value and p-value
  corr_value <- round(cor_test$estimate, 2)
  p_value <- cor_test$p.value / 2  # one-sided
  
  # Text for p-value stars
  if (p_value < .001) {
    p_stars <- "***"
  } else if (p_value < .01) {
    p_stars <- "**"
  } else if (p_value < .05) {
    p_stars <- "*"
  } else if (p_value > .05) {
    p_stars <- "ns."
  }
  
  # Text for plot
  corr_value_sub <- substr(format(corr_value), start=2, stop=5)
  corr_text <- paste0("italic(r) == '", corr_value_sub, "' ~ '", p_stars, "'")
  
  return(corr_text)
}

# Get correlation and p-value as text
cor_text = get_cor_output(df_decoding$neural_prototype_bias, 
                          df_decoding$behavioral_prototype_bias, 
                          measure='spearman')

# Plot correlation between neural prototype bias and behavioral prototype bias
fig_3c = df_decoding |>
  ggplot(aes(neural_prototype_bias, behavioral_prototype_bias)) +
  geom_smooth(method='lm', color='darkgray', se=F, size=1) +
  geom_point(color=color_single, alpha=0.8) +
  annotate("text", x=-.2, y=.7, label=cor_text, parse=T, family=theme_get()$text[["family"]]) +
  labs(x='Prototype vs exemplar proximity', y='Behavioral proximity effect') +
  lims(x=c(-.5, 1))
fig_3c

# Save plot
ggsave(paste0(path_plots,'Figure_3c.svg'), width=plot_size, height=plot_size)



#----------------- Correlation with hippocampal amplitude -------------------------#


# Spearman Rho between neural prototype bias and behavioral prototype bias
df_decoding |> cor_test(neural_prototype_bias, 
                        hpc_amplitude,
                        method='spearman', 
                        alternative='greater')


# Create Figure 3d

# Get correlation and p-value as text
cor_text = get_cor_output(df_decoding$neural_prototype_bias, 
                          df_decoding$hpc_amplitude, 
                          measure='spearman')

# Plot correlation between neural prototype bias and hippocampal amplitude
fig_3d = df_decoding |>
  ggplot(aes(neural_prototype_bias, hpc_amplitude)) +
  geom_smooth(method='lm', color='darkgray', se=F, size=1) +
  geom_point(color=color_single, alpha=0.8) +
  annotate("text", x=-.2, y=.075, label=cor_text, parse=T, family=theme_get()$text[["family"]]) +
  labs(x='Prototype vs exemplar proximity', y='Hippocampus amplitude') +
  lims(x=c(-.5, 1))
fig_3d

# Save plot
ggsave(paste0(path_plots,'Figure_3d.svg'), width=plot_size, height=plot_size)







