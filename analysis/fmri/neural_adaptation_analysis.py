#!/usr/bin/env python3
# -*- coding: utf-8 -*-
##===========================================================================##
#                       Neural Adaptation Analysis
#
# Publication: The hippocampus supports interpolation during category abstraction
# Nature Communications       
#
# Script Author: Dr. Theo AJ Schaefer                                                
# Contact: theo.schaefer@uni-hamburg.de             
#    
##===========================================================================##


##===========================================================================##
##                                SETUP                                      ##
##===========================================================================##

# Load packages
import numpy as np
import pandas as pd
import seaborn as sns
import nilearn
from nilearn.image import load_img, new_img_like, math_img, binarize_img
from nilearn.plotting import plot_stat_map, plot_glass_brain
from nilearn.reporting import get_clusters_table
from pathlib import Path

# Plotting options
sns.set_context('poster', font_scale=0.75)

# Statistical settings
alpha = 0.05
logp_threshold = -np.log10(alpha)  # ~1.3010


# Paths

# Root: make sure to set script folder as working directory!            
path_root = Path.cwd().parents[1]

# Plot path
path_plots = path_root / 'plots' / 'fmri' / 'adaptation_analysis'
path_plots.mkdir(parents=True, exist_ok=True)

# Data path
path_data = path_root / 'data' / 'fmri' / 'adaptation_analysis'
path_data.mkdir(parents=True, exist_ok=True)



#%%
#=============================================================================#
#---------------------- Prototype Modulation ---------------------------------#
#=============================================================================#


# Load TFCE-corrected p-value map and t-value map of prototype modulation contrast
fn_img_pval = path_data / 'modulation-prototype_contrast-prototype_mask-hpc_logp-max-tfce.nii.gz'
fn_img_tval = path_data / 'modulation-prototype_contrast-prototype_mask-hpc_t.nii.gz'

p_img = load_img(fn_img_pval)
t_img = load_img(fn_img_tval)


# Load nilearn MNI template with 1mm resolution (for plotting)
mni_1mm_template_img = nilearn.datasets.load_mni152_template(resolution=1)


# Create significant-voxel thresholded mask
mask_significant = binarize_img(p_img, threshold=logp_threshold)


# Threshold t image based on mask
t_img_thres = math_img('a*b', a=t_img, b=mask_significant)


# Create Figure 4b
plot_stat_map(t_img_thres, 
              draw_cross=False, 
              display_mode='yx',
              colorbar=True, 
              symmetric_cbar=1,
              cmap='BrBG', 
              bg_img=mni_1mm_template_img, 
              black_bg=False,
              # If you want to save plot:
              # output_file=(f'{path_plots}/Figure_4b__modulation-prototype_contrast-prototype_mask-hpc.svg')
              )

#--- Get significant cluster coordinates and peak statistics
df_clusters = get_clusters_table(stat_img=p_img, stat_threshold=logp_threshold)
df_clusters['pval'] = 10 ** -df_clusters['Peak Stat']  # convert log-p-values to p-values

# If no significant clusters, get peak/minimum p-value 
min_pval = 10 ** -load_img(p_img).get_fdata().max() 



#%%
#=============================================================================#
#----------------------- Exemplar Modulation ---------------------------------#
#=============================================================================#


# Load TFCE-corrected p-value map and t-value map of exemplar modulation contrast
fn_img_pval = path_data / 'modulation-exemplar_contrast-exemplar_mask-hpc_logp-max-tfce.nii.gz'
fn_img_tval = path_data / 'modulation-exemplar_contrast-exemplar_mask-hpc_t.nii.gz'

p_img = load_img(fn_img_pval)
t_img = load_img(fn_img_tval)


# Load nilearn MNI template with 1mm resolution (for plotting)
mni_1mm_template_img = nilearn.datasets.load_mni152_template(resolution=1)


# Create significant-voxel thresholded mask
mask_significant = binarize_img(p_img, threshold=logp_threshold)


# Threshold t image based on mask
t_img_thres = math_img('a*b', a=t_img, b=mask_significant)


# Create Figure (will result in empty image due to no surviving clusters)
plot_stat_map(t_img_thres, 
              draw_cross=False, 
              display_mode='yx',
              colorbar=True, 
              symmetric_cbar=1,
              cmap='BrBG', 
              bg_img=mni_1mm_template_img, 
              black_bg=False,
              # If you want to save plot:
              # output_file=(f'{path_plots}/modulation-exemplar_contrast-exemplar_mask-hpc.svg')
              )

#--- Get significant cluster coordinates and peak statistics
df_clusters = get_clusters_table(stat_img=p_img, stat_threshold=logp_threshold)
df_clusters['pval'] = 10 ** -df_clusters['Peak Stat']  # convert log-p-values to p-values

# If no significant clusters, get peak/minimum p-value 
min_pval = 10 ** -load_img(p_img).get_fdata().max() 




