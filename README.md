# cogmap_abstraction

## Computational Modeling
Here, we provide information for how to fit the category learning models and how to compute the proximity values in the feature inference task using these models.

### Setup

We fit the two models as Bayesian models in the R statistics environment using the cmdstanr package.

Two of the used packages cannot be installed from CRAN:

- First, install cmdstanr using these instructions: https://mc-stan.org/cmdstanr/articles/cmdstanr.html
- Second, pull the following github repo: https://github.com/MirkoTh/rutils and install the package from source or use the devtool package and install rtools as follows: devtools::install("your-selected-path\\rutils")

Download the files called "infpro_task-cat_beh" and "infpro_task-cat2_beh", which are going to be provided by the first author via the open science framework (OSF) upon publication of the associated manuscript. Create a folder called "data" on the root level of the project (i.e., /hierarchical-categorization/data) and copy the downloaded files to that folder.

### Fitting the Categorization Models

Run the file R/scripts/fit-all-participants-infpro.R

### Comparing Predictions of the Two Categorization Models for the Feature Inference Task

Run the file R/scripts/feature-inference.R

It generates a lookup table with distance values for both gaussian prototype and GCM exemplar models which can be converted to prototype and exemplar proximity values for the behavioral analysis.
