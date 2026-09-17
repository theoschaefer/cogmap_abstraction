## Computational Modeling

Here, we provide information for how to fit the category learning models and how to compute the proximity values in the feature inference task using these models.

### Setup

We fit the two models as Bayesian models in the R statistics environment using the cmdstanr package.

Two of the used packages cannot be installed from CRAN:

- First, install cmdstanr using these instructions: https://mc-stan.org/cmdstanr/articles/cmdstanr.html
- Second, pull the following github repo: https://github.com/MirkoTh/rutils and install the package from source or use the devtool package and install rtools as follows: devtools::install("your-selected-path\\rutils")

Download the files called "behavior-categorisation-train_data.csv" and "behavior-categorisation-transfer_data" within the zip file "behavior.zip" available at Zenodo (10.5281/zenodo.22809081). Create a folder called "data" on in the modeling folder and copy the downloaded files to that folder.

### Fitting the Categorization Models

Run the file /scripts/fit-all-participants.R

### Comparing Predictions of the Two Categorization Models for the Feature Inference Task

Run the file /scripts/feature-inference.R

It generates a lookup table with distance values for both gaussian prototype and GCM exemplar models which can be converted to prototype and exemplar proximity values for the behavioral analysis.
