# `sftools`: Statistical Forecasting team's tools

This package is meant to automate collaboration tasks and data analysis workflows. 

The package includes the following functions:

## Updating regressors

1. `sftools::empty_reg()`: creates a template to add regressors
1. `sftools::load_reg_inputs()`: load KNX's files needed to update the regressors' tables
1. `sftools::harvest_reg()`: gather regressors' updates and perform sanity checks to avoid loading KNX's loading errors.
