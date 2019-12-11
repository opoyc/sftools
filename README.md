# `sftools`: Statistical Forecasting team's tools

This package is meant to gather the functions we commonly use, automate collaboration tasks and data analysis workflows.

## Objectives:

1. Having everything (possible) in one place
1. Less time running and fixing scripts and more time thinking and generating insights
1. Automate workflows/pipelines
1. Functions' authors will maintain and solve the issues, push the fixes and build the package's next version.
1. Have a uniform syntaxis to name objects and arguments and outputs.


## Functions 

1. Statistical
  1. Adjusted Coefficient of Variance `sftools::acov()`
  1. Winsorization `sftools::winsorize()`
  1. Outlier method `sftools::outlier_method()`
  1. Seasonality detection `sftools::seas_detect()` *(beta)*
  1. Generate fitted `sftools::generate_fitted()`
  1. Recommend starting date `sftools::tscut()`
1. Summary 
  1. Filter the last 12 months `sftools:::sum_twelve()`
  1. Generate segmentation classes `sftools:::class_volval()`
1. Load common databases
  1. Wrangled/tidy version of GBU db `sftools:::load_gbu()`
  1. Wrangled/tidy version of Life Savers db `sftools:::load_lifesavers()`
1. Updating regressor table
  1. Create a template were to fill the regressors `sftools::empty_reg()` (soft deprecated due to KNX update)
  1. Gather teams' regressors `sftools::harvest_reg()` (soft deprecated due to KNX update)


## Future improvements

1. Pipeline for KNX's monthly and quaterly cycle based on drake