# `sftools (0.1.4)`: Statistical Forecasting team's tools

## Installation:

`remotes::install_github("opoyc/sftools")`  
`devtools::install_github("opoyc/sftools")`

This package is meant to gather the functions we commonly use, automate collaboration tasks and data analysis workflows.

## Objectives:

1. Having everything (possible) in one place
1. Less time running and fixing scripts and more time thinking and generating insights
1. Automate workflows/pipelines
1. Functions' authors will maintain and solve the issues, push the fixes and build the package's next version.
1. Have a uniform syntaxis to name objects and arguments and outputs.


## Functions:

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
   1. Wrangled/tidy version of GBU and Life Savers data bases `sftools::load_db()`
1. Kinaxis
   1. `sftools::read_knx()` automatically detects KNX tables and performs tidy transformations. Current compability:
      - [Forecast Item] - ABC XYZ Calculation
      - [Forecast Item] Configuration (and life savings)
      - [Forecast Item] Level Definition
      - Active Regressor Summary
      - Causal Factor Cleansing - Summary
      - Edit Regressor Values
      - Forecast Comparison
      - Forecast Items
      - Regressor Usage Summary
      - Regressor Values
      - Regressors
      - Statistical Outliers Cleansing
      - Demand Waterfall
