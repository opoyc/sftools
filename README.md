# Statistical Forecasting team's tools

## Last version: `sftools (0.1.5)`

## Installation:

### Remote

#### From Github

`remotes::install_github("opoyc/sftools")`  
`devtools::install_github("opoyc/sftools")`

#### From Gitlab

```{r}
install_sanofi_pkg <- function(){
  choice <- menu(choices = c("kinapi", "sftools", "None"), title = "Which package would you like to install?")
  if(choice == 1){
    pkg <- "https://emea-aws-gitlab.sanofi.com:3001/statistical_forecasting/packages/kinapi.git"
  } else if(choice == 2) {
    pkg <- "https://emea-aws-gitlab.sanofi.com:3001/statistical_forecasting/packages/sftools.git"
  } else if(choice == 3){
    message("Bye!")
  }
  cred <- git2r::cred_user_pass(rstudioapi::askForPassword("Username"), rstudioapi::askForPassword("Password"))
  devtools::install_git(pkg, credentials = cred)
}
```

Then run:

`install_sanofi_pkg()`

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

## Known problems

The package have several depedencies, if you face a problem installing it please update them using:

```{r}
suppressMessages(
  {
    packages = c('tidyverse', 'janitor', 'dplyr', 'readr', 'stringr', 'DescTools'
    , 'dvmisc', 'purrr', 'pracma', 'lubridate', 'tibble', 'tidyr', 'changepoint'
    , 'forecast', 'stlplus', 'readxl')
    check_packages <- lapply(
      packages,
      FUN = function(x) {
        if (!require(x, character.only = TRUE)) {
          install.packages(x, dependencies = TRUE)
          library(x, character.only = TRUE)
        }
      }
    )
    #source: https://vbaliga.github.io/verify-that-r-packages-are-installed-and-loaded/
  }
)
```
