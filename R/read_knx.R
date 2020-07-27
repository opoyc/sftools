# Read KNX -----------------------------------------------------------

#' Maps the file name and assigns its respective function
#'
#' @param file string. File path to the table extracted from KNX.
#'
#' @importFrom readxl read_excel
#' @importFrom readxl read_xlsx
#' @importFrom readr read_delim
#' @importFrom janitor clean_names
#' @importFrom stats setNames
#' @importFrom lubridate parse_date_time
#' @importFrom utils getFromNamespace
#' @import dplyr
#' @import purrr
#' @import stringr
#' @return
#' @export
read_knx <- function(file){
  knx_table_func <- fnc_map[["int_function"]][map_lgl(fnc_map[["regex"]], ~str_detect(string = file, pattern = .x))]
  suppressWarnings(
    suppressMessages(
      getFromNamespace(knx_table_func, ns = "sftools")(file)
      )
    )
}

# Function mapping --------------------------------------------------------

fnc_map <- tibble::tribble(
                                                       ~regex,              ~int_function,
  "\\[Forecast Item\\] - ABC XYZ Calculation(\\{\\})?(?=\\.)",                 "read_seg",
          "\\[Forecast Item\\] Configuration(\\{\\})?(?=\\.)",           "read_fcst_conf",
       "\\[Forecast Item\\] Level Definition(\\{\\})?(?=\\.)",             "read_lev_def",
                   "Active Regressor Summary(\\{\\})?(?=\\.)",        "read_act_reg_summ",
          "Causal Factor Cleansing - Summary(\\{\\})?(?=\\.)", "read_causal_factor_clean",
                      "Edit Regressor Values(\\{\\})?(?=\\.)",     "read_edit_reg_values",
                        "Forecast Comparison(\\{\\})?(?=\\.)",           "read_fcst_comp",
                             "Forecast Items(\\{\\})?(?=\\.)",       "read_fcst_reg_item",
                               "life savings(\\{\\})?(?=\\.)",           "read_fcst_conf",
                    "Regressor Usage Summary(\\{\\})?(?=\\.)",      "read_reg_usage_summ",
                           "Regressor Values(\\{\\})?(?=\\.)",          "read_reg_values",
                                 "Regressors(\\{\\})?(?=\\.)",          "read_regressors",
             "Statistical Outliers Cleansing(\\{\\})?(?=\\.)",  "read_stat_outlier_clean",
                      "WID  Demand WaterFall(\\{\\})?(?=\\.)",    "read_demand_waterfall"
  )



#' Renaming date in column names helper
#'
#' @param col_names string. column names in a data.frame
#'
#' @return data.frame
#' @keywords internal
#' @noRd
rename_knx <- function(col_names){
  suppressWarnings(
    tibble(col_names) %>%
      mutate(fix_name = str_replace_all(col_names, rename_cols) %>%
               parse_date_time(orders = "my") %>%
               str_remove_all(pattern = " UTC") %>%
               ifelse(is.na(.)==T, col_names, .)) %>%
      pull("fix_name")
  )
}

#' Reading ABC XYZ segmentation helper
#'
#' @param file string. File name
#'
#' @return data.frame
#' @keywords internal
#' @noRd
read_seg <- function(file){

  format <- str_extract(file, "[A-Za-z]+$")

  if(format == "xlsx") {
    read_knx_tmp <- function() read_xlsx(file, skip = 1)
  } else if(format == "tab") {
    read_knx_tmp <- function() read_delim(file = file, delim = "\t", skip = 1)
  }

  read_knx_tmp() %>%
    setNames(nm = segmentation_lab) %>%
    mutate_if(is.character, .funs = ~str_squish(.x))
}

#' Reading Forecast configuration helper
#'
#' @param file string. File name
#'
#' @return data.frame
#' @keywords internal
#' @noRd
read_fcst_conf <- function(file){

  format <- str_extract(file, "[A-Za-z]+$")

  if(format == "xlsx") {
    read_knx_tmp <- function() read_xlsx(path = file, skip = 3)
  } else if(format == "tab") {
    read_knx_tmp <- function() read_delim(file = file, delim = "\t", skip = 3) %>%
      mutate_if(is.character, .funs = ~str_trim(.x))
  }

  tmp <- read_knx_tmp() %>%
    setNames(nm = fcst_conf_lab)

  if(nrow(tmp)<=1){
    message("Table 'Forecast Configuration (or Life Savings)' have no records")
    return(tmp)
  } else {
    return(tmp)
  }
}

#' Reading Level Definition helper
#'
#' @param file string. File name
#'
#' @return data.frame
#' @keywords internal
#' @noRd
read_lev_def <- function(file){
  format <- str_extract(file, "[A-Za-z]+$")

  if(format == "xlsx") {
    read_knx_tmp <- function() read_xlsx(path = file, skip = 1)
  } else if(format == "tab") {
    read_knx_tmp <- function() read_delim(file = file, delim = "\t", skip = 1)
  }

  read_knx_tmp() %>%
    setNames(nm = level_def_lab)
}

#' Reading Active Regressor Summary helper
#'
#' @param file string. File name
#'
#' @return data.frame
#' @keywords internal
#' @noRd
read_act_reg_summ <- function(file){

  format <- str_extract(file, "[A-Za-z]+$")

  if(format == "xlsx") {
    read_knx_tmp <- function() read_excel(file, skip = 1)
  } else if(format == "tab") {
    read_knx_tmp <- function() read_delim(file = file, delim = "\t", skip = 1)
  }

  tmp <- read_knx_tmp() %>%
    setNames(nm = act_reg_summ_lab)

  if(nrow(tmp)<=1){
    message("Table 'Active Regressor Summary' have no records")
    return(tmp)
  } else {
    return(tmp)
  }
}

#' Reading Edit Regressor Values helper
#'
#' @param file string. File name
#'
#' @return data.frame
#' @keywords internal
#' @noRd
read_edit_reg_values <- function(file){
  format <- str_extract(file, "[A-Za-z]+$")

  if(format == "xlsx") {
    read_knx_tmp <- function() read_xlsx(path = file)
  } else if(format == "tab") {
    read_knx_tmp <- function() read_delim(file = file, delim = "\t")
  }

  read_knx_tmp() %>%
    rename(reg_name = 1, reg_category = 2, reg_date = 3, reg_value = 4) %>%
    mutate(reg_date = as.Date(reg_date))
}

#' Reading Regressor Values helper
#'
#' @param file string. File name
#'
#' @return data.frame
#' @keywords internal
#' @noRd
read_reg_values <- function(file){

  format <- str_extract(file, "[A-Za-z]+$")

  if(format == "xlsx") {
    read_knx_tmp <- function() read_xlsx(path = file)
  } else if(format == "tab") {
    read_knx_tmp <- function() read_delim(file = file, delim = "\t")
  }

  read_knx_tmp() %>%
    rename(reg_name = 1, reg_category = 2) %>%
    clean_names() %>%
    setNames(nm = rename_knx(names(.)))
}


#' Reading Forecast comparison helper
#'
#' @param file string. File name
#'
#' @return data.frame
#' @keywords internal
#' @noRd
read_fcst_comp <- function(file){

  format <- str_extract(file, "[A-Za-z]+$")

  if(format == "xlsx") {
    read_knx_tmp <- function() read_xlsx(path = file, skip = 2)
  } else if(format == "tab") {
    read_knx_tmp <- function() read_delim(file, delim = "\t", skip = 2)
  }

  read_knx_tmp() %>%
    rename(forecast_item = 1, item_category = 2, actuals_category = 3
           , lifecycle = 4, series_type = 5, scenario = 6) %>%
    clean_names() %>%
    setNames(nm = rename_knx(names(.))) %>%
    filter(is.na(forecast_item)==F)
}


#' Reading Forecast Regressor Items helper
#'
#' @param file string. File name
#'
#' @return data.frame
#' @keywords internal
#' @noRd
read_fcst_reg_item <- function(file){

  format <- str_extract(file, "[A-Za-z]+$")

  if(format == "xlsx") {
    read_knx_tmp <- function() read_xlsx(path = file, skip = 2)
  } else if(format == "tab") {
    read_knx_tmp <- function() read_delim(file = file, delim = "\t", skip = 2)
  }

  tmp <- read_knx_tmp()

  if(ncol(tmp)==27){
    tmp %>% setNames(nm = fcst_reg_items_lab)
  } else {
    tmp %>% setNames(nm = fcst_reg_items_lab_details)
  }
}


#' Reading Regressor Usage Summary helper
#'
#' @param file string. File name
#'
#' @return data.frame
#' @keywords internal
#' @noRd
read_reg_usage_summ <- function(file){
  format <- str_extract(file, "[A-Za-z]+$")

  if(format == "xlsx") {
    read_knx_tmp <- function() read_xlsx(path = file)
  } else if(format == "tab") {
    read_knx_tmp <- function() read_delim(file = file, delim = "\t")
  }

  read_knx_tmp() %>%
    setNames(reg_usage_summ_lab)
}

#' Reading Regressors table helper
#'
#' @param file string. File name
#'
#' @return data.frame
#' @keywords internal
#' @noRd
read_regressors <- function(file){
  format <- str_extract(file, "[A-Za-z]+$")

  if(format == "xlsx") {
    read_knx_tmp <- function() read_xlsx(path = file)
  } else if(format == "tab") {
    read_knx_tmp <- function() read_delim(file = file, delim = "\t")
  }

  read_knx_tmp() %>%
    setNames(nm = c("select", "reg_name", "reg_category"))
}

#' Reading Statistical Outlier Cleansing helper
#'
#' @param file string. File name
#'
#' @return data.frame
#' @keywords internal
#' @noRd
read_stat_outlier_clean <- function(file){

  # lazy switcher

  format <- str_extract(file, "[A-Za-z]+$")

  if(format == "xlsx") {
    read_knx_tmp <- function() read_xlsx(path = file, skip = 1)
  } else if(format == "tab") {
    read_knx_tmp <- function() read_delim(file = file, delim = "\t", skip = 1)
  }

  read_knx_tmp() %>%
    rename(forecast_item = 1, forecast_category = 2, actuals_category = 3, outlier_conf = 4
           , outlier_summary = 5, series_type_1 = 6, series_type_2 = 7) %>%
    clean_names() %>% # janitor
    setNames(nm = rename_knx(names(.)))
}

#' Reading Causal Factor Cleansing helper
#'
#' @param file string. File name
#'
#' @return data.frame
#' @keywords internal
#' @noRd
read_causal_factor_clean <- function(file){

  format <- str_extract(file, "[A-Za-z]+$")

  if(format == "xlsx") {
    read_knx_tmp <- function() read_xlsx(path = file, skip = 1)
  } else if(format == "tab") {
    read_knx_tmp <- function() read_delim(file = file, delim = "\t", skip = 1)
  }

  read_knx_tmp() %>%
    rename(forecast_item = 1, causal_data_class_1 = 2, causal_data_class_2 = 3) %>%
    clean_names() %>%
    setNames(nm = rename_knx(names(.))) %>%
    filter(is.na(forecast_item)==F, !str_detect(forecast_item, "TOTAL"))

}

#' Reading Demand Waterfall helper
#'
#' @param file string. File name
#'
#' @return data.frame
#' @keywords internal
#'
#' @noRd
read_demand_waterfall <- function(file){

  format <- str_extract(file, "[A-Za-z]+$")

  if(format == "xlsx") {
    read_knx_tmp <- function() read_xlsx(file, skip = 2)
  } else if(format == "tab") {
    read_knx_tmp <- function() read_delim(file = file, delim = "\t", skip = 2)
  }

  read_knx_tmp() %>%
    clean_names() %>%
    setNames(nm = rename_knx(names(.))) %>%
    select(-"past") %>%
    rename("forecast_item" = 1) %>%
    filter(!str_detect(forecast_item, "TOTAL|<blank>"))
}


# Column names ------------------------------------------------------------

fcst_conf_lab <- c("abc", "xyz", "total_buckets", "forecast_item", "local_description"
                   , "baseline", "item_category", "unit_measure_fcst", "col_09"
                   , "unit_measure_control_set", "actuals_category", "item_usage_rule"
                   , "item_status_update", "col_14", "configured", "col_16", "lifecycle"
                   , "col_18", "model_param_set", "skip_leading_zeros", "holdout", "forecast_model"
                   , "trend_decay_factor", "fit_measure", "model_constant_usage", "calendar", "col_27"
                   , "intervals_historical", "intervals_forecast", "intervals_seasonal_cycle", "intervals_ma_adjust"
                   , "confidence_level", "col_33", "best_fit_model_set", "best_fit_holdout_period", "best_fit_forecast_lag"
                   , "col_37", "arima_constant", "arima_terms_ar", "arima_terms_ma", "arima_terms_diff"
                   , "col_42", "ets_param_set", "elastic_net_weight", "elastic_net_regu", "arimax_constant"
                   , "arimax_diff", "ac_conf_level", "ac_conf_level_apply", "col_50", "hist_start_date"
                   , "use_items_actuals", "hist_from_other_items", "hist_from_count", "col_55", "hist_by_other_items"
                   , "col_57", "adjust_start_date", "adjust_profile", "adjust_quantity", "adjust_multiplier"
                   , "col_62", "forecast_start_date", "forecast_stop_date", "override_forecast_start_date"
                   , "override_forecast_stop_date", "outlier_type", "outlier_view", "has_outliers", "outlier_data"
                   , "outlier_detection", "outlier_threshold", "outlier_ma_window", "output_errors", "output_charac"
                   )


segmentation_lab <- c("forecast_item", "forecast_item_local_description", "abc", "abc_volume", "abc_revenue"
  , "xyz", "total_volume", "total_volume_percent"
  , "total_volume_cummulative", "total_revenue"
  , "total_revenue_percent", "total_revenue_cummulative"
  , "cov")


level_def_lab <- c("gmid_id", "description", "local_description", "customer", "product_family", "site"
                        , "forecast_item_part_market", "gmid_site", "gmid", "gmid_region", "market_gmid"
                        , "market_province_gmid", "market_province_local_bu_gmid", "gmid_cust_channel"
                        , "forecast_item_selection_editable", "calculated_forecast_item"
                        , "current_forecast_item_configuration")


act_reg_summ_lab <- c("forecast_item", "item_category", "forecast_model", "reg_name", "lag", "col_x6"
                      , "estimate", "p_value", "standard_error", "t_stat")

rename_cols <- c("x01_01_"="jan_20", "x02_01_"="feb_20", "x03_01_"="mar_20"
                 , "x04_01_"="apr_20", "x05_01_"="may_20", "x06_01_"="jun_20"
                 , "x07_01_"="jul_20", "x08_01_"="aug_20", "x09_01_"="sep_20"
                 , "x10_01_"="oct_20", "x11_01_"="nov_20", "x12_01_"="dec_20"
                 , "x2"="forecast_type", "reference_plan_1_unconstrained_forecast"="unc_forecast"
                 , "bb_statistical"="stat_forecast", "forecast_item"="forecast_item")

fcst_reg_items_lab <- c("select", "col_02", "forecast_item", "local_description", "col_05"
                        , "forecast_category", "col_07", "regressors_total", "regressors_active"
                        , "weight_decay_factor", "elastic_net_weight", "elastic_net_regu"
                        , "arimax_constant", "arimax_difference_level", "col_15", "skip_leading_zeros"
                        , "model_param_set", "model_param_constant_usage", "forecast_model"
                        , "trend_decay_factor", "autocorrelation_confidence_level", "col_22"
                        , "actuals_category", "col_24", "intervals_historical", "intervals_forecast"
                        , "intervals_seasonal_cycle")

fcst_reg_items_lab_details <- c("select", "col_02", "forecast_item", "local_description", "col_05"
                           , "forecast_category", "unit_measure_forecast", "unit_measure_control_set"
                           , "col_09", "regressors_total", "regressors_active", "model_param_set"
                           , "weight_decay_factor", "elastic_net_weight", "elastic_net_regu"
                           , "arimax_constant", "arimax_difference_level", "col_18", "skip_leading_zeros"
                           , "control_set", "model_param_set", "model_param_constant_usage"
                           , "forecast_model", "trend_decay_factor", "autocorrelation_confidence_level"
                           , "fit_measure", "calendar", "col_28", "actuals_category", "col_30"
                           , "intervals_historical", "intervals_forecast", "intervals_seasonal_cycle"
                           , "intervals_moving_average")


reg_usage_summ_lab <- c("forecast_item", "reg_name", "process_rule", "lag")





