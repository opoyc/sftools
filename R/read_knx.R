# Read KNX -----------------------------------------------------------

#' Maps the file name and assigns its respective function
#'
#' @param file string. File path to the table extracted from KNX.
#'
#' @importFrom readxl read_excel
#' @importFrom janitor clean_names
#' @importFrom stats setNames
#' @importFrom lubridate parse_date_time
#' @importFrom utils getFromNamespace
#' @import dplyr
#' @import stringr
#' @return
#' @export
#'
read_knx <- function(file){
  file_clean <- str_remove_all(file, pattern = ".*(////|/)|//..+$|_[A-Z]+(?=\\.)|(_[A-Z0-9]+)?\\..*$")

  knx_table_func <- fnc_map[["int_function"]][which(fnc_map[["file_name"]] == file_clean)]

  getFromNamespace(knx_table_func, ns = "sftools")(file)
}

# Function mapping --------------------------------------------------------

fnc_map <- tibble::tribble(
                                    ~file_name,                ~int_function,
  "[Forecast Item] - ABC XYZ Calculation",                 "read_seg",
          "[Forecast Item] Configuration",           "read_fcst_conf",
       "[Forecast Item] Level Definition",             "read_lev_def",
               "Active Regressor Summary",        "read_act_reg_summ",
      "Causal Factor Cleansing - Summary", "read_causal_factor_clean",
                  "Edit Regressor Values",     "read_edit_reg_values",
                    "Forecast Comparison",           "read_fcst_comp",
                         "Forecast Items",       "read_fcst_reg_item",
                           "life savings",           "read_fcst_conf",
                "Regressor Usage Summary",      "read_reg_usage_summ",
                       "Regressor Values",          "read_reg_values",
                             "Regressors",          "read_regressors",
         "Statistical Outliers Cleansing",  "read_stat_outlier_clean",
                  "WID  Demand WaterFall",    "read_demand_waterfall"
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
  read_excel(path = file, skip = 2, col_names = segmentation_lab)
}

#' Reading Forecast configuration helper
#'
#' @param file string. File name
#'
#' @return data.frame
#' @keywords internal
#' @noRd
read_fcst_conf <- function(file){
  read_excel(path = file, skip = 4, col_names = fcst_conf_lab) #%>%
  #mutate_at(.vars = vars(matches("_date")), .funs = ~as.Date(.x))
}

#' Reading Level Definition helper
#'
#' @param file string. File name
#'
#' @return data.frame
#' @keywords internal
#' @noRd
read_lev_def <- function(file){
  read_excel(path = file, skip = 2, col_names = level_def_lab)
}

#' Reading Active Regressor Summary helper
#'
#' @param file string. File name
#'
#' @return data.frame
#' @keywords internal
#' @noRd
read_act_reg_summ <- function(file){
  read_excel(path = file, skip = 2, col_names = act_reg_summ_lab)
}


#' Reading Causal Factor Cleansing helper
#'
#' @param file string. File name
#'
#' @return data.frame
#' @keywords internal
#' @noRd
read_causal_factor_clean <- function(file){
  suppressMessages({
    read_excel(path = file, skip = 1) %>%
      rename(fcst_item = 1, ausal_data_class_1 = 2, causal_data_class_2 = 3)
  })
}

#' Reading Edit Regressor Values helper
#'
#' @param file string. File name
#'
#' @return data.frame
#' @keywords internal
#' @noRd
read_edit_reg_values <- function(file){
  read_excel(path = file) %>%
    rename(reg_name = 1, reg_category = 2, reg_date = 3, reg_value = 4) %>%
    mutate(reg_date = as.Date(reg_date))
}

#' Reading Forecast comparison helper
#'
#' @param file string. File name
#'
#' @return data.frame
#' @keywords internal
#' @noRd
read_fcst_comp <- function(file){
  suppressMessages({
    read_excel(path = file, skip = 2) %>%
      rename(fcst_item = 1, item_category = 2, actuals_category = 3
             , lifecycle = 4, series_type = 5, scenario = 6) %>%
      clean_names() %>% # janitor
      setNames(nm = rename_knx(names(.)))
  })
}


#' Reading Forecast Regressor Items helper
#'
#' @param file string. File name
#'
#' @return data.frame
#' @keywords internal
#' @noRd
read_fcst_reg_item <- function(file){
  read_excel(path = file, skip = 3, col_names = fcst_reg_items_lab)
}

#' Reading Regressor Usage Summary helper
#'
#' @param file string. File name
#'
#' @return data.frame
#' @keywords internal
#' @noRd
read_reg_usage_summ <- function(file){
  read_excel(path = file, skip = 1, col_names = reg_usage_summ_lab)
}

#' Reading Regressors table helper
#'
#' @param file string. File name
#'
#' @return data.frame
#' @keywords internal
#' @noRd
read_regressors <- function(file){
  read_excel(path = file) %>%
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
  suppressMessages({
    read_excel(path = file, skip = 1) %>%
      rename(fcst_item = 1, fcst_category = 2, actuals_category = 3, outlier_conf = 4
             , outlier_summary = 5, series_type_1 = 6, series_type_2 = 7) %>%
      clean_names() %>% # janitor
      setNames(nm = rename_knx(names(.)))
  })
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
  read_excel(file, skip = 2) %>%
    janitor::clean_names() %>%
    setNames(nm = rename_knx(names(.))) %>%
    select(-past) %>%
    rename("fcst_item" = 1) %>%
    filter(!str_detect(fcst_item, "TOTAL"))
}


# Column names ------------------------------------------------------------

fcst_conf_lab <- c("abc", "xyz", "total_buckets", "fcst_item", "baseline"
                   , "item_category", "unit_measure_fcst", "col_x8"
                   , "unit_measure_control_set", "actuals_category", "item_usage_rule"
                   , "item_status_update", "col_x13", "configured", "col_x15"
                   , "lifecycle", "col_x17", "model_param_set", "skip_leading_zeros"
                   , "holdout", "fcst_model", "trend_decay_factor"
                   , "fit_measure", "model_constant_usage", "calendar", "col_x26"
                   , "history_window", "fcst_horizon", "seasonal_cycle"
                   , "ma_adjust", "conf_level", "col_x32", "best_fit_model_set"
                   , "best_fit_holdout_period", "best_fit_fcst_lag", "col_x36", "arima_constant"
                   , "arima_terms_ar", "arima_terms_ma", "arima_terms_diff", "col_x41", "ets_param_set"
                   , "elastic_net_weight", "elastic_net_regu"
                   , "arimax_constant", "arimax_diff", "ac_conf_level"
                   , "ac_conf_level_apply", "col_x49", "hist_start_date", "hist_items_actuals"
                   , "hist_from_other_items", "hist_from_count", "col_x54"
                   , "hist_by_other_items", "col_x56", "adjust_start_date", "adjust_profile"
                   , "adjust_quantity", "adjust_multiplier", "col_x61", "fcst_start_date", "fcst_stop_date"
                   , "override_fcst_start_date", "override_fcst_stop_date", "outlier_type", "outlier_view"
                   , "has_outliers", "outlier_data", "outlier_detection"
                   , "outlier_threshold", "outlier_ma_window", "output_errors"
                   , "output_charac")

segmentation_lab <- c("forecast_item", "abc", "abc_volume", "abc_revenue", "xyz", "total_volume"
                      , "total_volume_perc", "total_volume_cum", "total_revenue", "total_revenue_perc"
                      , "total_revenue_cum", "cov")


level_def_lab <- c("gmid", "desc", "local_desc", "customer", "prod_family", "site", "part_market","gmid_site"
  ,"gmid_2","gmid_region","market_gmid_local_desc","market_prov_gmid_local_desc"
  ,"market_prov_local_bu_gmid_local_desc","gmid_customer_channel","fcst_item_editable"
  ,"fcst_item_calculated","fcst_item_current")

act_reg_summ_lab <- c("fcst_item", "item_category", "fcst_model", "reg_name", "lag", "col_x6"
                      , "estimate", "p_value", "standard_error", "t_stat")

rename_cols <- c("x01_01_"="jan_20", "x02_01_"="feb_20", "x03_01_"="mar_20"
                 , "x04_01_"="apr_20", "x05_01_"="may_20", "x06_01_"="jun_20"
                 , "x07_01_"="jul_20", "x08_01_"="aug_20", "x09_01_"="sep_20"
                 , "x10_01_"="oct_20", "x11_01_"="nov_20", "x12_01_"="dec_20"
                 , "x2"="fcst_type", "reference_plan_1_unconstrained_forecast"="unc_fcst"
                 , "bb_statistical"="stat_fcst", "forecast_item"="fcst_item")

fcst_reg_items_lab <- c("select", "col_x2", "fcst_item", "col_x4", "fcst_category", "col_x6"
                        , "reg_total", "reg_active"
                        , "weight_decay_factor", "elastic_net_weight"
                        , "elastic_net_regu", "arimax_constant", "arimax_diff"
                        , "col_x14", "skip_leading_zeros", "model_param_set"
                        , "param_model_constant", "fcst_model", "trend_decay_factor"
                        , "ac_conf_level", "col_x21", "actuals_category"
                        , "x_6", "history_window", "fcst_horizon", "seasonal_cycle")

reg_usage_summ_lab <- c("fcst_item", "reg_name", "process_rule", "lag")





