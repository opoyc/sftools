#' Loads regressors' inputs
#'
#' Loading input .tab files from Kinaxis tables Regressors and Edit Regressors Values' worksheets
#' @param reg_list_path String. Path where the Regressors' names are located
#' @param reg_values_path String. Path where the Regressors' values are located
#'
#' @return reg_list and reg_values for regressors names and regressors values
#' @export
#' @import
#'  - dplyr
#'  - janitor
#'  - purrr
#'  - readr
#'  - stringr
#'
#' @examples load_reg_inputs()
load_reg_inputs <- function(reg_list_path, reg_values_path){
    if(missing(reg_list_path)==T | missing(reg_values_path)){
        readline(prompt = "Select 'Regressors.tab' file \nPress [ENTER] to continue")
        reg_list_path <- file.choose()
        readline(prompt = "Select 'Edit Regressor Values.tab' file \nPress [ENTER] to continue")
        reg_values_path <- file.choose()
    } else {
        reg_list_path <- reg_list_path
        reg_values_path <- reg_values_path
    }
    suppressMessages(
        {
            reg_list <- readr::read_delim(reg_list_path, delim = "\t")
            reg_values <- readr::read_delim(reg_values_path, delim = "\t")
        }
    )
    message(
        paste0("There are ", nrow(reg_values)," values and ", nrow(reg_list), " regressors"
               #,  "\nCreated ", file.info(reg_list_path)$ctime
               #, "\nModified ", file.info(reg_list_path)$mtime
               )
    )
    reg_list <<- reg_list
    reg_values <<- reg_values
    message("reg_list and reg_values had been added to your global environment")
}
