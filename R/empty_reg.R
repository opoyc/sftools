#' Generate regressors tables' template
#'
#' This function creates two CSV files on users' desktop. One is mean to
#' include regressors' values and other for regressors' names,
#' for Regressors and Edit Regressors Values respectively.
#' @author Obryan Poyser
#' @return two CSV files on users' desktop
#' @export
#' @import dplyr
#' @import janitor
#' @import purrr
#' @import readr
#' @import stringr
#'
#' @examples
#' \dontrun{
#' empty_reg()
#' }
empty_reg <- function(){
    readline(prompt="This function will create 2 empty CSV file in your desktop:
             - reg_list: describes the new regressors
             - reg_values: describes new regressors' values\nPress [ENTER] to continue")
    user <- Sys.info()[["user"]]
    readr::write_csv(x = data.frame(Regressor=as.character(), Category=as.character()
                                    , Month=as.numeric(), Year=as.numeric(), Quantity=as.numeric())
                     , path = paste0("C:/Users/",user, "/Desktop/", user, "_reg_values.csv")
                     , append = FALSE
                     , na = "")
    readr::write_csv(x = data.frame(Name=as.character(), Category=as.character())
                     , path = paste0("C:/Users/",user, "/Desktop/", user, "_reg_list.csv")
                     , append = FALSE
                     , na = "")
}
