#' Harvest team members regressors values
#'
#' This functions perform several task, first it loads the files into the environment then
#' brings the possibility to do sanity checks to prevent importing error in Kinaxis
#' @param dir_path String. Directory where all the CSV files are located.
#'
#' @return Depends on users' selections. Ultimately two tab files "Regressors.tab" and "Edit Regressors Values.tab"
#' @import dplyr
#' @import janitor
#' @import purrr
#' @import readr
#' @import stringr
#' @import lubridate
#' @import tibble
#' @import tidyr
#' @export
#'
#' @examples
#' \dontrun{
#' harvest_reg()
#' }
harvest_reg <- function(dir_path){

    # Selection source
    if(missing(dir_path)==T){
        dir_path <- paste0(choose.dir(), "\\")
    } else {
        dir_path <- dir_path
    }

    # Getting files' data
    reg_list_files <- list.files(dir_path, pattern = "[0-9]_reg_list.csv")
    reg_values_files <- list.files(dir_path, pattern = "[0-9]_reg_values.csv")

    # loading data
    reg_list_data <- purrr::map(paste0(dir_path, reg_list_files)
                                  , .f = ~suppressMessages(readr::read_csv(.x)))
    reg_values_data <- purrr::map(paste0(dir_path, reg_values_files)
                                  , .f = ~suppressMessages(readr::read_csv(.x)))
    # Naming frames
    names(reg_list_data) <- stringr::str_remove_all(reg_list_files, "_reg_list.csv")
    names(reg_values_data) <- stringr::str_remove_all(reg_values_files, "_reg_values.csv")

    message(
        paste0(names(reg_list_data)," added ", map(reg_list_data, nrow), " regressors \n")
        , paste0(names(reg_values_data)," added ", map(reg_values_data, nrow), " values \n")
    )

    # Append zeros to the last value?

    message("Appending zeros to regressor's values...")
    reg_values_data <- reg_values_data %>%
      bind_rows(.id = "user") %>%
      mutate(Year=str_sub(Year, start = -2, end=-1)
             , Date=lubridate::parse_date_time(paste0(Year,"-", Month, "-", "01")
                                             , orders = "ymd") %>%
               as.Date()) %>%
      dplyr::select(-Month, -Year) %>%
      group_nest(user, Regressor, Category) %>%
      mutate(data=map(data, function(data) {
        seq.Date(from = min(data[["Date"]])
                 , to = max(data[["Date"]]+months(1))
                 , by = "month") %>%
          enframe(value = "Date") %>%
          left_join(data, by="Date") %>%
          replace_na(list(Quantity=0)) %>%
          dplyr::select(-name)
      })) %>%
      unnest(data) %>%
      mutate(Date=paste0(str_extract(Date, "(?<=-)[0-9]{2}(?=-)")
                         , "-01-"
                         , str_extract(Date, "^[0-9]{4}") %>% str_sub(start = -2, end = -1)))

    message("\n Done! \n")

    # binding values

    added_reg_values <- reg_values_data %>%
        dplyr::select(user, Regressor, Category, Date, Quantity)
    added_reg_list <- bind_rows(reg_list_data, .id = "user") %>%
        mutate(Select="N") %>%
        dplyr::select(user, Select, Name, Category)

    # Append new values to the old tables

    new_reg_values <- bind_rows(reg_values, added_reg_values[,-c(1)])
    new_reg_list <- bind_rows(reg_list, added_reg_list[,-1])

    # data consistency checks
    if(menu(c("Yes", "No")
            , title="Do you want to perform data consistency checks?")==1){ # Should it be optional?

        # Analyzing Regressors' list----------------------------------------------------------

        message("\n\n--------Analyzing Regressors' list--------\n")
        message("1. Checking for category anomalies")
        if(all((added_reg_list$Category %in% reg_list$Category)==T)==T){
            message(">>> PASSED\n")
        } else {
            message(">>> FAILED\n")
            error <- added_reg_list %>%
                mutate(error=ifelse(Category %in% unique(reg_list$Category), T, F)) %>%
                filter(error==F)
            return(error)
            break()
        }
        message("2. Filtering unique regressors' names")
        new_reg_list <- unique(new_reg_list)

        # Analyzing Regressors' values-----------------------------------------------------------

        message("\n\n--------Analyzing Regressors' values--------\n")
        message("1. Checking for names' anomalies in 'reg_values'")
        if(all(is.na(added_reg_values[[1]])==F)==T){
            message(">>> PASSED\n")
        } else {
            stop(">>> FAILED\n")
        }
        message("2. Checking for category anomalies")
        if(all((added_reg_values$Category %in% reg_list$Category)==T)==T){
            message(">>> PASSED~\n")
        } else {
            message(">>> FAILED\n")
            error_cat <- added_reg_values %>%
                mutate(error=ifelse(Category %in% unique(reg_list$Category), T, F)) %>%
                filter(error==F)
            return(error_cat)
            break()
        }
        message("3. Checking for cross-reference with the updated regressors' list")
        if(all(unique(added_reg_values$Regressor) %in% unique(new_reg_list$Name))==T){
            message(">>> PASSED\n")
        } else {
            message(">>> FAILED\n")
            error_name <- added_reg_values %>%
                mutate(error=ifelse(Regressor %in% unique(new_reg_list$Name), T, F)) %>%
                filter(error==F)
            return(error_name)
            break()
        }
        message("4. Filtering unique values")
        new_reg_values <- unique(new_reg_values)
        message(">>> PASSED\n")
    }

    # Exporting dialogues--------------------------------------------------------------

    if(menu(choices = c("Yes", "No")
            , title = "Do you want to export the updated tables to the current environment?")==1){
        new_reg_values <<- new_reg_values
        new_reg_list <<- new_reg_list
        message("New objects have been added to your environment: new_reg_values & new_reg_list")
    }
    if(menu(choices = c("Yes", "No")
            , title = "Do you want to export the updated regressor's tables to your desktop?")==1){
        write.table(new_reg_values
                    , paste0("C:/Users/"
                             , Sys.info()[["user"]]
                             , "/Desktop/Edit Regressor Values_"
                             ,  janitor::make_clean_names(Sys.time())
                             , ".txt")
                    , sep="\t"
                    , row.names = FALSE, quote = FALSE, na = "")
        write.table(new_reg_list
                    , paste0("C:/Users/"
                             , Sys.info()[["user"]]
                             , "/Desktop/Regressors_"
                             , janitor::make_clean_names(Sys.time())
                             , ".txt")
                    , sep="\t"
                    , row.names = FALSE, quote = FALSE, na = "")
    }
}
