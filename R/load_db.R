#' Get Sanofi product descriptors
#'
#' Databases of Global Business Unit and Life Savings Drugs
#'
#' @param db string. For now "GBU" and "LSD" are available.
#' @param path logical = TRUE, if a path is provided replace the default path.
#' @param on_globalenv logical. Defines is the dataframe should be loaded in the global environment or not.
#' @return tibble
#' @author Obryan Poyser
#'
#' @export
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' load_db(db = "GBU", on_globalenv = T)
#' }
load_db <- function(db, default_path=TRUE){

  tmp_env <- new.env()

  if(db == "GBU"){

    if(default_path == TRUE){
      path <- "//E21flsbcnschub/BCN_SC_HUB/SC.DATA/DATA/Active/Specific.GBU.Rdata"
    } else {
      path <- default_path
    }

    load(path, envir = tmp_env)

    as.list(tmp_env)[[1]] %>%
      as_tibble() %>%
      setNames(nm = c("market", "gmid", "activity", "activity_pfwd", "gbu")) %>%
      filter(grepl("[0-9]{4,6}", gmid)) %>%
      transmute(key = paste0(gsub(pattern = "[0-9]+", replacement = "", market), ": ", gmid)
                , gbu) %>%
      group_by(key) %>%  # GBU is not well maintained, aggregating by key.
      summarise(gbu = paste0(unique(gbu), collapse = ""), .groups = "drop")

  } else if(db == "LSD"){

    if(default_path == TRUE){
      path <- "//E21flsbcnschub/BCN_SC_HUB/SC.DATA/DATA/Active/Specific.LSD.Rdata"
    } else {
      path <- default_path
    }

    load(path, envir = tmp_env)

    as.list(tmp_env)[[1]] %>%
      as_tibble() %>%
      setNames(nm = c("market", "gmid", "lsd")) %>%
      filter(grepl("[0-9]{4,6}", gmid)) %>%
      transmute(key = paste0(gsub(pattern = "[0-9]+", replacement = "", market), ": ", gmid)
                , lsd)
    }
}
