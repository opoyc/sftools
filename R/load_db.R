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
load_db <- function(db, path=TRUE, on_globalenv=FALSE){

  local_env <- new.env()

  if(db == "GBU"){
    if(path == TRUE){
      path <- "//E21flsbcnschub/BCN_SC_HUB/SC.DATA/DATA/Active/Specific.GBU.Rdata"
    }
    load(path, envir = local_env)
    names(local_env[["GBU"]]) <- c("market", "gmid", "activity", "activity_pfwd", "gbu")
    local_env[["GBU"]][["key"]] <- paste0(strtrim(local_env[["GBU"]][["market"]], 2), ": ", local_env[["GBU"]][["gmid"]])
    tmp <- local_env[["GBU"]][c("key", "gbu")]
    gbu <- as_tibble(tmp[nchar(tmp[["key"]])>5,]) %>%
      group_by(key) %>%  # GBU is not well maintained, aggregating by key.
      summarise(gbu = paste0(unique(gbu), collapse = ""), .groups = "drop")

    if(on_globalenv==T){
      gbu <<- gbu
    } else {
      return(gbu)
    }

  } else if(db == "LSD"){
    if(path == TRUE){
      path <- "//E21flsbcnschub/BCN_SC_HUB/SC.DATA/DATA/Active/Specific.LSD.Rdata"
    }
    load(path, envir = local_env)
    names(local_env[["LSD"]]) <- c("market", "gmid", "status", "lsd")
    local_env[["LSD"]][["key"]] <- paste0(strtrim(local_env[["LSD"]][["market"]], 2), ": ", local_env[["LSD"]][["gmid"]])
    tmp <- local_env[["LSD"]][c("key", "lsd")]
    lsd <- as_tibble(tmp[nchar(tmp[["key"]])>5,])

    if(on_globalenv==T){
      lsd <<- lsd
    } else {
      return(lsd)
    }
  }
}
