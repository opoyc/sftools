#' Get Global Business Unit data
#'
#' Analytics' teams manages a database of Global Business Unit that links Location, GMID.
#' There is a default directory located in ~Active/Specific.GBU.Rdata.
#'
#' @param gbu_path string. Has the default directory
#' @param db string. Defines which database to load. For now only two are available.
#' #' Options: *GBU*, *LS*.
#' @param on_globalenv logical. Defines is the dataframe should be loaded in the global environment or not.
#'
#' @return tibble
#' @author Obryan Poyser
#'
#' @importFrom dplyr as_tibble
#'
#' @examples
#' \dontrun{
#' load_gbu()
#' }
load_gbu <- function(gbu_path, data_base="GBU", on_globalenv=F){
  local_env <- new.env()
  load(gbu_path, envir = local_env)

  if (missing(gbu_path)) {
    if(data_base=="GBU"){
      gbu_path <- "//E21flsbcnschub/BCN_SC_HUB/SC.DATA/DATA/Active/Specific.GBU.Rdata"

      names(local_env$GBU) <- c("market", "gmid", "activity", "activity_pfwd", "gbu")
      local_env$GBU$key <- paste0(strtrim(local_env$GBU$market, 2), ": ", local_env$GBU$gmid)
      gbu <- local_env$GBU[c("key", "gmid", "gbu")]
      gbu <- as_tibble(gbu[nchar(gbu$key)>5,])
      if(on_globalenv==T){
        gbu <<- gbu
      } else {
        return(gbu)
      }
    } else {
      lifesavers_path <- "//E21flsbcnschub/BCN_SC_HUB/SC.DATA/DATA/Active/Specific.LSD.Rdata"

      names(local_env$LSD) <- c("loc", "gmid", "lsd")
      local_env$LSD$key <- paste0(strtrim(local_env$LSD$loc, 2), ": ", local_env$LSD$gmid)
      lsd <- as_tibble(na.omit(local_env$LSD[c("key", "gmid", "lsd")]))
      if(on_globalenv==TRUE){
        lsd <<- lsd
      } else {
        return(lsd)
      }
    }
  }
}
