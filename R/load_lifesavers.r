#' Get life savers data
#'
#' @param lifesavers_path string. Has the default directory
#' @param on_globalenv logical. Defines is the dataframe should be loaded in the global environment or not.
#'
#' @return tibble
#' @author Obryan Poyser
#'
#' @importFrom dplyr as_tibble
#'
#' @examples
#' \dontrun{
#' load_lifesavers()
#' }
load_lifesavers <- function(lifesavers_path, on_globalenv=FALSE){
    if(missing(lifesavers_path)){
        lifesavers_path <- "//E21flsbcnschub/BCN_SC_HUB/SC.DATA/DATA/Active/Specific.LSD.Rdata"
    }
    local_env <- new.env()
    load(lifesavers_path, envir = local_env)
    names(local_env$LSD) <- c("loc", "gmid", "lsd")
    local_env$LSD$key <- paste0(strtrim(local_env$LSD$loc, 2), ": ", local_env$LSD$gmid)
    lsd <- as_tibble(na.omit(local_env$LSD[c("key", "gmid", "lsd")]))
    if(on_globalenv==TRUE){
        lsd <<- lsd
    } else {
        return(lsd)
    }
}
