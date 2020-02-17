#' Get GBU data
#'
#' @param gbu_path string. Has the default directory
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
load_gbu <- function(gbu_path, on_globalenv=F){
    if (missing(gbu_path)) {
        gbu_path <- "//E21flsbcnschub/BCN_SC_HUB/SC.DATA/DATA/Active/Specific.GBU.Rdata"
    }
    local_env <- new.env()
    load(gbu_path, envir = local_env)
    names(local_env$GBU) <- c("market", "gmid", "activity", "activity_pfwd", "gbu")
    local_env$GBU$key <- paste0(strtrim(local_env$GBU$market, 2), ": ", local_env$GBU$gmid)
    gbu <- local_env$GBU[c("key", "gmid", "gbu")]
    gbu <- as_tibble(gbu[nchar(gbu$key)>5,])
    if(on_globalenv==T){
        gbu <<- gbu
    } else {
        return(gbu)
    }
}
