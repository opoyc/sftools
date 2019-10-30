#' Get GBU data
#'
#' @param gbu_path string. Has the default directory
#'
#' @return tibble
#' @author Obryan Poyser
#'
#' @examples
#' \dontrun{
#' load_gbu()
#' }
load_gbu <- function(gbu_path){
    if(missing(gbu_path)){
        gbu_path <- "//sinsdfs01/regional$/APJ-SC-HUB/SC.DATA/DATA/Active/Specific.GBU.Rdata"
    }
    load(gbu_path)
    gbu <<- GBU %>%
        as_tibble() %>%
        mutate(key=paste0(str_sub(Market.Code, start = 1, end = 2), "-", GMID.Code)) %>%
        dplyr::select(key, gmid=GMID.Code, gbu=GBU) %>%
        unique() %>%
        filter(gbu!="")
    message("gbu (tidy version) have been exported to the global env.")
}
