#' Get life savers data
#'
#' @param lifesavers_path string. Has the default directory
#'
#' @return tibble
#' @author Obryan Poyser
#'
#' @examples
#' \dontrun{
#' load_lifesavers()
#' }
load_lifesavers <- function(lifesavers_path){
    if(missing(lifesavers_path)){
        lifesavers_path <- "//sinsdfs01/regional$/APJ-SC-HUB/SC.DATA/DATA/Active/Specific.LSD.Rdata"
    }
    load(lifesavers_path)
    lifesavers <<- LSD %>%
    janitor::clean_names() %>%
    as_tibble() %>%
    mutate(words_length=str_count(loc, pattern = "[A-Z]")) %>%
    filter(words_length==2, str_detect(loc, pattern = "001")) %>%
    mutate(loc2=str_extract(string = loc, pattern = "[A-Z]{2}")
           , key=paste0(loc2, "-", gmid)
           , lsd=ifelse(lsd==T, "Yes", "No"))
    message("lifesavers (tidy version) have been exported to the global env.")
}
