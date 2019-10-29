#' Series' sum of the last 12 months of
#'
#' @param series numeric of ts object
#'
#' @return numeric
#' @author Obryan Poyser
#' @import purrr
#'
#' @examples
#' sum12(AirPassengers)
sum12 <- purrr::possibly(function(series){
    #series <- series[!cumsum(series)==0]
    length <- length(series)
    sum(series[(length-11):length], na.rm = T)
}, otherwise = NA)
