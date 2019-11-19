#' Series sum of the last 12 months
#'
#' @param series numeric of ts object
#'
#' @return numeric
#' @author Obryan Poyser
#' @import purrr
#'
#' @examples
#' \dontrun{
#' sum_twelve(AirPassengers)
#' }
sum_twelve <- purrr::possibly(function(series){
    length <- length(series)
    sum(series[(length-11):length], na.rm = T)
    }, otherwise = 0)
