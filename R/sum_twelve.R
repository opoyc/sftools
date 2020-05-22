#' Sums the last 12 observations
#'
#' @param series numeric of ts object
#'
#' @return numeric
#' @author Obryan Poyser
#' @importFrom purrr possibly
#'
#' @examples
#' \dontrun{
#' sum_twelve(AirPassengers)
#' }
sum_twelve <- possibly(function(series){
    length <- length(series)
    sum(series[(length-11):length], na.rm = T)
    }, otherwise = 0)
