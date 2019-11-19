#' Adjusted Coefficient of Variation
#'
#' This function calculates the adjusted coefficient of variation from the last 12 months
#' of a time series.
#' @param series numeric or ts object
#'
#' @return numeric or ts object
#' @export
#' @author Obryan Poyser
#' @import purrr
#'
#' @examples
#' \dontrun{
#' acov(AirPassengers)
#' }
acov <- purrr::possibly(function(series){
    series <- series[!cumsum(series)==0]
    class <- class(series)
    length <- length(series)
    if(class=="numeric"){
        series <- ts(series, start = c(1,1), frequency = 12)
    }
    if(length(series)>24){
        resi <- stl(series, s.window="periodic", robust=T)$time.series[,3]
        (sd(resi[(length-11):length], na.rm = T)*sqrt((12-1)/12))/(mean(series[(length-11):length], na.rm = T))
    } else {
        sd(series[(length-11):length], na.rm = T)/mean(series[(length-11):length], na.rm = T)
    }
}, otherwise = NA)
