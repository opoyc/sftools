#' Adjusted Standard Deviation
#'
#' This function calculates the adjusted standard deviation from the last 12 months
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
#' astd(AirPassengers)
#' }
astd <- purrr::possibly(function(series){
    series <- series[!cumsum(series)==0]
    class <- class(series)
    length <- length(series)
    if(class=="numeric"){
        series <- ts(series, start = c(1,1), frequency = 12)
    }
    if(length(series)>24){
        resi <- stl(series, s.window="periodic", robust=T)$time.series[,3]
        (sd(resi[(length-11):length], na.rm = T)*sqrt((12-1)/12))
    } else {
        sd(series[(length-11):length], na.rm = T)
    }
}, otherwise = NA)
