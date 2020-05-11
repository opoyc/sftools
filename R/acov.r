#' Adjusted Coefficient of Variation
#'
#' This function calculates the adjusted coefficient of variation (COV) from the last 12 months
#' of a time series.
#'
#' The COV measures the dispersion around the mean. By adjusted it means that the estimate of the dispersion
#' is taking as input the error component from a Seasonal and Trend Decomposition by LOESS, and adjusting by the degrees of freedom.
#' @param series numeric or ts object
#' @param adjusted_cov logical. If TRUE, return adjusted cov, otherwise adjusted standard deviation
#' @param ... other arguments from base functions
#' @return numeric or ts object
#' @export
#' @author Obryan Poyser
# @importFrom purrr possibly
#'
#' @examples
#' \dontrun{
#' acov(series = AirPassengers, adjusted_cov = T)
#' }
acov <- function(series, adjusted_cov=T){

    series <- series[!cumsum(series)==0] # eliminate leading zeros
    class <- class(series)
    length <- length(series)

    if(class=="numeric"){ # convert to ts freq=12 to be decomposed.
        series <- ts(series, start = c(1,1), frequency = 12)
    }

    if(length(series)>24 & adjusted_cov==T){
        resi <- stl(series, s.window="periodic", robust=T)$time.series[,3]
        sd <- (sd(resi[(length-11):length], na.rm = T)*sqrt((12-1)/12))
        mean <- (mean(series[(length-11):length], na.rm = T))

        if(mean==0){
          message("Zero-mean vector, the adjusted covariance cannot be calculated. Please check the data for the last 12 months.")
          return(NA)
        } else {
          return(sd/mean)
        }
    } else if(length(series)>24 & adjusted_cov!=T){

        resi <- stl(series, s.window="periodic", robust=T)$time.series[,3]
        (sd(resi[(length-11):length], na.rm = T)*sqrt((12-1)/12))

    } else if(length(series)<=24 & adjusted_cov==T){

        sd <- sd(series, na.rm = T)
        mean <- mean(series, na.rm = T)

        if(mean==0){
          message("Zero-mean vector, the adjusted covariance cannot be calculated. Please check the data for the last 12 months.")
          return(NA)
        } else {
            return(sd/mean)
        }
    } else {
        sd(series[(length-11):length], na.rm = T)
    }
}
