#' Returns the classification of a GMID according to its value or volume
#'
#' This is simple function that estimates the classification of an item given that its weight in the entire sample.
#' The classification is created as follow: $x <= .80 ~ A, x <= .95 ~ B, otherwise ~ C$.
#' @param series numeric vector
#'
#' @return string
#' @author Obryan Poyser
#'
#' @examples
#' \dontrun{
#' class_volval(AirPassengers)
#' }
class_volval <- function(series){
    df <- data.frame(index=1:length(series), series)
    df <- df[order(df[,"series"], decreasing = T),]
    df$cumsum <- cumsum(df$series)/sum(df$series, na.rm = T)
    df$class <- ifelse(df$cumsum<=.8, "A", ifelse(df$cumsum>.8 & df$cumsum<=.95, "B", "C"))
    return(df[order(df$index),"class"])
}

