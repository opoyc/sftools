#' Returns the classification of a GMID according to its value or volume
#'
#' @param series numeric vector
#'
#' @return string
#' @export
#' @author Obryan Poyser
#'
#' @examples
#' class_volval(AirPassengers)
class_volval <- function(series){
    df <- data.frame(index=1:length(series), series)
    df <- df[order(df[,"series"], decreasing = T),]
    df$cumsum <- cumsum(df$series)/sum(df$series)
    df$class <- ifelse(df$cumsum<=.8, "A", ifelse(df$cumsum>.8 & df$cumsum<=.95, "B", "C"))
    return(df[order(df$index),"class"])
}
