#' Cross-validation for time series data
#'
#' @param ts_data tibble, dataframe or matrix. It is assumed that it is ordered by date.
#' @param cv_size numeric. Cross validation size.
#' @param lag numeric. How many periods ahead to forecast.
#'
#' @return
#' @importFrom purrr map_df
#' @importFrom dplyr tibble
#' @export
#'
#' @examples
cvts <- function(ts_data, cv_size, lag){
  # define starting and ending train size
  start_train <- (nrow(ts_data)-nrow(test)-lag)
  stop_train <- start_train+cv_size-1

  # iterate over to create tupples of train and test
  map_df(start_train:stop_train
         , .f = function(i){
           tibble(train = list(ts_data[1:i,])
                  , test = list(ts_data[i+lag,])
           )}
  )
}
