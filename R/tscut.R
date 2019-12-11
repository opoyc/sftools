#' Suggested starting date
#'
#' This function uses changing points in variance and mean to suggest a starting date.
#'
#' @param x numeric or ts object.
#' @param freq numeric. Set the frequency if the input is numeric, 12 by default.
#'
#' @import changepoint
#' @import forecast
#' @return ts object or numeric indexed as year 1
#' @export
#'
#' @examples
#' \dontrun{
#' tscut(AirPassengers)
#' }
tscut <- function(series, freq=12) { # argument can be a vector or ts object
  x_1 <- c(10, 100, 1000, 10000)
  y <- c(10, 4, 2, 1.5)
  fit <- lm(log(y) ~ log(x_1))
  p_1 <- fit$coefficients[2]
  p_2 <- exp(fit$coefficients[1])

  # if it is a numeric vector convert is as monthly time series

  if(is.ts(series)==FALSE){
    x <- ts(series, freq=freq)
  } else {
    x <- series
  }

  if (length(x) != 0) {
    check_count <- 0
    for (check in 1:length(x)) {
      if (x[check] == 0) {
        check_count <- check_count + 1
      }
      else {
        x <- subset(x, start = check_count + 1) # this is a forecast function that mask the base one
        break
      }
    }
    x_no_leading_zeros <- x

    if (length(x) >= 4) {
      dfu_break <- cpt.meanvar(x, penalty = "None", method = "AMOC", Q = 1)
      bp <- cpts(dfu_break)
      x_after <- subset(x, start = bp + 1)

      dfu_break_var <- cpt.var(x, penalty = "None", method = "AMOC", Q = 1)
      bp_var <- cpts(dfu_break_var)
      x_after_var <- subset(x, start = bp_var + 1)
      if (length(x_after) > 24) {
        min_mean <- min(mean(x[1:bp]), mean(x[bp:length(x)]))
        max_mean <- max(mean(x[1:bp]), mean(x[bp:length(x)]))
        threshold <- p_2 * min_mean^p_1
        if (max_mean / min_mean >= threshold) {
          x <- x_after
        } else {
          if (length(x_after_var) > 24) {
            min_iqr <- min(IQR(x[1:bp_var]), IQR(x[bp_var:length(x)])) + 0.0001
            max_iqr <- max(IQR(x[1:bp_var]), IQR(x[bp_var:length(x)]))
            if (max_iqr / min_iqr > 2) {
              x <- x_after_var
            } else {
              x <- x
            }
          } else {
            x <- x
          }
        }
        if (x[1] == 0) {
          x <- x_no_leading_zeros
        }
        return(x)
      } else {
        if (length(x_after_var) > 24) {
          min_iqr <- min(IQR(x[1:bp_var]), IQR(x[bp_var:length(x)])) + 0.0001
          max_iqr <- max(IQR(x[1:bp_var]), IQR(x[bp_var:length(x)]))
          if (max_iqr / min_iqr > 2) {
            x <- x_after_var
          } else {
            x <- x
          }
        } else {
          x <- x
        }
        if (x[1] == 0) {
          x <- x_no_leading_zeros
        }
        return(x)
      }
    } else {
      x <- x
      return(x)
    }
  }
}
