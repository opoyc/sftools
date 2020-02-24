#' Winsorizing time series data
#'
#' This function performs a signal winsorization. It takes a ts object or a numeric vector and returns cleansed values.
#' For series with more than 24 observation the function winsorizes the remainder of a STL decomposition.
#'
#' @param time_series ts object.
#' @param frequency numeric. frequency of the time series
#'
#' @return ts object or numeric vector
#' @import dvmisc
#' @importFrom DescTools Winsorize
#' @export
#' @author Jillian Teo
#'
#' @examples \dontrun{
#' winsorize(AirPassengers)
#' }
winsorize = function(time_series, frequency=12){
  initial_class <- class(time_series)
  if (initial_class == "numeric") {
    time_series <- ts(time_series, frequency = frequency)
    }
  if (length(time_series) <=24) {
        # same as Kinaxis
        new_ts = time_series
        new_ts = Winsorize(new_ts, na.rm = TRUE)

        if (sum(new_ts, na.rm = T) == 0)
            {
                new_ts = time_series
            }  # if outlier method changes all values to zero, revert back to original ts
        new_ts[new_ts < 0] = 0  # Coerce negative values to zero
    }
  if (length(time_series) >24) {
    rstl = stl(time_series, s.window = "periodic", robust = T)
    resi = rstl$time.series[, 3]

    resi_new = resi
    win_resi = Winsorize(resi, na.rm = T)

    resi_new[resi_new < min(win_resi)] = min(win_resi)  #lower threshold
    resi_new[resi_new > max(win_resi)] = max(win_resi)  #upper threshold

    adjustment = resi_new - resi
    new_ts = time_series + adjustment

    if (sum(new_ts[!(is.na(new_ts))]) == 0){
    new_ts = time_series
    }  # if outlier method changes all values to zero, revert back to original ts
        new_ts[new_ts < 0] = 0  # Coerce negative values to zero
    }

  if (initial_class == "numeric") {
    out <- as.numeric(new_ts)
    return(out)
  } else {
    return(new_ts)
  }
}
