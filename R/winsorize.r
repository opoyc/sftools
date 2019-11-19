#' Winsorizing time series data
#'
#' This function performs a signal winsorization. It takes a ts object or a numeric vector and returns cleansed values.
#' For series with more than 24 observation the function winsorizes the remainder of a STL decomposition.
#' @param time.series either a ts object or numeric vector
#'
#' @return ts object or numeric vector
#' @import dvmisc
#' @import DescTools
#' @export
#' @author Jillian Teo
#'
#' @examples \dontrun{
#' winsorize(AirPassengers)
#' }
winsorize = function(time.series, frequency=12){
  initial_class <- class(time.series)
  if (initial_class == "numeric") {
    time.series <- ts(time.series, frequency = frequency)
    }
  if (length(time.series) <=24) {
        # same as Kinaxis
        new.ts = time.series
        new.ts = Winsorize(new.ts, na.rm = TRUE)

        if (sum(new.ts, na.rm = T) == 0)
            {
                new.ts = time.series
            }  # if outlier method changes all values to zero, revert back to original ts
        new.ts[new.ts < 0] = 0  # Coerce negative values to zero
    }
  if (length(time.series) >24) {
    rstl = stl(time.series, s.window = "periodic", robust = T)
    resi = rstl$time.series[, 3]

    resi.new = resi
    win.resi = Winsorize(resi, na.rm = T)

    resi.new[resi.new < min(win.resi)] = min(win.resi)  #lower threshold
    resi.new[resi.new > max(win.resi)] = max(win.resi)  #upper threshold

    adjustment = resi.new - resi
    new.ts = time.series + adjustment

    if (sum(new.ts[!(is.na(new.ts))]) == 0){
    new.ts = time.series
    }  # if outlier method changes all values to zero, revert back to original ts
        new.ts[new.ts < 0] = 0  # Coerce negative values to zero
    }

  if (initial_class == "numeric") {
    out <- as.numeric(new.ts)
    return(out)
  } else {
    return(new.ts)
  }
}
