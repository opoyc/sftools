library(pracma)
library(DescTools)
library(dvmisc)

Lag = 4


outlier_cleansing = function(time_series , outlier_method, data_rule , mov_avg_n, threshold_val, causal_factor = T, lag = 4){

  if(class(time_series)!="ts"){
    time_series <- ts(time_series, start = 1, frequency = 12)
  }

  if(outlier_method == "Standard Deviation"){ # Same as KNX
    if(data_rule == "Historical"){

      new_ts <-  time_series

      avg <- mean(new_ts)
      s_d <- sd(new_ts)

      new_ts[new_ts < avg - threshold_val * s_d] = avg - threshold_val * s_d # lower threshold
      new_ts[new_ts > avg + threshold_val * s_d] = avg + threshold_val * s_d # upper threshold

      if(sum(new_ts) == 0){ new_ts = time_series}    # if outlier method changes all values to zero, revert back to original ts
      new_ts <- pmax(0, new_ts)                       # Coerce negative values to zero

      return(
        list(new_ts = new_ts
             , method = "Standard Deviation"
             , data_rule = "Historical"
             , upper_threshold =  replace(x = time_series, list = 1:length(time_series), values = avg + threshold_val*s_d)
             , lower_threshold =  replace(x = time_series, list = 1:length(time_series), values = avg - threshold_val*s_d))
        )
      }

    if(data_rule == "Moving Average Error"){                          # ok

      new_ts = time_series

      ts_ma = movavg(new_ts, n = mov_avg_n, type = "s")

      resi <- new_ts - ts_ma
      avg <- mean(resi)
      s_d <- sd(resi)
      resi_new <- resi
      resi_new[resi_new < (avg-threshold_val * sd(resi))] = (avg - threshold_val * sd(resi))   # lower threshold
      resi_new[resi_new > (avg+threshold_val * sd(resi))] = (avg + threshold_val * sd(resi))   # upper threshold

      adjustment = resi_new - resi

      new_ts = time_series + adjustment
      if(sum(new_ts) == 0){new_ts =time_series }    # if outlier method changes all values to zero, revert back to original ts
      new_ts = pmax(0,new_ts)                       # Coerce negative values to zero

      return(list(new_ts = new_ts,
                  method = "SD",
                  data_rule = "Moving average",
                  upper_threshold = (time_series- resi + threshold_val * s_d),
                  lower_threshold = (time_series- resi - threshold_val * s_d))
      )
    }

    if(data_rule == "Rstl Error"  &  causal_factor ){                          # ok

      rstl = stlplus(time_series,s.window = "periodic",n_p = 12)
      resi = rstl$data[,"remainder"]

      avg = mean(resi[!(is_na(resi))])
      s_d = sd(resi[!(is_na(resi))])
      resi_new = resi
      resi_new[resi_new<   (avg-threshold_val * sd( resi)) ] = (avg-threshold_val * s_d) # lower threshold
      resi_new[resi_new>   (avg+threshold_val * sd( resi)) ] = (avg+threshold_val * s_d) # upper threshold

      adjustment =resi_new -resi
      new_ts = time_series + adjustment

      if(sum(new_ts[!is_na(new_ts)]) == 0){new_ts =time_series }    # if outlier method changes all values to zero, revert back to original ts
      new_ts = pmax(0,new_ts)                       # Coerce negative values to zero

      return(list(new_ts = new_ts,
                  method = "Standard Deviation",
                  data_rule = "Rstl Error",
                  upper_threshold = (time_series-  rstl$data[,"remainder"]+ threshold_val * s_d) ,
                  lower_threshold = (time_series-  rstl$data[,"remainder"]- threshold_val * s_d))
      )
    }

    if(data_rule == "Rstl Error" & causal_factor == F){                          # use stl for ts with causal factor

      rstl = stl(time_series,s.window = "periodic",robust = T)
      resi = rstl$time_series[,3]

      avg = mean(resi)
      s_d = sd(resi)
      resi_new = resi
      resi_new[resi_new<   (avg-threshold_val * sd( resi)) ] = (avg-threshold_val * s_d) # lower threshold
      resi_new[resi_new>   (avg+threshold_val * sd( resi)) ] = (avg+threshold_val * s_d) # upper threshold

      adjustment =resi_new -resi
      new_ts = time_series + adjustment

      if(sum(new_ts) == 0){new_ts =time_series }    # if outlier method changes all values to zero, revert back to original ts
      new_ts = pmax(0,new_ts)                       # Coerce negative values to zero

      return(list(new_ts = new_ts,
                  method = "Standard Deviation",
                  data_rule = "Rstl Error",
                  upper_threshold = (time_series-  rstl$time_series[,3]+ threshold_val * s_d) ,
                  lower_threshold = (time_series-  rstl$time_series[,3]- threshold_val * s_d))
      )
    }

  }

  if(outlier_method == "Iglewicz Hoaglin Method"){

    if(data_rule == "Historical"){                      # ok
      dist_lower = median(time_series) - threshold_val*(mad(time_series,constant = 1)/0.6745)
      dist_upper = median(time_series) + threshold_val*(mad(time_series,constant = 1)/0.6745)

      new_ts <- time_series
      new_ts[time_series < dist_lower] <- dist_lower
      new_ts[time_series > dist_upper] <- dist_upper

      if(sum(new_ts) == 0){new_ts =time_series }    # if outlier method changes all values to zero, revert back to original ts
      new_ts = pmax(0, new_ts)                       # Coerce negative values to zero

      return(
        list(new_ts = new_ts
             , method = "Iglewicz Hoaglin Method"
             , data_rule = "Historical"
             , upper_threshold = replace(x =time_series,list = 1:length(time_series), values = dist_lower)
             , lower_threshold = replace(x =time_series,list = 1:length(time_series), values = dist_upper))
        )
    }

    if(data_rule == "Moving Average Error"){                          # ok

      ts_ma <- movavg(time_series, n = mov_avg_n, type = "s")
      resi <- time_series - ts_ma
      resi_new <- resi

      dist_lower =  - threshold_val * (mad(resi, constant = 1)/0.6745) # see "-"
      dist_upper =    threshold_val * (mad(resi, constant = 1)/0.6745)

      resi_new[resi_new < dist_lower ] =  dist_lower  # lower threshold
      resi_new[resi_new > dist_upper ] =  dist_upper   # upper threshold

      adjustment = resi_new - resi
      new_ts = time_series + adjustment
      if(sum(new_ts) == 0){ new_ts = time_series }    # if outlier method changes all values to zero, revert back to original ts
      new_ts <- pmax(0, new_ts)                       # Coerce negative values to zero

      return(list(new_ts = new_ts,
                  method = "Iglewicz Hoaglin Method",
                  data_rule = "Moving Average Error",
                  upper_threshold = time_series - resi + dist_upper,
                  lower_threshold = time_series - resi + dist_lower)
      )
    }

    if(data_rule == "Rstl Error" &  causal_factor){                          # ok

      rstl = stlplus(time_series,s.window = "periodic",robust = T)
      resi = rstl$data[,"remainder"]

      resi_new = resi

      dist_lower =  - threshold_val*(mad(resi[!is_na(resi)],constant = 1)/0.6745)
      dist_upper =    threshold_val*(mad(resi[!is_na(resi)],constant = 1)/0.6745)

      resi_new[resi_new< dist_lower ] = dist_lower  #lower threshold
      resi_new[resi_new> dist_upper ] = dist_upper  #upper threshold

      adjustment =resi_new -resi
      new_ts = time_series + adjustment

      if(sum(new_ts[!is_na(new_ts)]) == 0){new_ts =time_series }    # if outlier method changes all values to zero, revert back to original ts
      new_ts = pmax(0,new_ts)                       # Coerce negative values to zero

      lower = time_series - rstl$data[,"remainder"] + dist_lower
      upper = time_series - rstl$data[,"remainder"] + dist_upper

      return(list(new_ts = new_ts,
                  method = "Iglewicz Hoaglin Method",
                  data_rule = "Rstl Error",
                  upper_threshold = upper,
                  lower_threshold = lower)
      )
    }

    if(data_rule == "Rstl Error" & causal_factor == F){

      rstl = stl(time_series,s.window = "periodic",robust = T)
      resi = rstl$time_series[,3]

      resi_new = resi

      dist_lower =  - threshold_val*(mad(resi,constant = 1)/0.6745)
      dist_upper =    threshold_val*(mad(resi,constant = 1)/0.6745)

      resi_new[resi_new< dist_lower ] = dist_lower  #lower threshold
      resi_new[resi_new> dist_upper ] = dist_upper  #upper threshold

      adjustment =resi_new -resi
      new_ts = time_series + adjustment

      if(sum(new_ts[!is_na(new_ts)]) == 0){new_ts =time_series }    # if outlier method changes all values to zero, revert back to original ts
      new_ts = pmax(0,new_ts)                       # Coerce negative values to zero

      lower = time_series - rstl$time_series[,3] + dist_lower
      upper = time_series - rstl$time_series[,3] + dist_upper

      return(list(new_ts = new_ts,
                  method = "Iglewicz Hoaglin Method",
                  data_rule = "Rstl Error",
                  upper_threshold = upper,
                  lower_threshold = lower)
      )
    }


  }

  if(outlier_method == "Winsorizing"){

    if(data_rule == "Historical"){                                        # same as Kinaxis

      new_ts = time_series
      new_ts = Winsorize(new_ts, probs=c(threshold_val, 1 - threshold_val), na.rm = TRUE)

      if(sum(new_ts, na.rm = T) == 0){new_ts = time_series}    # if outlier method changes all values to zero, revert back to original ts
      new_ts = pmax(0, new_ts)                       # Coerce negative values to zero

      return(list(new_ts = new_ts,
                  method = "Winzorize",
                  data_rule = "Hist",
                  upper_threshold = replace(x =time_series,list = 1:length(time_series), values = max(new_ts)),
                  lower_threshold = replace(x =time_series,list = 1:length(time_series), values = min(new_ts)))
             )
    }

    if(data_rule == "Moving Average Error"){                              # same as Kinaxis

      ts_ma = movavg(time_series, n = mov_avg_n , type = "s")
      resi  = time_series - ts_ma
      resi_new = resi

      win_resi = Winsorize(resi, probs=c(threshold_val,1-threshold_val))

      resi_new[resi_new< min(win_resi) ] = min(win_resi)  #lower threshold
      resi_new[resi_new> max(win_resi) ] = max(win_resi)  #upper threshold

      adjustment =resi_new - resi
      new_ts = time_series + adjustment

      if(sum(new_ts) == 0){new_ts = time_series }    # if outlier method changes all values to zero, revert back to original ts
      new_ts = pmax(0, new_ts)                       # Coerce negative values to zero

      return(list(new_ts = new_ts,
                  method = "Winsorizing",
                  data_rule = "Moving Average Error",
                  upper_threshold = time_series - resi + max(win_resi),
                  lower_threshold = time_series - resi + min(win_resi))
      )
    }

    if(data_rule == "Rstl Error" & causal_factor == T){
      #This is not correct_ normal STL should be applied if there's no regressor attached to that GMID_
      stl_try <- try(stlplus(time_series, n.p = 12, s.window = "periodic"), silent = TRUE)

      if(class(stl_try) != "try-error"){
        rstl = stlplus(time_series,s.window = "periodic",n_p = 12)
        resi = rstl$data[,"remainder"]

        resi_new = resi
        win_resi = Winsorize(resi, probs=c(threshold_val, 1-threshold_val), na.rm = T )

        resi_new[resi_new < min(win_resi)] = min(win_resi)  #lower threshold
        resi_new[resi_new > max(win_resi)] = max(win_resi)  #upper threshold

        adjustment =resi_new -resi
        new_ts = time_series + adjustment

        if(sum(new_ts[!(is_na(new_ts))]) == 0){new_ts =time_series }    # if outlier method changes all values to zero, revert back to original ts
        new_ts = pmax(0,new_ts)                       # Coerce negative values to zero

        lower = time_series - rstl$data[,"remainder"] + min(win_resi, na.rm = T)
        upper = time_series - rstl$data[,"remainder"] + max(win_resi, na.rm = T)

        return(list(new_ts = new_ts,
                    method = "Winsorizing",
                    data_rule = "Rstl Error",
                    upper_threshold = upper,
                    lower_threshold = lower)
        )

      } else {

        new_ts = time_series
        new_ts = Winsorize(new_ts, probs=c(threshold_val, 1-threshold_val), na.rm = TRUE)

        if(sum(new_ts, na.rm = TRUE) == 0){new_ts = time_series}    # if outlier method changes all values to zero, revert back to original ts
        new_ts = pmax(0, new_ts)

        return(list(new_ts = new_ts,
                    method = "Winzorize",
                    data_rule = "Hist",
                    upper_threshold = replace(x =time_series,list = 1:length(time_series), values = max(new_ts, na.rm = T)),
                    lower_threshold = replace(x =time_series,list = 1:length(time_series), values = min(new_ts, na.rm = T))
        )
        )
      }

    }

    if(data_rule == "Rstl Error" & causal_factor == F){

      rstl = stl(time_series, s.window = "periodic", robust = T)
      resi = rstl$time_series[,3]

      resi_new = resi
      win_resi = Winsorize(resi, probs=c(threshold_val, 1 - threshold_val),na.rm = T )

      resi_new[resi_new< min(win_resi) ] = min(win_resi)  #lower threshold
      resi_new[resi_new> max(win_resi) ] = max(win_resi)  #upper threshold

      adjustment =resi_new -resi
      new_ts = time_series + adjustment

      if(sum(new_ts[!(is_na(new_ts))]) == 0){new_ts =time_series }    # if outlier method changes all values to zero, revert back to original ts
      new_ts = pmax(0,new_ts)                       # Coerce negative values to zero

      lower = time_series - rstl$time_series[,3] + min(win_resi,na.rm = T)
      upper = time_series - rstl$time_series[,3] + max(win_resi,na.rm = T)

      return(list(new_ts = new_ts,
                  method = "Winsorizing",
                  data_rule = "Rstl Error",
                  upper_threshold = upper,
                  lower_threshold = lower)
      )
    }
  }


  if(outlier_method == "IQR"){

    new_ts = time_series
    qnt  = quantile(new_ts, probs=c(0.25, 0.75))
    caps = quantile(new_ts, probs=c(0.05, 0.95))
    H = 1.5*IQR(new_ts)
    new_ts[new_ts<(qnt[1]-H)] = caps[1]
    new_ts[new_ts>(qnt[2]+H)] = caps[2]

    if(sum(new_ts) == 0){new_ts =time_series }    # if outlier method changes all values to zero, revert back to original ts
    new_ts = pmax(0,new_ts)                       # Coerce negative values to zero

    return(list(new_ts = new_ts,
                method = "IQR",
                upper_threshold = qnt[2]+H,
                lower_threshold = qnt[1]-H)
    )

  }

  warning("No method applied, original time series is returned_")


  return(list(new_ts = time_series))

}
