# Detect GMIDs seasonal
library(tidyverse)
library(forecast)
library(tsfeatures)
"%not.in%" <- Negate("%in%")


seasonality.func <- function(df, GMID) {
  lm.1 <- lm(Value ~ factor(month), data = df)
  p.vals <- summary(lm.1)$coefficients[, 4]
  p.vals.lt.01 <- as.numeric(sum(p.vals < .05, na.rm = T))
  if (p.vals.lt.01 > 1) {
    return(GMID)
  }
}

seasonality.func.Fbased <- function(df, GMID) {
  lm.1 <- lm(Value ~ factor(month), data = df)
  p.vals <- summary(lm.1)
  p.vals.lt <- pf(p.vals$fstatistic[1], p.vals$fstatistic[2], # Compute p-value from the F-statistics
    p.vals$fstatistic[3],
    lower.tail = FALSE
  ) # and degree of freedom

  # p.vals.lt.01=as.numeric(sum(p.vals.lt<.05,na.rm = T))

  if (p.vals.lt < 0.05 & !is.nan(p.vals.lt)) {
    return(GMID)
  }
}

seasonality.func.Fbased2 <- function(df, GMID) {
  lm.1 <- lm(Value ~ factor(month), data = df)
  p.vals <- summary(lm.1)
  p.vals.lt <- pf(p.vals$fstatistic[1], p.vals$fstatistic[2], # Compute p-value from the F-statistics
    p.vals$fstatistic[3],
    lower.tail = FALSE
  ) # and degree of freedom

  # p.vals.lt.01=as.numeric(sum(p.vals.lt<.05,na.rm = T))

  if (p.vals.lt < 0.05 & !is.nan(p.vals.lt)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}




detect_seas <- function(ts) {
  seasonality.func.Fbased2 <- function(df, GMID) {
    lm.1 <- lm(Value ~ factor(month), data = df)
    p.vals <- summary(lm.1)
    p.vals.lt <- pf(p.vals$fstatistic[1], p.vals$fstatistic[2], # Compute p-value from the F-statistics
      p.vals$fstatistic[3],
      lower.tail = FALSE
    ) # and degree of freedom

    # p.vals.lt.01=as.numeric(sum(p.vals.lt<.05,na.rm = T))

    if (p.vals.lt < 0.05 & !is.nan(p.vals.lt)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }


  # ts here corresponds to "x" in Forecast Main script
  Kruskal.seas <- F
  Seasonality.strength <- F
  ETS.seas <- F
  LM.seas <- F


  # tmp<-ts
  ################################## KRUSKAL TEST ##########################################
  ts1 <- ts %>%
    mutate(FcstItem = Item.c) %>%
    dplyr::select(FcstItem, StartDate, MSales) %>%
    mutate(StartDate = dmy(StartDate)) %>%
    spread("StartDate", "MSales") %>%
    t() %>%
    data.frame()

  ts <- data.frame(StartDate = ymd(rownames(ts1)[-1]), as.numeric(as.character(ts1$.[-1])))
  colnames(ts)[2] <- as.character(ts1$.[1])

  # Kruskal test
  s <- 12
  t <- nrow(ts)
  first <- (12 / (t * (t + 1)))

  gmid_name <- GMID.c
  names(ts) <- c("Startdate", "Value")

  temp <- ts %>%
    mutate(Month = month(.$Startdate)) %>%
    arrange(.$Value) %>%
    mutate(Range = rownames(.)) %>%
    arrange(Startdate) %>%
    mutate(Range = as.integer(Range)) %>%
    group_by(Month) %>%
    summarise(
      Sum_range_months = sum(Range),
      number_elements_month = n()
    ) %>%
    mutate(
      Sum_range_squared = (Sum_range_months^2),
      elem = (Sum_range_squared / number_elements_month)
    )

  second <- sum(temp$elem)

  third <- (3 * (t + 1))

  KW <- first * second - third

  table_value <- qchisq(.95, df = (s - 1)) # signification level alpha=5%

  if (abs(KW) > table_value) { # value of statistic greater than table value = p-value of the test <0.05
    Kruskal.seas <- TRUE
  }
  ######################################################################################

  ################################## Seasonality strength ##########################################

  # automatically detecting start point as the first data point !=0
  tmp <- ts %>% spread("Startdate", names(.)[2])

  starting_point <- names(tmp)[first(which((tmp != 0) == T))] # first position of those data points that are not 0 = first month with data !=0

  starting_point <- c(year(starting_point), month(starting_point))

  tmp <- tmp %>%
    gather("date", "value") %>%
    dplyr::select(value) %>%
    ts(frequency = 12, start = starting_point) %>%
    {
      . ->> tmp_acf
    } %>%
    tsfeatures() %>%
    mutate(Item = Item.c) %>%
    # select(Item,names(.)[1:(length(names(.))-1)]) %>% #old version without everything()
    dplyr::select(Item, everything()) %>%
    dplyr::select(Item, seasonal_strength) %>%
    data.frame() # in this case we only want seasonality strenght

  # ts_features_GMID<-ts_features_GMID %>% rbind(tmp)

  if (tmp$seasonal_strength > 0.5) { # Criteria: if seasonality strength > 0.5 consider seasonal
    Seasonality.strength <- T
  }
  ######################################################################################

  ################################## LM criteria ##########################################

  temporal <- ts %>%
    mutate(month = month(Startdate))


  GMID <- names(temporal)[2]

  names(temporal) <- c("Startdate", "Value", "month")

  # GMID_seasonal_lm<-c(GMID_seasonal_lm,seasonality.func(temporal,GMID = GMID)) #t-test

  if (seasonality.func.Fbased2(temporal, GMID = GMID)) {
    LM.seas <- T
  }

  ######################################################################################

  ################################## ETS criteria ##########################################

  model_ets <- ets(ts(temporal$Value, starting_point, frequency = 12)) # ets


  if (substring(model_ets$method, 9, 9) %in% c("A", "M")) { # fitting ets model.
    # If it detects seasonal model is because data has seasonal pattern (Hyndman)

    model_ets_2 <- ets(ts(temporal$Value, starting_point, frequency = 12), model = paste0(
      substring(model_ets$method, 5, 5),
      substring(model_ets$method, 7, 7),
      "N"
    ))

    deviance <- 2 * c(logLik(model_ets) - logLik(model_ets_2))
    df <- attributes(logLik(model_ets))$df - attributes(logLik(model_ets_2))$df
    # P value

    if ((1 - pchisq(deviance, df)) <= 0.05) {
      ETS.seas <- T
    }
  }
  ######################################################################################

  if (sum(Kruskal.seas, Seasonality.strength, ETS.seas) == 3) { # if the GMID pass through the 4 filters then it's considered as seasonal
    return(TRUE)
  } else {
    return(FALSE)
  }
}
