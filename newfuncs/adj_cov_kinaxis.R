
# Calculating AdjCoV Kinaxis way


# if the Fcst Item has data rule = stl error, then calculate Adj CoV as
# stdev(stl error)/mean(ts), last year if doesn't, use normal Adj CoV
# with raw data

# x is a table containing, (only per 1 GMID), SKU, DATA (outlier),
# SALESQTY, STaRTDATE

adj_cov_kinaxis <- function(x) {
    cat("Doing GMID:", unique(x$SKU), "\n")
    x.1 <- x %>% filter(SALESQTY != 0) %>% group_by(SKU) %>% summarise(Actual.start = min(STARTDATE))
    x <- merge(x, x.1, by = c("SKU")) %>% filter(!(STARTDATE < Actual.start)) %>% 
        select(-Actual.start) %>% arrange(SKU, STARTDATE)
    x <- x %>% group_by(SKU) %>% mutate(n = n()) %>% filter(n >= 12) %>% 
        select(-n) %>% ungroup
    x <- transmute(x, SKU, outlier_data = DATA, sales = SALESQTY, date = STARTDATE) %>% 
        mutate(l = n())  # length fo time series
    x_lasty <- x %>% filter(date >= (max(.$date) - months(11)))
    l <- length(x$sales)
    intermittent <- sum(x$sales <= 0)/l
    if (l == 0) 
        return("NA")
    temp_ts <- (ts(x$sales, start = c(year(min(x$date)), month(min(x$date))), 
        frequency = 12))
    temp_ts2 <- temp_ts[(length(temp_ts) - 11):length(temp_ts)]
    CoV <- round(sd(temp_ts2)/mean(temp_ts2), 2)
    if (unique(x$outlier_data) == "Rstl Error") {
        # calculating stl decomposition
        # rstl = stlplus(temp_ts, n.p = 12, s.window = 'periodic')  
        if (length(temp_ts) <= 24) {
            stdev <- sd(temp_ts)
            adjCoV <- CoV
        } else {
            rstl <- stats::stl(temp_ts, robust = T, s.window = "periodic")
            # resi <- rstl$data[,'remainder']
            resi <- rstl$time.series[, 3]
            resi <- resi[(length(resi) - 11):length(resi)]  #errors only last 12 months, calculated with all the TS
            stdev <- sd(resi)
            stdev.pop <- sd(resi) * sqrt((12 - 1)/12)
            avg.ts <- mean(temp_ts2)
            fake.avg.ts.kinaxis <- sum(temp_ts2)  #kinaxis is not calculating the avg but the aggregation!!! 
            adjCoV <- round(stdev.pop/avg.ts, 2)
        }
    } else {
        stdev <- sd(temp_ts)
        adjCoV <- CoV
    }
    return(data.frame(intermittent = round(intermittent, 2), Average1 = mean(temp_ts), 
        stdDeviation1 = sd(temp_ts), stdDeviation1.adj = stdev, adjCoV = adjCoV, 
        CoV = CoV))
}