# Contains code for:
# Importing HCHO data, time series, seasonal cycle, overall trends, trends for each month,
# trends for limited time periods, model comparison, satellite comparison

# Import raw HCHO data as .dat files
test <- read.table("/Users/kaitlynlieschke/Desktop/UOW_HCHO/asc_hcho1.dat", header = T)
test2 <- read.table("/Users/kaitlynlieschke/Desktop/UOW_HCHO/asc_hcho2.dat", header = T)
test <- rbind(test, test2)
# Save as csv then import csv
write.csv(test, "/Users/kaitlynlieschke/Desktop/UOW_HCHO/asc_hcho.csv")
hcho_oe<-import(file = "/Users/kaitlynlieschke/Desktop/UOW_HCHO/asc_hcho.csv", file.type = "csv",
                sep = ",", header.at = 1, data.at = 2, date = "YYYYMMDD",
                date.format = "%Y%m%d", time = "HHMMSS", time.format = "%H%M%S",
                na.strings = c("", "NAN","-9999", "NA" ),
                correct.time = NULL)

# use 'openair' library
library(openair)

# deseasonalise data using stl function (same as in Theilsen)
mydata <-hcho_oe_month
############################ FROM THEILSEN CODE
## interpolate missing data
mydata[["total_column"]] <- approx(mydata[["total_column"]], 
                              n = length(mydata[["total_column"]]))$y
myts <- ts(mydata[["total_column"]], start = c(1996, 5),
           end = c(2015, 12), frequency = 12)
## key thing is to allow the seanonal cycle to vary, hence
## s.window should not be "periodic"; set quite high to avoid
## overly fitted seasonal cycle
## robustness also makes sense for sometimes noisy data
ssd <- stl(myts, s.window = 35, robust = TRUE, s.degree = 0)
deseas <- ssd$time.series[, "trend"] + ssd$time.series[, "remainder"]
deseas <- as.vector(deseas)
############################################

plot(deseas)
plot(ssd)
plot(ssd$time.series[,"remainder"])

# Assign instrument variable to deseasonalised data
# first 135 entries = 0 (Bomem from May 1996 to Jul 2007)
# last 101 entries = 1 (Bruker August 2007 to Dec 2015)
# 138th and 144th entries = NaN (both instruments were running Oct 2007 and Apr 2008)
instrument <- c(rep(0,135),rep(1,101))
instrument[138] = NaN
instrument[144] = NaN

deseas_months <- (1:236)/12
deseas_test = data.frame(deseas_months, deseas, instrument)
library(plyr)
deseas_test <- rename(deseas_test, c("deseas_months"="date", "deseas"="total_column","instrument"="instrument"))

# One plot showing overall trend as a percentage after deseasonalisation
TheilSen(hcho_oe_month, pollutant = "total_column",deseason="TRUE",slope.percent="FALSE",fontsize=20,
         xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),main="HCHO at Wollongong",
         pch=".:",cex=0.5,data.col="royalblue",line.col="black",autocor = "TRUE",shade="WHITE")

# Test for trend and significance of different variables using Sandy's code below
fit1 <- lm(deseas_test$total_column ~ deseas_test$date)
summary(fit1)
plot(fit1)

fit2 <- lm(deseas_test$total_column ~ deseas_test$date + deseas_test$instrument)
summary(fit2)
plot(fit2)

#  NEED TO DO A BETTER JOB OF DEFINING EVENT - wasnt there are bushfire etc...
deseas_test$event <- factor(ifelse(deseas_test$total_column > 3e16, 1, 0))
plot(deseas_test$date, deseas_test$total_column)
points(deseas_test$date[deseas_test$event==1],  deseas_test$total_column[deseas_test$event==1], col=2)

fit3 <- lm(deseas_test$total_column ~ deseas_test$date + deseas_test$instrument + deseas_test$event)
summary(fit3)
plot(fit3)