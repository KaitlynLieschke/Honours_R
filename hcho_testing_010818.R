# Contains code for:
# Importing HCHO data, time series, seasonal cycle, overall trends, trends for each month,
# trends for limited time periods, model comparison, satellite comparison

# Import raw HCHO data as .dat files
test <- read.table("/Users/kaitlynlieschke/Documents/GitHub/Honours_RStudio/asc_hcho1.dat", header = T)
test2 <- read.table("/Users/kaitlynlieschke/Documents/GitHub/Honours_RStudio/asc_hcho2.dat", header = T)
test <- rbind(test, test2)

test$hhmmss <- formatC(test$HHMMSS,width = 6,format = "d",flag = "0")
test$DateTime <- as.POSIXct(paste(test$YYYYMMDD, test$hhmmss), format="%Y%m%d %k%M%S")
test <-subset(test,select = c(DateTime,total_column,error,instrument))
#test$hhmmss <- formatC(test$HHMMSS,width = 6,format = "d",flag = "0")
#test = subset(test,select = c(YYYYMMDD,hhmmss,JULIAN,YEAR,total_column,error,instrument))
#test <- cbind(test$YYYYMMDD,hhmmss,test$JULIAN,test$YEAR,test$total_column,test$error,test$instrument)
#names(test) <- c("YYYYMMDD","HHMMSS")

# Save as csv then import csv
write.csv(test, "/Users/kaitlynlieschke/Documents/GitHub/Honours_RStudio/asc_hcho.csv",row.names = FALSE)
library(openair)
hcho_oe<-import(file = "/Users/kaitlynlieschke/Documents/GitHub/Honours_RStudio/asc_hcho.csv", file.type = "csv",
                sep = ",", header.at = 1, data.at = 2, date = "DateTime",
                date.format = "%Y-%m-%d %H:%M:%S",
                na.strings = c("", "NAN","-9999", "NA" ),
                correct.time = NULL)

# Limit dataset to midday hours (11am to 3pm)
midday_hcho_oe <- selectByDate(hcho_oe, hour=11:15)

# Calculate monthly averages using bottom ?? percentile of measurements
avg_midday_hcho_oe <- timeAverage(midday_hcho_oe,avg.time = "month",statistic = "percentile", percentile = 40)

# deseasonalise data using stl function (same as in Theilsen)
mydata <-avg_midday_hcho_oe
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

# One plot showing overall trend as a percentage after deseasonalisation, isolating bottom 10th percentile
TheilSen(midday_hcho_oe, pollutant = "total_column",deseason="TRUE",slope.percent="TRUE",fontsize=20,
         xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),main="HCHO at Wollongong",
         pch=".:",cex=0.5,data.col="royalblue",line.col="black",autocor = "TRUE",shade="WHITE",
         statistic = "percentile", percentile = 20)

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