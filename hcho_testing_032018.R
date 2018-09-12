# Contains code for:
# Importing HCHO data and anaysing step change presence

# Import raw HCHO data as .dat files adn combine into one object
test <- read.table("/Users/kaitlynlieschke/Documents/GitHub/Honours_RStudio/asc_hcho1.dat", header = T)
test2 <- read.table("/Users/kaitlynlieschke/Documents/GitHub/Honours_RStudio/asc_hcho2.dat", header = T)
test <- rbind(test, test2)

# Format date and time, and remove irrelevant columns
test$hhmmss <- formatC(test$HHMMSS,width = 6,format = "d",flag = "0")
test$DateTime <- as.POSIXct(paste(test$YYYYMMDD, test$hhmmss), format="%Y%m%d %k%M%S")
totest <-subset(test,select = c(DateTime,total_column))

# Save as csv and import using openair
write.csv(totest, "/Users/kaitlynlieschke/Documents/GitHub/Honours_RStudio/asc_hcho_totest.csv",row.names = FALSE)
library(openair)
hcho_oe<-import(file = "/Users/kaitlynlieschke/Documents/GitHub/Honours_RStudio/asc_hcho_totest.csv", file.type = "csv",
                sep = ",", header.at = 1, data.at = 2, date = "DateTime",
                date.format = "%Y-%m-%d %H:%M:%S",
                na.strings = c("", "NAN","-9999", "NA" ),
                correct.time = NULL)

# Limit dataset to midday hours (11am to 3pm)
midday_totest <- selectByDate(hcho_oe, hour=11:15)

# Plot number of observations in every month
timePlot(midday_totest,pollutant = "total_column", avg.time = "month",statistic = "frequency",
         ylab="number of midday observations", plot.type = "h",ylim=c(0,5))
timePlot(hcho_oe,pollutant = "total_column", avg.time = "month",statistic = "frequency",
         ylab="number of observations", plot.type = "h",ylim=c(0,5))

#####---------------------------------------------------------------------------#####

# Randomly sample 5 obs within each month to use for creating monthly means/medians
# create for loop where you select all values within a YYYY-MM, randomly select 5 obs
# and save them to a new array and do this for all YYYY-MMs

# create new dataframe to hold only selected values
bootstrappedmeans = data.frame(matrix(NA,nrow=10000,ncol=152))
bootstrappedmedians = data.frame(matrix(NA,nrow=10000,ncol=152))
avgbtstmeans = data.frame(matrix(NA,nrow=152,ncol=2))
avgbtstmedians = data.frame(matrix(NA,nrow=152,ncol=2))
nummonth=1
# loop over years and months
for (singleyear in 1996:2016){
  syyyy = sprintf("%04d", singleyear)
  
  for (singlemonth in 1:12){
    smm = sprintf("%02d", singlemonth)
    
    if (singlemonth == 12){
      esyyyy=sprintf("%04d", (singleyear+1))
      esmm = sprintf("%02d", 1)
    } else {
      esyyyy=syyyy
      esmm = sprintf("%02d", (singlemonth+1))
    }
    
    start.date<- as.POSIXct(paste(syyyy,smm,"01 00:00",sep="-"))
    end.date<- as.POSIXct(paste(esyyyy,esmm,"01 00:00",sep="-"))
    submydata<-subset(mydata,date>= start.date & date <= end.date)
    
    #count number of observations within each month, only keep observations if more than
    #5 exist within the month
    if (NROW(na.omit(submydata)) >= 5){
      #monthmedian = boot(submydata,median,R=10000)#### ADD MORE HERE, calculate based on five samples each time
      for (nboot in 1:10000){
        obsindex=runif(5,min=0,max=1)
        obsindex=obsindex*NROW(na.omit(submydata))
        obsindex=round(obsindex,digits=0)
        
        monthmedian=median(submydata[obsindex,2])
        monthmean=mean(submydata[obsindex,2])
        colnames(bootstrappedmedians)[nummonth]<-paste(syyyy,smm,"01",sep="-")
        colnames(bootstrappedmeans)[nummonth]<-paste(syyyy,smm,"01",sep="-")
        avgbtstmedians[nummonth,1]<-paste(syyyy,smm,"01",sep="-")
        avgbtstmeans[nummonth,1]<-paste(syyyy,smm,"01",sep="-")
        bootstrappedmedians[nboot,nummonth]=monthmedian
        bootstrappedmeans[nboot,nummonth]=monthmean
      }
      nummonth=nummonth+1
      
    } 
    
  }
  
}  

#avgbtstmeans<-bootstrappedmeans[0,]
#avgbtstmedians<-bootstrappedmedians[0,]
for (zz in 1:152){
  avgbtstmeans[zz,2]=mean(bootstrappedmeans[[zz]])
  avgbtstmedians[zz,2]=mean(bootstrappedmedians[[zz]])
}
colnames(avgbtstmeans)<-c("Date","total_column")
colnames(avgbtstmedians)<-c("Date","total_column")
write.csv(avgbtstmedians, "/Users/kaitlynlieschke/Documents/GitHub/Honours_RStudio/monthavg_btstmedian_hcho.csv",row.names = FALSE)
write.csv(avgbtstmeans, "/Users/kaitlynlieschke/Documents/GitHub/Honours_RStudio/monthavg_btstmean_hcho.csv",row.names = FALSE)
hcho_btstmedian<-import(file = "/Users/kaitlynlieschke/Documents/GitHub/Honours_RStudio/monthavg_btstmedian_hcho.csv", file.type = "csv",
                sep = ",", header.at = 1, data.at = 2, date = "Date",
                date.format = "%Y-%m-%d", na.strings = c("", "NAN","-9999", "NA" ))

TheilSen(hcho_btstmean,pollutant="total_column",deseason="TRUE",slope.percent="TRUE",#type="month",
         fontsize=20,xlab='Year',ylab=expression(HCHO ~ (molecs.cm^{-2})),lty=1,pch=20,
         main="HCHO at Wollongong (midday only, bootstrapped mean)",data.col="black",line.col="blue",lwd=4,cex=0.05)
timePlot(hcho_btstmean,pollutant="total_column",cols="black",main="HCHO at Wollongong (midday only, bootstrapped mean)")

##COMPARE TO ORIGINAL DATASET ###
TheilSen(hcho_oe,pollutant="total_column",deseason="TRUE",slope.percent="TRUE",#type="month",
         fontsize=20,xlab='Year',ylab=expression(HCHO ~ (molecs.cm^{-2})),lty=1,pch=20,
         main="HCHO at Wollongong (original dataset)",data.col="black",line.col="blue",lwd=4,cex=0.05)
timePlot(hcho_oe,pollutant="total_column",cols="black",main="HCHO at Wollongong (original dataset)",avg.time="month")

# Use boostrapping to calculate median for each month with more than 5 observations
# (as selected above)

#####---------------------------------------------------------------------------#####

# deseasonalise data using stl function (same as in Theilsen)
mydata <-midday_totest
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