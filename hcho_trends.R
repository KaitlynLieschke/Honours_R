# Contains code for:
# Importing HCHO data, time series, seasonal cycle, overall trends, trends for each month,
# trends for limited time periods, model comparison, satellite comparison

# Import data (Dropbox/2016/UOW/HCHO\ Research\ Assistantship/FTIR\ data/woll_hcho_oe.csv)
# Optimal estimation retrieval of HCHO from Wollongong FTIR from 05/96 - 12/15
hcho_oe<-import(file = file.choose(), file.type = "csv",
                            sep = ",", header.at = 1, data.at = 2, date = "DATE",
                            date.format = "%Y-%m-%d", time = "TIME", time.format = "%H:%M:%S",
                            na.strings = c("", "NAN","-9999", "NA" ),
                            correct.time = NULL)

#Time Plot
timePlot(hcho_oe, pollutant = "HCHO", group=TRUE,plot.type="p",pch=20,
         cex=0.25,ylab=expression(HCHO ~ (molecs.cm^{-2})),
         fontsize=20,ylim=c(0,2e17),col="blue",key="FALSE")
#per month
timePlot(hcho_oe, pollutant = "HCHO", group=TRUE,plot.type="p",pch=20,
         cex=0.25,ylab=expression(HCHO ~ (molecs.cm^{-2})),
         fontsize=20,ylim=c(0,2e17),col="blue",key="FALSE",type="month")

# Seasonal cycle plot
MyOutput<-timeVariation(hcho_oe,pollutant="HCHO",ylim=c(3e15,3e16),cols="blue", fontsize=20,
                        ylab=expression(HCHO ~ (molecs.cm^{-2})))
plot(MyOutput,subset="month",fontsize=20,key="FALSE",xlab="Month")

# One plot showing overall trend as a percentage after deseasonalisation
TheilSen(hcho_oe,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",fontsize=20,
         xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),main="HCHO at Wollongong",
         pch=".:",cex=0.5,data.col="royalblue",line.col="black",autocor = "TRUE",shade="WHITE")

# One plot per month showing change as a percentage
# No autocorrelation
TheilSen(hcho_oe,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",type="month",
         fontsize=20,xlab='Year',ylab=expression(HCHO ~ (molecs.cm^{-2})),lty=1,pch=20,
         main="HCHO at Wollongong",data.col="black",line.col="blue",lwd=4,cex=0.05)
#Autocorrelation
TheilSen(hcho_oe,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",type="month",
         fontsize=20,xlab='Year',ylab=expression(HCHO ~ (molecs.cm^{-2})),lty=1,pch=20,
         main="HCHO at Wollongong (autocor)",data.col="black",line.col="blue",lwd=4,
         cex=0.05,autocor="TRUE")

#BAR PLOT OF MONTHLY TRENDS
hcho_mon_trends <- data.frame(
  months = c("January","February","March","April","May","June","July","August","September","October","November","December"))
#No Autocorrelation
hcho_mon_trends$trends <- c(-2.11,-1.67,-2.02,-2.28,-2.08,-2.36,-2.61,-1.94,-1.68,-1.20,-0.13,-2.45)
hcho_mon_trends$poserr <- c(-1.47,-1.19,-1.54,-2.00,-1.31,-2.10,-2.41,-1.69,-1.29,-0.85,0.34,-1.96)
hcho_mon_trends$negerr <- c(-3.05,-2.09,-2.48,-2.54,-2.49,-2.64,-2.80,-2.15,-1.92,-1.56,-0.66,-2.86)
#Autocorrelation
hcho_mon_trends$trends <- c(-2.11,-1.67,-2.02,-2.28,-2.08,-2.36,-2.61,-1.94,-1.68,-1.20,-0.13,-2.45)
hcho_mon_trends$poserr <- c(-0.63,-0.28,-1.12,-1.24,-0.21,-1.53,-2.14,-1.24,-0.58,-0.45,1.24,-1.45)
hcho_mon_trends$negerr <- c(-4.21,-2.64,-3.10,-3.09,-2.78,-3.06,-3.20,-2.48,-2.32,-2.10,-1.69,-3.82)

ggplot(hcho_mon_trends, aes(x = months, y = trends), y) +  
  geom_bar(position = position_dodge(width = ), stat = "identity", fill="blue") +
  scale_x_discrete(limits=c("January","February","March","April","May","June","July","August","September","October","November","December")) +
  geom_errorbar(aes(ymin=negerr, ymax=poserr), position=position_dodge(width=0.9), width=0.25) +
  xlab("Month") + ylab("HCHO Trend (%/yr)") +
  ggtitle("HCHO Trends at Wollongong with 95% confidence intervals") + # plot title
  theme_bw() + # remove grey background (because Tufte said so)
  theme(panel.grid.major = element_blank()) # remove x and y major grid lines (because Tufte said so)

# Overall trend from 2000-2015
start.date<- as.POSIXct("2000-01-01 00:00", tz= "GMT")
end.date<- as.POSIXct("2015-12-31 23:59", tz = "GMT")
sub_hcho_oe<-subset(hcho_oe,date>= start.date & date <= end.date)
TheilSen(sub_hcho_oe,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",fontsize=20,
         xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),main="HCHO at Wollongong (oe_autocor) >2000",
         pch=20,cex=0.5,data.col="black",line.col="blue",autocor = "TRUE")
#Monthly trend from 2000-2015 (no autocorrelation)
TheilSen(sub_hcho_oe,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",type="month",
         fontsize=20,xlab='Year',ylab=expression(HCHO ~ (molecs.cm^{-2})),lty=1,pch=20,
         main="HCHO at Wollongong (no autocor)",data.col="black",line.col="blue",lwd=4,
         cex=0.05,autocor="FALSE")

# Overall trend from 1999-2015
start.date<- as.POSIXct("1999-01-01 00:00", tz= "GMT")
end.date<- as.POSIXct("2015-12-31 23:59", tz = "GMT")
sub_hcho_oe<-subset(hcho_oe,date>= start.date & date <= end.date)
TheilSen(sub_hcho_oe,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",fontsize=20,
         xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),main="HCHO at Wollongong (oe_autocor) >2000",
         pch=20,cex=0.5,data.col="black",line.col="blue",autocor = "TRUE")
#Monthly trend from 2000-2015 (no autocorrelation)
TheilSen(sub_hcho_oe,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",type="month",
         fontsize=20,xlab='Year',ylab=expression(HCHO ~ (molecs.cm^{-2})),lty=1,pch=20,
         main="HCHO at Wollongong (no autocor)",data.col="black",line.col="blue",lwd=4,
         cex=0.05,autocor="FALSE")

# Overall trend from 2004-2015
start.date<- as.POSIXct("2004-01-01 00:00", tz= "GMT")
end.date<- as.POSIXct("2015-12-31 23:00", tz = "GMT")
sub_hcho_oe<-subset(hcho_oe,date>= start.date & date <= end.date)
TheilSen(sub_hcho_oe,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",fontsize=20,
         xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),main="HCHO at Wollongong (oe_autocor) 2004-2015",
         pch=":.",cex=0.5,data.col="black",line.col="blue",autocor = "TRUE")
#Monthly trend from 2004-2015 (no autocorrelation)
TheilSen(sub_hcho_oe,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",type="month",
         fontsize=20,xlab='Year',ylab=expression(HCHO ~ (molecs.cm^{-2})),lty=1,pch=20,
         main="HCHO at Wollongong (no autocor)",data.col="black",line.col="blue",lwd=4,
         cex=0.05,autocor="FALSE")

# Overall trend from 2003-2013
start.date<- as.POSIXct("2003-02-01 00:00", tz= "GMT")
end.date<- as.POSIXct("2013-09-30 23:00", tz = "GMT")
sub_hcho_oe<-subset(hcho_oe,date>= start.date & date <= end.date)
TheilSen(sub_hcho_oe,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",fontsize=20,
         xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),main="HCHO at Wollongong (oe_autocor) 2003-2013",
         pch=20,cex=0.5,data.col="royalblue",line.col="blue",autocor = "TRUE",shade="WHITE")
# Monthly trend from 2003-2013 (no autocorrelation)
TheilSen(sub_hcho_oe,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",type="month",
         fontsize=20,xlab='Year',ylab=expression(HCHO ~ (molecs.cm^{-2})),lty=1,pch=20,
         main="HCHO at Wollongong (no autocor)",data.col="black",line.col="blue",lwd=4,
         cex=0.05,autocor="FALSE")

# Overall trend from 2003-2015
start.date<- as.POSIXct("2003-01-01 00:00", tz= "GMT")
end.date<- as.POSIXct("2015-12-31 23:00", tz = "GMT")
sub_hcho_oe<-subset(hcho_oe,date>= start.date & date <= end.date)
TheilSen(sub_hcho_oe,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",fontsize=20,
         xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),main="HCHO at Wollongong (oe_autocor) 2003-2013",
         pch=20,cex=0.5,data.col="royalblue",line.col="blue",autocor = "TRUE",shade="WHITE")
# Monthly trend from 2003-2013 (no autocorrelation)
TheilSen(sub_hcho_oe,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",type="month",
         fontsize=20,xlab='Year',ylab=expression(HCHO ~ (molecs.cm^{-2})),lty=1,pch=20,
         main="HCHO at Wollongong (no autocor)",data.col="black",line.col="blue",lwd=4,
         cex=0.05,autocor="FALSE")

#BAR PLOT OF MONTHLY TRENDS
hcho_sub_mon_trends <- data.frame(
  months = c("January","February","March","April","May","June","July","August","September","October","November","December"))
#No Autocorrelation
hcho_sub_mon_trends$trends <- c(-3.99,0.60,-3.06,-4.88,-4.48,-8.45,-5.10,-4.06,-5.01,-3.20,-3.04,-5.91)
hcho_sub_mon_trends$poserr <- c(-2.48,2.15,-2.45,-3.27,-3.85,-6.30,-4.17,-2.31,-3.73,-2.46,-2.05,-5.36)
hcho_sub_mon_trends$negerr <- c(-5.53,-0.67,-3.74,-6.23,-4.73,-9.94,-6.50,-5.69,-6.09,-4.22,-3.86,-6.21)
ggplot(hcho_sub_mon_trends, aes(x = months, y = trends), y) +  
  geom_bar(position = position_dodge(width = ), stat = "identity", fill="blue") +
  scale_x_discrete(limits=c("January","February","March","April","May","June","July","August","September","October","November","December")) +
  geom_errorbar(aes(ymin=negerr, ymax=poserr), position=position_dodge(width=0.9), width=0.25) +
  xlab("Month") + ylab("HCHO Trend (%/yr)") +
  ggtitle("HCHO Trends at Wollongong 2003-2013 with 95% confidence intervals") + # plot title
  theme_bw() + # remove grey background (because Tufte said so)
  theme(panel.grid.major = element_blank()) # remove x and y major grid lines (because Tufte said so)


# Overall trend from 1997-2006
start.date<- as.POSIXct("1997-01-01 00:00", tz= "GMT")
end.date<- as.POSIXct("2007-10-31 23:00", tz = "GMT")
sub_hcho_oe<-subset(hcho_oe,date>= start.date & date <= end.date)
TheilSen(sub_hcho_oe,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",fontsize=20,
         xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),main="HCHO at Wollongong (oe_autocor) 1997-2002",
         pch=20,cex=0.5,data.col="blue",line.col="black",autocor = "TRUE")

# Overall trend from 2007-2015
start.date<- as.POSIXct("2007-10-01 00:00", tz= "GMT")
end.date<- as.POSIXct("2015-12-31 23:00", tz = "GMT")
sub_hcho_oe<-subset(hcho_oe,date>= start.date & date <= end.date)
TheilSen(sub_hcho_oe,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",fontsize=20,
         xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),main="HCHO at Wollongong (oe_autocor) 2005-2015",
         pch=20,cex=0.5,data.col="blue",line.col="black",autocor = "TRUE")

# Overall trend from from 1997-2014 (for comparison with CO and HCN)
start.date<- as.POSIXct("1997-05-25 00:00", tz= "GMT")
end.date<- as.POSIXct("2014-04-02 23:59", tz = "GMT")
sub_hcho_oe<-subset(hcho_oe,date>= start.date & date <= end.date)
TheilSen(sub_hcho_oe,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",fontsize=20,
         xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),main="HCHO at Wollongong (oe_autocor) 2005-2011",
         pch=20,cex=0.5,data.col="blue",line.col="black",autocor = "TRUE")

# Monthly trends from 2005-2015
start.date<- as.POSIXct("2005-01-01 00:00", tz= "GMT")
end.date<- as.POSIXct("2015-12-31 23:00", tz = "GMT")
sub_hcho_oe<-subset(hcho_oe,date>= start.date & date <= end.date)
TheilSen(sub_hcho_oe,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",type="month",
         fontsize=20,xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),
         main="HCHO at Wollongong (autocor) 2005-2015",data.col="blue",line.col="black",
         pch=20,lsd=4,cex=0.5,autocor = "TRUE")

# Monthly trends from 2005-2011
start.date<- as.POSIXct("2005-01-01 00:00", tz= "GMT")
end.date<- as.POSIXct("2011-12-31 23:00", tz = "GMT")
sub_hcho_oe<-subset(hcho_oe,date>= start.date & date <= end.date)
TheilSen(sub_hcho_oe,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",type="month",
         fontsize=20,xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),
         main="HCHO at Wollongong (autocor) 2005-2011",data.col="blue",line.col="black",
         pch=20,lsd=4,cex=0.5,autocor = "TRUE")

# Monthly trends from 1997-2014 (for comparison with CO and HCN)
start.date<- as.POSIXct("1997-05-25 00:00", tz= "GMT")
end.date<- as.POSIXct("2014-04-02 23:59", tz = "GMT")
sub_hcho_oe<-subset(hcho_oe,date>= start.date & date <= end.date)
TheilSen(sub_hcho_oe,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",type="month",
         fontsize=20,xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),
         main="HCHO at Wollongong (autocor) 1997-2011",data.col="blue",line.col="black",
         pch=20,lsd=4,cex=0.5,autocor = "TRUE")
#No Autocorrelation
TheilSen(sub_hcho_oe,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",type="month",
         fontsize=20,xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),
         main="HCHO at Wollongong (no autocor) 1997-2011",data.col="blue",line.col="black",
         pch=20,lsd=4,cex=0.5)

# MODEL COMPARISON
#Limit dates of FTIR data
start.date<- as.POSIXct("1997-01-01 00:00", tz= "GMT")
end.date<- as.POSIXct("2011-12-31 23:59", tz = "GMT")
hcho_oe_model<-subset(hcho_oe,date>= start.date & date <= end.date)
TheilSen(hcho_oe_model,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",fontsize=20,
         xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),main="HCHO at Wollongong (0e_model)",
         pch=20,cex=0.5,data.col="black",line.col="blue",autocor = "TRUE")
TheilSen(hcho_oe_model,pollutant="HCHO",type="season",hemisphere="southern",
         slope.percent="TRUE",main = "HCHO at Wollongong (oe_model)",autocor="TRUE")
TheilSen(hcho_oe_model,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",type="month",
         fontsize=20,xlab='Year',ylab=expression(HCHO ~ (molecs.cm^{-2})),lty=1,pch=20,
         main="HCHO at Wollongong (oe_model)",data.col="black",line.col="blue",lwd=4,
         cex=0.05,autocor="TRUE")

# SATELLITE COMPARISON
#Limit dates of FTIR data
start.date<- as.POSIXct("2004-12-01 00:00", tz= "GMT")
end.date<- as.POSIXct("2016-01-01 23:59", tz = "GMT")
hcho_tik_satellite<-subset(hcho_tik,date>= start.date & date <= end.date)
hcho_oe_satellite<-subset(hcho_oe,date>= start.date & date <= end.date)
TheilSen(hcho_tik_satellite,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",fontsize=20,
         xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),main="HCHO at Wollongong (tik_satellite)",
         pch=20,cex=0.5,data.col="black",line.col="blue",autocor = "TRUE")
TheilSen(hcho_oe_satellite,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",fontsize=20,
         xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),main="HCHO at Wollongong (oe_satellite)",
         pch=20,cex=0.5,data.col="black",line.col="blue",autocor = "TRUE")
#Limit dates of FTIR data
start.date<- as.POSIXct("2005-01-01 00:00", tz= "GMT")
end.date<- as.POSIXct("2015-12-31 23:59", tz = "GMT")
hcho_oe_satellite<-subset(hcho_oe,date>= start.date & date <= end.date)
TheilSen(hcho_oe_satellite,pollutant="HCHO",type="season",hemisphere="southern",
         slope.percent="TRUE",main = "HCHO at Wollongong (oe_satellite)",autocor="TRUE")
TheilSen(hcho_oe_satellite,pollutant="HCHO",deseason="TRUE",slope.percent="TRUE",type="month",
         fontsize=20,xlab='Year',ylab=expression(HCHO ~ (molecs.cm^{-2})),lty=1,pch=20,
         main="HCHO at Wollongong (oe_satellite)",data.col="black",line.col="blue",lwd=4,
         cex=0.05,autocor="TRUE")