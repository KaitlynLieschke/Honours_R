hcho_fts_model<-import(file = file.choose(), file.type = "csv",
                       sep = ",", header.at = 1, data.at = 2, date = "DATE",
                       date.format = "%Y%m%d", na.strings = c("", "NAN","-9999", "NA" ),
                       correct.time = NULL)

timePlot(hcho_fts_model, pollutant = c("FTS","GC_AK"), group=TRUE,plot.type="p",pch=c(20,1),cex=0.25,
         main="HCHO at Wollongong",ylab=expression(HCHO ~ (molecs.cm^{-2})),fontsize=20,#ylim=c(0,2e16),
         col=c("blue","red2"))

#Correlation coefficient
cor(hcho_fts_model$FTS,hcho_fts_model$GC_AK)
#Correlation statistics
modStats(hcho_fts_model, mod = "GC_AK", obs = "FTS", statistic = c("MB","NMB"))

# Add bias/offset column
hcho_fts_model$bias <- hcho_fts_model$FTS - hcho_fts_model$GC_AK
timePlot(hcho_fts_model, pollutant = c("bias"), group=TRUE,plot.type="p",pch=c(20),cex=0.25,
         main="HCHO model bias at Wollongong",ylab=expression(HCHO ~ (molecs.cm^{-2})),fontsize=20,ylim=c(-1e16,3e16),
         col=c("black"))
TheilSen(hcho_fts_model,pollutant="bias",deseason="TRUE",slope.percent="TRUE",fontsize=20,
         xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),main="HCHO at Wollongong (GEOS-Chem)",
         pch=20,cex=0.5,data.col="black",line.col="red",autocor = "TRUE")
hcho_fts_model$ratio <- hcho_fts_model$bias / hcho_fts_model$FTS
timePlot(hcho_fts_model, pollutant = c("ratio"), group=TRUE,plot.type="p",pch=c(20),cex=0.25,
         main="HCHO bias:fts ratio at Wollongong",ylab=expression(HCHO ~ (molecs.cm^{-2})),fontsize=20,#ylim=c(-1e16,3e16),
         col=c("black"))
TheilSen(hcho_fts_model,pollutant="ratio",deseason="TRUE",slope.percent="TRUE",fontsize=20,
         xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),main="HCHO at Wollongong (GEOS-Chem)",
         pch=20,cex=0.5,data.col="black",line.col="red",autocor = "TRUE")

#Seasonal Cycle
MyhchoOutput<-timeVariation(hcho_fts_model,pollutant=c("FTS","GC_AK"),ylim=c(0,3.2e16),cols=c("blue","red2"),
                            main="HCHO Seasonal Cycle",ylab=expression(HCHO ~ (molecs.cm^{-2})),
                            fontsize=20)
plot(MyhchoOutput,subset="month",fontsize=20)

# Time plot full HCHO timeseries with limited model values
#hcho_fts_gc_ts <- data.frame(
#  date = c(hcho_oe$DATETIME))
#hcho_fts_gc_ts$fts <- c(hcho_oe$HCHO)
#hcho_fts_gc_ts$gc <- 
#timePlot(hcho_fts_gc_ts, pollutant = c("fts","gc"), group=TRUE,plot.type="p",pch=20,
#         cex=0.25,ylab=expression(HCHO ~ (molecs.cm^{-2})),
#         fontsize=20,ylim=c(0,2e17),col="blue",key="FALSE")

# TRENDS
TheilSen(hcho_fts_model,pollutant="GC_AK",deseason="TRUE",slope.percent="TRUE",fontsize=20,
         xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),main="HCHO at Wollongong (GEOS-Chem)",
         pch=20,cex=0.5,data.col="red2",line.col="black",autocor = "TRUE")
TheilSen(hcho_fts_model,pollutant="GC_AK",type="season",hemisphere="southern",data.col="red2",
         line.col="black",slope.percent="TRUE",main = "HCHO at Wollongong (GEOS-Chem)",autocor="TRUE")
TheilSen(hcho_fts_model,pollutant="GC_AK",deseason="TRUE",slope.percent="TRUE",type="month",
         fontsize=20,xlab='Year',ylab=expression(HCHO ~ (molecs.cm^{-2})),lty=1,pch=20,
         main="HCHO at Wollongong (GEOS-Chem)",data.col="red2",line.col="black",lwd=4,
         cex=0.05,autocor="TRUE")

# Overall trend from 2005-2011
start.date<- as.POSIXct("2005-01-01 00:00", tz= "GMT")
end.date<- as.POSIXct("2011-12-31 23:00", tz = "GMT")
sub_hcho_fts_model<-subset(hcho_fts_model,date>= start.date & date <= end.date)
TheilSen(sub_hcho_fts_model,pollutant="GC_AK",deseason="TRUE",slope.percent="TRUE",fontsize=20,
         xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),main="HCHO at Wollongong (gc_ak) 2005-2011",
         pch=20,cex=0.5,data.col="blue",line.col="black",autocor = "TRUE")

# Monthly trends from 2005-2011
start.date<- as.POSIXct("2005-01-01 00:00", tz= "GMT")
end.date<- as.POSIXct("2011-12-31 23:00", tz = "GMT")
sub_hcho_fts_model<-subset(hcho_fts_model,date>= start.date & date <= end.date)
TheilSen(sub_hcho_fts_model,pollutant="GC_AK",deseason="TRUE",slope.percent="TRUE",type="month",
         fontsize=20,xlab="Year",ylab=expression(HCHO ~ (molecs.cm^{-2})),
         main="HCHO at Wollongong (autocor) 2005-2011",data.col="blue",line.col="black",
         pch=20,lsd=4,cex=0.5,autocor = "TRUE")

#BAR PLOT OF MONTHLY TRENDS
# MODEL TREND DATA
hcho_mod_mon_trends <- data.frame(
  months = c("January","February","March","April","May","June","July","August","September","October","November","December"))
hcho_mod_mon_trends$trends <- c(-0.01, 0.91,-2.04,-0.89,2.40,-1.78,-1.20, 1.26, 0.12,-0.78, 3.91,-2.16)
#-0.23,0.63,-1.82,-1.35,2.35,-1.44,-0.47,0.85,0.19,-0.70,3.36,-2.40)
hcho_mod_mon_trends$poserr <- c(1.30, 2.92,-0.49, 0.59, 8.16,-1.38,-0.30, 5.09, 1.50, 0.90, 8.39,-0.26)
#1.42,2.59,-0.34,0.19,5.71,-1.24,0.23,3.05,1.57,1.06,7.63,-0.53)
hcho_mod_mon_trends$negerr <- c(-1.41,-1.89,-3.15,-1.88,0.62,-3.81,-2.43,-0.76,-1.13,-2.61, 1.75,-3.35)
#-1.68,-1.96,-2.96,-2.50,1.00,-3.51,-1.53,-0.59,-1.07,-2.50,1.21,-3.64)
# FTS TREND DATA
hcho_fts_mon_trends <- data.frame(
  months = c("January","February","March","April","May","June","July","August","September","October","November","December"))
#No autocorrelation
hcho_fts_mon_trends$trends <- c(0.01,-1.05,-2.57,-3.37,-0.61,-2.35,-3.01,-1.73,-1.75,-2.36,1.27,-3.77)
hcho_fts_mon_trends$poserr <- c(0.90,0.11,-2.08,-2.86,0.16,-1.64,-2.73,-1.09,-1.23,-1.97,2.45,-2.94)
hcho_fts_mon_trends$negerr <- c(-1.33,-2.03,-3.03,-3.69,-1.79,-3.30,-3.52,-2.21,-2.18,-2.75,0.25,-4.61)
#Autocorrelation
# hcho_fts_mon_trends$trends <- c(0.01,-1.05,-2.57,-3.37,-0.61,-2.35,-3.01,-1.73,-1.75,-2.36,1.27,-3.77)
# hcho_fts_mon_trends$poserr <- c(2.28,1.67,-1.35,-2.39,3.22,3.00,-2.22,1.23,-0.15,-1.43,5.97,-1.84)
# hcho_fts_mon_trends$negerr <- c(-4.04,-3.65,-3.79,-4.15,-2.78,-4.02,-5.18,-2.84,-2.90,-3.63,-2.00,-6.53)
# Not sure where these numbers came from:  ??
# hcho_fts_mon_trends$trends <- c(-1.23,-0.26,-2.62,-2.97,-1.92,-2.36,-3.48,-1.78,-2.34,-2.24,2.61,-3.72)
# hcho_fts_mon_trends$poserr <- c(2.48,1.71,-1.14,-1.85,2.17,2.96,-2.91,1.07,-1.30,-0.94,8.82,-1.73)
# hcho_fts_mon_trends$negerr <- c(-4.92,-2.65,-3.54,-3.93,-3.27,-4.14,-4.51,-2.98,-3.07,-3.18,-0.21,-6.29)
#BOTH MODEL AND FTS TREND DATA
hcho_both_mon_trends <- data.frame(
  months = c("January","February","March","April","May","June","July","August","September","October","November","December","January","February","March","April","May","June","July","August","September","October","November","December"))
hcho_both_mon_trends$datasource <- c("Model","Model","Model","Model","Model","Model","Model","Model","Model","Model","Model","Model","FTS","FTS","FTS","FTS","FTS","FTS","FTS","FTS","FTS","FTS","FTS","FTS")
#NB: First 12 inputs are from model, last 12 are from FTS
#No autocorrelation
hcho_both_mon_trends$trends <- c(-0.01, 0.91,-2.04,-0.89,2.40,-1.78,-1.20, 1.26, 0.12,-0.78, 3.91,-2.16,0.01,-1.05,-2.57,-3.37,-0.61,-2.35,-3.01,-1.73,-1.75,-2.36,1.27,-3.77)
hcho_both_mon_trends$poserr <- c(1.30, 2.92,-0.49, 0.59, 8.16,-1.38,-0.30, 5.09, 1.50, 0.90, 8.39,-0.26,0.90,0.11,-2.08,-2.86,0.16,-1.64,-2.73,-1.09,-1.23,-1.97,2.45,-2.94)
hcho_both_mon_trends$negerr <- c(-1.41,-1.89,-3.15,-1.88,0.62,-3.81,-2.43,-0.76,-1.13,-2.61, 1.75,-3.35,-1.33,-2.03,-3.03,-3.69,-1.79,-3.30,-3.52,-2.21,-2.18,-2.75,0.25,-4.61)

ggplot(hcho_mod_mon_trends, aes(x = months, y = trends), y) +  
  geom_bar(position = position_dodge(), stat = "identity", fill="pink") +
  scale_x_discrete(limits=c("January","February","March","April","May","June","July","August","September","October","November","December")) +
  geom_errorbar(aes(ymin=negerr, ymax=poserr)) +
  xlab("Month") + ylab("HCHO Trend (%/yr)") +
  ggtitle("HCHO Trends at Wollongong with 95% confidence intervals") + # plot title
  theme_bw() + # remove grey background (because Tufte said so)
  theme(panel.grid.major = element_blank()) # remove x and y major grid lines (because Tufte said so)

ggplot(hcho_fts_mon_trends, aes(x = months, y = trends), y) +  
  geom_bar(position = position_dodge(), stat = "identity", fill="pink") +
  scale_x_discrete(limits=c("January","February","March","April","May","June","July","August","September","October","November","December")) +
  geom_errorbar(aes(ymin=negerr, ymax=poserr)) +
  xlab("Month") + ylab("HCHO Trend (%/yr)") +
  ggtitle("HCHO Trends at Wollongong with 95% confidence intervals") + # plot title
  theme_bw() + # remove grey background (because Tufte said so)
  theme(panel.grid.major = element_blank()) # remove x and y major grid lines (because Tufte said so)

# Both with error bars
ggplot(hcho_both_mon_trends, aes(fill=datasource, y=trends, x=months), y) +
  geom_bar(position=position_dodge(width=0.9), stat = "identity", colour="black") +
  scale_fill_manual(values=c("blue", "red2")) + 
  xlab("Month") + ylab("HCHO Trend (%/yr)") +
  ggtitle("HCHO Trends at Wollongong with 95% confidence intervals") + # plot title
  scale_x_discrete(limits=c("January","February","March","April","May","June","July","August","September","October","November","December")) +
  theme_bw() + # remove grey background (because Tufte said so)
  theme(panel.grid.major = element_blank()) +# remove x and y major grid lines (because Tufte said so)
  geom_errorbar(aes(ymin=negerr, ymax=poserr), position=position_dodge(width=0.9), width=0.25)
