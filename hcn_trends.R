# Trend analysis of long-term HCN measurements at Wollongong

# Import data (Dropbox/2016/UOW/HCHO\ Research\ Assistantship/FTIR\ data/hcn_fts_data.csv)
# Retrieval of HCN from Wollongong FTIR from 05/96 - 12-14
hcn_long<-import(file = file.choose(), file.type = "csv",
                sep = ",", header.at = 1, data.at = 2, date = "DATE",
                date.format = "%Y%m%d", time = "TIME", time.format = "%H%M%S",
                na.strings = c("", "NAN","-9999", "NA" ),
                correct.time = NULL)

# Limit time series to range when measurements of all species are available
start.date<- as.POSIXct("1997-05-25 00:00", tz= "GMT")
end.date<- as.POSIXct("2014-04-02 23:59", tz = "GMT")
hcn_long_cut<-subset(hcn_long,date>= start.date & date <= end.date)

#Time Plot
timePlot(hcn_long_cut, pollutant = "FTS", group=TRUE,plot.type="p",pch=20,
         cex=0.25,ylab=expression(HCN ~ (molecs.cm^{-2})),
         fontsize=20,col="blue",key="FALSE")#ylim=c(0,2e17),

# One plot showing overall trend as a percentage after deseasonalisation
TheilSen(hcn_long_cut,pollutant="FTS",deseason="TRUE",slope.percent="TRUE",fontsize=20,
         xlab="Year",ylab=expression(HCN ~ (molecs.cm^{-2})),main="HCN at Wollongong",
         pch=20,cex=0.5,data.col="royalblue",line.col="black",autocor = "TRUE-",shade="WHITE")

# One plot per month showing change as a percentage
# No autocorrelation
TheilSen(hcn_long_cut,pollutant="FTS",deseason="TRUE",slope.percent="TRUE",type="month",
         fontsize=20,xlab='Year',ylab=expression(HCN ~ (molecs.cm^{-2})),lty=1,pch=20,
         main="HCN at Wollongong",data.col="black",line.col="blue",lwd=4,cex=0.05)
# Autocorrelation
TheilSen(hcn_long_cut,pollutant="FTS",deseason="TRUE",slope.percent="TRUE",type="month",
         fontsize=20,xlab='Year',ylab=expression(HCN ~ (molecs.cm^{-2})),lty=1,pch=20,
         main="HCN at Wollongong (autocor)",data.col="black",line.col="blue",lwd=4,
         cex=0.05,autocor="TRUE")

#BAR PLOT OF MONTHLY TRENDS
hcn_mon_trends <- data.frame(
  months = c("January","February","March","April","May","June","July","August","September","October","November","December"))
#No Autocorrelation
hcn_mon_trends$trends <- c(-2.11,-1.67,-2.02,-2.28,-2.08,-2.36,-2.61,-1.94,-1.68,-1.20,-0.13,-2.45)
hcn_mon_trends$poserr <- c(-1.47,-1.19,-1.54,-2.00,-1.31,-2.10,-2.41,-1.69,-1.29,-0.85,0.34,-1.96)
hcn_mon_trends$negerr <- c(-3.05,-2.09,-2.48,-2.54,-2.49,-2.64,-2.80,-2.15,-1.92,-1.56,-0.66,-2.86)
#Autocorrelation
hcn_mon_trends$trends <- c(-2.11,-1.67,-2.02,-2.28,-2.08,-2.36,-2.61,-1.94,-1.68,-1.20,-0.13,-2.45)
hcn_mon_trends$poserr <- c(-0.63,-0.28,-1.12,-1.24,-0.21,-1.53,-2.14,-1.24,-0.58,-0.45,1.24,-1.45)
hcn_mon_trends$negerr <- c(-4.21,-2.64,-3.10,-3.09,-2.78,-3.06,-3.20,-2.48,-2.32,-2.10,-1.69,-3.82)

ggplot(hcn_mon_trends, aes(x = months, y = trends), y) +  
  geom_bar(position = position_dodge(width = ), stat = "identity", fill="blue") +
  scale_x_discrete(limits=c("January","February","March","April","May","June","July","August","September","October","November","December")) +
  geom_errorbar(aes(ymin=negerr, ymax=poserr), position=position_dodge(width=0.9), width=0.25) +
  xlab("Month") + ylab("HCN Trend (%/yr)") +
  ggtitle("HCN Trends at Wollongong with 95% confidence intervals") + # plot title
  theme_bw() + # remove grey background (because Tufte said so)
  theme(panel.grid.major = element_blank()) # remove x and y major grid lines (because Tufte said so)
