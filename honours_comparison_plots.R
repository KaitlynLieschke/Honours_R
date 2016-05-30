# Import data
hcn_data_two_sevena<-import(file = file.choose(), file.type = "csv",
                 sep = ",", header.at = 1, data.at = 2, date = "DATE",
                 date.format = "%Y%m%d", na.strings = c("", "NAN","-9999", "NA" ),
                 correct.time = NULL)

co_data_two_eleven<-import(file = file.choose(), file.type = "csv",
                            sep = ",", header.at = 1, data.at = 2, date = "DATE",
                            date.format = "%Y%m%d", na.strings = c("", "NAN","-9999", "NA" ),
                            correct.time = NULL)

hcho_data_two_eleven<-import(file = file.choose(), file.type = "csv",
                           sep = ",", header.at = 1, data.at = 2, date = "DATE",
                           date.format = "%Y%m%d", na.strings = c("", "NAN","-9999", "NA" ),
                           correct.time = NULL)

# Combine data sets for each year
hcn_data_comb<-rbind(hcn_data_one_seven,hcn_data_one_eight,hcn_data_one_nine,hcn_data_two_zero,
                     hcn_data_two_one,hcn_data_two_two,hcn_data_two_three,hcn_data_two_four,
                     hcn_data_two_five,hcn_data_two_six,hcn_data_two_sevena,hcn_data_two_sevenb,
                     hcn_data_two_eight,hcn_data_two_nine,hcn_data_two_ten,hcn_data_two_eleven)

co_data_comb<-rbind(co_data_one_seven,co_data_one_eight,co_data_one_nine,co_data_two_zero,
                     co_data_two_one,co_data_two_two,co_data_two_three,co_data_two_four,
                     co_data_two_five,co_data_two_six,co_data_two_sevena,co_data_two_sevenb,
                     co_data_two_eight,co_data_two_nine,co_data_two_ten,co_data_two_eleven)

hcho_data_comb<-rbind(hcho_data_one_seven,hcho_data_one_eight,hcho_data_one_nine,hcho_data_two_zero,
                    hcho_data_two_one,hcho_data_two_two,hcho_data_two_three,hcho_data_two_four,
                    hcho_data_two_five,hcho_data_two_six,hcho_data_two_seven,
                    hcho_data_two_eight,hcho_data_two_nine,hcho_data_two_ten,hcho_data_two_eleven)

# Correlation coefficients
cor(co_data_comb$GC,co_data_comb$FTS)
cor(hcn_data_comb$GC,hcn_data_comb$FTS)
cor(hcho_data_comb$GC,hcho_data_comb$FTS)

# Correlation statistics
modStats(co_data_comb, mod = "GC", obs = "FTS", statistic = c("MB","NMB"))
modStats(hcn_data_comb, mod = "GC", obs = "FTS", statistic = c("MB","NMB"))
modStats(hcho_data_comb, mod = "GC", obs = "FTS", statistic = c("MB","NMB"))

# Plot

# Time plot
#hcn_err_sum<-hcn_data_comb$SYS+hcn_data_comb$RAN
#hcn_date<-hcn_data_comb$date
#hcn_fts<-hcn_data_comb$FTS
plot.new()
timePlot(hcn_data_comb, pollutant = c("FTS","GC"), cols=c("black","red"),group=TRUE,
         main="HCN at Wollongong",ylab=expression(HCN ~ (molecs.cm^{-2})),fontsize=20,
         plot.type="p",pch=c(20,1),cex=0.25)
#arrows(hcn_date, hcn_fts+hcn_err_sum, hcn_date, hcn_fts-hcn_err_sum, length=0.05, angle=90, code=3)
#errbar(hcn_date, hcn_fts, hcn_fts+hcn_err_sum, hcn_fts-hcn_err_sum)

timePlot(co_data_comb, pollutant = c("FTS","GC"), cols=c("black","red"),group=TRUE,plot.type="p",
         main="CO at Wollongong",ylab=expression(CO ~ (molecs.cm^{-2})),fontsize=20,ylim=c(0.5e18,5e18),
         pch=c(20,1),cex=0.25)
timePlot(hcho_data_comb, pollutant = c("FTS","GC"), group=TRUE,plot.type="p",pch=c(20,1),cex=0.25,
         main="HCHO at Wollongong",ylab=expression(HCHO ~ (molecs.cm^{-2})),fontsize=20,ylim=c(0,6e16),
         col=c("black","red"))

timePlot(subset(hcn_data_comb,format(date,"%Y") > 1998), pollutant = c("GC"))

# Seasonal cycle plot
MyOutput<-timeVariation(hcn_data_comb,pollutant=c("FTS","GC"),ylim=c(1.5e15,8.5e15),cols=c("black","red"),
                        main="HCN Seasonal Cycle 1997-2011",ylab=expression(HCN ~ (molecs.cm^{-2})),
                        fontsize=20)
MySecondOutput<-timeVariation(subset(hcn_data_comb,format(date,"%Y") > 1998),pollutant=c("FTS","GC"))
plot(MyOutput,subset="month",fontsize=20)
MycoOutput<-timeVariation(co_data_comb,pollutant=c("FTS","GC"),ylim=c(1e18,2e18),cols=c("black","red"),
                        main="CO Seasonal Cycle 1997-2011",ylab=expression(CO ~ (molecs.cm^{-2})),
                        fontsize=20)
MySecondcoOutput<-timeVariation(subset(co_data_comb,format(date,"%Y") > 1998),pollutant=c("FTS","GC"))
plot(MycoOutput,subset="month",fontsize=20)
MyhchoOutput<-timeVariation(hcho_data_comb,pollutant=c("FTS","GC"),ylim=c(0,2e16),cols=c("black","red"),
                          main="HCHO Seasonal Cycle 1997-2011",ylab=expression(HCHO ~ (molecs.cm^{-2})),
                          fontsize=20)
MySecondhchoOutput<-timeVariation(subset(hcho_data_comb,format(date,"%Y") > 1998),pollutant=c("FTS","GC"))
plot(MyhchoOutput,subset="month",fontsize=20)

# Seasonal cycle with and without 1997-8
a<-timeVariation(hcn_data_comb,pollutant=c("FTS","GC"),ylim=c(1.5e15,8.5e15),cols=c("black","red"))
b<-timeVariation(subset(hcn_data_comb,format(date,"%Y")>1998),pollutant=c("FTS","GC"),
                 ylim=c(1.5e15,8.5e15),cols=c("gray30","darkorange2"))
print(a,split=c(1,1,2,1),subset="month",main="HCN Seasonal Cycle 1997-2011",
      ylab=expression(HCN ~ (molecs.cm^{-2})),fontsize=20)
print(b,split=c(2,1,2,1),subset="month",newpage=FALSE,main="HCN Seasonal Cycle after 1999-2011",
      ylab=expression(HCN ~ (molecs.cm^{-2})),fontsize=20)
c<-timeVariation(co_data_comb,pollutant=c("FTS","GC"),ylim=c(1e18,2e18),cols=c("black","red"),
                 ylab=expression(CO ~ (molecs.cm^{-2})),fontsize=20)
d<-timeVariation(subset(co_data_comb,format(date,"%Y")>1998),pollutant=c("FTS","GC"),
                 ylim=c(1e18,2e18),cols=c("gray30","darkorange2"))
print(c,split=c(1,1,2,1),subset="month",main="CO Seasonal Cycle 1997-2011",
      ylab=expression(CO ~ (molecs.cm^{-2})),fontsize=20)
print(d,split=c(2,1,2,1),subset="month",newpage=FALSE,main="CO Seasonal Cycle after 1999-2011",
      ylab=expression(CO ~ (molecs.cm^{-2})),fontsize=20)
e<-timeVariation(hcho_data_comb,pollutant=c("FTS","GC"),ylim=c(0,2e16),cols=c("black","red"),
                 ylab=expression(HCHO ~ (molecs.cm^{-2})),fontsize=20)
f<-timeVariation(subset(hcho_data_comb,format(date,"%Y")>1998),pollutant=c("FTS","GC"),
                 ylim=c(0,6e16),cols=c("gray30","darkorange2"))
print(e,split=c(1,1,2,1),subset="month",main="HCHO Seasonal Cycle 1997-2011",
      ylab=expression(HCHO ~ (molecs.cm^{-2})),fontsize=20)
print(f,split=c(2,1,2,1),subset="month",newpage=FALSE,main="HCHO Seasonal Cycle after 1999-2011",
      ylab=expression(CO ~ (molecs.cm^{-2})),fontsize=20)


# Trend analysis (95% confidence interval default) ###################################

# One plot per season showing change as a percentage
TheilSen(hcn_data_comb,pollutant="GC",type="season",hemisphere="southern",slope.percent="TRUE")
TheilSen(co_data_comb,pollutant="GC",type="season",hemisphere="southern",slope.percent="TRUE")
TheilSen(hcho_data_comb,pollutant="GC",type="season",hemisphere="southern",slope.percent="TRUE")

# One plot showing overall trend as a percentage after deseasonalisation
TheilSen(hcn_data_comb,pollutant="FTS",deseason="TRUE",slope.percent="TRUE",fontsize=20,xlab="Year",
         ylab=expression(HCN ~ (molecs.cm^{-2})),main="FTIR",pch=20,cex=0.5,#ylim=c(1.5e15,1.05e16),
         data.col="black",line.col="blue")#,line.type="p")
TheilSen(hcn_data_comb,pollutant="GC",deseason="TRUE",slope.percent="TRUE",fontsize=20,xlab="Year",
         ylab=expression(HCN ~ (molecs.cm^{-2})),main="GEOS-Chem",pch=1,cex=0.3,#ylim=c(1.5e15,1.05e16),
         data.col="red",line.col="blue")
TheilSen(co_data_comb,pollutant="FTS",deseason="TRUE",slope.percent="TRUE",fontsize=20,xlab="Year",
         ylab=expression(CO~ (molecs.cm^{-2})),main="FTIR",pch=20,cex=0.5,#ylim=c(8e17,3e18),
         data.col="black",line.col="blue")
TheilSen(co_data_comb,pollutant="GC",deseason="TRUE",slope.percent="TRUE",fontsize=20,xlab="Year",
         ylab=expression(CO ~ (molecs.cm^{-2})),main="GEOS-Chem",pch=1,cex=0.3,#ylim=c(8e17,3e18),
         data.col="red",line.col="blue")
TheilSen(hcho_data_comb,pollutant="FTS",deseason="TRUE",slope.percent="TRUE",fontsize=20,xlab="Year",
         ylab=expression(HCHO~ (molecs.cm^{-2})),main="FTIR",pch=20,cex=0.5,#ylim=c(0,3e16),
         data.col="black",line.col="blue")#,statistic="median")
TheilSen(hcho_data_comb,pollutant="GC",deseason="TRUE",slope.percent="TRUE",fontsize=20,xlab="Year",
         ylab=expression(HCHO ~ (molecs.cm^{-2})),main="GEOS-Chem",pch=1,cex=0.3,#ylim=c(0,3e16),
         data.col="red",line.col="blue")#,statistic="median")
# Plot trend for years after 1998 after deseasonalisation
TheilSen(subset(hcn_data_comb,format(date,"%Y") > 1998),pollutant="FTS",fontsize=20,xlab='Year',
         ylab=expression(HCN ~ (molecs.cm^{-2})),main="FTIR",deseason="TRUE",slope.percent="TRUE")
TheilSen(subset(hcn_data_comb,format(date,"%Y") > 1998),pollutant="GC",fontsize=20,xlab='Year',
         ylab=expression(HCN ~ (molecs.cm^{-2})),main="GEOS-Chem",deseason="TRUE",slope.percent="TRUE")
TheilSen(subset(co_data_comb,format(date,"%Y") > 1998),pollutant="FTS",fontsize=20,xlab='Year',
         ylab=expression(CO ~ (molecs.cm^{-2})),main="FTIR",deseason="TRUE",slope.percent="TRUE")
TheilSen(subset(co_data_comb,format(date,"%Y") > 1998),pollutant="GC",fontsize=20,xlab='Year',
         ylab=expression(CO ~ (molecs.cm^{-2})),main="GEOS-Chem",deseason="TRUE",slope.percent="TRUE")
TheilSen(subset(hcho_data_comb,format(date,"%Y") < 2009),pollutant="FTS",fontsize=20,pch=20,cex=0.5,xlab='Year',
         ylab=expression(HCHO ~ (molecs.cm^{-2})),main="FTIR",deseason="TRUE",slope.percent="TRUE")
TheilSen(subset(hcho_data_comb,format(date,"%Y") < 2009),pollutant="GC",fontsize=20,pch=20,cex=0.5,xlab='Year',
         ylab=expression(HCHO ~ (molecs.cm^{-2})),main="GEOS-Chem",deseason="TRUE",slope.percent="TRUE")

# One plot per month showing change as a percentage
TheilSen(hcn_data_comb,pollutant="GC",deseason="TRUE",slope.percent="TRUE",type="month",#statistic="median",
         fontsize=20,xlab='Year',ylab=expression(HCN ~ (molecs.cm^{-2})),main="GEOS-Chem",lty=1,
         data.col="red",line.col="blue",lwd=4,pch=20,cex=0.05)
TheilSen(hcn_data_comb,pollutant="FTS",deseason="TRUE",slope.percent="TRUE",type="month",#statistic="median",
         fontsize=20,xlab='Year',ylab=expression(HCN ~ (molecs.cm^{-2})),main="FTIR",lty=1,
         data.col="black",line.col="blue",lwd=4,pch=20,cex=0.05)
TheilSen(co_data_comb,pollutant="GC",deseason="TRUE",slope.percent="TRUE",type="month",#statistic="median",
         fontsize=20,xlab='Year',ylab=expression(CO ~ (molecs.cm^{-2})),main="GEOS-Chem",lty=1,
         data.col="red",line.col="blue",lwd=4,pch=20,cex=0.05)
TheilSen(co_data_comb,pollutant="FTS",deseason="TRUE",slope.percent="TRUE",type="month",#statistic="median",
         fontsize=20,xlab='Year',ylab=expression(CO ~ (molecs.cm^{-2})),main="FTIR",lty=1,
         data.col="black",line.col="blue",lwd=4,pch=20,cex=0.05)
TheilSen(hcho_data_comb,pollutant="GC",deseason="TRUE",slope.percent="TRUE",type="month",#statistic="median",
         fontsize=20,xlab='Year',ylab=expression(HCHO ~ (molecs.cm^{-2})),main="GEOS-Chem",lty=1,
         data.col="red",line.col="blue",lwd=4,pch=20,cex=0.05)
TheilSen(hcho_data_comb,pollutant="FTS",deseason="TRUE",slope.percent="TRUE",type="month",#statistic="median",
         fontsize=20,xlab='Year',ylab=expression(HCHO ~ (molecs.cm^{-2})),main="FTIR",lty=1,
         data.col="black",line.col="blue",lwd=4,pch=20,cex=0.05) #best one so far but still not lines
# One plot per month from 1998 showing change as a percentage
TheilSen(subset(hcn_data_comb,format(date,"%Y")>1997),pollutant="GC",
         deseason="TRUE",slope.percent="TRUE",type="month")
TheilSen(subset(hcho_data_comb,format(date,"%Y")<2009),pollutant="FTS",deseason="TRUE",slope.percent="TRUE",type="month",#statistic="median",
         fontsize=20,xlab='Year',ylab=expression(HCHO ~ (molecs.cm^{-2})),main="FTIR",lty=1,
         data.col="black",line.col="blue",lwd=4,pch=20,cex=0.05) #best one so far but still not lines
# One plot per season for years from 1999 after deseasonalisation
TheilSen(subset(hcn_data_comb,format(date,"%Y") > 1998),pollutant="GC",
         deseason="TRUE",slope.percent="TRUE",type="month")