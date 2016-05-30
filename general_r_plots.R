variable<-import(file = file.choose(), file.type = "csv",
             sep = ",", header.at = 1, data.at = 2, date = "DateW2",
             date.format = "%d/%m/%Y %H:%M:%S", time = NULL,
             time.format = NULL, na.strings = c("", "NAN","-9999", "NA" ),
             correct.time = NULL)

#Plot colour scale on year calendar from FTS
calendarPlot(Woll_CO_FTS, pollutant='co',year=1909,avg.time='day')

GP_firea<-timeAverage(GP_fire, avg.time = "5 min",start.date = "2014-05-30 18:30:00")
GP_Met<-import(file = file.choose(), file.type = "csv",
               sep = ",", header.at = 1, data.at = 2, date = "Date",
               date.format = "%d/%m/%Y %H:%M", time = NULL,
               time.format = NULL, na.strings = c("", "NAN","-9999", "NA" ),
               ws = "ws", wd = "wd",
               correct.time = NULL)
GP_fireall <- merge(GP_firea, GP_NOxa, by.x = "date", by.y = "date")

timePlot(co_comparison,pollutant="GC")
timePlot(GP_all2,pollutant="CH4")
summaryPlot(GP_all2,pollutant="co")
scatterPlot(GP_all2,x="date",y="wd",z="ws")
percentileRose(GP_all2,pollutant="CO", wd = "wd")
pollutionRose(GP_all2, ws="ws", wd="wd", pollutant="CH4")
TestSubset<-subset(GP_all,nox_real<23)
polarPlot(TestSubset, ws="ws", wd="wd", pollutant="no_real")

write.csv(day10, file = file.choose())

TestSubset1<-subset(GP_all,CO<2.4)
remove()
polarFreq(GP_all)
polarCluster(GP_all, pollutant="co", n.clusters=2:10, cols= "Set3")

# select a subset of time to focus on
start.date<- as.POSIXct("2009-01-01", tz= "GMT")
end.date<- as.POSIXct("2010-01-01", tz = "GMT")
day10<-subset(CRDS,date>= start.date & date <= end.date)

daytime_MUMBA<-selectByDate(GP_all,hour=4:8)

#time cyles
timeVariation(GP_all, pollutant="CO")