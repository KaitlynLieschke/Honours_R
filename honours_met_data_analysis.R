# Import data
albionpark_min<-import(file = file.choose(), file.type = "csv",
                            sep = ",", header.at = 1, data.at = 2, date = "Date",
                            date.format = "%Y%m%d", na.strings = c("", "NAN","-9999", "NA" ),
                            correct.time = NULL)

# Time plot
timePlot(albionpark_max, pollutant = "Albion_Park_(Wollongong_Airport)_Temperature_Maximum",
         main="Maximum Temperature at Albion Park",ylab=expression(Temperature ~ (C)),
         plot.type = "p",pch=20,cex=0.25)

# Plot overall trend
TheilSen(albionpark_min,pollutant="Albion_Park_(Wollongong_Airport)_Temperature_Minimum",deseason="TRUE",
         slope.percent="TRUE",fontsize=20,xlab="Year", ylab=expression(Temperature ~ (oC)),
         main="Minimum Temperature Trend at Albion Park",pch=20,cex=0.5,
         data.col="black",line.col="blue",line.type="p")