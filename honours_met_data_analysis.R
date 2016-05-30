# Import data
bowral_min<-import(file = file.choose(), file.type = "csv",sep = ",", header.at = 1, data.at = 2, date = "Date",
                    date.format = "%Y%m%d", na.strings = c("", "NAN","-9999", "NA" ),correct.time = NULL)

# Time plot
timePlot(albionpark_max, pollutant="Albion_Park_(Wollongong_Airport)_Temperature_Maximum",plot.type="p",
         main="Maximum Temperature at Albion Park",ylab=expression(Temperature ~ (C)),pch=20,cex=0.25)

# Plot overall trend
TheilSen(albionpark_max,pollutant="Albion_Park_(Wollongong_Airport)_Temperature_Maximum",deseason="TRUE",
         slope.percent="TRUE",fontsize=20,xlab="Year", ylab=expression(Temperature ~ (oC)),
         main="Maximum Temperature Trend at Albion Park",pch=20,cex=0.5,data.col="black",line.col="blue")

TheilSen(albionpark_min,pollutant="Albion_Park_(Wollongong_Airport)_Temperature_Minimum",deseason="TRUE",
         slope.percent="TRUE",fontsize=20,xlab="Year", ylab=expression(Temperature ~ (oC)),
         main="Minimum Temperature Trend at Albion Park",pch=20,cex=0.5,data.col="black",line.col="blue")

TheilSen(bellambi_max,pollutant="Bellambi_aws_Temperature_Maximum",deseason="TRUE",
         slope.percent="TRUE",fontsize=20,xlab="Year", ylab=expression(Temperature ~ (oC)),
         main="Maximum Temperature Trend at Bellambi",pch=20,cex=0.5,data.col="black",line.col="blue")

TheilSen(bellambi_min,pollutant="Bellambi_Aws_Temperature_Minimum",deseason="TRUE",
         slope.percent="TRUE",fontsize=20,xlab="Year", ylab=expression(Temperature ~ (oC)),
         main="Minimum Temperature Trend at Bellambi",pch=20,cex=0.5,data.col="black",line.col="blue")

TheilSen(bowral_max,pollutant="Bowral_(Parry_Drive)_Temperature_Maximum",deseason="TRUE",
         slope.percent="TRUE",fontsize=20,xlab="Year", ylab=expression(Temperature ~ (oC)),
         main="Maximum Temperature Trend at Bowral",pch=20,cex=0.5,data.col="black",line.col="blue")

TheilSen(bowral_min,pollutant="Bowral_(Parry_Drive)_Temperature_Minimum",deseason="TRUE",
         slope.percent="TRUE",fontsize=20,xlab="Year", ylab=expression(Temperature ~ (oC)),
         main="Minimum Temperature Trend at Bowral",pch=20,cex=0.5,data.col="black",line.col="blue")

# One plot per month showing change as a percentage
TheilSen(albionpark_max,pollutant="Albion_Park_(Wollongong_Airport)_Temperature_Maximum",deseason="TRUE",
         slope.percent="TRUE",type="month",fontsize=20,xlab='Year',ylab=expression(Temperature ~ (oC)),
         main="Monthly Maximum Temperature Trends at Albion Park",lty=1,data.col="red",line.col="blue",
         lwd=4,pch=20,cex=0.05)

TheilSen(albionpark_min,pollutant="Albion_Park_(Wollongong_Airport)_Temperature_Minimum",deseason="TRUE",
         slope.percent="TRUE",type="month",fontsize=20,xlab='Year',ylab=expression(Temperature ~ (oC)),
         main="Monthly Minimum Temperature Trends at Albion Park",lty=1,data.col="red",line.col="blue",
         lwd=4,pch=20,cex=0.05)