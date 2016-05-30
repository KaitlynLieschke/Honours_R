# Import data
campbelltown_max<-import(file = file.choose(), file.type = "csv",sep = ",", header.at = 1, data.at = 2, date = "Date",
                    date.format = "%Y%m%d", na.strings = c("", "NAN","-9999", "NA" ),correct.time = NULL)

# Identify site for plotting and set variables
# Uncomment relevant site information
dataset=albionpark_min
sitemet="Albion_Park_(Wollongong_Airport)_Temperature_Minimum"
plottitle="Trend in Minimum Temperature at Albion Park"
'''
dataset=bellambi_min
sitemet="Bellambi_Aws_Temperature_Minimum"
plottitle="Trend in Minimum Temperature at Bellambi"

dataset=bowral_min
sitemet="Bowral_(Parry_Drive)_Temperature_Minimum"
plottitle="Trend in Minimum Temperature at Bowral"

dataset=campbelltown_min
sitemet="Campbelltown_(Mount_Annan)_Temperature_Minimum"
plottitle="Trend in Minimum Temperature at Campbelltown"
'''
# Time plot
timePlot(albionpark_max, pollutant=sitemet,plot.type="p",main="Maximum Temperature at Albion Park",
         ylab=expression(Temperature ~ (C)),pch=20,cex=0.25)

# Plot overall trend
TheilSen(dataset,pollutant=sitemet,deseason="TRUE",slope.percent="TRUE",fontsize=20,xlab="Year",
         ylab=expression(Temperature ~ (oC)),main=plottitle,pch=20,cex=0.5,data.col="black",line.col="blue")

# One plot per month showing change as a percentage
TheilSen(dataset,pollutant=sitemet,deseason="TRUE",slope.percent="TRUE",type="month",fontsize=20,xlab='Year',
         ylab=expression(Temperature ~ (oC)),main=plottitle,lty=1,data.col="red",line.col="blue",lwd=4,pch=20,
         cex=0.05)

