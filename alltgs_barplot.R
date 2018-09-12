# Contains code for:
# Creating arrays of all trends and errors for CO, HCN and HCHO, bar graph of trends for
# each month for each individual gas, bar graph of trends for each month for all gases

#BAR PLOT OF MONTHLY TRENDS
# HCHO TREND DATA (values in parentheses are determined from TheilSen trend calculations in other R script)
hcho_mon_trends <- data.frame(
  months = c("January","February","March","April","May","June","July","August","September","October","November","December"))
#No Autocorrelation
hcho_mon_trends$trends <- c(-2.70,-1.46,-2.70,-2.23,-1.98,-2.62,-2.92,-1.91,-1.79,-1.75,-0.21,-3.71)
hcho_mon_trends$poserr <- c(-0.98,0.87,-1.56,-0.77,0.02,-0.83,-2.49,-0.70,-1.19,-1.10,2.72,-2.81)
hcho_mon_trends$negerr <- c(-5.21,-2.33,-3.58,-3.40,-2.92,-3.59,-4.19,-2.56,-2.69,-2.81,-2.28,-5.12)
# HCN TREND DATA
hcn_mon_trends <- data.frame(
  months = c("January","February","March","April","May","June","July","August","September","October","November","December"))
#No Autocorrelation
hcn_mon_trends$trends <- c(-1.58,0.31,-0.68,-0.54,-0.89,-0.99,-0.53,-0.86,-1.37,-0.84,-1.06,-1.63)
hcn_mon_trends$poserr <- c(-1.04,0.74,-0.57,-0.09,-0.64,-0.73,-0.19,-0.54,-1.14,-0.55,-0.38,-1.17)
hcn_mon_trends$negerr <- c(-2.04,-0.16,-0.84,-0.94,-1.12,-1.30,-0.91,-1.22,-1.58,-1.11,-1.59,-2.08)
# CO TREND DATA
co_mon_trends <- data.frame(
  months = c("January","February","March","April","May","June","July","August","September","October","November","December"))
#No Autocorrelation
co_mon_trends$trends <- c(-1.41,-1.17,-0.94,-1.10,-0.99,-0.94,-0.93,-1.15,-1.53,-0.63,-0.87,-1.71)
co_mon_trends$poserr <- c(-1.07,-0.99,-0.82,-0.75,-0.76,-0.75,-0.73,-1.01,-1.34,-0.37,-0.53,-1.54)
co_mon_trends$negerr <- c(-1.76,-1.33,-1.08,-1.46,-1.21,-1.13,-1.13,-1.27,-1.69,-0.87,-1.17,-1.90)

#BOTH MODEL AND FTS TREND DATA (HCHO, HCN then CO)
#No autocorrelation
alltg_mon_trends <- data.frame(
  months = c("January","February","March","April","May","June","July","August","September","October","November","December","January","February","March","April","May","June","July","August","September","October","November","December","January","February","March","April","May","June","July","August","September","October","November","December"))
alltg_mon_trends$datasource <- c("aHCHO","aHCHO","aHCHO","aHCHO","aHCHO","aHCHO","aHCHO","aHCHO","aHCHO","aHCHO","aHCHO","aHCHO","bHCN","bHCN","bHCN","bHCN","bHCN","bHCN","bHCN","bHCN","bHCN","bHCN","bHCN","bHCN","CO","CO","CO","CO","CO","CO","CO","CO","CO","CO","CO","CO")
alltg_mon_trends$trends <- c(-2.70,-1.46,-2.70,-2.23,-1.98,-2.62,-2.92,-1.91,-1.79,-1.75,-0.21,-3.71,-1.58,0.31,-0.68,-0.54,-0.89,-0.99,-0.53,-0.86,-1.37,-0.84,-1.06,-1.63,-1.41,-1.17,-0.94,-1.10,-0.99,-0.94,-0.93,-1.15,-1.53,-0.63,-0.87,-1.71)
alltg_mon_trends$poserr <- c(-0.98,0.87,-1.56,-0.77,0.02,-0.83,-2.49,-0.70,-1.19,-1.10,2.72,-2.81,-1.04,0.74,-0.57,-0.09,-0.64,-0.73,-0.19,-0.54,-1.14,-0.55,-0.38,-1.17,-1.07,-0.99,-0.82,-0.75,-0.76,-0.75,-0.73,-1.01,-1.34,-0.37,-0.53,-1.54)
alltg_mon_trends$negerr <- c(-5.21,-2.33,-3.58,-3.40,-2.92,-3.59,-4.19,-2.56,-2.69,-2.81,-2.28,-5.12,-2.04,-0.16,-0.84,-0.94,-1.12,-1.30,-0.91,-1.22,-1.58,-1.11,-1.59,-2.08,-1.76,-1.33,-1.08,-1.46,-1.21,-1.13,-1.13,-1.27,-1.69,-0.87,-1.17,-1.90)

ggplot(hcho_mon_trends, aes(x = months, y = trends), y) +  
  geom_bar(position = position_dodge(), stat = "identity", fill="pink") +
  scale_x_discrete(limits=c("January","February","March","April","May","June","July","August","September","October","November","December")) +
  geom_errorbar(aes(ymin=negerr, ymax=poserr)) +
  xlab("Month") + ylab("HCHO Trend (%/yr)") +
  ggtitle("HCHO Trends at Wollongong with 95% confidence intervals") + # plot title
  theme_bw() + # remove grey background (because Tufte said so)
  theme(panel.grid.major = element_blank()) # remove x and y major grid lines (because Tufte said so)

ggplot(hcn_mon_trends, aes(x = months, y = trends), y) +  
  geom_bar(position = position_dodge(), stat = "identity", fill="pink") +
  scale_x_discrete(limits=c("January","February","March","April","May","June","July","August","September","October","November","December")) +
  geom_errorbar(aes(ymin=negerr, ymax=poserr)) +
  xlab("Month") + ylab("HCN Trend (%/yr)") +
  ggtitle("HCN Trends at Wollongong with 95% confidence intervals") + # plot title
  theme_bw() + # remove grey background (because Tufte said so)
  theme(panel.grid.major = element_blank()) # remove x and y major grid lines (because Tufte said so)

ggplot(co_mon_trends, aes(x = months, y = trends), y) +  
  geom_bar(position = position_dodge(), stat = "identity", fill="pink") +
  scale_x_discrete(limits=c("January","February","March","April","May","June","July","August","September","October","November","December")) +
  geom_errorbar(aes(ymin=negerr, ymax=poserr)) +
  xlab("Month") + ylab("CO Trend (%/yr)") +
  ggtitle("CO Trends at Wollongong with 95% confidence intervals") + # plot title
  theme_bw() + # remove grey background (because Tufte said so)
  theme(panel.grid.major = element_blank()) # remove x and y major grid lines (because Tufte said so)

# Both with error bars
ggplot(alltg_mon_trends, aes(fill=datasource, y=trends, x=months), y) +
  geom_bar(position=position_dodge(width=0.9), stat = "identity", colour="black") +
  scale_fill_manual(values=c("blue", "yellow", "brown")) + 
  xlab("Month") + ylab("Trend (%/yr)") +
  ggtitle("Trace Gas Trends at Wollongong with 95% confidence intervals") + # plot title
  scale_x_discrete(limits=c("January","February","March","April","May","June","July","August","September","October","November","December")) +
  theme_bw() + # remove grey background (because Tufte said so)
  theme(panel.grid.major = element_blank()) +# remove x and y major grid lines (because Tufte said so)
  geom_errorbar(aes(ymin=negerr, ymax=poserr), position=position_dodge(width=0.9), width=0.25)
