# July 29, 2016
# A script to:
# -graph MET data from a CDMO-generated QC csv file
# -put those graphs in a pdf with the same name as the original csv

# IMPORTANT
# need to have some packages installed
# only need to uncomment and run these once; you can ignore them in future script runs
# install.packages('clifro')
# install.packages('ggplot2')

# IMPORTANT
# You'll need to minimize RStudio to see the pop-up boxes
# that let you choose your working directory and file


# interactively choose which folder you want to work in

library(tcltk) #this package is part of base R and does not need to be installed separately
my.dir <- tk_choose.dir(getwd(), caption = "Set your working directory")
setwd(my.dir)

# interactively choose the file to work on
myFile <-  tk_choose.files(getwd(), caption="Choose file")
  
met.data <- read.csv(myFile)
x <- nchar(myFile) # counting the characters in the file name
  
Title = substr(myFile,x-20,x-4) # this should return the full name of the file (minus '.csv')
Titlepdf <- paste(Title, ".pdf", sep="") #this will be used later for the output file


#format DateTime as POSIXct, which will turn it into a number that can be graphed
#we are retaining the format of mm/dd/yyyy hh:mm
met.data$DateTime <- as.POSIXct(met.data$TIMESTAMP, format = "%m/%d/%Y %H:%M", tz = 'America/Regina')
  
# open up a pdf file to print to
pdf(file=Titlepdf) 
  
# make the graph page layout 4 rows and 2 columns so all graphs will fit on a page
par(mfcol=c(4,2), mar=c(2.1, 4.1, 1.1, 1.1), oma=c(1,1,2,1))
  
#make line graphs
plot(ATemp~DateTime, data=met.data, type="l", xlab = "", xaxt='n', col="darkred")
axis.POSIXct(1, at=seq(min(met.data$DateTime, na.rm=TRUE), max(met.data$DateTime, na.rm=TRUE), length.out=5), format="%m/%d", cex.axis=0.9)


plot(RH~DateTime, data=met.data, type="l", xlab = "", xaxt='n', col="darkblue")
axis.POSIXct(1, at=seq(min(met.data$DateTime, na.rm=TRUE), max(met.data$DateTime, na.rm=TRUE), length.out=5), format="%m/%d", cex.axis=0.9)


plot(BP~DateTime, data=met.data, type="l", xlab = "", xaxt='n',col="darkgreen")
axis.POSIXct(1, at=seq(min(met.data$DateTime, na.rm=TRUE), max(met.data$DateTime, na.rm=TRUE), length.out=5), format="%m/%d", cex.axis=0.9)

plot(TotPAR~DateTime, data=met.data, type="l", xlab = "", xaxt='n',col="darkturquoise")
axis.POSIXct(1, at=seq(min(met.data$DateTime, na.rm=TRUE), max(met.data$DateTime, na.rm=TRUE), length.out=5), format="%m/%d", cex.axis=0.9)

plot(WSpd~DateTime, data=met.data, type="l", xlab = "", xaxt='n',col="darkslategray")
axis.POSIXct(1, at=seq(min(met.data$DateTime, na.rm=TRUE), max(met.data$DateTime, na.rm=TRUE), length.out=5), format="%m/%d", cex.axis=0.9)

plot(MaxWSpd~DateTime, data=met.data, type="l", xlab = "", xaxt='n',col="darkorange")
axis.POSIXct(1, at=seq(min(met.data$DateTime, na.rm=TRUE), max(met.data$DateTime, na.rm=TRUE), length.out=5), format="%m/%d", cex.axis=0.9)

plot(CumPrcp~DateTime, data=met.data, type="l", xlab = "", xaxt='n',col="darkmagenta")
axis.POSIXct(1, at=seq(min(met.data$DateTime, na.rm=TRUE), max(met.data$DateTime, na.rm=TRUE), length.out=5), format="%m/%d", cex.axis=0.9)

plot(AvgVolt~DateTime, data=met.data, type="l", xlab = "", xaxt='n',col="darkkhaki")
axis.POSIXct(1, at=seq(min(met.data$DateTime, na.rm=TRUE), max(met.data$DateTime, na.rm=TRUE), length.out=5), format="%m/%d", cex.axis=0.9)


# put the title of the file above all the plots on the page
mtext(Title, outer=TRUE, side=3, cex=0.9, font=2)


# make a wind rose on the next page
#reset to one graph per page
par(mfrow=c(1,1))

library(clifro)
library(ggplot2)


windrose(met.data$WSpd, met.data$Wdir, speed_cuts = c(2,4,6,8,10,15,30), legend_title='Wind Speed (m/s)', ggtheme='minimal') + 
  ggtitle(Title) +
  theme(plot.title = element_text(size = 12, face='bold'),
        legend.title = element_text(size=9),
        legend.text = element_text(size=8),
        legend.key.size = unit(5, 'mm'),
        axis.text.y = element_text(size=8),
        strip.text = element_text(face='bold')) 

  
#turn off pdf printer
dev.off()


print('Finished!')