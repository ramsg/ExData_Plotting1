#Plotting Assignment 1 for Exploratory Data Analysis ----  Plot#1

# need library "data.table" for fread
library(data.table)

plot1 <- function() {
  
  # read in all the entries in the file to variable energy
  # with fread you can mention sep=auto and it will 
  # automatically decide the seperator - Same for the header.
  # Need to treat "?" as  NA, so set the na.string = "?"
  energy <- fread("household_power_consumption.txt",sep="auto",
                  header="auto",na.strings = "?")
  
  # mydays is a boolean list for the twodays which we are interested in 
  # 2007-02-01 and 2007-02-02
  # energy$Date is in a character representation, hence need to use as.Date 
  # to compare the days -  -- use  '|' (or) operator
  mydays <- (as.Date(energy$Date, format = "%d/%m/%Y") == as.Date("2007-02-01")) | 
            (as.Date(energy$Date, format = "%d/%m/%Y") == as.Date("2007-02-02"))
  
  # Use the above created boolean to get the subset of the values to plot
  twodays <- energy[mydays]
  
  # Open a png device and copy the plot to a new file name "plot1.png"
  # width = 480 and height=480
  
  png(file="plot1.png", bg="transparent",width=480, height=480)
  
  ## Note:::  Using dev.copy() will not produce the same result due to lossy compression
  # USe the hist function to plot the first plot for Global Acvite Power vs Freqeuncy
  hist(as.numeric(twodays$Global_active_power), 
       main=paste("Global Active Power"), 
       xlab="Global Active Power (in kilowatts)", 
       col="red")
  
  # We need to close the device !!
  dev.off()
  
}

##  Call the plot1() function to create the plot1.png
plot1()

