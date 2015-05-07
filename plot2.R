#Plotting Assignment 1 for Exploratory Data Analysis  ---  PLOT #2

# need library "data.table" for fread
library(data.table)

plot2 <- function() { 
  
  # read in all the entries in the file to variable energy
  # with fread you can mnetion sep=auto and it will 
  # automatically decide the seperator - Same for the header.
  # Need to treat "?" as  NA, so set the na.string = "?"
  energy <- fread("household_power_consumption.txt",
                  sep="auto",header="auto",
                  na.strings = "?")
  
  # mydays is a boolean list for the twodays which we are interested in 
  # 2007-02-01 and 2007-02-02
  
  # energy$Date is in a character representation, hence need to use 
  # as.Date to compare the days
  mydays <- (as.Date(energy$Date, format = "%d/%m/%Y") == as.Date("2007-02-01")) | 
            (as.Date(energy$Date, format = "%d/%m/%Y") == as.Date("2007-02-02"))
  
  # Use the above created boolean to get the subset of the values for which we
  # to plot
  twodays <- energy[mydays]
  
  # Now create another list POSIXlt which will contail the time values 
  # which can be used to plot
  datetime <- strptime(paste(energy$Date[mydays],energy$Time[mydays]),"%d/%m/%Y %H:%M:%S")
  
  # Open a png device and copy the plot to a new file name "plot2.png"
  # width = 480 and height=480
  ## Note:::  Using dev.copy() will not produce the same result due to lossy compression
  
  png(file="plot2.png",  bg="transparent",width=480, height=480)
    
  # USe the plot function to plot Date/Time vs for Global Acvite Power
  plot(datetime,as.numeric(twodays$Global_active_power), type="l", 
       xlab="", ylab="Global Active Power (in kilowatts)")
  
  # We need to close the png device !!
  dev.off()
  
}

##  Call the plot2() function to create the plot2.png

plot2()
