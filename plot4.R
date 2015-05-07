#Plotting Assignment 1 for Exploratory Data Analysis ---- Plot #4

# need library "data.table" for fread
library(data.table)

plot4<- function() { 
  # read in all the entries in the file to variable energy
  # with fread you can mnetion sep=auto and it will 
  # automatically decide the seperator - Same for the header.
  # Need to treat "?" as  NA, so set the na.string = "?"
  energy <- fread("household_power_consumption.txt",
                  sep="auto",
                  header="auto",
                  na.strings = "?")
  
  # mydays is a boolean list for the twodays which we are interested in 
  # 2007-02-01 and 2007-02-02
  # energy$Date is in a character representation, hence need to use as.Date to compare
  # the days  -- use the '|' or operator
  mydays <- (as.Date(energy$Date, format = "%d/%m/%Y") == as.Date("2007-02-01")) | 
            (as.Date(energy$Date, format = "%d/%m/%Y") == as.Date("2007-02-02"))
  
  # Use the above created boolean to get the subset of the values for which we
  # to plot
  twodays <- energy[mydays]
  
  # Now create another list POSIXlt which will contail the time values 
  # which can be used to plot
  datetime <- strptime(paste(energy$Date[mydays],energy$Time[mydays]),"%d/%m/%Y %H:%M:%S")
  twodays$DateTime <- as.POSIXct(datetime)
  
  # Open a png device and copy the plot to a new file name "plot3.png"
  # width = 480 and height=480
  ## Note:::  Using dev.copy() will not produce the same result due to lossy compression
  
  png(file="plot4.png",  bg="transparent", width=480, height=480)
  
  # Use par to set a 2x2 matrix of sub-plots and the margins
  par(mfrow = c(2,2), pty = "m", mar=c(5,4,4,2))
  
  # sub-plot 1 @ location (1,1)
  plot(datetime,as.numeric(twodays$Global_active_power), 
       type="l", 
       xlab="", 
       ylab="Global Active Power")
  
  # sub-plot 2 @ location (1,2)
  plot(datetime,as.numeric(twodays$Voltage), 
       type="l", 
       ylab="Voltage")
  
  # sub-plot 3 @ location (2,1)
  # USe the plot function to plot Date/Time vs for Sub metering 
  # Add in the second plot for sub_metering_2 using points() function
  # Do the same for sub_metering_3 using points() function
  with(twodays, plot(DateTime,as.numeric(Sub_metering_1),type="l", xlab="", ylab="Energy Sub Metering"))
  with(twodays, points(DateTime,as.numeric(Sub_metering_2),type="l", col="red"))
  with(twodays, points(DateTime,as.numeric(Sub_metering_3),type="l", col="blue"))
  
  #Now add legend for all the three plots
  legend("topright", lty=1,col = c("black","red", "blue"), 
         legend=c("Sub Metering 1", "Sub Metering 2","Sub Metering 3"), 
         bty="n", xjust=0)
  
  # sub-plot 4 @ location (2,2)
  plot(datetime,as.numeric(twodays$Global_reactive_power), 
       type="l",  
       ylab="Global Reactive Power")
  # We need to close the file !!
  dev.off()
  
}

##  Call the plot3() function to create the plot3.png
plot4()
