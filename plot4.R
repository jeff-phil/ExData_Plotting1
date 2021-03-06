# This file will create a plot for Exploratory Data Analysis Coursera class
# exdata-011
# @author https://github.com/jeff-phil

# Function to cache data, since large dataset
# Has set / get command that can be used as x$set() or x$get()
cacheData <- function(x = data.frame()) {
        x <- NULL
        set <- function(y) {
                x <<- y
        }
        get <- function() x
        list(set = set, get = get)
}

# Load data function will load the base data and return either the read
# or previously read cached data.
loadData <- function() {
        
        ###  [1] Date: format dd/mm/yyyy  > Will be combined with Time on load
        ###  [x] Time: format HH:mm:ss    > NOTE: Removed after combining
        ###  [2] Global_active_power: in kilowatt
        ###  [3] Global_reactive_power: in kilowatt
        ###  [4] Voltage: in volt
        ###  [5] Global_intensity: in ampere
        ###  [6] Sub_metering_1: in watt-hour of active energy
        ###  [7] Sub_metering_2: in watt-hour of active energy
        ###  [8] Sub_metering_3: in watt-hour of active energy
        d <- NULL
        if(!exists("x")) {
                d <- read.table("data/household_power_consumption.txt", 
                                header = TRUE, sep = ";", na.strings = "?",
                                colClasses = c("character", "character", "numeric", 
                                               "numeric", "numeric"))
                
                # Filter just 2/1/07 and 2/2/07 dates
                d  <- d[d$Date == "1/2/2007" | d$Date == "2/2/2007", ]
                
                # Combine date/time fields and set as a POSIXlt
                Date <- strptime(paste(d[[1]], d[[2]]), "%d/%m/%Y %H:%M:%S")
                
                # Remove columns 1 and 2, then add back in the Date field
                d <- d[, -(1:2)]
                d <- cbind(Date, d)
                
                # Cache the data
                x <<- cacheData()
                x$set(d)
        } else {
                ##message("getting cached data")
                d <- x$get()
        }
        d
}

plotData <- loadData()

png(filename = "plot4.png", width = 480, height = 480, units = "px",
    bg = "white")

# Going by columns, then rows 2x2
par(mfcol = c(2,2))

# first plot
plot(plotData$Date, plotData$Global_active_power, type="l", 
     xlab = "", ylab = "Global Active Power")

# second plot
plot(plotData$Date, plotData$Sub_metering_1, 
     xlab = "", ylab = "Energy sub metering",
     type = "l", col = "black")
lines(plotData$Date, plotData$Sub_metering_2, type = "l", col = "red")
lines(plotData$Date, plotData$Sub_metering_3, type = "l", col = "blue")
legend("topright", 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
       bty = "n",
       lty = c(1,1,1), col = c("black", "red", "blue"))

# third plot
plot(plotData$Date, plotData$Voltage, type = "l",
     xlab = "datetime", ylab = "Voltage")

# fourth plot
plot(plotData$Date, plotData$Global_reactive_power, type = "l",
     xlab = "datetime", ylab = "Global_reactive_power")

dev.off()

