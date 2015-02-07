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
                x <<- cacheData()
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
                x$set(d)
        } else {
                ##message("getting cached data")
                d <- x$get()
        }
        d
}

plotData <- loadData()

# Open png device
png(filename = "plot1.png", width = 480, height = 480, units = "px",
    bg = "transparent")

# Create plot
hist(plotData$Global_active_power, 
     col = "red", 
     xlab = "Global Active Power (kilowatts)", 
     main = "Global Active Power")

# Close png device
dev.off()

