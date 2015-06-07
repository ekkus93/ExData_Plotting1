library(dplyr)

loadRawData <- function(fileName) {
  read.table(fileName, header=TRUE, sep=';', na.strings='?')
}

readData_graph4 <- function(p1) {
  # gotcha - the date format is d/m/yyyy, not m/d/yyyy
  p1 <- mutate(p1, DateVal = as.Date(Date, "%d/%m/%Y"))
  p1a <- filter(p1, DateVal >= as.Date('02/01/2007', "%m/%d/%Y") & DateVal <= as.Date('02/02/2007', "%m/%d/%Y"))
  p1a <- p1a[complete.cases(p1a), ]
  p1a <- mutate(p1a, Day = weekdays(DateVal), DateTime = paste(Date, Time))
  mutate(p1a, DateTimeVal = as.POSIXct(strptime(DateTime, format = "%d/%m/%Y %H:%M:%S")))
}

rawData <- loadRawData('household_power_consumption.txt')

# prep data
d <- readData_graph4(rawData)

# set up grid
par(mfrow=c(2,2), mar = c(4, 4, 1, 1), oma = c(0, 0, 2, 0))

# draw graphs
with(d, {
  plot(DateTimeVal, Global_active_power, type = "l", ylab = 'Global Active Power', xlab ='')
  plot(DateTimeVal, Voltage, type='l', xlab = 'datetime')
  {
    plot(DateTimeVal, Sub_metering_1, type='l', ylab='Energy Sub metering', xlab = '')
    lines(DateTimeVal, Sub_metering_2, type='l', col='red')
    lines(DateTimeVal, Sub_metering_3, type='l', col='blue')
    legend("topright", lty=c(1,1), lwd=c(2.5,2.5), 
           col = c("black", "blue", "red"), 
           bty = "n",
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")) 
  }
  plot(DateTimeVal, Global_reactive_power, type='l', xlab = 'datetime')
})

# save png
dev.copy(png, file = "plot4.png") 
dev.off() 