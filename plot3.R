library(dplyr)

readData_graph3 <- function(fileName) {
  p1 <- read.table('household_power_consumption.txt', header=TRUE, sep=';', na.strings='?')
  
  # gotcha - the date format is d/m/yyyy, not m/d/yyyy
  p1 <- mutate(p1, DateVal = as.Date(Date, "%d/%m/%Y"))
  p1a <- filter(p1, DateVal >= as.Date('02/01/2007', "%m/%d/%Y") & DateVal <= as.Date('02/02/2007', "%m/%d/%Y"))
  p1a <- p1a[complete.cases(p1a), ]
  p1a <- mutate(p1a, Day = weekdays(DateVal), DateTime = paste(Date, Time))
  p1a <- mutate(p1a, DateTimeVal = as.POSIXct(strptime(DateTime, format = "%d/%m/%Y %H:%M:%S")))
  
  select(p1a, DateTimeVal, Sub_metering_1, Sub_metering_2, Sub_metering_3)
}

drawGraph3 <- function(p1b) {
  with(p1b, plot(DateTimeVal, Sub_metering_1, type='l', ylab='Energy Sub metering'))
  with(p1b, lines(DateTimeVal, Sub_metering_2, type='l', col='red'))
  with(p1b, lines(DateTimeVal, Sub_metering_3, type='l', col='blue'))
  legend("topright", lty=c(1,1), lwd=c(2.5,2.5), 
         col = c("black", "blue", "red"), 
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
}

p1b <- readData_graph3('household_power_consumption.txt')
drawGraph3(p1b)