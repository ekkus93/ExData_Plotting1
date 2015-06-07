library(dplyr)

loadRawData <- function(fileName) {
  read.table(fileName, header=TRUE, sep=';', na.strings='?')
}

readData_graph2 <- function(p1) {
  # gotcha - the date format is d/m/yyyy, not m/d/yyyy
  p1 <- mutate(p1, DateVal = as.Date(Date, "%d/%m/%Y"))
  p1a <- filter(p1, DateVal >= as.Date('02/01/2007', "%m/%d/%Y") & DateVal <= as.Date('02/02/2007', "%m/%d/%Y"))
  p1a <- p1a[complete.cases(p1a), ]
  p1a <- mutate(p1a, Day = weekdays(DateVal), DateTime = paste(Date, Time))
  mutate(p1a, DateTimeVal = as.POSIXct(strptime(DateTime, format = "%d/%m/%Y %H:%M:%S")))  
}

rawData <- loadRawData('household_power_consumption.txt')
p1a <- readData_graph2(rawData)
par(mfrow=c(1,1))
plot(p1a$DateTimeVal, p1a$Global_active_power, type = "l", ylab = 'Global Active Power (kilowatts)', xlab ='')

# save png
dev.copy(png, file = "plot2.png") 
dev.off() 