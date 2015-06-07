library(dplyr)

loadRawData <- function(fileName) {
  read.table(fileName, header=TRUE, sep=';', na.strings='?')
}

readData_graph1 <- function(p1) {
  # gotcha - the date format is d/m/yyyy, not m/d/yyyy
  p1 <- mutate(p1, DateVal = as.Date(Date, "%d/%m/%Y"))
  p1a <- filter(p1, DateVal >= as.Date('02/01/2007', "%m/%d/%Y") & DateVal <= as.Date('02/02/2007', "%m/%d/%Y"))
  p1a[complete.cases(p1a), ]  
}

rawData <- loadRawData('household_power_consumption.txt')
p1a <- readData_graph1(rawData)
par(mfrow=c(1,1))
hist(p1a$Global_active_power, col="red", xlab='Global Active Power (kilowatts)', main='Global Active Power')

# save png
dev.copy(png, file = "plot1.png") 
dev.off() 