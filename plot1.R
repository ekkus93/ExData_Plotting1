library(dplyr)

readData_graph1 <- function(fileName) {
  p1 <- read.table('household_power_consumption.txt', header=TRUE, sep=';', na.strings='?')
  
  # gotcha - the date format is d/m/yyyy, not m/d/yyyy
  p1 <- mutate(p1, DateVal = as.Date(Date, "%d/%m/%Y"))
  p1a <- filter(p1, DateVal >= as.Date('02/01/2007', "%m/%d/%Y") & DateVal <= as.Date('02/02/2007', "%m/%d/%Y"))
  p1a[complete.cases(p1a), ]  
}

p1a <- readData_graph1('household_power_consumption.txt')
hist(p1a$Global_active_power, col="red", xlab='Global Active Power (kilowatts)', main='Global Active Power')
