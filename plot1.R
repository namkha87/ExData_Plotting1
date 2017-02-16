library(data.table)

readData <- function () {
    data <- fread('household_power_consumption.txt', header = T, na.strings = c('?'))
    data$Date <- as.Date(data$Date, '%d/%m/%Y')
    min <- as.Date('01/02/2007', '%d/%m/%Y')
    max <- as.Date('02/02/2007', '%d/%m/%Y')
    data[Date >= min & Date <= max]
}

plot1 <- function (x) {
    hist(x$Global_active_power, main = 'Global Active Power', xlab = 'Global Active Power (kilowatts)', col = c("red"))
}

data <- readData()
png("plot1.png", 480, 480)
plot1(data)
dev.off()