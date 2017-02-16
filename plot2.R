library(data.table)

readData <- function () {
    data <- fread('household_power_consumption.txt', header = T, na.strings = c('?'))
    data$Date <- as.Date(data$Date, '%d/%m/%Y')
    min <- as.Date('01/02/2007', '%d/%m/%Y')
    max <- as.Date('02/02/2007', '%d/%m/%Y')
    data[Date >= min & Date <= max]
}

plot2 <- function (x) {
    timev <- strptime(paste(format(x$Date, '%d/%m/%Y'), x$Time), '%d/%m/%Y %H:%M:%S')
    plot(range(timev), range(x$Global_active_power), type = 'n', ylab = 'Global Active Power (kilowatts)', xlab = )
    lines(timev, x$Global_active_power, col = 'black')
}

data <- readData()
png("plot2.png", 480, 480)
plot2(data)
dev.off()