library(data.table)

readData <- function () {
    data <- fread('household_power_consumption.txt', header = T, na.strings = c('?'))
    data$Date <- as.Date(data$Date, '%d/%m/%Y')
    min <- as.Date('01/02/2007', '%d/%m/%Y')
    max <- as.Date('02/02/2007', '%d/%m/%Y')
    data[Date >= min & Date <= max]
}


plotVoltage <- function (x) {
    timev <- strptime(paste(format(x$Date, '%d/%m/%Y'), x$Time), '%d/%m/%Y %H:%M:%S')
    plot(range(timev), range(x$Voltage), type = 'n', xlab = 'datetime', ylab = 'Voltage')
    lines(timev, x$Voltage, col = 'black')
}

plotGlobalActivePower <- function (x) {
    timev <- strptime(paste(format(x$Date, '%d/%m/%Y'), x$Time), '%d/%m/%Y %H:%M:%S')
    plot(range(timev), range(x$Global_active_power), type = 'n', ylab = 'Global Active Power (kilowatts)', xlab = '')
    lines(timev, x$Global_active_power, col = 'black')
}

plotGlobalReactivePower <- function (x) {
    timev <- strptime(paste(format(x$Date, '%d/%m/%Y'), x$Time), '%d/%m/%Y %H:%M:%S')
    plot(range(timev), range(x$Global_reactive_power), type = 'n', ylab = 'Global Reactive Power', xlab = 'datetime')
    lines(timev, x$Global_reactive_power, col = 'black')
}

plotEnergySubMeeting <- function (x) {
    timev <- strptime(paste(format(x$Date, '%d/%m/%Y'), x$Time), '%d/%m/%Y %H:%M:%S')
    colors <- c('black', 'red', 'blue')
    legends <- colnames(x)[7:9]
    
    plot(range(timev), range(x$Sub_metering_1, x$Sub_metering_2, x$Sub_metering_3), type = 'n', xlab = '', ylab = 'Energy sub meeting')
    for(i in seq(colors)) lines(timev, x[[6+i]], col = colors[i])
    legend("topright", col = colors, legend = legends, lty = 1, bty = 'n')
}

plot4 <- function (x) {
    par(mfrow=c(2,2))
    plotGlobalActivePower(x)
    plotVoltage(x)
    plotEnergySubMeeting(x)
    plotGlobalReactivePower(x)
}

data <- readData()
png("plot4.png", 480, 480)
plot4(data)
dev.off()