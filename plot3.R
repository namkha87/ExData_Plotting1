library(data.table)

readData <- function () {
    data <- fread('household_power_consumption.txt', header = T, na.strings = c('?'))
    data$Date <- as.Date(data$Date, '%d/%m/%Y')
    min <- as.Date('01/02/2007', '%d/%m/%Y')
    max <- as.Date('02/02/2007', '%d/%m/%Y')
    data[Date >= min & Date <= max]
}

plot3 <- function (x) {
    timev <- strptime(paste(format(x$Date, '%d/%m/%Y'), x$Time), '%d/%m/%Y %H:%M:%S')
    colors <- c('black', 'red', 'blue')
    legends <- colnames(x)[7:9]
    
    plot(range(timev), range(x$Sub_metering_1, x$Sub_metering_2, x$Sub_metering_3), type = 'n', xlab = '', ylab = 'Energy sub meeting')
    for(i in seq(colors)) lines(timev, x[[6+i]], col = colors[i])
    legend("topright", col = colors, legend = legends, lty = 1, border = 1)
}

data <- readData()
png("plot3.png", 480, 480)
plot3(data)
dev.off()