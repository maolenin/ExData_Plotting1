# Exploratory Data Analysis

# Course Project 1

#Loading Libraries
library(lubridate)
library(tidyverse)
library(readr)

# Get the data Row with type of data by column
#types <- c(col_date(format="%d/%m/%Y"), col_time(format = "%H:%M:%S"), col_double(), col_double(), col_number(), col_double(), col_double(), col_double(), col_double())
dataraw <- read_delim("household_power_consumption.txt", delim = ";", na = "?",
                      col_types = cols(col_date(format="%d/%m/%Y"), col_time(format = "%H:%M:%S"),
                                      col_double(), col_double(), col_number(), col_double(), col_double(), col_double(), col_double()))

# Get the data work
power <- subset(dataraw, Date == "2007-02-01" | Date == "2007-02-02")

# Plot1
par(mfrow = c(1, 1), cex = 1.0)
hist(power$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowats)", ylab = "Frequency")
dev.copy(png, file="plot1.png", width = 480, height = 480)
dev.off()

# Plot2
power <- mutate(power, "DateTime" = ymd(Date) + hms(Time))
plot(Global_active_power ~ DateTime, data = power, type = "l", xlab = "", ylab = "Global Active Power (kilowats)")
dev.copy(png, file="plot2.png", width = 480, height = 480)
dev.off()

# Plot3
plot(Sub_metering_1 ~ DateTime, data = power, type = "l", xlab = "", ylab = "Energy sub metering")
lines(Sub_metering_2 ~ DateTime, data = power, col = "red", type = "l")
lines(Sub_metering_3 ~ DateTime, data = power, col = "blue", type = "l")
legend("topright", col = c("black", "red", "blue"), lty = c(1, 1, 1), legend = c(names(power[7:9])))
dev.copy(png, file="plot3.png", width = 480, height = 480)
dev.off()

#Plot4
par(mfrow = c(2, 2), cex = 0.7)
plot(Global_active_power ~ DateTime, data = power, type = "l", xlab = "", ylab = "Global Active Power")
plot(Voltage ~ DateTime, data = power, type = "l", xlab = "datetime", ylab = "Voltage")
plot(Sub_metering_1 ~ DateTime, data = power, type = "l", xlab = "", ylab = "Energy sub metering")
lines(Sub_metering_2 ~ DateTime, data = power, col = "red", type = "l")
lines(Sub_metering_3 ~ DateTime, data = power, col = "blue", type = "l")
legend("topright", col = c("black", "red", "blue"), lty = c(1, 1, 1), legend = c(names(power[7:9])), box.lty = 0, inset = 0.01)
plot(Global_reactive_power ~ DateTime, data = power, type = "l", xlab = "datetime")
dev.copy(png, file="plot4.png", width = 480, height = 480)
dev.off()