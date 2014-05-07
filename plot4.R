
lct <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")

df <- read.table("../household_power_consumption.txt", sep = ";", na.strings = "?", header=T, stringsAsFactors = F)
df <- df[df$Date %in% c("1/2/2007", "2/2/2007"),]

df$DateTime <- strptime(paste(df$Date,df$Time), "%d/%m/%Y %H:%M:%S")


png(filename = "plot4.png", width = 480, height = 480)

par(mfcol=c(2,2))

plot(df$DateTime,df$Global_active_power,type="l",xlab="",ylab="Global Active Power (kilowatts)")

plot(df$DateTime,df$Sub_metering_1,type="l",xlab="",ylab="Energy sub metering")
lines(df$DateTime,df$Sub_metering_2,col="red")
lines(df$DateTime,df$Sub_metering_3,col="blue")
legend("topright",c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
       col = c("black", "red", "blue"),lty = 1, cex=.8, bty="n")

plot(df$DateTime,df$Voltage,pch=".",type="l",ylab="Voltage", xlab="daytime")
plot(df$DateTime,df$Global_reactive_power ,pch=".",type="l",ylab="Global_reactive_power ", xlab="daytime")

dev.off()

# reset
Sys.setlocale("LC_TIME", lct)