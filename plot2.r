setwd("D:/GD/E-Books/COURSES/COURSERA/Exploratory data analysis")

lct <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")

df <- read.table("household_power_consumption.txt", sep = ";", na.strings = "?", header=T, stringsAsFactors = F)
df <- df[df$Date %in% c("1/2/2007", "2/2/2007"),]

df$DateTime <- strptime(paste(df$Date,df$Time), "%d/%m/%Y %H:%M:%S")

png(file="plot2.png", width=480,height=480)
plot(df$DateTime,df$Global_active_power,type="l",xlab="",ylab="Global Active Power (kilowatts)")
dev.off()

# reset
Sys.setlocale("LC_TIME", lct)
