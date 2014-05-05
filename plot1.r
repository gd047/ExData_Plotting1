setwd("D:/GD/E-Books/COURSES/COURSERA/Exploratory data analysis/ExData_Plotting1")


df <- read.table("household_power_consumption.txt", sep = ";", na.strings = "?", header=T, stringsAsFactors = F)


df <- df[df$Date %in% c("1/2/2007", "2/2/2007"),]


png(file="plot1.png", width=480,height=480)

hist(df$Global_active_power, main = "Global Active Power", col=2
     , xlab="Global Active Power (kilowatts)")

dev.off()


