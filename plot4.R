plot4 <- function(){

    ##### Data already downloaded to directory #####

    # read data
    cols <- read.table("household_power_consumption.txt", sep=";", nrows=1)
    dat  <- read.table("household_power_consumption.txt", sep=";", skip=1)
    names(dat) <- as.character(unlist(cols[1,]))

    # cut data to just 1/2/2007 to 2/2/2007
    ## no "?" errors in these dates!
    datsplit <- split(dat, dat[,1])
    datdate1 <- data.frame(datsplit["1/2/2007"])
    names(datdate1) <- as.character(unlist(cols[1,]))
    datdate2 <- data.frame(datsplit["2/2/2007"])
    names(datdate2) <- as.character(unlist(cols[1,]))
    datdate  <- rbind(datdate1,datdate2)

    # read.table returns factors, and as.numeric gets level #s, so:
    gap    <-as.numeric(levels(datdate[,3]))[datdate[,3]]
    submet1<-as.numeric(levels(datdate[,7]))[datdate[,7]]
    submet2<-as.numeric(levels(datdate[,8]))[datdate[,8]]
    volt   <-as.numeric(levels(datdate[,5]))[datdate[,5]]
    grp    <-as.numeric(levels(datdate[,4]))[datdate[,4]]
    # was a number anyway
    submet3<-as.numeric(datdate$Sub_metering_3)
        
    # make time in minutes
    minute <- c(1:2880)

    # plot 4: 4 plots
    par(mfrow= c(2,2))

    ## plot 2 repeated
    plot(minute, gap, type="l",
         ylab = "Global Active Power (kilowatts)", 
         xaxt="n", xlab="")
    axis(1, at=c(0, 1440, 2880), labels = c("Thur", "Fri", "Sat") )

    ## new voltage plot
    plot(minute, volt, type="l",
         ylab = "Voltage", xlab="datetime",xaxt="n")
    axis(1, at=c(0, 1440, 2880), labels = c("Thur", "Fri", "Sat") )
    
    ## plot 3 repeated, legend modified
    plot(minute, submet1, type="n", ylab="Energy sub metering",
         xlab="",xaxt="n")
    axis(1, at=c(0, 1440, 2880), labels = c("Thur", "Fri", "Sat") )
    points(minute, submet1, type="l")
    points(minute, submet2, type="l", col="red")
    points(minute, submet3, type="l", col="blue")
    legend("top", col=c("black","red","blue"), 
           legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
           lty=c(1,1,1), bty="n", cex = 0.4)
    
    ## new global reactive power plot
    plot(minute, grp, type="l",
         ylab = "Global reactive power", xlab="datetime", xaxt="n")
    axis(1, at=c(0, 1440, 2880), labels = c("Thur", "Fri", "Sat") )

    ###### Save as PNG using EXPORT in RStudio ######

}