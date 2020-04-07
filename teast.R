##unzip the data
if (!file.exists('activity.csv')) {
    unzip("activity.csv")
}
activitiesdata <-  read.csv("activity.csv")


#q1=--------------------------------------------------------------
library(ggplot2)

stepsperday <- aggregate(steps ~ date, data = activitiesdata, sum)
plot1 <- ggplot(stepsperday, aes(x = steps)) +
    geom_histogram(fill = "red4", bins = 15) +
    labs(title = "mean of total steps taken per day") +
    labs(x = "Total steps per day", y = "Frequency")
print(plot1)

paste("mean = ", round(mean(stepsperday$steps, na.rm = TRUE), 4))

paste("median = ", median(stepsperday$steps, na.rm = TRUE))
#q2-----------------------------------------------------------------
library(stringr)
inervalstepsmean <-
    aggregate(steps ~ interval, data = activitiesdata, mean)
inervalstepsmean$daytime <- as.character(inervalstepsmean$interval)
for (i in 1:length(inervalstepsmean$daytime)) {
    variable <- inervalstepsmean$daytime[i]
    if (str_length(variable) < 4) {
        pading <- strrep(0, (4 - str_length(variable)))
        variable <- paste(pading, variable, sep = "", collapse = "")
    }
    varhour  <- substr(variable, 1, 2)
    varmints <- substr(variable, 3, 4)
    variable <- paste(varhour, varmints, sep = "")
    inervalstepsmean$daytime[i] <- as.character(variable)
}
inervalstepsmean$daytime <-
    as.POSIXct(inervalstepsmean$daytime, format = "%H%M")

g <- ggplot(inervalstepsmean, aes(y = steps, x = daytime))
(
    g  + geom_line(col = "red4")
    + labs(y = "steps mean" , x = "day time")
    + labs(title = "mean of total steps taken per day time", lwd = 3)
    + scale_x_datetime(date_labels = "%H:%M", date_breaks = "3 hours")
)

inervalstepsmean[which.max(inervalstepsmean$steps), 1:2]


#q3--------------------------------------------------------
NaIntervals <-is.na(activitiesdata$steps)
NaForDays <-tapply(NaIntervals, activitiesdata$date, FUN = sum )
sum(NaIntervals)
NaForDays[which(NaForDays==288)]
mean(NaIntervals)
subset(NaForDays, NaForDays != 0 & NaForDays != 288 )
#--------
MissingDays <- names(NaForDays[which(NaForDays == 288)])
Daysval <- names(NaForDays[which(NaForDays==0)])
data <- data.frame(subset(activitiesdata, activitiesdata$date %in% Daysval ))
dates <- unique(activitiesdata$date)

newactivitiesdata <- data.frame(activitiesdata)
weeklist <- function(daydate){
    dayindex <- grep(daydate,dates)
    MissDindex <-grep(daydate,MissingDays)
    PreDaysval <-  (dayindex-MissDindex)
    NxtDaysval <-(dayindex-MissDindex+1)
    
    if ((PreDaysval)  < 4) {
        dayslist <- Daysval[1:6]
    } else if ((NxtDaysval)  > length(Daysval)-4) {
        dayslist <- Daysval[(length(Daysval)-5):length(Daysval)]   
    } else {
        dayslist <- Daysval[(PreDaysval-2):(NxtDaysval+2)]   
    }
    return(dayslist)
}

for (missday in MissingDays) {
    dayslist <- weeklist(missday)
    inervalsmean <- subset(data, data$date %in% dayslist )
    inervalsmean<- aggregate(steps ~ interval, data = inervalsmean, mean)
    missingdayindex <- grep(missday,newactivitiesdata$date)
    newactivitiesdata[missingdayindex,"steps"] <- inervalsmean$steps
}
#---------------------
stepsperday <-aggregate(steps ~ date, data = newactivitiesdata, sum)
g <-ggplot(stepsperday, aes(x=steps))

(g + geom_histogram( fill = "red4",bins = 15) +
        labs(title = "mean of total steps taken per day")+
        labs(x = "Total steps per day", y = "Frequency"))

paste("mean = ", round(mean(stepsperday$steps,na.rm = TRUE),4))
paste("median = ",median(stepsperday$steps,na.rm = TRUE))
#q4--------------------------------------------------------
weekday <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
weekends <- as.character(weekdays(as.POSIXct(newactivitiesdata$date)))
weekTrns <- function(dayvar) {
    if (dayvar %in% weekday) {
        dayt <- "Weekday" }
    else {
        dayt <-"Weekend" }
    return(dayt) }
weekends <- sapply(weekends, weekTrns)
newactivitiesdata$weekends <- weekends
dataByWE <- aggregate(steps ~ interval + weekends, data = newactivitiesdata, mean)
dataByWE$daytime <- as.character(dataByWE$interval)

for (i in 1:length(dataByWE$daytime)) {
    variable <- dataByWE$daytime[i]
    if (str_length(variable) < 4) {
        pading <- strrep(0, (4 - str_length(variable)))
        variable <- paste(pading, variable, sep = "", collapse = "")
    }
    varhour  <- substr(variable, 1, 2)
    varmints <- substr(variable, 3, 4)
    variable <- paste(varhour, varmints, sep = "")
    dataByWE$daytime[i] <- as.character(variable)
}
dataByWE$daytime <-as.POSIXct(dataByWE$daytime, format = "%H%M")

g <- ggplot(dataByWE, aes(y = steps, x = daytime,col = factor(weekends)))
(g  + geom_line()
    + facet_grid(.~factor(weekends)) 
    + labs(y = "steps mean" , x = "day time")
    + labs(title = "mean of total steps taken per day time", lwd = 3)
    + scale_x_datetime(date_labels = "%H:%M", date_breaks = "5 hours")
)