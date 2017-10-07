library(ggplot2)
library(plyr)
if(!file.exists("./Project1")){dir.create("./Project1")}
fileURL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL,destfile="./Project1/ProjectData.zip")
zipF<-file.choose("./Project1/ProjectData.zip") 
outDir<-"./Project1"
unzip(zipF,exdir=outDir)
list.files("./Project1")
setwd("./Project1")
df<-read.csv("activity.csv")
head(df)
dailysteps=aggregate(df$steps, by=list(date=df$date), FUN=sum, na.rm=TRUE)
qplot(dailysteps$x, geom="histogram") 
mean(dailysteps$x)
median(dailysteps$x)
timesteps=aggregate(df$steps, by=list(interval=df$interval), FUN=mean, na.rm=TRUE)
timesteps
ggplot(timesteps, aes(timesteps$interval, timesteps$x)) + geom_line() + xlab("Time Inteval") + ylab("Average Steps")
timesteps[which.max(timesteps$x),]
table(is.na(df)) 
## Replacing Missing Values
df$day <- weekdays(as.Date(df$date))
df$DateTime<- as.POSIXct(df$date, format="%Y-%m-%d")
clean <- df[!is.na(df$steps),]
## Create the average number of steps per weekday and interval
avgTable <- ddply(clean, .(interval, day), summarize, Avg = mean(steps))
## Create dataset with all NAs for substitution
nadata<- df[is.na(df$steps),]
## Merge NA data with average weekday interval for substitution
newdata<-merge(nadata, avgTable, by=c("interval", "day"))
## Reorder the new substituded data in the same format as clean data set
newdata2<- newdata[,c(6,4,1,2,5)]
colnames(newdata2)<- c("steps", "date", "interval", "day", "DateTime")
##Merge the NA averages and non NA data together
mergeData <- rbind(clean, newdata2)
head(mergeData)
dailysteps2=aggregate(mergeData$steps, by=list(date=mergeData$date), FUN=sum, na.rm=TRUE)
qplot(dailysteps2$x, geom="histogram") 
mean(dailysteps2$x)
median(dailysteps2$x)
## Create column for weekday and weekend
mergeData$DayCategory <- ifelse(mergeData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
head(mergeData)
daytypeAve=aggregate(cbind(steps)~DayCategory+interval, data=mergeData, mean, na.rm=TRUE)
ggplot(daytypeAve,aes(x=interval, y=steps)) +geom_line() +facet_wrap(~DayCategory)
