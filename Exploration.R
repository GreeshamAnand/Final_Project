library(ggplot2)
library(lubridate)
library(scales)
library(plyr)
library(readr)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

train$season  <- factor(train$season, labels = c("Winter", "Spring", "Summer", "Fall"))
train$weather <- factor(train$weather, labels = c("Good", "Normal", "Bad", "Very Bad"))
train$hour <- format(as.POSIXct(strptime(train$datetime,"%m/%d/%Y %H:%M",tz="")) ,format = "%H:%M")
train$dates <- format(as.POSIXct(strptime(train$datetime,"%m/%d/%Y %H:%M",tz="")) ,format = "%d/%m/%Y")
train$Weekday <- weekdays(as.Date(train$dates))

test$season  <- factor(test$season, labels = c("Winter", "Spring", "Summer", "Fall"))
test$weather <- factor(test$weather, labels = c("Good", "Normal", "Bad", "Very Bad"))
test$hour <- format(as.POSIXct(strptime(test$datetime,"%m/%d/%Y %H:%M",tz="")) ,format = "%H:%M")
test$dates <- format(as.POSIXct(strptime(test$datetime,"%m/%d/%Y %H:%M",tz="")) ,format = "%d/%m/%Y")
test$Weekday <- weekdays(as.Date(test$dates))


season_summary <- ddply(train,.(season,hour),summarise, count = mean(counting))
ggplot(train, aes(x = hour, y = count, colour = season)) +
  geom_point(data = season_summary, aes(group = season)) +
  geom_line(data = season_summary, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("LEAST RENTING IN WINTER, MOST IN SUMMER\n") + 
  theme(plot.title=element_text(size=14))

weather_summary <- ddply(train,.(weather,hour),
                         summarise, count = mean(counting))
ggplot(train, aes(x = hour, y = count, colour = weather)) +
  geom_point(data = weather_summary, aes(group = weather)) +
  geom_line(data = weather_summary, aes(group = weather)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("GOOD WEATHER = MORE BIKES RENTED.\n") + 
  theme(plot.title=element_text(size=14))


library(ggplot2)
library(lubridate)
library(scales)
library(plyr)
library(readr)

train <- read_csv("../input/train.csv")

train$season  <- factor(train$season, labels = c("Spring", "Summer", "Fall", "Winter"))
train$weather <- factor(train$weather, labels = c("Good", "Normal", "Bad", "Very Bad"))
train$hour    <- factor(hour(ymd_hms(train$datetime)))
train$times   <- as.POSIXct(strftime(ymd_hms(train$datetime), format="%H:%M:%S"), format="%H:%M:%S")
train$Weekday <- wday(ymd_hms(train$datetime), label=TRUE)

season_summary <- ddply(train,.(season,hour),
                        summarise, count = mean(counting))
ggplot(train, aes(x = hour, y = count, colour = season)) +
  geom_point(data = season_summary, aes(group = season)) +
  geom_line(data = season_summary, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("People rent bikes more in Fall, and much less in Spring.\n") + 
  theme(plot.title=element_text(size=18))

weather_summary <- ddply(train,.(weather,hour),
                         summarise, count = mean(counting))
ggplot(train, aes(x = hour, y = count, colour = weather)) +
  geom_point(data = weather_summary, aes(group = weather)) +
  geom_line(data = weather_summary, aes(group = weather)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("People rent bikes more when the weather is Good.\n") + 
  theme(plot.title=element_text(size=18))


day_summary <- ddply(train,.(Weekday,hour),
                     summarise,count = mean(counting))
ggplot(train, aes(x = hour, y = count, colour = Weekday)) +
  geom_point(data = day_summary, aes(group=Weekday)) +
  geom_line(data = day_summary, aes(group=Weekday)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("People rent bikes for morning/evening commutes on weekdays,
          and daytime rides on weekends\n")

weather_prob <- ddply(train,.(season, hour),
                      summarise, Good = mean(weather == "Good"),
                      Normal = mean(weather == "Normal"),
                      Bad = mean(weather == "Bad"),
                      Very_bad = mean(weather == "Very Bad"))


ggplot(train, aes(x = hour, y = Good, colour = season)) +
  geom_point(data = weather_prob, aes(group = season)) +
  geom_line(data = weather_prob, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Prob of Good") +
  theme_minimal() +
  ggtitle("The probability of Good weather is higher in all. \n") + 
  theme(plot.title=element_text(size=18))

ggplot(train, aes(x = hour, y = Normal, colour = season)) +
  geom_point(data = weather_prob, aes(group = season)) +
  geom_line(data = weather_prob, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Prob of Normal") +
  theme_minimal() +
  ggtitle("The probability of Normal weather is higher in Fall \n") + 
  theme(plot.title=element_text(size=18))

ggplot(train, aes(x = hour, y = Bad, colour = season)) +
  geom_point(data = weather_prob, aes(group = season)) +
  geom_line(data = weather_prob, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Prob of Bad") +
  theme_minimal() +
  ggtitle("The probability of Bad weather is higher in Summer and Winter. \n") + 
  theme(plot.title=element_text(size=18))

colfunc <- colorRampPalette(c("red", "blue"))


ggplot(train, aes(x = atemp, y = counting, color = humidity)) + geom_point() + scale_colour_gradientn(colours = colfunc(10))



