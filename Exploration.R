
library(ggplot2)
library(lubridate)

train <- read.csv("train.csv")
test <- read.csv("test.csv")


train$season  <- factor(train$season, labels = c("Spring", "Summer", "Fall", "Winter"))
train$weather <- factor(train$weather, labels = c("Good", "Normal", "Bad", "Very Bad"))
train$hour    <- factor(hour(ymd_hms(train$datetime)))
train$times   <- as.POSIXct(strftime(ymd_hms(train$datetime), format="%H:%M:%S"), format="%H:%M:%S")
train$Weekday <- wday(ymd_hms(train$datetime), label=TRUE)


test$season  <- factor(test$season, labels = c("Spring", "Summer", "Fall", "Winter"))
test$weather <- factor(test$weather, labels = c("Good", "Normal", "Bad", "Very Bad"))
test$hour    <- factor(hour(ymd_hms(test$datetime)))
test$times   <- as.POSIXct(strftime(ymd_hms(test$datetime), format="%H:%M:%S"), format="%H:%M:%S")
test$Weekday <- wday(ymd_hms(test$datetime), label=TRUE)

library(randomForest)

extractFeatures <- function(data) {
  features <- c("season",
                "holiday",
                "workingday",
                "weather",
                "temp",
                "atemp",
                "humidity",
                "windspeed",
                "hour")
  data$hour <- hour(ymd_hms(data$datetime))
  return(data[,features])
}

trainfea <- extractFeatures(train)
testfea  <- extractFeatures(test)

submission <- data.frame(datetime=test$datetime, count=NA)

# We only use past data to make predictions on the test set, 
# so we train a new model for each test set cutoff point
for (i_year in unique(year(ymd_hms(test$datetime)))) {
  for (i_month in unique(month(ymd_hms(test$datetime)))) {
    cat("Year: ", i_year, "\tMonth: ", i_month, "\n")
    testLocs   <- year(ymd_hms(test$datetime))==i_year & month(ymd_hms(test$datetime))==i_month
    testSubset <- test[testLocs,]
    trainLocs  <- ymd_hms(train$datetime) <= min(ymd_hms(testSubset$datetime))
    rf <- randomForest(extractFeatures(train[trainLocs,]), train[trainLocs,"count"], ntree=100)
    submission[testLocs, "count"] <- predict(rf, extractFeatures(testSubset))
  }
}

write.csv(submission, file = "1_random_forest_submission.csv", row.names=FALSE)

# Train a model across all the training data and plot the variable importance
rf <- randomForest(extractFeatures(train), train$count, ntree=100, importance=TRUE)
imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("Importance") +
  ylab("") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))

ggsave("2_feature_importance.png", p)