library(data.table)
library(dplyr)
library(pROC)
library(caret)
library(utils)
library(lubridate)
library(doParallel)

#import data
raw <- read.csv("/Users/matt/Documents/kobe/data.csv", header = TRUE, sep = ",")

#move the target variable to the last
raw <- select(raw, -shot_made_flag, everything())

raw$game_event_id <- NULL
raw$game_id <- NULL
raw$team_id <- NULL
raw$team_name <- NULL
raw$shot_id <- NULL
raw$matchup <- NULL
raw$game_date <- NULL
raw$action_type <- NULL

#define data types
raw$action_type <- factor(raw$action_type, ordered = FALSE)
raw$combined_shot_type <- factor(raw$combined_shot_type, ordered = FALSE)
raw$period <- factor(raw$period, ordered = FALSE)
raw$playoffs <- factor(ifelse(raw$playoffs == 1,"y","n"))
raw$shot_type <- factor(raw$shot_type, ordered = TRUE)
raw$shot_zone_area <- factor(raw$shot_zone_area, ordered = FALSE)
raw$shot_zone_basic <- factor(raw$shot_zone_basic, ordered = FALSE)
raw$shot_zone_range <- factor(raw$shot_zone_range, ordered = FALSE)
raw$season <- factor(raw$season, ordered= FALSE)
raw$opponent <- factor(raw$opponent, ordered = FALSE)
#raw$game_date <- lapply(raw$game_date, ymd)
raw$shot_made_flag <- factor(ifelse(raw$shot_made_flag == 1,"y","n"))


#see the structure
str(raw)

#dummy-vised

dummies <- dummyVars(data = raw, shot_made_flag ~ .)

dummies2 <- predict(dummies, newdata = raw) %>% 
  data.table()

done <- cbind.data.frame(dummies2, raw$shot_made_flag)

#split into test and train set
train <- dplyr::filter(done, !is.na(V2))

test <- dplyr::filter(done, is.na(V2))

test$V2 <- NULL

#split train set
set.seed(1234)

splitindex <- createDataPartition(train$V2, p = 0.7, list = FALSE)

train_set <- train[splitindex,]

valid_set <- train[-splitindex,]

# see if need to balance data
print(prop.table(table(train_set$V2))*100) #no need

#establish train parameters
train_control <- trainControl(method = "adaptive_cv",
                              number = 5,
                              repeats = 2,
                              savePredictions = TRUE,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              verboseIter = TRUE,
                              returnResamp = "none",
                              search = "random")

#create model based on different methods:
#set time
ptm <- proc.time()

set.seed(1045)
# #parallel
# cl <- makeCluster(detectCores())
# 
# registerDoParallel(cl)

model <- train(data = train_set,
               V2~.,
               trControl = train_control,
               method = "rf",
               tuneLength = 5,
               preProcess = c("center", "scale","pca"),
               verbose = TRUE,
               size = 3,
               metric = "ROC")

