library(data.table)
library(dplyr)
library(pROC)
library(caret)
library(utils)
library(lubridate)
library(doParallel)

#import data
raw <- read.csv("F:/R/kobe/data.csv", header = TRUE, sep = ",")

#move the target variable to the last
raw <- select(raw, -shot_made_flag, everything())

raw$game_event_id <- NULL
raw$game_id <- NULL
raw$team_id <- NULL
raw$team_name <- NULL
raw$shot_id <- NULL
raw$matchup <- NULL
raw$game_date <- NULL

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


#split into test and train set
train <- dplyr::filter(raw, !is.na(shot_made_flag))

test <- dplyr::filter(raw, is.na(shot_made_flag))

test$shot_made_flag <- NULL

#split train set
set.seed(1234)

splitindex <- createDataPartition(train$shot_made_flag, p = 0.7, list = FALSE)

train_set <- train[splitindex,]

valid_set <- train[-splitindex,]

# see if need to balance data
print(prop.table(table(train_set$shot_made_flag))*100) #no need

#establish train parameters
train_control <- trainControl(method = "repeatedcv",
                              repeats = 3,
                              number = 10,
                              savePredictions = TRUE,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary)

#create model based on different methods:
#set time
ptm <- proc.time()

set.seed(1045)
#parallel
cl <- makeCluster(detectCores())

registerDoParallel(cl)

model <- train(data = train_set,
               shot_made_flag~.,
               trControl = train_control,
               method = "glmboost",
               tuneLength = 5,
               preProcess = c("center", "scale","pca"),
               allowParallel = TRUE,
               verbose = TRUE,
               size = 3,
               metric = "ROC")

#summary model
summary(model)

#stop Cluster
stopCluster(cl)

#stop recording time
time <- proc.time() - ptm

#train set performance
probs <- predict(model,newdata = train_set, type ="prob")

pred  <- factor(ifelse(probs[,"y"] > 0.5,"y","n"))

summary(pred)

matrix <- confusionMatrix(data = pred, 
                          train_set$Survived,
                          positive = levels(train_set$Survived)[2]) %>% 
  print()

rocCurve  <- roc(response = train_set$Survived,
                 predictor = probs[,"y"],
                 levels = levels(train_set$Survived))

curve <- plot(rocCurve, print.thres = c(.5), type = "S",
              print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
              print.thres.cex = .8,
              legacy.axes = TRUE)

curve#AUC


