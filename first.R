library(data.table)
library(dplyr)
library(pROC)
library(caret)
library(utils)

raw <- fread("F:/R/kobe/data.csv")

#move the target variable to the last
raw <- select(raw, -shot_made_flag, everything())

raw$game_event_id <- NULL
raw$game_id <- NULL
raw$team_id <- NULL
raw$team_name <- NULL

#define data types
raw$action_type <- factor(raw$action_type, ordered = FALSE)
raw$combined_shot_type <- factor(raw$combined_shot_type, ordered = FALSE)
raw$period <- factor(raw$period, ordered = FALSE)
raw$playoffs <- factor(ifelse(raw$playoffs == 1,"y","n"))
raw$shot_type <- factor(raw$shot_type, ordered = TRUE)
raw$shot_zone_area <- factor(raw$shot_zone_area, ordered = FALSE)
raw$shot_zone_basic <- factor(raw$shot_zone_basic, ordered = FALSE)



#split into test and train set
train <- dplyr::filter(raw, !is.na(shot_made_flag))

test <- dplyr::filter(raw, is.na(shot_made_flag))

test$shot_made_flag <- NULL


