library(data.table)
library(dplyr)
library(pROC)
library(caret)
library(utils)
library(lubridate)
library(doParallel)
library(ggplot2)
library(grid)
library(jpeg)
library(RCurl)

#import data
raw <- read.csv("/Users/matt/Documents/kobe/data.csv", header = TRUE, sep = ",")

#move the target variable to the last
raw <- select(raw, -shot_made_flag, everything())


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
ymd(raw$game_date)
raw$shot_made_flag <- factor(ifelse(raw$shot_made_flag == 1,"Make shot","Missed shot"))

#see the structure
str(raw)

save(raw, file = "/Users/matt/Documents/kobe/raw.rda")

#new year column
raw$year <- year(raw$game_date)

sub <- subset(raw, year == 2016)

#create short chart for kobe
court_img <- "http://robslink.com/SAS/democd54/nba_court_dimensions.jpg"
court <- rasterGrob(readJPEG(getURLContent(court_img)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

# plot using NBA court background and colour by shot zone
ggplot(sub, aes(x=loc_x, y=loc_y)) + 
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(colour = shot_zone_basic, shape = shot_made_flag)) +
  xlim(-250, 250) +
  ylim(-50, 420)












