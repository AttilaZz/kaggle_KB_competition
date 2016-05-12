### This script aim to give a first dive into the Data, explore its features and discover first...
### insights that might be relevents for the analysis design to set up.


## let's load owr data from the data.csv file
data <- read.csv("data.csv", stringsAsFactors = TRUE)

data$game_event_id  <- as.factor(data$game_event_id )
data$game_id <- as.factor(data$game_id)
data$period <- as.factor(data$period)
data$shot_made_flag <- as.factor(data$shot_made_flag)
data$team_id <- as.factor(data$team_id)
data$shot_id <- as.factor(data$shot_id)
data$playoffs <- as.factor(data$playoffs)

## we have to divide owr data into train data ( obs. that have y diffrent from NA ) and test data (obs. that have y not available)
train <- data[!is.na(data$shot_made_flag),]
test <- data[is.na(data$shot_made_flag),]

## since the y predictive variable is an Integer class, we should make it as factor.
data$shot_made_flag <- as.factor(data$shot_made_flag)


library(dplyr)
library(ggplot2)



# a plot to see position by feature
combined_shot_type_plot <- function() {
        f1 <- train[train$combined_shot_type == "Jump Shot",]
        f2 <- train[train$combined_shot_type != "Jump Shot",]

        ggplot() +
                geom_point(data = f1,
                           aes(x = lon, y = lat), color = "grey", alpha = 0.3, size = 2) +
                geom_point(data = f2,
                           aes(x = lon, y = lat, 
                               color = shot_made_flag), alpha = 0.7, size = 3) +
                ylim(c(33.7, 34.0883)) +
                scale_color_brewer(palette = "Set1") +
                theme_void() +
                ggtitle("Shot Types")
}

filtered <- train[train$combined_shot_type != "Jump Shot" & train$shot_distance < 5,]
sum(as.numeric(filtered$shot_made_flag))


# a plot to see position by feature
combined_shot_type_with_distance_plot <- function() {
        
        filtered <- train[train$combined_shot_type != "Jump Shot" & train$shot_distance < 5,]
        ggplot() +
                geom_point(data = filtered,
                           aes(x =loc_x ,y = loc_y,
                               color =shot_made_flag),
                           alpha = 0.7, size = 3) +
                scale_color_brewer(palette = "Set1") +
                geom_point(aes(x =0, y = 0), size = 5, shape = 4) +
                theme_void() +
                ggtitle("Shots from up close (5) : 9376 : 1  | 3615 : 0 ")
}


train$x_bins <- cut(train$loc_x, breaks = 25)


courtplot <- function(feat) {
        feat <- substitute(feat)
        train %>% 
                ggplot(aes(x = lon, y = lat)) +
                geom_point(aes_q(color = feat), alpha = 0.7, size = 3) +
                ylim(c(33.7, 34.0883)) +
                scale_color_brewer(palette = "Set1") +
                theme_void() +
                ggtitle(paste(feat))
}

pplot <- function(feat) {
        feat <- substitute(feat)
        ggplot(data = train, aes_q(x = feat)) +
                geom_bar(aes(fill = shot_made_flag), stat = "count", position = "fill") +
                scale_fill_brewer(palette = "Set1", direction = -1) +
                ggtitle(paste("accuracy by", feat))
        
}


pplot(x_bins) + geom_bar() + ggtitle("Shot Distribution by x_bins") +
        theme(axis.text.x = element_blank())


pplot(x_bins) + theme(axis.text.x = element_blank())




## understand the spacial features :

courtplot(shot_zone_area)
courtplot(shot_zone_basic)
courtplot(shot_zone_range)

## understand the time features :

pplot(minutes_remaining) 
pplot(period)
pplot(seconds_remaining)
pplot(seconds_remaining) + geom_bar() + ggtitle("Histogram of Shots by second_remaining")

## understand the distance and season features :
pplot(season) + coord_flip()
pplot(shot_distance) + xlim(0, 60)  + scale_fill_brewer(palette = "Set1", direction = -2)

## understand shot types features :
pplot(combined_shot_type)
pplot(shot_type)
pplot(shot_zone_area) + coord_flip()
pplot(shot_zone_basic) + coord_flip()

## what about the opponent
pplot(opponent) + coord_flip()