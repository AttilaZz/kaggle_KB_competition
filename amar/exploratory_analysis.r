### This script aim to give a first dive into the Data, explore its features and discover first...
### insights that might be relevents for the analysis design to set up.


## let's load owr data from the data.csv file
data <- read.csv("data.csv", stringsAsFactors = FALSE)

## we have to divide owr data into train data ( obs. that have y diffrent from NA ) and test data (obs. that have y not available)
train <- data[!is.na(data$shot_made_flag),]
test <- data[is.na(data$shot_made_flag),]

## since the y predictive variable is an Integer class, we should make it as factor.
train$shot_made_flag <- as.factor(train$shot_made_flag)

## let's visualize attributes names and a readble structure of the entire data:

names(train)
str(train)
