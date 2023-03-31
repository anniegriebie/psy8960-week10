# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)

## Data Import and Cleaning
#reads in data, renames HRS1 to workhours, and removes anyone who has a missing value for workhours 
gss_tbl <- read_sav("../data/GSS2016.sav")%>%
  rename(workhours = HRS1) %>%
  filter(complete.cases(workhours))

#removing variables with equal to or more than 75% missingness, variables are columns so subset columns
gss_tbl <- gss_tbl[, which(colMeans(!is.na(gss_tbl)) >= 0.75)] %>%
mutate_all(as.numeric)



##Visualization 
#adding visualization
ggplot(gss_tbl, aes(x = workhours)) +
  geom_histogram() +
  labs(x = "workhours")

## Analysis

#set seed for reproducibility
set.seed(331)

#creating train and holdout samples using technique demonstrated in DataCamp
rows <- sample(nrow(gss_tbl))
shuffled_data <- gss_tbl[rows,]
split <-round(nrow(shuffled_data) *0.75)
train_gss_tbl <- shuffled_data[1:split, ] %>%
  mutate_all(as.numeric)
test_gss_tbl <- shuffled_data[(split +1): nrow(shuffled_data),] 

#creating a training vector to do 
#10 folds
folds <- createFolds(train_gss_tbl$workhours, 10)
#is this the right thing to use, should be using a different fold function?

#Creating gss_control to input into trControl for each model so don't have to redo it each time 
gss_control <- trainControl(method="cv", indexOut = folds, number = 10, search = "grid", verboseIter=T) # For some reason models wont run with gss_control variable come back and figure out

#Creating gss_process to input into preProcess for each model so don't have to redo it each time 
gss_process <- c("medianImpute","nzv", "center", "scale") #also not working so manually added, delete this if cant get to work. 

#OLS ready
OLS <- train(
  workhours ~ .,
  train_gss_tbl,
  method = "lm",
  na.action = na.pass,
  preProcess = "medianImpute",
  trControl = trainControl(method="cv", indexOut = folds, number = 10, search = "grid", verboseIter=T)
)
summary(OLS)

#Elastic Net
ElasticNet <-
  train(
    workhours ~ .,
    train_gss_tbl, 
    method = "glmnet",
    na.action = na.pass,
    preProcess = "medianImpute",
    trControl = trainControl(method="cv", indexOut = folds, number = 10, search = "grid", verboseIter=T)
  )
summary(ElasticNet)

#Random forest
RandomForest <-
  train(
    workhours ~ .,
    train_gss_tbl, 
    method = "ranger",
    na.action = na.pass,
    preProcess = "medianImpute",
    trControl = trainControl(method="cv", indexOut = folds, number = 10, search = "grid", verboseIter=T)
  )
summary(RandomForest)


#XGBoost
boost <-
  train(
    workhours ~ .,
    train_gss_tbl, 
    method = "xgbTree",
    na.action = na.pass,
    preProcess = "medianImpute",
    trControl = trainControl(method="cv", indexOut = folds, number = 10, search = "grid", verboseIter=T)
  )
summary(boost)

#Publication
