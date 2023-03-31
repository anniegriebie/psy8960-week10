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

#creating a training vector to do 
#10 folds
index <- createFolds(train_gss_tbl$workhours, k=10)
train_gss_tbl <-
test_gss_tbl <-

  
#Creating gss_control to input into trControl for each model so don't have to redo it each time 
gss_control <- trainControl(method= "cv", number = "10", search = "grid", verboseIter = T, indexOut = index)

#Creating gss_process to input into preProcess for each model so don't have to redo it each time 
gss_process <- c("medianImpute","nzv", "center", "scale")

#OLS ready
OLS <- train(
  workhours ~ .,
  train_gss_tbl,
  method ="lm",
  preProcess = gss_process,
  na.acton = na.pass,
  trControl = gss_control)
summary(OLS)

#Elastic Net
ElasticNet <-
  train(
    workhours ~ .,
    train_gss_tbl, 
    method = "glmnet",
    preProcess = gss_process,
    na.action = na.pass,
    trControl = gss_control)
summary(ElasticNet)

#Random forest
RandomForest <-
  train(
    workhours ~ .,
    train_gss_tbl, 
    method = "ranger",
    preProcess = gss_process,
    na.action = na.pass,
    trControl = gss_control)
summary(RandomForeset)


#XGBoost
boost <-
  train(
    workhours ~ .,
    train_gss_tbl, 
    method = "xgbTree",
    preProcess = gss_process,
    na.action = na.pass,
    trControl = gss_control)
summary(boost)

#Publication
