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

#removing variables with equal to or more than 75% missingness, variables are columns so subset columns also need variables in numeric later
gss_tbl <- gss_tbl[, which(colMeans(!is.na(gss_tbl)) >= 0.75)] %>%
mutate_all(as.numeric)


##Visualization 
#adding visualization, lets bins set to default at 30
ggplot(gss_tbl, aes(x = workhours)) +
  geom_histogram() +
  labs(x = "workhours", y="frequency", title= "Univariate Distribution of Workhours")

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

#creating a training vector to do 10 folds
folds <- createFolds(train_gss_tbl$workhours, 10)

#OLS model created #do I need to change the preprocess?
OLS <- train(
  workhours ~ .,
  train_gss_tbl,
  method = "lm",
  na.action = na.pass,
  preProcess = c("center", "scale", "nzv", "medianImpute"),
  trControl = trainControl(method="cv", indexOut = folds, number = 10, search = "grid", verboseIter=T)
)
#estimate of 10-fold CV
OLSR<-OLS$results$Rsquared
#estimate holdout CV, R^2 is the correlation squared
predictOLS <- predict(OLS, test_gss_tbl, na.action = na.pass)
OLSho <-(cor(test_gss_tbl$workhours, predictOLS))^2

#Elastic Net # Do I need to change the preprocess
ElasticNet <-
  train(
    workhours ~ .,
    train_gss_tbl, 
    method = "glmnet",
    na.action = na.pass,
    preProcess = c("center", "scale", "nzv", "medianImpute"),
    trControl = trainControl(method="cv", indexOut = folds, number = 10, search = "grid", verboseIter=T)
  )
#estimate of 10-fold CV. Took the mean because as 9 outputs
ElasticR <-mean(ElasticNet$results$Rsquared)
#estimate holdout CV, R^2 is the correlation squared
predictElastic <- predict(ElasticNet, test_gss_tbl, na.action = na.pass)
Elasticho <-(cor(test_gss_tbl$workhours, predictElastic))^2

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
#estimate of 10-fold CV
RandomR<- mean(RandomForest$results$Rsquared)
#estimate holdout CV
predictRandom <- predict(RandomForest, test_gss_tbl, na.action = na.pass)
Randomho <-(cor(test_gss_tbl$workhours, predictRandom))^2

#XGBoost
boost <-
  train(
    workhours ~ .,
    train_gss_tbl, 
    method = "xgbLinear",
    na.action = na.pass,
    preProcess = "medianImpute",
    trControl = trainControl(method="cv", indexOut = folds, number = 10, search = "grid", verboseIter=T)
  )
#estimate of 10-fold CV
boostR <- mean(boost$results$Rsquared)
#estimate holdout CV, R^2 is the correlation squared
predictboost <- predict(boost, test_gss_tbl, na.action = na.pass)
boostho <-(cor(test_gss_tbl$workhours, predictboost))^2

#Publication
algo = c("OLS", "Elastic Net", "Random Forest", "eXtreme Gradient Boosting")
cv_rsq <- c( str_remove(round(OLSR, 2), pattern = "^0"),
             str_remove(round(ElasticR, 2), pattern = "^0"),
             str_remove(round(RandomR, 2), pattern = "^0"),
             str_remove(round(boostR, 2), pattern = "^0")
)

ho_rsq <- c( str_remove(round(OLSho, 2), pattern = "^0"),
            str_remove(round(Elasticho, 2), pattern = "^0"),
            str_remove(round(Randomho, 2), pattern = "^0"),
            str_remove(round(boostho, 2), pattern = "^0")
)

#formating into tbl as specified in Canvas
table1_tbl <- tibble(
  algo, cv_rsq, ho_rsq
)

#1. Answers changed across models in that for the k-fold CV the OLS and Elastic Net models had smaller R^2 values than the Random forest and Extreme gradient boosting models.I think this patterns has to do with the number of parameters within each model, namely has the number of parameters increases, and the complexity of the models increases then the models have bigger R^2 values.
#2.Results changed between the k-fold CV and holdout CV in that the holdout CV had smaller R^2 values for each of the models compared to the k-fold CV R^2 values. I think this happened because I believe in k-fold CV the data was divided into 10 data subsets so each time that one of the 10-subsets was used as a testing set to train the model thus leading to less variability. 
#3.Among the four models I would choose the Random Forest model for a real-life prediction problem. The main reason why is because the k-fold CV R^2 value for the Random Forest model was large for the 10-fold CV and also had a larger R^2 value for the holdout CV. Although this is a benefit, one potential trade off of using the Random Forest model is the length of time that it took to run the model. However, the time it took to run the Random Forest model was still less than the time it took to run the Extreme Gradient Boost model for the models I specified.