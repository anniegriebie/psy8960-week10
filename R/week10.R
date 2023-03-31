# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
set.seed(331)

## Data Import and Cleaning
#reads in data, renames HRS1 to workhours, and removes anyone who has a missing value for workhours 
gss_tbl <- read_sav("../data/GSS2016.sav")%>%
  rename(workhours = HRS1) %>%
  filter(complete.cases(workhours))

#removing variables with equal to or more than 75% missingness, variables are columns so subset columns
gss_tbl <- gss_tbl[, which(colMeans(!is.na(gss_tbl)) >= 0.75)] %>%
mutate_all(as.numeric)