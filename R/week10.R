# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
set.seed(331)

# Data Import and Cleaning
gss_tbl <- read_sav("../data/GSS2016.sav")