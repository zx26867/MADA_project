###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(tidymodels) # for modeling

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)

# This enables the analysis to be reproducible when random numbers are used 
set.seed(111)
# Put 3/4 of the data into the training set
data_split <- initial_split(mydata, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

all_prdtr_rec <- recipe(Salary ~ ., data = train_data) 
single_prdtr_rec <- recipe(Salary ~ Education, data = train_data)
lm_mod <- linear_reg() %>% set_engine("lm")

single_workflow <- 
  workflow() %>% 
  add_model(lm_mod) %>% 
  add_recipe(single_prdtr_rec) 

#model fitting to training data 
single_fit <- 
  single_workflow %>% 
  fit(data = train_data)

# make some predictions on test data
predict(single_fit, test_data)

salary_predict_single <- augment(single_fit, test_data)
salary_predict_single %>% select(Salary,.pred)

#evaluate rmse to see how good it fits the data
salary_predict_single %>% rmse(truth = Salary, .pred) 

#### try all predictor's linear model ###
all_workflow <- 
  workflow() %>% 
  add_model(lm_mod) %>% 
  add_recipe(all_prdtr_rec) 

#model fitting to training data 
all_fit <- 
  all_workflow %>% 
  fit(data = train_data)

# make some predictions on test data
predict(all_fit, test_data)

salary_predict_all <- augment(all_fit, test_data)
salary_predict_all %>% select(Salary,.pred)

#evaluate rmse to see how good it fits the data
salary_predict_all %>% rmse(truth = Salary, .pred)

# Null model: use mean to predict everything
pred_by_mean = salary_predict_all
pred_by_mean$.pred = mean(pred_by_mean$Salary)
pred_by_mean %>% rmse(truth = Salary, .pred)
# rmse for null model > single predictor model > all predictor model


#save data frame table to file for later use in manuscript
#summarytable_file = here("results", "summarytable.rds")
#saveRDS(summary_df, file = summarytable_file)


# save fit results table  
#table_file = here("results", "resulttable.rds")
#saveRDS(lmtable, file = table_file)

  