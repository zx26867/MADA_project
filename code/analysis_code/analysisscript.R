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
library(tidyverse)
library(ggplot2)
library(rpart.plot)
library(rpart)
library(vip)
library(glmnet)
library(ranger)
library(mlbench)
library(kernlab)
library(kknn)


#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)

# turn categorical data into ordinal data
mydata$Age = mydata$Age %>% factor(order = TRUE, levels = c("18-24", "25-34", "35-44","45-54","55-64"))
mydata$YearsProExp = mydata$YearsProExp %>% factor(order = TRUE, levels = c("1 year or less", "2 - 4 years","5-7 years","8 - 10 years","11 - 20 years","21 - 30 years","31 - 40 years"))
mydata$YearsExp = mydata$YearsExp %>% factor(order = TRUE, levels = c("1 year or less", "2 - 4 years","5-7 years","8 - 10 years","11 - 20 years","21 - 30 years","31 - 40 years"))
mydata$Education = mydata$Education %>% factor(order = TRUE, levels = c("High School","Some college","College degree","Master's degree","PhD","Professional degree (MD, JD, etc.)"))

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

set.seed(111)
folds <- vfold_cv(train_data, v = 5, repeats = 5, strata = Salary)
#
folds

# receipe for model with all predictors
data_rec <- recipe(Salary ~ ., data = train_data) %>% step_dummy(all_nominal())

################## decision tree model  #################
tune_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
  ) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

tune_spec

tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 5)

tree_grid

set.seed(111)

tree_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_recipe(data_rec)

tree_res <- 
  tree_wf %>% 
  tune_grid(
    resamples = folds,
    grid = tree_grid
  )

tree_res

a = tree_res %>% collect_metrics()
tree_res %>% autoplot()

best_tree <- tree_res %>% select_best("rmse")

best_tree

final_wf <- tree_wf %>% finalize_workflow(best_tree)

final_fit <- final_wf %>% fit(train_data) 

df_tr <- final_fit %>% augment(train_data) %>% select(.pred, Salary) %>% mutate(residue = Salary - .pred)
df_tr$id <- seq.int(nrow(df_tr))
ggplot() + geom_point(data = df_tr, aes(x = id, y = Salary), color = "blue") + geom_point(data = df_tr, aes(x = id, y = .pred), color = "red") 
# blue curve is the actual, red curve is predicted

ggplot(df_tr, aes(x = id, y = residue)) + geom_point()
# residue plots

a = a %>% filter(.metric=="rmse") %>% arrange(mean)
a

################ random forest model ###############
cores <- parallel::detectCores()
cores

#rf_grid  <- expand.grid(mtry = c(3, 4, 5, 6), min_n = c(40,50,60))

rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("regression")

rf_workflow <- 
  workflow() %>% 
  add_recipe(data_rec) %>% 
  add_model(rf_mod) 

set.seed(111)
rf_res <- 
  rf_workflow %>% 
  tune_grid(resamples = folds,
            grid = 25,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(rmse))

c = rf_res %>% collect_metrics()
rf_res %>% autoplot()

best_rf <- rf_res %>% select_best("rmse")

best_rf

final_wf_rf <- rf_workflow %>% finalize_workflow(best_rf)

final_fit_rf <- final_wf_rf %>% fit(train_data) 

df_rf <- final_fit_rf %>% augment(train_data) %>% select(.pred, Salary) %>% mutate(residue = Salary - .pred)
df_rf$id <- seq.int(nrow(df_rf))
ggplot() + geom_point(data = df_rf, aes(x = id, y = Salary), color = "blue") + geom_point(data = df_rf, aes(x = id, y = .pred), color = "red")
# blue curve is the actual, red curve is predicted

ggplot(df_rf, aes(x = id, y = residue)) + geom_point()
# residue plots

c = c %>% filter(.metric=="rmse") %>% arrange(mean)
c

################## SVM Model ################

svm_mod <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_mode("regression") %>%
  set_engine("kernlab")

svm_rec <-
  recipe(Salary ~ ., data = train_data)  %>%
  # remove any zero variance predictors
  step_zv(all_predictors()) %>% 
  # remove any linear combinations
  step_lincomb(all_numeric())

svm_workflow <- 
  workflow() %>% 
  add_model(svm_mod) %>% 
  add_recipe(data_rec)

set.seed(111)
recipe_res <-
  svm_mod %>% 
  tune_grid(
    svm_rec,
    resample = folds,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(rmse)
  )

show_best(recipe_res, metric = "rmse")

d = recipe_res %>% collect_metrics()
recipe_res %>% autoplot()

best_svm <- recipe_res %>% select_best("rmse")

best_svm

final_wf_svm <- svm_workflow %>% finalize_workflow(best_svm)

final_fit_svm <- final_wf_svm %>% fit(train_data) 

df_svm <- final_fit_svm %>% augment(train_data) %>% select(.pred, Salary) %>% mutate(residue = Salary - .pred)
df_svm$id <- seq.int(nrow(df_svm))
ggplot() + geom_point(data = df_svm, aes(x = id, y = Salary), color = "blue") + geom_point(data = df_svm, aes(x = id, y = .pred), color = "red")
# blue curve is the actual, red curve is predicted

ggplot(df_svm, aes(x = id, y = residue)) + geom_point()
# residue plots

d = d %>% filter(.metric=="rmse") %>% arrange(mean)
d

################# knn model ####################
knn_recipe <- data_rec %>% step_scale(all_predictors()) %>% step_center(all_predictors())

knn_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = tune()) %>% set_engine("kknn") %>% set_mode("regression")
knn_wkflw <- workflow() %>% add_recipe(knn_recipe) %>% add_model(knn_spec)

gridvals <- tibble(neighbors = seq(from = 1, to = 200, by = 3))

knn_results <- knn_wkflw %>% tune_grid(resamples = folds, grid = gridvals)

show_best(knn_results, metric = "rmse")

e = knn_results %>% collect_metrics()
knn_results %>% autoplot()

best_knn <- knn_results %>% select_best("rmse")

best_knn

final_wf_knn <- knn_wkflw %>% finalize_workflow(best_knn)

final_fit_knn <- final_wf_knn %>% fit(train_data) 

df_knn <- final_fit_knn %>% augment(train_data) %>% select(.pred, Salary) %>% mutate(residue = Salary - .pred)
df_knn$id <- seq.int(nrow(df_knn))
ggplot() + geom_point(data = df_knn, aes(x = id, y = Salary), color = "blue") + geom_point(data = df_knn, aes(x = id, y = .pred), color = "red")
# blue curve is the actual, red curve is predicted

ggplot(df_knn, aes(x = id, y = residue)) + geom_point()
# residue plots

e = e %>% filter(.metric=="rmse") %>% arrange(mean)
e

############# LASSO model ##############

lr_mod <- linear_reg(penalty = tune(), mixture = 1) %>% set_engine("glmnet") %>% set_mode("regression")

lr_workflow <- workflow() %>% add_model(lr_mod) %>% add_recipe(data_rec)

lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

lr_res <- 
  lr_workflow %>% 
  tune_grid(resamples = folds,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(rmse))

b = lr_res %>% collect_metrics()
lr_res %>% autoplot()

best_lr <- lr_res %>% select_best("rmse")

best_lr

final_wf_lr <- lr_workflow %>% finalize_workflow(best_lr)

final_fit_lr <- final_wf_lr %>% fit(train_data) 

df_lr <- final_fit_lr %>% augment(train_data) %>% select(.pred, Salary) %>% mutate(residue = Salary - .pred)
df_lr$id <- seq.int(nrow(df_lr))
ggplot() + geom_point(data = df_lr, aes(x = id, y = Salary), color = "blue") + geom_point(data = df_lr, aes(x = id, y = .pred), color = "red")
# blue curve is the actual, red curve is predicted

ggplot(df_lr, aes(x = id, y = residue)) + geom_point()
# residue plots

b = b %>% filter(.metric=="rmse") %>% arrange(mean)
b

#save data frame table to file for later use in manuscript
#summarytable_file = here("results", "summarytable.rds")
#saveRDS(summary_df, file = summarytable_file)

# save fit results table  
#table_file = here("results", "resulttable.rds")
#saveRDS(lmtable, file = table_file)

  