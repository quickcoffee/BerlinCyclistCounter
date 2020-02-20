load(file = "after_preprocessing.RData")

# splitting the data ------------------------------------------------------
#omit observations with NAs (mostly missing weather data)
combined_no_na <- combined_raw %>%
  na.omit()
print(paste("Deleted rows containing at least one NA:", nrow(combined_raw) - nrow(combined_no_na)))

split_date <- "2019-12-01"
train <- combined_no_na %>% 
  filter(as.Date(date) < split_date)
test <- combined_no_na %>% 
  filter(as.Date(date) > split_date)

train_folds <- rolling_origin(train,
                              initial = 24*365*3,
                              assess = 24*7,
                              cumulative = F,
                              skip = 24*7*9)


#number of folds
train_folds %>% 
  nrow()


# Model Training ----------------------------------------------------------

#define control_grid for all models
cntrl <-  control_grid(verbose = TRUE, save_pred = TRUE, allow_par = TRUE)

##### LM ######
#setup recipe with normalization of numerical variables
jannowitz_rec_lm <- recipe(jannowitz_n ~ ., data = train) %>%
  step_pca(starts_with("lag"), num_comp = 1) %>%
  step_num2factor(weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) %>%
  step_num2factor(hour, levels = as.character(0:23), transform = function(x) x+1) %>% 
  step_num2factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>%
  step_num2factor(week, levels = as.character(1:53)) %>% 
  step_dummy(weekday, hour, month, week, one_hot = TRUE) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  prep()

bake(jannowitz_rec_lm, train) #bake recipe to see if it works

#lm model - baseline
lm_mod <- linear_reg(mode = "regression") %>% 
  set_engine("lm")

#define workflow
lm_wflow <- workflow() %>%
  add_recipe(jannowitz_rec_lm) %>%
  add_model(lm_mod)

#run model with resampling
set.seed(456321)
initial_lm <- fit_resamples(lm_wflow, resamples = train_folds, control = cntrl)

#show performance across resamples
initial_lm %>% 
  collect_metrics(summarize = F)
#show summarized performance across resamples
initial_lm %>% 
  collect_metrics(summarize = T)


# MARS --------------------------------------------------------------------
#setup recipe with normalization of numerical variables
jannowitz_rec_mars <- recipe(jannowitz_n ~ ., data = train) %>%
  step_pca(starts_with("lag"), num_comp = 1) %>%
  step_num2factor(weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) %>%
  step_num2factor(hour, levels = as.character(0:23), transform = function(x) x+1) %>% 
  step_num2factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>%
  step_num2factor(week, levels = as.character(1:53)) %>% 
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  prep()

bake(jannowitz_rec_mars, train) #bake recipe to see if it works

<<<<<<< HEAD
#mars model
mars_mod <- mars(mode = "regression", num_terms = tune(), prod_degree = tune()) %>% 
  set_engine("earth")
=======
#knn model with kknn
knn_mod <- nearest_neighbor(mode = "regression",
                            neighbors = tune()) %>%
  set_engine("kknn")
>>>>>>> 78b46e1514626106d3d1564451ea75a2b9be6eb5

#define workflow
mars_wflow <- workflow() %>%
  add_recipe(jannowitz_rec_mars) %>%
  add_model(mars_mod)

#define parameter grid
knn_grid <- grid_regular(neighbors() %>% range_set(c(2,21)), levels = c(20))

#run model with resampling
set.seed(456321)
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores(logical = T)-1)
registerDoParallel(cl)
<<<<<<< HEAD
initial_mars <- fit_resamples(mars_wflow, resamples = train_folds, control = 20)
=======
initial_knn <- tune_grid(knn_wflow, resamples = train_folds, control = cntrl, grid = knn_grid)


#show best
initial_knn %>% 
  show_best(metric = "rmse", maximize = FALSE)

autoplot(initial_knn)
>>>>>>> 78b46e1514626106d3d1564451ea75a2b9be6eb5

#show performance across resamples
initial_mars %>% 
  collect_metrics(summarize = F)
#show summarized performance across resamples
initial_mars %>% 
  collect_metrics(summarize = T)

saveRDS(initial_mars, file = "mars.rds")
# random forest -----------------------------------------------------------

#setup recipe with normalization of numerical variables
jannowitz_rec_rf <- recipe(jannowitz_n ~ ., data = train) %>%
  step_rm(date) %>% 
  step_num2factor(weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) %>%
  step_num2factor(hour, levels = as.character(0:23), transform = function(x) x+1) %>% 
  step_num2factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>%
  step_num2factor(week, levels = as.character(1:53)) %>%
  prep()

bake(jannowitz_rec_rf, train) #bake recipe to see if it works
#rf model
rf_mod <- rand_forest(mode = "regression", trees = tune(), mtry = tune(), min_n = tune()) %>% 
  set_engine(engine = "ranger")

rf_wflow <- workflow() %>%
  add_recipe(jannowitz_rec_rf) %>%
  add_model(rf_mod)

<<<<<<< HEAD
set.seed(456321)
=======

>>>>>>> 78b46e1514626106d3d1564451ea75a2b9be6eb5
initial_rf <- tune_grid(rf_wflow, resamples = train_folds, grid = 20, control = cntrl)
#show best
initial_rf %>% 
  show_best(metric = "rmse", maximize = FALSE)

autoplot(initial_rf)

initial_rf %>% 
  collect_metrics(summarize = F)

saveRDS(initial_rf, file = "rf.rds")
# xgboost -----------------------------------------------------------------

#setup recipe with normalization of numerical variables
jannowitz_rec_xgb <- recipe(jannowitz_n ~ ., data = train) %>%
  step_rm(date) %>% 
  step_num2factor(weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) %>%
  step_num2factor(hour, levels = as.character(0:23), transform = function(x) x+1) %>% 
  step_num2factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>%
  step_num2factor(week, levels = as.character(1:53)) %>%
  step_dummy(all_nominal(), one_hot = T) %>% 
  prep()

bake(jannowitz_rec_xgb, train) #bake recipe to see if it works
#xgboost
xgb_mod <- boost_tree(mode = "regression", trees = tune(), tree_depth = tune(), mtry = tune(), learn_rate = tune()) %>% 
  set_engine("xgboost")

xgb_wflow <- workflow() %>%
  add_recipe(jannowitz_rec_xgb) %>%
  add_model(xgb_mod)



initial_xgb <- tune_grid(xgb_wflow, resamples = train_folds, grid = 30, control = cntrl)

<<<<<<< HEAD
=======
table(initial_xgb %>% 
  unnest(.notes) %>% 
    select(.notes))
>>>>>>> 78b46e1514626106d3d1564451ea75a2b9be6eb5

#show best
initial_xgb %>% 
  show_best(metric = "rmse", maximize = FALSE)

autoplot(initial_xgb)


saveRDS(initial_xgb, file = "xgb.rds")




