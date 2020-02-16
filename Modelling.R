load(file = "after_preprocessing.RData")

# splitting the data ------------------------------------------------------
#omit observations with NAs
combined_no_na <- combined_raw %>%
  na.omit()
print(paste("Deleted rows containing at least one NA:", nrow(combined_raw) - nrow(combined_no_na)))

split_date <- "2019-12-01"
train <- combined_no_na %>% 
  filter(as.Date(date) < split_date)
test <- combined_no_na %>% 
  filter(as.Date(date) > split_date)

train_folds <- rolling_origin(train,
                              initial = 365*24*3,
                              assess = 24*7*4,
                              cumulative = F,
                              skip = 24*7*4*4)

train_folds %>% 
  nrow()


# Model Training ----------------------------------------------------------

#define control_grid for all models
cntrl <-  control_grid(verbose = TRUE)

##### LM ######
#setup recipe with normalization of numerical variables
jannowitz_rec_lm <- recipe(jannowitz_n ~ ., data = train) %>%
  step_pca(starts_with("lag"), num_comp = 1) %>%
  step_num2factor(weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) %>%
  step_num2factor(hour, levels = as.character(0:23), transform = function(x) x+1) %>% 
  step_num2factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>%
  step_num2factor(week, levels = as.character(1:53)) %>% 
  step_dummy(weekday, hour, month, week, one_hot = TRUE) %>%
  step_normalize(all_predictors()) %>%
  prep()

bake(jannowitz_rec_lm, train) #bake recipe to see if it works

#lm model with glmnet
lm_mod <- linear_reg(mode = "regression") %>% 
  set_engine("lm")

#define workflow
lm_wflow <- workflow() %>%
  add_recipe(jannowitz_rec_lm) %>%
  add_model(lm_mod)

#run model with resampling
set.seed(456321)
initial_lm <- fit_resamples(lm_wflow, resamples = train_folds, control = cntrl)

#show best
initial_lm %>% 
  show_best(metric = "rmse", maximize = FALSE)

initial_lm %>% 
  unnest(.notes) %>% 
  select(.notes)

autoplot(initial_lm)



# knn ---------------------------------------------------------------------
#setup recipe with normalization of numerical variables
jannowitz_rec_knn <- recipe(jannowitz_n ~ ., data = train) %>%
  step_rm(date) %>%
  step_pca(starts_with("lag"), num_comp = 1) %>%
  step_num2factor(weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) %>%
  step_num2factor(hour, levels = as.character(0:23), transform = function(x) x+1) %>% 
  step_num2factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>%
  step_num2factor(week, levels = as.character(1:53)) %>% 
  step_dummy(weekday, hour, month, week, one_hot = T) %>%
  step_normalize(all_predictors()) %>% 
  prep()

bake(jannowitz_rec_lm, train) #bake recipe to see if it works

#knn model with kknn
knn_mod <- nearest_neighbor(mode = "regression",
                            neighbors = 5) %>% 
  set_engine("kknn")

#define workflow
knn_wflow <- workflow() %>%
  add_recipe(jannowitz_rec_knn) %>%
  add_model(knn_mod)

#run model with resampling
set.seed(456321)
#library(doParallel)
#cl <- makePSOCKcluster(parallel::detectCores(logical = T)-1)
#registerDoParallel(cl)
initial_knn <- tune_grid(knn_wflow, resamples = train_folds, control = cntrl, grid = 10)
#registerDoSEQ()
#showConnections()

#show best
initial_knn %>% 
  show_best(metric = "rmse", maximize = FALSE)

autoplot(initial_knn)


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
#xgboost
rf_mod <- rand_forest(mode = "regression", trees = tune(), mtry = tune()) %>% 
  set_engine(engine = "ranger")

rf_wflow <- workflow() %>%
  add_recipe(jannowitz_rec_rf) %>%
  add_model(rf_mod)



initial_rf <- tune_grid(rf_wflow, resamples = train_folds, grid = 10, control = cntrl)
#show best
initial_rf %>% 
  show_best(metric = "rmse", maximize = FALSE)

autoplot(initial_rf)

view(initial_rf %>% 
       unnest(.notes) %>% 
       count(.notes))

initial_rf %>% 
  select(.notes) %>% 
  unlist()


# xgboost -----------------------------------------------------------------

#setup recipe with normalization of numerical variables
jannowitz_rec_xgb <- recipe(jannowitz_n ~ ., data = train) %>%
  step_rm(date) %>% 
  step_num2factor(weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) %>%
  step_num2factor(hour, levels = as.character(0:23), transform = function(x) x+1) %>% 
  step_num2factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>%
  step_num2factor(week, levels = as.character(1:53)) %>%
  prep()

bake(jannowitz_rec_xgb, train) #bake recipe to see if it works
#xgboost
xgb_mod <- boost_tree(mode = "regression", trees = tune(), tree_depth = tune()) %>% 
  set_engine("xgboost")

xgb_wflow <- workflow() %>%
  add_recipe(jannowitz_rec_xgb) %>%
  add_model(xgb_mod)



initial_xgb <- tune_grid(xgb_wflow, resamples = train_folds, grid = 10, control = cntrl)
#show best
initial_xgb %>% 
  show_best(metric = "rmse", maximize = FALSE)

autoplot(initial_xgb)

view(initial_xgb %>% 
       unnest(.notes) %>% 
       count(.notes))

initial_lm %>% 
  select(.notes) %>% 
  unlist()


#jannowitz_lm <- linear_reg(mode = "regression", penalty = tune()) %>%
#  set_engine("lm")# %>%
#  fit(jannowitz_n ~ ., data = train)

#predict(jannowitz_lm, test) %>% 
#bind_cols(test) %>%
#  metrics(truth = jannowitz_n, estimate = .pred)


#create recipe
jannowitz_rec <-recipe(jannowitz_n ~ ., data = train)


#create a grid
knn_grid <- expand.grid(neighbors = c(15, 16, 17, 18, 19, 20, 21, 22, 23, 24))

#create model
jannowitz_knn <- nearest_neighbor(mode = "regression", neighbors = tune()) %>%
  set_engine("kknn")

#run model with resampling
set.seed(2132)
grid_results <- tune_grid(jannowitz_rec, model = jannowitz_knn, resamples = train_folds, grid = knn_grid)

initial_lm %>% 
  show_best(metric = "rmse", maximize = FALSE)

autoplot(initial_lm)
# caret -------------------------------------------------------------------
library(doParallel)
library(caret)
no_cores <- detectCores() - 1
# Initiate cluster
registerDoParallel(cores=no_cores)
set.seed(123)

#create a grid
xgb_grid <- expand.grid(nrounds = 2, 
                        max_depth = c(10), 
                        eta = c(0.001), 
                        gamma = c(1), 
                        colsample_bytree = c(0.7, 1.0), 
                        min_child_weight = c(0.5, 1),
                        subsample = c(1, 6, 13))


myTimeControl <- trainControl(method = "timeslice",
                              initialWindow = 365*24*3,
                              horizon = 24*7*4,
                              fixedWindow = FALSE,
                              skip = 24*7*4*3)

lmFitTime <- train(jannowitz_n ~ .,
                   data = train,
                   method = "lm",
                   trControl = myTimeControl,
                   tuneLength = 3)
varImp(lmFitTime)
summary(lmFitTime)

xgb_pred <- predict(lmFitTime, test)

test %>% 
  mutate(pred = predict(lmFitTime, test)) %>% 
  ggplot(aes(jannowitz_n, pred, color = hour))+
  geom_jitter(alpha = 0.5)

ggplot(aes(test$jannowitz_n, xgb_pred))+
  geom_jitter()
