pacman::p_load(tidyverse, tidymodels, lubridate, readxl, DataExplorer, timeDate, tune, workflows, rcartocolor, ggmap, gganimate, ggrepel)
load(file = "data/after_preprocessing.RData")

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
                              assess = 24*7*30,
                              cumulative = F,
                              skip = 24*7*6)

try_folds <- rolling_origin(test,
                              initial = 24*21,
                              assess = 24*1,
                              cumulative = F,
                              skip = 24*7)


#number of folds
train_folds %>% 
  nrow()


# Model Training ----------------------------------------------------------

#define control_grid for all models
cntrl <-  control_grid(verbose = TRUE, save_pred = FALSE, allow_par = TRUE)

# LM ----------------------------------------------------------------------
#setup recipe with normalization of numerical variables
jannowitz_rec_lm <- recipe(jannowitz_n ~ ., data = train) %>%
  step_rm(date) %>% 
  step_normalize(starts_with("lag")) %>% 
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
lm_mod <- linear_reg(mode = "regression", penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

#define workflow
lm_wflow <- workflow() %>%
  add_recipe(jannowitz_rec_lm) %>%
  add_model(lm_mod)

#run model with resampling
set.seed(456321)
initial_lm <- tune_grid(lm_wflow, resamples = train_folds, control = cntrl, grid = 20)

#show performance across resamples
initial_lm %>% 
  collect_metrics(summarize = F)
#show summarized performance across resamples
initial_lm %>% 
  collect_metrics(summarize = T)

initial_lm %>% 
  show_best(metric = "rmse", maximize = F)

autoplot(initial_lm)

saveRDS(initial_lm, file = "model_backup/initial_lm.rds")

lm_fitted <- initial_lm %>% 
  select_best(metric = "rmse", maximize = FALSE) %>% 
  finalize_workflow(x = lm_wflow) %>% 
  fit(data=train)

# MARS --------------------------------------------------------------------
#setup recipe with normalization of numerical variables
jannowitz_rec_mars <- recipe(jannowitz_n ~ ., data = train) %>%
  step_rm(date) %>% 
  step_normalize(starts_with("lag")) %>% 
  step_pca(starts_with("lag"), num_comp = 1) %>%
  step_num2factor(weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) %>%
  step_num2factor(hour, levels = as.character(0:23), transform = function(x) x+1) %>% 
  step_num2factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>%
  step_num2factor(week, levels = as.character(1:53)) %>% 
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  prep()

bake(jannowitz_rec_mars, train) #bake recipe to see if it works


#mars model
mars_mod <- mars(mode = "regression", num_terms = tune(), prod_degree = tune()) %>% 
  set_engine("earth")


#define workflow
mars_wflow <- workflow() %>%
  add_recipe(jannowitz_rec_mars) %>%
  add_model(mars_mod)

#run model with resampling
set.seed(456321)
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores(logical = T)-1)
registerDoParallel(cl)

initial_mars <- tune_grid(mars_wflow, resamples = train_folds, control = cntrl, grid = 20)

#show performance across resamples
initial_mars %>% 
  collect_metrics(summarize = F)
#show summarized performance across resamples
initial_mars %>% 
  collect_metrics(summarize = T)

#show best
initial_mars %>% 
  show_best(metric = "rmse", maximize = FALSE)

autoplot(initial_mars)

saveRDS(initial_mars, file = "model_backup/initial_mars.rds")

mars_fitted <- initial_mars %>% 
  select_best(metric = "rmse", maximize = FALSE) %>% 
  finalize_workflow(x = mars_wflow) %>% 
  fit(data=train)

# KNN ---------------------------------------------------------------------
#knn model - baseline
knn_mod <- nearest_neighbor(mode = "regression", neighbors = tune(), weight_func = tune()) %>% 
  set_engine("kknn")

#define workflow
knn_wflow <- workflow() %>%
  add_recipe(jannowitz_rec_lm) %>%
  add_model(knn_mod)

#create parameter grid for knn
knn_grid <-  grid_max_entropy(neighbors(range = c(1,20)),
                              weight_func(),
                            size = 20)

#run model with resampling
set.seed(456321)
initial_knn <- tune_grid(knn_wflow, resamples = train_folds, control = cntrl, grid = knn_grid)

#show performance across resamples
initial_knn %>% 
  collect_metrics(summarize = F) %>% 
  filter(.metric == "rmse")
#show summarized performance across resamples
initial_knn %>% 
  collect_metrics(summarize = T)

autoplot(initial_knn)

saveRDS(initial_knn, file = "model_backup/initial_knn.rds")

knn_fitted <- initial_knn %>% 
  select_best(metric = "rmse", maximize = FALSE) %>% 
  finalize_workflow(x = knn_wflow) %>% 
  fit(data=train)

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


set.seed(456321)
initial_rf <- tune_grid(rf_wflow, resamples = train_folds, grid = 20, control = cntrl)
#show best
initial_rf %>% 
  show_best(metric = "rmse", maximize = FALSE)

autoplot(initial_rf)

initial_rf %>% 
  collect_metrics(summarize = F)

saveRDS(initial_rf, file = "model_backup/initial_rf.rds")

rf_fitted <- initial_rf %>% 
  select_best(metric = "rmse", maximize = FALSE) %>% 
  finalize_workflow(x = rf_wflow) %>% 
  fit(data=train)

rf_test <- rf_fitted %>% 
  predict(new_data = test) %>% 
  mutate(truth = test$jannowitz_n,
         week = as.factor(isoweek(test$date)),
         day = date(test$date),
         hour = hour(test$date))
ggplotly(
rf_test %>% 
  ggplot(aes(x=truth, y=.pred, color = week,
             text = paste('Truth ', truth,
                          '<br>Prediction: ', round(.pred),
                          '<br>Date: ', as.Date(day), " ", hour, ":00")))+
  geom_point()+
    labs(title = paste("Random Forest on test set (02-12-2019 to 31-12-2019), RMSE: ", round(rmse(data= rf_test, truth = truth, estimate = .pred)[3], 2))), tooltip = c("text"))




# evaluation --------------------------------------------------------------

rmse_best_models <- rbind(show_best(initial_lm, metric = "rmse", maximize = F, n = 1) %>% select(mean) %>% mutate(model = "lm"),
      show_best(initial_mars, metric = "rmse", maximize = F, n = 1)%>% select(mean) %>% mutate(model = "MARS"),
      show_best(initial_knn, metric = "rmse", maximize = F, n = 1)%>% select(mean) %>% mutate(model = "KNN"),
      show_best(initial_rf, metric = "rmse", maximize = F, n = 1)%>% select(mean) %>% mutate(model = "rF"))
rmse_best_models %>% 
  ggplot(aes(x=model, y=mean, fill=model))+
  geom_col()+
  labs(y="Mean RMSE for best candidate model across resamples",
       x="Model",
       fill="Model")

test_performace <- tibble(truth = test$jannowitz_n) %>% 
  mutate(lm = predict(lm_fitted, new_data = test)$.pred,
         mars = predict(mars_fitted, new_data = test)$.pred,
         knn = predict(knn_fitted, new_data = test)$.pred,
         rf = predict(rf_fitted, new_data = test)$.pred,
         week = as.factor(isoweek(test$date)),
         day = date(test$date),
         hour = hour(test$date))

plot_residuals <- function(model, test_tibble=test_performace){
  mod_str <- deparse(substitute(model))
  mod_var <- enquo(model)
  ggplotly(
    test_tibble %>% 
      ggplot(aes(x=truth, y=!!mod_var, color = week,
                 text = paste('Truth ', truth,
                              '<br>Prediction: ', round(!!mod_var),
                              '<br>Date: ', as.Date(day), " ", hour, ":00")))+
      geom_point()+
      labs(title = paste(mod_str,"on test set (02-12-2019 to 31-12-2019), RMSE:",
                         round(rmse(data= test_tibble, truth = truth,
                                    estimate = !!mod_var)[3], 2)),
           y="Predictions",
           x="True value",
           color = "Week")+
      theme_light(),
    tooltip = c("text"))
}
plot_residuals(lm) #zero values and not agreat perforamce in general
plot_residuals(mars) # better performance but not great
plot_residuals(knn) # good RMSE but predictions seem very noisy
plot_residuals(rf) # very good predictions, but outliers in week 52/1

