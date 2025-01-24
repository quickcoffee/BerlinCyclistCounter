pacman::p_load(tidyverse, tidymodels, lubridate, readxl, DataExplorer, timeDate, tune, workflows, rcartocolor, ggmap, gganimate, ggrepel, plotly)
load(file = "data/after_preprocessing.RData")

# splitting the data ------------------------------------------------------
#omit observations with NAs (mostly missing weather data)
combined_no_na <- combined_raw %>%
  na.omit()
print(paste("Deleted rows containing at least one NA:", nrow(combined_raw) - nrow(combined_no_na)))

split_date <- "2019-10-31"
train <- combined_no_na %>% 
  filter(as.Date(date) < split_date)
test <- combined_no_na %>% 
  filter(as.Date(date) > split_date)

train_folds <- rolling_origin(train,
                                  initial = 24*365*2, #two years as a moving window
                                  assess = 24*7*8, #8 weeks of assessment
                                  cumulative = F,
                                  skip = 24*7*14) #moving the window by 14 weeks


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
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores(logical = T)-1)
registerDoParallel(cl)
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
initial_rf <- tune_grid(rf_wflow, resamples = train_folds, grid = 25, control = cntrl)
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



# evaluation --------------------------------------------------------------

rmse_best_models <- rbind(show_best(initial_lm, metric = "rmse", maximize = F, n = 1) %>% select(mean) %>% mutate(model = "lm"),
      show_best(initial_rf, metric = "rmse", maximize = F, n = 1)%>% select(mean) %>% mutate(model = "rF"))
rmse_best_models %>% 
  ggplot(aes(x=model, y=mean, fill=model))+
  geom_col()+
  labs(y="Mean RMSE for best candidate model across resamples",
       x="Model",
       fill="Model")

test_performace <- tibble(truth = test$jannowitz_n) %>% 
  mutate(lm = predict(lm_fitted, new_data = test)$.pred,
         rf = predict(rf_fitted, new_data = test)$.pred,
         week = as.factor(isoweek(test$date)),
         day = date(test$date),
         hour = hour(test$date))

scatter_residuals <- function(model, test_tibble=test_performace){
  mod_str <- deparse(substitute(model))
  mod_var <- enquo(model)
  ggplotly(
    test_tibble %>% 
      ggplot(aes(x=truth, y=!!mod_var, color = week,
                 text = paste0('Truth ', truth,
                              '<br>Prediction: ', round(!!mod_var),
                              '<br>Date: ', wday(day,label = T), " ",as.Date(day), " ", hour, ":00")))+
      geom_point()+
      geom_abline(slope=1, intercept=0)+
      labs(title = paste(mod_str,"on test set (01-11-2019 to 31-12-2019), RMSE:",
                         round(rmse(data= test_tibble, truth = truth,
                                    estimate = !!mod_var)[3], 2)),
           y="Predicted value",
           x="True value",
           color = "Week")+
      theme_light(),
    tooltip = c("text"))
}


scatter_residuals(lm) #zero values and not a great perforamce in general
scatter_residuals(rf) # good predictions, but outliers in week 52/1
