pacman::p_load(tidyverse, tidymodels, lubridate, readxl, DataExplorer, timeDate, tune, workflows, rcartocolor, ggmap, gganimate, ggrepel)

# Read in XLSX file (only counts)-------------------------------------------------------
path <-  "Gesamtdatei_Stundenwerte_2012-2019.xlsx"
sheetnames <- excel_sheets(path)[7:11]
sheets_list <- lapply(excel_sheets(path)[7:11], read_excel, path = path, guess_max = 8000)

# name the dataframes
bike_count_raw <- bind_rows(sheets_list) %>% #combine sheets into one dataframe
  select(date = 1, jannowitzbrücke_north = 2, jannowitzbrücke_south = 3) %>% #select only date + jannowitzbrücke counters (noth + south)
  mutate(jannowitz_n = jannowitzbrücke_north+jannowitzbrücke_south) %>%  #aggreagte one variable for noth and south counter
  select(date, jannowitz_n) %>% 
  filter(date > as.Date("2015-04-01")) #filter out NA's before the counters where installed


# Read in XLSX file (incl station locations -------------------------------
sheetnames_loc <- excel_sheets(path)[3]
counter_loc <- read_excel(path = path, sheet = sheetnames_loc) %>% 
  rename(id = Zählstelle, name = 2, lat = 3, lon = 4, date_of_installation = 5)

year_last_installation <- year(as.Date(max(counter_loc$date_of_installation)))

all_counts <- bind_rows(sheets_list) %>%
  purrr::set_names(~ str_replace_all(. ,pattern = "([:blank:][:digit:]{2}\\.[:digit:]{2}\\.[:digit:]{4})", replacement = "")) %>%
  rename(date = 1) %>% 
  tidyr::gather("station_id", "n", 2:27) %>% #making datset tidy --> one observation per row, one variable per column
  na.omit() %>%
  mutate(weekday = wday(date, label = TRUE, abbr = TRUE))

# background maps
ggmap::register_google(key = "AIzaSyDNncjwcg8vxRIzMXZzP1hCi2rDvzgclrQ") #Maps API key

#maps map
berlin_map <- get_googlemap(center = c(lon = 13.4, lat = 52.5),
                              zoom = 11, scale = 4,
                              maptype ='roadmap',
                              color = 'bw')
#plot map with all bike counter stations in berlin (incl. Jannowitz stations)
ggmap(berlin_map)+
  geom_point(data = counter_loc %>% mutate(target = ifelse(str_detect(string = name, pattern = "^Jannowitz"), "Yes", "No")), aes(x = lon, y=lat, color = target), size = 3)+
  geom_label_repel(data = counter_loc, aes(label = name), size = 2)+
  theme_nothing()
  

#grouped by month and year (only from 2017 - when all stations were installed)
p_animated_all_conts <- all_counts %>%
  filter(year(date) > year_last_installation) %>%
  group_by(year = as.factor(year(date)), month = month(date, label = TRUE)) %>% 
  summarise(n = sum(n)) %>% 
  ggplot(aes(x=month, y=n, group=year, colour=year))+ 
  geom_line(size=1.5)+
  theme_minimal()+
  labs(title = "Cyclists per Month in Berlin",
  subtitle= "Based on data from the city's counter stations between 2017 and 2019",
       y = "Number of Cyclists per Month",
       x = "Month",
       color = "Year")+
  gganimate::transition_reveal(as.numeric(month))
gganimate::animate(p_animated_all_conts, end_pause = 20)

  
# feature engineering -----------------------------------------------------

#function to load and rename the value column
load_format_weather <- function(path, value){
  weather_df <- read_csv(path) %>% 
    select(date = Zeitstempel, Wert) %>% 
    rename_(.dots = setNames("Wert", value))
  weather_df$date <- as.POSIXct(strptime(weather_df$date, "%Y%m%d%H%M%OS"))
  return(weather_df)
}

#wind
weather_wind <- load_format_weather("weather/data_FF_MN008.csv", "wind_speed")
#clouds
weather_clouds <- load_format_weather("weather/data_N_MN008.csv", "cloud_coverage") %>% 
  mutate(cloud_coverage = ifelse(cloud_coverage == -1, NA, cloud_coverage))
#precipitation
weather_precipitation <- load_format_weather("weather/data_R1_MN008.csv", "precipitation")
#humidity
weather_humidity <- load_format_weather("weather/data_RF_TU_MN009.csv", "humidity")

#combine everything
combined_raw <- bike_count_raw %>%
  left_join(weather_wind, by = c("date"))%>%
  left_join(weather_clouds, by = c("date"))%>%
  left_join(weather_precipitation, by = c("date"))%>%
  left_join(weather_humidity, by = c("date"))

#load public holidays (via spiketime API)
berlin_holidays <- read.csv("https://www.spiketime.de/feiertagapi/feiertage/csv/2015/2019", sep = ";") %>% 
  filter(Land == "Berlin") %>% #filter only for berlin holidays
  select(date = Datum)

#create holiday dummy variable and date variables (month, weekday, hour)
`%not_in%` <- purrr::negate(`%in%`)
combined_raw <- combined_raw %>%
  mutate(is_holiday = ifelse(as.Date(date) %in% as.Date(berlin_holidays$date), 1, 0), #holiday dummy variable
         tomorrow_is_holiday = ifelse((as.Date(date)+1) %in% as.Date(berlin_holidays$date), 1, 0),
         weekday = wday(date, week_start = 1), #weekday
         week = week(date), #week no
         is_brueckentag = ifelse(((as.Date(date)-1) %in% as.Date(berlin_holidays$date) & weekday == 5 & (as.Date(date) %not_in% as.Date(berlin_holidays$date))) | #create "brückentag" (Monday/Friday before a holiday) dummy variable
                                  ((as.Date(date)+1) %in% as.Date(berlin_holidays$date) & weekday == 1 & (as.Date(date) %not_in% as.Date(berlin_holidays$date))),
                                 1, 0),
         hour = hour(date), #hour
         month = month(date)) #month

#fix missing value in jannowitz_n
combined_raw %>% 
  filter(is.na(jannowitz_n)) %>% 
  select(date) #missing value on 2019-10-28

october_mondays <- combined_raw %>%
  filter(weekday == 1 & month == 10 & is_holiday == 0 & is_brueckentag == 0 & hour == 4) %>%
  na.omit() %>%
  select(jannowitz_n) #get similar hours (all mondays in october)

combined_raw$jannowitz_n[is.na(combined_raw$jannowitz_n)] <- mean(october_mondays$jannowitz_n) #replace missing value with mean of october mondays at 4am

#add lagged variable
combined_raw <- combined_raw %>% 
  mutate(lag_last_month = lag(jannowitz_n, n=7*24*4),
         lag_last_week = lag(jannowitz_n, n=7*24))

# data exploration --------------------------------------------------------
#explore missings and histogram
skimr::skim(combined_raw) #--> one missing for jannowitz_n
DataExplorer::plot_missing(combined_raw)
DataExplorer::plot_histogram(combined_raw)

#holiday vs no holiday
combined_raw %>%
  group_by(hour, is_holiday) %>%
  summarise(avg_n = mean(jannowitz_n)) %>% 
  ggplot(aes(x=hour, y=avg_n, fill=plyr::revalue(as.factor(is_holiday), c("1" = "Yes", "0" = "No"))))+
  geom_col()+
  theme_minimal()+
  labs(title = "Average count of cyclists at Jannowitzbrücke, Berlin (01.04.2015-31.12.2019)",
       x = "Hour",
       y = "Average Count of Cyclists",
       fill = "Public Holiday?")

#brückentag effect
combined_raw %>%
  filter(weekday < 6) %>% #only weekdays
  group_by(hour, is_brueckentag) %>%
  summarise(avg_n = mean(jannowitz_n)) %>% 
  ggplot(aes(x=hour, y=avg_n, fill=plyr::revalue(as.factor(is_brueckentag), c("1" = "Yes", "0" = "No"))))+
  geom_col()+
  theme_minimal()+
  labs(title = "The Brückentag Effect:\nAverage count of cyclists at Jannowitzbrücke, Berlin (01.04.2015-31.12.2019)",
       x = "Hour",
       y = "Average Count of Cyclists",
       fill = "Brückentag?")
  


#
combined_raw %>%
  group_by(date = year(date)) %>% 
  summarise(jannowitz_n = sum(jannowitz_n)) %>%
  ggplot(aes(x=date, y=jannowitz_n))+
  geom_line()

#heatmap weekday/hour
combined_raw %>%
  mutate(weekday_lable = wday(x=date, label = T, abbr = F, week_start = 1)) %>% 
  group_by(weekday_lable, hour) %>%
  summarise(avg_n = mean(jannowitz_n)) %>%
  ggplot(aes(x=hour, y=weekday_lable))+
  geom_tile(aes(fill=avg_n),colour = "white")+
  rcartocolor::scale_fill_carto_c(palette = "SunsetDark", direction = -1)+
  theme_bw() +
  theme_minimal()+
  labs(title = "Average count of cyclists at Jannowitzbrücke, Berlin (01.04.2015-31.12.2019)",
       x = "Hour",
       y = "Weekday",
       fill = "Average Count")

#correlation plot
cor(combined_raw %>% select(-date), use = "complete.obs") %>% 
  corrplot::corrplot()


# splitting the data ------------------------------------------------------
#omit observations with NAs
combined_no_na <- combined_raw %>%
  na.omit()

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
  step_rm(date) %>% 
  step_num2factor(weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) %>%
  step_num2factor(hour, levels = as.character(0:23), transform = function(x) x+1) %>% 
  step_num2factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>%
  step_num2factor(week, levels = as.character(1:53)) %>% 
  step_dummy(weekday, hour, month, week) %>%
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
  #step_log(jannowitz_n, lag_last_month, lag_last_week) %>% 
  step_num2factor(weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) %>%
  step_num2factor(hour, levels = as.character(0:23), transform = function(x) x+1) %>% 
  step_num2factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) %>%
  step_num2factor(week, levels = as.character(1:53)) %>% 
  step_dummy(weekday, hour, month, week) %>%
  step_normalize(all_predictors()) %>% 
  prep()

bake(jannowitz_rec_lm, train) #bake recipe to see if it works

#knn model with kknn
knn_mod <- nearest_neighbor(mode = "regression",
                            neighbors = tune()) %>% 
  set_engine("kknn")

#define workflow
knn_wflow <- workflow() %>%
  add_recipe(jannowitz_rec_knn) %>%
  add_model(knn_mod)

#run model with resampling
set.seed(456321)
library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores(logical = T)-1)
registerDoParallel(cl)
initial_knn <- tune_grid(knn_wflow, resamples = train_folds, control = cntrl, grid = 10)

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
