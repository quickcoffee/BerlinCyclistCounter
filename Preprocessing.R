pacman::p_load(tidyverse, tidymodels, lubridate, readxl, DataExplorer, timeDate, tune, workflows, rcartocolor, ggmap, gganimate, ggrepel, plotly)

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
ggmap::register_google(key = "XXX") #Maps API key

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
  scale_y_continuous(labels = comma)+
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
#temperature
weather_temperature <- load_format_weather("weather/data_TT_TU_MN009.csv", "temperature")

#combine everything
combined_raw <- bike_count_raw %>%
  left_join(weather_wind, by = c("date"))%>%
  left_join(weather_clouds, by = c("date"))%>%
  left_join(weather_precipitation, by = c("date"))%>%
  left_join(weather_humidity, by = c("date")) %>% 
  left_join(weather_temperature, by = c("date"))

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

#too hot?
ggplotly(
combined_raw %>%
  group_by(day = date(date), temperature = round(pmax(temperature))) %>% 
  summarise(n_day = sum(jannowitz_n)) %>% 
  ungroup() %>% 
  group_by(temperature) %>% 
  summarise(avg_day = mean(n_day)) %>% 
  ggplot(aes(y=avg_day, x=temperature, fill = temperature, text=paste('Maximal temperature: ', temperature, '°C',
                                                                      '<br>Average daily cyclists:', round(avg_day))))+
  rcartocolor::scale_fill_carto_c(palette = "SunsetDark", direction = -1)+
  geom_col()+
  theme_minimal(), tooltip = "text"))


  labs(title = "Average count of cyclists at Jannowitzbrücke, Berlin (01.04.2015-31.12.2019)",
       x = "Hour",
       y = "Average Count of Cyclists",
       fill = "Public Holiday?")+
  scale_y_continuous(labels = comma)

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
       fill = "Public Holiday?")+
  scale_y_continuous(labels = comma)

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
       fill = "Brückentag?")+
  scale_y_continuous(labels = comma)

#cyclist per year at jannowitz brücke
combined_raw %>%
  group_by(date = year(date)) %>% 
  summarise(jannowitz_n = sum(jannowitz_n)) %>%
  ggplot(aes(x=date, y=jannowitz_n))+
  geom_col(fill="#ef6548")+
  theme_light()+
  labs(title = "Cyclists per Year at the Jannowitz Counter Stations",
       y = "Numer of Cyclists",
       x = "Year")+
  scale_y_continuous(labels = comma)

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

save.image(file = "after_preprocessing.RData")

