## Select code copied over from dataWrangling.R, written by Gillian McGinnis
## Created 21 October 2020
## Updated 21 October 2020


## Loading necessary libraries, data, and functions

library(tidyverse) ## Needed for dplyr, strinr, etc.
library(lubridate) ## Needed for time conversions since our data is in milliseconds.

dataClinton <- read_csv("data/dataClinton.csv")
dataWoodstock <- read_csv("data/dataWoodstock.csv")
dataTimes <- read_csv("data/dataTimes.csv")

all_na <- function(x) any(!is.na(x)) ## New function to drop NAs


## Times

dataTimes_final <- dataTimes %>%
  mutate(Sensor = case_when(
    Name == "Cristina" ~ "S04",
    Name != "Cristina" ~ Sensor)) %>% ## Fixing a mislabeled set of sensors
  filter(Sensor != "S00") %>% ## Removing example
  select(Location, Sublocation, Sensor, Start) %>% ## Removing irrelevant columns
  mutate(sub = case_when(
    Sublocation == "Const" ~ "const",
    Sublocation == "Bike" ~ "bike",
    Sublocation == "Ped" ~ "ped",
    Sublocation == "Firehydrant" ~ "fire",
    Sublocation == "front of library" ~ "lib",
    Sublocation == "martin" ~ "mart")) %>% ## Shortening sublocation names
  select(-Sublocation) %>%
  unite("alttag", c(Sensor, sub), sep = "_", remove = FALSE) %>% ## Might be useful later
  rename_all(tolower) ## I like my columns lowercase


## Clinton

dataClinton_long <- dataClinton %>%
  select_if(all_na) %>% ## Removing empty rows
  pivot_longer(cols =! contains("time"),
               names_to = "tag",
               values_to = "pmValues") %>%
  mutate(pmType = substr(tag, start = 4, stop = 7),
         location = substr(tag, start = 9, stop = 13),
         sensor = substr(tag, start = 1, stop = 3)) %>% ## Creates new columns based on sensor number and PM type.
  unite("alttag", c(sensor, location), sep = "_", remove = FALSE) %>%
  mutate(timestamp = case_when(
    sensor == "S17" & location == "const" ~ S17time_const,
    sensor == "S17" & location == "bike" ~ S17time_bike,
    sensor == "S17" & location == "ped" ~ S17time_ped,
    sensor == "S04" & location == "const" ~ S04time_const,
    sensor == "S04" & location == "bike" ~ S04time_bike,
    sensor == "S04" & location == "ped" ~ S04time_ped,
    sensor == "S16" & location == "const" ~ S16time_const,
    sensor == "S16" & location == "bike" ~ S16time_bike,
    sensor == "S16" & location == "ped" ~ S16time_ped,
    sensor == "S02" & location == "const" ~ S02time_const,
    sensor == "S02" & location == "bike" ~ S02time_bike,
    sensor == "S02" & location == "ped" ~ S02time_ped))

dataClinton_times <- left_join(dataClinton_long, dataTimes_final, by = "alttag")

dataClinton_final <- dataClinton_times %>%
  mutate(timestampSec = timestamp/1000,
         fullTime = start + timestampSec,
         hmsTime = hms::hms(seconds_to_period((fullTime)))) %>%
  select(tag, pmValues, pmType, alttag, location.x, sensor.x, hmsTime) %>%
  rename(location = location.x,
         sensor = sensor.x) %>%
  drop_na()


## Woodstock

dataWoodstock_long <- dataWoodstock  %>%
  select_if(all_na) %>% ## Removing empty rows
  pivot_longer(cols =! contains("time"),
               names_to = "tag",
               values_to = "pmValues") %>%
  mutate(pmType = substr(tag, start = 4, stop = 7),
         location = substr(tag, start = 9, stop = 12),
         sensor = substr(tag, start = 1, stop = 3)) %>% ## Creates new columns based on sensor number and PM type.
  unite("alttag", c(sensor, location), sep = "_", remove = FALSE) %>%
  mutate(timestamp = case_when(
    sensor == "S13" & location == "fire" ~ S13time_fire,
    sensor == "S13" & location == "lib" ~ S13time_lib,
    sensor == "S13" & location == "mart" ~ S13time_martin,
    sensor == "S08" & location == "fire" ~ S08time_fire,
    sensor == "S08" & location == "lib" ~ S08time_lib,
    sensor == "S08" & location == "mart" ~ S08time_martin,
    sensor == "S06" & location == "fire" ~ S06time_fire,
    sensor == "S06" & location == "lib" ~ S06time_lib,
    sensor == "S06" & location == "mart" ~ S06time_martin,
    sensor == "S20" & location == "fire" ~ S20time_fire,
    sensor == "S20" & location == "lib" ~ S20time_lib,
    sensor == "S20" & location == "mart" ~ S20time_martin)) %>%
  select(tag, pmValues, pmType, alttag, location, sensor, timestamp)

dataWoodstock_times <- left_join(dataWoodstock_long, dataTimes_final, by = "alttag")

dataWoodstock_final <- dataWoodstock_times %>%
  mutate(timestampSec = timestamp/1000,
         fullTime = start + timestampSec,
         hmsTime = hms::hms(seconds_to_period((fullTime)))) %>%
  select(tag, pmValues, pmType, alttag, location.x, sensor.x, hmsTime) %>%
  rename(location = location.x,
         sensor = sensor.x) %>%
  drop_na()


## Joining
allData <- full_join(dataClinton_final, dataWoodstock_final)

dataClinton_toMerge <- dataClinton_final %>%
  mutate(set = "Clinton")

dataWoodstock_toMerge <- dataWoodstock_final %>%
  mutate(set = "Woodstock")

allData <- full_join(dataClinton_toMerge, dataWoodstock_toMerge)
allData <- allData %>%
  select(pmValues, pmType, location, hmsTime, set)

## Testing some data viz
ggplot(allData, aes(x = hmsTime,
                    y = pmValues,
                    color = pmType))+
  facet_wrap(~set)+
  geom_point(alpha = 0.3)

ggplot(allData, aes(x = hmsTime,
                    y = pmValues,
                    color = pmType))+
  geom_point(alpha = 0.3)+
  facet_wrap(~location)

ggplot(allData, aes(x = hmsTime,
                    y = pmValues))+
  geom_point(alpha = 0.3)+
  facet_grid(rows = vars(location), cols = vars(pmType))