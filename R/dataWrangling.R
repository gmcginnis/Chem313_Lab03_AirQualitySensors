## Data wrangling for off-campus data from CSV files
## Written by Gillian McGinnis
## Created 20 October 2020
## Updated 20 October 2020

library(tidyverse) ## Needed for dplyr, strinr, etc.
library(lubridate) ## Needed for time conversions since our data is in milliseconds.

dataClinton <- read_csv("data/dataClinton.csv")
dataWoodstock <- read_csv("data/dataWoodstock.csv")
dataTimes <- read_csv("data/dataTimes.csv")

all_na <- function(x) any(!is.na(x)) ## New function to drop NAs

dataTimesMod <- dataTimes %>%
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
  unite("longtag", c(Sensor, sub), sep = "_", remove = FALSE) %>% ## Might be useful later
  rename_all(tolower) ## I like my columns lowercase

## Clinton

## Testing pipes with ONE sensor (S17)
## Direct input until I'm able to get the dataframes united neatly.
S17const <- hms("06:48:00")
S17ped <- hms("07:00:00")
S17bike <- hms("07:08:00")

## A pipe. It works, but it could probably be simplified. Oh well.

dataClintonMod <- dataClinton %>%
  select(starts_with("S17")) %>% ## Filtering just for now so I can test with a smaller dataset. Will be removed once pipe is done.
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
    sensor == "S17" & location == "ped" ~ S17time_ped)) %>% ## This will be a long set for everyone's data. Will try simplifying.
  ##select(tag, pmValues, pmType, location, timestamp) ## Filtering out now-redundant columns. Can only do once timestamp is sorted out.
  mutate(fullTime = case_when(
    sensor == "S17" & location == "const" ~ (S17const + milliseconds(timestamp)),
    sensor == "S17" & location == "bike" ~ (S17bike + milliseconds(timestamp)),
    sensor == "S17" & location == "ped" ~ (S17ped + milliseconds(timestamp))
  )) %>%
  mutate(hmsTime = hms::as_hms(period_to_seconds(hms(fullTime))))

## Backup because in the past for some reason dplyr didn't like this being in the same pipe as above
##dataClintonMod <- dataClintonMod %>%
##  mutate(hmsTime = hms::as_hms(period_to_seconds(hms(fullTime))))

## Quick plot to see how this looks
ggplot(dataClintonMod, aes(x = hmsTime,
                           y = pmValues,
                           color = pmType,
                           shape = location))+
  geom_point()


## A rather lengthy set that should work. But I will simplify eventually.
S17const <- hms("06:48:00")
S17ped <- hms("07:00:00")
S17bike <- hms("07:08:00")
S04const <- hms("09:37:00")
S04ped <- hms("09:46:00")
S04bike <- hms("09:58:00")
S16const <- hms("18:11:00")
S16bike <- hms("18:18:00")
S16ped <- hms("18:27:00")
S02const <- hms("14:33:00")
S02bike <- hms("14:53:00")
S02ped <- hms("14:43:00")

dataClintonLong <- dataClinton %>%
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
    sensor == "S02" & location == "ped" ~ S02time_ped)) %>% ## This will be a long set for everyone's data. Will try simplifying.
  mutate(fullTime = case_when(
    sensor == "S17" & location == "const" ~ (S17const + milliseconds(timestamp)),
    sensor == "S17" & location == "bike" ~ (S17bike + milliseconds(timestamp)),
    sensor == "S17" & location == "ped" ~ (S17ped + milliseconds(timestamp)),
    sensor == "S04" & location == "const" ~ (S04const + milliseconds(timestamp)),
    sensor == "S04" & location == "bike" ~ (S04bike + milliseconds(timestamp)),
    sensor == "S04" & location == "ped" ~ (S04ped + milliseconds(timestamp)),
    sensor == "S16" & location == "const" ~ (S16const + milliseconds(timestamp)),
    sensor == "S16" & location == "bike" ~ (S16bike + milliseconds(timestamp)),
    sensor == "S16" & location == "ped" ~ (S16ped + milliseconds(timestamp)),
    sensor == "S02" & location == "const" ~ (S02const + milliseconds(timestamp)),
    sensor == "S02" & location == "bike" ~ (S02bike + milliseconds(timestamp)),
    sensor == "S02" & location == "ped" ~ (S02ped + milliseconds(timestamp))
  )) %>%
  mutate(hmsTime = hms::as_hms(period_to_seconds(hms(fullTime)))) %>% ## Seems cyclic but it works
  select(pmValues, pmType, location, sensor, hmsTime)

## Quick data viz checks
ggplot(dataClintonLong, aes(x = hmsTime,
                            y = pmValues,
                            color = pmType,
                            shape = location))+
  geom_point()

ggplot(dataClintonLong, aes(x = hmsTime,
                            y = pmValues,
                            shape = pmType,
                            color = location))+
  geom_point()


## Woodstock

S13fire <- hms("09:33:00")
S13lib <- hms("09:50:00")
S13mart <- hms("09:42:00")
S08fire <- hms("10:29:30")
S08lib <- hms("10:49:00")
S08mart <- hms("10:59:50")
S06fire <- hms("19:00:30")
S06lib <- hms("19:08:01")
S06mart <- hms("19:16:10")
S20fire <- hms("17:15:00")
S20lib <- hms("17:24:30")
S20mart <- hms("17:30:00")

dataWoodstockLong <- dataWoodstock %>%
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
    sensor == "S20" & location == "mart" ~ S20time_martin)) %>% ## This will be a long set for everyone's data. Will try simplifying.
  mutate(fullTime = case_when(
    sensor == "S13" & location == "fire" ~ (S13fire + milliseconds(timestamp)),
    sensor == "S13" & location == "lib" ~ (S13lib + milliseconds(timestamp)),
    sensor == "S13" & location == "mart" ~ (S13mart + milliseconds(timestamp)),
    sensor == "S08" & location == "fire" ~ (S08fire + milliseconds(timestamp)),
    sensor == "S08" & location == "lib" ~ (S08lib + milliseconds(timestamp)),
    sensor == "S08" & location == "mart" ~ (S08mart + milliseconds(timestamp)),
    sensor == "S06" & location == "fire" ~ (S06fire + milliseconds(timestamp)),
    sensor == "S06" & location == "lib" ~ (S06lib + milliseconds(timestamp)),
    sensor == "S06" & location == "mart" ~ (S06mart + milliseconds(timestamp)),
    sensor == "S20" & location == "fire" ~ (S20fire + milliseconds(timestamp)),
    sensor == "S20" & location == "lib" ~ (S20lib + milliseconds(timestamp)),
    sensor == "S20" & location == "mart" ~ (S20mart + milliseconds(timestamp))
  )) %>%
  mutate(hmsTime = hms::as_hms(period_to_seconds(hms(fullTime)))) %>%
  select(pmValues, pmType, location, sensor, hmsTime) %>%
  drop_na() ## One of the S13 fire values gave a rather high time (841000 hours? It didn't have a PM value anyway)


## Quick data viz checks
ggplot(dataWoodstockLong, aes(x = hmsTime,
                            y = pmValues,
                            color = pmType,
                            shape = location))+
  geom_point()

ggplot(dataWoodstockLong, aes(x = hmsTime,
                            y = pmValues,
                            shape = pmType,
                            color = location))+
  geom_point()


## With geom_smooth. Probably not a good estimation though.
ggplot(dataWoodstockLong, aes(x = hmsTime,
                              y = pmValues,
                              color = pmType))+
  geom_smooth()+
  geom_point(aes(shape = location))



## Time for a very very large dataframe.

dataWoodstock_mega <- dataWoodstockLong %>%
  mutate(set = "Woodstock")

dataClinton_mega <- dataClintonLong %>%
  mutate(set = "Clinton")

allData <- full_join(dataWoodstock_mega, dataClinton_mega)

ggplot(allData, aes(x = hmsTime,
                    y = pmValues,
                    color = pmType))+
  geom_point()+
  facet_grid(rows = vars(location), cols = vars(set))

ggplot(allData, aes(x = hmsTime,
                    y = pmValues,
                    color = pmType))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~set)



#####
## Pipe tests. These don't work.
## df <- dataClinton %>%
  ##mutate(start = case_when(dataTimesMod$longtag == alttag ~ dataTimesMod$start)) %>%
  ##mutate(timestamp ~ 'sensor'time_'location') %>% ## Not really sure what I'm doing here
  ##mutate(adjTimestamp = (case_when(dataTimesMod$longtag == alttag) ~ dataTimesMod$start) + milliseconds(timestamp)) %>% ## Still WIP
