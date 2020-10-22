## Data tidying and wrangling for select on-campus data from CSV file
## Written by Gillian McGinnis
## Originally created 14 October 2020
## Updated 21 October 2020

library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggthemes)

onCamp_orig <- read_csv("data/onCamp.csv")

onCamp <- onCamp_orig %>%
  select(starts_with("S03"),
         starts_with("S05"),
         starts_with("S10"))

onCampMod <- onCamp %>%
  pivot_longer(cols =! contains("time"),
               names_to = "tag",
               values_to = "values") %>%
  mutate(shortTag = case_when(
    str_detect(tag, "S03") ~ "S03",
    str_detect(tag, "S05") ~ "S05",
    str_detect(tag, "S10") ~ "S10"
  )) %>%
  mutate(pmType = case_when(
    str_detect(tag, "time") ~ "timestamp",
    str_detect(tag, "pm10") ~ "pm10",
    str_detect(tag, "pm 10") ~ "pm10",
    str_detect(tag, "pm 1") ~ "pm01",
    str_detect(tag, "pm1") ~ "pm01",
    str_detect(tag, "pm 25") ~ "pm25",
    str_detect(tag, "pm 2.5") ~ "pm25",
    str_detect(tag, "pm25") ~ "pm25",
    str_detect(tag, "pm2.5") ~ "pm25"
  )) %>%
  mutate(location = case_when(
    str_detect(tag, "in") ~ "in",
    str_detect(tag, "IN") ~ "in",
    str_detect(tag, "out") ~ "out",
    str_detect(tag, "OUT") ~ "out"
  )) %>%
  rename(
    S03in = 'S03 time (in)',
    S03out = 'S03 time (out)',
    S05in = 'S05 time IN',
    S05out = 'S05 time OUT',
    S10in = S10timeIN,
    S10out = S10timeOUT
  ) %>%
  mutate(timestamp = case_when(
    shortTag == "S03" & location == "in" ~ S03in,
    shortTag == "S03" & location == "out" ~ S03out,
    shortTag == "S05" & location == "in" ~ S05in,
    shortTag == "S05" & location == "out" ~ S05out,
    shortTag == "S10" & location == "in" ~ S10in,
    shortTag == "S10" & location == "out" ~ S10out
  )) %>%
  select(values, pmType, timestamp, shortTag, location) %>%
  mutate(timeSec = timestamp/1000) %>%
  mutate(timeMin = timeSec/60)

## Gottem
ggplot(onCampMod, aes(x = timeMin,
                      y = values,
                      color = pmType,
                      shape = shortTag))+
  facet_wrap(~location)+
  geom_point(alpha=0.5)

ggplot(onCampMod, aes(x = timeMin,
                      y = values,
                      color = pmType,
                      shape = location))+
  facet_wrap(~shortTag)+
  geom_point(alpha = 0.5)

ggplot(onCampMod, aes(x= timeMin,
                      y = values,
                      color = pmType,
                      linetype = shortTag))+
  facet_wrap(~location)+
  geom_line(alpha=0.5)


## Individual colors for each sensor AND PM category
onCampMod2 <- onCampMod %>%
  unite("tagPM", shortTag, pmType)

ggplot(onCampMod2, aes(x = timeMin,
                       y = values,
                       color = tagPM))+
  facet_wrap(~location)+
  geom_point(alpha=0.5)

ggplot(onCampMod2, aes(x = timeMin,
                       y = values,
                       color = tagPM))+
  facet_wrap(~location)+
  geom_line(alpha = 0.7)