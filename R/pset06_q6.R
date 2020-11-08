## Problem Set 6, Question 6
## Written by Gillian McGinnis
## Created 07 November 2020
## Updated 07 November 2020

library(tidyverse)
library(lubridate)
library(ggthemes)
library(wesanderson)

test1 <- read_csv("data/test1.csv")

all_na <- function(x) any(!is.na(x))

opData <- test1 %>%
  select(c(1,2)) %>%
  drop_na() %>%
  rename(Time = 1,
         "pm25" = X2) %>%
  slice(-1) %>%
  mutate(pm25 = as.numeric(pm25))

start <- hms("14:25:00")
startSec <- (14*60*60 + 25*60)

testData <- test1 %>%
  select_if(all_na) %>%
  select(-c(1:3)) %>%
  select(-c(12:15)) %>% #Excluding S05 because it was taken at a different time
  pivot_longer(cols =! contains("time"),
               names_to = "tag",
               values_to = "pmValues") %>%
  mutate(pmType = substr(tag, start = 4, stop = 7),
         sensor = substr(tag, start = 1, stop = 3)) %>%
  mutate(timestamp = case_when(
    sensor == "S02" ~ S03time, #S02 did not report their times. Other sensor timestamps are likely close.
    sensor == "S03" ~ S03time,
    sensor == "S04" ~ S04time,
    sensor == "S08" ~ S08time,
    sensor == "S09" ~ S09time,
    sensor == "S10" ~ S10time,
    sensor == "S11" ~ S11time,
    sensor == "S14" ~ S14time,
    sensor == "S15" ~ `S15 Time`,
    sensor == "S17" ~ S17time,
    sensor == "S18" ~ S18time,
    sensor == "S19" ~ S19time,
    sensor == "S20" ~ S20time)) %>%
  select(c(tag, pmValues, pmType, sensor, timestamp))

dataTimes <- testData %>%
  mutate(timestampSec = timestamp/1000,
         fullTime = startSec + timestampSec) %>%
  mutate(period = seconds_to_period(fullTime),
         hmsTime = hms::hms(seconds_to_period((period)))) %>%
  select(-c(timestamp, period, fullTime, timestampSec, tag)) %>%
  mutate(pmType = case_when(
    pmType == "M1" ~ "PM01",
    pmType == "PM1" ~ "PM01",
    pmType == "PM2." ~ "PM25",
    pmType == "PM10" ~ "PM10",
    pmType == "PM01" ~ "PM01",
    pmType == "PM25" ~ "PM25"
  )) %>%
  filter(pmType == "PM25",
         sensor != "S11") %>% #Excluding S11 because it only recorded 0s. Likely malfunctioning.
  select(-c(pmType)) %>%
  filter(sensor != "S02") # Decided to exclude S02. Timestamp not precise.


labTime <- "Time of day (HH:MM:SS)"
labPM <- expression(paste("PM"[2.5]*" Value (", mu, "g/m"^3*")"))
labTitle <- expression(paste("Comparing PM"[2.5]*" values over time"))

labCaption <- "Figure 1. Curve fits of PM data over time by sensor added with loess regression, and include a 95% CI.
Data from S02 (did not report timestamps), S05 (taken at a different time), and S11 (malfunctioning sensor)\nhave been excluded."
#labCaption <- expression(paste("Figure 1. Comparing PM"[2.5]*" values over time. Curve fits added with loess regression, and include a 95% CI.\nData from S02 (did not report timestamps), S05 (taken at a different time), and S11 (malfunctioning sensor) have been excluded."))

aqPlot <- ggplot(dataTimes, aes(x = hmsTime, y = pmValues, color = sensor, fill = sensor))+
  stat_smooth()+
  scale_colour_manual(
    values = wes_palette(12, name = "Darjeeling1", type = "continuous"),
    aesthetics  = c("color", "fill"))+
  labs(color = "Sensor",
       fill = "Sensor",
       x = labTime,
       y = labPM,
       title = labTitle,
       caption = labCaption)+
  theme_few()+
  theme(plot.caption = element_text(hjust = 0))
  # # Code to add labels at ends of lines; works somewhat.
  # geom_text(data=. %>%
  #             arrange(desc(hmsTime)) %>%
  #             group_by(sensor) %>%
  #             slice(1),
  #           aes(label=sensor),
  #           position=position_nudge(0.1), hjust=0, show.legend=FALSE)

aqPlot
  
opDataMod <- opData %>%
  mutate(period = c(
    (13*60*60 + 45*60),
    (13*60*60 + 55*60),
    (14*60*60 + 55*60)),
    hmsTime = hms::hms(seconds_to_period((period)))) %>%
  select(-c(period, Time)) %>%
  mutate(sensor = "OPS") %>%
  rename(pmValues = pm25)

opMean <- mean(opDataMod$pmValues)
opSd <- sd(opDataMod$pmValues)

opStats <- opDataMod %>%
  mutate(sd = sd(pmValues),
         mean = mean(pmValues),
         min = mean-sd,
         max = mean+sd) %>%
  select(!c(hmsTime))

aqPlot +
  geom_hline(data = opStats, aes(yintercept = mean, color = sensor))+
  geom_ribbon(data = opStats, aes(color = sensor, fill = sensor,
                                  ymin = min, ymax = max,
                                  xmin = -Inf, xmax = Inf),
              alpha = 0.3)

aqPlot +
  geom_hline(data = opStats, aes(yintercept = mean, color = sensor))+
  geom_hline(data = opStats, aes(yintercept = min, color = sensor),
             linetype = "dotted")+
  geom_hline(data = opStats, aes(yintercept = max, color = sensor),
             linetype = "dotted")

aqPlot +
  geom_hline(data = opDataMod, aes(yintercept = pmValues, color = sensor),
             linetype = "longdash")


mergedData <- full_join(dataTimes, opDataMod)


mergedStats <- mergedData %>%
  select(!c(hmsTime)) %>%
  filter(!is.na(pmValues)) %>%
  group_by(sensor) %>%
  summarize(mean = mean(pmValues),
            sd = sd(pmValues))

collapsedData <- mergedData %>%
  select(!c(hmsTime)) %>%
  filter(!is.na(pmValues)) %>%
  mutate(newSensor = case_when(
    sensor == "OPS" ~ "OPS",
    sensor == "S03" ~ "AQ",
    sensor == "S04" ~ "AQ",
    sensor == "S08" ~ "AQ",
    sensor == "S09" ~ "AQ",
    sensor == "S10" ~ "AQ",
    sensor == "S11" ~ "AQ",
    sensor == "S14" ~ "AQ",
    sensor == "S15" ~ "AQ",
    sensor == "S17" ~ "AQ",
    sensor == "S18" ~ "AQ",
    sensor == "S19" ~ "AQ",
    sensor == "S20" ~ "AQ")) %>%
  select(!c(sensor)) %>%
  group_by(newSensor) %>%
  summarize(mean = mean(pmValues),
            sd = sd(pmValues))


aqData <- dataTimes %>%
  group_by(sensor) %>%
  filter(!is.na(pmValues))

## Some ANOVA stuff. This gets messy quickly.
aqT <- t.test(pmValues ~ sensor, data = aqData)

aqAov <- aov(pmValues ~ sensor, data = aqData)
tidy(aqAov)
aqTuckey <- TukeyHSD(aqAov)
plot(aqTuckey, las = 1)
pairwise.t.test(aqData$pmValues, aqData$sensor)





## Ugly plot 1
# aqPlot +
#   geom_point(data = opDataMod, aes(x = hmsTime, y = pmValues, color = NULL, fill = NULL),
#              color = "darkgray")




## Plot w added vertical lines
aqPlot +
  geom_hline(data = opDataMod, aes(yintercept = pmValues, color = NULL, fill = NULL),
             linetype = "longdash",
             color = "darkgray")+
  labs(caption = "Scatterplots of data points. Curve fit added with loess regression.
       Dashed lines represent measured values of lab-grade Optical Particle Spectrometer (OPS).")

## Ugly plot 2
# ggplot(mergedData, aes(x = hmsTime, y = pmValues, color = sensor, fill = sensor))+
#   stat_smooth()+
#   scale_colour_manual(
#     values = wes_palette(12, name = "Zissou1", type = "continuous"),
#     aesthetics  = c("color", "fill"))+
#   labs(color = "Sensor",
#        fill = "Sensor",
#        x = labTime,
#        y = labPM,
#        title = labTitle,
#        caption = labCaption)+
#   theme_few()+
#   theme(plot.caption = element_text(hjust = 0))
