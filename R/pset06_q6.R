## Problem Set 6, Question 6
## Written by Gillian McGinnis
## Created 07 November 2020
## Updated 11 November 2020

## This script is kind of all over the place but is generally in order of proceedings.
## General wrangling up top, start of data viz, more wrangling, more data viz, and some quick stat tests (w viz) that I ended up not using.
## To find sources for each visualization I ended up using, ctrl-f for Table 1, Figure 1, Figure 2, Figure 3, or Figure 4.

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
  filter(pmType == "PM25") %>%
  select(-c(pmType))

dataFiltered <- dataTimes %>%
  filter(sensor != "S11") %>% #Excluding S11 because it only recorded 0s. Likely malfunctioning.
  filter(sensor != "S02") # Decided to exclude S02. Timestamp not precise.


labTime <- "Time of day (HH:MM:SS)"
labPM <- expression(paste("PM"[2.5]*" Value (", mu, "g/m"^3*")"))
labTitle <- expression(paste("Comparing PM"[2.5]*" values over time"))

labCaption <- "Figure 1. Curve fits of PM data over time by sensor added with loess regression, and include a 95% CI.
Data from S02 (did not report timestamps), S05 (taken at a different time), and S11 (malfunctioning sensor)\nhave been excluded."
#labCaption <- expression(paste("Figure 1. Comparing PM"[2.5]*" values over time. Curve fits added with loess regression, and include a 95% CI.\nData from S02 (did not report timestamps), S05 (taken at a different time), and S11 (malfunctioning sensor) have been excluded."))

## This ended up as Figure 4
aqPlot <- ggplot(dataFiltered, aes(x = hmsTime, y = pmValues, color = sensor, fill = sensor))+
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


## playing with line charts. These ended up as very busy graphs due to the sheer amount of datapoints.
# dataFilteredDouble <- dataFiltered %>%
#   filter(sensor != "S09")
# 
# ggplot(dataFilteredDouble, aes(x = hmsTime, y = pmValues, color = sensor, fill = sensor))+
#   geom_point(alpha=0.1)+
#   geom_line(alpha=0.1)+
#   scale_colour_manual(
#     values = wes_palette(12, name = "Darjeeling1", type = "continuous"),
#     aesthetics  = c("color", "fill"))+
#   labs(color = "Sensor",
#        fill = "Sensor",
#        x = labTime,
#        y = labPM,
#        title = labTitle,
#        caption = labCaption)+
#   theme_few()+
#   theme(plot.caption = element_text(hjust = 0))



  
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

opSummary <- opStats %>%
  select(c(sensor, sd, mean, min, max)) %>%
  slice(1)

aqPlot +
  geom_hline(data = opSummary, aes(yintercept = mean, color = sensor))+
  geom_hline(data = opSummary, aes(yintercept = min, color = sensor),
             linetype = "dotted")+
  geom_hline(data = opSummary, aes(yintercept = max, color = sensor),
             linetype = "dotted")


##Some plot tests
# aqPlot +
#   geom_hline(data = opSummary, aes(yintercept = mean, color = sensor))+
#   geom_ribbon(data = opSummary, aes(color = sensor, fill = sensor,
#                                   ymin = min, ymax = max,
#                                   xmin = -Inf, xmax = Inf),
#               alpha = 0.3)

# aqPlot +
#   geom_hline(data = opStats, aes(yintercept = mean, color = sensor))+
#   geom_ribbon(data = opStats, aes(color = sensor, fill = sensor,
#                                   ymin = min, ymax = max,
#                                   xmin = -Inf, xmax = Inf),
#               alpha = 0.3)

## This ended up as Figure 1
aqPlot +
  geom_hline(data = opStats, aes(yintercept = mean, color = sensor))+
  geom_hline(data = opStats, aes(yintercept = min, color = sensor),
             linetype = "dotted")+
  geom_hline(data = opStats, aes(yintercept = max, color = sensor),
             linetype = "dotted")


# aqPlot +
#   geom_hline(data = opDataMod, aes(yintercept = pmValues, color = sensor),
#              linetype = "longdash")


mergedData <- full_join(dataTimes, opDataMod)


mergedStats <- mergedData %>%
  select(!c(hmsTime)) %>%
  filter(!is.na(pmValues)) %>%
  group_by(sensor) %>%
  summarize(mean = mean(pmValues),
            sd = sd(pmValues))

## This ended up as Table 1
statsMinimal <- mergedStats %>%
  mutate(mean = round(mean, 0),
         sd = round(sd, 0))


labTitleBox <- expression(paste("Comparing PM"[2.5]*" values by sensor"))
labCaptionBox <- "Figure 2. Boxplots of PM data by sensor. No sensors excluded."

## This ended up as Figure 1
aqBox <- ggplot(mergedData, aes(x = sensor, y = pmValues, color = sensor))+
  geom_boxplot()+
  scale_colour_manual(
    values = wes_palette(14, name = "Darjeeling1", type = "continuous"),
    aesthetics  = c("color", "fill"))+
  labs(color = "Sensor",
       x = "Sensor",
       y = labPM,
       title = labTitleBox,
       caption = labCaptionBox)+
  theme_few()+
  theme(plot.caption = element_text(hjust = 0))

mergedFiltered <- mergedData %>%
  filter(sensor != "S09",
         sensor != "S11")

## This ended up as Figure 2
aqBoxMin <- ggplot(mergedFiltered, aes(x = sensor, y = pmValues, color = sensor))+
  geom_boxplot()+
  scale_colour_manual(
    values = wes_palette(14, name = "Darjeeling1", type = "continuous"),
    aesthetics  = c("color", "fill"))+
  labs(color = "Sensor",
       x = "Sensor",
       y = labPM,
       title = labTitleBox,
       caption = "Some sensors excluded")+
  theme_few()+
  theme(plot.caption = element_text(hjust = 0))


#### Some very messy stat tests down here

# collapsedData <- mergedData %>%
#   select(!c(hmsTime)) %>%
#   filter(!is.na(pmValues)) %>%
#   filter(sensor != "S02") %>%
#   mutate(newSensor = case_when(
#     sensor == "OPS" ~ "OPS",
#     #sensor == "S02" ~ "AQ",
#     sensor == "S03" ~ "AQ",
#     sensor == "S04" ~ "AQ",
#     sensor == "S08" ~ "AQ",
#     sensor == "S09" ~ "AQ",
#     sensor == "S10" ~ "AQ",
#     sensor == "S11" ~ "AQ",
#     sensor == "S14" ~ "AQ",
#     sensor == "S15" ~ "AQ",
#     sensor == "S17" ~ "AQ",
#     sensor == "S18" ~ "AQ",
#     sensor == "S19" ~ "AQ",
#     sensor == "S20" ~ "AQ")) %>%
#   select(!c(sensor)) %>%
#   group_by(newSensor) %>%
#   summarize(mean = mean(pmValues),
#             sd = sd(pmValues)) %>%
#   mutate(min = mean-sd,
#          max = mean+sd)
# 
# # ggplot(collapsedData, aes(x = newSensor, y = mean))+
# #   geom_bar(stat='identity')+
# #   geom_errorbar(ymin = min, ymax = max)
# 
#   
# aqData <- dataTimes %>%
#   group_by(sensor) %>%
#   filter(!is.na(pmValues))
# 
# ## Some ANOVA stuff. This gets messy quickly.
# #aqT <- t.test(pmValues ~ sensor, data = aqData)
# 
# aqAov <- aov(pmValues ~ sensor, data = aqData)
# tidy(aqAov)
# aqTuckey <- TukeyHSD(aqAov)
# plot(aqTuckey, las = 1)
# pairwise.t.test(aqData$pmValues, aqData$sensor)
# 
# 
# collapsedT <- mergedData %>%
#   select(!c(hmsTime)) %>%
#   filter(!is.na(pmValues)) %>%
#   filter(sensor != "S11") %>%
#   mutate(newSensor = case_when(
#     sensor == "OPS" ~ "OPS",
#     sensor == "S02" ~ "AQ",
#     sensor == "S03" ~ "AQ",
#     sensor == "S04" ~ "AQ",
#     sensor == "S08" ~ "AQ",
#     sensor == "S09" ~ "AQ",
#     sensor == "S10" ~ "AQ",
#     #sensor == "S11" ~ "AQ",
#     sensor == "S14" ~ "AQ",
#     sensor == "S15" ~ "AQ",
#     sensor == "S17" ~ "AQ",
#     sensor == "S18" ~ "AQ",
#     sensor == "S19" ~ "AQ",
#     sensor == "S20" ~ "AQ")) %>%
#   select(!c(sensor)) %>%
#   group_by(newSensor)
# 
# 
# collapsedAQ <- collapsedT %>%
#   filter(newSensor == "AQ")
# collapsedOP <- collapsedT %>%
#   filter(newSensor == "OPS")
# 
# var.test(collapsedAQ$pmValues, collapsedOP$pmValues)
# ## Critical value at numdf = inf, denomdf = 2: 19.5
# ## F_calc = 8.343067, p = 0.2259
# 
# sensorComp <- t.test(pmValues ~ newSensor, data = collapsedT)
# tidy(sensorComp)
# # value of student's t @ 95CI, 2DF = 12.706
# # t_critical(DF = 2) = 12.706, p = 0.05
# # |t-stat| = 18.358, p-value = 0.00288
# # They are statistically significantly different
# 
# 
# fullAov <- aov(pmValues ~ sensor, data = collapsedData)
# tidy(fullAov)
# fullTuckey <- TukeyHSD(fullAov)
# plot(fullTuckey, las = 1)
# pairwise.t.test(aqData$pmValues, aqData$sensor)
# 
# tukeyDf <- as.data.frame(fullTuckey$sensor)
# tukeyDf$pair = rownames(tukeyDf)
# 
# tukeyPlot <- tukeyDf %>%
#   mutate(label = case_when(
#     `p adj` < 0.05 ~ "p < 0.05", ## Reject null hypothesiss; diff is significant
#     `p adj` >= 0.05 ~ "Non-Sig" ## Fail to reject null hyp; diff is not significant
#   ))
# 
# tukeyMinimal <- tukeyPlot %>%
#   filter(label == "Non-Sig")
# 
# tukeyColorful <- tukeyPlot %>%
#   mutate(sensor = case_when(
#     str_detect(pair, "OPS") ~ "OPS"
#   )) %>%
#   replace_na(list(sensor = "AQ"))
# 
# 
# ggplot(tukeyColorful, aes(color = sensor))+
#   geom_hline(yintercept=0, lty="11", color="grey30") +
#   geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.2) +
#   geom_point(aes(pair, diff)) +
#   labs(color="",
#        x = "Pairing",
#        y = "Difference")+
#   scale_x_discrete()+
#   theme_few()+
#   coord_flip()

#tukeyFull$set <- factor(tukeyFull$set, levels = c('pm01', 'pm25', 'pm10'))


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
