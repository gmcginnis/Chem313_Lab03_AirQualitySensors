## Data visualziations for off-campus data from CSV files
## Written by Gillian McGinnis
## Created 21 October 2020
## Updated 21 October 2020

library(ggthemes)
source("R/dataWranglingNeat.R") ## Runs necessary warngles and prepares environment
rm(list = setdiff(ls(), c("allData", "dataClinton_final", "dataWoodstock_final"))) ## Cleans up environment; keep only dataframes of use

## Adjusting labels for later
pmOrder <- c("pm01", "pm25", "pm10")
pmLabels <- c("PM 1.0", "PM 2.5", "PM 10")

## Data viz tests!
## Note: loess method for geom_smooth takes time to process. "gam" method is faster but gives much curvier lines.

## 2 scatter plots; does not include location variable
ggplot(allData, aes(x = hmsTime,
                    y = pmValues,
                    color = pmType))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "loess")+
  facet_wrap(~set)+
  scale_color_discrete(breaks = pmOrder,
                       labels = pmLabels)+
  labs(color = "PM Type",
       x = "Time",
       y = "PM Value (ppm)")

## 6 scatterplots; facets by location, does not include set (i.e. Woodstock or Clinton) (but this is implied by location)
ggplot(allData, aes(x = hmsTime,
                    y = pmValues,
                    color = pmType))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "loess")+
  facet_wrap(~location)

## 18 scatterplots; facets by location and PM type
ggplot(allData, aes(x = hmsTime,
                    y = pmValues))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "loess")+
  facet_grid(rows = vars(location), cols = vars(pmType))

## 6 scatterplots; facets by location, color by PM type
ggplot(allData, aes(x = hmsTime,
                    y = pmValues,
                    color = pmType))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "loess")+
  facet_wrap(~location)

## 2 boxplots
ggplot(allData, aes(x = location,
                    y = pmValues,
                    color = pmType))+
  facet_wrap(~set, scales = "free")+
  geom_boxplot()

ggplot(allData, aes(x = location,
                    y = pmValues,
                    color = pmType))+
  facet_grid(.~set, scales = "free", space = "free")+
  geom_boxplot(color = "gray50", outlier.shape = NA, fill = NA)+
  geom_point(alpha = 0.7, position = position_jitterdodge())

ggplot(allData, aes(x = hmsTime,
                    y = pmValues,
                    color = pmType))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "loess")+
  facet_wrap(~location)