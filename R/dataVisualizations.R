## Data visualziations for off-campus data from CSV files
## Written by Gillian McGinnis
## Created 21 October 2020
## Updated 22 October 2020

library(ggthemes)
source("R/dataWranglingNeat.R") ## Runs necessary warngles and prepares environment
rm(list = setdiff(ls(), c("allData", "dataClinton_final", "dataWoodstock_final"))) ## Cleans up environment; keep only dataframes of use

## Adjusting labels for later
pmOrder <- c("pm01", "pm25", "pm10")
pmLabels <- expression('PM'[1.0], 'PM'[2.5], 'PM'[10])

pmLabeller <- c(pm01 = "PM 1.0", pm25 = "PM 2.5", pm10 = "PM 10")
locationLabeller <- c(bike = "Bike", const = "Construction", fire = "Fire Hydrant", lib = "Library", mart = "Martins St.", ped = "Pedestrian")

allData$location_f <- factor(allData$location, levels = c('const', 'bike', 'ped', 'mart', 'lib', 'fire'))
allData$pmType_f <- factor(allData$pmType, levels = c('pm01', 'pm25', 'pm10'))

labTime <- "Time of day (HH:MM:SS)"
labPM <- expression(paste("PM Value (", mu, "g/m"^3*")"))
##labPM <- expression('PM Value (µg/m'^3*')') ## alt method
## Note: if using expression in legend, will horizontally misalign; fix this (maybe?) by adding +theme(legend.text = element_text(hjust = 0))

## Data viz tests!
## Note: loess method for geom_smooth takes time to process. "gam" method is faster but gives much curvier lines.

## 1 scatterplot: locations and sublocations not included at all
ggplot(allData, aes(x = hmsTime,
                    y = pmValues,
                    color = pmType))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "loess")+
  scale_color_few(breaks = pmOrder,
                  labels = pmLabels,
                  palette = "Dark")+
  labs(color = "PM Type",
       x = labTime,
       y = labPM,
       title = "Comparing PM Values over time",
       caption = "Scatterplots of data points. Curve fit added with loess regression.")+
  theme(legend.text = element_text(hjust = 0))+
  theme_few()

## 2 scatter plots; does not include sublocation variable
ggplot(allData, aes(x = hmsTime,
                    y = pmValues,
                    color = pmType))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "loess")+
  facet_wrap(~set)+
  scale_color_few(breaks = pmOrder,
                  labels = pmLabels,
                  palette = "Dark")+
  labs(color = "PM Type",
       x = labTime,
       y = labPM,
       title = "Comparing PM Values for the two locations over time",
       caption = "Scatterplots of data points, faceted by location. Curve fit added with loess regression.")+
  theme(legend.text = element_text(hjust = 0))+
  theme_few()


## 6 scatterplots; facets by location, does not include set (i.e. Woodstock or Clinton) (but this is implied by location)
ggplot(allData, aes(x = hmsTime,
                    y = pmValues,
                    color = pmType))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "loess")+
  facet_wrap(~location_f,
             labeller = labeller(location_f = locationLabeller))+
  scale_color_few(breaks = pmOrder,
                  labels = pmLabels,
                  palette = "Dark")+
  labs(color = "PM Type",
       x = labTime,
       y = labPM,
       title = "Comparing PM Values for the sublocations over time",
       caption = "Scatterplots of data points, faceted by sublocation. Curve fit added with loess regression.")+
  theme(legend.text = element_text(hjust = 0))+
  theme_few()


## 18 scatterplots; facets by location and PM type
ggplot(allData, aes(x = hmsTime,
                    y = pmValues))+
  geom_smooth(method = "loess",
              color = "gray")+
  geom_point(alpha = 0.3,
             aes(color = set),
             show.legend = FALSE)+
  facet_grid(rows = vars(location_f),
             cols = vars(pmType_f),
             labeller = labeller(pmType_f = pmLabeller,
                                 location_f = locationLabeller))+
  labs(x = labTime,
       y = labPM,
       title = "Comparing different PM Values for each sublocation over time.",
       caption = "Scatterplots of data points, faceted by location. Curve fit added with loess regression.")+
  scale_color_few(palette = "Dark")+
  theme_few()

## Another set of 18
ggplot(allData, aes(x = hmsTime,
                    y = pmValues,
                    color = pmType))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "loess")+
  facet_grid(rows = vars(location_f),
             cols = vars(pmType_f),
             labeller = labeller(location_f = locationLabeller,
                                 pmType_f = pmLabeller))+
  scale_color_few(breaks = pmOrder,
                  labels = pmLabels,
                  palette = "Dark")+
  labs(color = "PM Type",
       x = labTime,
       y = labPM,
       title = "Comparing PM Values for the sublocations over time",
       caption = "Scatterplots of data points, faceted by location. Curve fit added with loess regression.")+
  theme(legend.text = element_text(hjust = 0))+
  theme_few()

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