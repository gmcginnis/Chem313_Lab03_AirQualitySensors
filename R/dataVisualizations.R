## Data visualziations for off-campus data from CSV files
## Written by Gillian McGinnis
## Created 21 October 2020
## Updated 21 October 2020

source("R/dataWranglingNeat.R") ## Runs necessary warngles and prepares environment
rm(list = setdiff(ls(), c("allData", "dataClinton_final", "dataWoodstock_final"))) ## Cleans up environment; keep only dataframes of use


## Data viz tests!
## Note: loess method for geom_smooth takes time to process. "gam" method is faster but gives much curvier lines.

## 2 scatter plots; does not include location variable
ggplot(allData, aes(x = hmsTime,
                    y = pmValues,
                    color = pmType))+
  geom_point(alpha = 0.3)+
  geom_smooth(method = "loess")+
  facet_wrap(~set)

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
