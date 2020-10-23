## Statistical tests for off-campus data from CSV files
## Written by Gillian McGinnis
## Created 22 October 2020
## Updated 22 October 2020

source("R/dataWranglingNeat.R") ## Runs necessary warngles and prepares environment
rm(list = setdiff(ls(), c("allData", "dataClinton_final", "dataWoodstock_final"))) ## Cleans up environment; keep only dataframes of use

t.test(pmValues ~ set, data = allData)$p.value
t.test(pmValues ~ set, data = allData)$statistic

var.test(dataWoodstock_final$pmValues, dataClinton_final$pmValues)
t.test(dataWoodstock_final$pmValues, dataClinton_final$pmValues, mu = 0, paired = FALSE, var.equal = FALSE)

## Yielded negative values
## var.test(dataClinton_final$pmValues, dataWoodstock_final$pmValues)
## t.test(dataClinton_final$pmValues, dataWoodstock_final$pmValues, mu = 0, paired = FALSE, var.equal = FALSE)

## Pipe test. No dice, will fix later. Does the same as above. Source: https://sebastiansauer.github.io/multiple-t-tests-with-dplyr/
# allDataSum <- allData %>%
#   select(-set) %>%
#   group_by(location, pmType) %>%
#   summarize(pmValues = list(pmValues)) %>%
#   spread(location, pmValues) %>%
#   mutate(p_test = t.test(unlist(bike),
#                          unlist(const),
#                          unlist(fire),
#                          unlist(lib),
#                          unlist(mart),
#                          unlist(ped))$p.value,
#          t_test = t.test(unlist(bike),
#                          unlist(const),
#                          unlist(fire),
#                          unlist(lib),
#                          unlist(mart),
#                          unlist(ped))$statistic)