## Statistical tests for off-campus data from CSV files
## Written by Gillian McGinnis
## Created 22 October 2020
## Updated 24 October 2020

library(infer)
library(broom)
source("R/dataWranglingNeat.R") ## Runs necessary warngles and prepares environment
rm(list = setdiff(ls(), c("allData", "dataClinton_final", "dataWoodstock_final"))) ## Cleans up environment; keep only dataframes of use

pmOrder <- c("pm01", "pm25", "pm10")
pmLabels <- expression('PM'[1.0], 'PM'[2.5], 'PM'[10])

pmLabeller <- c(pm01 = "PM 1.0", pm25 = "PM 2.5", pm10 = "PM 10")
# pmLabeller <- c(pm01 = (expression('PM'[1.0])),
#                 pm25 = (expression('PM'[2.5])),
#                 pm10 = (expression('PM'[10])))

locationLabeller <- c(bike = "Bike", const = "Construction", fire = "Fire Hydrant", lib = "Library", mart = "Martins St.", ped = "Pedestrian")

allData$location_f <- factor(allData$location, levels = c('const', 'bike', 'ped', 'mart', 'lib', 'fire'))
allData$pmType_f <- factor(allData$pmType, levels = c('pm01', 'pm25', 'pm10'))

labTime <- "Time of day (HH:MM:SS)"
labPM <- expression(paste("PM Value (", mu, "g/m"^3*")"))

# ## just for set (aka between main locations)
# t.test(pmValues ~ set, data = allData)$p.value
# t.test(pmValues ~ set, data = allData)$statistic
# var.test(dataWoodstock_final$pmValues, dataClinton_final$pmValues)
# t.test(dataWoodstock_final$pmValues, dataClinton_final$pmValues, mu = 0, paired = FALSE, var.equal = FALSE)

## Yielded negative values
## var.test(dataClinton_final$pmValues, dataWoodstock_final$pmValues)
## t.test(dataClinton_final$pmValues, dataWoodstock_final$pmValues, mu = 0, paired = FALSE, var.equal = FALSE)
## 15 combinations of sublocations :/

allData_01 <- allData %>%
  filter(pmType == "pm01") %>%
  select(pmValues, location)
aov01 <- aov(pmValues ~ location, data = allData_01)
tidy(aov01)
tukeyTable01 <- TukeyHSD(aov01)
plot(tukeyTable01, las = 1)
pairwise.t.test(allData_01$pmValues, allData_01$location)

allData_25 <- allData %>%
  filter(pmType == "pm25") %>%
  select(pmValues, location)
aov25 <- aov(pmValues ~ location, data = allData_25)
tidy(aov25)
tukeyTable25 <- TukeyHSD(aov25)
plot(tukeyTable25, las = 1)
pairwise.t.test(allData_25$pmValues, allData_25$location)

allData_10 <- allData %>%
  filter(pmType == "pm10") %>%
  select(pmValues, location)
aov10 <- aov(pmValues ~ location, data = allData_10)
tidy(aov10)
tukeyTable10 <- TukeyHSD(aov10)
plot(tukeyTable10, las = 1)
pairwise.t.test(allData_10$pmValues, allData_10$location)

tukey01 <- as.data.frame(tukeyTable01$location)
tukey01$pair = rownames(tukey01)
tukey01$set = "pm01"

tukey25 <- as.data.frame(tukeyTable25$location)
tukey25$pair = rownames(tukey25)
tukey25$set = "pm25"

tukey10 <- as.data.frame(tukeyTable10$location)
tukey10$pair = rownames(tukey10)
tukey10$set = "pm10"

tukeyFull <- bind_rows(tukey01, tukey25, tukey10)
tukeyFull <- tukeyFull %>%
  mutate(label = case_when(
    `p adj` < 0.05 ~ "p < 0.05", ## Reject null hypothesiss; diff is significant
    `p adj` >= 0.05 ~ "Non-Sig" ## Fail to reject null hyp; diff is not significant
  ))

tukeyFull$set <- factor(tukeyFull$set, levels = c('pm01', 'pm25', 'pm10'))

ggplot(tukeyFull, aes(color = label))+
  facet_wrap(~set,
             labeller = labeller(set = pmLabeller))+
  geom_hline(yintercept=0, lty="11", color="grey30") +
  geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.2) +
  geom_point(aes(pair, diff)) +
  labs(color="",
       x = "Sublocation pairing",
       y = "Difference")+
  scale_x_discrete()+
  theme_few()+
  coord_flip()

pTab <- tukeyFull %>%
  select(pair, set, `p adj`) %>%
  pivot_wider(names_from = set,
              values_from = `p adj`)

sumAll <- allData %>%
  group_by(location, pmType) %>%
  summarize(count = n(),
            mean = mean(pmValues),
            sd = sd(pmValues)) %>%
  pivot_wider(names_from = pmType,
              values_from = c(count, mean, sd)) %>%
  mutate(count = count_pm01) %>%
  select(!c(count_pm01, count_pm25, count_pm10)) %>%
  relocate(location, count,
           mean_pm01, sd_pm01,
           mean_pm25, sd_pm25,
           mean_pm10, sd_pm10) %>%
  arrange(factor(location, levels = c('const', 'bike', 'ped', 'mart', 'lib', 'fire'))) %>%
  mutate(count = as.character(count)) %>%
  mutate_if(is.numeric, round, 0) %>%
  mutate(count = as.numeric(count))


# ## TESTING BELOW
# 
# ## Source: https://stackoverflow.com/questions/33644034/how-to-visualize-pairwise-comparisons-with-ggplot2
# ggplot(tukey01, aes(colour=cut(`p adj`, c(0, 0.01, 0.05, 1), 
#                            label=c("p<0.01","p<0.05","Non-Sig")))) +
#   geom_hline(yintercept=0, lty="11", colour="grey30") +
#   geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.2) +
#   geom_point(aes(pair, diff)) +
#   labs(colour="")+
#   coord_flip()+
#   theme_few()
# 
# 
# ## one way ANOVA
# testStat <- allData %>%
#   specify(pmValues ~ location) %>%
#   calculate(stat = "F")
# testStat
# 
# summaryData <- allData %>%
#   group_by(location) %>%
#   summarize(n(), mean(pmValues), sd(pmValues))
# summaryData
# 
# aovTable <- aov(pmValues ~ location, data = allData)
# tidy(aovTable)
# 
# ## tukey
# tukeyTable <- TukeyHSD(aovTable)
# tidyTukey <- tidy(tukeyTable)
# plot(tukeyTable, las = 1)
# 
# pairwise.t.test(allData$pmValues, allData$location)
# 
# ## One way ANOVA
# aov <- allData %>%
#   mutate(sublocation = as.factor(location))
# summary(aov(pmValues ~ sublocation, aov))
# 
# mod <- aov(pmValues ~ location, data = allData)
# summary(mod)
# tidy(mod)
# 
# lm <- lm(pmValues ~ location, allData)
# summary(lm)
# tidy(lm)
# 
# ggplot(allData, aes(x = location, y = pmValues))+
#   geom_boxplot()+
#   stat_summary(fun.y = mean, colour = "red", geom = "point", size = 3)
# 
# allData_stat <- allData %>%
#   specify(pmValues ~ location) %>%
#   calculate(stat = "F")
# allData_stat
# 
# allData_nullDist <- allData %>%
#   specify(pmValues ~ location) %>%
#   hypothesize(null = "independence") %>%
#   generate(reps = 1000, type = "permute") %>%
#   calculate(stat = "F")
# 
# allData_summary <- allData %>%
#   group_by(location) %>%
#   summarize(n(), mean(pmValues), sd(pmValues))
# 
# allData_aov <- aov(pmValues ~ location, data = allData)
# tidy(allData_aov)
# summary(allData_aov)
# 
# 
# ## Two-way anova
# table(allData$location, allData$pmType)
# 
# allData_aov2 <- aov(pmValues ~ location + pmType, data = allData)
# tidy(allData_aov2)
# summary(allData_aov2)
# 
# summStats <- allData %>%
#   group_by(location, pmType) %>%
#   summarize(n(), mean(pmValues), sd(pmValues))
# summStats
# 
# 
# ## Pipe test. No dice, will fix later. Does the same as above. Source: https://sebastiansauer.github.io/multiple-t-tests-with-dplyr/
# # allDataSum <- allData %>%
# #   select(-set) %>%
# #   group_by(location, pmType) %>%
# #   summarize(pmValues = list(pmValues)) %>%
# #   spread(location, pmValues) %>%
# #   mutate(p_test = t.test(unlist(bike),
# #                          unlist(const),
# #                          unlist(fire),
# #                          unlist(lib),
# #                          unlist(mart),
# #                          unlist(ped))$p.value,
# #          t_test = t.test(unlist(bike),
# #                          unlist(const),
# #                          unlist(fire),
# #                          unlist(lib),
# #                          unlist(mart),
# #                          unlist(ped))$statistic)