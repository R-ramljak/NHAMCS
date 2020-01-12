
  
library(tidyverse)
library(broom)
library(survey)
library(sampling)
library(poliscidata)
library(svglite)
'%!in%' <- function(x,y)!('%in%'(x,y))
# setwd("~/Desktop/Studie/M&S/R") # Set this to the directory where your "nhamcsed2010-short.csv" is present




data <- read_csv("nhamcsed2010-short.csv") %>% # Place the file in R working memory
  rownames_to_column("id") %>% # The first column now contains the row numbers under the label "id"
  mutate_at(vars(-LOV, -WAITTIME), ~replace(., . < 0, NA)) # All values below 0 (the "missings") are set to NA. For WAITTIME and LOV however, the values below 0 are kept





### 1. The variable CPSUM represents geographic primary sampling units (clusters), a variable CSTRATM represents relevant strata (e.g., emergency service areas within emergency departments or outpatient departments), variable PATWT represents an additional patient visit weight variable.

#### a. Explain this multistage sampling design. Do the PATWT weights sum up to N, and what does this mean?


sum(data$PATWT) # Take the sum of all datapoints in variable PATWT


#### b. Define the svydesign() function with clusters, strata, and (additional) weights.


data.first <- data %>% 
  mutate(WAITTIME = replace(WAITTIME, WAITTIME < 0, values = NA)) # Creating a temporary data frame that also sets negative value for Waiting time to NA

data.design <- svydesign(id = ~CPSUM, strata = ~CSTRATM, weight = ~PATWT, data = data.first) # Specification of survey design using survey package


#### c. When considering gender differences in emergency visits, is it more likely for men than women to visit an emergency department, explain?


# Note: In this dataset males and females are coded as following:

# 1 --> Female
# 2 --> Male

# Population estimates for Males and Females 
svytotal(~interaction(SEX), data.design) 



#### d. Is there a mean difference between males and females in weights (PATWT), what do you conclude from this?


# Summarise data
data %>% 
  group_by(SEX) %>% # Group the data into male and female
  summarise(n = n(), mean(PATWT)) # Count the number of males and females, and show their means



#### Further discussing the weights --> Piet can you make a nicer graph please


# Exploring the weights
weight.summary <- data %>%      # Creates new variable which uses 'data'
  group_by(PATWT) %>%           # Group data by weights
  summarise(number = n()) %>%   # Count how many of each weights are present in 'data'
  arrange(PATWT)                # Sort them low to high
n_distinct(data$PATWT)          # Gives the amount of unique weights as output

# Histogram of weights variable PATWT
PATWT.plot <- data %>% 
  ggplot() +
  geom_histogram(aes(x = PATWT), bins = 410) +
  labs(title = "Histogram of survey weights 'PATWT'", 
       x = "PATWT (weights)", y = "Amount") +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  scale_x_continuous(limits = c(0, 15100)) 

ggsave(filename = "results/PATWT.plot.svg", plot = PATWT.plot, device = "svg")
ggsave(filename = "results/PATWT.plot.png", plot = PATWT.plot, device = "png")



### 2. Estimate the average waiting time in the population under the specified svydesign. 

#### a. What do you conclude?


(result.2a <- svymean(~WAITTIME, data.design, na.rm = T)) # Calculate mean waiting time taking the survey design into account



# Analysis of missing data and potentially imputable observations by using the total length of visit (LOV)
imputation.table <- data %>% 
  # Mutate creates a new variable called helper.impute
  # In this new variable, case_when functions as an if-then statement
  # "Missing, but not imputable" = -9 on both WAITTIME and LOV
  # "Missing, but imputable" = -9 on WAITTIME but not -9 on LOV
  # "not applicable" = -7 on WAITTIME
  # "Observed" = complete observations
  mutate(helper.impute = case_when(WAITTIME == -9 & LOV == -9 ~ "Missing, but not imputable",
                                   WAITTIME == -9 & LOV != -9 ~ "Missing, but imputable",
                                   WAITTIME == -7 ~ "not applicable",
                                   TRUE ~ "Observed")) %>% # TRUE functions as 'everything else' 
  group_by(helper.impute) %>% # Now we sort data by the four categories made
  # Here we create descriptive statistics about the missing values
  summarise(percentage.sample = round( (n()/34936) * 100, 2), 
            percentage.population = round((sum(PATWT)/129843377) * 100, 2),
            sample.total = n(),
            population.total = sum(PATWT))
# Create a csv table for Appendix 
write.csv2(file = "results/imputatation.table.csv", x = imputation.table, row.names = F)


# Imputing values single imputation by calculating the average waiting time proportion and multiplying it by the  LOV time
data.imputed <- data %>%
  # First we create a variable which contains the waiting time proportion of length of visit (under the condition that both values are observed)
  mutate(WAITTIME.prop = case_when(WAITTIME != -9 & WAITTIME != -7 & LOV != -9 ~ WAITTIME/LOV,
                                   TRUE ~ NA_real_)) %>%
  # Then we multiply the waiting time/length-of-visit proportion with length of visit. This is done under the condition that WAITTIME equals -7 and LOV is not missing. 
  mutate(WAITTIME.imputed = case_when(WAITTIME != -9 & WAITTIME != -7 ~ WAITTIME,
                                      WAITTIME == -7 ~ -7,
                                      WAITTIME == -9 & LOV != -9 ~ LOV * weighted.mean(x = WAITTIME.prop, 
                                                                                       w = PATWT,
                                                                                       na.rm = T))) %>% 
  # Here we keep track of what we have done and save this in variable impute.group
  mutate(impute.group = case_when(WAITTIME != -9 & WAITTIME != -7 ~ "Observed, not imputed",
                                  WAITTIME == -7  ~ "Not applicable",
                                  WAITTIME == -9 & LOV != -9 ~ "Imputed",
                                  TRUE ~ "Missing, but not imputable")) %>% 
  # Finally we turn the remaining missing values into NA
  mutate(WAITTIME.imputed = case_when(WAITTIME.imputed %in% c(-7, -9) ~ NA_real_,
                                      TRUE ~ WAITTIME.imputed))

# Summary statistics for waiting time/length-of-visit proportion
weighted.mean(data.imputed$WAITTIME.prop, w = data.imputed$PATWT, na.rm = T) # Calcuates the ratio we use for imputation
# New design with imputed values for waiting time
data.design.imputed <- svydesign(id = ~CPSUM, strata = ~CSTRATM, weight = ~PATWT, data = data.imputed)
data.imputed.filter <- data.imputed %>% 
  filter(impute.group == "Imputed") # This creates a variable containing only the imputed data
data.design.imputed.filter <- svydesign(id = ~CPSUM, strata = ~CSTRATM, weight = ~PATWT, data = data.imputed.filter)
# Comparing imputation strategy with original data 
result.2a <- svymean(~WAITTIME, data.design, na.rm = T)
# Only the imputed ones
result.2a.imputed.filter <- svymean(~WAITTIME.imputed, data.design.imputed.filter, na.rm = T)
# Combined 
result.2a.imputed <- svymean(~WAITTIME.imputed, data.design.imputed, na.rm = T)
# Analysing the weights concerning the imputing.group variable
imputation.comparison.table <- data.imputed %>% 
  mutate(LOV.new = case_when(LOV == -9 ~ NA_real_,
                             TRUE ~ LOV)) %>% 
  group_by(impute.group) %>% 
  summarise(mean.weight = mean(PATWT),
            mean.WAITTIME = mean(WAITTIME.imputed, na.rm = T))
write.csv2(x = imputation.comparison.table, file = "results/imputation.comparison.table.csv")




#### b. Exclude the cluster and strata information, (redefine the svydesign), how this change your result in 2a, explain?


# Defining the design object but without any design specifications
data.design.exc <- svydesign(id = ~id, weight = ~PATWT, data = data.imputed) 

result.2b <- svymean(~WAITTIME.imputed, data.design.exc, na.rm = T)


#### c. Draw an SRS sample to estimate the average waiting time. Estimate the average waiting time, and compare the result(s) with the result(s) of 2a and 2b.



# Drawing three srs samples from our original dataset (n = 1000, 5000, 10000) and calculating the final weights
srs.names <- c("SRS.1000", "SRS.5000", "SRS.10000")
srs.sizes <- c(1000, 5000, 10000)
set.seed(347)
srs.samples.vec <- pmap(list(x = length(srs.sizes), y = srs.sizes), ~srswr(n = .y, N = length(data.imputed$id)))

srs.samples.data <- map(srs.samples.vec, ~getdata(data.imputed, .x)) %>% 
  map2(., srs.sizes, ~mutate(.x, final.weights = PATWT * (34936 / .y)))

# Checking if the the new combined weights approximately add up to the original N 
map(srs.samples.data, ~sum(.$final.weights)) 

# Create for each SRS sample a svydesign object with the specific weights, excluding strata and cluster info
srs.samples.designs <- map(srs.samples.data, ~svydesign(id = ~id, weight = ~final.weights, data = .)) %>% 
  set_names(nm = srs.names)

# Calculating the mean estimate of WAITTIME of the SRS samples and comparing them to the designs from before
results.srs <- map(srs.samples.designs, ~svymean(~WAITTIME.imputed, .x, na.rm = T))

# Comparing the results 
name.results <- c("2a Estimate (design specified)", "2b Estimate (design not specified)",
                  "2c Estimate SRS n = 1000", "2c Estimate SRS n = 5000", "2c Estimate SRS n = 10000")

# Still need to add the SEs to the table for better comparison
results.2 <- tibble(sample = name.results, 
                    estimates = c(result.2a.imputed, result.2b, results.srs[[1]], results.srs[[2]], results.srs[[3]]),
                    SE = c(map_dbl(list(result.2a.imputed, result.2b, results.srs[[1]], results.srs[[2]], results.srs[[3]]), 
                                   ~sqrt(attr(., which = "var")))),
                    Bias = estimates - result.2a.imputed[[1]]) # add design effect




#### d. Draw a stratified sample to investigate the variability in waiting times over regions (Northeast, Midwest, South, West). Is there variability in waiting times over regions?


# In order for drawing a stratified sample we need to arrange the original dataset in ascending strata order (Region)
data.strata.prep <- data.imputed %>%
  arrange(REGION)

# building a helper dataset that contains Region specific values for calculating the strata specific sizes and weights (Equal size, PPS, Neyman) for complete sample sizes of 10000
strata.sizes <- data.imputed %>%
  group_by(REGION) %>%
  summarise(prop = (n() / 34936), sd.region = sd(WAITTIME.imputed, na.rm = T)) %>%
  mutate(population.size = prop * 34936) %>% 
  mutate(strata.equal = 2500) %>% 
  mutate(strata.pps = round(prop * 10000)) %>%
  mutate(strata.neyman = round((((sd.region * population.size) / sum(population.size * sd.region)) * 10000 ), 0))


# Drawing the stratified samples with the same seed to get relative similar samples and without replacement to have constant inclusion probabilities
# Equal size
set.seed(87)
equal.dataset <- strata(data = data.strata.prep, stratanames = "REGION", size = strata.sizes$strata.equal, method = "srswr")
# PPS
set.seed(87)
pps.dataset <- strata(data = data.strata.prep, stratanames = "REGION", size = strata.sizes$strata.pps, method = "srswr")
# Neyman
set.seed(87)
neyman.dataset <- strata(data = data.strata.prep, stratanames = "REGION", size = strata.sizes$strata.neyman, method = "srswr")

# deriving the stratified sample datasets and calculating the specfic weight combination
stratified.samples.datasets <- list(equal.dataset, pps.dataset, neyman.dataset) %>% 
  map(~getdata(data = data.strata.prep, m = .)) %>% 
  map(~left_join(., strata.sizes, by = "REGION")) %>% # join strata specific sizes (4 levels for 4 regions)
  map2(., tibble(.[[1]]$strata.equal, .[[2]]$strata.pps, .[[3]]$strata.neyman), 
       ~mutate(.x, weights.strata = population.size / .y)) %>% # calculating strata specific weights (Nh / nh) -_> equal between strata
  map(~mutate(., final.weights = PATWT * weights.strata)) # Combining the original weights with the strata specific weights

# Checking if the the new combined weights approximately add up to the original N 
map_dbl(stratified.samples.datasets, ~sum(.$final.weights)) 

# Creating the survey design objects with specialized weights for each of the stratified samples
stratified.samples.design <- map(stratified.samples.datasets, 
                                 ~svydesign(id = ~id, strata = ~REGION, weights = ~final.weights, data = .))


# Calculating the mean estimate of WAITTIME of for all stratified samples
results.stratified <- map(stratified.samples.design, ~svymean(~WAITTIME.imputed, .x, na.rm = T))

# Comparing the results and adding them to the srs comparison table
name.stratified.results <- c("2d Estimate Equal sizes n = 10000", "2d Estimate PPS n = 10000", "2d Estimate Neyman n = 10000")
results.stratified.2 <- tibble(sample = name.stratified.results, 
                               estimates = c(results.stratified[[1]], results.stratified[[2]], results.stratified[[3]]),
                               SE = map_dbl(results.stratified, 
                                            ~sqrt(attr(., which = "var"))),
                               Bias = estimates - result.2a.imputed[[1]]) # add design effect
results.WT.final <- bind_rows(results.2, results.stratified.2)
write.csv2(x = results.WT.final, file = "results/Q2.final.table.csv", row.names = F)


# Testing the variability between regions concerning waiting time only for the neyman sample
regional.estimate <- svyby(~WAITTIME.imputed, ~REGION, stratified.samples.design[[3]], svymean, deff = T, na.rm = T) # check this 
write.csv2(x = regional.estimate, file = "results/Q2.regional.estimate.csv", row.names = F)



### 3. Explain that a ratio estimator can be used to improve the estimation of the average waiting time. 

#### b. Describe potential variables for ratio estimation, and choose one to compute a ratio estimate of the average weighting time. 

#### c. Give a 95% confidence interval for your preferable estimate of the average waiting time, and explain the result.


# Calculating a ratio estimator and its corresponding CI with the variable ARREMS --> If one arrived by ambulance or not
ratio.3 <- svyratio(~WAITTIME.imputed, ~ARREMS, design = data.design.imputed, na.rm = T)
ratio.3.predict <- predict(ratio.3, total = mean(data.imputed$ARREMS, na.rm = T), interval = "confidence")

ci.lb <- ratio.3.predict$total - (ratio.3.predict$se * 1.96) 
ci.ub <- ratio.3.predict$total + (ratio.3.predict$se * 1.96) 
ci <- c(ci.lb, ci.ub)



### 4. Investigate differences in waiting times among ethnic groups by fitting a linear regression model. Compare estimated results of a stratified sampling design and an unequal probability sampling design.



## Testing the relationship between ethnic groups and their waiting times with the original design, the SRS design (n=10000) and the neyman design

# regression diagnostics
mod.Q4.diag <- lm(WAITTIME.imputed ~ as.factor(ETHUN), data = data.imputed)
plot(mod.Q4.diag)

## Checking for influential outliers in order to possibly exclude cases
mod.baseline.w.outlier <- data.imputed
mod.baseline.wo.outlier <- data.imputed %>% 
  filter(WAITTIME.imputed <= 600) # dropping cases with waiting time higher than 10 hours (600 minutes)

# Design with ourliers
mod.baseline.w.outlier.design <- svydesign(id = ~CPSUM, strata = ~CSTRATM, weight = ~PATWT, data = mod.baseline.w.outlier)

# Design without outliers
mod.baseline.wo.outlier.design <- svydesign(id = ~CPSUM, strata = ~CSTRATM, weight = ~PATWT, data = mod.baseline.wo.outlier)

# Regression with outliers
(model.Q4.original <- svyglm(formula = WAITTIME.imputed ~ as.factor(ETHUN), design = mod.baseline.w.outlier.design, 
                             data = mod.baseline.w.outlier))
(model.Q4.original$R2 <- 1 - model.Q4.original$deviance/model.Q4.original$null.deviance)

# Regression without outliers
(model.Q4.original.wo.outlier <- svyglm(formula = WAITTIME.imputed ~ as.factor(ETHUN), design = mod.baseline.w.outlier.wo.design, 
                                        data = mod.baseline.wo.outlier))
(model.Q4.original.wo.outlier$R2 <- 1 - model.Q4.original.wo.outlier$deviance/model.Q4.original.wo.outlier$null.deviance)



## --> SRS model n = 10000

mod.srs <- srs.samples.designs[[3]]

(model.Q4.srs <- svyglm(formula = WAITTIME.imputed ~ as.factor(ETHUN), design = mod.srs, 
                        data = data.imputed))
(model.Q4.srs$R2 <- 1 - model.Q4.srs$deviance/model.Q4.srs$null.deviance)


## --> Neymann model n = 10000

mod.neyman <- stratified.samples.design[[3]]

(model.Q4.neyman <- svyglm(formula = WAITTIME.imputed ~ as.factor(ETHUN), design = mod.neyman, 
                           data = data.imputed))
(model.Q4.neyman$R2 <- 1 - model.Q4.neyman$deviance/model.Q4.neyman$null.deviance)


## plots

datasets.combined <- list(data.imputed, srs.samples.data[[3]], stratified.samples.datasets[[3]]) %>%
  set_names(nm = c("Original.design", "SRS", "Neyman")) %>% 
  map_at("Original.design", ~mutate(., final.weights = PATWT)) %>% 
  map2_dfr(c("Original design", "SRS", "Neyman"), ~mutate(.x, design.kind = paste0(.y))) %>% 
  mutate(design.kind = factor(design.kind, levels = c("Original design", "SRS", "Neyman"), 
                              labels = c("Original design", "SRS n = 10,000", "Neyman n = 10,000"), ordered = T)) %>% 
  filter_at(vars(matches("ETHUN|WAITTIME")), ~ !is.na(.)) %>%
  mutate(Ethnicity = factor(ETHUN, levels = c(1, 2), labels = c("Hispanic or Latino", "Not Hispanic or Latino"))) %>% 
  select(Ethnicity, WAITTIME.imputed, final.weights, design.kind)

(reg.plot.Q4 <- ggplot() +
    geom_smooth(data = datasets.combined, 
                aes(x = Ethnicity, y = WAITTIME.imputed, group = design.kind, color = design.kind, weight = final.weights), 
                method = "lm", se = F) +
    labs(title = "Waiting Time over different Ethnicities and Designs", subtitle = "Question Nr. 4",
         x = "", y = "Waiting Time in Minutes", color = "Sampling Design") +
    theme_bw() +
    theme(text = element_text(size = 15), 
          axis.text.x = element_text(size = 13, face = "bold")))
ggsave(filename = "results/plot.Q4.svg", plot = reg.plot.Q4, device = "svg")
ggsave(filename = "results/plot.Q4.png", plot = reg.plot.Q4, device = "png")




### 5. Assume unequal probability sampling, can you identify the important predictor variables of waiting times of patients visiting an ED? What can be concluded?



# Manipulating variables to use them as potential predictors
data.Q5 <- data.imputed %>% 
  mutate(day.kind = factor(case_when(VDAYR %in% c(2:6) ~ "Weekdays",
                                     TRUE ~ "Weekend"),
                           levels = c("Weekdays", "Weekend"))) %>% 
  mutate(time = case_when(nchar(ARRTIME) == 4 ~ sub("(.{2})(.*)", "\\1:\\2", ARRTIME),
                          nchar(ARRTIME) == 3 ~ paste0("0", sub("(.{1})(.*)", "\\1:\\2", ARRTIME)),
                          nchar(ARRTIME) == 2 ~ paste0("00:", ARRTIME),
                          nchar(ARRTIME) == 1 ~ paste0("00:0", ARRTIME))) %>% 
  mutate(hour = substr(time, 1, 2)) %>% 
  mutate(time.kind = factor(case_when(hour %in% c(7:23) ~ "Daytime",
                                      TRUE ~ "Nighttime"),
                            levels = c("Daytime", "Nighttime"))) %>%
  mutate(age.kind = factor(case_when(AGE < 18 ~ "Under 18",
                                     AGE %in% c(18:65) ~ "Between 18 and 65",
                                     AGE > 65 ~ "Over 65"), 
                           levels = c("Under 18", "Between 18 and 65", "Over 65"))) %>% 
  mutate(sex.kind = factor(case_when(SEX == 1 ~ "Female",
                                     SEX == 2 ~ "Male"),
                           levels = c("Female", "Male"))) %>%
  mutate(ethnicity.kind = factor(case_when(ETHUN == 1 ~ "Hispanic or Latino",
                                           ETHUN == 2 ~ "Not Hispanic or Latino",
                                           is.na(ETHUN) ~ "Missing or Blank"),
                                 levels = c("Hispanic or Latino", "Not Hispanic or Latino", "Missing or Blank"))) %>%
  mutate(race.kind = factor(case_when(RACEUN == 1 ~ "White",
                                      RACEUN == 2 ~ "Black/African American",
                                      RACEUN == 3 ~ "Asian",
                                      RACEUN == 4 ~ "Native Hawaiian/Other Pacific Islander",
                                      RACEUN == 5 ~ "American Indian/Alaska Native",
                                      RACEUN == 6 ~ "More than one race reported",
                                      is.na(RACEUN) ~ "Missing or Blank"),
                            levels = c("White", "Black/African American", "Asian", "Native Hawaiian/Other Pacific Islander",
                                       "American Indian/Alaska Native", "More than one race reported", "Missing or Blank"))) %>%
  mutate(residence.kind = factor(case_when(RESIDNCE == 1 ~ "Private residence",
                                           RESIDNCE == 2 ~ "Nursing home",
                                           RESIDNCE == 3 ~ "Homeless",
                                           RESIDNCE == 4 ~ "Other",
                                           is.na(RESIDNCE) ~ "Missing or Blank"),
                                 levels = c("Private residence", "Nursing home", "Homeless", "Other", "Missing or Blank"))) %>%
  mutate(arrival.kind = factor(case_when(ARREMS == 1 ~ "Arrival by Ambulance",
                                         ARREMS == 2 ~ "No Arrival by Ambulance",
                                         is.na(ARREMS) ~ "Missing or Blank"),
                               levels = c("Arrival by Ambulance", "No Arrival by Ambulance", "Missing or Blank"))) %>%
  mutate(payment.kind = factor(case_when(PAYPRIV == 1 ~ "Private",
                                         PAYMCAID == 1 | PAYMCARE == 1 & PAYPRIV == 0 ~ "Medicare/Medicaid",
                                         TRUE ~ "No expected source of Payment"),
                               levels = c("Private", "Medicare/Medicaid", "No expected source of Payment")))




##Estimate table

std <- function(x) {
  sd(x, na.rm = T)/sqrt(length(x))
}
# Vector helper for further group iteration
group.vars <- grep("kind", names(data.Q5), value = T)
group.syms <- rlang::syms(group.vars)

# Creating a descriptives table based on all the groups within each variable respectively
pred.estimate.table <- map_dfr(.x = group.syms, ~ data.Q5 %>% 
                                 group_by(!!.x) %>% 
                                 summarise(Variable = paste(.x), 
                                           Population.total = sum(PATWT), 
                                           Total.sample = n(), 
                                           Mean.Weight = round(mean(PATWT), 2), 
                                           Mean.WaitingTime = round(mean(WAITTIME.imputed, na.rm = T), 2), 
                                           SE.Mean.WaitingTime = round(std(WAITTIME.imputed), 2),
                                           Median.WaitingTime = round(median(WAITTIME.imputed, na.rm = T), 0))) %>% 
  unite(name, group.vars, sep = "", remove = T, na.rm = T) %>% 
  select(Variable, everything())
write.csv2(x = pred.estimate.table, file = "results/pred.estimate.table.csv", row.names = F)


# Creating a new design object with the potential predictor variables
data.Q5.design <- svydesign(id = ~CPSUM, strata = ~CSTRATM, weight = ~PATWT, data = data.Q5)

# Predictor variable helper for easy iterating different models
predictor.helper <- list(mod1 = c("day.kind"),
                         mod2 = c("day.kind", "time.kind"),
                         mod3 = c("day.kind", "time.kind", "age.kind"),
                         mod4 = c("day.kind", "time.kind", "age.kind", "sex.kind"),
                         mod5 = c("day.kind", "time.kind", "age.kind", "sex.kind", "ethnicity.kind"),
                         mod6 = c("day.kind", "time.kind", "age.kind", "sex.kind", "ethnicity.kind", "race.kind"),
                         mod7 = c("day.kind", "time.kind", "age.kind", "sex.kind", "ethnicity.kind", "race.kind", "arrival.kind"),
                         mod8 = c("day.kind", "time.kind", "age.kind", "sex.kind", "ethnicity.kind", "race.kind", "arrival.kind",
                                  "residence.kind"),
                         mod9 = c("day.kind", "time.kind", "age.kind", "sex.kind", "ethnicity.kind", "race.kind", "arrival.kind",
                                  "residence.kind", "payment.kind"))

# Stepwise Regression models for Q5
models.Q5 <- predictor.helper %>% 
  map(~svyglm(as.formula(paste("WAITTIME.imputed ~", paste(.x, collapse = "+"))),
              design = data.Q5.design, 
              data = data.Q5))

# Building a table with GOF measures of all the tables
fit.table <- models.Q5 %>% 
  map(~append(., tibble(r2 = fit.svyglm(.x)[1], adj.r2 = fit.svyglm(.x)[2]))) %>% 
  map2_dfr(., names(predictor.helper), ~tibble(Model = .y, n = length(.x$fitted.values), r2 = .x$r2, adj.r2 = .x$adj.r2)) %>% 
  pivot_longer(-Model, names_to = "key", values_to = "value", values_ptypes = list(value = 'character')) %>% 
  pivot_wider(names_from = Model, values_from = value) %>% 
  rename(term = key)

# Building a table with all the estimate measures + appending the GOF measures to it and making it ready for publication in word
predictors.table <- models.Q5 %>% 
  map(~tidy(.)) %>% 
  map2(names(predictor.helper), ~mutate(.x, Model = .y)) %>% 
  map_dfr(~select(., Model, everything())) %>% 
  mutate(sig = case_when(p.value > 0.05 ~ "",
                         p.value <= 0.05 & p.value > 0.01 ~ "*",
                         p.value <= 0.01 & p.value > 0.001 ~ "**",
                         p.value < 0.001 ~ "***")) %>% 
  mutate(estimate.new = paste0(round(estimate, 2), " ", sig, " (", round(std.error, 2), ")")) %>% 
  select(Model, term, estimate.new) %>% 
  pivot_wider(names_from = c(Model), values_from = estimate.new) %>% 
  bind_rows(fit.table)
write.csv2(x = predictors.table, file = "results/predictors.table.csv", row.names = F)





a <- data.Q5 %>% 
  group_by(CPSUM, ethnicity.kind) %>% 
  summarise(pop.group = sum(PATWT), mean.wait = mean(WAITTIME.imputed, na.rm = T)) %>%
  mutate(percentage.population = pop.group / sum(pop.group)) %>%
  filter(!ethnicity.kind == "Missing or Blank") %>% 
  pivot_wider(CPSUM, names_from = ethnicity.kind, values_from = c(mean.wait, percentage.population)) %>% 
  mutate(wait.diff = abs(`mean.wait_Not Hispanic or Latino` - `mean.wait_Hispanic or Latino`)) %>% 
  mutate(pop.diff = abs(`percentage.population_Hispanic or Latino` - `percentage.population_Not Hispanic or Latino`)) %>% 
  mutate(majority = case_when(`percentage.population_Hispanic or Latino` > `percentage.population_Not Hispanic or Latino` ~ "Hispanic",
                              `percentage.population_Hispanic or Latino` == `percentage.population_Not Hispanic or Latino` ~ "None",
                              TRUE ~ "Not Hispanic")) %>% 
  ggplot() +
  geom_point(aes(x = pop.diff, y = wait.diff, color = majority))


