# Load packages
library(haven)

library(ggplot2)
library(car)
library(patchwork)

library(tidyverse)
library(psych)    # for descriptive stats
library(corrplot) # for correlation matrix visualization


# Read in data
# This is the only path that needs adaptations :)
data_muris <- read_sav("~/Desktop/Maastrich Data/Final dataset master thesis.sav")


###############################################################################
# Renaming variables
###############################################################################

# Two gender variables found: gender and filter_$
# Check for gender
table(data_muris$`filter_$`) # 0 = female, 1 = male
# Check for gender
table(data_muris$gender) # 1 = male, 2 = female

# Delete gender variable
data_muris$gender <- NULL

# Rename filter_$ to gender
names(data_muris)[names(data_muris) == "filter_$"] <- "gender"
# Rename column description of the new gender variable
attr(data_muris$gender, "label") <- "0 = female, 1 = male"


###############################################################################
# Reproduction of muris' sample demographics
###############################################################################
# Missing values in variable age (used as "control")
sum(is.na(data_muris$age)) # no missing values

# Total number of participants (calculated with "control variable" age)
total_n <- length(data_muris$age)


# Mean age of sample
mean(data_muris$age, na.rm = TRUE)

# Standart deviation of age
sd(data_muris$age, na.rm = TRUE)

# Range of age
range(data_muris$age, na.rm = TRUE)

# Number of participants between 18 and 28 (inclusive)
n_18_28 <- sum(data_muris$age >= 18 & data_muris$age <= 28, na.rm =  TRUE)
# Percentage of participants between 18 and 28 (inclusive)
percent_18_28 <- (n_18_28 / total_n) * 100
percent_18_28


# Table of variable ethnicgroup
table(data_muris$ethnicgroup) # Problem:variable is numerically coded, while no explicit codebook was available
# Based on frequency distributions and reference in the original publication by Muris et al. (2020) I inferred:
# "2" = caucasian

# Table of variable edu (education)
table(data_muris$edu) # Problem:variable is numerically coded, while no explicit codebook was available
# Based on frequency distributions and reference in the original publication by Muris et al. (2020) I inferred:
# "1" = High school diploma or equivalent (24.6%)
# "2" = Bachelor's degree (44.6%)
# "3" = Master's degree (22.3%)
# "4" = Doctoral degree (5.8%)
# "5" = Vocational training (0.8%)
# Rename for clarity:
data_muris$edu <- factor(data_muris$edu,
                                 levels = c(1, 2, 3, 4, 5),
                                 labels = c("HS", "BSC", "MSC", "PhD", "VT"))


# Explore gender (categorical) differences regarding age (continuous)
t.test(age ~gender, data = data_muris)

# Explore gender (categorical) differences regarding ethinicity (categorical)
# Gender by education:
table_gender_ethnic <- table(data_muris$gender, data_muris$ethnicgroup)
chisq.test(table_gender_ethnic) # Since on cell in contigency table has frequency below 5 -> fisher.test (violation of chisq.test)
fisher.test(table_gender_ethnic) 

# Explore gender (categorical) differences regarding education (categorical)
# Gender by education:
table_gender_edu <- table(data_muris$gender, data_muris$edu)
chisq.test(table_gender_edu) # Since on cell in contigency table has frequency below 5 -> fisher.test (violation of chisq.test)
fisher.test(table_gender_edu) 

# Explore found gender differences (for edu only since ethnicgroup could not be reliably inferred) more:
prop.table(table(data_muris$gender, data_muris$edu), margin = 1) * 100


###############################################################################
# Reproduction of muris' reporting on internal consistency 
###############################################################################


# Confirm report of Muris Cronbach's alpha total score for SRPS
# Select all 26 items
srps_total <- data_muris[, paste0("psy_", 1:26)]
# Calculate Cronbach's Alpha for total score
alpha(srps_total) # Muris found α = .9, I found α = .9

# Calculate Cronbach's Alpha for subscale scores
# Check full description of Items Muris used tp confirm that he really used SRPS by Levenson et al. (1995)
# Show all labels from psy_1 to psy_26 
labels <- sapply(data_muris[, paste0("psy_", 1:26)], function(x) attr(x, "label"))
for (i in seq_along(labels)) {
  cat(paste0("Item ", i, " (", names(labels)[i], "): ", labels[[i]], "\n"))
}
# Since Levenson (1995) defined which items refer to primary and which to secondary psychopathy, I applied their codebook;
# This resulted in:
# Define items for each subscale:
primary_items <- data_muris[, paste0("psy_", c(1, 4, 5, 7, 9, 10, 11, 12, 13, 14, 15, 16, 18, 19, 20, 24))]
secondary_items <- data_muris[, paste0("psy_", c(2, 3, 6, 8, 17, 21, 22, 23, 25, 26))]  
# Since i could perfectly reproduce the total score Cronbach's alpha from Muris I could strongly assume that  he already applied reverse scoring to the relevant items

# Primary Psychopathy Subscale Cronbach's alpha
alpha(primary_items)

# Secondary Psychopathy Subscale Cronbach's alpha
alpha(secondary_items)

# Check alphas
library(psych)
alpha(primary_items)
alpha(secondary_items)



# Confirm report of Muris Cronbach's alpha total score for MACH-IV

# Select all 20 items
machIV_total <- data_muris[, paste0("mach_", 1:20)]
# Calculate Cronbach's Alpha for total score
alpha(machIV_total) 
# Output: Some items were negatively correlated with the first principal component and probably should be reversed.  
# Therefore:
alpha(machIV_total, check.keys = TRUE) # Muris found α = .78, I found α = .80

# Check full description of Items Muris used to confirm that he really used MAchIV 
# Show all labels from mach_1 to mach_20 
labels <- sapply(data_muris[, paste0("mach_", 1:20)], function(x) attr(x, "label"))
for (i in seq_along(labels)) {
  cat(paste0("Item ", i, " (", names(labels)[i], "): ", labels[[i]], "\n"))
}


# Confirm report of Muris Cronbach's alpha total score for NPI

# Select all 40 items
NPI_total <- data_muris[, paste0("narc_", 1:40)]
# Calculate Cronbach's Alpha for total score
alpha(NPI_total) #
# Output: Some items were negatively correlated with the first principal component and probably should be reversed.  
# Therefore:
alpha(NPI_total, check.keys = TRUE) # Muris reported: α = .86; I found: α = .86

# Check full description of Items Muris used to confirm that he really used NPI
# Show all labels from narc_1 to narc_40 
labels <- sapply(data_muris[, paste0("narc_", 1:40)], function(x) attr(x, "label"))
for (i in seq_along(labels)) {
  cat(paste0("Item ", i, " (", names(labels)[i], "): ", labels[[i]], "\n"))
}
# PROBLEM: Items were not labelled


# Confirm report of Muris Cronbach's alpha total score for HH

# Select all 16 items
HH_total <- data_muris[, paste0("hh_", 1:16)]
# Calculate Cronbach's Alpha for total score
alpha(HH_total) # Muris reported: α = .84; I found: α = .84

# Check full description of Items Muris used to confirm that he really used HEXACO-PI-R
# Show all labels from hh_1 to hh_16 
labels <- sapply(data_muris[, paste0("hh_", 1:16)], function(x) attr(x, "label"))
for (i in seq_along(labels)) {
  cat(paste0("Item ", i, " (", names(labels)[i], "): ", labels[[i]], "\n"))
}


# Calculate Cronbach's Alpha for subscale scores

# Check full description of Items Muris used in all four subscales
# Show all labels from hh_1 to hh_16 
labels <- sapply(data_muris[, paste0("hh_", 1:16)], function(x) attr(x, "label"))
for (i in seq_along(labels)) {
  cat(paste0("Item ", i, " (", names(labels)[i], "): ", labels[[i]], "\n"))
}


# Define items for each subscale (this has been done after checking if Muris used same order as in original HEXACO-PI-R which could be confirmed)
sincerity_items <- data_muris[, c("hh_1", "hh_5", "hh_10", "hh_13")]
fairness_items <- data_muris[, c("hh_2", "hh_6", "hh_9", "hh_14")]
greed_items <- data_muris[, c("hh_3", "hh_7", "hh_11", "hh_15")]
modesty_items <- data_muris[, c("hh_4", "hh_8", "hh_12", "hh_16")]

# Calculate cronbach's alpha
alpha(sincerity_items)
alpha(fairness_items)
alpha(greed_items)
alpha(modesty_items)


###############################################################################
# Calculate KR-20 for deviant and real deviant pornography use 
###############################################################################
# Calculate KR-20 for deviant pornography use

# Select all 7 items
DP_total <- data_muris[, paste0("devpor_", 1:7)]
# Calculate KR-20 for total score
kr20_result_DP <- psych::alpha(DP_total)  # this gives you KR-20 if items are binary
kr20_result_DP # α = .75

# Check full description of Items Muris used to confirm that he really used NPI
# Show all labels from devpor_1 to devpor_7
labels <- sapply(data_muris[, paste0("devpor_", 1:7)], function(x) attr(x, "label"))
for (i in seq_along(labels)) {
  cat(paste0("Item ", i, " (", names(labels)[i], "): ", labels[[i]], "\n"))
}


# Calculate KR-20 for real deviant pornography use

# Select the 3 items referring to real deviant pornography use
# devpor_1 (sex with young people under 18), devpor_4 (sex with animals), devpor_6 (rape)
RDP_total <- data_muris[, c("devpor_1", "devpor_4", "devpor_6")]
# Calculate KR-20 for total score
kr20_result_RDP <- psych::alpha(RDP_total)  # this gives you KR-20 if items are binary
kr20_result_RDP


###############################################################################
# Assumption checking: outliers
###############################################################################

# Visualize
# Show all DT at once
p1_box <- ggplot(data_muris, aes(y = psy_score)) +
  geom_boxplot(fill = "darkred") +
  theme_minimal() +
  labs(title = "Psychopathy", y = NULL)

p2_box <- ggplot(data_muris, aes(y = psy_sub1_prim)) +
  geom_boxplot(fill = "red") +
  theme_minimal() +
  labs(title = "Primary Psychopathy", y = NULL)

p3_box <- ggplot(data_muris, aes(y = psy_sub1_sec)) +
  geom_boxplot(fill = "red") +
  theme_minimal() +
  labs(title = "Secondary Psychopathy", y = NULL)

p4_box <- ggplot(data_muris, aes(y = mach_score)) +
  geom_boxplot(fill = "darkred") +
  theme_minimal() +
  labs(title = "Machiavellianism", y = NULL)

p5_box <- ggplot(data_muris, aes(y = narc_score)) +
  geom_boxplot(fill = "darkred") +
  theme_minimal() +
  labs(title = "Narcissism", y = NULL)

(p1_box | p2_box | p3_box) / (p4_box | p5_box)


# Show HH boxplots at once
hh1_box <- ggplot(data_muris, aes(y = hh_finalscore)) +
  geom_boxplot(fill = "darkgreen") +
  theme_minimal() +
  labs(title = "Honesty-Humility", y = NULL)

hh2_box <- ggplot(data_muris, aes(y = hh_sub1_sinc)) +
  geom_boxplot(fill = "green") +
  theme_minimal() +
  labs(title = "Sincerity", y = NULL)

hh3_box <- ggplot(data_muris, aes(y = hh_sub2_fair)) +
  geom_boxplot(fill = "green") +
  theme_minimal() +
  labs(title = "Fairness", y = NULL)

hh4_box <- ggplot(data_muris, aes(y = hh_sub3_gree)) +
  geom_boxplot(fill = "green") +
  theme_minimal() +
  labs(title = "Greed Avoidance", y = NULL)

hh5_box <- ggplot(data_muris, aes(y = hh_sub4_mod)) +
  geom_boxplot(fill = "green") +
  theme_minimal() +
  labs(title = "Modesty", y = NULL)

(hh1_box | hh2_box | hh3_box) / (hh4_box | hh5_box)


# Show Deviant Porn use boxplots at once
dp1_box <- ggplot(data_muris, aes(y = devpor_score)) +
  geom_boxplot(fill = "darkblue") +
  theme_minimal() +
  labs(title = "Deviant Porn Use", y = NULL)

dp2_box <- ggplot(data_muris, aes(y = realdeviantporn)) +
  geom_boxplot(fill = "blue") +
  theme_minimal() +
  labs(title = "Real Deviant Porn Use", y = NULL)

dp1_box | dp2_box


###############################################################################
# Assumption checking: skewness
###############################################################################

# Choose all variables of interest 
vars_to_check <- c("psy_score", "psy_sub1_prim", "psy_sub1_sec",
                   "mach_score", "narc_score",
                   "hh_finalscore", "hh_sub1_sinc", "hh_sub2_fair", 
                   "hh_sub3_gree", "hh_sub4_mod",
                   "devpor_score", "realdeviantporn")
# Calculate skewness 
skew_values <- psych::describe(data_muris[, vars_to_check])[, c("mean", "sd", "skew", "kurtosis")]
print(round(skew_values, 2))
# Attention should be paid to:
# !!! realdeviantporn (extremely negatively skewed)


# Visualize
# Show all DT histogramms at once
p1_hist <- ggplot(data_muris, aes(x = psy_score)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "grey", color = "black") +
  geom_density(color = "darkred", size = 1.2) +
  theme_minimal() +
  labs(title = "Psychopathy Total", x = NULL, y = NULL)

p2_hist <- ggplot(data_muris, aes(x = psy_sub1_prim)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "grey", color = "black") +
  geom_density(color = "red", size = 1.2) +
  theme_minimal() +
  labs(title = "Primary Psychopathy", x = NULL, y = NULL)

p3_hist <- ggplot(data_muris, aes(x = psy_sub1_sec)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "grey", color = "black") +
  geom_density(color = "red", size = 1.2) +
  theme_minimal() +
  labs(title = "Secondary Psychopathy", x = NULL, y = NULL)

p4_hist <- ggplot(data_muris, aes(x = mach_score)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "grey", color = "black") +
  geom_density(color = "darkred", size = 1.2) +
  theme_minimal() +
  labs(title = "Machiavellianism", x = NULL, y = NULL)

p5_hist <- ggplot(data_muris, aes(x = narc_score)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "grey", color = "black") +
  geom_density(color = "darkred", size = 1.2) +
  theme_minimal() +
  labs(title = "Narcissism", x = NULL, y = NULL)

(p1_hist | p2_hist | p3_hist) / (p4_hist | p5_hist)


# Show all HH histogramms at once
hh1_hist <- ggplot(data_muris, aes(x = hh_finalscore)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "grey", color = "black") +
  geom_density(color = "darkgreen", size = 1.2) +
  theme_minimal() +
  labs(title = "Honesty-Humility", x = NULL, y = NULL)

hh2_hist <- ggplot(data_muris, aes(x = hh_sub1_sinc)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "grey", color = "black") +
  geom_density(color = "green", size = 1.2) +
  theme_minimal() +
  labs(title = "Sincerity", x = NULL, y = NULL)

hh3_hist <- ggplot(data_muris, aes(x = hh_sub2_fair)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "grey", color = "black") +
  geom_density(color = "green", size = 1.2) +
  theme_minimal() +
  labs(title = "Fairness", x = NULL, y = NULL)

hh4_hist <- ggplot(data_muris, aes(x = hh_sub3_gree)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "grey", color = "black") +
  geom_density(color = "green", size = 1.2) +
  theme_minimal() +
  labs(title = "Greed Avoidance", x = NULL, y = NULL)

hh5_hist <- ggplot(data_muris, aes(x = hh_sub4_mod)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "grey", color = "black") +
  geom_density(color = "green", size = 1.2) +
  theme_minimal() +
  labs(title = "Modesty", x = NULL, y = NULL)

(hh1_hist | hh2_hist | hh3_hist) / (hh4_hist | hh5_hist)

# Show Deviant Porn use histogramms at once
dp1_hist <- ggplot(data_muris, aes(x = devpor_score)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "grey", color = "black") +
  geom_density(color = "darkblue", size = 1.2) +
  theme_minimal() +
  labs(title = "Deviant Porn Use", x = NULL, y = NULL)

dp2_hist <- ggplot(data_muris, aes(x = realdeviantporn)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "grey", color = "black") +
  geom_density(color = "blue", size = 1.2) +
  theme_minimal() +
  labs(title = "Real Deviant Porn Use", x = NULL, y = NULL)

dp1_hist | dp2_hist


###############################################################################
# Assumption checking: multicollinearity
###############################################################################

# 1. Extract the variables which shall be the predictors in the model.
predictors_set1 <- c("narc_score", "mach_score", "psy_score")
# 2. Simulate a continuous dummy outcome variables
dummyOutcome <- rnorm(n=nrow(data_muris))
# 3. Build dummy Outcome
data_temp1 <- data.frame(dummyOutcome, data_muris[, predictors_set1])
# 4. Model + VIF
model1 <- lm(dummyOutcome ~ ., data = data_temp1)
vif(model1)
# Some have recommended values of 5 or higher as indicative of multicollinearity
# others recommend to use values of 10 instead of 5.


# 1. Extract the variables which shall be the predictors in the model.
predictors_set2 <- c("narc_score", "mach_score", "psy_sub1_prim", "psy_sub1_sec")
# 2. Simulate a continuous dummy outcome variables
dummyOutcome <- rnorm(n=nrow(data_muris))
# 3. Build dummy Outcome
data_temp2 <- data.frame(dummyOutcome, data_muris[, predictors_set2])
# 4. Model + VIF
model2 <- lm(dummyOutcome ~ ., data = data_temp2)
vif(model2)
# Some have recommended values of 5 or higher as indicative of multicollinearity
# others recommend to use values of 10 instead of 5.


# 1. Extract the variables which shall be the predictors in the model.
predictors_set3 <- c("hh_sub1_sinc", "hh_sub4_mod", "hh_sub2_fair", "hh_sub3_gree")
# 2. Simulate a continuous dummy outcome variables
dummyOutcome <- rnorm(n=nrow(data_muris))
# 3. Build dummy Outcome
data_temp3 <- data.frame(dummyOutcome, data_muris[, predictors_set3])
# 4. Model + VIF
model3 <- lm(dummyOutcome ~ ., data = data_temp3)
vif(model3)
# Some have recommended values of 5 or higher as indicative of multicollinearity
# others recommend to use values of 10 instead of 5.


###############################################################################
# Assumption checking: linearity
###############################################################################
# plot each predictor against  outcome (devpor_score & realdeviantporn) and check whether the trend is linear
# Show all DT against dev_porn

p1_lin <- ggplot(data_muris, aes(x = psy_score, y = devpor_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "darkred", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Psychopathy",
    x = "Psychopathy score",
    y = "Deviant pornography use"
  )

p2_lin <- ggplot(data_muris, aes(x = psy_sub1_prim, y = devpor_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Primary Psychopathy",
    x = "Primary psychopathy score",
    y = "Deviant pornography use"
  )

p3_lin <- ggplot(data_muris, aes(x = psy_sub1_sec, y = devpor_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Secondary Psychopathy",
    x = "Secondary psychopathy score",
    y = "Deviant pornography use"
  )

p4_lin <- ggplot(data_muris, aes(x = mach_score, y = devpor_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "darkred", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Machiavellianism",
    x = "Machiavellianism score",
    y = "Deviant pornography use"
  )

p5_lin <- ggplot(data_muris, aes(x = narc_score, y = devpor_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "darkred", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Narcissism",
    x = "Narcissism score",
    y = "Deviant pornography use"
  )

# Combine plots in 2x3 layout
(p1_lin | p2_lin | p3_lin) / (p4_lin | p5_lin)



# Show all HH against dev_porn

hh1_lin <- ggplot(data_muris, aes(x = hh_finalscore, y = devpor_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "darkgreen", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Honesty-Humility",
    x = "Honesty-Humility score",
    y = "Deviant pornography use"
  )

hh2_lin <- ggplot(data_muris, aes(x = hh_sub1_sinc, y = devpor_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "green", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Sincerity",
    x = "Sincerity score",
    y = "Deviant pornography use"
  )

hh3_lin <- ggplot(data_muris, aes(x = hh_sub2_fair, y = devpor_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "green", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Fairness",
    x = "Fairness score",
    y = "Deviant pornography use"
  )

hh4_lin <- ggplot(data_muris, aes(x = hh_sub3_gree, y = devpor_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "green", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Greed Avoidance",
    x = "Greed avoidance score",
    y = "Deviant pornography use"
  )

hh5_lin <- ggplot(data_muris, aes(x = hh_sub4_mod, y = devpor_score)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "green", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Modesty",
    x = "Modesty score",
    y = "Deviant pornography use"
  )

(hh1_lin | hh2_lin | hh3_lin) / (hh4_lin | hh5_lin)


# Show all DT against realdeviantporn

p1_lin_real <- ggplot(data_muris, aes(x = psy_score, y = realdeviantporn)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "darkred", se = FALSE) +
  theme_minimal() +
  labs(title = "Psychopathy",
       x = "Psychopathy score",
       y = "Real deviant pornography use")

p2_lin_real <- ggplot(data_muris, aes(x = psy_sub1_prim, y = realdeviantporn)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  theme_minimal() +
  labs(title = "Primary Psychopathy",
       x = "Primary psychopathy score",
       y = "Real deviant pornography use")

p3_lin_real <- ggplot(data_muris, aes(x = psy_sub1_sec, y = realdeviantporn)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  theme_minimal() +
  labs(title = "Secondary Psychopathy",
       x = "Secondary psychopathy score",
       y = "Real deviant pornography use")

p4_lin_real <- ggplot(data_muris, aes(x = mach_score, y = realdeviantporn)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "darkred", se = FALSE) +
  theme_minimal() +
  labs(title = "Machiavellianism",
       x = "Machiavellianism score",
       y = "Real deviant pornography use")

p5_lin_real <- ggplot(data_muris, aes(x = narc_score, y = realdeviantporn)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "darkred", se = FALSE) +
  theme_minimal() +
  labs(title = "Narcissism",
       x = "Narcissism score",
       y = "Real deviant pornography use")

(p1_lin_real | p2_lin_real | p3_lin_real) / (p4_lin_real | p5_lin_real)


# Show all HH against dev_porn

hh1_lin_real <- ggplot(data_muris, aes(x = hh_finalscore, y = realdeviantporn)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "darkgreen", se = FALSE) +
  theme_minimal() +
  labs(title = "Honesty-Humility",
       x = "Honesty-Humility score",
       y = "Real deviant pornography use")

hh2_lin_real <- ggplot(data_muris, aes(x = hh_sub1_sinc, y = realdeviantporn)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "green", se = FALSE) +
  theme_minimal() +
  labs(title = "Sincerity",
       x = "Sincerity score",
       y = "Real deviant pornography use")

hh3_lin_real <- ggplot(data_muris, aes(x = hh_sub2_fair, y = realdeviantporn)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "green", se = FALSE) +
  theme_minimal() +
  labs(title = "Fairness",
       x = "Fairness score",
       y = "Real deviant pornography use")

hh4_lin_real <- ggplot(data_muris, aes(x = hh_sub3_gree, y = realdeviantporn)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "green", se = FALSE) +
  theme_minimal() +
  labs(title = "Greed Avoidance",
       x = "Greed Avoidance score",
       y = "Real deviant pornography use")

hh5_lin_real <- ggplot(data_muris, aes(x = hh_sub4_mod, y = realdeviantporn)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "green", se = FALSE) +
  theme_minimal() +
  labs(title = "Modesty",
       x = "Modesty score",
       y = "Real deviant pornography use")

# Combine plots
(hh1_lin_real | hh2_lin_real | hh3_lin_real) / (hh4_lin_real | hh5_lin_real)



###############################################################################
# Calculating means and standard deviations
###############################################################################

### Mean and SD FOR WHOLE SAMPLE
# NPI Narcissism
mean(data_muris$narc_score, na.rm = TRUE)
sd(data_muris$narc_score, na.rm = TRUE)

# SRPS Psychopathy
mean(data_muris$psy_score, na.rm = TRUE)
sd(data_muris$psy_score, na.rm = TRUE)
# SRPS Psychopathy: Primary Psychopathy
mean(data_muris$psy_sub1_prim, na.rm = TRUE)
sd(data_muris$psy_sub1_prim, na.rm = TRUE)
# SRPS Psychopathy: Secondary Psychopathy
mean(data_muris$psy_sub1_sec, na.rm = TRUE)
sd(data_muris$psy_sub1_sec, na.rm = TRUE)

# MACH-IV Machiavellianism
mean(data_muris$mach_score, na.rm = TRUE)
sd(data_muris$mach_score, na.rm = TRUE)

# HEXACO PI-R Honesty/Humility
mean(data_muris$hh_score, na.rm = TRUE)
sd(data_muris$hh_score, na.rm = TRUE)
# HEXACO PI-R Honesty/Humility: Sincerity
mean(data_muris$hh_sub1_sinc, na.rm = TRUE)
sd(data_muris$hh_sub1_sinc, na.rm = TRUE)
# HEXACO PI-R Honesty/Humility: Fairness
mean(data_muris$hh_sub2_fair, na.rm = TRUE)
sd(data_muris$hh_sub2_fair, na.rm = TRUE)
# HEXACO PI-R Honesty/Humility: Greed Avoidance
mean(data_muris$hh_sub3_gree, na.rm = TRUE)
sd(data_muris$hh_sub3_gree, na.rm = TRUE)
# HEXACO PI-R Honesty/Humility: Modesty
mean(data_muris$hh_sub4_mod, na.rm = TRUE)
sd(data_muris$hh_sub4_mod, na.rm = TRUE)

# Deviant pornography use
mean(data_muris$devpor_score, na.rm = TRUE)
sd(data_muris$devpor_score, na.rm = TRUE)
# Sex with young people	
mean(data_muris$devpor_1, na.rm = TRUE) * 100
sd(data_muris$devpor_1, na.rm = TRUE)
# Sadomasochistic sex
mean(data_muris$devpor_2, na.rm = TRUE) * 100
sd(data_muris$devpor_2, na.rm = TRUE)
# Bondage and domination sex
mean(data_muris$devpor_3, na.rm = TRUE) * 100
sd(data_muris$devpor_3, na.rm = TRUE)
# Sex with animals
mean(data_muris$devpor_4, na.rm = TRUE) * 100
sd(data_muris$devpor_4, na.rm = TRUE)
# Violent sex
mean(data_muris$devpor_5, na.rm = TRUE) * 100
sd(data_muris$devpor_5, na.rm = TRUE)
# Rape
mean(data_muris$devpor_6, na.rm = TRUE) * 100
sd(data_muris$devpor_6, na.rm = TRUE)
# Rough Sex
mean(data_muris$devpor_7, na.rm = TRUE) * 100
sd(data_muris$devpor_7, na.rm = TRUE)
# Real deviant pornography use
mean(data_muris$realdeviantporn, na.rm = TRUE)
sd(data_muris$realdeviantporn, na.rm = TRUE)


### Mean and SD FOR MEN ONLY (gender == 1)

# NPI Narcissism
mean(data_muris$narc_score[data_muris$gender == 1], na.rm = TRUE)
sd(data_muris$narc_score[data_muris$gender == 1], na.rm = TRUE)

# SRPS Psychopathy
mean(data_muris$psy_score[data_muris$gender == 1], na.rm = TRUE)
sd(data_muris$psy_score[data_muris$gender == 1], na.rm = TRUE)
# SRPS Psychopathy: Primary Psychopathy
mean(data_muris$psy_sub1_prim[data_muris$gender == 1], na.rm = TRUE)
sd(data_muris$psy_sub1_prim[data_muris$gender == 1], na.rm = TRUE)
# SRPS Psychopathy: Secondary Psychopathy
mean(data_muris$psy_sub1_sec[data_muris$gender == 1], na.rm = TRUE)
sd(data_muris$psy_sub1_sec[data_muris$gender == 1], na.rm = TRUE)

# MACH-IV Machiavellianism
mean(data_muris$mach_score[data_muris$gender == 1], na.rm = TRUE)
sd(data_muris$mach_score[data_muris$gender == 1], na.rm = TRUE)

# HEXACO PI-R Honesty/Humility
mean(data_muris$hh_score[data_muris$gender == 1], na.rm = TRUE)
sd(data_muris$hh_score[data_muris$gender == 1], na.rm = TRUE)
# HEXACO PI-R Honesty/Humility: Sincerity
mean(data_muris$hh_sub1_sinc[data_muris$gender == 1], na.rm = TRUE)
sd(data_muris$hh_sub1_sinc[data_muris$gender == 1], na.rm = TRUE)
# HEXACO PI-R Honesty/Humility: Fairness
mean(data_muris$hh_sub2_fair[data_muris$gender == 1], na.rm = TRUE)
sd(data_muris$hh_sub2_fair[data_muris$gender == 1], na.rm = TRUE)
# HEXACO PI-R Honesty/Humility: Greed Avoidance
mean(data_muris$hh_sub3_gree[data_muris$gender == 1], na.rm = TRUE)
sd(data_muris$hh_sub3_gree[data_muris$gender == 1], na.rm = TRUE)
# HEXACO PI-R Honesty/Humility: Modesty
mean(data_muris$hh_sub4_mod[data_muris$gender == 1], na.rm = TRUE)
sd(data_muris$hh_sub4_mod[data_muris$gender == 1], na.rm = TRUE)

# Deviant pornography use
mean(data_muris$devpor_score[data_muris$gender == 1], na.rm = TRUE)
sd(data_muris$devpor_score[data_muris$gender == 1], na.rm = TRUE)
# Sex with young people
mean(data_muris$devpor_1[data_muris$gender == 1], na.rm = TRUE) * 100
sd(data_muris$devpor_1[data_muris$gender == 1], na.rm = TRUE)
# Sadomasochistic sex
mean(data_muris$devpor_2[data_muris$gender == 1], na.rm = TRUE) * 100
sd(data_muris$devpor_2[data_muris$gender == 1], na.rm = TRUE)
# Bondage and domination sex
mean(data_muris$devpor_3[data_muris$gender == 1], na.rm = TRUE) * 100
sd(data_muris$devpor_3[data_muris$gender == 1], na.rm = TRUE)
# Sex with animals
mean(data_muris$devpor_4[data_muris$gender == 1], na.rm = TRUE) * 100
sd(data_muris$devpor_4[data_muris$gender == 1], na.rm = TRUE)
# Violent sex
mean(data_muris$devpor_5[data_muris$gender == 1], na.rm = TRUE) * 100
sd(data_muris$devpor_5[data_muris$gender == 1], na.rm = TRUE)
# Rape
mean(data_muris$devpor_6[data_muris$gender == 1], na.rm = TRUE) * 100
sd(data_muris$devpor_6[data_muris$gender == 1], na.rm = TRUE)
# Rough Sex
mean(data_muris$devpor_7[data_muris$gender == 1], na.rm = TRUE) * 100
sd(data_muris$devpor_7[data_muris$gender == 1], na.rm = TRUE)
# Real deviant pornography use
mean(data_muris$realdeviantporn[data_muris$gender == 1], na.rm = TRUE)
sd(data_muris$realdeviantporn[data_muris$gender == 1], na.rm = TRUE)


### Mean and SD FOR WOMEN ONLY (gender == 0)

# NPI Narcissism
mean(data_muris$narc_score[data_muris$gender == 0], na.rm = TRUE)
sd(data_muris$narc_score[data_muris$gender == 0], na.rm = TRUE)

# SRPS Psychopathy
mean(data_muris$psy_score[data_muris$gender == 0], na.rm = TRUE)
sd(data_muris$psy_score[data_muris$gender == 0], na.rm = TRUE)
# SRPS Psychopathy: Primary Psychopathy
mean(data_muris$psy_sub1_prim[data_muris$gender == 0], na.rm = TRUE)
sd(data_muris$psy_sub1_prim[data_muris$gender == 0], na.rm = TRUE)
# SRPS Psychopathy: Secondary Psychopathy
mean(data_muris$psy_sub1_sec[data_muris$gender == 0], na.rm = TRUE)
sd(data_muris$psy_sub1_sec[data_muris$gender == 0], na.rm = TRUE)

# MACH-IV Machiavellianism
mean(data_muris$mach_score[data_muris$gender == 0], na.rm = TRUE)
sd(data_muris$mach_score[data_muris$gender == 0], na.rm = TRUE)

# HEXACO PI-R Honesty/Humility
mean(data_muris$hh_score[data_muris$gender == 0], na.rm = TRUE)
sd(data_muris$hh_score[data_muris$gender == 0], na.rm = TRUE)
# HEXACO PI-R Honesty/Humility: Sincerity
mean(data_muris$hh_sub1_sinc[data_muris$gender == 0], na.rm = TRUE)
sd(data_muris$hh_sub1_sinc[data_muris$gender == 0], na.rm = TRUE)
# HEXACO PI-R Honesty/Humility: Fairness
mean(data_muris$hh_sub2_fair[data_muris$gender == 0], na.rm = TRUE)
sd(data_muris$hh_sub2_fair[data_muris$gender == 0], na.rm = TRUE)
# HEXACO PI-R Honesty/Humility: Greed Avoidance
mean(data_muris$hh_sub3_gree[data_muris$gender == 0], na.rm = TRUE)
sd(data_muris$hh_sub3_gree[data_muris$gender == 0], na.rm = TRUE)
# HEXACO PI-R Honesty/Humility: Modesty
mean(data_muris$hh_sub4_mod[data_muris$gender == 0], na.rm = TRUE)
sd(data_muris$hh_sub4_mod[data_muris$gender == 0], na.rm = TRUE)

# Deviant pornography use
mean(data_muris$devpor_score[data_muris$gender == 0], na.rm = TRUE)
sd(data_muris$devpor_score[data_muris$gender == 0], na.rm = TRUE)
# Sex with young people
mean(data_muris$devpor_1[data_muris$gender == 0], na.rm = TRUE) * 100
sd(data_muris$devpor_1[data_muris$gender == 0], na.rm = TRUE)
# Sadomasochistic sex
mean(data_muris$devpor_2[data_muris$gender == 0], na.rm = TRUE) * 100
sd(data_muris$devpor_2[data_muris$gender == 0], na.rm = TRUE)
# Bondage and domination sex
mean(data_muris$devpor_3[data_muris$gender == 0], na.rm = TRUE) * 100
sd(data_muris$devpor_3[data_muris$gender == 0], na.rm = TRUE)
# Sex with animals
mean(data_muris$devpor_4[data_muris$gender == 0], na.rm = TRUE) * 100
sd(data_muris$devpor_4[data_muris$gender == 0], na.rm = TRUE)
# Violent sex
mean(data_muris$devpor_5[data_muris$gender == 0], na.rm = TRUE) * 100
sd(data_muris$devpor_5[data_muris$gender == 0], na.rm = TRUE)
# Rape
mean(data_muris$devpor_6[data_muris$gender == 0], na.rm = TRUE) * 100
sd(data_muris$devpor_6[data_muris$gender == 0], na.rm = TRUE)
# Rough Sex
mean(data_muris$devpor_7[data_muris$gender == 0], na.rm = TRUE) * 100
sd(data_muris$devpor_7[data_muris$gender == 0], na.rm = TRUE)
# Real deviant pornography use
mean(data_muris$realdeviantporn[data_muris$gender == 0], na.rm = TRUE)
sd(data_muris$realdeviantporn[data_muris$gender == 0], na.rm = TRUE)



###############################################################################
# Calculating correlations 
###############################################################################

# Define Variables
vars_cor <- c("narc_score", "mach_score", "psy_score", 
              "psy_sub1_prim", "psy_sub1_sec", 
              "hh_score", 
              "hh_sub1_sinc", "hh_sub2_fair", "hh_sub3_gree", "hh_sub4_mod",  
              "devpor_score", "realdeviantporn")

# Calculate Correlations for WHOLE SAMPLE
cor(data_muris[vars_cor], use = "pairwise.complete.obs")

# Calculate Correlations FOR MEN
cor(data_muris[data_muris$gender == 1, vars_cor], use = "pairwise.complete.obs")

# Calculate Correlations FOR WOMEN
cor(data_muris[data_muris$gender == 0, vars_cor], use = "pairwise.complete.obs")


# Calculate Spearman Correlations for WHOLE SAMPLE
cor(data_muris[vars_cor], use = "pairwise.complete.obs", method = "spearman")

# Calculate Spearman Correlations FOR MEN
cor(data_muris[data_muris$gender == 1, vars_cor], use = "pairwise.complete.obs", method = "spearman")

# Calculate Spearman Correlations FOR WOMEN
cor(data_muris[data_muris$gender == 0, vars_cor], use = "pairwise.complete.obs", method = "spearman")


###############################################################################
# Reproduction of Muris' regression model with male subpopulation
###############################################################################

#### Deviant porn use ###
# simple linear regression with dv = devpor_score and predictor = hh_finalscore
model_regression_hh <- lm(devpor_score ~ hh_finalscore, data = data_muris[data_muris$gender == 1, ])

summary(model_regression_hh)

# My findings   : F(1, 41) = 17.53
# Muris findings: F(1, 44) = 17.53
# My findings   : p = 0.000134
# Muris findings: p < .001	
# My findings.  : Multpile R-squared =  0.2849 = 28.5%
# Muris fidnings: R-Squared = 29%
# Intercept: 7.4276 = Wert von devpor_score, wenn hh_finalscore = 0
# Unstandardisierte Regressionskoeffizient (b-Koeffizient) = –1.3198 = Änderung von devpor_score pro 1 Einheit Zunahme in hh_finalscore


# simple linear regression with dv = devpor_score and predictors = narc_score, mach_score, psy_score

model_regression_dt <- lm(devpor_score ~ mach_score + narc_score + psy_score, data = data_muris[data_muris$gender == 1, ])
summary(model_regression_dt)


#### REAL deviant porn use ###
# simple linear regression with dv = realdeviantporn and predictor = hh_finalscore
model_regression_rd_hh <- lm(realdeviantporn ~ hh_finalscore, data = data_muris[data_muris$gender == 1, ])
summary(model_regression_rd_hh)

# My findings   : F(1,44) = 10.87
# Muris findings: F(1,44) = 10.87
# My findings   : p =  0.001942
# Muris findings: p < .01	
# My findings.  : Multpile R-squared =  0.198 = 19,8%
# Muris fidnings: R-Squared = 20%
# Intercept: 2.7596 = Wert von realdeviantporn, wenn hh_finalscore = 0
# Unstandardisierte Regressionskoeffizient (b-Koeffizient) = -0.6378 = Änderung von realdeviantporn pro 1 Einheit Zunahme in hh_finalscore

# simple linear regression with dv = realdeviantporn and predictors = narc_score, mach_score, psy_score
model_regression_rd_dt <- lm(realdeviantporn ~ mach_score + narc_score + psy_score, data = data_muris[data_muris$gender == 1, ])
summary(model_regression_rd_dt)
# My findings   : F(3,42) = 2.594
# My findings.  : Multpile R-squared =  0.1563 = 15.6%
# Intercept: -0.767255
# Unstandardisierte Regressionskoeffizienten (b-Koeffizient): mach_score = -0.009723 | narc_score = 0.010379 | psy_score = 0.032892


###############################################################################
# MAIN ANAYLSES: Lasso models
###############################################################################
install.packages(c("glmnet", "glmnetUtils"))

# URL with detailed descriptions, examples, and exemplary R code:
# https://glmnet.stanford.edu/articles/glmnet.html

library(glmnet)
library(glmnetUtils)

# LASSO
# -----
# LASSO regression is one option among penalized regression.
# L = Least
# A = Absolute
# S = Shrinkage and
# S = Selection
# O = Operator.
# ----------------

maleIdx <- data_muris$gender == 1
femaleIdx <- data_muris$gender == 0
############
# H1a MEN
############
#  Predictors that are supposed to be entered into the model (all at once). LASSO will make them compete against one another. Finally, it will be returned who "survived" that competition (there can be more than one "winner").
vars_H1a <- c("narc_score", "mach_score", "psy_score")
# Step 1: Set up the model matrix (without the intercept).
X_H1a <- model.matrix(~ ., data=data_muris[maleIdx, vars_H1a])[,-1]
# Step 2: Which outcome shall be used?
y <- data_muris[maleIdx, "devpor_score"]

# Step 3: Run competition, use cross-validation (default: 10-fold CV)
set.seed(1) # Use set.seed to enable perfect reproducibility
(mod_H1a <- cv.glmnet(x=X_H1a,
                      y=dplyr::pull(data_muris[maleIdx,"devpor_score"]),
                      family="gaussian",
                      # alpha = 1 means to select LASSO from the glmnet family.
                      intercept = FALSE, alpha=1))

# Results:
# Short summary:
coef(mod_H1a, c(mod_H1a$lambda.min, mod_H1a$lambda.1se))
# Long summary:
mod_H1a$glmnet.fit

# Save the summary of the dgCMatrix in variable name smrydgC
smrydgC_H1a <- summary(coef(mod_H1a, c(mod_H1a$lambda.min, mod_H1a$lambda.1se)))
smrydgC_H1a[4,]
matrix_H1a <- as.matrix(smrydgC)
# !! copypaste into excel, 

# # Save the dgCMatrix in variable name obj
# obj <- coef(mod_H1a, c(mod_H1a$lambda.min, mod_H1a$lambda.1se))
# # Save the variable names in the model, which remained for the minimal squared error.
# select.min.Preds <- obj@Dimnames[[1]][smrydgC$i[smrydgC$j==1]]
# # Print these variable names in the R console.
# cat(paste0('"', select.min.Preds, '"', collapse = ", "),"\n")
# # # Save the variable names in the model, which remained within 1 standard error of the minimal squared error.
# # select.1se.Preds <- obj@Dimnames[[1]][smrydgC$i[smrydgC$j==2]]
# # # Print these variable names in the R console.
# # cat(paste0('"', select.1se.Preds, '"', collapse = ", "),"\n")

# Visualize the LASSO result:
plot(mod_H1a)

############
# H1a WOMEN
############

# Subset index for females (already defined earlier)
# femaleIdx <- data_muris$gender == 0

# Define predictors
vars_H1a_f <- c("narc_score", "mach_score", "psy_score")

# Step 1: Create model matrix without intercept
X_H1a_f <- model.matrix(~ ., data = data_muris[femaleIdx, vars_H1a_f])[,-1]

# Step 2: Define outcome variable
y_female <- data_muris[femaleIdx, "devpor_score"]


# Step 3: Run competition, use cross-validation (default: 10-fold CV)
set.seed(1) # Use set.seed to enable perfect reproducibility
(mod_H1a_f <- cv.glmnet(x=X_H1a_f,
                      y=dplyr::pull(data_muris[femaleIdx,"devpor_score"]),
                      family="gaussian",
                      # alpha = 1 means to select LASSO from the glmnet family.
                      intercept = FALSE, alpha=1))


# Results:
# Short summary:
coef(mod_H1a_f, c(mod_H1a$lambda.min, mod_H1a_f$lambda.1se))
# Long summary:
mod_H1a_f$glmnet.fit

# Save the summary of the dgCMatrix in variable name smrydgC
smrydgC_H1a_f <- summary(coef(mod_H1a_f, c(mod_H1a_f$lambda.min, mod_H1a_f$lambda.1se)))
smrydgC_H1a_f[4,]
matrix_H1a_f <- as.matrix(smrydgC)

# Visualize the LASSO result:
plot(mod_H1a_f)


############
# H1b
############
#  Predictors that are supposed to be entered into the model (all at once). LASSO will make them compete against one another. Finally, it will be returned who "survived" that competition (there can be more than one "winner").
vars_H1b <- c("narc_score", "mach_score", "psy_sub1_prim", "psy_sub1_sec")
¨# Step 1: Set up the model matrix (without the intercept).
X_H1b <- model.matrix(~ ., data=data_muris[maleIdx, vars_H1b])[,-1]
# Step 2: Which outcome shall be used?
y <- data_muris[maleIdx, "devpor_score"]

# Step 3: Run competition, use cross-validation (default: 10-fold CV)
set.seed(1) # Use set.seed to enable perfect reproducibility
(mod_H1b <- cv.glmnet(x=X_H1b,
                      y=dplyr::pull(data_muris[maleIdx,"devpor_score"]),
                      family="gaussian",
                      # alpha = 1 means to select LASSO from the glmnet family.
                      intercept = FALSE, alpha=1))

# Results:
# Short summary:
coef(mod_H1b, c(mod_H1b$lambda.min, mod_H1b$lambda.1se))
# Long summary:
mod_H1b$glmnet.fit

# Save the summary of the dgCMatrix in variable name smrydgC
smrydgC_H1b <- summary(coef(mod_H1b, c(mod_H1b$lambda.min, mod_H1b$lambda.1se)))
smrydgC_H1b[4,]
matrix_H1b <- as.matrix(smrydgC)


# Visualize the LASSO result:
plot(mod_H1b)


############
# H1b WOMEN
############
#  Predictors that are supposed to be entered into the model (all at once). LASSO will make them compete against one another. Finally, it will be returned who "survived" that competition (there can be more than one "winner").
vars_H1b_f <- c("narc_score", "mach_score", "psy_sub1_prim", "psy_sub1_sec")
¨# Step 1: Set up the model matrix (without the intercept).
X_H1b_f <- model.matrix(~ ., data=data_muris[femaleIdx, vars_H1b_f])[,-1]
# Step 2: Which outcome shall be used?
y_female <- data_muris[femaleIdx, "devpor_score"]

# Step 3: Run competition, use cross-validation (default: 10-fold CV)
set.seed(1) # Use set.seed to enable perfect reproducibility
(mod_H1b_f <- cv.glmnet(x=X_H1b_f,
                      y=dplyr::pull(data_muris[femaleIdx,"devpor_score"]),
                      family="gaussian",
                      # alpha = 1 means to select LASSO from the glmnet family.
                      intercept = FALSE, alpha=1))

# Results:
# Short summary:
coef(mod_H1b_f, c(mod_H1b_f$lambda.min, mod_H1b_f$lambda.1se))
# Long summary:
mod_H1b_f$glmnet.fit

# Save the summary of the dgCMatrix in variable name smrydgC
smrydgC_H1b_f <- summary(coef(mod_H1b_f, c(mod_H1b_f$lambda.min, mod_H1b_f$lambda.1se)))
smrydgC_H1b_f[4,]
matrix_H1b_f <- as.matrix(smrydgC)


# Visualize the LASSO result:
plot(mod_H1b_f)


############
# H2
############
#  Predictors that are supposed to be entered into the model (all at once). LASSO will make them compete against one another. Finally, it will be returned who "survived" that competition (there can be more than one "winner").
vars_H2 <- c("hh_sub1_sinc", "hh_sub2_fair", "hh_sub3_gree", "hh_sub4_mod")
# Step 1: Set up the model matrix (without the intercept).
X_H2 <- model.matrix(~ ., data=data_muris[maleIdx, vars_H2])[,-1]
# Step 2: Which outcome shall be used?
y <- data_muris[maleIdx, "devpor_score"]

# Step 3: Run competition, use cross-validation (default: 10-fold CV)
set.seed(1) # Use set.seed to enable perfect reproducibility
(mod_H2 <- cv.glmnet(x=X_H2,
                      y=dplyr::pull(data_muris[maleIdx,"devpor_score"]),
                      family="gaussian",
                      # alpha = 1 means to select LASSO from the glmnet family.
                      intercept = FALSE, alpha=1))

# Results:
# Short summary:
coef(mod_H2, c(mod_H2$lambda.min, mod_H2$lambda.1se))
# Long summary:
mod_H2$glmnet.fit

# Save the summary of the dgCMatrix in variable name smrydgC
smrydgC_H2 <- summary(coef(mod_H2, c(mod_H2$lambda.min, mod_H2$lambda.1se)))
smrydgC_H2[4,]
matrix_H2 <- as.matrix(smrydgC)

# Visualize the LASSO result:
plot(mod_H2)


############
# H2 WOMEN
############
#  Predictors that are supposed to be entered into the model (all at once). LASSO will make them compete against one another. Finally, it will be returned who "survived" that competition (there can be more than one "winner").
vars_H2_f <- c("hh_sub1_sinc", "hh_sub2_fair", "hh_sub3_gree", "hh_sub4_mod")
# Step 1: Set up the model matrix (without the intercept).
X_H2_f <- model.matrix(~ ., data=data_muris[femaleIdx, vars_H2_f])[,-1]
# Step 2: Which outcome shall be used?
y_female <- data_muris[femaleIdx, "devpor_score"]

# Step 3: Run competition, use cross-validation (default: 10-fold CV)
set.seed(1) # Use set.seed to enable perfect reproducibility
(mod_H2_f <- cv.glmnet(x=X_H2_f,
                     y=dplyr::pull(data_muris[femaleIdx,"devpor_score"]),
                     family="gaussian",
                     # alpha = 1 means to select LASSO from the glmnet family.
                     intercept = FALSE, alpha=1))

# Results:
# Short summary:
coef(mod_H2_f, c(mod_H2_f$lambda.min, mod_H2_f$lambda.1se))
# Long summary:
mod_H2_f$glmnet.fit

# Save the summary of the dgCMatrix in variable name smrydgC
smrydgC_H2_f <- summary(coef(mod_H2_f, c(mod_H2_f$lambda.min, mod_H2_f$lambda.1se)))
smrydgC_H2_f[4,]
matrix_H2_f <- as.matrix(smrydgC)

# Visualize the LASSO result:
plot(mod_H2_f)


############
# H3a
############
#  Predictors that are supposed to be entered into the model (all at once). LASSO will make them compete against one another. Finally, it will be returned who "survived" that competition (there can be more than one "winner").
vars_H3a <- c("psy_score", "mach_score", "narc_score", "hh_finalscore")
# Step 1: Set up the model matrix (without the intercept).
X_H3a <- model.matrix(~ ., data=data_muris[maleIdx, vars_H3a])[,-1]
# Step 2: Which outcome shall be used?
y <- data_muris[maleIdx, "devpor_score"]

# Step 3: Run competition, use cross-validation (default: 10-fold CV)
set.seed(1) # Use set.seed to enable perfect reproducibility
(mod_H3a <- cv.glmnet(x=X_H3a,
                     y=dplyr::pull(data_muris[maleIdx,"devpor_score"]),
                     family="gaussian",
                     # alpha = 1 means to select LASSO from the glmnet family.
                     intercept = FALSE, alpha=1))

# Results:
# Short summary:
coef(mod_H3a, c(mod_H3a$lambda.min, mod_H3a$lambda.1se))
# Long summary:
mod_H3a$glmnet.fit

# Save the summary of the dgCMatrix in variable name smrydgC
smrydgC_H3a <- summary(coef(mod_H3a, c(mod_H3a$lambda.min, mod_H3a$lambda.1se)))
smrydgC_H3a[4,]
matrix_H3a <- as.matrix(smrydgC)

# Visualize the LASSO result:
plot(mod_H3a)


############
# H3a WOMEN
############
#  Predictors that are supposed to be entered into the model (all at once). LASSO will make them compete against one another. Finally, it will be returned who "survived" that competition (there can be more than one "winner").
vars_H3a_f <- c("psy_score", "mach_score", "narc_score", "hh_finalscore")
# Step 1: Set up the model matrix (without the intercept).
X_H3a_f <- model.matrix(~ ., data=data_muris[femaleIdx, vars_H3a_f])[,-1]
# Step 2: Which outcome shall be used?
y_female <- data_muris[femaleIdx, "devpor_score"]

# Step 3: Run competition, use cross-validation (default: 10-fold CV)
set.seed(1) # Use set.seed to enable perfect reproducibility
(mod_H3a_f <- cv.glmnet(x=X_H3a_f,
                      y=dplyr::pull(data_muris[femaleIdx,"devpor_score"]),
                      family="gaussian",
                      # alpha = 1 means to select LASSO from the glmnet family.
                      intercept = FALSE, alpha=1))

# Results:
# Short summary:
coef(mod_H3a_f, c(mod_H3a_f$lambda.min, mod_H3a_f$lambda.1se))
# Long summary:
mod_H3a_f$glmnet.fit

# Save the summary of the dgCMatrix in variable name smrydgC
smrydgC_H3a_f <- summary(coef(mod_H3a_f, c(mod_H3a_f$lambda.min, mod_H3a_f$lambda.1se)))
smrydgC_H3a_f[4,]
matrix_H3a_f <- as.matrix(smrydgC)

# Visualize the LASSO result:
plot(mod_H3a_f)

############
# H3b
############
#  Predictors that are supposed to be entered into the model (all at once). LASSO will make them compete against one another. Finally, it will be returned who "survived" that competition (there can be more than one "winner").
vars_H3b <- c("psy_sub1_prim","psy_sub1_sec", "mach_score", "narc_score", "hh_finalscore")
# Step 1: Set up the model matrix (without the intercept).
X_H3b <- model.matrix(~ ., data=data_muris[maleIdx, vars_H3b])[,-1]
# Step 2: Which outcome shall be used?
y <- data_muris[maleIdx, "devpor_score"]

# Step 3: Run competition, use cross-validation (default: 10-fold CV)
set.seed(1) # Use set.seed to enable perfect reproducibility
(mod_H3b <- cv.glmnet(x=X_H3b,
                      y=dplyr::pull(data_muris[maleIdx,"devpor_score"]),
                      family="gaussian",
                      # alpha = 1 means to select LASSO from the glmnet family.
                      intercept = FALSE, alpha=1))

# Results:
# Short summary:
coef(mod_H3b, c(mod_H3b$lambda.min, mod_H3b$lambda.1se))
# Long summary:
mod_H3b$glmnet.fit

# Save the summary of the dgCMatrix in variable name smrydgC
smrydgC_H3b <- summary(coef(mod_H3b, c(mod_H3b$lambda.min, mod_H3b$lambda.1se)))
smrydgC_H3b[4,]
matrix_H3b <- as.matrix(smrydgC)

# Visualize the LASSO result:
plot(mod_H3b)



############
# H3b WOMEN
############
#  Predictors that are supposed to be entered into the model (all at once). LASSO will make them compete against one another. Finally, it will be returned who "survived" that competition (there can be more than one "winner").
vars_H3b_f <- c("psy_sub1_prim","psy_sub1_sec", "mach_score", "narc_score", "hh_finalscore")
# Step 1: Set up the model matrix (without the intercept).
X_H3b_f <- model.matrix(~ ., data=data_muris[femaleIdx, vars_H3b_f])[,-1]
# Step 2: Which outcome shall be used?
y_female <- data_muris[femaleIdx, "devpor_score"]

# Step 3: Run competition, use cross-validation (default: 10-fold CV)
set.seed(1) # Use set.seed to enable perfect reproducibility
(mod_H3b_f <- cv.glmnet(x=X_H3b_f,
                      y=dplyr::pull(data_muris[femaleIdx,"devpor_score"]),
                      family="gaussian",
                      # alpha = 1 means to select LASSO from the glmnet family.
                      intercept = FALSE, alpha=1))

# Results:
# Short summary:
coef(mod_H3b_f, c(mod_H3b_f$lambda.min, mod_H3b_f$lambda.1se))
# Long summary:
mod_H3b_f$glmnet.fit

# Save the summary of the dgCMatrix in variable name smrydgC
smrydgC_H3b_f <- summary(coef(mod_H3b_f, c(mod_H3b_f$lambda.min, mod_H3b_f$lambda.1se)))
smrydgC_H3b_f[4,]
matrix_H3b_f <- as.matrix(smrydgC)

# Visualize the LASSO result:
plot(mod_H3b_f)


############
# H3c
############
#  Predictors that are supposed to be entered into the model (all at once). LASSO will make them compete against one another. Finally, it will be returned who "survived" that competition (there can be more than one "winner").
vars_H3c <- c("psy_sub1_prim", "psy_sub1_sec", "mach_score", "narc_score", "hh_sub1_sinc", "hh_sub2_fair", "hh_sub3_gree", "hh_sub4_mod")
# Step 1: Set up the model matrix (without the intercept).
X_H3c <- model.matrix(~ ., data=data_muris[maleIdx, vars_H3c])[,-1]
# Step 2: Which outcome shall be used?
y <- data_muris[maleIdx, "devpor_score"]

# Step 3: Run competition, use cross-validation (default: 10-fold CV)
set.seed(1) # Use set.seed to enable perfect reproducibility
(mod_H3c <- cv.glmnet(x=X_H3c,
                      y=dplyr::pull(data_muris[maleIdx,"devpor_score"]),
                      family="gaussian",
                      # alpha = 1 means to select LASSO from the glmnet family.
                      intercept = FALSE, alpha=1))

# Results:
# Short summary:
coef(mod_H3c, c(mod_H3c$lambda.min, mod_H3c$lambda.1se))
# Long summary:
mod_H3c$glmnet.fit

# Save the summary of the dgCMatrix in variable name smrydgC
smrydgC_H3c <- summary(coef(mod_H3c, c(mod_H3c$lambda.min, mod_H3c$lambda.1se)))
smrydgC_H3c[4,]
matrix_H3c <- as.matrix(smrydgC)
# !! copypaste into excel, 

# Visualize the LASSO result:
plot(mod_H3c)


############
# H3c WOMEN
############
#  Predictors that are supposed to be entered into the model (all at once). LASSO will make them compete against one another. Finally, it will be returned who "survived" that competition (there can be more than one "winner").
vars_H3c_f <- c("psy_sub1_prim", "psy_sub1_sec", "mach_score", "narc_score", "hh_sub1_sinc", "hh_sub2_fair", "hh_sub3_gree", "hh_sub4_mod")
# Step 1: Set up the model matrix (without the intercept).
X_H3c_f <- model.matrix(~ ., data=data_muris[femaleIdx, vars_H3c_f])[,-1]
# Step 2: Which outcome shall be used?
y_female <- data_muris[femaleIdx, "devpor_score"]

# Step 3: Run competition, use cross-validation (default: 10-fold CV)
set.seed(1) # Use set.seed to enable perfect reproducibility
(mod_H3c_f <- cv.glmnet(x=X_H3c_f,
                      y=dplyr::pull(data_muris[femaleIdx,"devpor_score"]),
                      family="gaussian",
                      # alpha = 1 means to select LASSO from the glmnet family.
                      intercept = FALSE, alpha=1))

# Results:
# Short summary:
coef(mod_H3c_f, c(mod_H3c_f$lambda.min, mod_H3c_f$lambda.1se))
# Long summary:
mod_H3c_f$glmnet.fit

# Save the summary of the dgCMatrix in variable name smrydgC
smrydgC_H3c_f <- summary(coef(mod_H3c_f, c(mod_H3c_f$lambda.min, mod_H3c_f$lambda.1se)))
smrydgC_H3c_f[4,]
matrix_H3c_f <- as.matrix(smrydgC)
# !! copypaste into excel, 

# Visualize the LASSO result:
plot(mod_H3c_f)






