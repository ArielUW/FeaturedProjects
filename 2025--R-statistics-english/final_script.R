#### FINAL ASSIGNMENT by ARIEL DROZD ####

### Step 1: loading the libraries and data ###
install.packages("car") # for Levene's test
library(car)
library(readxl) # for loading data in XLSX format
library(purrr)
library(tidyverse)
install.packages("moments") # for kurtosis and skewness
library(moments)
library(ggpubr)
install.packages("rstudioapi") # for setting the correct directory
library(rstudioapi)

# Setting the working directory to the one with the relevant XLSX files (and no other ones)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
print(getwd())

# Creating a joint database from the two XLSX files
df <- list.files(pattern = "\\.xlsx$") %>% 
  map_dfr(read_excel)

# Creating IDs
df <- tibble::rowid_to_column(df, "ID")
# making the IDs string-like, so that they don't appear in summaries
df$ID <- as.character(df$ID)

### RESEARCH QUESTIONS FOR THE 2X2 ANOVA ##
#
# RQ1: Are there differences between deviant and standard pronunciations in terms of:
# a. stressed syllable's F0
# b. stressed syllable's intensity
# c. stressed syllable's duration?
#
# RQ2: Are there differences between APU and PU words in terms of:
# a. stressed syllable's F0
# b. stressed syllable's intensity
# c. stressed syllable's duration?
#
# RQ3: Are there interactions of the two conditions (APU/PU and deviant/standard)? 
# I.e. Does the placement of the stress (APU/PU) influence how the deviant/standard pronunciation
# affects
# a. F0,
# b. intensity or 
# c. duration 
# of the stressed syllable?
#
# 1st main effect -> standard/deviant pronunciation
# 2nd main effect -> placement (APU/PU) of the stressed
# 3rd point -> interaction
#
# dependent variable a -> stressed syllable's F0 (SS_F0)
# dependent variable b -> stressed syllable's intensity (SS_int)
# dependent variable c -> stressed syllable's duration (SS_dur)
# 
# NULL HYPOTHESIS: there's no difference between the groups = the words are suitable
# for the experiments.
# NON-NULL HYPOTHESES:
# H1a: F0 of the stressed syllable is different for deviant and standard pronunciations.
# H1b: Intensity of the stressed syllable is different for deviant and standard pronunciations.
# H1c: Duration of the stressed syllable is different for deviant and standard pronunciations.
# H2a: F0 of the stressed syllable is different for APU and PU placement.
# H2b: Intensity of the stressed syllable is different for APU and PU placement.
# H2c: Duration of the stressed syllable is different for APU and PU placement.
# H3a: The influence of standard/deviant pronunciation on the F0 of the stressed syllable 
# is different for APU and PU placement.
# H3b: The influence of standard/deviant pronunciation on the intensity of the stressed syllable 
# is different for APU and PU placement.
# H3c: The influence of standard/deviant pronunciation on the duration of the stressed syllable 
# is different for APU and PU placement.
#
# alpha = 0.05
# I assume the tests for dependent variables a, b, c to be "separate studies"
# -> the corrections are to be made only for multiple comparisons for *a single* dependent variable
#
# Assumptions for the ANOVA:
# 1. Categorical independent variables: passed ✅
# 2. Continuous dependent variables: passed ✅
# 3. Independence of observations: passed ✅
# 4. Normality: to be checked
# 5. Homogeneity of variance: to be checked
# 6. No significant outliers: to be checked

### Step 2: preparing the data for analysis ###

# after loading, those two columns were chars for some reason,
# so they need to be transformed into numeric:
df$SS_int <- as.numeric(df$SS_int) 
df$SS_dur <- as.numeric(df$SS_dur)

# auxiliary vectors for data transformation
deviant <- c("APUd", "Pud")
standard <- c("Pus", "APUs")
apu <- c("APUd", "APUs")
pu <- c("Pus", "Pud")

# adding new columns for 2x2 ANOVA
df <- df %>%
  mutate(stress = case_match(Word_type,
                             apu ~ "APU",
                             pu ~ "PU"))

df <- df %>%
  mutate(correctness = case_match(Word_type,
                             deviant ~ "dev",
                             standard ~ "std"))

### Step 3: exploring the data ###

## Descriptive statistics for the whole dataset ##
table(df$stress, df$correctness)
df %>% select(where(is.numeric)) %>% summary()
df %>% select(where(is.numeric)) %>% var()

## Descriptive statistics for each group for all the dependent variables ##
df %>% filter(Word_type=="APUs") %>% select(where(is.numeric)) %>% summary()
df %>% filter(Word_type=="APUs") %>% select(where(is.numeric)) %>% var()
df %>% filter(Word_type=="APUd") %>% select(where(is.numeric)) %>% summary()
df %>% filter(Word_type=="Pus") %>% select(where(is.numeric)) %>% summary()
df %>% filter(Word_type=="Pud") %>% select(where(is.numeric)) %>% summary()
df %>% filter(stress=="APU") %>% select(where(is.numeric)) %>% summary()
df %>% filter(stress=="PU") %>% select(where(is.numeric)) %>% summary()
df %>% filter(correctness=="std") %>% select(where(is.numeric)) %>% summary()
df %>% filter(correctness=="dev") %>% select(where(is.numeric)) %>% summary()

## Standard deviations for the whole dataset and by group
# SD for F0
df %>%  summarise(
  mean = mean(SS_F0, na.rm = TRUE),
  sd = sd(SS_F0, na.rm = TRUE)
)
df %>% group_by(stress) %>%
  summarise(
    mean = mean(SS_F0, na.rm = TRUE),
    sd = sd(SS_F0, na.rm = TRUE)
)
df %>% group_by(correctness) %>%
  summarise(
    mean = mean(SS_F0, na.rm = TRUE),
    sd = sd(SS_F0, na.rm = TRUE)
)
df %>% group_by(stress, correctness) %>%
  summarise(
    mean = mean(SS_F0, na.rm = TRUE),
    sd = sd(SS_F0, na.rm = TRUE)
)

# SD for intensity
df %>% summarise(
    mean = mean(SS_int, na.rm = TRUE),
    sd = sd(SS_int, na.rm = TRUE)
)
df %>% group_by(stress) %>%
  summarise(
    mean = mean(SS_int, na.rm = TRUE),
    sd = sd(SS_int, na.rm = TRUE)
)
df %>% group_by(correctness) %>%
  summarise(
    mean = mean(SS_int, na.rm = TRUE),
    sd = sd(SS_int, na.rm = TRUE)
)
df %>% group_by(stress, correctness) %>%
  summarise(
    mean = mean(SS_int, na.rm = TRUE),
    sd = sd(SS_int, na.rm = TRUE)
)

# SD for duration
df %>% summarise(
    mean = mean(SS_dur, na.rm = TRUE),
    sd = sd(SS_dur, na.rm = TRUE)
)
df %>% group_by(stress) %>%
  summarise(
    mean = mean(SS_dur, na.rm = TRUE),
    sd = sd(SS_dur, na.rm = TRUE)
)
df %>% group_by(correctness) %>%
  summarise(
    mean = mean(SS_dur, na.rm = TRUE),
    sd = sd(SS_dur, na.rm = TRUE)
)
df %>% group_by(stress, correctness) %>%
  summarise(
    mean = mean(SS_dur, na.rm = TRUE),
    sd = sd(SS_dur, na.rm = TRUE)
)

### Step 4: visual exploration and looking for outliers ###

## boxplots by group for F0 ##
df %>% ggplot() +
  aes(x = correctness, y = SS_F0) +
  geom_boxplot()
df %>% ggplot() +
  aes(x = stress, y = SS_F0) +
  geom_boxplot()
df %>% ggplot() +
  aes(x = correctness, y = SS_F0, fill = stress) +
  geom_boxplot()
## POSSIBLE OUTLIER: an APUs, ID = 49 with F0 = 148
# possibly also a PUs, ID = 34 with F0 = 248

## boxplots by group for intensity ##
df %>% ggplot() +
  aes(x = correctness, y = SS_int) +
  geom_boxplot()
df %>% ggplot() +
  aes(x = stress, y = SS_int) +
  geom_boxplot()
df %>% ggplot() +
  aes(x = correctness, y = SS_int, fill = stress) +
  geom_boxplot()
## POSSIBLE OUTLIER: APUd, ID = 12 with int = 63.6
# additionally: PUs, ID = 24 with int = 64.1; PUs, ID = 21 with int = 65.0;
# and maybe APUs, ID = 49 with int = 64.1, APUs, ID = 42 with int = 78.9
# PUd, ID = 67 with int = 77.7

## boxplots by group for duration ##
df %>% ggplot() +
  aes(x = correctness, y = SS_dur) +
  geom_boxplot()
df %>% ggplot() +
  aes(x = stress, y = SS_dur) +
  geom_boxplot()
df %>% ggplot() +
  aes(x = correctness, y = SS_dur, fill = stress) +
  geom_boxplot()
# no apparent outliers to verify

## Q-Q plots and histograms for each dependent variable ##

# F0
qqnorm(df$SS_F0)
qqline(df$SS_F0)
hist(df$SS_F0)
df %>% ggplot(aes(sample = SS_F0)) +  geom_qq() + geom_qq_line() + facet_wrap(~stress)
df %>% ggplot(aes(sample = SS_F0)) +  geom_qq() + geom_qq_line() + facet_wrap(~correctness)
# One very visible possible outlier: ID = 49 with F0 = 148 (the same one as the most odd case visible on the boxplots).
# Another one gets visible when data is split by stress (PUd, ID = 73, with f0 = 274).
# On the main Q-Q plot, there might be a skew which is "masked" by that last data point (ID = 73)
# that dot is quite far from others, making it a likely outlier.
# That point might also be responsible for the suspiciously long whiskers seen on the boxplots seen before.
# When split by stress, the data seems "skewy", especially for APU (and especially taking into account the outlier!)
# but by correctness it seems fine.
# Judging by the histogram and the main Q-Q plot, kurtosis seems to too high (positive).

# intensity
qqnorm(df$SS_int)
qqline(df$SS_int)
hist(df$SS_int)
df %>% ggplot(aes(sample = SS_int)) +  geom_qq() + geom_qq_line() + facet_wrap(~stress)
df %>% ggplot(aes(sample = SS_int)) +  geom_qq() + geom_qq_line() + facet_wrap(~correctness)
# The Q-Q plot doesn't look great, but it's not terrible: it follows more or less a straight line,
# but it's tilted relative to the qqline: possible (positive) kurtosis.
# No significant skew visible.

# duration
qqnorm(df$SS_dur)
qqline(df$SS_dur)
hist(df$SS_dur)
df %>% ggplot(aes(sample = SS_dur)) +  geom_qq() + geom_qq_line() + facet_wrap(~stress)
df %>% ggplot(aes(sample = SS_dur)) +  geom_qq() + geom_qq_line() + facet_wrap(~correctness)
# The dots are tilted again, but this time to the other side (= possible negative kurtosis)
# and they do not follow a straight line (= possible skewness).
# On the histogram, the negative skew seems visible,
# and the data seems to have a negative kurtosis relative to normal distribution.
# The skew is especially visible for PU in the Q-Q plot by stress.
df %>% ggplot(aes(x = SS_dur)) +  geom_histogram() + facet_wrap(~stress)
# additional graph to see that the negative skew is indeed quite visible for PU

### Step 5: checking the assumptions quantitatively ###

## Homogeneity of variance ##

# Our notes on Kampus suggested using fligner.test(SS_F0~interaction(stress, correctness), data=df),
# (similar code was used in a similar scenario, just with different variables)
# but during our statistics classes we were taught to perform Levene's test, and without including the interaction.
# One of the variables fails the assumption of homogeneity of variance (both with Flinger and Levene) if the interaction is included.
# I made the decision to continue and to take into account only the Levene's test with *no interactions*, as we were taught last year.

# dependent variable a - SS_F0
# without interaction
leveneTest(SS_F0~stress, data=df) # p = 0.4519 -> PASSED
leveneTest(SS_F0~correctness, data=df) # p = 0.7468 -> PASSED
# with interaction
fligner.test(SS_F0~interaction(stress, correctness), data=df)
# p-value = 0.002425 -> FAILED
leveneTest(SS_F0~stress*correctness, data=df)
# p-value = 0.003191 -> FAILED

# dependent variable b - SS_int
# without interaction
leveneTest(SS_int~stress, data=df) # p = 0.6993 -> PASSED
leveneTest(SS_int~correctness, data=df) # p = 0.201 -> PASSED
# with interactions
fligner.test(SS_int~interaction(stress, correctness), data=df)
# p-value = 0.285 -> PASSED
leveneTest(SS_int~interaction(stress, correctness), data=df)
# p-value = 0.2314 -> PASSED

# dependent variable c - SS_dur
# without interaction
leveneTest(SS_dur~stress, data=df) # p = 0.6714 -> PASSED
leveneTest(SS_dur~correctness, data=df) # p = 0.1458 -> PASSED
# with interaction
fligner.test(SS_dur~interaction(stress, correctness), data=df)
# p-value = 0.05814 -> BARELY PASSED
leveneTest(SS_dur~interaction(stress, correctness), data=df)
# p-value = 0.05147 -> BARELY PASSED

## Normality ##

# dependent variable a - F0 (SS_F0) #

# whole dataset
shapiro.test(df$SS_F0)
# p-value = 0.7389 -> PASSED
kurtosis(df$SS_F0) # 3.214804
skewness(df$SS_F0) # -0.01323476

# by group
df_split <- split(df$SS_F0, df$correctness)
lapply(df_split, shapiro.test)
# p-value for deviant = 0.2689 -> PASSED
# p-value for standard = 0.4584 -> PASSED
df_split <- split(df$SS_F0, df$stress)
lapply(df_split, shapiro.test)
# p-value for APU = 0.004817 -> FAILED
# p-value for PU = 0.001574 -> FAILED
df %>% group_by(correctness) %>% summarise(
  kurt = kurtosis(SS_F0), # dev: 2.69 and std: 3.09
  skew = skewness(SS_F0), # dev: 0.492 and std: -0.388
)
df %>% group_by(stress) %>% summarise(
  kurt = kurtosis(SS_F0), # APU: 4.90 and PU: 2.73
  skew = skewness(SS_F0), # APU: -1.18 and PU: 0.836
)
# Based on the output of SPSS, and adjusted kurtosis for APU = 3.28
# and adjusted skewness for APU = -3.44: both are too big, but I don't know
# how to properly check skewness and kurtosis for a small dataset in R.
# (I don't know how SPSS gets its error for kurtosis/skewness)
# In any case, the kurtosis is too high for APU here as well, and
# quite high for PU, but skewness seems fine, despite the fact that
# the plots and SPPS suggest there might be a problem.
# Kurtosis by stress is also very high for both groups, esp. standard.
# All the observations are in line with the visual inspection and Shapiro test.

# dependent variable b - INTENSITY (SS_int) #

# whole dataset
shapiro.test(df$SS_int)
# p-value = 0.2475 -> PASSED
kurtosis(df$SS_int) # 3.622674
skewness(df$SS_int) # -0.1406077

# by group
df_split <- split(df$SS_int, df$correctness)
lapply(df_split, shapiro.test)
# p-value for deviant = 0.3421 -> PASSED
# p-value for standard = 0.6627 -> PASSED
df_split <- split(df$SS_int, df$stress)
lapply(df_split, shapiro.test)
# p-value for APU = 0.4657 -> PASSED
# p-value for PU = 0.8264 -> PASSED
df %>% group_by(correctness) %>% summarise(
  kurt = kurtosis(SS_int), # dev: 4.31 and std: 3.36
  skew = skewness(SS_int), # dev: -0.370 and std: 0.120
)
df %>% group_by(stress) %>% summarise(
  kurt = kurtosis(SS_int), # APU: 3.67 and PU: 3.42
  skew = skewness(SS_int), # APU: -0.136 and PU: -0.135
)
# Kurtosis for all the groups is too big, but since the plots looked ok
# and all the Shapiro tests have been passed, I will not do anything about that.

# dependent variable c - DURATION (SS_dur) #

# whole dataset
shapiro.test(df$SS_dur)
# p-value = 0.09123 -> PASSED
kurtosis(df$SS_dur) # 2.098178
skewness(df$SS_dur) # -0.08949304

# by group
df_split <- split(df$SS_dur, df$correctness)
lapply(df_split, shapiro.test)
# p-value for deviant = 0.07201 -> PASSED
# p-value for standard = 0.2251 -> PASSED
df_split <- split(df$SS_dur, df$stress)
lapply(df_split, shapiro.test)
# p-value for APU = 0.4765 -> PASSED
# p-value for PU = 0.009221 -> FAILED
df %>% group_by(correctness) %>% summarise(
  kurt = kurtosis(SS_dur), # dev: 2.17 and std: 2.42
  skew = skewness(SS_dur), # dev: -0.399 and std: 0.381
)
df %>% group_by(stress) %>% summarise(
  kurt = kurtosis(SS_dur), # APU: 2.29 and PU: 1.72
  skew = skewness(SS_dur), # APU: 0.0615 and PU: -0.193
)
# The skewness for SS_dur is not too high in theory, but since it's visible on the graphs
# and the Shapiro test was failed for one of the groups with no apparent outliers,
# my interpretation is that the negative skewness is a problem indeed.

# The overall analysis points to issues with normality:
# for dependent variable SS_F0, there are issues with both APU and PU stress,
# for dependent variable SS_dur, the seems to be an issue for penultimate stress only.
# The dependent variable SS_int seems to be fine in every group.
# Taking into account all the exploration, the issues will be dealt with as follows:
# - for SS_F0, the best strategy seems to be a sensitivity analysis with and without the two outliers
# - for SS_dur, a transformation will be performed in and attempt to fix the negative skewness
# of the PU group, and possibly ameliorating the kurtosis which is quite high, although acceptable.

### Step 7: data transformation ###

## SS_dur transformation
df <- df %>% mutate(tSS_dur = 1/(max(SS_dur+1) - SS_dur))
# I checked several possible transformations and this one gave the best results

## Standardizing ##
df$zSS_F0 <- scale(df$SS_F0)
# ID = 49 is indeed an outlier, and very much so
# ID = 34 turned out not to be outlier in the context of the whole dataset,
# but it probably is one in relation to its own groups.
# Based on z-scores, ID = 73 is an outlier as well, but only by a little bit.
# It was also visible on normality plots, so it might be importnant for the normality.
df$zSS_int <- scale(df$SS_int)
# there are 2 outliers, but since the datset is small and they are not too outstanding,
# I'm not going to do anything about it.
df$ztSS_dur <- scale(df$tSS_dur)
# No outliers, just as expected.

### Step 8: Re-examining normality ###

# dependent variable a - F0 (SS_F0), but without the outliers #
filtered_df <- subset(df, zSS_F0>-2.5 & zSS_F0<2.5)
# by group
df_split <- split(filtered_df$SS_F0, filtered_df$correctness)
lapply(df_split, shapiro.test)
# p-value for deviant = 0.2982 -> PASSED
# p-value for standard = 0.3854 -> PASSED
df_split <- split(filtered_df$SS_F0, filtered_df$stress)
lapply(df_split, shapiro.test)
# p-value for APU = 0.1048 -> PASSED (removing ID = 49 helped)
# p-value for PU = 0.001825 -> FAILED (removing ID = 73 didn't help)
# I've also checked normality with ID = 34 removed (a suspicious data point 
# from the boxplot), but it didn't help,
# neither with and without removing ID = 73 alongside.
# Since the kurtosis was quite big, let's try transforming the variable:
skewness(df$SS_F0) # -0.01323476 -> the variable should be inverted for transformation
df <- df %>% mutate(tSS_F0 = sqrt(max(SS_F0) - SS_F0))
df$ztSS_F0 <- scale(df$tSS_F0)
df %>% ggplot(aes(sample = ztSS_F0)) +  geom_qq() + geom_qq_line() + facet_wrap(~stress)
df %>% ggplot(aes(sample = ztSS_F0)) +  geom_qq() + geom_qq_line() + facet_wrap(~correctness)
filtered_df <- subset(df, ztSS_F0>-2.5 & ztSS_F0<2.5)
df_split <- split(filtered_df$ztSS_F0, filtered_df$stress)
lapply(df_split, shapiro.test)
# p-value for APU = 0.3215 -> PASSED
# p-value for PU = 7.759e-05 -> FAILED: and it's terrible
# I've tried multiple transformations and nothing helps, so let's stick to sensitivity analysis
# without transformations of SS_F0.

# Dependent variable c - DURATION (SS_dur) #

# whole dataset
shapiro.test(df$ztSS_dur)
# p-value = 0.1105 -> PASSED
kurtosis(df$ztSS_dur) # 2.114936
skewness(df$ztSS_dur) # 0.03198789

# by group
df_split <- split(df$ztSS_dur, df$correctness)
lapply(df_split, shapiro.test)
# p-value for deviant = 0.1276 -> PASSED
# p-value for standard = 0.1011 -> PASSED
df_split <- split(df$ztSS_dur, df$stress)
lapply(df_split, shapiro.test)
# p-value for APU = 0.4 -> PASSED
# p-value for PU = 0.01268 -> FAILED, but it's better than before
qqnorm(df$ztSS_dur)
qqline(df$ztSS_dur)
hist(df$ztSS_dur)
df %>% ggplot(aes(sample = ztSS_dur)) +  geom_qq() + geom_qq_line() + facet_wrap(~stress)
df %>% ggplot(aes(sample = ztSS_dur)) +  geom_qq() + geom_qq_line() + facet_wrap(~correctness)
df %>% ggplot(aes(x = ztSS_dur)) +  geom_histogram() + facet_wrap(~stress)
df %>% ggplot(aes(x = ztSS_dur)) +  geom_histogram() + facet_wrap(~stress)
# The normality is better than it was, but it's sill bad in the PU group.
# There are no outliers that could be removed.
# Out of curiosity I checked if removing SS_F0's outliers would help 
# with SS_dur's normality by pure luck, but it won't.
filtered_df <- subset(df, zSS_F0>-2.5 & zSS_F0<2.5)
df_split <- split(filtered_df$ztSS_dur, filtered_df$stress)
lapply(df_split, shapiro.test)
# p-value = 0.01579 for PU without the SS_F0's outliers.

# CONCLUSION: normality for F0 and duration is unacheivable for the PU group.
# Since there are no other options and I *need* to perform the ANOVA 
# with the data I have, I'm going to proceed as if the data was normal.
# SS_dur transformation is going to be kept, as it helps a little,
# and ID = 49 will be removed for sensitivity analysis.
# Removing IDs = 34 and 73 doesn't help at all, so they stay.

### Step 9: ANOVAs ###

## ANOVA for F0
filtered_df <- subset(df, ID!=49)

interaction.plot(filtered_df$correctness, filtered_df$stress, filtered_df$zSS_F0)
interaction.plot(df$correctness, df$stress, df$zSS_F0)
# there seems to be an interaction and the design is unbalanced
# -> we use type III ANOVA

## without the outlier
fit <- aov(zSS_F0~correctness*stress, data = filtered_df)
Anova(fit, type=3)
# the only significant result is the interaction (p=0.02804)

## with the outlier
fit <- aov(zSS_F0~correctness*stress, data = df)
Anova(fit, type=3)
# nothing is significant

# CONCLUSION: the data is not reliable enough to use ANOVA:
# sensitivity analysis yeilds different results with and without the outlier

## ANOVA for intensity

interaction.plot(df$correctness, df$stress, df$zSS_int)
# there seems to be an interaction and the design is unbalanced
# -> we use type III ANOVA
fit <- aov(zSS_int~correctness*stress, data = df)
Anova(fit, type=3)
# significant results for stress (p = 0.0068769)
# and for the interaction (p = 0.0001582)
# there is no need for post-hocs, since there are only 2 groups per factor

# CONCLUSION: null hypothesis is rejected: the groups are significantly
# different in terms of the intensity.
# The data for this dependent variable was the most reliable,
# so this conclusion is quite well supported.

## ANOVA for duration

interaction.plot(df$correctness, df$stress, df$ztSS_dur)
# there seems to be no apparent interaction and the design is unbalanced
# -> we use type II ANOVA
fit <- aov(zSS_dur~correctness*stress, data = df)
Anova(fit, type=2)
# p-value is significant for correctness (p = 0.001238)

# CONCLUSION: This test was performed for the sake of completeness,
# as the null hypothesis has already been rejected in the previous step.
# Luckily so, because the data for this analysis wasn't as reliable.

### Results visualization ###
# Note: only the results for intensity are visualized, as
# they are the most reliable ones and they are enough
# to reject the null hypothesis.

ggline(df,
       x = "stress",
       y = "SS_int",
       color = "correctness",
       add = c("mean_se")
) +
  labs(x = "Stress placement",
    y = "Intensity (unstandardised values)",
    color = "Deviant vs standard pronunciation")

### FINAL CONCLUSIONS ###
# The dataset is not suitable for a correctness judgement test,
# because the words with different stress placement differ at least
# by the intensity of the stressed syllable.
# The intensity for stressed syllables in each placement is affected differently 
# depending on whether the stress is placed correctly or not.
# Participants may judge the words as correct on incorrect
# based on the intensity (which can be interpreted as
# confidence or a lack of thereof, which in turn may serve as a proxy
# for deciding if the person is either "lying" and trying to convince the listener,
# or feeling sure that they are pronouncing a word correctly).
# It is advisable to provide new stimuli, probably in a greater number,
# and to pick the most uniform ones.
#
## Some additional worries:
#
# In SPSS, only the interaction and intercepts are significant,
# and the confidence intervals on the plot overlap for APU stress.
# The stress in SPSS is completely insignificant (p = .983),
# which was something I noticed while trying to plot the main effect
# for my R model:
ggline(df,
       x = "stress",
       y = "SS_int",
       add = c("mean_se")
)
# This doesn't look significant at all :(
# I can understand how the main effect plot looks so flat,
# since the interaction makes the two groups go opposite directions,
# but I am worried about the inconsistency in p-values
# between the SPSS output and my analysis in R (interaction plot looks the same,
# but the p-value in between-subjects effects is basically the opposite).
# The correctness looks much more believable as 
# a potential significant main effect:
ggline(df,
       x = "correctness",
       y = "SS_int",
       add = c("mean_se")
)
# However you can see that the error bars are too close, so I makes a lot of sense
# for it to be insignificant both in SPSS and in R.
# There is something going on, but I reject the null hypothesis nevertheless,
# because at least the interaction is (almost!) a 100% sure to be significant.