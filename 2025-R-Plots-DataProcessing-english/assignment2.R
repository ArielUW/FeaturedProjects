### Ariel Drozd, Assignment 2 ###
### Exercise 1 ###

library("tidyverse")
getwd()

#Prepare your script and working directory. Load Ling_Van_data. Note that it’s an Excel file! Solve it using your imagination or R documentation (help).
#install.packages("readxl")
library("readxl")
data <- read_excel("Ling_Van_data.xlsx")

#How many columns and how many rows does the database have?
ncol(data) #18
nrow(data) #509
#alternative solution:
data #A tibble: 509 × 18

#What are the ages of the participants?
data %>% distinct(data$age) #24 and 25

#How many observations do we have per participant?
data %>% count(data$subject) #between 85 and 183 per participant
#<dbl> <int>
#   1    85
#   2   123
#   3   118
#   4   183
#or if the question was to count the average number per participant:
#nrow(data)/n_distinct(data$subject) #127.25 observations per participant

#Select only “p” sounds and only in the laboratory setting, then provide the mean intensity difference 
#for each participant from this subdatabase.
data %>% 
  filter(annotated_sound=="p"&recording_setting=="lab") %>% 
  group_by(subject) %>% 
  summarize(mean_intensity_dif=mean(intensity_difference))
#subject mean_intensity_dif
#<dbl>            <dbl>
# 1               28.5
# 2               18.3
# 3               24.7
# 4               19.4

#Select only the numeric columns and provide a summary of descriptive statistics for all of them.
data %>%
  select(where(is.numeric)) %>%
  summary()
#subject           age           int_minC        int_maxV2       intensity_difference
#Min.   :1.000   Min.   :24.00   Min.   :17.81   Min.   :41.85   Min.   : 0.254      
#1st Qu.:2.000   1st Qu.:25.00   1st Qu.:36.06   1st Qu.:54.47   1st Qu.:12.220      
#Median :3.000   Median :25.00   Median :42.66   Median :59.36   Median :16.324      
#Mean   :2.784   Mean   :24.83   Mean   :44.53   Mean   :61.64   Mean   :17.109      
#3rd Qu.:4.000   3rd Qu.:25.00   3rd Qu.:53.62   3rd Qu.:70.34   3rd Qu.:21.100      
#Max.   :4.000   Max.   :25.00   Max.   :73.72   Max.   :83.64   Max.   :44.751      

#VCV_dur          relative_C_dur  
#Min.   :0.1069   Min.   :0.1193  
#1st Qu.:0.1580   1st Qu.:0.2960  
#Median :0.1820   Median :0.3438  
#Mean   :0.2029   Mean   :0.3534  
#3rd Qu.:0.2216   3rd Qu.:0.4043  
#Max.   :0.7016   Max.   :3.9196  

#Change column names to uppercase.
data <- rename_with(data, toupper)

#Create a new column called pronunciation based on ‘annotated_sound’. In this new column, [p t k] should be named ‘voiceless stops’, [b d g] should be named ‘voiced stops’ and the remaining sounds should be named ‘approximants’.
#unique(data$ANNOTATED_SOUND) #"b" "g" "d" "G" "t" "p" "B" "k" "D"

data <- data %>% mutate(PRONUNCIATION = case_when(
  ANNOTATED_SOUND %in% c("p", "t", "k") ~ "voiceless stop",
  ANNOTATED_SOUND %in% c("b", "d", "g") ~ "voiced stop",
  .default = "approximant"))

#Create a new column called ‘lenition’ in which:
##/p/ annotated as [p] has a value of ‘0’ (no lenition)
##/t/ annotated as [t] has a value of ‘0’ (no lenition)
##/k/ annotated as [k] has a value of ‘0’ (no lenition)
##/p/ annotated as [b] has a value of ‘1’ (one change)
##/p/ annotated as [d] has a value of ‘1’ (one change)
##/p/ annotated as [g] has a value of ‘1’ (one change)
##/p/ annotated as [B] has a value of ‘2’ (two changes)
##/p/ annotated as [D] has a value of ‘2’ (two changes)
##/p/ annotated as [G] has a value of ‘2’ (two changes)

data <- data %>% mutate(LENITION = case_when(
  ANNOTATED_SOUND %in% c("p", "t", "k") ~ 0,
  ANNOTATED_SOUND %in% c("b", "d", "g") ~ 1,
  ANNOTATED_SOUND %in% c("B", "D", "G") ~ 2))

data$LENITION <- ordered(data$LENITION,levels=c(0,1,2), labels=c("no lenition", "one change", "two changes"))

### Exercise 2 ###
#Plot a histogram of intensity difference depending on each level of lenition.
#Embellish and name the plot. Save it to your computer.

plot1 <- data %>% ggplot(aes(x=INTENSITY_DIFFERENCE)) +
  geom_histogram(color="coral4", fill="coral") +
  geom_vline(aes(xintercept = mean(INTENSITY_DIFFERENCE)), linetype="dashed") +
  labs(x = "Intensity difference",
       y = "Count",
       title = "Intensity difference histogram\nfor each level of lenition") +
  facet_grid(rows = data$LENITION)

ggsave(plot1, 
       filename = "histogram.png",
       device = "png",
       height = 2000, width = 1800, units = "px")

#Try to recreate this plot: (you can use different colors)
install.packages("ggpubr")
library(ggpubr)
data$RECORDING_SETTING <-factor(data$RECORDING_SETTING,levels=c("lab", "social_media"), labels=c("Lab", "Social media"))

colors = c("lightgreen", "darkgreen")

burst <- data %>% ggplot(aes(fill=BURST, y=1, x=RECORDING_SETTING)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = colors) +
  labs(x = "",
       y = "Percent")

formants <- data %>% ggplot(aes(fill=FORMANTS, y=1, x=RECORDING_SETTING)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = colors) +
  labs(x = "",
       y = "Percent")

plot2 <- ggarrange(burst, formants, ncol=2, nrow=1)

ggsave(plot2, 
       filename = "barplot.png",
       device = "png",
       height = 2000, width = 1900, units = "px")

#Create a scatterplot that shows whether intensity difference differs depending on sound duration (relative_C_dur).
#Add pronunciation as an additional variable marked by different colours. Embellish and save the plot.
#Hint: get rid of the obvious outlier so that the plot makes sense.
data$z_scores <- scale(data$RELATIVE_C_DUR)

plot3 <- data %>% 
  subset(z_scores<3) %>%
  ggplot(aes(x = RELATIVE_C_DUR,
             y = INTENSITY_DIFFERENCE,
             color = PRONUNCIATION,
             shape = PRONUNCIATION)) + 
  geom_point(alpha = 0.6, size = 3) +
  scale_color_manual(values = c("darkorange","mediumpurple3", "green3")) +
  scale_shape_manual(values = c(16,17,4)) +
  labs(x = "Relative sound duration",
       y = "Intensity difference",
       color = "Pronunced sound",
       shape = "Pronunced sound",
       title = "Intensity difference by sound duration\nfor different types of pronunced sounds")

ggsave(plot3, 
       filename = "scatterplot.png",
       device = "png",
       height = 1800, width = 2000, units = "px")
  
#Create a boxplot showing relative sound duration in seconds (‘relative_C_dur’) depending on the level of lenition.
#Does the degree of lenition influence the duration of the consonant? Embellish and save the plot.
plot4 <- data %>%
  subset(z_scores<3) %>%
  ggplot(aes(x = LENITION,
    y = RELATIVE_C_DUR,
    group = LENITION,
    fill = LENITION)) +
  geom_boxplot() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("gold1","pink2", "turquoise3")) +
  labs(x = "Level of lenition",
    y = "Relative sound duration [s]",
    title = "Relative sound duration by level of lenition")

ggsave(plot4, 
       filename = "boxplot.png",
       device = "png",
       height = 1800, width = 2000, units = "px")
