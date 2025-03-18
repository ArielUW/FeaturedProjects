library(tidyverse)
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
print(getwd())
ultrasound <- readRDS("ultrasound_data_sample.rds")

## Exercise 1
# Add a new column called "Context" based on PrecedingSound, with the 
# following values: vowel, consonant, pause (na = pause)
# Create new plots showing the trajectories of the tongue 
# to answer the following questions:
# 1) are the trajectories different depending on the context?
# 2) are the trajectories different depending on the following sound?
# Embellish the two plots by adding a title, axis labels and legend labels
# You can change colours and theme. PEN = Peninsular speakers, CAN = Canarian speakers
# Save the plots to your computer.

##unique(dat$PrecedingSound)
##[1] "o"  "na" "r"  "u"  "e"  "s"  "a" 
vowels = c("o", "u", "e", "a")
consonants = c("r", "s")
pause = c("na")
ultrasound <- ultrasound %>%
  mutate(
    Context = case_match(PrecedingSound, vowels ~ "vowel",
         consonants ~ "consonant",
         pause ~ "pause"))

long <- ultrasound %>% filter(Sample == 3) %>% 
  pivot_longer(
    cols = `01x`:`11y`, 
    names_to = c("coord_number", "coord_type"),
    names_pattern = "(..)(.)",
    values_to = "coord_value"
  )

long <- long %>% 
  pivot_wider( 
    names_from = "coord_type",
    values_from = "coord_value"
  )

data_context <- long %>% group_by(Context, Group, coord_number, Sound) %>% 
  summarise(m_x= mean(x), m_y= mean(y), n=n())

group.labels = c("Canary Islands", "Iberian Peninsula")
names(group.labels) = c("CAN", "PEN")

data_context %>% ggplot(aes(x = m_x, y = m_y, group = Context, colour = Context)) + 
  geom_line() +
  geom_point() +
  labs(x = "X coordinate of the tongue", 
       y = "Y coordinate of the tongue",
       title ="Average tongue trajectory for J, L and tS sounds\nby preceding context and by geographical variety") +
  scale_color_manual(values = c("coral", "mediumpurple4", "aquamarine3")) +
  facet_grid(Group ~ Sound,
             labeller = labeller(Group = group.labels))

data_following <- long %>% group_by(FollowingSegment, Group, coord_number, Sound) %>% 
  summarise(m_x= mean(x), m_y= mean(y), n=n())

data_following %>% ggplot(aes(x = m_x, y = m_y, group = FollowingSegment, colour = FollowingSegment)) + 
  geom_line() +
  geom_point(shape = 1, size = 2) +
  labs(x = "X coordinate of the tongue", 
       y = "Y coordinate of the tongue",
       title ="Average tongue trajectory for J, L and tS sounds\nby following sound and by geographical variety",
       color = "Following segment") +
  scale_color_manual(values = c("coral", "darkorchid3", "steelblue3", "chartreuse4", "gold2"))+
  facet_grid(Sound ~ Group,
             labeller = labeller(Group = group.labels))

  ## Exercise 2
# Are there differences between the groups depending on the sound?
# Use the original database to create a long format and draw trajectories.
# Embellish and save the plot. Let's assume that J is a palatal fricative, 
# L is a palatal lateral and tS is a palatal affricate.

data_geo <-  long %>% group_by(Group, coord_number, Sound) %>% 
  summarise(m_x= mean(x), m_y= mean(y), n=n())

sound.labels = c("palatal fricative", "palatal lateral", "palatal affricate")
names(sound.labels) = c("J", "L", "tS")

data_geo %>% ggplot(aes(x = m_x, y = m_y, group = interaction(Group, Sound), colour = Sound)) + 
  geom_point(aes(shape = Group), size = 2) +
  geom_line() +
  labs(x = "X coordinate of the tongue", 
       y = "Y coordinate of the tongue",
       title ="Average tongue trajectory for palatal sounds\nby geographical variety",
       color = "Sound",
       shape = "Geographical variety") +
  scale_color_manual(values = c("coral", "chartreuse4", "gold2"), 
                     labels = c("palatal fricative", "palatal lateral", "palatal affricate"))+
  scale_shape_manual(values = c(16,17), labels = c("Canary Islands", "Iberian Penninsula"))
  
