###########################################
#    ______  ______ _                     #
#    | ___ \ | ___ (_)                    #
#    | |_/ / | |_/ /_ _ __   ___  ___     #
#    |    /  |  __/| | '_ \ / _ \/ __|    #
#    | |\ \  | |   | | |_) |  __/\__ \    #
#    \_| \_| \_|   |_| .__/ \___||___/    #
#                    | |                  #
#                    |_|                  #
###########################################

###########################################
############# Intro to %>% ################
###########################################

if (! "tidyverse" %in% rownames(installed.packages())) {
  install.packages("tidyverse")
}
library(tidyverse)
library(magrittr)

### How does %>% work? ###

## A simple example without pipes ##
# Get some words
words <- c("!", "pipes", "HELLO ")
words

# Make everything lower case
words <- tolower(words)
words

# Reverse the order
words <- rev(words)
words

# Paste together into a sentence
words <- paste(words, collapse = "")
words

# Convert to sentence case
words <- str_to_sentence(words)
words

# Print the sentence using cat
cat(words)

## The same example, but with pipes ##

# How to use a pipe? Basic example with tolower()
words <- c("!", "pipes", "HELLO ")
tolower(words)  # This is the same thing as...
words %>% tolower() # this.

# Straight thru and print
words <- c("!", "pipes", "HELLO ")
words %>%  
  tolower() %>%
  rev() %>%
  paste(collapse = "") %>%
  str_to_sentence() 

# Assign to a new variable
words <- c("!", "pipes", "HELLO ")
new_words <- words %>%  
  tolower() %>%
  rev() %>%
  paste(collapse = "") %>%
  str_to_sentence() 
cat(new_words)

# Assign to a new variable using <- ->
words <- c("!", "pipes", "HELLO ")
words %>%  
  tolower() %>%
  rev() %>%
  paste(collapse = "") %>%
  str_to_sentence() -> new_words2
cat(new_words2)

# Re-assign to original variable with %<>% pipe
words <- c("!", "pipes", "HELLO ")
words %<>%
  tolower() %>%
  rev() %>%
  paste(collapse = "") %>%
  str_to_sentence()
cat(words)

# What happens if we include cat() in the pipeline?...
words <- c("!", "pipes", "HELLO ")
words %>%  
  tolower() %>%
  rev() %>%
  paste(collapse = "") %>%
  str_to_sentence() %>%
  cat()

# AND try to assign the output?
words <- c("!", "pipes", "HELLO ")
words %<>%  
  tolower() %>%
  rev() %>%
  paste(collapse = "") %>%
  str_to_sentence() %>%
  cat()

# What if we just try putting cat() earlier in the pipe?
words <- c("!", "pipes", "HELLO ")
words %<>%  
  tolower() %>%
  cat() %>%
  rev() %>%
  paste(collapse = "") %>%
  str_to_sentence() 

# Accomplish this with the Tee pipe!
words <- c("!", "pipes", "HELLO ")
words %<>%  
  tolower() %T>%
  cat() %>%
  rev() %>%
  paste(collapse = "") %>%
  str_to_sentence() 

### Example: Star Wars! ###

data("starwars")

# Let's do some exploratory data analysis!
starwars

# Filter the data to include male characters which are Human, Gungan, or Wookie
starwars_males <- filter(starwars, sex == "male" & ! is.na(height) &
                           species %in% c("Human", "Gungan", "Wookiee"))
starwars_males

## What is the average height for each species? ##
# Step #1. Group by species
starwars_males_grouped <- group_by(starwars_males, species)
starwars_males_grouped

# Step #2. Summarize to get the average height
average_male_height <- summarise(starwars_males_grouped, avg_height=mean(height))
average_male_height

## Could we do this in one line?? ##
starwars %>%
  filter(sex == "male" & ! is.na(height) & 
           species %in% c("Human", "Gungan", "Wookiee")) %>%
  group_by(species) %>%
  summarise(avg_height=mean(height)) 

## What about mixing plotting with analysis...? ##
# Plot the results!
ggplot(starwars_males,
       mapping = aes(x = species, y = height, fill = species)) +
  geom_boxplot() +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") + 
  xlab(NULL) +
  ylab("Height (cm)") +
  labs(title = "Height of characters from Star Wars") 

# How can we incorporate this in the pipeline?
starwars %>%
  filter(sex == "male" & ! is.na(height) & 
           species %in% c("Human", "Gungan", "Wookiee")) %>%
  ggplot(mapping = aes(x = species, y = height, fill = species)) +
  geom_boxplot() +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") + 
  xlab(NULL) +
  ylab("Height (cm)") +
  labs(title = "Height of characters from Star Wars") 

# Is there a way to also get our summary still? 
starwars %>%
  filter(sex == "male" & ! is.na(height) & 
           species %in% c("Human", "Gungan", "Wookiee")) %T>%
  {group_by(., species) %>%
           summarise(avg_height=mean(height)) %>%
      print()} %>%
  ggplot(mapping = aes(x = species, y = height, fill = species)) +
  geom_boxplot() +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") + 
  xlab(NULL) +
  ylab("Height (cm)") +
  labs(title = "Height of characters from Star Wars") 

# Can we also include a one-way ANOVA (with post-hoc) to compare species means?
aov <- aov(formula = height ~ species, data=starwars_males)
TukeyHSD(aov)

# Add this into the pipe
# Is there a way to also get our summary still? 
starwars %>%
  filter(sex == "male" & ! is.na(height) & 
           species %in% c("Human", "Gungan", "Wookiee")) %T>%
  {group_by(., species) %>%
      summarise(avg_height=mean(height)) %>%
      print()} %T>%
  {aov(formula = height ~ species, data=.) %>%
      TukeyHSD() %>%
      print()} %>%
  ggplot(mapping = aes(x = species, y = height, fill = species)) +
  geom_boxplot() +
  theme_bw(base_size = 16) +
  theme(legend.position = "none") + 
  xlab(NULL) +
  ylab("Height (cm)") +
  labs(title = "Height of characters from Star Wars") 



