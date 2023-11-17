#550 Project 
#Exploratory Analysis 

library(ggplot2)
library(here)
library(dplyr)
library(tidyr)
library(tidyverse)
library(here) 
library(dplyr)

#Exploratory Analysis 

#read in data 
data <- read.csv(here::here("Desktop", "550project", "BHM_habitat.csv"),
                 head=TRUE)

key <- read.csv(here::here("Desktop", "550project", "datasets",
                           "full_key_final.csv"), #from report 
                head=TRUE)

ncol(data) #143 
nrow(data) #15268

#what kind of information do we have? 
#we have information on 1) substrate (categorical)
                        #2) vegetation percentage (#) 
                        #3) invertebrate and algal species presence/absence (binary)

#SUBSTRATE ####
#substrate (sub cat) 
  #1 - hard
  #2 - mixed
  #3 - soft

  #1a - bedrock dominant
  #1b - boulder dominant
  #2 - mixed 
  #3a - sand/shel
  #3b - mud 

#create new column called substrate 

data <- data %>%
  mutate(Substrate= case_when(
    SubCat == 1 & SubSubCat == 'a' ~ 'bedrock',
    SubCat == 1 & SubSubCat == 'b' ~ 'boulder',
    SubCat == 2 ~ 'mixed',
    SubCat == 3 & SubSubCat == 'a' ~ 'shell_sand',
    SubCat == 3 & SubSubCat == 'b' ~ 'mud',
    TRUE ~ 'unknown'  
  )) %>%
  select(SubCat, SubSubCat, Substrate, everything())

#plot

ggplot(data, aes(x = Substrate, fill = Substrate)) +
  geom_bar() +
  labs(title = "Bar Plot of Substrate",
       x = "Substrate",
       y = "Count") +
  theme_minimal()

#stacked plot
ggplot(data, aes(x = factor(SubCat), fill = SubSubCat)) +
  geom_bar() +
  labs(title = "Stacked Bar Plot of Substrate",
       x = "SubCat",
       y = "Count") +
  theme_minimal()

#ALGAE ####
#select columns 17-143
selected_columns <- data[, 18:143]

#summarize presence absence for each species code 
summary_data <- selected_columns %>%
  summarise(across(everything(), sum)) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "Row_Names") %>%
  rename(
    key = Row_Names,
    count = V1
  )

#add species key so we know what these counts are 
sum_sp <- merge(summary_data, key, all.x=TRUE)
sum_sp <- sum_sp[!is.na(sum_sp$species), ] #remove NA

#remove all 0 data 
sum_sp <- sum_sp[sum_sp$count != 0, ]

#select only algae 
algae_sp <- sum_sp[sum_sp$type == 'algae', ]

#boxplot 
ggplot(algae_sp, aes(x = phylum, y = count, fill=phylum)) +
  geom_boxplot() +
  labs(title = "Boxplot of 'phylum' by 'count'",
       x = "Phylum",
       y = "Count") +
  theme_minimal()

#INVERTS ####

#select only inverts
invert_sp <- sum_sp[sum_sp$type == 'invertebrate', ]


#boxplot 
ggplot(invert_sp, aes(x = phylum, y = count, fill=phylum)) +
  geom_boxplot() +
  labs(title = "Boxplot of 'phylum' by 'count'",
       x = "Phylum",
       y = "Count") +
  theme_minimal()

#VEGETATION ####

veg <- data[, c('CanopyPct', 'UnderstoryPct', 'TurfPct', 'EncrustingPct', 'DriftPct')]

#reshape data
data_long <- tidyr::gather(veg, key = "Category", value = "Percentage")

#boxplot
ggplot(data_long, aes(x = Category, y = Percentage)) +
  geom_boxplot() +
  labs(title = "Boxplot of Percentage Cover by Vegetative Category",
       x = "Category",
       y = "Percentage") +
  theme_minimal()

#Okay, now to some stats/models? 

#We're gonna have to determine if we want to go at the transect or quadrat level 
  #The transects are made up of many quadrats
  #Let's start at the quadrat level (this way we don't have to group our data by transect yet and keep it in the 
  #format that we already have it in )

#let's first create some metrics of 'species' 

#make a column that counts the number of all species 
#make a column that counts the number of algae species in a quadrat
#make a column that counts the number of invert species in a quadrat 

#make a column that counts the number of species across each algal phylum 
#make a column that counts the number of species across each invert phylum 


#explore relationship between algal + invert 
#explore relationship between all/algal/invert + substrate types

