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
data <- read.csv(here::here("Desktop","550project","datasets", "raw_data", "BHM_habitat.csv"),
                 head=TRUE)

key <- read.csv(here::here("Desktop","550project","datasets", "keys", 
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

#remove NA 
data <- na.omit(data)

#plot

plot <- ggplot(data, aes(x = Substrate, fill = Substrate)) +
  geom_bar() +
  labs(title = "Bar Plot of Substrate per Quadrat",
       x = "Substrate",
       y = "Count") +
  theme_minimal()
plot

#save plot
ggsave("./outputs/exploratory/substrate_bar.png", 
       plot = plot,
       width = 6, height = 6, units = "in")

#stacked plot
plot <- ggplot(data, aes(x = factor(SubCat), fill = SubSubCat)) +
  geom_bar() +
  labs(title = "Stacked Bar Plot of Substrate",
       x = "SubCat",
       y = "Count") +
  theme_minimal()
plot

ggsave("./outputs/exploratory/subsubstrate_stackedbar.png", 
       plot = plot,
       width = 6, height = 6, units = "in")


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
plot <- ggplot(algae_sp, aes(x = phylum, y = count, fill=phylum)) +
  geom_boxplot() +
  labs(title = "Boxplot of Algae Phylums per Quadrat",
       x = "Phylum",
       y = "Count") +
  theme_minimal()
plot

ggsave("./outputs/exploratory/algal_phylum.png", 
       plot = plot,
       width = 6, height = 6, units = "in")


#INVERTS ####

#select only inverts
invert_sp <- sum_sp[sum_sp$type == 'invertebrate', ]


#boxplot 
plot <- ggplot(invert_sp, aes(x = phylum, y = count, fill=phylum)) +
  geom_boxplot() +
  labs(title = "Boxplot of 'phylum' by 'count'",
       x = "Phylum",
       y = "Count") +
  theme_minimal()
plot 

ggsave("./outputs/exploratory/invert_phylum.png", 
       plot = plot,
       width = 6, height = 6, units = "in")

#VEGETATION ####

veg <- data[, c('CanopyPct', 'UnderstoryPct', 'TurfPct', 'EncrustingPct', 'DriftPct')]

#reshape data
data_long <- tidyr::gather(veg, key = "Category", value = "Percentage")

#boxplot
plot <- ggplot(data_long, aes(x = Category, y = Percentage, fill=Category)) +
  geom_boxplot() +
  labs(title = "Boxplot of Percentage Cover by Vegetative Category",
       x = "Category",
       y = "Percentage") +
  theme_minimal()
plot

ggsave("./outputs/exploratory/vegetation.png", 
       plot = plot,
       width = 6, height = 6, units = "in")


