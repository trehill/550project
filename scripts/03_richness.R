#Richness and other metrics 
#Author: Tessa Rehill 
#Making new variables 

#We can only calculate species richness (and not diversity, because we only have presence/absence) and 
#not abundance values 

#Different variables that we want to calculate based on our current data

#Species richness per quadrat (each row of df)
#Species richness per transect (multiple rows of df)
#Algal species richness 
#Invert species richness
#Kelp presence (binary, y/n)
#Species richness of kelp ONLY
#Green urchin presence
#Purple urchin presence
#Any urchin species presence 

#set-up 
library(tidyverse)
library(dplyr)
library(here)

#read in data 
data <- read.csv(here::here("Desktop","550project","datasets", "raw_data", "BHM_habitat.csv"),
                 head=TRUE)

data <- na.omit(data) #remove NA

key <- read.csv(here::here("Desktop","550project","datasets", "keys",
                           "full_key_final.csv"), #from report 
                head=TRUE)

key <- na.omit(key) #remove NA

#Determine species richness (number of species) per quadrat ####

#this is just the sum of the presence/absence matrix per row, isolate
new_data <- data[c('Quadrat', 'Transect', colnames(data)[17:ncol(data)])]

#make new column as key 
new_data$Transect <- as.character(new_data$Transect)
new_data$transectquad <- paste0(new_data$Transect, new_data$Quadrat)

#make first column
new_data <- new_data[, c('transectquad', names(new_data)[setdiff(seq_along(new_data), which(names(new_data) == 'transectquad'))])]

#make numeric 
#new_data <- new_data[, 4:ncol(new_data)] <- sapply(new_data[, 4:ncol(new_data)], as.numeric)

df <- new_data %>%
  mutate_at(vars(4:ncol(new_data)), as.numeric) 

df_selected <- df %>%
  select(starts_with("D"))

df_selected <- df %>%
  select(-(1:3))

#sum row
dataset3 <- mutate (df_selected, quadrat_richness = rowSums (df_selected))

colnames(dataset3)

#add 'total_richness' column to 'data'
data$quadrat_richness <- dataset3$quadrat_richness

#reorder columns in 'data' to bring 'total_richness' to the third position
data <- data[, c(1, 2, ncol(data), 3:(ncol(data)-1))]

print(data)

#Determine species richness per transect (multiple rows of df) ####

#here we will sum the 'total_richness' column per transect to create a new 
#column called transect_richness

result <- data %>%
  group_by(Transect) %>%
  summarise(transect_richness = sum(quadrat_richness))

data <- data %>%
  left_join(result, by = "Transect")

#move 'transect_richness' to the fourth position
data <- data %>%
  select(1:3, transect_richness, 4:(ncol(data)-1))

print(data)

#Determine species richness for algal and inverts ####

#we want to remove urchins from the key so that when we calculate invert richness 
#it only includes non-urchin species 
#remove RH - red urchins, AL -green urchins, PU - purple urchins
new_key <- key 

new_key <- new_key %>% 
  filter(!(key %in% c('RH', 'AL', 'PU')))


#select transect, quadrat + presence/absence matrix 
df <- data %>%
  select(Transect, Quadrat, 19:ncol(data))

#subset for algal 
#filter the columns in 'df' where the corresponding 'type' in 'key' is 'algae'
algae_columns <- df %>%
  select(names(df)[new_key$type == 'algae']) #now we have only 52 columns, does this makes sense? 

count_algae_rows <- sum(new_key$type == 'algae') #52, nice! 

#subset for inverts 
invert_columns <- df %>%
  select(-any_of(names(df)[new_key$type == 'algae']))

#sum values in matrix into new column 
#algal species richness 
algae_columns <- algae_columns %>%
  mutate(algae_richness = rowSums(select(., -c(Quadrat, Transect)), na.rm = TRUE))

#invert species richness 
invert_columns$invert_richness <- rowSums(invert_columns)

#add columns to data
data$invert_richness <- invert_columns$invert_richness

data$algae_richness <- algae_columns$algae_richness

colnames(data)

#Determine kelp presence and richness (quadrat level) ####

#select transect, quadrat + presence/absence matrix 
df <- data %>%
  select(Transect, Quadrat, 19:ncol(data))

#filter the columns in 'df' where the corresponding 'type' in 'key' is 'brown_algae'
kelp_columns <- df %>%
  select(names(df)[key$phylum == 'brown_algae']) 

#create new column that is kelp richness
kelp_columns$kelp_richness <- rowSums(kelp_columns)

#add column that is 'y' if kelp richness >0, and n if < 0 
kelp_columns$kelp_presence <- ifelse(kelp_columns$kelp_richness > 0, 'y', 'n')

#add these two columns back into data 
data$kelp_richness <- kelp_columns$kelp_richness

data$kelp_presence <- kelp_columns$kelp_presence

colnames(data)

#Determine urchin presence ####
#here we want four different columns
  #1. urchin presence (either red or green or purple)
  #2. green urchin presence (AL)
  #3. purple urchin presence  (PU)
  #4. red urchin presence (RH)

df <- data

#green urchin presence 
df$g_presence <- ifelse(df$AL > 0, 'y', 'n')

#purple urchin presence 
#df$p_presence <- ifelse(df$PU > 0, 'y', 'n') #there doesn't exist a PU, so this code won't run

#red urchin presence 
df$r_presence <- ifelse(df$RH > 0, 'y', 'n') 

#urchin presence in general 
df$urchin_presence <- ifelse(df$g_presence == 'y' | df$r_presence == 'y', 'y', 'n')

#add columns to data 
data$g_presence <- df$g_presence
data$r_presence <- df$r_presence
data$urchin_presence <- df$urchin_presence

colnames(data)

#save data 
write_csv(data,
          here( "Desktop","550project","datasets","processed_data",
               "processed_data.csv")) 
