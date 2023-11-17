#550 Project 
#Author: Tessa Rehill 

#SET-UP ####

#install.packages('here')
#install.packages('tidyverse')

setwd('/Users/tessarehill/Desktop/BIOL550/550project') #change this
getwd()


library(ggplot2)
library(here)
library(dplyr)
library(tidyr)
library(tidyverse)
library(here) 
library(dplyr)

#Exploratory Analysis 

#read in data 
data <- read.csv(here::here("BIOL550", "550project", "datasets","raw_data","BHM_habitat.csv"),
                     head=TRUE)

colnames(data)

#create new df for each 'coded' letters 
#select columns 17-143
selected_columns <- data[, 17:143]

#summarize presence absence for each species code 

# Assuming your dataframe is named 'presence_absence_data'
summary_data <- selected_columns %>%
  summarise(across(everything(), sum)) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "Row_Names") %>%
  rename(
    key = Row_Names,
    count = V1
  )


#create new df where these column names are rows 
new_dataframe <- data.frame(Column_Names = names(selected_columns))

#save this new dataframe (so we can manually create a key for each)
write_csv(new_dataframe,
          here("BIOL550", "550project", "datasets", "keys",
               "key.csv")) 

algae <- read.csv(here::here("BIOL550", "550project","datasets", "keys",
                            "algae_key.csv"), #from report (manually made)
                 head=TRUE)

algae <- algae[, 0:3]


invert <- read.csv(here::here("BIOL550", "550project","datasets","keys",
                             "invert_key.csv"), #from report 
                  head=TRUE)

key <- rbind(algae, invert)

write_csv(key,
          here("BIOL550", "550project","datasets","keys",
               "full_key.csv")) 

#now that we have a key, let see what codes are in and are NOT in our dataset 
# Assuming your dataframes are named 'key' and 'new_dataframe'

# Merge based on the 'key' column
merged<- merge(new_dataframe, key, by.x = "Column_Names", by.y = "key", all.x = TRUE)

merged <- merged %>% 
  rename(
    key = Column_Names
  )

 #why are so many codes missing? 

#merge this dataframe with our count data

count_merged <- merge(merged, summary_data, by = "key", all = TRUE)

  #okay so some of the missing codes, we dont have presence for! That's good! 

  #let's find out which codes HAVE count but no key! 

  #remove any 0 in count column 

count_merged_filtered <- count_merged %>%
  filter(count != 0)

  #now isolate those with NA

na_frame <- count_merged_filtered %>%
  filter(is.na(species))

#okay were missing 19 species codes in total 

write_csv(na_frame,
          here("BIOL550", "550project","datasets","keys",
               "missing_key.csv")) 


#add to full key 

#okay now we have a full key of species to species codes for all our present species! 

sp_key <- read.csv(here::here("datasets",
                             "full_key_final.csv"), #from report
                  head=TRUE)







