#making data into the transect level 

library(dplyr)
library(readr)

#read in data 
data <- read.csv(here::here("datasets", "processed_data", "processed_data.csv"),
                 head=TRUE)

env <- read.csv(here::here("datasets", "raw_data", "BHM_RA.csv"),
                head=TRUE)
env <- subset(env, select = -Transect)

#merge with env (includes Year)

data <- merge(data, env, by = 'HKey', all.x = TRUE)

#select only relevant columns 
# Select only the specified columns
selected_columns <- c("HKey", "Transect", "transect_richness","invert_richness", "kelp_richness",
                      "algae_richness", "kelp_presence", "g_presence", "SubCat", "SubSubCat",
                      "r_presence", "urchin_presence", "algae_richness","Year","Month","Day")

data <- data %>% 
  select(selected_columns)


colnames(data)


#variables we want in final dataset
  #transect richness
  #sum invert richness (across quadrats)
  #sum algae richness (across quadrats)
  #sum kelp richness (across quadrats)
  #if any quadrat level kelp_presence >0, then kelp presence = 1 
  #if any quadrat level g_presence >0, then g presence = 1   
  #if any quadrat level r_presence >0, then r presence = 1  
  #urchin_presence 

# Assuming your original dataframe is named 'data'
# Replace the following column names with your actual column names

# Dominant SubCat in transect
new_data <- data %>%
  group_by(Transect) %>%
  mutate(SubCatTran = as.integer(names(which.max(table(SubCat)))))

colnames(new_data)

# Dominant SubSubCat in transect
new_data <- new_data %>%
  group_by(Transect) %>%
  mutate(SubSubCatTran= names(sort(table(SubSubCat), decreasing = TRUE)[1]))

colnames(new_data)

# Sum invert_richness across quadrats
new_data <- new_data %>% 
  group_by(Transect) %>% 
  mutate(invert_richness_sum = sum(invert_richness)) 

colnames(new_data)
# Sum algae_richness across transects

new_data <- new_data %>% 
  group_by(Transect) %>% 
  mutate(algae_richness_sum = sum(algae_richness))

colnames(new_data)

# Sum kelp_richness across quadrats
new_data <- new_data %>% 
  group_by(Transect) %>% 
  mutate(kelp_richness_sum = sum(kelp_richness))

colnames(new_data)

# Sum algal_richness across quadrats
new_data <- new_data %>% 
  group_by(Transect) %>% 
  mutate(algae_richness_sum = sum(algae_richness))

colnames(new_data)

# If any quadrant has kelp_presence > 0, then set kelp_presence at transect level to 1
new_data <- new_data %>% 
  group_by(Transect) %>% 
  mutate(kelp_presence_transect = ifelse(any(kelp_presence == 'y'), 1, 0))

colnames(new_data)

# If any quadrant has urchin_presence > 0, then set kelp_presence at transect level to 1
new_data <- new_data %>% 
  group_by(Transect) %>% 
  mutate(urchin_presence_transect = ifelse(any(urchin_presence == 'y'), 1, 0))

colnames(new_data)

# Create the final dataframe for transect-level summary
transect_summary <- new_data %>% 
  distinct(Transect, .keep_all = TRUE) %>% 
  select(Transect, transect_richness, invert_richness_sum, kelp_richness_sum, kelp_presence_transect, 
         urchin_presence_transect,SubCatTran, SubSubCatTran, algae_richness_sum, Year, Month, Day)

colnames(transect_summary)


#make subcat one columns 
transect_summary <- transect_summary %>%
  mutate(Substrate = paste(SubCatTran, SubSubCatTran, sep = ""))
  
transect_summary$transect_richness <- transect_summary$transect_richness - transect_summary$kelp_richness_sum

colnames(transect_summary)

#write file
write_csv(transect_summary,
          here("datasets", "processed_data",
               "transect.csv")) 


