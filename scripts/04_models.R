#exploring mixed effect models 
#author: Tessa Rehill

#install.packages("devtools") # Devtools is a package which allows to do this.
#devtools::install_github("dustinfife/flexplot")

#install.packages("lme4")
#install.packages("Matrix")

library(flexplot)
library(lme4)

#read in data 
data <- read.csv(here::here("550project", "datasets", "processed_data"),
                 head=TRUE)

key <- read.csv(here::here("550project", "datasets",
                           "full_key_final.csv"), #from report 
                head=TRUE)



#fit a baseline model 
baseline = lmer(MathAch~1+ (1|School), data=math) #MathAch is a numerical value

icc(baseline)
