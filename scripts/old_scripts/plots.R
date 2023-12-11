#Plots 
install.packages('ggmosaic')

library(readr)
library(ggplot2)
library(ggmosaic)

#read in data 
data <- read.csv(here::here("550project", "datasets", "processed_data", "processed_data.csv"),
                 head=TRUE)

###
# input data
library(readr)
titanic <- read_csv("titanic.csv")

# create a table
tbl <- xtabs(~Survived + Class + Sex, titanic)
ftable(tbl)


#confusion matrix / mosaic plot 

#x axis - kelp presence, y-axis - species richness 

#x axis - urchin presence, y-axis - species richness 


#from Mark: https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html



