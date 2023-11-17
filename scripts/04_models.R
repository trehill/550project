#Exploring mixed effect models 
#author: Tessa Rehill

#following along this tutorial:
#https://www.youtube.com/watch?v=Wtk5iZ65XHk&list=PL8F480DgtpW9_IT7xN1XeRF_dglZmK0nM&index=4&ab_channel=QuantPsych

#install.packages("devtools") # Devtools is a package which allows to do this.
#devtools::install_github("dustinfife/flexplot")

#install.packages("lme4")
#install.packages("Matrix")

library(flexplot)
library(lme4)

#read in data 
data <- read.csv(here::here("BIOL550", "550project", "datasets", "processed_data", "processed_data.csv"),
                 head=TRUE)

#let's select only the columns that we are interested in ('get rid of presence/absence matrix)
#select all columns except columns 19 through 145
selected_columns <- data[, c(1:18, 146:152)]

#create a new dataframe with the selected columns
df <- data.frame(selected_columns)
colnames(df)

key <- read.csv(here::here("BIOL550", "550project", "datasets", "keys",
                           "full_key_final.csv"), #from report 
                head=TRUE)

#workflow: 
#need to determine fixed and random effects based on data structure, theory + model comparisons

#data structure: 
  #cluster variable = Transect 
  #transect_richness NEVER random effect (stays constant across transects)

#Looking at total species richness (and urchin presence) at the quadrat level ####

#fit a baseline model 
#baseline model (random effect ANOVA), no predictors
#fixed and random slope 

baseline = lmer(quadrat_richness~1+ (1|Transect), data=df) 

#compute ICC
icc(baseline)
  #24% of the variance can be explained by clustering within transect
  #indicates that using mixed models in important here 

#visualize baseline model 
visualize(baseline)

#fit reduced model - urchin presence effect on richness
fixed_slopes = lmer(quadrat_richness~urchin_presence+ (1|Transect), data=df) 
random_slopes = lmer(quadrat_richness~urchin_presence+ (urchin_presence|Transect), data=df) 

visualize(random_slopes, plot="model") #typically we dont add random effects of categorical variables

#model comparison
compare.fits(quadrat_richness~urchin_presence|Transect, data=df, fixed_slopes, random_slopes)

model.comparison(fixed_slopes, baseline) 
model.comparison(random_slopes, baseline) 
model.comparison(fixed_slopes, random_slopes) #fixed slope is better


#Looking at algae richness (and invert richness) at the quadrat level ####

#algal richness to invert richness
baseline = lmer(algae_richness~1+ (1|Transect), data=df) 
icc(baseline) #26%
visualize(baseline)
#fit reduced model 
fixed_slopes = lmer(algae_richness~invert_richness+ (1|Transect), data=df) 
random_slopes = lmer(algae_richness~invert_richness+ (invert_richness|Transect), data=df) 

visualize(random_slopes, plot="model") 

#compare models
compare.fits(algae_richness~invert_richness|Transect, data=df, fixed_slopes, random_slopes)

model.comparison(fixed_slopes, baseline) 
model.comparison(random_slopes, baseline) 
model.comparison(fixed_slopes, random_slopes) #very comparable 


###
#practice from tutorial 

baseline = lmer(MathAch~1+ (1|School), data=math) 
icc(baseline)
visualize(baseline)

#fit reduced model 
fixed_slopes = lmer(MathAch~SES+ (1|School), data=math) 
random_slopes = lmer(MathAch~SES+ (SES|School), data=math) 

#visualize 
visualize(random_slopes, plot="model")

#compare 
compare.fits(MathAch~SES|School, data=math, fixed_slopes, random_slopes)

