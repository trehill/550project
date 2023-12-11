### BIOL 550B Final Project
# Logistic regression GLM
# Author: Eva MacLennan
#Edited by Tessa Rehill 

#to install flexplot; 
#install.packages("devtools") # Devtools is a package which allows to do this.
#devtools::install_github("dustinfife/flexplot")
#install.packages('GGally')

library(flexplot)
library(tidyverse)
library(here)
library(lme4)
library(ggplot2)
library(GGally)


# We'll build a suite of logistic glms to output t/f (kelp presence) based on:
## Fixed effects:
#     urchin presence
#     other invertebrate presence?
#     substrate
#     transect

## Random effects:
#     year
#     transect nested in year (unless we do at transect level?)


# Do some model selection using deltaAIC a la Lab 3

#read in data 
data <- read.csv(here::here("datasets", "processed_data", "transect.csv"),
                 head=TRUE)

# Exploratory (irrelevant)
flexplot(kelp_presence_transect~1, data=data)
flexplot(urchin_presence_transect~1, data=data)


flexplot(kelp_presence_transect~urchin_presence_transect, data=data, family=binomial)

base <- glm(kelp_presence_transect~urchin_presence_transect, data=data, family=binomial)
summary(base)

subs <- glm(kelp_presence_transect~SubCatTran, data=data, family=binomial)
summary(subs)

#Workflow 

#1) determine random effects (REML = TRUE)
    #run with all three random effect structures (ie. only Transect, only Year, and Transect + Year)
    #compare AIC across three structures (most probably Transect + Year has least AIC)
    #chose model with lowest AIC - this will be your random effect structure 
  
    #note- maybew we want to include dates or month?

#2 determien fixed effects (REML = FALSE)
    #evaluate different combinations of potential fixed effects 
    #Compare AIC
    #use model with lowest AIC as best model choice 

#DETERMINING RANDOM EFFECTS ####

#Model 1: Random effect: ONLY Transect 
m1 <- glmer(kelp_presence_transect ~ (1|Transect), 
                        data = data, 
                        family = binomial)
AIC1 <- AIC(m1)

#Model 2: Random effect: ONLY Year 
m2 <- glmer(kelp_presence_transect ~ (1|Year), 
            data = data, 
            family = binomial)

AIC2 <- AIC(m2)

#Model 3: Random effect: ONLY Month
m3 <- glmer(kelp_presence_transect ~  (1|Month), 
            data = data, 
            family = binomial)

AIC3 <- AIC(m3)

#Model 4: Random effect: ONLY Day
m4 <- glmer(kelp_presence_transect ~ (1|Day), 
            data = data, 
            family = binomial)

AIC4 <- AIC(m4)

#Model 5: Random effect: Transect + Year 
m5 <- glmer(kelp_presence_transect ~  (1|Year) + (1|Transect), 
                              data = data, 
                              family = binomial,
                              control = glmerControl(optimizer = "bobyqa"))

AIC5 <- AIC(m5)

#Model 6: random effect: Transect + Month 
m6 <- glmer(kelp_presence_transect ~ (1|Year) + (1|Month), 
            data = data, 
            family = binomial,
            control = glmerControl(optimizer = "bobyqa"))

AIC6 <- AIC(m6)


#DETERMINING FIXED EFFECTS BASED ON BEST RANDOM EFFECT STRUCTURE ####
#based on above the best model has random effects on JUST transect (AIC1), or we can Transect + Month (AIC6)

#lets work on Transect + Month as our base 

#We explore the following fixed effects 
#     urchin presence
#     other invertebrate presence?
#     substrate
#     transect

#going to read in data again so we can clear environment to look at AICs
data <- read.csv(here::here("datasets", "processed_data", "transect.csv"),
                 head=TRUE)

#Model 10: Random effects: Year, Month Fixed effects: urchin presence
m6 <- glmer(kelp_presence_transect ~ urchin_presence_transect + (1|Year) + (1|Month), 
            data = data, 
            family = binomial,
            control = glmerControl(optimizer = "bobyqa"))

#EVAS CODE BELOW ######################

# Playing around with random effects

# Model 1: transect level, ignoring substrate/all quadrat level data, fixed effects: urchin presence

m1 <- glm(kelp_presence_transect~transect_richness, data=data, family=binomial)
AIC1 <- AIC(m1)

# Model 1b: transect level, fixed effects: dominant substrate
m1b <- glm(kelp_presence_transect~transect_richness+SubCatTran+Year, data=data, family=binomial)
AIC1b <- AIC(m1b)

# Model 1c: transect level, fixed effects: trans richness, dominant substrate
m1c <- glm(kelp_presence_transect~transect_richness+Substrate, data=data, family=binomial)
AIC1c <- AIC(m1c)

# Model 1cc: transect level, fixed effects: trans richness, dominant substrate
#              random effect: Year
m1cc <- glmer(kelp_presence_transect~transect_richness+Substrate+(1|Year), data=transdata, family=binomial)
AIC1cc <- AIC(m1cc)

library(flextable)

flexplot(kelp_presence_transect~transect_richness+SubCatTran, data=transdata, family=binomial)

kelptransplot <- flexplot(kelp_presence_transect~transect_richness, data=transdata, family=binomial)
kelptransplot + 
  labs(x = "Transect Richness", y = "Kelp Presence")

kelpinvertplot <- flexplot(kelp_presence_transect~invert_richness_sum, data=transdata, family=binomial)
kelpinvertplot +
  labs(x = "Invertebrate Richness", y = "Kelp Presence")

urchtransplot <- flexplot(urchin_presence_transect~transect_richness, data=transdata, family=binomial)
kelpinvertplot +
  labs(x = "Transect Richness", y = "Urchin Presence")

urchalgplot <- flexplot(urchin_presence_transect~kelp_richness_sum, data=transdata, family=binomial)
urchalgplot +
  labs(x = "Algal Richness", y = "Urchin Presence")

mu <- glm(urchin_presence_transect~transect_richness, data=transdata, family=binomial)
AICmu <- AIC(mu)

flexplot(urchin_presence_transect~transect_richness, data=transdata, family=binomial)

# Model 2: transect level, fixed effects: urchin presence, invert richness

m2 <- glm(kelp_presence_transect~urchin_presence_transect+invert_richness_sum, data=transdata, family=binomial)
AIC2 <- AIC(m2)

# Model 2b: transect level, fixed effects: urchin presence, invert richness, dominant substrate

m2b <- glm(kelp_presence_transect~urchin_presence_transect+invert_richness_sum+SubCatTran, data=transdata, family=binomial)
AIC2b <- AIC(m2b)

# Model 2c: transect level, fixed effects: invert richness, dominant substrate
m2c <- glm(kelp_presence_transect~invert_richness_sum+SubCatTran, data=transdata, family=binomial)
AIC2c <- AIC(m2c)

m2cc <- glm(kelp_presence_transect~invert_richness_sum+Substrate, data=transdata, family=binomial)
AIC2cc <- AIC(m2cc)

# Model 2d: transect level, fixed effects: urchin presence, dominant substrate
m2d <- glm(kelp_presence_transect~urchin_presence_transect+SubCatTran, data=transdata, family=binomial)
AIC2d <- AIC(m2d)


# Model 2e: transect level, fixed effects: urchin presence, invert richness, dominant substrate
#           random effects: year
## still identical to model 2, waiting for dominant substrate term and year data
#m2e <- glm(kelp_presence_transect~urchin_presence_transect+invert_richness_sum, data=transdata, family=binomial)
#AIC2e <- AIC(m2e)

# Model 2f: transect level, fixed effects: urchin presence, invert richness
#           random effects: year
## still identical to model 2, waiting for dominant substrate term and year data
#m2f <- glm(kelp_presence_transect~urchin_presence_transect+invert_richness_sum, data=transdata, family=binomial)
#AIC2f <- AIC(m2f)

# Model 2g: transect level, fixed effects: urchin presence, dominant substrate
#           random effects: year
## still identical to model 2, waiting for dominant substrate term and year data
#m2g <- glm(kelp_presence_transect~urchin_presence_transect+, data=transdata, family=binomial)
#AIC2g <- AIC(m2g)

# Model 2h: transect level, fixed effects: invert richness, dominant substrate
#           random effects: year
## still identical to model 2, waiting for dominant substrate term and year data
#m2h <- glm(kelp_presence_transect~invert_richness_sum, data=transdata, family=binomial)
#AIC2h <- AIC(m2h)


# Model 3: quadrat level, fixed effects: transect, urchin presence

m3 <-  glm(kelp_presence~urchin_presence+Transect, data=data, family=binomial)
AIC3 <- AIC(m3)  

# Model 4: quadrat level, fixed effects: transect, urchin presence, substrate

m4 <- glm(kelp_presence~urchin_presence+Transect+SubCat, data=data, family=binomial)
AIC4 <- AIC(m4)

# Model 5: quadrat level, fixed effects: transect, urchin presence, substrate
#           random effects: year

m5 <- glmer(kelp_presence~urchin_presence, data=data, family=binomial)

# Model 6: quadrat level, fixed effects: urchin presence
#           random effects: transect

m6 <- glmer(kelp_presence~urchin_presence+(1|Transect), data=data, family=binomial)
AIC6 <- AIC(m6)

# Model 7: quadrat level, fixed effects: urchin presence, substrate
#           random effects: transect

m7 <- glmer(kelp_presence~urchin_presence+(1|Transect)+SubCat, data=data, family=binomial)
AIC7 <- AIC(m7)

