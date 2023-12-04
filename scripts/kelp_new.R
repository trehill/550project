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
data <- read.csv(here::here("datasets", "processed_data", "transect_final.csv"),
                 head=TRUE)

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
m1 <- glmer(kelp_presence_transect ~ urchin_presence_transect + avg_depth + Substrate + invert_richness_sum + (1|Transect), 
            data = data, 
            family = binomial)
AIC1 <- AIC(m1)


#Model 2: Random effect: ONLY Year 
m2 <- glmer(kelp_presence_transect ~ urchin_presence_transect + avg_depth + Substrate + invert_richness_sum + (1|Year), 
            data = data, 
            family = binomial)

AIC2 <- AIC(m2)

#Model 3: Random effect: ONLY Month
m3 <- glmer(kelp_presence_transect ~ urchin_presence_transect + avg_depth + Substrate + invert_richness_sum + (1|Month), 
            data = data, 
            family = binomial)

AIC3 <- AIC(m3)

#Model 4: Random effect: ONLY Day
m4 <- glmer(kelp_presence_transect ~ urchin_presence_transect + avg_depth + Substrate + invert_richness_sum + (1|Day), 
            data = data, 
            family = binomial)

AIC4 <- AIC(m4)

#Model 5: Random effect: Transect + Year 
m5 <- glmer(kelp_presence_transect ~ urchin_presence_transect + avg_depth + Substrate + invert_richness_sum + (1|Year) + (1|Transect), 
            data = data, 
            family = binomial)

AIC5 <- AIC(m5)

#Model 6: random effect: Transect + Month 
m6 <- glmer(kelp_presence_transect ~ urchin_presence_transect + avg_depth + Substrate + invert_richness_sum +(1|Transect) + (1|Month), 
            data = data, 
            family = binomial)

AIC6 <- AIC(m6)

#Model 7: random effect: Transect + Day 
m7 <- glmer(kelp_presence_transect ~ urchin_presence_transect + avg_depth + Substrate + invert_richness_sum +(1|Transect) + (1|Day), 
            data = data, 
            family = binomial)

AIC7 <- AIC(m7)

#Model 8: random effect: Year Month
m8 <- glmer(kelp_presence_transect ~ urchin_presence_transect + avg_depth + Substrate + invert_richness_sum +(1|Year) + (1|Month), 
            data = data, 
            family = binomial)

AIC8 <- AIC(m8)

#Model 9: random effect: Year Month
m9 <- glmer(kelp_presence_transect ~ urchin_presence_transect + avg_depth + Substrate + invert_richness_sum +(1|Year) + (1|Day), 
            data = data, 
            family = binomial)

AIC9 <- AIC(m9)


#from this- the model with random effects with the least AIC is model 2 - ONLY Year

#DETERMINING FIXED EFFECTS BASED ON BEST RANDOM EFFECT STRUCTURE ####

#lets work on Transect only as our base 

#We explore the following fixed effects 
#     urchin presence
#     other invertebrate presence?
#     substrate
#     depth

#going to read in data again so we can clear environment to look at AICs
#data <- read.csv(here::here("datasets", "processed_data", "transect.csv"),
#                 head=TRUE)
#on Eva's computer
#data <- read.csv('transectfinal.csv')


#Model 10: Random effects: Trasect Transect Transect Fixed effects: urchin presence
m10 <- glmer(kelp_presence_transect ~ urchin_presence_transect + (1|Year), 
            data = data, 
            family = binomial)

AIC10 <- AIC(m10)


#Model 11: Random effects: Transect Fixed effects: invertebrate richness
m11 <- glmer(kelp_presence_transect ~ invert_richness_sum + (1|Year), 
            data = data, 
            family = binomial)

AIC11 <- AIC(m11)

#Model 12: Random effects: Transect Fixed effects: substrate
m12 <- glmer(kelp_presence_transect ~ Substrate + (1|Year), 
            data = data, 
            family = binomial,
            control = glmerControl(optimizer = "bobyqa"))


AIC12 <- AIC(m12)

#Model 13: Random effects: Transect Fixed effects: urchin presence, invert richness
m13 <- glmer(kelp_presence_transect ~ urchin_presence_transect + invert_richness_sum + (1|Year), 
             data = data, 
             family = binomial)

AIC13 <- AIC(m13)

#Model 14: Random effects: Transect Fixed effects: urchin presence, substrate
m14 <- glmer(kelp_presence_transect ~ urchin_presence_transect + Substrate + (1|Year), 
             data = data, 
             family = binomial, 
             control = glmerControl(optimizer = "bobyqa"))

AIC14 <- AIC(m14)

#Model 15: Random effects: Transect, Fixed effects: invert richness, substrate
m15 <- glmer(kelp_presence_transect ~ invert_richness_sum + Substrate + (1|Year), 
             data = data, 
             family = binomial)

AIC15 <- AIC(m15)

#Model 16: Random: Transect, Fixed: urchin, depth
m16 <- glmer(kelp_presence_transect ~ urchin_presence_transect + avg_depth +(1|Year), 
             data = data, 
             family = binomial)

AIC16 <- AIC(m16)

#Model 17: Random: Transect, Fixed: urchin, depth, substrate
m17 <- glmer(kelp_presence_transect ~ urchin_presence_transect + avg_depth + Substrate + (1|Year), 
             data = data, 
             family = binomial, 
             control = glmerControl(optimizer = "Nelder_Mead"))

AIC17 <- AIC(m17)

#Model 18: Random: Transect, Fixed: urchin, depth, substrate,invert
m18 <- glmer(kelp_presence_transect ~ urchin_presence_transect + avg_depth + Substrate + invert_richness_sum + (1|Year), 
             data = data, 
             family = binomial, 
             control = glmerControl(optimizer = "bobyqa"))

AIC18 <- AIC(m18)


#Plots
install.packages('sjPlot')
install.packages('sjlabelled')
install.packages('sjmisc')

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

#Model (not sure if were supposed to plot the model with lowest AIC or a model of all possible effects?)
m <- glmer(kelp_presence_transect ~ urchin_presence_transect + invert_richness_sum + Substrate + avg_depth + (1|Year), 
           data = data, 
           family = binomial)
m

plot_model(m, sort.est=TRUE, vline.color = "black", title = "Kelp Presence")

library(jtools)
plot_summs(m7) #i think this is right

