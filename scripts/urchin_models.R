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


#read in data 
data <- read.csv(here::here("datasets", "processed_data", "transect_final.csv"),
                 head=TRUE)

#DETERMINING RANDOM EFFECTS ####

#Model 1: Random effect: ONLY Transect 
m1 <- glmer(urchin_presence_transect ~ (1|Transect), 
            data = data, 
            family = binomial)
AIC1 <- AIC(m1)

#Model 2: Random effect: ONLY Year 
m2 <- glmer(urchin_presence_transect ~ (1|Year), 
            data = data, 
            family = binomial)

AIC2 <- AIC(m2)

#Model 3: Random effect: ONLY Month
m3 <- glmer(urchin_presence_transect ~  (1|Month), 
            data = data, 
            family = binomial)

AIC3 <- AIC(m3)

#Model 4: Random effect: ONLY Day
m4 <- glmer(urchin_presence_transect ~ (1|Day), 
            data = data, 
            family = binomial)

AIC4 <- AIC(m4)

#Model 5: Random effect: Transect + Year 
m5 <- glmer(urchin_presence_transect ~  (1|Year) + (1|Transect), 
            data = data, 
            family = binomial)

AIC5 <- AIC(m5)

#Model 6: random effect: Year + Month 
m6 <- glmer(urchin_presence_transect ~ (1|Year) + (1|Month), 
            data = data, 
            family = binomial)

AIC6 <- AIC(m6)

#Model 7: random effect: Year + Month 
m7 <- glmer(urchin_presence_transect ~ (1|Transect) + (1|Month), 
            data = data, 
            family = binomial)

AIC7 <- AIC(m7)

#from this- the model with random effects with Transect + Month (AIC6)

#DETERMINING FIXED EFFECTS BASED ON BEST RANDOM EFFECT STRUCTURE ####

#Model 7: Random effects: Transect, Month Fixed: Kelp
m7 <- glmer(urchin_presence_transect ~ kelp_presence_transect + (1|Transect) + (1|Month), 
            data = data, 
            family = binomial)

AIC7 <- AIC(m7)


#Model 8: Random effects: Transect, Month Fixed effects: invertebrate richness
m8 <- glmer(urchin_presence_transect ~ invert_richness_sum + (1|Transect) + (1|Month), 
            data = data, 
            family = binomial)

AIC8 <- AIC(m8)

#Model 9: Random effects: Transect, Month Fixed effects: substrate
m9 <- glmer(urchin_presence_transect ~ Substrate + (1|Transect)  + (1|Month), 
            data = data, 
            family = binomial)

AIC9 <- AIC(m9)

#Model 10: Random effects: Transect, Month Fixed effects: kelp presence, invert richness
m10 <- glmer(urchin_presence_transect ~ kelp_presence_transect + invert_richness_sum + (1|Transect) + (1|Month), 
             data = data, 
             family = binomial)

AIC10 <- AIC(m10)

#Model 11: Random effects: Transect, Month Fixed effects: kelp presence, substrate
m11 <- glmer(urchin_presence_transect ~ kelp_presence_transect + Substrate + (1|Transect)+ (1|Month), 
             data = data, 
             family = binomial)

AIC11 <- AIC(m11)

#Model 12: Random effects: Transect, Month Fixed effects: invert richness, substrate
m12 <- glmer(urchin_presence_transect ~ invert_richness_sum + Substrate + (1|Transect)+ (1|Month), 
             data = data, 
             family = binomial)

AIC12 <- AIC(m12)

#Model 13: Random: Transect,Month, Fixed: kelp, depth
m13 <- glmer(urchin_presence_transect ~ kelp_presence_transect + avg_depth +(1|Transect)+ (1|Month), 
             data = data, 
             family = binomial)

AIC13 <- AIC(m13)

#Model 14: Random: Transect,Month, Fixed: kelp, depth, substrate
m14 <- glmer(urchin_presence_transect ~ kelp_presence_transect + avg_depth + Substrate + (1|Transect) + (1|Month), 
             data = data, 
             family = binomial)

AIC14 <- AIC(m14)

#Model 15: Random: Transect, Fixed: kelp, depth, substrate,invert
m15 <- glmer(urchin_presence_transect ~ kelp_presence_transect + avg_depth + Substrate + invert_richness_sum + (1|Transect)  + (1|Month), 
             data = data, 
             family = binomial)

AIC15 <- AIC(m15)


#lowest AIC = AIC 15 = kelp, depth, substrate, invert richness
#Plots
install.packages('sjPlot')
install.packages('sjlabelled')
install.packages('sjmisc')

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)



#Model (not sure if were supposed to plot the model with lowest AIC or a model of all possible effects?)
# Assuming your data frame is named 'data'
n <- glmer(urchin_presence_transect ~ kelp_presence_transect + invert_richness_sum + Substrate + avg_depth + (1|Transect)+(1|Month), 
           data = data, 
           family = binomial)
n

plot_model(n, sort.est=TRUE, vline.color = "black", title = "Urchin Presence")

library(jtools)
plot_summs(m15) #i think this is right


