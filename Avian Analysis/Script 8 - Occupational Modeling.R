#Project: Atlantic Coast Joint Venture - SHARP Analysis
#Title:  Prediction, Occupancy, and Abundance Modelling with 'unmarked' package
#Author: Grant McKown (james.mckown@unh.edu)
#Date Created: March 4th, 2024



#Purpose: 

# (1) Model the prediction, occupancy, and abundance probability of the salt marsh sparrow and 
#    salty sparrows across the entire dataset
# (2) Model the occupancy and abundance probability of the salt marsh sparrow and salty sparrows
#     across vegetation metrics using the 2023 SHARP datasets

rm(list = ls())

# Chapter 1: Set up Code

#Library & Packages
#Stats & Data Organization Packages
library(tidyr)
library(dplyr)
library(vegan)
library(broom)
library(ggeffects)
library(stringr)

#Graphing Packages
library(ggplot2)
library(patchwork)

#Modeling Packages
library(unmarked)
library(auk)
library(MuMIn)

#Chapter 2: Import and format the 50 m SHARP Dataset & 2023 SHARP Vegetation Dataset

# Formatting will include:
# Removal of unwanted sites (Ipswich RUN West, Moody Marsh), Note - Broad Cove NAC is not in the 2023 dataset
# Filter birding dataset to 2023 monitoring
# Creating a 'time elapsed' variable from the Site Date column
# Selecting only the bird, site, and observation variables


#Note, the dadtaset used for occupational modeling is the 50 m SHARP with BOTH site visits in a single season

sparrows <- read.csv("Avian Analysis\\Formatted Datasets\\SHARP Bird 50 m Distance.csv") %>%
  select(-X) %>%
  filter(Year != 2023) %>%
  filter(PointID != "Ipswich RUN West") %>%
  filter(PointID != "Broad Cove" | Treatment != "No Action") %>%
  filter(Site != "Moody Marsh") %>%
  mutate(State = ifelse(str_starts("Broad Cove", Site), "RI", State)) %>%
  mutate(Region = ifelse(State == "RI", "Narragansett Bay", 
                         ifelse (State == "MA", "North Shore Mass", "North Shore Mass"))) %>%
  mutate(survey_elapsed = difftime(time1 = as.POSIXct(SurveyDate, format = '%m/%d/%Y'),
                                   time2 = as.POSIXct('01/01/2023', format = '%m/%d/%Y'),
                                   units = 'days'),
         survey_elapsed = round(as.numeric(survey_elapsed), 1)) %>%
  select(PointID, Year, VisitNum, State, Site, Region, Treatment, 
         SurveyWindow, survey_elapsed, Tide, TempF:Noise, saltysparrow)



#Import the vegetation dataset

veg <- read.csv("Avian Analysis\\Input Data\\SHARP_Veg_2023.csv")

#Merge the Birds and Vegetation Dataset together

# Merging the two datasets by the PointID identifier
# Changing saltysparrow variable from abundance to absence/presence

sparrows_veg <- sparrows %>%
  merge(select(veg, PointID, low_marsh:SCMAR), by = 'PointID') %>%
  select(PointID:saltysparrow, 
         low_marsh, high_marsh, pannes, open_water,
         SPALT_short, SPPAT, DISPI)%>%
  mutate(saltysparrow = ifelse(saltysparrow > 0, 1, 0))


#Chapter 3: Formatting the dataset into the Occu Dataframe Format for Single Season Occupancy Model

# auk package from the Cornell Lab of Ornithology created a neat function that immediately creates
# the proper dataframe. The dataframe contains: 

# (1) site_id --> PointID (site identifier to gather and spread the data)
# (2) response --> saltysparrow (absence/presence of endemic salt marsh sparrows)
# (3) site_covs --> vegetation data (site characteristics that might impact sparrow occupancy)
# (4) obs_covs --> visit data (unique visit characteristics that might impact sparrow detection by observers)


sparrow_occu <- format_unmarked_occu(sparrows_veg,
                                     site_id = 'PointID',
                                     response = 'saltysparrow',
                                     site_covs = c('low_marsh', 'high_marsh', 'pannes', 'open_water',
                                                   'SPALT_short', 'SPPAT', 'DISPI',
                                                   'Treatment', 'Region'),
                                     obs_cov = c('SurveyWindow', 'survey_elapsed', 'Tide', 'TempF', 'Sky', 'WindSp', 'Noise'))

sparrow_occu_data <- formatWide(sparrow_occu, type ='unmarkedFrameOccu')


#Chapter 4: Single Season Occupancy Modeling

# Single Season Occupancy will be broken down into:

# (1) Calculation of overall detection and occupancy probability for the study in 2023
# (2) Occupancy probability modeling for marsh habitats (low marsh, high marsh, pannes, and open water)
# (3) Occupancy probability modeling for marsh vegetation (Short form SPALT, S. patens, and Distichlis spicata)


#Task 1 - Calculation of overall detection and occupancy probability for 2023


occu_model <- occu(formula = ~1
                            ~ 1,
                   data = sparrow_occu_data)

summary(occu_model)

#Back-calculate the Real estimate of occupancy with 95% CI

predict(occu_model,
        newdata = data.frame(site = 1),
        type = 'state')

# Occupancy probability of 77% with a 95% CI of 64% - 87%

# Back-calculate the real estimate of detection with 95% CI

predict(occu_model,
        newdata = data.frame(site = 1),
        type = 'det')

# Detection probability of 68% with a 95% CI of 75% - 93%


# Task 2 - Model selection for observation covariates

#Before proceeding to the marsh habitats and vegetation occupation models, we need to select the proper 
# survey covariates to include in the model. Using the dredge() function of the MuMin package, we will
# test all model combinations of the survey covariates


survey_occu <- occu(formula = ~ Noise
                    ~ Region,
                  data = sparrow_occu_data)

summary(survey_occu)


survey_dredge <- dredge(global.model = survey_occu,
                      rank = 'AICc')

summary(survey_dredge)

survey_dredge[1:5, ]


region <- predict(survey_occu,
        new_data = data.frame(Treatment = c('Reference', 'Runnel', 'No Action')),
        type = 'det') %>%
  cbind(select(sparrow_occu, Treatment)) %>%
  distinct(Treatment, .keep_all = TRUE)



ggplot(region,
       aes(y = Predicted, group = Treatment)) +
  geom_bar(aes(x = Treatment, fill = Treatment),
           stat = 'identity') + 
  geom_errorbar(aes(ymin = lower, ymax = upper, x = Treatment),
                stat = 'identity')


region <- predict(survey_occu,
                  new_data = data.frame(Treatment = c('Reference', 'Runnel', 'No Action'),
                                        Region = c('Narragansett Bay', 'North Shore Mass')),
                  type = 'state') %>%
  cbind(select(sparrow_occu, Treatment, Region)) %>%
  distinct(Treatment, Region, .keep_all = TRUE)



ggplot(region,
       aes(y = Predicted, group = Region)) +
  geom_bar(aes(x = Treatment, fill = Region),
           stat = 'identity', position = position_dodge(0.9)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper, x = Treatment, group = Region),
                stat = 'identity', position = position_dodge(0.9))

#It appears that the null model, with no survey covariates is best describes the 
# the variance in the model. Null model had lowest AIC value. 



# Task 2 - Occupancy modeling for marsh habitats

# In task 1, we just wanted to know the overall occupancy and detection probability of the 2023 dataset without
# considering any site or observation covariates

# In task 2, we will want to understand how selected marsh vegetation habitats - low marsh, high marsh, pannes, 
# and open water, may impact marsh occupancy probability.Additionally, we will include the observation covariates
# for each visit including temperature, noise, rainfall, survey date, personnel, and tidal stage

# Let's look at the model first without considering any observation covariates (detection probability = 77%)

hab_global <- occu(formula = ~ 1
                   ~ Region,
                   data = sparrow_occu_data)

hab_dredge <- dredge(global.model = hab_global,
       rank = 'AICc')

hab_dredge[1:5, ]

modSel(fit)



#Graph up an example of the Occupany Model

SPPAT_predict <- predict(fm,
                         newdata = data.frame(SPPAT = seq(min(site_occu$SPPAT), max(site_occu$SPPAT), 0.1)),
                                              type = "state") %>%
  cbind(data.frame(SPPAT = seq(min(site_occu$SPPAT), max(site_occu$SPPAT), 0.1)))
                        


