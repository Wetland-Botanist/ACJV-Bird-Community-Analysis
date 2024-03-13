#Project: Atlantic Coast Joint Venture - SHARP Analysis
#Title:  Prediction, Occupancy, and Abundance Modelling with 'unmarked' package
#Author: Grant McKown (james.mckown@unh.edu)
#Date Created: March 4th, 2024



#Purpose: 

# (1) Model the prediction, occupancy, and abundance probability of the salt marsh sparrow and 
#    salty sparrows across the entire dataset
# (2) Model the occupancy and abundance probability of the salt marsh sparrow and salty sparrows
#     across vegetation metrics using the 2023 SHARP datasets



# Chapter 1: Set up Code

#Library & Packages
#Stats & Data Organization Packages
library(tidyr)
library(dplyr)
library(vegan)
library(broom)

#Graphing Packages
library(ggplot2)
library(patchwork)

#Modeling Packages
library(unmarked)


#Chapter 2: Import and format the 50 m SHARP Dataset for 'unmarked' package

birds <- read.csv("Avian Analysis\\Formatted Datasets\\SHARP Bird Dataset 50 m Distance.csv") %>%
  select(-X) %>%
  filter(PointID != "Ispswich RUN West")


veg <- read.csv("Avian Analysis\\Input Data\\SHARP_Veg_2023.csv")

#Subset the Dataset to only focus on salt sparrows (Nelson, Salt Marsh, and Sharp Tailed)

birds <- birds %>%
  gather(key = Species, value = Count, totalbirds:YEWA) %>%
  filter(Species == "saltysparrow")



#Chapter 3: Single Season Occupancy Model
#Perform a single season occupancy model for 2023, since we will apply the SHARP vegetation metrics to it

#Format the Species to the proper capture history dataset
  # Each row is a site and each column is a visit (only 2 visits for each site in 2023 - May - June, June - July)
  # Only looking at occupancy, so count numbers will be transformed into binary (0 - absence, 1 - presence)


occu_2023_data <- birds %>%
  filter(Year == 2023) %>%
  arrange(PointID, VisitNum) %>%
  mutate(Count = ifelse(Count > 0, 1, 0)) %>%
  group_by(PointID) %>%
  mutate(Visit1 = Count[1],
         Visit2 = Count[2]) %>%
  distinct(PointID, .keep_all = TRUE)

birds_occu <- occu_2023_data %>%
  select(Visit1, Visit2)

site_occu <- occu_2023_data %>%
  distinct(PointID, .keep_all = TRUE) %>%
  select(PointID, State, Site, Treatment) %>%
  merge(select(veg, -Treatment, -State), by = "PointID")








