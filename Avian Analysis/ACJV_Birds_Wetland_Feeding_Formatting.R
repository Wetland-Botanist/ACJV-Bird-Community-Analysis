#Title: Avian Community Data Formatting within 50m (Wetland Score and Feeding Guild)
#Author: Grant McKown (james.mckown@unh.edu)
#Date Created: February 14th, 2024
#Date Last Edited: February 14th, 2024


# Purpose: Combine the separate SHARP datasets over 2021 - 2023 for exploratory, multivariate, and modeling analysis

# Chapter 1: Set up Code

#Library & Packages
rm(list = ls())

#Stats & Data Organization Packages
library(tidyr)
library(dplyr)


#Chapter 2: Import Bird Dataset & Determine Species List for Dataset

birds <- read.csv("Avian Analysis\\Formatted Datasets\\SHARP Bird Dataset 50 m Distance.csv")

bird_species <- birds %>%
  gather(key = AlphaCode, value = "Count", totalbirds:YEWA) %>%
  group_by(PointID) %>%
  filter(Count != 0) %>%
  ungroup() %>%
  distinct(AlphaCode)

write.csv(bird_species,
          "Avian Analysis\\Formatted Datasets\\Bird Species List at 50m band.csv")


#Chapter 3: Import the Bird Species List with Wetland Score and Feeding Guild

#After the bird species list was generated, I compiled the feeding guilds and wetland scores for each bird species

#Feeding Guild:
# Trophic guilds were derived from Shriver 2015 Masters Thesis based on Graaf et al. 1985
# There are 7 feeding guilds
# Grant McKown created on additional feeding guild to characterize upland edge foragers and gleaners

#Wetland Score:
# Each species was assigned a wetland habitat dependency based on Croonquist et al. 1996
# All species were native to Pennsylvania except several. Wetland scores were assigned by Grant McKown based
# on other similar species and external sources. 
#Wetland dependencies are 0, 1, 3, and 5 with 0 being no dependence and 5 being wetland obligates

bird_species <- read.csv("Avian Analysis\\Input Data\\Bird_Species_50m.csv")

birds <- birds %>%
  dplyr::select(-totalbirds, -saltysparrow, -totalsparrow) %>%
  gather(key = "AlphaCode", value = "Count", AGWT:YEWA) %>%
  merge(., 
        dplyr::select(bird_species, AlphaCode, Feeding.Guild.Code, Wetland.Score),
        by = "AlphaCode") %>%
  rename(wetland_score = Wetland.Score,
         feeding_guild = Feeding.Guild.Code) %>%
  dplyr::select(PointID, Site, State, RegionNum, Treatment, VisitNum, runnel_age, Year, Point_X:Site_Date, 
         DistBand, AlphaCode, Count, feeding_guild, wetland_score)

write.csv(birds,
          "Avian Analysis\\Formatted Datasets\\SHARP Bird Dataset 50m Wetland and Feeding Scores.csv")




