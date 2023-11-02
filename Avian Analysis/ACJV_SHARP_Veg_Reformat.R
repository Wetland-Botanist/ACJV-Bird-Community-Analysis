#Title: Trustees Vegetation Analysis
#Author: Grant McKown (james.mckown@unh.edu)
#Date Created: September 18, 2023
#Date Last Edited: 


# Purpose: Calculate and visualize descriptive statistics and change over time of vegetation data
# in salt marsh restoration sites of Trustees land in Massachusetts including:
# Kent's Island, Ipswich, Essex, and Old Town Hill





# Chapter 1: Set up Code

#Library & Packages
rm(list = ls())

#Stats & Data Organization Packages
library(tidyr)
library(dplyr)
library(broom)
library(purrr)
library(broom.mixed)
library(modelr)
library(multcomp)
library(MuMIn)

#Data Visualization Packages
library(patchwork)
library(gridExtra)
library(drc)
library(ggfortify)
library(ggforce)
library(ggplot2)
library(viridis)
library(mgcv)
library(ggformula)
library(wesanderson)

#Data Analysis Packages
library(lme4)
library(rstatix)
library(splines2)
library(splines)
library(afex)
library(ggeffects)

set.seed(1010)

getwd()

#Import the SHARP Vegetation CSV

SHARP_Radius <- read.csv("Avian Analysis/Raw_Data/SHARP_Veg_RadiusSurvey_2023.csv") 

SHARP_Radius <- SHARP_Radius %>%
  select(-DomSp7:-Comments) %>%
  arrange(DomSp1, DomSp2, DomSp3, DomSp4, DomSp5, DomSp6)

SHARP_Radius1 <- spread(SHARP_Radius, DomSp1, Sp1Percent)

r.bind
    


