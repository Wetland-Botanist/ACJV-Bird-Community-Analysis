#Title:  Exploratory Analysis of SHARP Point Count Surveys (Regressions)
#Author: Grant McKown (james.mckown@unh.edu)
#Date Created: February 12th, 2024
#Date Last Edited: February 12th, 2024


# Purpose: Calculate univariate statistics of the avian community from SHARP point surveys including:
# (1) Mean +/- Standard Error of Communtiy Richness, Abundance, and Wetland Species

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
library(ggrepel)




#Chapter 2: Load the 0 - 50 m SHARP Point Count Survey, where bird totals are summed across visits for each year

birds <- read.csv("Avian Analysis\\Formatted Datasets\\Bird totals by year within 50 m band.csv") %>%
  select(2:length(.))


#Chapter 3: Abundance of Salty Sparrow Abundance by Runnel Age

birds_run <- filter(birds, Treatment == "Runnel")

runnel_regression_graph <- ggplot(birds_run,
                               aes(x = runnel_age, y = SALS)) +
  geom_point(aes(colour = State),
             size = 6) + 
  geom_text_repel(aes(x = runnel_age - 0.25,
                y = saltysparrow + 0.25,
                label = Site)) + 
  geom_smooth(aes(),
              method = "lm", colour = "black") + 
  scale_y_continuous(limits = c(-2, 30),
                     breaks = seq(0, 30, 5),
                     expand = c(0,0)) + 
  scale_x_continuous(limits = c(-2, 13),
                     breaks = seq(-2, 13, 2),
                     expand = c(0,0)) + 
  labs(y = "Cumulative Salty Sparrows per Season",
       x = element_blank()) +
  theme_bw() +
  theme(
    legend.text = element_text(size = 16, colour = "black"),
    legend.title = element_blank(),
    legend.position = c(0.075, 0.80),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 18, colour = "black"))

runnel_regression_graph
