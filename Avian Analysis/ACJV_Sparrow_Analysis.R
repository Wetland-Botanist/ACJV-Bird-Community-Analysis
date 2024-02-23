#Title:  Exploratory and Statistical Analysis of Sparrows at SHARP Points (Exploratory and Statistical Analysis)
#Author: Grant McKown (james.mckown@unh.edu)
#Date Created: February 13th, 2024
#Date Last Edited: February 13th, 2024


# Purpose: Describe and calculate the mean count of sparrows observed at the SHARP Points

# Chapter 1: Set up Code

rm(list = ls())

#Library & Packages
#Stats & Data Organization Packages
library(tidyr)
library(dplyr)
library(nlme)
library(broom)

#Graphing Packages
library(ggplot2)
library(patchwork)
library(ggrepel)
library(viridis)
library(pals)


#Chapter 2: Import necessary datasets

#Import the 50 m distance dataset

birds <- read.csv("Avian Analysis\\Formatted Datasets\\SHARP Bird Dataset 50 m Distance.csv") %>%
  select(-X) 

colnames(birds)

#Remember, saltysparrow metric is the sum of Saltmarsh, Nelson, SharpTail, and Nelsons sparrows




#Chapter 3: Calculate descriptive stats for each Sharp Point and Treatment

#Calculate the descriptive statistics of saltysparrow metric


sparrows_point <- birds %>%
  mutate(saltysparrow_percent = (saltysparrow/totalbirds)* 100) %>%
  group_by(Site, State, Treatment, Year, runnel_age) %>%
  summarise(across(c(totalbirds, saltysparrow, saltysparrow_percent), 
                   list(
                     m = ~mean(., na.rm = TRUE),
                     se = ~sd(., na.rm = TRUE)/sqrt(n())
                     ))) %>%
  ungroup()
  mutate(across(totalbirds_m:saltysparrow_percent_se, 
         ~round(., 1)))

write.csv(sparrows_point,
          "Avian Analysis\\Output Stats\\Sparrow 50 m Dataset per SHARP Point.csv")  


# Calculate the descriptive stats for each treatment over time using the newly created sparrows_point dataset

sparrows_treatment <- sparrows_point %>%
  group_by(Treatment, Year) %>%
  summarise(across(c(totalbirds_m, saltysparrow_m, saltysparrow_percent_m), 
                   list(
                     ~mean(., na.rm = TRUE),
                     se = ~sd(., na.rm = TRUE)/sqrt(n())
                   ))) %>%
  ungroup() %>%
    rename(totalbirds.m = "totalbirds_m_1",
           totalbirds.se = "totalbirds_m_se",
           saltysparrow.m = "saltysparrow_m_1",
           saltysparrow.se = "saltysparrow_m_se",
           saltysparrow_percent.m = "saltysparrow_percent_m_1",
           saltysparrow_percent.se = "saltysparrow_percent_m_se") %>%
  mutate(across(totalbirds.m:saltysparrow_percent.se, 
                ~round(., 1)))

write.csv(sparrows_treatment,
          "Avian Analysis\\Output Stats\\Sparrow 50 m Dataset per Treatment.csv")  





#Chapter 4: Graph the descriptive statistics of the treatments in two graphs (bar graphs)

#Graph 1: The total number of birds and the salty sparrows

#First, we need to tweak the dataset to make the graph easy 


#Next, time to make a 3 panel graph using the facetwrap function with ggplot()

sparrows_graph <- ggplot(sparrows_treatment,
                            aes(x = Year, 
                                y = saltysparrow.m, 
                                group = Treatment)) + 
  geom_bar(aes(fill = Treatment),
           stat = 'identity', 
           position = position_dodge(0.9),
           size = 1.25, colour = "black") +
  geom_errorbar(aes(x = Year, 
                    ymax = saltysparrow.m + saltysparrow.se, 
                    ymin = saltysparrow.m - saltysparrow.se),
                position = position_dodge(0.9),
                width = 0.5, size = 1.0) + 
  scale_y_continuous(limits = c(0, 6),
                     breaks = seq(0, 6, 1),
                     expand = c(0,0)) +
  labs(y = "Mean Count per Sharp Point", 
       x = "") + 
  theme_bw() +
  theme(
    legend.position = c(0.10, 0.80),
    legend.title = element_blank(),
    legend.text = element_text(size = 15, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 20, colour = "black"),
    axis.text = element_text(size = 20, colour = "black"),
    strip.text = element_text(size = 16, colour = "black"),
    strip.background = element_blank())

sparrows_graph


#Graph 2: Percent of Sparrows for the Bird Community

sparrows_percent_graph <- ggplot(sparrows_treatment,
                         aes(x = Year, 
                             y = saltysparrow_percent.m, 
                             group = Treatment)) + 
  geom_bar(aes(fill = Treatment),
           stat = 'identity', 
           position = position_dodge(0.9),
           size = 1.25, colour = "black") +
  geom_errorbar(aes(x = Year, 
                    ymax = saltysparrow_percent.m + saltysparrow_percent.se, 
                    ymin = saltysparrow_percent.m - saltysparrow_percent.se),
                position = position_dodge(0.9),
                width = 0.5, size = 1.0) + 
  scale_y_continuous(limits = c(0, 50),
                     breaks = seq(0, 50, 10),
                     expand = c(0,0)) +
  labs(y = "Percent of Bird Community", 
       x = "") + 
  theme_bw() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 15, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 20, colour = "black"),
    axis.text = element_text(size = 20, colour = "black"),
    strip.text = element_text(size = 16, colour = "black"),
    strip.background = element_blank())

sparrows_percent_graph



sparrows_overall_graph <- sparrows_graph / sparrows_percent_graph

sparrows_overall_graph  


ggsave(sparrows_overall_graph,
       filename = "Avian Analysis\\Figures\\Sparrow Graph.jpg",
       dpi = 300, units = "in", limitsize = FALSE,
       height = 10, width = 12)

  
  
  
  