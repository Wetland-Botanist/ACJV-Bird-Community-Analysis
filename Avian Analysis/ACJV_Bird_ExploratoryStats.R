#Title:  Exploratory Analysis of SHARP Point Count Surveys (Bird Totals and Community Richness)
#Author: Grant McKown (james.mckown@unh.edu)
#Date Created: February 12th, 2024
#Date Last Edited: February 12th, 2024


# Purpose: Calculate univariate statistics of the avian community from SHARP point surveys including:
# (1) Mean +/- Standard Error of Community Richness, Abundance, and Diversity

# Chapter 1: Set up Code

#Library & Packages
#Stats & Data Organization Packages
library(tidyr)
library(dplyr)
library(vegan)
library(broom)
library(lme4)
library(afex)
library(emmeans)
library(ggResidpanel)


#Graphing Packages
library(ggplot2)
library(patchwork)




#Chapter 2: Load the 0 - 50 m SHARP Point Count Survey

birds <- read.csv("Avian Analysis\\Formatted Datasets\\SHARP Bird Dataset 50 m Distance.csv") %>%
  select(2:length(.))


#Chapter 3: Calculation of Abundance, Richness, Diversity, and Sparrow Count for Annual Count, SHARP Point
# Shannon's Diversity will be calculated for the diversity metric
# Sparrow count is already calculated as the summation of Saltmarsh, Nelson, and Sharptail sparrows

# Importantly, the code is the MEAN of the two site visits per year
birds_annual_mean <- birds %>%
  group_by(PointID, State, Site, Treatment, Year, runnel_age, Point_X, Point_Y) %>%
  summarise(across(totalbirds:YEWA, ~mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(
    richness = specnumber(select(., AGWT:YEWA) > 0),
    shannon_div = round(diversity(select(., AGWT:YEWA), "shannon"), 2))

write.csv(birds_annual_mean,
          "Avian Analysis\\Formatted Datasets\\Bird totals MEAN by year within 50 m band.csv")


#Chapter 4: Calculate Descriptive Stats of selected avian metrics for Site Visits 

birds_treatment_stats <- birds_annual_mean %>%
  group_by(Treatment, Year) %>%
  summarise(
    totalbirds.m = mean(totalbirds, na.rm = TRUE),
    richness.m = mean(richness, na.rm = TRUE),
    shannon.m = mean(shannon_div, na.rm = TRUE),
    saltysparrow.m = mean(saltysparrow, na.rm = TRUE),
    totalsparrow.m = mean(totalsparrow, na.rm = TRUE),
    SALS.m = mean(SALS, na.rm = TRUE),
    Count = n(),
    
    totalbirds.sd = sd(totalbirds, na.rm = TRUE)/sqrt(n()),
    richness.sd = sd(richness, na.rm = TRUE)/sqrt(n()),
    shannon.sd = sd(shannon_div, na.rm = TRUE)/sqrt(n()),
    saltysparrow.sd = sd(saltysparrow, na.rm = TRUE)/sqrt(n()),
    totalsparrow.sd = sd(totalsparrow, na.rm = TRUE)/sqrt(n()),
    SALS.sd = sd(SALS, na.rm = TRUE)/sqrt(n())) %>%
  mutate(across(totalbirds.m:SALS.sd,
         ~round(., 2))) %>%
  ungroup()

write.csv(birds_treatment_stats,
          "Avian Analysis\\Formatted Datasets\\Bird Descriptive Stats (MEAN) by Treatment - Year.csv")


#Chapter 5: Graph the Descriptive Stats of the Avian Metrics

# I want to do 2 graphs: Total Birds - Salty Sparrows & Richneess - Shannon Diversity

#Step 1: Prep the dataset by gathering the mean and standard error values

birds_stats_grouped <- birds_treatment_stats %>%
  select(Treatment, Year, totalbirds.m:SALS.m) %>%
  gather(key = "Metric", "Mean", totalbirds.m:SALS.m)

birds_stats_sd <- birds_treatment_stats %>%
  select(Treatment, Year, totalbirds.sd:SALS.sd) %>%
  gather(key = "Metric", "SD", totalbirds.sd:SALS.sd)

birds_stats_grouped <- cbind(birds_stats_grouped,
                               select(birds_stats_sd, SD)) %>%
  select(Treatment, Year, Metric, Mean, SD) %>%
  mutate(
    Metric = ifelse(Metric == "totalbirds.m", "Total Birds", Metric),
    Metric = ifelse(Metric == "richness.m", "Bird Richness", Metric),
    Metric = ifelse(Metric == "shannon.m", "Shannon Diversity", Metric),
    Metric = ifelse(Metric == "saltysparrow.m", "Salty Sparrows", Metric),
    Metric = ifelse(Metric == "SALS.m", "Saltmarsh Sparrow", Metric))


rm(birds_stats_sd)


# Step 2: Graph of the Total Bird counts and Salty Sparrows

birds_number_graph <- ggplot(filter(birds_stats_grouped, 
                                    Metric == "Total Birds" | Metric == "Salty Sparrows"),
                             aes(x = Year, y = Mean, group = Metric)) +
  geom_bar(aes(fill = Metric),
           stat = 'identity', position = position_dodge(0.9),
           size = 1.25, colour = "black") +
  geom_errorbar(aes(x = Year, 
                    ymax = Mean + SD, 
                    ymin = Mean - SD),
                position = position_dodge(0.9),
                width = 0.5, size = 1.0) + 
  scale_y_continuous(limits = c(0, 14),
                     breaks = seq(0, 14, 2),
                     expand = c(0,0)) +
  labs(y = "Number of Birds per Season",
       x = element_blank()) +
  theme_bw() +
  theme(
    legend.position = c(0.10, 0.85),
    legend.title = element_blank(),
    legend.text = element_text(size = 16, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 18, colour = "black"),
    strip.text = element_text(size = 20, colour = "black"),
    strip.background = element_blank()) +
  facet_wrap(~Treatment)

birds_number_graph


# Step 3: Graph of the Total Bird counts and Salty Sparrows

birds_richness_graph <- ggplot(filter(birds_stats_grouped, 
                                    Metric == "Bird Richness" | Metric == "Shannon Diversity"),
                             aes(x = Year, y = Mean, group = Metric)) +
  geom_bar(aes(fill = Metric),
           stat = 'identity', position = position_dodge(0.9),
           size = 1.25, colour = "black") +
  geom_errorbar(aes(x = Year, 
                    ymax = Mean + SD, 
                    ymin = Mean - SD),
                position = position_dodge(0.9),
                width = 0.5, size = 1.0) + 
  scale_y_continuous(limits = c(0, 8),
                     breaks = seq(0, 8, 1),
                     expand = c(0,0)) +
  labs(y = "Number of Birds per Season",
       x = element_blank()) +
  theme_bw() +
  theme(
    legend.position = c(0.15, 0.90),
    legend.title = element_blank(),
    legend.text = element_text(size = 16, colour = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size = 18, colour = "black"),
    axis.text = element_text(size = 18, colour = "black"),
    strip.text = element_text(size = 20, colour = "black"),
    strip.background = element_blank()) +
  facet_wrap(~Treatment)

birds_richness_graph


#Step 4: Combine the number and richness graphs into an exploratory graph of the bird community

community_exploratory_graph = birds_number_graph / birds_richness_graph

community_exploratory_graph


ggsave(community_exploratory_graph,
       filename = "Avian Analysis\\Figures\\Bird Community by Treatment and Year.jpg",
       dpi = 300, units = "in", limitsize = FALSE,
       width = 14, height = 10)



#Chapter 5: Two-way ANOVA of Bird Metrics (Treatment x Year)

#Conducting a two-way ANOVA with Treatment, Year as fixed effects, PointID as a random effect (repeated sampling)


#Prep the annual mean bird dataset for the model

birds_data <- birds_annual_mean %>%
  dplyr::select(Site, PointID, Treatment, Year, richness, shannon_div, saltysparrow, totalbirds) %>%
  mutate(Year = as.factor(Year))

glimpse(birds_data)


#Analysis 1: Mixed Two-way ANOVA for 'saltysparrow' metric

#Before final version of R code, I checked the residual plots of 'saltysparrow' and found 
# a square-transformation was required

salty <- lmer(sqrt(saltysparrow) ~ Treatment * Year + (1|Site/PointID),
              data = birds_data)

#Residual plots of the model -- checking assumptions of ANOVA
resid_panel(salty)

#Returning the ANOVA table for the model
salty_anova <- tidy(anova(salty))

salty_anova

#Returning the Fixed and Random Effects table of the model
salty_tidy <- broom.mixed::tidy(salty)

salty_tidy


#Post-hoc analysis between just Years for the model
posthoc <- tidy(emmeans(salty, specs = pairwise ~ Year)$contrasts)

posthoc





#Analysis 2: Mixed Two-way ANOVA for 'totalbirds' metric

#Before final version of R code, I checked the residual plots of 'totalbirds' and found 
# a square-transformation was required

totalbirds <- lmer(totalbirds ~ Treatment * Year + (1|Site/PointID),
              data = birds_data)

#Residual plots of the model -- checking assumptions of ANOVA
resid_panel(totalbirds)

#Returning the ANOVA table for the model
totalbirds_anova <- tidy(anova(totalbirds))

totalbirds_anova

#Returning the Fixed and Random Effects table of the model
totalbirds_tidy <- broom.mixed::tidy(totalbirds)

totalbirds_tidy


#Post-hoc analysis between just Years for the model
posthoc <- tidy(emmeans(totalbirds, specs = pairwise ~ Year)$contrasts)

posthoc


#Analysis 3: Mixed Two-way ANOVA for 'richness' metric

#Before final version of R code, I checked the residual plots of 'saltysparrow' and found 
# a square-transformation was required

richness <- lmer(richness ~ Treatment * Year + (1|Site/PointID),
              data = birds_data)

#Residual plots of the model -- checking assumptions of ANOVA
resid_panel(richness)

#Returning the ANOVA table for the model
richness_anova <- tidy(anova(richness))

richness_anova

#Returning the Fixed and Random Effects table of the model
richness_tidy <- broom.mixed::tidy(richness)

richness_tidy


#No post-hoc test required, since there were no significant terms




#Analysis 4: Mixed Two-way ANOVA for 'shannon_div' metric

#Before final version of R code, I checked the residual plots of 'shannon_div' and found 
# a square-transformation was required

shannon_div <- lmer(shannon_div ~ Treatment * Year + (1|Site/PointID),
                 data = birds_data)

#Residual plots of the model -- checking assumptions of ANOVA
resid_panel(shannon_div)

#Returning the ANOVA table for the model
shannon_div_anova <- tidy(anova(shannon_div))

shannon_div_anova

#Returning the Fixed and Random Effects table of the model
shannon_div_tidy <- broom.mixed::tidy(shannon_div)

shannon_div_tidy


#No post-hoc test required, since there were no significant terms















  
