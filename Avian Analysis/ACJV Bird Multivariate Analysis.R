#Title:  Multivariate Analysis of ACJV Avian Community
#Author: Grant McKown (james.mckown@unh.edu)
#Date Created: February 13th, 2024
#Date Last Edited: February 13th, 2024


# Purpose: 

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
library(viridis)

#Chapter 2: Import the Birding Dataset at 50 m with Complete Distance Bands, Add Wetland Score and Feeding Guild

birds <- read.csv("Avian Analysis\\Formatted Datasets\\SHARP Bird Dataset 50 m Distance.csv") %>%
  select(-X) 

# In order to complete the NMDS, I want to compile the wetland scores and feeding guild preferences of each species

#In the code below, I reduce the 50 m dataset to just the bird species observations, remove all species with 0 counts,
# then keep only a distinct list of species that were ACTUALLY observed within 50 m across all site visits

birds_species <- birds %>%
  select(AGWT:YEWA) %>%
  gather(key = AlphaCode, value = Count, ) %>%
  filter(Count != 0) %>%
  distinct(AlphaCode)

write.csv(birds_species,
          "Avian Analysis\\Formatted Datasets\\Bird Species List at 50 m.csv")


# In the meantime, I compiled the wetland score and feeding guildS of each bird species

# FEEDING GUILD:
  # Feeding guild preference was originally created by Graaf et al. 1985 that characterized the feeding habits
  # of each individual bird species in North America. Shriver 2015 (Master's Thesis) subsetted and simplified the
  # feeding guild preference to a handful of guilds for wetland species in Louisiana. I have subsetted all species
  # within the 50 m distance band to Shriver 2015's guilds. Species that were not in Shriver were located into a guild
  # based on observations, research, and similarly related species. An additional Upland Forager and Gleaner type was
  # created to bin upland species. 

# WETLAND SCORE:
  #Wetland habitat dependency score of each bird species in Pennsylvania was compiled by Croonquist et al. 1996. Almost
  # all species from the bird observations were assigned a score in Croonquist et al. Several species were manually
  # assigned a score by Grant McKown by similarly related species. Wetland scores are 0, 1, 3, and 5 with 0 being no 
  # wetland dependence by and 5 being fully dependent for habitat needs. 

# Import the bird species list with wetland score and feeding guild attributes

bird_descrip <- read.csv("Avian Analysis\\Input Data\\Bird_Species_50m.csv") %>%
  rename(feeding_guild_code = Feeding.Guild.Code,
         wetland_score = Wetland.Score)

# Add the feeding guild and wetland score to the bird 50 m dataset

birds_compiled <- birds %>%
  gather(key = AlphaCode, value = Count, AGWT:YEWA) %>%
  merge(., select(bird_descrip, AlphaCode, feeding_guild_code, wetland_score), by = "AlphaCode")

# Calculate the total number of bird observations per site visit for each feeding guild,
#Calculate the total number of panne and pool dependenet species 

bird_feeding_visit <- birds_compiled %>%
  group_by(Site_Date, feeding_guild_code) %>%
  summarise(Count = sum(Count)) %>%
  spread(feeding_guild_code, Count) %>%
  mutate(Panne_Dependent = DABBLER + MUDFLAT + WADING + PISCIVORE)

# Calculate the mean wetland score (non-weighted) for each site

bird_wetland_visit <- birds_compiled %>%
  group_by(Site_Date) %>%
  summarise(wetland_score = round(mean(wetland_score[Count > 0]), 2))





# Chapter 3: Create the dataframes for the Non-metric Dimensional Analysis

# For the NMDS, we need to create 3 dataframes for the analysis:

# 1) Species - Abundance Matrix
# 2) Site Covariates Matrix


# First, we need to get our bird_compiled dataset into a wide dataframe, 
# Remove any rare species, remove any sites without any bird observations,
# Merge the count of feeding guilds and mean wetland score for each

birds_nmds_dataset <- birds_compiled %>%
  #Remove miscellaneous or incomplete datasets
  select(-feeding_guild_code, -wetland_score, -runnel_age) %>%
  # Create wide dataset
  spread(AlphaCode, Count) %>%
  # Removes all columns that do no have observations in at least 5% of site visits (all visit metadata is retained)
  select_if(colSums(. != 0) > (nrow(.) * 0.05)) %>%
  # Removes any site visits that do not have a single bird observation (may need to change beginning column # for bird species)
  filter(rowSums(.[26:length(.)]) > 0) %>%
  #Merge the feeding guild counts and mean wetland score to dataset
  merge(bird_feeding_visit, by = "Site_Date") %>%
  merge(bird_wetland_visit, by = "Site_Date")


# Create species - abundance matrix

species <- select(birds_nmds_dataset, BARS:YEWA)

plots <- select(birds_nmds_dataset, Site_Date:saltysparrow, AERIAL:wetland_score)

# Now, we have three refined datasets that have removed the rare species and
# all sites that do not have any bird observations

# Chapter 5: Run the NMDS, Graph Stress Plot, and Run Ordination Analysis

nmds = metaMDS(species, distance = "bray", trymax = 20, k = 3)

nmds

stressplot(nmds)


# Format for Graphing purposes


#Calculate the locations of the each site on the NMDS ordination (for future ggplot graph)
nmds.points <- as.data.frame(scores(nmds$points)) %>%
  mutate(Treatment = plots$Treatment,
         Site = plots$Site,
         State = plots$State,
         Latitude = plots$Plot_Y,
         VisitNum = plots$VisitNum,
         Year = as.character(plots$Year),
         PointID = plots$PointID,
         Site_Date = plots$Site_Date,
         saltysparrow = plots$saltysparrow) %>%
  ungroup()


#Calculation of Species Scores (regression of species to ordination) and save locations for ggplot2
#We can calculate the species scores with the envfit() function of the vegan package
#Essentially it conducts a regression (r^2 and p-value) for each species to the ordination
#Be sure to record the Species Scores in Excel for future reporting purposes! 

nmds.species.env <- envfit(nmds, species, permuations = 999, na.rm = TRUE)

# Creation of dataframe with NMDS point locations for each individual species
# Species are then filtered by being significant (p < 0.05) and explained at least 10% of the varians (r > 0.10)

nmds.species <- data.frame(r = nmds.species.env$vectors$r, p = nmds.species.env$vectors$pvals) %>%
  cbind(nmds.species.env$vector$arrows) %>%
  mutate(metric = rownames(.)) %>%
  filter(p <= 0.05 & r > 0.10)


# Creation of dataframe with NMDS point locations for each individual species
# Species are then filtered by being significant (p < 0.05) and explained at least 10% of the varians (r > 0.10)

# Calculation of Site Characteristics Scores (regression of site characteristics to ordination)

nmds.site.env <- envfit(nmds, plots, permuations = 999, na.rm = TRUE)

nmds.site <- data.frame(r  = nmds.site.env$vector$r, 
                          p = nmds.site.env$vectors$pvals) %>%
  cbind(nmds.site.env$vector$arrows) %>%
  mutate(metric = rownames(.)) %>%
  filter(p <= 0.05 & r > 0.10)




# Chapter 7: Graph the NMDS

Bird_NMDS = ggplot(nmds.points,
                 aes(x = MDS1, y = MDS2)) + 
  geom_point(aes(colour = Treatment, shape = Year),
             size = 6, alpha = 1, stroke = 1.5) + 
 # geom_text_repel(aes(label = Site_Date)) +
  geom_text(data = nmds.species,
            aes(x = NMDS1, y = NMDS2), colour = "black", 
            fontface = "bold", label = row.names(nmds.species), size = 5) + 
  geom_segment(data = nmds.species,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               size =1, alpha = 0.5, colour = "grey30") +
  geom_text(data = nmds.site,
                      aes(x = NMDS1, y = NMDS2), colour = "black", 
                      fontface = "bold", label = row.names(nmds.site), size = 5) + 
  geom_segment(data = nmds.site,
                aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               size =1, alpha = 0.5, colour = "grey30") +
  theme(axis.title.y = element_text(face = "bold", size = 20, colour = "black"),
        axis.text.y = element_text(colour = "black", size = 18, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 18), 
        legend.text = element_text(size = 12, face ="bold", colour ="black"), 
        legend.position = "right",
        axis.title.x = element_text(face = "bold", size = 20, colour = "black"), 
        legend.title = element_text(size = 14, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) + 
  labs(x = "Axis 1", y = "Axis 2")

Bird_NMDS



















