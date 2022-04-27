## What I want to do with this sheet:

## Download all nectarivore data
## Since moult is a summer to autumn thing, we'll reformat date for this by adding 12 to months <6
## Get the max proportion in moult month, and PIM value
## geographic quadrant score = long + lat (in dec degrees e.g. 19 - 34 will give us a low quadrant)
## Across the set of species the peak month should correlate with GQS or associated climate data
## Or can we look simply as glm(pmoult ~ long+lat, binom)

# Clean the environment
rm(list = ls())

# Libraries
library(tidyverse)

# Read the read and clean data function
source(
  file = "function_read_clean_safring_data.R"
)

# Grab target spp codes. I added a Bex filter here

adu_names <- read_csv("adu_species 2022-04-04.csv") %>% filter(Bex_filter == 1)
#

adu_names

# Get all the data. I'll keep the SABAP filter off. Ignore the warnings. 
nectarivores <- safring_get_clean_data(Spp_number = adu_names$number[1], checkSABAP2 = F)

for(i in 2:nrow(adu_names)){
  nectarivores <- bind_rows(nectarivores, safring_get_clean_data(Spp_number = adu_names$number[i], checkSABAP2 = F))
}

count(nectarivores, Spp)

ggplot(nectarivores, aes(Longitude, Latitude, colour = Geo_Quad_Score))+geom_point()
# I will cut those very 'out' data
nectarivores <- filter(nectarivores, Longitude>15, Longitude<35, Latitude<(-20))
ggplot(nectarivores, aes(Longitude, Latitude, colour = Geo_Quad_Score))+geom_point()


# illustrate broad patterns by species
ggplot(nectarivores, aes(Moult_Month, active_moult))+geom_smooth()+facet_grid(~Spp)
# Wow a geographic pattern
summary(glm(active_moult ~ Geo_Quad_Score,data = nectarivores, family = binomial))
# Probably need to control that for species
summary(glm(active_moult ~ factor(Spp)+Geo_Quad_Score,data = nectarivores, family = binomial))
summary(glm(active_moult ~ factor(Spp)*Geo_Quad_Score,data = nectarivores, family = binomial))
