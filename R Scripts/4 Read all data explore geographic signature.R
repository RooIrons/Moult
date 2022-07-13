## What I want to do with this sheet:

## Download all nectarivore data
## Since moult is a summer to autumn thing, we'll reformat date for this by adding 12 to months <6
## Get the max proportion in moult month, and PIM value
## geographic quadrant score = long + lat (in dec degrees e.g. 19 - 34 will give us a low quadrant)
## Across the set of species the peak month should correlate with GQS or associated climate data

# Clean the environment
rm(list = ls())
setwd("C:/Users/bexir/OneDrive/Desktop/Academics/Thesis/Moult Code/Moult/R Scripts")

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



# Decide if to do SABAP check. For CSB, it reduces rows from 4735 to 4513. 
# nectarivores <- read_csv("Filtered nectarivores.csv"). The following 5 lines results in this file

nectarivores <- safring_get_clean_data(Spp_number = adu_names$number[1], checkSABAP2 = T)
for(i in 2:nrow(adu_names)){
  nectarivores <- bind_rows(nectarivores, safring_get_clean_data(Spp_number = adu_names$number[i], checkSABAP2 = T))
}
nectarivores <- left_join(nectarivores, select(adu_names, Spp = number, Name = English))


count(nectarivores, Name)

#Name    n
#1         Amethyst (Black) Sunbird 3716
#2                   Cape Sugarbird 4513
#3  Greater Double-collared Sunbird 1981
#4               Gurney's Sugarbird  260
#5                Malachite Sunbird 1960
#6          Orange-breasted Sunbird 1959
#7          Scarlet-chested Sunbird  451
#8 Southern Double-collared Sunbird 4962
#9 White-bellied (breasted) Sunbird 1853

library(sf)
citation("sf")
library(ggspatial)
citation("ggspatial")
library(rosm)
citation("rosm")
Locations <- st_as_sf(nectarivores, coords = c("Longitude", "Latitude"), crs = 4326)
class(Locations)
names(Locations)

windows()
ggplot(nectarivores, aes(Longitude, Latitude, colour = Geo_Quad_Score))+geom_point()

ggplot(nectarivores, aes(Longitude, Latitude, colour = Name))+geom_jitter(size = 0.5)

ggplot() + 
  annotation_map_tile(type = "osm", progress = "none", zoomin = 0) + 
  geom_sf(data=Locations, aes(colour = Name))+geom_jitter(size = 0.5)

# illustrate broad patterns by species
ggplot(nectarivores, aes(Moult_Month, active_moult))+geom_smooth()+facet_wrap(~Name)


# We want to grab the month with the highest probability of moult
library(mgcv)
citation("mgcv")

nectarivores <-   filter(nectarivores, !is.na(gender)) 

# Example species CSB
CSB <- subset(nectarivores, nectarivores$Spp == 772) #Amethyst
CSB <- subset(nectarivores, nectarivores$Spp == 749) #Cape sugarbird
CSB <- subset(nectarivores, nectarivores$Spp == 758) #GDC sunbird
CSB <- subset(nectarivores, nectarivores$Spp == 750) #Gurney's Sugarbird
CSB <- subset(nectarivores, nectarivores$Spp == 751) #Malachite Sunbird
CSB <- subset(nectarivores, nectarivores$Spp == 753) #Orange-breasted Sunbird
CSB <- subset(nectarivores, nectarivores$Spp == 774) #Scarlet-chested Sunbird
CSB <- subset(nectarivores, nectarivores$Spp == 760) #Southern Double-collared Sunbird
CSB <- subset(nectarivores, nectarivores$Spp == 763) #White-bellied (breasted) Sunbird

# fit a smooth term for day of year
summary(gam(active_moult ~ Month,data = CSB, family = binomial))

CSB$csb_prediction <- predict(gam(active_moult ~ s(Month, by = factor(gender), bs="cc"),data = CSB, family = binomial), data = CSB)

max(CSB$csb_prediction)
my_max_min <- group_by(CSB, gender) %>% summarise(Max = max(csb_prediction))
my_max_min <- left_join(my_max_min, group_by(CSB, gender) %>% summarise(Min = min(csb_prediction)) )

filter(CSB, gender=="Female", csb_prediction==my_max_min$Max[1]) %>% select(Month) %>% distinct
filter(CSB, gender=="Female", csb_prediction==my_max_min$Min[1]) %>% select(Month) %>% distinct
filter(CSB, gender=="Male", csb_prediction==my_max_min$Max[2]) %>% select(Month) %>% distinct
filter(CSB, gender=="Male", csb_prediction==my_max_min$Min[2]) %>% select(Month) %>% distinct

summary(gam(active_moult ~ s(Day)+West_East,data = CSB, family = binomial))
summary(gam(active_moult ~ s(Day)+West_East,data = CSB, family = binomial))

MSB <- subset(nectarivores, nectarivores$Spp == 751)

# model summary,
## cc makes start and end points equivalent, for cyclical data
summary(gam(active_moult ~ s(Day,by = factor(West_East), bs = "cc"),data = MSB, family = binomial))

# This is the prediction...
MSB$msb_output <- predict(gam(active_moult ~ s(Day, by = factor(West_East), bs="cc"),data = MSB, family = binomial), data = MSB)
ggplot(MSB, aes(Day, msb_output, colour=West_East))+geom_point()

ggplot(MSB, aes(Day, active_moult, colour=West_East))+geom_smooth()
ggplot(MSB, aes(Moult_Month, active_moult, colour=West_East))+geom_smooth()
ggplot(MSB, aes(Day, active_moult, colour=West_East))+geom_smooth()

# Reduce degrees of freedoms to get 'smoother' curves


# # replace GQS with climate data

## Commented out section was an unnecessary diversion. 

# ## https://github.com/nicholas-kotlinski/worldclim_tutorial?msclkid=8d2eef6bd12e11eca17573b3d59e4a93
# library(raster)
# r <- getData("worldclim", var="bio", res=10) # Resultion is 10 arc second (~1km)
# bio <- r[[c(1,12)]] 
# 
# points <- dplyr::select(nectarivores, Longitude, Latitude)
# 
# spatial_points <- SpatialPointsDataFrame(coords = points, data = points,
#                        proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
#   
# values <- extract(bio, points)
# values <- as.tibble(values)
# nectarivores$Mean_annual_temp <- scale(values$bio1, center = F)
# hist(nectarivores$Mean_annual_temp)
# nectarivores$Annual_rainfall <- scale(values$bio12, center = F)
# hist(nectarivores$Annual_rainfall)
# 
# summary(glmer(active_moult ~ Annual_rainfall+Mean_annual_temp + (1|Spp),data = nectarivores, family = binomial))
# summary(glm(active_moult ~ Annual_rainfall+Mean_annual_temp + English,data = nectarivores, family = binomial))
# 
# 
# summary(glmer(active_moult ~ Geo_Quad_Score+Annual_rainfall*Mean_annual_temp + (1|Spp),data = nectarivores, family = binomial))
# summary(glm(active_moult ~ Geo_Quad_Score + English,data = nectarivores, family = binomial))
# summary(glm(active_moult ~ factor(Moult_Month)+Mean_annual_temp + English,data = nectarivores, family = binomial))
# summary(glm(active_moult ~ Month+Mean_annual_temp + English,data = nectarivores, family = binomial))
# summary(glm(active_moult ~ Month*Geo_Quad_Score + English,data = nectarivores, family = binomial))
# summary(glm(active_moult ~ Month*English,data = nectarivores, family = binomial))
# 
