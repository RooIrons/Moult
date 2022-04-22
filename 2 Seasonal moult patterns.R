
## Here we read all the data in the SAFRING database, convert moult to binary and then examine temporal patterns. 

# Author: Alan Lee
# File initially created April 2018 based on code to clean data for the ringing/climate change manuscript

# Update 2022 March: old api not valid, but new 

library(dplyr); library(ggplot2); library(broom) # packages for data manipulation
# library(RCurl); library(rjson) # packages to read SABAP/SAFRING data via api calls
library(lmerTest) # stats package

## Get the data. Ideally this was done from spreadsheet 1.
# 
# SPP <- 749 # This is the unique species code for the number. You can get these from the SABAP2 website.
# ringing_data <- read.csv(paste('https://api.birdmap.africa/safring/species/records_list/',SPP,'?format=csv',sep=""))
    
  head(ringing_data)

## clean the brood patch column
  table(ringing_data$Brood_patch)  
  ringing_data$BP <-  as.numeric(ringing_data$Brood_patch) 
 table(ringing_data$BP)
 
 temp$BP <-  as.numeric(temp$Brood_patch) 
 table(temp$BP)
 
 #simplify
 ringing_data$BP <-  ifelse(ringing_data$BP >0 , 1, ringing_data$BP)
 ringing_data$BP <-  ifelse(ringing_data$BP <0 , 0, ringing_data$BP)
 
temp$BP <-  ifelse(temp$BP >0 , 1, temp$BP)
temp$BP <-  ifelse(temp$BP <0 , 0, temp$BP)
 
 ringing_data$BP <-  ifelse(ringing_data$Brood_patch %in% c("1.0", "2.0", "y", "BP", "Brood Patch") , 1, ringing_data$BP)
 ringing_data$BP <-  ifelse(ringing_data$Brood_patch %in% c("-1.0", "n", "N", "no") , 0, ringing_data$BP)
 
 temp$BP <-  ifelse(temp$Brood_patch %in% c("1.0", "2.0", "y", "BP", "Brood Patch") , 1, temp$BP)
 temp$BP <-  ifelse(temp$Brood_patch %in% c("-1.0", "n", "N", "no") , 0, temp$BP)
 
 table(ringing_data$BP)
 table(temp$BP) # not a ton of data
 
 
# Get the date info organized
 
ringing_data$Startdate1 <- as.Date(ringing_data$Startdate, "%Y-%m-%d")
ringing_data$Month <- format(ringing_data$Startdate1, "%m") 
str(ringing_data$Month)
ringing_data$Month <- as.numeric(ringing_data$Month)
ringing_data$Year <- format(ringing_data$Startdate1, "%Y") 
table(ringing_data$Year)

temp$Startdate1 <- as.Date(temp$Startdate, "%Y-%m-%d")
temp$Month <- format(temp$Startdate1, "%m") 
str(temp$Month)
temp$Month <- as.numeric(temp$Month)
temp$Year <- format(temp$Startdate1, "%Y") 
table(temp$Year)

# figure out if active molt is occurring
ringing_data$Moult <- as.numeric(ringing_data$Moult)
ringing_data <- filter(ringing_data, !is.na(Moult))

ringing_data$gender <- ifelse(ringing_data$Sex==1, "Male", ifelse(ringing_data$Sex==2, "Female", NA))
table(ringing_data$gender)
ringing_data$active_moult <- ifelse(ringing_data$Moult>0&ringing_data$Moult<5555555555, 1, 0)
table(ringing_data$active_moult)

temp$Moult <- as.numeric(temp$Moult)
temp <- filter(temp, !is.na(Moult))

temp$gender <- ifelse(temp$Sex==1, "Male", ifelse(temp$Sex==2, "Female", NA))
table(temp$gender)
temp$active_moult <- ifelse(temp$Moult>0&temp$Moult<5555555555, 1, 0)
table(temp$active_moult)
#ggplot(data = filter(ringing_data, Age==4, Sex%in%c(1,2)), aes(Month, active_moult))+geom_jitter()+facet_grid(.~Sex)

# ggplot(data = filter(ringing_data, Age==4, Sex%in%c(1,2)), aes(as.numeric(Month), active_moult))+  
#   geom_smooth(method="glm", method.args = list(family = "binomial"))+
#   facet_grid(.~Sex)

ggplot(data = filter(ringing_data, Age==4, Sex%in%c(1,2)), aes(as.numeric(Month), active_moult))+  
  geom_smooth()+
  facet_grid(.~gender) +xlab("Month")

plot1 <- ggplot(data = filter(ringing_data, Age==4, Sex%in%c(1)), aes(as.numeric(Month), active_moult))+  
  geom_smooth() +xlab("Month") + ylab("Active Moult")+theme_bw(base_size = 14)+
  geom_smooth(data = filter(ringing_data, Age==4, Sex%in%c(2)), aes(as.numeric(Month), active_moult), colour="pink", size = 2)+
  geom_smooth(data = filter(ringing_data, Age==4, Sex%in%c(2)), aes(as.numeric(Month), BP), colour="black", size = 0.5)
print(plot1 + ggtitle("Orange-breasted Sunbird"))  

summary(glm(active_moult ~ BP, data = filter(ringing_data, Sex == 2), family = binomial))


ggplot(data = filter(temp, Age==4, Sex%in%c(1,2)), aes(as.numeric(Month), active_moult))+  
  geom_smooth()+
  facet_grid(.~gender) +xlab("Month")

plot1 <- ggplot(data = filter(temp, Age==4, Sex%in%c(1)), aes(as.numeric(Month), active_moult))+  
  geom_smooth() +xlab("Month") + ylab("Active Moult")+theme_bw(base_size = 14)+
  geom_smooth(data = filter(temp, Age==4, Sex%in%c(2)), aes(as.numeric(Month), active_moult), colour="pink", size = 2)+
  geom_smooth(data = filter(temp, Age==4, Sex%in%c(2)), aes(as.numeric(Month), BP), colour="black", size = 0.5)
print(plot1 + ggtitle("Orange-breasted Sunbird"))

### How to look at change by time? Maybe monthly by decade

ringing_data$Decade <- NA
ringing_data$Decade <- ifelse(ringing_data$Year < 2000, "Pre 2000", ringing_data$Decade)
ringing_data$Decade <- ifelse(ringing_data$Year < 2010 & ringing_data$Year > 2000, "2000 to 2009", ringing_data$Decade)
ringing_data$Decade <- ifelse(ringing_data$Year >2009, ">2010", ringing_data$Decade)
table(ringing_data$Decade)

ggplot(data = filter(ringing_data, !is.na(gender), !is.na(Decade)), aes(as.numeric(Month), active_moult, colour=Decade))+  
  geom_smooth() +xlab("Month")+theme_bw(base_size = 14)+facet_wrap(.~gender)


temp$Decade <- NA
temp$Decade <- ifelse(temp$Year < 2000, "Pre 2000", temp$Decade)
temp$Decade <- ifelse(temp$Year < 2010 & temp$Year > 2000, "2000 to 2009", temp$Decade)
temp$Decade <- ifelse(temp$Year >2009, ">2010", temp$Decade)
table(temp$Decade)

ggplot(data = filter(temp, !is.na(gender), !is.na(Decade)), aes(as.numeric(Month), active_moult, colour=Decade))+  
  geom_smooth() +xlab("Month")+theme_bw(base_size = 14)+facet_wrap(.~gender)

# Spatial

# Functions to get the bits of pentad for mapping
left = function(text, num_char) {substr(text, 1, num_char)}
mid = function(text, start_num, num_char) {  substr(text, start_num, start_num + num_char - 1)}

# This will give us rough Lat Long from pentads
ringing_data$Lat <- as.numeric(left(ringing_data$pentad, 2)) +  as.numeric(mid(ringing_data$pentad, 3, 2))/60
ringing_data$Latitude <- ifelse(mid(ringing_data$pentad,5,1)=="_", ringing_data$Lat*-1, ringing_data$Lat)
ringing_data$Longitude <- as.numeric(mid(ringing_data$pentad, 6,2)) +  as.numeric(mid(ringing_data$pentad, 8, 2))/60

temp$Lat <- as.numeric(left(temp$pentad, 2)) +  as.numeric(mid(temp$pentad, 3, 2))/60
temp$Latitude <- ifelse(mid(temp$pentad,5,1)=="_", temp$Lat*-1, temp$Lat)
temp$Longitude <- as.numeric(mid(temp$pentad, 6,2)) +  as.numeric(mid(temp$pentad, 8, 2))/60
# quick plot of where the data comes from


# Course spatial split by Longitude
ringing_data$West_East <- ifelse(ringing_data$Longitude<27, "West", "East")

temp$West_East <- ifelse(temp$Longitude<27, "West", "East")

ggplot(temp, aes(Longitude, Latitude, colour = West_East))+geom_point()


# Trying GIS

library(sf)
library(ggspatial)

Locations <- st_as_sf(ringing_data, coords = c("Longitude", "Latitude"), crs = 4326)

Locations1 <- st_as_sf(temp, coords = c("Longitude", "Latitude"), crs = 4326)

class(Locations)
class(Locations1)

names(Locations)
names(Locations1)

ggplot() + 
  annotation_map_tile(type = "osm", progress = "none", zoomin = -1) + 
  geom_sf(data=Locations1, aes(colour = West_East))

## Plotting moult, female and male, east to west
GEW_753 <- ggplot(data = filter(ringing_data, !is.na(gender), !is.na(West_East)), 
       aes(as.numeric(Month), active_moult, colour=West_East))+  
  geom_smooth() +xlab("Month") + ylab("Active Moult") +theme_bw(base_size = 14)+facet_wrap(.~gender)
print(GEW_753 + ggtitle("Orange-breasted Sunbird"))

# Here I reproduce January as an additional month to extend the curve over the summer period

temp <- filter(ringing_data, as.numeric(Month) == 1)
temp$Month <- 13

ringing_data2 <- bind_rows(ringing_data, temp)

Title <- filter(adu_names, number == SPP) %>% select(English) %>% as.character(.)

ggplot(data = filter(ringing_data2, !is.na(gender), !is.na(West_East)), 
       aes(as.numeric(Month), active_moult, colour=West_East))+  
  geom_smooth() +xlab("Month")+theme_bw(base_size = 14)+facet_wrap(.~gender)+
  coord_cartesian(xlim = c(0, 13), ylim=c(0, 1))+
  ggtitle(Title)



# We also need to look at distribution of records across the year. Ideally these need to be in the chart above too.

ringing_data %>% group_by(Month) %>% tally

temp <- ringing_data %>% group_by(Month) %>% tally
temp$n <- temp$n / max(temp$n)
temp

ringing_data %>% filter(!is.na(gender)) %>% group_by(Month) %>% tally
sum(is.na(ringing_data$gender))

ringing_data %>% group_by(Month, active_moult) %>% tally %>% 
  ggplot(aes(Month, n))+geom_col()

# We can double check the smooth chart with this one
ringing_data %>% filter(!is.na(West_East)) %>% group_by(West_East, Month, active_moult) %>% tally %>% 
  ggplot(aes(Month, n, fill = active_moult))+geom_col(position = "fill")+facet_wrap(.~West_East)

# We can double check the smooth chart with this one
# Stacked chart
ringing_data %>% filter(!is.na(West_East))%>% group_by(West_East, Month, active_moult) %>% tally %>% 
  ggplot(aes(Month, n, fill = active_moult))+geom_col(position = "fill")+facet_wrap(.~West_East)

# This shows distribution of data by sample size. 
ringing_data %>% filter(!is.na(West_East)) %>% group_by(West_East, Month, active_moult) %>% tally %>% 
  ggplot(aes(Month, n, fill = active_moult))+geom_col()+facet_wrap(.~West_East)

# Stacked chart
ringing_data %>% filter(!is.na(gender)) %>% group_by(gender, Month, active_moult) %>% tally %>% 
  ggplot(aes(Month, n, fill = active_moult))+geom_col(position = "fill")+facet_wrap(.~gender)

# Stacked chart
ringing_data %>% filter(!is.na(gender)) %>% group_by(gender, Month, active_moult) %>% tally %>% 
  ggplot(aes(Month, n, fill = active_moult))+geom_col(position = "fill")+facet_wrap(.~gender)

# add bar

A_Sunbird <- ggplot(data = filter(ringing_data, !is.na(gender), !is.na(West_East)), 
       aes(as.numeric(Month), active_moult))+  
  geom_col(data = temp, aes(Month, n, alpha = 0.25), show.legend = F)+
geom_smooth() +xlab("Month")+ ylab("Active Moult")+theme_bw(base_size = 14)

print(A_Sunbird + ggtitle("Orange-breasted Sunbird"))  
