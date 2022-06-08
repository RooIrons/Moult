## Do Moult Patterns differ significantly between female and males 
## of our nectarivore species?

# Clean the environment
rm(list = ls())
setwd("C:/Users/Rebecca Irons/Desktop/Thesis/Moult Code/Moult/R Scripts")

# Load the required packages
require(dplyr); require(ggplot2); library(broom) # packages for data manipulation
library(RCurl); library(rjson) # packages to read SABAP/SAFRING data via api calls

##** Reading in the Data for chosen species: **##
##*
# need a file of adu_names and numbers. Make sure this file is in the directory where the code will be run!

adu_names <- read.csv("adu_species 2022-04-04.csv", stringsAsFactors = F)
adu_names <- adu_names[, 1:3]
# 
# # TEST SUBSET. 
adu_names <- filter(adu_names, number%in%c(758, 760, 772, 749, 751, 753, 774,750,763)) # a bunch of sunbirds

adu_names
## initiate the dataframes to store variables (summary metrics and error file)

checkCulmen=NA; checkHead=NA; checkmass=NA; checkTail=NA; checkTarsus=NA; checkWing=NA;  

## Get the SAFRING data for chosen species 
SPP <- 758
# Get the data
ringing_data <- 
  read.csv(paste('https://api.birdmap.africa/safring/species/records_list/',SPP,'?format=csv',sep=""))

##** Tidy data: **##
##*
# we only want adult birds
ringing_data$Age <- as.numeric(ringing_data$Age)
ringing_data <- filter(ringing_data, Age>3)

# ensure all metric data is numeric
ringing_data$Mass <- as.numeric(as.character(ringing_data$Mass))
ringing_data$Wing <- as.numeric(as.character(ringing_data$Wing))
ringing_data$Culmen <- as.numeric(as.character(ringing_data$Culmen)) 
ringing_data$Head <- as.numeric(as.character(ringing_data$Head))
ringing_data$Tarsus <- as.numeric(as.character(ringing_data$Tarsus))

# fix a common issue: 0 is entered when value should be blank (these values cannot be 0)
# SAFRING: create sql to fix these in your database: these will not be flagged in the biometric checks
ringing_data$Mass[ringing_data$Mass==0] <- NA
ringing_data$Wing[ringing_data$Wing==0] <- NA
ringing_data$Head[ringing_data$Head==0] <- NA
ringing_data$Culmen[ringing_data$Culmen==0] <- NA
ringing_data$Tarsus[ringing_data$Tarsus==0] <- NA
ringing_data$Tail[ringing_data$Tail==0] <- NA
# this is for BP (leave for now)
# ringing_data[ringing_data=="-"] <- NA

# Here we deal with the very obvious measurement errors
# We remove those measures outside the 99%quantile - this should be conservative i.e. should keep most records except extreme errors
# as the original quantile definition will include the error outliers

# first create a reference set with spp# # also include the pentad check filter here:

checkmass <- ringing_data%>%filter(!is.na(Mass))%>%do(data.frame(lc=quantile(.$Mass, c(.05)), uq=quantile(.$Mass, c(.995)), n=length(.$Mass) ))
checkWing <- ringing_data%>%filter(!is.na(Wing))%>%do(data.frame(lcwing=quantile(.$Wing, c(.005)), uqwing=quantile(.$Wing, c(.995)), n=length(.$Wing)))
checkTarsus <- ringing_data%>%filter(!is.na(Tarsus))%>%do(data.frame(lctarsus=quantile(.$Tarsus, c(.005)), uqtarsus=quantile(.$Tarsus, c(.995)), n=length(.$Tarsus)))
checkHead <- ringing_data%>%filter(!is.na(Head))%>%do(data.frame(lcHead=quantile(.$Head, c(.005)), uqHead=quantile(.$Head, c(.995)), n=length(.$Head)))
checkCulmen <- ringing_data%>%filter(!is.na(Culmen))%>%do(data.frame(lcCulmen=quantile(.$Culmen, c(.005)), uqCulmen=quantile(.$Culmen, c(.995)), n=length(.$Culmen)))
checkTail <- ringing_data%>%filter(!is.na(Tail))%>%do(data.frame(lcTail=quantile(.$Tail, c(.005)), uqTail=quantile(.$Tail, c(.995)), n=length(.$Tail)))

ringing_data$masscheck <- NA
ringing_data$wingcheck <- NA
ringing_data$tarsuscheck <- NA
ringing_data$headcheck <- NA
ringing_data$culmencheck <- NA
ringing_data$tailcheck <- NA

# start the loop through the species data to validate each measurement (inside 99% quantile)
for(k in 1:nrow(ringing_data)){
  
  ringing_data$masscheck[k] <- ifelse(ringing_data$Mass[k]<checkmass$uq & ringing_data$Mass[k]>checkmass$lc , "OK", "error")  
  ringing_data$wingcheck[k] <- ifelse(ringing_data$Wing[k]<checkWing$uq & ringing_data$Wing[k]>checkWing$lc , "OK", "error")  
  ringing_data$tarsuscheck[k] <- ifelse(ringing_data$Tarsus[k]<checkTarsus$uq & ringing_data$Tarsus[k]>checkTarsus$lc , "OK", "error")  
  ringing_data$headcheck[k] <- ifelse(ringing_data$Head[k]<checkHead$uq & ringing_data$Head[k]>checkHead$lc , "OK", "error")  
  ringing_data$culmencheck[k] <- ifelse(ringing_data$Culmen[k]<checkCulmen$uq & ringing_data$Culmen[k]>checkCulmen$lc , "OK", "error")  
  ringing_data$tailcheck[k] <- ifelse(ringing_data$Tail[k]<checkTail$uq & ringing_data$Tail[k]>checkTail$lc , "OK", "error")  
  
  #print(paste("completed biometric check assignment for: ", adu_names$English[j]))  
  
} #end for i 1:nrow(ringing_data)

# replace NA with 'OK' otherwise next filter step is too harsh
ringing_data$masscheck[is.na(ringing_data$masscheck)] <- "OK"
ringing_data$tarsuscheck[is.na(ringing_data$tarsuscheck)] <- "OK"
ringing_data$headcheck[is.na(ringing_data$headcheck)] <- "OK"
ringing_data$culmencheck[is.na(ringing_data$culmencheck)] <- "OK"
ringing_data$tailcheck[is.na(ringing_data$tailcheck)] <- "OK"

# Filter out the 'error'

ringing_data <- filter(ringing_data, masscheck!='error', tarsuscheck!='error',headcheck!='error',culmencheck!='error',tailcheck!='error')

head(ringing_data)

## clean the brood patch column
table(ringing_data$Brood_patch)  
ringing_data$BP <-  as.numeric(ringing_data$Brood_patch) 
table(ringing_data$BP)

#simplify
ringing_data$BP <-  ifelse(ringing_data$BP >0 , 1, ringing_data$BP)
ringing_data$BP <-  ifelse(ringing_data$BP <0 , 0, ringing_data$BP)

ringing_data$BP <-  ifelse(ringing_data$Brood_patch %in% c("1.0", "2.0", "y", "BP", "Brood Patch") , 1, ringing_data$BP)
ringing_data$BP <-  ifelse(ringing_data$Brood_patch %in% c("-1.0", "n", "N", "no") , 0, ringing_data$BP)

table(ringing_data$BP)
# not a ton of data


# Get the date info organized

ringing_data$Startdate1 <- as.Date(ringing_data$Startdate, "%Y-%m-%d")
ringing_data$Month <- format(ringing_data$Startdate1, "%m") 
str(ringing_data$Month)
ringing_data$Month <- as.numeric(ringing_data$Month)
ringing_data$Year <- format(ringing_data$Startdate1, "%Y") 
table(ringing_data$Year)

# figure out if active molt is occurring
ringing_data$Moult <- as.numeric(ringing_data$Moult)
ringing_data <- filter(ringing_data, !is.na(Moult))

ringing_data$gender <- ifelse(ringing_data$Sex==1, "Male", ifelse(ringing_data$Sex==2, "Female", NA))
table(ringing_data$gender)
ringing_data$active_moult <- ifelse(ringing_data$Moult>0&ringing_data$Moult<5555555555, 1, 0)
table(ringing_data$active_moult)

##** Plotting **##
##*
#ggplot(data = filter(ringing_data, Age==4, Sex%in%c(1,2)), aes(Month, active_moult))+geom_jitter()+facet_grid(.~Sex)

# ggplot(data = filter(ringing_data, Age==4, Sex%in%c(1,2)), aes(as.numeric(Month), active_moult))+  
#   geom_smooth(method="glm", method.args = list(family = "binomial"))+
#   facet_grid(.~Sex)

ggplot(data = filter(ringing_data, Age==4, Sex%in%c(1,2)), aes(as.numeric(Month), active_moult))+  
  geom_smooth()+
  facet_grid(.~gender) +xlab("Month")

# Spatial

# Functions to get the bits of pentad for mapping
left = function(text, num_char) {substr(text, 1, num_char)}
mid = function(text, start_num, num_char) {  substr(text, start_num, start_num + num_char - 1)}

# This will give us rough Lat Long from pentads
ringing_data$Lat <- as.numeric(left(ringing_data$pentad, 2)) +  as.numeric(mid(ringing_data$pentad, 3, 2))/60
ringing_data$Latitude <- ifelse(mid(ringing_data$pentad,5,1)=="_", ringing_data$Lat*-1, ringing_data$Lat)
ringing_data$Longitude <- as.numeric(mid(ringing_data$pentad, 6,2)) +  as.numeric(mid(ringing_data$pentad, 8, 2))/60

# quick plot of where the data comes from

# Course spatial split by Longitude
ringing_data$West_East <- ifelse(ringing_data$Longitude<27, "West", "East")

## Plotting moult, female and male, east to west
GEW_758 <- ggplot(data = filter(ringing_data, !is.na(gender), !is.na(West_East)), 
                  aes(as.numeric(Month), active_moult, colour=West_East))+  
  geom_smooth() +xlab("Month") + ylab("Active Moult") +theme_bw(base_size = 14)+facet_wrap(.~gender)
print(GEW_758 + ggtitle("GDCS"))

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




##** With Stacked Chart: **##
##*
# We also need to look at distribution of records across the year. Ideally these need to be in the chart above too.

ringing_data2 %>% group_by(Month) %>% tally

temp <- ringing_data2 %>% group_by(Month) %>% tally
temp$n <- temp$n / max(temp$n)
temp

ringing_data2 %>% filter(!is.na(gender)) %>% group_by(Month) %>% tally
sum(is.na(ringing_data2$gender))

ringing_data2 %>% group_by(Month, active_moult) %>% tally %>% 
  ggplot(aes(Month, n))+geom_col()

# This shows distribution of data by sample size. 
ringing_data2 %>% filter(!is.na(West_East)) %>% group_by(West_East, Month, active_moult) %>% tally %>% 
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

print(A_Sunbird + ggtitle("GDCS"))  

