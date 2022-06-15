### Initial code from SAFRING 'diagnostics 2.R' manuscript

## Updated to exclude the SABAP location check.

# Author: Alan Lee
# File initially created April 2022 based on code to clean data for the ringing/climate change manuscript
rm(list = ls())
setwd("C:/Users/Rebecca Irons/Desktop/Thesis/Moult Code/Moult/R Scripts")
require(dplyr); require(ggplot2); library(broom) # packages for data manipulation
# library(RCurl); library(rjson) # packages to read SABAP/SAFRING data via api calls

# need a file of adu_names and numbers. Make sure this file is in the directory where the code will be run!

 adu_names <- read.csv("adu_species 2022-04-04.csv", stringsAsFactors = F)
 adu_names <- adu_names[, 1:3]
# 
# # TEST SUBSET. 
 adu_names <- filter(adu_names, number%in%c(758, 760, 772, 749, 751, 753, 774,750,763, 869, 855, 867, 866, 856)) # a bunch of sunbirds

adu_names
 ## initiate the dataframes to store variables (summary metrics and error file)

checkCulmen=NA; checkHead=NA; checkmass=NA; checkTail=NA; checkTarsus=NA; checkWing=NA;  

## Get the SAFRING data

# j <-  2 # Get the first species number in the filtered list of preferred species. 
SPP <- adu_names$number[j]
SPP <- 856
# Get the data
ringing_data <- 
  read.csv(paste('https://api.birdmap.africa/safring/species/records_list/',SPP,'?format=csv',sep=""))

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

# reduce the file to exclude birds with no key metric data? 
# Yes: otherwise more pentads to check down the line and it all takes too long...
# No = not all IDs will be checked (How nb is this?)
# ringing_data <-
#   filter(ringing_data, !is.na(Mass)|!is.na(Wing)|!is.na(Tarsus)|!is.na(Culmen)|!is.na(Tail)|!is.na(Head))


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

#### range check against SABAP2 data. Optional. This relies on there being SABAP2 records for SAFRING ringing locations.
# 
sabap2url <- paste('https://api.birdmap.africa/sabap2/v2/cards/species/info/',SPP,'?format=csv',sep="")
sabap2data <- read.csv(sabap2url)
# 
Spp_pentads <- unique(sabap2data$Pentad)
temp <- filter(ringing_data, pentad%in%Spp_pentads)
