# This is function for cleaning SAFRING data with some preformatting for analysis
## Get and clean data function
## 23 April 2022
# Alan Lee

## Get the SAFRING data

safring_get_clean_data <- function(Spp_number=749, checkSABAP2=TRUE, filterAdults = TRUE, filterMoult = TRUE){
  library(tidyverse)
  SPP <- Spp_number
  # Get the data
  ringing_data <- 
    read.csv(paste('https://api.birdmap.africa/safring/species/records_list/',SPP,'?format=csv',sep=""))
  
  
  checkCulmen=NA; checkHead=NA; checkmass=NA; checkTail=NA; checkTarsus=NA; checkWing=NA;  
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

  # Here we deal with the very obvious measurement errors
  # We remove those measures outside the 99%quantile - this should be conservative i.e. should keep most records except extreme errors
  # as the original quantile definition will include the error outliers
  
  # first create a reference set with spp# 
  
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
  
  ##### Option loops #####
  ## range check against SABAP2 data. Optional. This relies on there being SABAP2 records for SAFRING ringing locations. #####
  if(checkSABAP2 == TRUE){
    sabap2url <- paste('https://api.birdmap.africa/sabap2/v2/cards/species/info/',SPP,'?format=csv',sep="")
    sabap2data <- read.csv(sabap2url)
    
    Spp_pentads <- unique(sabap2data$Pentad)
    ringing_data <- filter(ringing_data, pentad%in%Spp_pentads)
  }
  
  if(filterAdults == TRUE){
    ringing_data <- filter(ringing_data, Age>3)
  }

    ###### Some preprocessing #######
  
  ringing_data$Age <- as.numeric(ringing_data$Age)
  ringing_data$BP <-  as.numeric(ringing_data$Brood_patch) 
  ringing_data$BP <-  ifelse(ringing_data$BP >0 , 1, ringing_data$BP)
  ringing_data$BP <-  ifelse(ringing_data$BP <0 , 0, ringing_data$BP)
  
  ringing_data$BP <-  ifelse(ringing_data$Brood_patch %in% c("1.0", "2.0", "y", "BP", "Brood Patch") , 1, ringing_data$BP)
  ringing_data$BP <-  ifelse(ringing_data$Brood_patch %in% c("-1.0", "n", "N", "no") , 0, ringing_data$BP)
  
  # process Dates
  ringing_data$Startdate <- as.Date(ringing_data$Startdate, "%Y-%m-%d")
  ringing_data$Month <- format(ringing_data$Startdate, "%m") 
  ringing_data$Month <- as.numeric(ringing_data$Month)
  ringing_data$Year <- format(ringing_data$Startdate, "%Y") 
  library(lubridate)
  ringing_data$Day <- yday(ringing_data$Startdate)
  
  # Moult_Month will offset months to focus over summer for visual display grouped by month
  
  ringing_data$Moult_Month <- ifelse(ringing_data$Month<6, ringing_data$Month + 12, ringing_data$Month)

  # Do the active moult thing
  # figure out if active molt is occurring. Type 1 data?
  ringing_data$Moult <- as.numeric(ringing_data$Moult)
  
  if(filterMoult == TRUE){
  ringing_data <- filter(ringing_data, !is.na(Moult))
  }
  ringing_data$gender <- ifelse(ringing_data$Sex==1, "Male", ifelse(ringing_data$Sex==2, "Female", NA))
  ringing_data$active_moult <- ifelse(ringing_data$Moult>0&ringing_data$Moult<5555555555, 1, 0)

  # Functions to get the bits of pentad for mapping
  left = function(text, num_char) {substr(text, 1, num_char)}
  mid = function(text, start_num, num_char) {  substr(text, start_num, start_num + num_char - 1)}
  
  # This will give us rough Lat Long from Location_code (IF formatted correctly!!)
  ringing_data$Latitude <- -1 * (as.numeric(left(ringing_data$Location_code, 2)) +  as.numeric(mid(ringing_data$Location_code, 3, 2))/60)
  ringing_data$Longitude <- as.numeric(mid(ringing_data$Location_code, 6,2)) +  as.numeric(mid(ringing_data$Location_code, 8, 2))/60
  
  ringing_data$Geo_Quad_Score <- ringing_data$Latitude + ringing_data$Longitude
  ringing_data$West_East <- ifelse(ringing_data$Longitude<27, "West", "East")
  
  # 
    
  # End preprocessing
  
  return(ringing_data)
}
