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

# Get some data, include non adults:
gdcs <- safring_get_clean_data(Spp_number = 758, checkSABAP2 = F, filterAdults = F) 

table(gdcs$Age)

gdcs$Age_class <- NA
gdcs$Age_class <- ifelse(gdcs$Age %in% c(1,2,3), "Juvenile", ifelse(gdcs$Age==4, "Adult", NA))
table(gdcs$Age_class)                                 

gdcs <- gdcs %>% filter(!is.na(Age_class))

ggplot(gdcs, aes(Moult_Month, 1, fill = Age_class))+geom_col(position = "fill")

ggplot(gdcs, aes(Moult_Month, active_moult))+geom_smooth()+
  geom_col(data = gdcs, aes(Moult_Month, 1, fill = Age_class),position = "fill", alpha = 0.5)+
  geom_smooth(data = filter(gdcs, Sex == 2), aes(Moult_Month, BP), se = F)+ggtitle("GDCS")


# Get some data:
csb <- safring_get_clean_data(Spp_number = 749, checkSABAP2 = F, filterAdults = F) 

table(csb$Age)

csb$Age_class <- NA
csb$Age_class <- ifelse(csb$Age %in% c(1,2,3), "Juvenile", ifelse(csb$Age==4, "Adult", NA))
table(csb$Age_class)                                 

csb <- csb %>% filter(!is.na(Age_class))

ggplot(csb, aes(Moult_Month, 1, fill = Age_class))+geom_col(position = "fill")

ggplot(csb, aes(Moult_Month, active_moult))+geom_smooth()+
  geom_col(data = csb, aes(Moult_Month, 1, fill = Age_class),position = "fill", alpha = 0.5)+
  geom_smooth(data = filter(csb, Sex == 2), aes(Moult_Month, BP), se = F)


ggplot(data = filter(csb, Sex == 2), aes(Month, active_moult))+geom_smooth()+
  geom_col(data = csb, aes(Month, 1, fill = Age_class),position = "fill", alpha = 0.5)+
  geom_smooth(data = filter(csb, Sex == 2), aes(Month, BP), se = F, colour = "black", size = 2)+ggtitle("Cape Sugarbird phenology")



# Get some data, include non adults:
obs <- safring_get_clean_data(Spp_number = 753, checkSABAP2 = F, filterAdults = F) 

table(obs$Age)

obs$Age_class <- NA
obs$Age_class <- ifelse(obs$Age %in% c(1,2,3), "Juvenile", ifelse(obs$Age==4, "Adult", NA))
table(obs$Age_class)                                 

obs <- obs %>% filter(!is.na(Age_class))

ggplot(obs, aes(Moult_Month, 1, fill = Age_class))+geom_col(position = "fill")

ggplot(obs, aes(Moult_Month, active_moult))+geom_smooth()+
  geom_col(data = obs, aes(Moult_Month, 1, fill = Age_class),position = "fill", alpha = 0.5)+
  geom_smooth(data = filter(obs, Sex == 2), aes(Moult_Month, BP), se = F)+ggtitle("obs")

ggplot(obs, aes(Month, active_moult))+geom_smooth()+
  geom_col(data = obs, aes(Month, 1, fill = Age_class),position = "fill", alpha = 0.5)+
  geom_smooth(data = filter(obs, Sex == 2), aes(Month, BP), se = F)+ggtitle("obs")
