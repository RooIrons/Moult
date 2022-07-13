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

setwd("C:/Users/bexir/OneDrive/Desktop/Academics/Thesis/Moult Code/Moult/R Scripts")

# Read the read and clean data function
source(
  file = "function_read_clean_safring_data.R"
)

# Get some data, include non adults:
##** Greater-Double Collared Sunbird **##
gdcs <- safring_get_clean_data(Spp_number = 758, checkSABAP2 = F, filterAdults = F) 

table(gdcs$Age)

gdcs$Age_class <- NA
gdcs$Age_class <- ifelse(gdcs$Age %in% c(1,2,3), "Juvenile", ifelse(gdcs$Age==4, "Adult", NA))

table(gdcs$Age_class)                                 
# Adult Juvenile 
# 1875      201  

gdcs <- gdcs %>% filter(!is.na(Age_class))

ggplot(gdcs, aes(Moult_Month, 1, fill = Age_class))+geom_col(position = "fill")

ggplot(gdcs, aes(Moult_Month, active_moult))+geom_smooth()+
  geom_col(data = gdcs, aes(Moult_Month, 1, fill = Age_class),position = "fill", alpha = 0.5)+
  geom_smooth(data = filter(gdcs, Sex == 2), aes(Moult_Month, BP), colour = "black", se = F)+ggtitle("GDCS")


# Get some data:
##** Cape Sugarbird **##
csb <- safring_get_clean_data(Spp_number = 749, checkSABAP2 = F, filterAdults = F) 

table(csb$Age)

csb$Age_class <- NA
csb$Age_class <- ifelse(csb$Age %in% c(1,2,3), "Juvenile", ifelse(csb$Age==4, "Adult", NA))

table(csb$Age_class)                                 
# Adult Juvenile 
# 4459      755 

csb <- csb %>% filter(!is.na(Age_class))

ggplot(csb, aes(Moult_Month, 1, fill = Age_class))+geom_col(position = "fill")

ggplot(csb, aes(Moult_Month, active_moult))+geom_smooth()+
  geom_col(data = csb, aes(Moult_Month, 1, fill = Age_class),position = "fill", alpha = 0.5)+
  geom_smooth(data = filter(csb, Sex == 2), aes(Moult_Month, BP), se = F)+ggtitle("csb")

ggplot(data = filter(csb, Sex == 2), aes(Month, active_moult))+geom_smooth()+
  geom_col(data = csb, aes(Month, 1, fill = Age_class),position = "fill", alpha = 0.5)+
  geom_smooth(data = filter(csb, Sex == 2), aes(Month, BP), se = F, colour = "black", size = 1)+ggtitle("Cape Sugarbird Phenology")

#### TRYING

csb %>% group_by(Month) %>% tally

counts <- csb %>% group_by(Month) %>% tally
counts$n <- counts$n / max(counts$n)
counts

A_Sunbird <- ggplot(data = filter(csb, !is.na(gender), !is.na(West_East)), 
                    aes(as.numeric(Month), active_moult))+  
  geom_col(data = counts, aes(Month, n, alpha = 0.25), show.legend = T)+
  geom_smooth() +xlab("Month")+ ylab("Active Moult")+theme_bw(base_size = 14)

print(A_Sunbird + ggtitle("Cape Sugarbird"))  


Ringing_Moult <- ggplot(data = filter(csb, !is.na(gender), !is.na(West_East)), 
                    aes(as.numeric(Month), active_moult))+  
  geom_col(data = counts, aes(Month, n, alpha = 0.25), show.legend = F)+
  scale_y_continuous("Active Moult", sec.axis = sec_axis(~(.), name = "Ringing Counts")) +
  scale_x_continuous("Month", breaks = 1:12) 

print(Ringing_Moult+ ggtitle("Cape Sugarbird"))  

# Get some data, include non adults:
##** Orange-breasted Sunbird **##
obs <- safring_get_clean_data(Spp_number = 753, checkSABAP2 = F, filterAdults = F) 

table(obs$Age)

obs$Age_class <- NA
obs$Age_class <- ifelse(obs$Age %in% c(1,2,3), "Juvenile", ifelse(obs$Age==4, "Adult", NA))

table(obs$Age_class)                                 
# Adult Juvenile 
# 1924      688  

obs <- obs %>% filter(!is.na(Age_class))

ggplot(obs, aes(Moult_Month, 1, fill = Age_class))+geom_col(position = "fill")

ggplot(obs, aes(Moult_Month, active_moult))+geom_smooth()+
  geom_col(data = obs, aes(Moult_Month, 1, fill = Age_class),position = "fill", alpha = 0.5)+
  geom_smooth(data = filter(obs, Sex == 2), aes(Moult_Month, BP), se = F)+ggtitle("obs")

ggplot(obs, aes(Month, active_moult))+geom_smooth()+coord_cartesian(ylim = c(0,1))+
  geom_col(data = obs, aes(Month, 1, fill = Age_class),position = "fill", alpha = 0.5)+
  geom_smooth(data = filter(obs, Sex == 2), aes(Month, BP), se = F, colour = "black", size = 1)+ggtitle("obs")

# Get some data, include non adults:
##** Amethyst Sunbird **##
as <- safring_get_clean_data(Spp_number = 772, checkSABAP2 = F, filterAdults = F) 

table(as$Age)

as$Age_class <- NA
as$Age_class <- ifelse(as$Age %in% c(1,2,3), "Juvenile", ifelse(as$Age==4, "Adult", NA))

table(as$Age_class)                                 
# Adult Juvenile 
# 2905     1534 

as <- as %>% filter(!is.na(Age_class))

ggplot(as, aes(Moult_Month, 1, fill = Age_class))+geom_col(position = "fill")

ggplot(as, aes(Moult_Month, active_moult))+geom_smooth()+
  geom_col(data = as, aes(Moult_Month, 1, fill = Age_class),position = "fill", alpha = 0.5)+
  geom_smooth(data = filter(as, Sex == 2), aes(Moult_Month, BP), se = F)+ggtitle("as")

ggplot(as, aes(Month, active_moult))+geom_smooth()+
  geom_col(data = as, aes(Month, 1, fill = Age_class),position = "fill", alpha = 0.5)+
  geom_smooth(data = filter(as, Sex == 2), aes(Month, BP), se = F, colour = "black", size = 1)+ggtitle("Amethyst Sunbird Phenology")

# Get some data, include non adults:
##** Gurney's Sunbird **##
gs <- safring_get_clean_data(Spp_number = 750, checkSABAP2 = F, filterAdults = F) 

table(gs$Age)

gs$Age_class <- NA
gs$Age_class <- ifelse(gs$Age %in% c(1,2,3), "Juvenile", ifelse(gs$Age==4, "Adult", NA))

table(gs$Age_class)                                 
# Adult Juvenile 
# 304       43 

gs <- gs %>% filter(!is.na(Age_class))

ggplot(gs, aes(Moult_Month, 1, fill = Age_class))+geom_col(position = "fill")

ggplot(gs, aes(Moult_Month, active_moult))+geom_smooth()+
  geom_col(data = gs, aes(Moult_Month, 1, fill = Age_class),position = "fill", alpha = 0.5)+
  geom_smooth(data = filter(gs, Sex == 2), aes(Moult_Month, BP), se = F)+ggtitle("gs")

ggplot(gs, aes(Month, active_moult))+geom_smooth()+
  geom_col(data = gs, aes(Month, 1, fill = Age_class),position = "fill", alpha = 0.5)+
  geom_smooth(data = filter(gs, Sex == 2), aes(Month, BP), se = F, colour = "black", size = 1)+ggtitle("Gurney's Sunbird Phenology")

# Get some data, include non adults:
##** Malachite Sunbird **##
ms <- safring_get_clean_data(Spp_number = 751, checkSABAP2 = F, filterAdults = F) 

table(ms$Age)

ms$Age_class <- NA
ms$Age_class <- ifelse(ms$Age %in% c(1,2,3), "Juvenile", ifelse(ms$Age==4, "Adult", NA))

table(ms$Age_class)                                 
# Adult Juvenile 
# 1946      344

ms <- ms %>% filter(!is.na(Age_class))

ggplot(ms, aes(Moult_Month, 1, fill = Age_class))+geom_col(position = "fill")

ggplot(ms, aes(Moult_Month, active_moult))+geom_smooth()+
  geom_col(data = ms, aes(Moult_Month, 1, fill = Age_class),position = "fill", alpha = 0.5)+
  geom_smooth(data = filter(ms, Sex == 2), aes(Moult_Month, BP), se = F)+ggtitle("ms")

ggplot(ms, aes(Month, active_moult))+geom_smooth()+
  geom_col(data = ms, aes(Month, 1, fill = Age_class),position = "fill", alpha = 0.5)+
  geom_smooth(data = filter(ms, Sex == 2), aes(Month, BP), se = F, colour = "black", size = 1)+ggtitle("Malachite Sunbird Phenology")

# Get some data, include non adults:
##** Scarlet-Chested Sunbird **##
scs <- safring_get_clean_data(Spp_number = 774, checkSABAP2 = F, filterAdults = F) 

table(scs$Age)

scs$Age_class <- NA
scs$Age_class <- ifelse(scs$Age %in% c(1,2,3), "Juvenile", ifelse(scs$Age==4, "Adult", NA))

table(scs$Age_class)                                 
# Adult Juvenile 
# 238      101 

scs <- scs %>% filter(!is.na(Age_class))

ggplot(scs, aes(Moult_Month, 1, fill = Age_class))+geom_col(position = "fill")

ggplot(scs, aes(Moult_Month, active_moult))+geom_smooth()+
  geom_col(data = scs, aes(Moult_Month, 1, fill = Age_class),position = "fill", alpha = 0.5)+
  geom_smooth(data = filter(scs, Sex == 2), aes(Moult_Month, BP), se = F)+ggtitle("scs")

ggplot(scs, aes(Month, active_moult))+geom_smooth()+
  geom_col(data = scs, aes(Month, 1, fill = Age_class),position = "fill", alpha = 0.5)+
  geom_smooth(data = filter(scs, Sex == 2), aes(Month, BP), se = F, colour = "black", size = 1)+ggtitle("Scarlet-chested Sunbird Phenology")

# Get some data, include non adults:
##** Southern Double-Collared Sunbird *##
sdcs <- safring_get_clean_data(Spp_number = 760, checkSABAP2 = F, filterAdults = F) 

table(sdcs$Age)

sdcs$Age_class <- NA
sdcs$Age_class <- ifelse(sdcs$Age %in% c(1,2,3), "Juvenile", ifelse(sdcs$Age==4, "Adult", NA))

table(sdcs$Age_class)                                 
# Adult Juvenile 
#  4793      756 

sdcs <- sdcs %>% filter(!is.na(Age_class))

ggplot(sdcs, aes(Moult_Month, 1, fill = Age_class))+geom_col(position = "fill")

ggplot(sdcs, aes(Moult_Month, active_moult))+geom_smooth()+
  geom_col(data = sdcs, aes(Moult_Month, 1, fill = Age_class),position = "fill", alpha = 0.5)+
  geom_smooth(data = filter(sdcs, Sex == 2), aes(Moult_Month, BP), se = F)+ggtitle("sdcs")

ggplot(sdcs, aes(Month, active_moult))+geom_smooth()+
  geom_col(data = sdcs, aes(Month, 1, fill = Age_class),position = "fill", alpha = 0.5)+
  geom_smooth(data = filter(sdcs, Sex == 2), aes(Month, BP), se = F, colour = "black", size = 1)+ggtitle("Southern Double-collared Sunbird Phenology")

# Get some data, include non adults:
##** White-bellied Sunbird **##
wbs <- safring_get_clean_data(Spp_number = 763, checkSABAP2 = F, filterAdults = F) 

table(wbs$Age)

wbs$Age_class <- NA
wbs$Age_class <- ifelse(wbs$Age %in% c(1,2,3), "Juvenile", ifelse(wbs$Age==4, "Adult", NA))

table(wbs$Age_class)                                 
# Adult Juvenile 
# 1656      586  

wbs <- wbs %>% filter(!is.na(Age_class))

ggplot(wbs, aes(Moult_Month, 1, fill = Age_class))+geom_col(position = "fill")

ggplot(wbs, aes(Moult_Month, active_moult))+geom_smooth()+
  geom_col(data = wbs, aes(Moult_Month, 1, fill = Age_class),position = "fill", alpha = 0.5)+
  geom_smooth(data = filter(wbs, Sex == 2), aes(Moult_Month, BP), se = F)+ggtitle("wbs")

ggplot(wbs, aes(Month, active_moult))+geom_smooth()+
  geom_col(data = wbs, aes(Month, 1, fill = Age_class),position = "fill", alpha = 0.5)+
  geom_smooth(data = filter(wbs, Sex == 2), aes(Month, BP), se = F, colour = "black", size = 1)+ggtitle("White-bellied Sunbird Phenology")
