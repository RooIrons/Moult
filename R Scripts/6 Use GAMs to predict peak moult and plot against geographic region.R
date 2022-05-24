
# A continuation of the geographic moult exploration from sheet 4
# I learnt in a run of this the sp split means we don't have enough to do the models by gender
# Now I am going to split MSB and SDCS by geo region

nectarivores <- read_csv("Filtered nectarivores.csv")
nectarivores <-   filter(nectarivores, !is.na(gender), !is.na(Month)) 

nectarivores$Geo_Name <- nectarivores$Name
nectarivores$Geo_Name <- ifelse(nectarivores$Spp == 751 & nectarivores$West_East=="West", "Western MSB", nectarivores$Geo_Name)
nectarivores$Geo_Name <- ifelse(nectarivores$Spp == 760 & nectarivores$West_East=="West", "Western SDCS", nectarivores$Geo_Name)
nectarivores$Geo_Name <- ifelse(nectarivores$Spp == 758 & nectarivores$West_East=="West", "Western GDCS", nectarivores$Geo_Name)

# gam collapsing for MSB, possibly due to low month count for August. Make those 9
#nectarivores$Month <- ifelse(nectarivores$Geo_Name == "Malachite Sunbird" & nectarivores$Month==8, 9, nectarivores$Month)

table(nectarivores$Geo_Name)

names(nectarivores)
nectarivores$Spp

summary(nectarivores)

# Example species CSB
CSB <- filter(nectarivores, Geo_Name == "Amethyst (Black) Sunbird")
CSB <- filter(nectarivores, Geo_Name == "Cape Sugarbird")
CSB <- filter(nectarivores, Geo_Name == "Greater Double-collared Sunbird")
CSB <- filter(nectarivores, Geo_Name == "Gurney's Sugarbird")
CSB <- filter(nectarivores, Geo_Name == "Malachite Sunbird")
CSB <- filter(nectarivores, Geo_Name == "Orange-breasted Sunbird")
CSB <- filter(nectarivores, Geo_Name == "Scarlet-chested Sunbird")
CSB <- filter(nectarivores, Geo_Name == "Southern Double-collared Sunbird")
# CSB <- filter(nectarivores, Geo_Name == "Western GDCS")
# CSB <- filter(nectarivores, Geo_Name == "Western MSB")
# CSB <- filter(nectarivores, Geo_Name == "Western SDCS")
CSB <- filter(nectarivores, Geo_Name == "White-bellied (breasted) Sunbird")

# fit a smooth term for day of year
library(mgcv)

CSB$csb_prediction <- predict(gam(active_moult ~ s(Day, bs="cc"),data = CSB, family = binomial),type = 'response', data = CSB)

my_max_min <- summarise(CSB, Max = max(csb_prediction))
my_max_min <- bind_cols(my_max_min, summarise(CSB, Min = min(csb_prediction)) )
my_max_min$Name <- select(CSB, Name) %>% distinct %>% as.character()

my_max_min
table(CSB$Month)


# initiate holding frame
my_max_min$Peak_Breed_Month <- NA
my_max_min$Peak_Moult_Month <- NA

filter(CSB, csb_prediction == my_max_min$Max) %>% select(Month, Day, Age, Sex)

my_max_min$Peak_Moult_Month <-  filter(CSB, csb_prediction==my_max_min$Max[1]) %>% 
  select(Month) %>% distinct %>% as.numeric()

my_max_min$Peak_Breed_Month <- filter(CSB,  csb_prediction==my_max_min$Min[1]) %>% 
  select(Month) %>% distinct %>% as.numeric()

bird_names <- nectarivores %>% select(Geo_Name) %>% distinct
bird_names <- filter(bird_names, Geo_Name!="Western MSB")

for(i in 2:nrow(bird_names)){
  temp_df <- filter(nectarivores, Geo_Name == bird_names$Geo_Name[i])
  temp_df$temp_df_prediction <- predict(gam(active_moult ~ s(Day, bs="cc"),data = temp_df, family = binomial), type = 'response', data = temp_df)
  
  temp_max_min <- summarise(temp_df, Max = max(temp_df_prediction))
  temp_max_min <- bind_cols(temp_max_min, summarise(temp_df, Min = min(temp_df_prediction)) )
  temp_max_min$Name <- select(temp_df, Geo_Name) %>% distinct %>% as.character()
  
  # initate holding frame
  temp_max_min$Peak_Breed_Month <- NA
  temp_max_min$Peak_Moult_Month <- NA
  
  temp_max_min$Peak_Moult_Month <-  filter(temp_df, temp_df_prediction==temp_max_min$Max[1]) %>% 
    select(Month) %>% distinct %>% as.numeric()
  
  temp_max_min$Peak_Breed_Month <- filter(temp_df, temp_df_prediction==temp_max_min$Min[1]) %>% 
    select(Month) %>% distinct %>% as.numeric()

  my_max_min <- bind_rows(my_max_min, temp_max_min)
}

my_max_min

my_max_min <- left_join(my_max_min, 
                        group_by(nectarivores, Name = Geo_Name) %>% summarise(GQS = mean(Geo_Quad_Score))
)

ggplot(my_max_min, aes(Peak_Breed_Month, GQS, colour = Name, label = Name))+geom_jitter(size = 3, alpha = 0.5)+
  geom_text(nudge_y = 1, check_overlap = T)+
  coord_cartesian(xlim = c(0, 13))

ggplot(my_max_min, aes(Peak_Moult_Month, GQS, colour = Name, label = Name))+geom_jitter(size = 3, alpha = 0.5)+
  geom_text(nudge_y = 1, check_overlap = T)+
  coord_cartesian(xlim = c(0, 13))
