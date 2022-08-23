# This pulls data from the SAFRING database

# Species of interest: Cape Sugarbird, Cape Weaver, Orange-breasted Sunbird


library(moult)
?moult
citation()


rm(list = ls())

# Define a species to pull data from database
SPP <- 749 # Cape Sugarbird; 753 = Orange-breasted Sunbird, 799 = Cape Weaver

source("function_read_clean_safring_data.R") # this returns Cape Sugarbird data

ringing_data <- safring_get_clean_data(Spp_number = SPP, checkSABAP2 = F) #data download speeded up without the checkSABAP2 data

#Birgi Erni suggests this workflow for prep
if (is.numeric(ringing_data$Moult)) {
  ringing_data$scores <- format(ringing_data$Moult, scientific = FALSE, trim = TRUE)
} else {
  ringing_data$scores <- ringing_data$Moult
}
ringing_data$mscores <- substr(ringing_data$scores, 1, 10)
unique(ringing_data$mscores)

(rema <- grep("8", ringing_data$mscores))             # 8 = indeterminate moult socre
(remb <- which(nchar(ringing_data$mscores) != 10))
(remc <- grep("\\(",ringing_data$mscores))
(remd <- grep("F", ringing_data$mscores))
(reme <- !grepl("\\D", ringing_data$mscores))      # all strings which contain non-digits

ringing_data$mscores[rema] <- NA
ringing_data$mscores[remb] <- NA
ringing_data$mscores[remc] <- NA
ringing_data$mscores[remd] <- NA
ringing_data$mscores[!reme] <- NA

ringing_data$mscores <- ifelse(ringing_data$Moult==0, "0000000000", ringing_data$mscores)
sum(is.na(ringing_data$mscores))
unique(ringing_data$mscores)
sum(ringing_data$mscores  == "5555555555", na.rm = T)
sum(ringing_data$mscores  == "0000000000", na.rm = T)

# feather masses for Lesser DBL Collared Sunbird, Underhill & Joubert 1995
feather.mass <- c(9.0, 9.7, 10.3, 11.0, 11.9, 12.1, 12.2, 12.0, 10.0, 1.9)
sum(feather.mass)

#feather.mass <- c(10.4, 10.8, 11.5, 12.8, 14.4, 15.6, 16.3, 15.7, 15.7)
## convert moult scores to proportion of feather mass grown
ringing_data2 <- ringing_data[!is.na(ringing_data$mscores),]

ringing_data2$pfmg <- ms2pfmg(ringing_data2$mscores, feather.mass)

hist(ringing_data2$pfmg)
sum(is.na(ringing_data2$pfmg))

ringing_data2$day <- date2days(ringing_data2$Startdate, dateformat = "yyyy-mm-dd", startmonth = 8)

ssex <- ifelse(ringing_data2$Sex == 1 | ringing_data2$Sex == 3, "male",
               ifelse(ringing_data2$Sex == 2 | ringing_data2$Sex == 4, "female", NA))
table(ssex)
ringing_data2$ssex <- as.factor(ssex)

plot(pfmg ~ day, data = ringing_data2, col = ssex, pch = 19, las = 1)

## model with duration and mean start date of moult depending on sex
mmf <- moult(pfmg ~ day , data = ringing_data2, type = 1)
mmf2 <- moult(pfmg ~ day , data = ringing_data2, type = 2)
mmf3 <- moult(pfmg ~ day , data = ringing_data2, type = 3)


summary(mmf)
summary(mmf2)
summary(mmf3)


## predict duration and start of moult (then both) for males and females
ssex <- c("male", "female")
day <- 150
(p1 <- predict.moult(mmf, newdata = data.frame(day, ssex), predict.type = "duration"))
(p2 <- predict.moult(mmf, newdata = data.frame(day, ssex), predict.type = "start"))
(p3 <- predict(mmf, newdata = data.frame(day, ssex), predict.type = "both"))


durationmean2ab <- function(duration, mean)
{ ab <- c(- mean / duration, 1 / duration)
names(ab) <- c("intercept", "slope")
return(ab)
}

mmf.ab <- durationmean2ab(coef(mmf)[1], coef(mmf)[2])
mmf.ab2 <- durationmean2ab(coef(mmf2)[1], coef(mmf2)[2])

mmf.ab3 <- durationmean2ab(coef(mmf3)[1], coef(mmf3)[2])

plot(pfmg ~ day, data = ringing_data2, col = ssex, pch = 19, las = 1)
abline(mmf.ab, lwd = 2)
abline(mmf.ab3, lwd = 3)
abline(mmf.ab2, lwd = 5)

mmf.ab

ggplot(data = filter(ringing_data2, !is.na(active_moult), !is.na(ssex)), aes(day, active_moult, colour = ssex))+
  geom_point()+geom_smooth()+theme_bw()+
geom_point(data = ringing_data2, aes(day, pfmg))+
  geom_abline(intercept = mmf.ab[[1]], slope = mmf.ab[[2]])+coord_cartesian(ylim = c(0,1))+
geom_abline(intercept = mmf.ab3[[1]], slope = mmf.ab3[[2]], colour = "blue")

# AL: I want to see what is scored 0 and 5 and moulting
ringing_data2$zero_or_5 <- NA
ringing_data2$zero_or_5 <- ifelse(mscores == "0000000000", "Not yet moulted", ifelse(mscores == "5555555555", "Completed", "Active"))
table(ringing_data2$zero_or_5)

library(lubridate)
# ringing_data$Date <- ymd(ringing_data$Startdate)
ggplot(data = filter(ringing_data2, !is.na(zero_or_5)), aes(day, pfmg, colour = zero_or_5))+
  geom_jitter(alpha = 0.5)

# what is the diff between pfmg and Msum?
ringing_data2$MSum <- NA

sum(as.numeric(unlist(strsplit(mscores[1], split  = ""))), na.rm = T)

for (i in 1:nrow(ringing_data2)){
ringing_data2$MSum[i] <- sum(as.numeric(unlist(strsplit(mscores[i], split  = ""))), na.rm = T)
}
hist(ringing_data2$MSum)

# pfmg seems somewhat overstated
ggplot(ringing_data2, aes(MSum, pfmg))+geom_jitter()
ggplot(data = filter(ringing_data2, !is.na(zero_or_5)), aes(day, MSum, colour = zero_or_5))+
  geom_jitter(alpha = 0.5)
