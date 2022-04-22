# Now use the cleaned data and put it into moult format (using example code from moult)

# 14 April 2022
# Assumes data has been read and cleaned (sheet 1)

install.packages("moult")
library(moult)

if (is.numeric(ringing_data$Moult)) {
  scores <- format(ringing_data$Moult, scientific = FALSE, trim = TRUE)
} else {
  scores <- ringing_data$Moult
}
mscores <- substr(scores, 1, 10)
unique(mscores)
feather.mass <- c(10.4, 10.8, 11.5, 12.8, 14.4, 15.6, 16.3, 15.7, 15.7, 10.0)
## convert moult scores to proportion of feather mass grown
ringing_data$pfmg <- ms2pfmg(mscores, feather.mass)
str(ringing_data$pfmg)
hist(ringing_data$pfmg)
ringing_data$day <- date2days(ringing_data$Startdate, dateformat = "yyyy-mm-dd", startmonth = 8)
str(ringing_data$day)
table(ringing_data$day)
ssex <- ifelse(ringing_data$Sex == 1 | ringing_data$Sex == 3, "male",
               ifelse(ringing_data$Sex == 2 | ringing_data$Sex == 4, "female", NA))
table(ssex)
ringing_data$ssex <- as.factor(ssex)


## model with duration and mean start date of moult depending on sex
mmf <- moult(pfmg ~ day | ssex | ssex, data = ringing_data, type = 1)
summary(mmf)
## predict duration and start of moult (then both) for males and females
ssex <- c("male", "female")
day <- 150
(p1 <- predict.moult(mmf, newdata = data.frame(day, ssex), predict.type = "duration"))
(p2 <- predict.moult(mmf, newdata = data.frame(day, ssex), predict.type = "start"))
(p3 <- predict.moult(mmf, newdata = data.frame(day, ssex), predict.type = "both"))
