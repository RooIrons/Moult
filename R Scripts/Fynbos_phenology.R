rm(list = ls())

library(tidyverse); library(readxl)

flowers <- read_excel("flower.totals2.meta.xlsx")

ggplot(flowers, aes(londd, latdd))+geom_point()
flowers$WestEast <- ifelse(flowers$londd<23 , "West", "East")
flowers$WestEast <- ifelse(flowers$latdd> -30 , "East", flowers$WestEast)

ggplot(flowers, aes(londd, latdd, colour = WestEast))+geom_point()

df <- select(flowers, jan:dec)

##??
#df1 <- select(rf, jan:dec)

flowers$Means <- rowMeans(df)
flowers$Sums <- rowSums(df)
# 1 row has no flowers, so rowMean == 0
filter(flowers, Means == 0)
filter(flowers, Sums ==0)
flowers <- filter(flowers, Sums>0)
flowers$jan <- flowers$jan/flowers$Means
colSums(flowers[,'jan'])
flowers$feb <- flowers$feb/flowers$Means
flowers$mar <- flowers$mar/flowers$Means
flowers$apr <- flowers$apr/flowers$Means
flowers$may <- flowers$may/flowers$Means
flowers$jun <- flowers$jun/flowers$Means
flowers$jul <- flowers$jul/flowers$Means
flowers$aug <- flowers$aug/flowers$Means
flowers$sep <- flowers$sep/flowers$Means
flowers$oct <- flowers$oct/flowers$Means
flowers$nov <- flowers$nov/flowers$Means
flowers$dec <- flowers$dec/flowers$Means

wflowers <- flowers %>% filter(WestEast =="West")
colMeans(wflowers[,c(3:14)])
pheno = tibble(Fynbos_flowers = colMeans(wflowers[,c(3:14)]))
eflowers <- flowers %>% filter(WestEast =="East")
colMeans(eflowers[,c(3:14)])
pheno = bind_cols(pheno, tibble(E_flowers = colMeans(eflowers[,c(3:14)])))
pheno$Month <- seq(1:12)
ggplot(pheno, aes(Month, Fynbos_flowers))+geom_line()+
  geom_line(data = pheno, aes(Month, E_flowers), colour = "red", size = 2)
pheno = bind_cols(pheno, tibble(Flowers = colMeans(flowers[,c(3:14)])))
ggplot(pheno, aes(Month, Fynbos_flowers))+geom_line(colour = "red", size = 2)
