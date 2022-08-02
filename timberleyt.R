#Libraries
library(ezids)
library(tidyverse)

#Data
cardio.df<- read.csv("C:/Users/thomp/Desktop/GWU/Summer 2022/dats6101/cardio_train_copy.csv")
str(cardio.df)

cardio.df$id<- as.character(cardio.df$id)
cardio.df$age<- cardio.df$age / 365
cardio.df$gender<- as.factor(cardio.df$gender)
cardio.df$cholesterol<- factor(cardio.df$cholesterol, levels = c("1", "2", "3"))
cardio.df$gluc<- factor(cardio.df$gluc, levels = c("1", "2", "3"))
cardio.df$smoke<- as.factor(cardio.df$smoke)
cardio.df$alco<- as.factor(cardio.df$alco)
cardio.df$active<- as.factor(cardio.df$active)
cardio.df$cardio<- as.factor(cardio.df$cardio)
cardio.df$bmi<- cardio.df$weight / ((cardio.df$height / 100) * (cardio.df$height / 100))
str(cardio.df)

#Remove Outliers
summary(cardio.df)

cardio1<- outlierKD2(cardio.df, height, rm = TRUE, histogram = FALSE)
cardio2<- outlierKD2(cardio1, weight, rm = TRUE, histogram = FALSE)
cardio3<- outlierKD2(cardio2, bmi, rm = TRUE, histogram = FALSE)
cardio4<- outlierKD2(cardio3, ap_hi, rm = TRUE, histogram = FALSE)
cardio5<- outlierKD2(cardio4, ap_lo, rm = TRUE, histogram = FALSE)

Cardio<- na.omit(cardio5)

summary(Cardio)





