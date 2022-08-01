#Libraries
library(ezids)
library(tidyverse)

#Data
cardio.df<- read.csv("C:/Users/thomp/Desktop/GWU/Summer 2022/dats6101/final project/Team2_final-project/cardio_train.csv")
str(cardio.df)

cardio.df$id<- as.character(cardio.df$id)
cardio.df$gender<- as.factor(cardio.df$gender)
cardio.df$cholesterol<- factor(cardio.df$cholesterol, levels = c("1", "2", "3"))
cardio.df$gluc<- factor(cardio.df$gluc, levels = c("1", "2", "3"))
cardio.df$smoke<- as.factor(cardio.df$smoke)
cardio.df$alco<- as.factor(cardio.df$alco)
cardio.df$active<- as.factor(cardio.df$active)
cardio.df$cardio<- as.factor(cardio.df$cardio)
str(cardio.df)

#Summary/Descriptive Statistics
summary(cardio.df)


#Remove observations with BP Outliers
  #ap_hi 
Q1 <- quantile(cardio.df$ap_hi, .25)
Q3 <- quantile(cardio.df$ap_hi, .75)
IQR <- IQR(cardio.df$ap_hi)

hi.nouts <- subset(cardio.df, cardio.df$ap_hi > (Q1 - 1.5*IQR) & cardio.df$ap_hi < (Q3 + 1.5*IQR))

  #ap_lo
Q1 <- quantile(cardio.df$ap_lo, .25)
Q3 <- quantile(cardio.df$ap_lo, .75)
IQR <- IQR(cardio.df$ap_lo)

lo.nouts <- subset(cardio.df, cardio.df$ap_lo > (Q1 - 1.5*IQR) & cardio.df$ap_lo < (Q3 + 1.5*IQR))

#Summary/Descriptive Stats w/o Outliers
cardio.df<- lo.nouts
summary(cardio.df)
