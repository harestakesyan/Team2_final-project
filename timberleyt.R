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
cardio.df$cardio.yn<- as.factor(ifelse(cardio.df$cardio == "0", "No", "Yes"))
cardio.df$active.yn<- as.factor(ifelse(cardio.df$active == "0", "No", "Yes"))
cardio.df$bmi<- cardio.df$weight / ((cardio.df$height / 100) * (cardio.df$height / 100))
str(cardio.df)

#Remove Outliers
summary(cardio.df)

cardio1<- outlierKD2(cardio.df, height, rm = TRUE, histogram = FALSE)
cardio2<- outlierKD2(cardio1, weight, rm = TRUE, histogram = FALSE)
cardio3<- outlierKD2(cardio2, bmi, rm = TRUE, histogram = FALSE)
cardio4<- outlierKD2(cardio3, ap_hi, rm = TRUE, histogram = FALSE)
cardio5<- outlierKD2(cardio4, ap_lo, rm = TRUE, histogram = FALSE)

cardio.final <- na.omit(cardio5)
summary(cardio.final)
rm(cardio1, cardio2, cardio3, cardio4, cardio5)

#EDA Graphics
  #cholesterol
cholesterol.1 <- ggplot(cardio.final, aes(x=cardio, fill= cholesterol)) +
  geom_bar(position="fill") +
  scale_x_discrete(labels=c("1" = "Has CVD", "0" = "Does NOT Have CVD")) +
  scale_y_continuous(labels = scales::percent) +
  labs( title = "Patients with CVD Likely to Have Higher Cholesterol Levels", y = "", x = "",
        fill = "Cholesterol Level") +
  scale_fill_manual(values=c("#f4cccc", "#e06666", "#990000")) +
  theme_minimal()
cholesterol.1

cholesterol_1 <- ggplot(cardio.final, aes(x=cholesterol, fill= cardio.yn)) + geom_bar(position="fill") +
  scale_x_discrete(labels=c("1" = "Normal", "2" = "Above normal","3" = "Well Above Normal")) +
  scale_y_continuous(labels = scales::percent) +
  labs( title = "Patients with CVD Likely to Have Higher Cholesterol Levels",
        subtitele = "",
        y = "", x = "Cholesterol Level",
        fill = "Has CVD") +
  scale_fill_manual(values=c("#990000", "#0000da")) +
  theme_minimal()

cholesterol_1
  
  
  #Physical Activity
pa <- ggplot(cardio.final, aes(x=cardio, fill= active.yn)) +
  geom_bar(position="fill") +
  scale_x_discrete(labels=c("1" = "Has CVD", "0" = "Does NOT Have CVD")) +
  scale_y_continuous(labels = scales::percent) +
  labs( title = "Patients with CVD Likely to Have Higher Cholesterol Levels", y = "", x = "",
        fill = "Physically Active") +
  scale_fill_manual(values=c("#990000", "#0000da")) +
  theme_bw()
pa



