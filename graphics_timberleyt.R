#### Startup ####

#Libraries
library(ezids)
library(tidyverse)

#Data
cardio.df<- read.csv("C:/Users/thomp/Desktop/GWU/Summer 2022/dats6101/cardio_train_copy.csv")
str(cardio.df)

#### Categorical Variables ####
#cholesterol
cholesterol.1 <- ggplot(cardio_clean_final, aes(x=cardio, fill= cholesterol)) +
  geom_bar(position="fill") +
  scale_x_discrete(labels=c("1" = "Heart Disease", "0" = "Healthy")) +
  scale_y_continuous(labels = scales::percent) +
  labs( title = "Cholesterol", y = "", x = "",
        fill = "Cholesterol Level") +
  scale_fill_manual(values=c("#f4cccc", "#e06666", "#990000")) +
  theme_minimal()

#Glucose
gluc.1 <- ggplot(cardio_clean_final, aes(x=cardio, fill= gluc)) +
  geom_bar(position="fill") +
  scale_x_discrete(labels=c("1" = "Heart Disease", "0" = "Healthy")) +
  scale_y_continuous(labels = scales::percent) +
  labs( title = "Glucose", y = "", x = "",
        fill = "Glucose Level") +
  scale_fill_manual(values=c("#f4cccc", "#e06666", "#990000")) +
  theme_minimal()

#Gender
cardio_clean_final$gender.yn<- as.factor(ifelse(cardio_clean_final$gender== "1", "Women", "Men"))

gender.1 <- ggplot(cardio_clean_final, aes(x=cardio, fill= gender.yn)) +
  geom_bar(position="fill") +
  scale_x_discrete(labels=c("1" = "Heart Disease", "0" = "Healthy")) +
  scale_y_continuous(labels = scales::percent) +
  labs( title = "Gender",
        y = "", x = "", fill = "") +
  scale_fill_manual(values=c("#0000da", "#990000")) +
  theme_minimal()

#Smoke
cardio_clean_final$smoke.yn<- as.factor(ifelse(cardio_clean_final$smoke== "1", "Yes", "No"))

smoke.1 <- ggplot(cardio_clean_final, aes(x=cardio, fill= smoke.yn)) +
  geom_bar(position="fill") +
  scale_x_discrete(labels=c("1" = "Heart Disease", "0" = "Healthy")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Tobacco Use", y = "", x = "", fill = "Smoker") +
  scale_fill_manual(values=c("#0000da", "#990000")) +
  theme_minimal()

#alcohol
cardio_clean_final$alco.yn<- as.factor(ifelse(cardio_clean_final$alco== "1", "Yes", "No"))

alco.1 <- ggplot(cardio_clean_final, aes(x=cardio, fill= alco.yn)) +
  geom_bar(position="fill") +
  scale_x_discrete(labels=c("1" = "Heart Disease", "0" = "Healthy")) +
  scale_y_continuous(labels = scales::percent) +
  labs( title = "Alcohol Use", y = "", x = "", fill = "Drinks Alcohol") +
  scale_fill_manual(values=c("#0000da", "#990000")) +
  theme_minimal()

#active
cardio_clean_final$active.yn<- as.factor(ifelse(cardio_clean_final$active== "1", "Yes", "No"))

active.1 <- ggplot(cardio_clean_final, aes(x=cardio, fill= active.yn)) +
  geom_bar(position="fill") +
  scale_x_discrete(labels=c("1" = "Heart Disease", "0" = "Healthy")) +
  scale_y_continuous(labels = scales::percent) +
  labs( title = "Physical Activity", y = "", x = "", fill = "Physically Active") +
  scale_fill_manual(values=c("#0000da", "#990000")) +
  theme_minimal()
































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
cardio.df$bmi<- cardio.df$weight / ((cardio.df$height / 100)**2)
str(cardio.df)

#Remove Outliers
summary(cardio.df)

#cardio1<- outlierKD2(cardio.df, height, rm = TRUE, histogram = FALSE)
#cardio2<- outlierKD2(cardio1, weight, rm = TRUE, histogram = FALSE)
cardio3<- outlierKD2(cardio.df, bmi, rm = TRUE, histogram = FALSE)
cardio4<- outlierKD2(cardio3, ap_hi, rm = TRUE, histogram = FALSE)
cardio5<- outlierKD2(cardio4, ap_lo, rm = TRUE, histogram = FALSE)

cardio.final <- na.omit(cardio5)
summary(cardio.final)
rm(cardio3, cardio4, cardio5)

#### EDA Graphics: Categorical Variables ####

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

  #Glucose
glucose.1 <- ggplot(cardio.final, aes(x=cardio, fill= gluc)) +
  geom_bar(position="fill") +
  scale_x_discrete(labels=c("1" = "Has CVD", "0" = "Does NOT Have CVD")) +
  scale_y_continuous(labels = scales::percent) +
  labs( title = "Patients with CVD Likely to Have Higher Glucose Levels", y = "", x = "",
        fill = "Glucose Level") +
  scale_fill_manual(values=c("#f4cccc", "#e06666", "#990000")) +
  theme_minimal()
glucose.1

#Gender
cardio.final$gender.yn<- as.factor(ifelse(cardio.final$gender== "1", "Women", "Men"))

gender.1 <- ggplot(cardio.final, aes(x=cardio, fill= gender.yn)) +
  geom_bar(position="fill") +
  scale_x_discrete(labels=c("1" = "Has CVD", "0" = "Does NOT Have CVD")) +
  scale_y_continuous(labels = scales::percent) +
  labs( y = "", x = "", fill = "") +
  scale_fill_manual(values=c("#990000", "#0000da")) +
  theme_minimal()
gender.1

#Smoke
cardio.final$smoke.yn<- as.factor(ifelse(cardio.final$smoke== "1", "Yes", "No"))

smoke.1 <- ggplot(cardio.final, aes(x=cardio, fill= smoke.yn)) +
  geom_bar(position="fill") +
  scale_x_discrete(labels=c("1" = "Has CVD", "0" = "Does NOT Have CVD")) +
  scale_y_continuous(labels = scales::percent) +
  labs( y = "", x = "", fill = "Smoker") +
  scale_fill_manual(values=c("#990000", "#0000da")) +
  theme_minimal()
smoke.1

#alco
cardio.final$alco.yn<- as.factor(ifelse(cardio.final$alco== "1", "Yes", "No"))

alco.1 <- ggplot(cardio.final, aes(x=cardio, fill= alco.yn)) +
  geom_bar(position="fill") +
  scale_x_discrete(labels=c("1" = "Has CVD", "0" = "Does NOT Have CVD")) +
  scale_y_continuous(labels = scales::percent) +
  labs( y = "", x = "", fill = "Drinks Alcohol") +
  scale_fill_manual(values=c("#990000", "#0000da")) +
  theme_minimal()
alco.1

#### EDA Graphics: Continuous Variables ####

#age
cvdage<- filter(cardio.final, cardio.yn == "Yes")
mean(cvdage$age) #54.98427

n.cvdage<- filter(cardio.final, cardio.yn == "No")
mean(n.cvdage$age) #51.81747

age.kde <- ggplot(cardio.final, aes(x = age, fill= cardio.yn)) + 
  geom_density(position = "stack") +
  scale_x_continuous(limits = c(30,70)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "CVD Patients are Typically Older Than CVD Patients",
       subtitle = "Age Distribution (Stacked)",
       fill = "Has CVD", x = "Age", y = "Distribution") +
  scale_fill_manual(values=c("#990000", "#0000da")) +
  theme_minimal() +
  annotate(geom = "segment", x = 54.98, xend = 54.98, y = 0, yend = .053, size = 1, color = "steelblue") +
  annotate(geom = "text", x= 56, y =.015, label = "Mean Age of CVD", size = 4,
           hjust = 0, color = "steelblue", fontface = 2) +
  annotate(geom = "text", x= 56, y =.01, label = "Patients is 55", size = 4,
           hjust = 0, color = "steelblue", fontface = 2) +
  annotate(geom = "segment", x = 51.81, xend = 51.81, y = 0, yend = .1, size = 1, color = "pink3") +
  annotate(geom = "text", x= 53, y =.08, label = "Mean Age of Healthy", size = 4,
           hjust = 0, color = "pink3", fontface = 2) +
  annotate(geom = "text", x= 53, y =.075, label = "Patients is 52", size = 4,
           hjust = 0, color = "pink3", fontface = 2)
age.kde

#bmi
cvd.bmi<- filter(cardio.final, cardio.yn == "Yes")
mean(cvd.bmi$bmi) #27.83434

ncvd.bmi<- filter(cardio.final, cardio.yn == "No")
mean(ncvd.bmi$bmi) #26.25536

bmi.kde <- ggplot(cardio.final, aes(x = bmi, fill= cardio.yn)) + 
  geom_density(position = "stack") +
  scale_x_continuous(breaks = c(seq(0, 40, 5))) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "CVD Patients  Typically Have Higher BMIs Than Non-CVD Patients",
       subtitle = "BMI Distribution (Stacked)",
       fill = "Has CVD", x = "BMI", y = "Distribution") +
  scale_fill_manual(values=c("#990000", "#0000da")) +
  theme_minimal()+
  annotate(geom = "segment", x = 27.83, xend = 27.83, y = 0, yend = .077, size = 1, color = "steelblue") +
  annotate(geom = "text", x= 28.5, y =.04, label = "Mean BMI of CVD", size = 4,
           hjust = 0, color = "steelblue", fontface = 2) +
  annotate(geom = "text", x= 28.5, y =.03, label = "Patients is 28", size = 4,
           hjust = 0, color = "steelblue", fontface = 2) +
  annotate(geom = "segment", x = 26.25, xend = 26.25, y = 0, yend = .175, size = 1, color = "pink3") +
  annotate(geom = "text", x= 27, y =.185, label = "Mean Age of Healthy", size = 4,
           hjust = 0, color = "pink3", fontface = 2) +
  annotate(geom = "text", x= 27, y =.175, label = "Patients is 26", size = 4,
           hjust = 0, color = "pink3", fontface = 2)

bmi.kde


#height
cvd.height<- filter(cardio.final, cardio.yn == "Yes")
mean(cvd.height$height) #164.5428

ncvd.height<- filter(cardio.final, cardio.yn == "No")
mean(ncvd.height$height) #164.7847

#weight
cvd.weight<- filter(cardio.final, cardio.yn == "Yes")
mean(cvd.weight$weight) #75.31369

ncvd.weight<- filter(cardio.final, cardio.yn == "No")
mean(ncvd.weight$weight) #71.27051

weight.kde <- ggplot(cardio.final, aes(x = weight, fill= cardio.yn)) + 
  geom_density(position = "stack") +
  scale_x_continuous(breaks = c(seq(0, 40, 5))) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "CVD Patients  Typically Have Higher weights Than Non-CVD Patients",
       subtitle = "weight Distribution (Stacked)",
       fill = "Has CVD", x = "weight", y = "Distribution") +
  scale_fill_manual(values=c("#990000", "#0000da")) +
  theme_minimal()+
  annotate(geom = "segment", x = 27.83, xend = 27.83, y = 0, yend = .077, size = 1, color = "steelblue") +
  annotate(geom = "text", x= 28.5, y =.04, label = "Mean weight of CVD", size = 4,
           hjust = 0, color = "steelblue", fontface = 2) +
  annotate(geom = "text", x= 28.5, y =.03, label = "Patients is 28", size = 4,
           hjust = 0, color = "steelblue", fontface = 2) +
  annotate(geom = "segment", x = 26.25, xend = 26.25, y = 0, yend = .175, size = 1, color = "pink3") +
  annotate(geom = "text", x= 27, y =.185, label = "Mean Age of Healthy", size = 4,
           hjust = 0, color = "pink3", fontface = 2) +
  annotate(geom = "text", x= 27, y =.175, label = "Patients is 26", size = 4,
           hjust = 0, color = "pink3", fontface = 2)
#aphi
...

#aplo






