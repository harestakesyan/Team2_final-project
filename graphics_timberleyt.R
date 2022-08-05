#### Startup ####

#Libraries
library(ezids)
library(tidyverse)

#Data
cardio_orig = data.frame(read.csv("cardio_train.csv", header = TRUE, sep=';'))
str(cardio_orig)

cardio = cardio_orig[-c(1)] #get rid of id
cardio$age = cardio$age/365 # transfer days to years
cardio$bmi = cardio$weight/((cardio$height/100)**2) #add bmi variable

cardio_clean = outlierKD2(cardio, bmi, TRUE, TRUE, TRUE, TRUE) 
cardio_clean = outlierKD2(cardio_clean, ap_hi, TRUE, TRUE, TRUE, TRUE) 
cardio_clean = outlierKD2(cardio_clean, ap_lo, TRUE, TRUE, TRUE, TRUE)
cardio_clean = na.omit(cardio_clean) #remove NA rows

cardio_clean_final = cardio_clean
cardio_clean_final$gender = as.factor(cardio_clean$gender)
cardio_clean_final$cholesterol = as.factor(cardio_clean$cholesterol) 
cardio_clean_final$gluc = as.factor(cardio_clean$gluc)
cardio_clean_final$smoke = as.factor(cardio_clean$smoke)
cardio_clean_final$alco = as.factor(cardio_clean$alco)
cardio_clean_final$active = as.factor(cardio_clean$active)
cardio_clean_final$cardio = as.factor(cardio_clean$cardio)


#### Categorical Variables ####
#cholesterol
cholesterol.1 <- ggplot(cardio_clean_final, aes(x=cardio, fill= cholesterol)) +
  geom_bar(position="fill") +
  scale_x_discrete(labels=c("1" = "Has CVD", "0" = "NO CVD")) +
  scale_y_continuous(labels = scales::percent) +
  labs( title = "Cholesterol", y = "", x = "",
        fill = "Cholesterol Level") +
  scale_fill_manual(values=c("#f4cccc", "#e06666", "#990000"), 
                    labels = c("Normal", "Above Normal", "Well Above Normal")) +
  theme_minimal() +
  theme(text = element_text(size = 20))

#Glucose
gluc.1 <- ggplot(cardio_clean_final, aes(x=cardio, fill= gluc)) +
  geom_bar(position="fill") +
  scale_x_discrete(labels=c("1" = "Has CVD", "0" = "NO CVD")) +
  scale_y_continuous(labels = scales::percent) +
  labs( title = "Glucose", y = "", x = "",
        fill = "Glucose Level") +
  scale_fill_manual(values=c("#f4cccc", "#e06666", "#990000"),
                    labels = c("Normal", "Above Normal", "Well Above Normal")) +
  theme_minimal() +
  theme(text = element_text(size = 20))

#Gender
cardio_clean_final$gender.yn<- as.factor(ifelse(cardio_clean_final$gender== "1", "Women", "Men"))

gender.1 <- ggplot(cardio_clean_final, aes(x=cardio, fill= gender.yn)) +
  geom_bar(position="fill") +
  scale_x_discrete(labels=c("1" = "Has CVD", "0" = "NO CVD")) +
  scale_y_continuous(labels = scales::percent) +
  labs( title = "Gender",
        y = "", x = "", fill = "") +
  scale_fill_manual(values=c("#0000da", "#990000")) +
  theme_minimal() +
  theme(text = element_text(size = 20))

#Smoke
cardio_clean_final$smoke.yn<- as.factor(ifelse(cardio_clean_final$smoke== "1", "Yes", "No"))

smoke.1 <- ggplot(cardio_clean_final, aes(x=cardio, fill= smoke.yn)) +
  geom_bar(position="fill") +
  scale_x_discrete(labels=c("1" = "Has CVD", "0" = "NO CVD")) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Tobacco Use", y = "", x = "", fill = "Smoker") +
  scale_fill_manual(values=c("#0000da", "#990000")) +
  theme_minimal() +
  theme(text = element_text(size = 20))

#alcohol
cardio_clean_final$alco.yn<- as.factor(ifelse(cardio_clean_final$alco== "1", "Yes", "No"))

alco.1 <- ggplot(cardio_clean_final, aes(x=cardio, fill= alco.yn)) +
  geom_bar(position="fill") +
  scale_x_discrete(labels=c("1" = "Has CVD", "0" = "NO CVD")) +
  scale_y_continuous(labels = scales::percent) +
  labs( title = "Alcohol Use", y = "", x = "", fill = "Drinks Alcohol") +
  scale_fill_manual(values=c("#0000da", "#990000")) +
  theme_minimal() +
  theme(text = element_text(size = 20))

#active
cardio_clean_final$active.yn<- as.factor(ifelse(cardio_clean_final$active== "1", "Yes", "No"))

active.1 <- ggplot(cardio_clean_final, aes(x=cardio, fill= active.yn)) +
  geom_bar(position="fill") +
  scale_x_discrete(labels=c("1" = "Has CVD", "0" = "NO CVD")) +
  scale_y_continuous(labels = scales::percent) +
  labs( title = "Physical Activity", y = "", x = "", fill = "Physically Active") +
  scale_fill_manual(values=c("#0000da", "#990000")) +
  theme_minimal() +
  theme(text = element_text(size = 20))

#### Continuous Variables (w/ annotations) ####

disease <- subset(cardio_clean_final, cardio==1)
healthy <- subset(cardio_clean_final, cardio==0)

#age
cardio_clean_final$cardio.yn<- as.factor(ifelse(cardio_clean_final$active== "1", "Yes", "No"))

age.kde <- ggplot(cardio_clean_final, aes(x = age, fill= cardio.yn)) + 
  geom_density(position = "stack") +
  geom_vline(aes(xintercept=mean(disease$age)), color="pink3", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mean(healthy$age)), color="steelblue", linetype="dashed", size=1) +
  scale_x_continuous(limits = c(30,70)) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Age",
       fill = "Has CVD", x = "Age", y = " ") +
  scale_fill_manual(values=c("#0000da", "#990000")) +
  theme_minimal() +
  annotate(geom = "text", x= 56, y =.015, label = "Mean Age of CVD", size = 5,
           hjust = 0, color = "pink3", fontface = 2) +
  annotate(geom = "text", x= 56, y =.01, label = "Patients is 55", size = 5,
           hjust = 0, color = "pink3", fontface = 2) +
  annotate(geom = "text", x= 39, y =.08, label = "Mean Age of Healthy", size = 5,
           hjust = 0, color = "steelblue", fontface = 2) +
  annotate(geom = "text", x= 39, y =.075, label = "Patients is 52", size = 5,
           hjust = 0, color = "steelblue", fontface = 2) +
  theme(text = element_text(size = 20))

age.kde

#height
height.kde <- ggplot(cardio_clean_final, aes(x = height, fill= cardio.yn)) + 
  geom_density(position = "stack") +
  geom_vline(aes(xintercept=mean(disease$height)), color="pink3", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mean(healthy$height)), color="steelblue", linetype="dashed", size=1) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Height",
       fill = "Has CVD", x = "Height (cm)", y = " ") +
  scale_fill_manual(values=c("#0000da", "#990000")) +
  theme_minimal() +
  theme(text = element_text(size = 20))

height.kde

#weight
weight.kde <- ggplot(cardio_clean_final, aes(x = weight, fill= cardio.yn)) + 
  geom_density(position = "stack") +
  geom_vline(aes(xintercept=mean(disease$weight)), color="pink3", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mean(healthy$weight)), color="steelblue", linetype="dashed", size=1) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Weight",
       fill = "Has CVD", x = "Weight (kg)", y = " ") +
  scale_fill_manual(values=c("#0000da", "#990000")) +
  theme_minimal() +
  annotate(geom = "text", x= 100, y =.025, label = "Mean Weight of CVD", size = 5,
           hjust = 0, color = "pink3", fontface = 2) +
  annotate(geom = "text", x= 100, y =.02, label = "Patients is 75kg", size = 5,
           hjust = 0, color = "pink3", fontface = 2) +
  annotate(geom = "text", x= 30, y =.06, label = "Mean Weight of Healthy", size = 5,
           hjust = 0, color = "steelblue", fontface = 2) +
  annotate(geom = "text", x= 30, y =.055, label = "Patients is 71kg", size = 5,
           hjust = 0, color = "steelblue", fontface = 2) +
  theme(text = element_text(size = 20))

weight.kde


#bmi
bmi.kde <- ggplot(cardio_clean_final, aes(x = bmi, fill= cardio.yn)) + 
  geom_density(position = "stack") +
  geom_vline(aes(xintercept=mean(disease$bmi)), color="pink3", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mean(healthy$bmi)), color="steelblue", linetype="dashed", size=1) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "BMI",
       fill = "Has CVD", x = "BMI", y = " ") +
  scale_fill_manual(values=c("#0000da", "#990000")) +
  theme_minimal() +
  annotate(geom = "text", x= 28.5, y =.04, label = "Mean BMI of CVD", size = 5,
           hjust = 0, color = "pink3", fontface = 2) +
  annotate(geom = "text", x= 28.5, y =.03, label = "Patients is 28", size = 5,
           hjust = 0, color = "pink3", fontface = 2) +
  annotate(geom = "text", x= 16, y =.185, label = "Mean BMI of Healthy", size = 5,
           hjust = 0, color = "steelblue", fontface = 2) +
  annotate(geom = "text", x= 16, y =.175, label = "Patients is 26", size = 5,
           hjust = 0, color = "steelblue", fontface = 2) +
  theme(text = element_text(size = 20))

bmi.kde

#systolic bp
ap_hi.kde <- ggplot(cardio_clean_final, aes(x = ap_hi, fill= cardio.yn)) + 
  geom_density(position = "stack") +
  geom_vline(aes(xintercept=mean(disease$ap_hi)), color="pink3", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mean(healthy$ap_hi)), color="steelblue", linetype="dashed", size=1) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(seq(90,170,20))) +
  labs(title = "Systolic BP",
       fill = "Has CVD", x = "Systolic BP", y = " ") +
  scale_fill_manual(values=c("#0000da", "#990000")) +
  theme_minimal() +
  annotate(geom = "text", x= 135, y =.09, label = "Mean SBP of CVD", size = 5,
           hjust = 0, color = "pink3", fontface = 2) +
  annotate(geom = "text", x= 135, y =.08, label = "Patients is 133", size = 5,
           hjust = 0, color = "pink3", fontface = 2) +
  annotate(geom = "text", x= 97, y =.15, label = "Mean SBP of Healthy", size = 5,
           hjust = 0, color = "steelblue", fontface = 2) +
  annotate(geom = "text", x= 97, y =.14, label = "Patients is 120", size = 5,
           hjust = 0, color = "steelblue", fontface = 2) +
  theme(text = element_text(size = 20))

ap_hi.kde

#diastolic bp
ap_lo.kde <- ggplot(cardio_clean_final, aes(x = ap_lo, fill= cardio.yn)) + 
  geom_density(position = "stack") +
  geom_vline(aes(xintercept=mean(disease$ap_lo)), color="pink3", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mean(healthy$ap_lo)), color="steelblue", linetype="dashed", size=1) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(seq(65,105,10))) +
  labs(title = "Diastolic BP",
       fill = "Has CVD", x = "Diastolic BP", y = " ") +
  scale_fill_manual(values=c("#0000da", "#990000")) +
  theme_minimal() +
  annotate(geom = "text", x= 86, y =.25, label = "Mean DBP of CVD", size = 5,
           hjust = 0, color = "pink3", fontface = 2) +
  annotate(geom = "text", x= 86, y =.22, label = "Patients is 84", size = 5,
           hjust = 0, color = "pink3", fontface = 2) +
  annotate(geom = "text", x= 68, y =.45, label = "Mean DBP of Healthy", size = 5,
           hjust = 0, color = "steelblue", fontface = 2) +
  annotate(geom = "text", x= 68, y =.42, label = "Patients is 79", size = 5,
           hjust = 0, color = "steelblue", fontface = 2) +
  theme(text = element_text(size = 20))

ap_lo.kde


#### Correlations ####

library(patchwork)
#install.packages("ggExtra")
#library(ggExtra)

ap_hi_lo <- ggplot(cardio_clean_final, aes(x=ap_lo, y=ap_hi, color=cardio)) +
  geom_point(size = 2.5, alpha=0.7) + 
  labs(title = "Systolic vs Diastolic Blood Pressure", color='Has CVD') + 
  xlab("Diastolic BP ") + ylab("Systolic BP") +
  scale_color_manual(values=c("#0000da", "#990000"), labels = c("No", "Yes")) +
  theme_minimal() +
  theme(text = element_text(size = 15),
        legend.position = "none")

ap_hi_lo

ap1_hi <- ggplot(cardio_clean_final,aes(x = cholesterol , y = ap_hi, fill = cardio)) + 
  geom_violin(scale="width", width=1, alpha=.5) + 
  geom_boxplot(width=.1, cex=.5, position=position_dodge(1))+
  labs(title = "Systolic Blood Pressure vs Cholesterol", fill='Has CVD') +
  xlab("Cholesterol") + ylab("Systolic BP") + 
  scale_fill_manual(values=c("#0000da", "#990000"),labels=c("No", "Yes")) +
  scale_x_discrete(labels=c("1" = "normal", "2" = "above normal","3" = "well above normal")) +
  theme_minimal()

ap1_hi

ap2 <- ggplot(cardio_clean_final, aes(x=age, y=ap_hi, color=cardio)) +
  geom_point(size = 1.5, alpha=0.7) + 
  labs(title = "Systolic Blood Pressure vs Age", color="Has CVD") + 
  xlab("Age ") + ylab("Systolic BP") +
  scale_color_manual(values=c("#0000da", "#990000"), labels = c("No", "Yes")) +
  theme_minimal()

ap2


ap3 <- ggplot(cardio_clean_final, aes(x=bmi, y=ap_hi, color=cardio)) +
  geom_point(size=1.5,alpha=0.7) + 
  labs(title = "Systolic Blood Pressure vs BMI", color="Has CVD") + 
  xlab("BMI ") + ylab("Systolic BP") +
  scale_color_manual(values=c("#0000da", "#990000"), labels = c("No", "Yes")) +
  theme_minimal() +
  theme(text = element_text(size = 20))

ap3


require(ggpubr)
ggarrange(ap1_hi, ap_hi_lo, ap2, ap3, common.legend = TRUE, legend = "bottom")
































