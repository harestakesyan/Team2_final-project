# R Script to generate "cardio_clean_final"

#read in original data set
cardio_orig = data.frame(read.csv("cardio_train.csv", header = TRUE, sep=';'))

#remove id column, convert age to years, add bmi col
cardio = cardio_orig[-c(1)]
cardio$age = cardio$age/365
cardio$bmi = cardio$weight/(cardio$height/100)**2

#remove outliers from bmi, api_hi and api_lo
cardio_clean = outlierKD2(cardio, bmi, TRUE, TRUE, TRUE, TRUE) 
cardio_clean = outlierKD2(cardio_clean, ap_hi, TRUE, TRUE, TRUE, TRUE) 
cardio_clean = outlierKD2(cardio_clean, ap_lo, TRUE, TRUE, TRUE, TRUE)

#remove NA rows
cardio_clean = na.omit(cardio_clean)

#change variables to factor
cardio_clean_final <- transform(cardio_clean, gender=as.factor(gender),
                                alco = as.factor(alco),
                                smoke = as.factor(smoke), 
                                active = as.factor(active), 
                                cholesterol=as.factor(cholesterol), 
                                gluc= as.factor(gluc), 
                                cardio=as.factor(cardio))