##Manually coded class due to the columns being indivisual account names###




### Transpose 2019 Dataset and add year column ##
transposed_data2019 <- t(P_L_2019)
P_L2019t <- as.data.frame(transposed_data2019)
P_L2019t$year <- 2019

### Transpose 2020 Dataset and add year column ##
transposed_data2020 <- t(P_L_2020)
P_L2020t <- as.data.frame(transposed_data2020)
P_L2020t$year <- 2020

### Transpose 2021 Dataset and add year column ##
transposed_data2021 <- t(P_L_2021)
P_L2021t <- as.data.frame(transposed_data2021)
P_L2021t$year <- 2021

### Transpose 2022 Dataset and add year column ##
transposed_data2022 <- t(P_L_2022)
P_L2022t <- as.data.frame(transposed_data2022)
P_L2022t$year <- 2022

### Transpose 2023 Dataset and add year column ##
transposed_data2023 <- t(P_L_2023)
P_L2023t <- as.data.frame(transposed_data2023)
P_L2023t$year <- 2023

###Merge all datasets together####
# Combine all years into one dataset
PandL <- rbind(P_L2019t, P_L2020t, P_L2021t, P_L2022t, P_L2023t)

#### visually look at merged dataset###
View(PandL)
