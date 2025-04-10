###Data Cleaning for Project###
### Manually coded classes for indivisual accounts for all years ###
View("2019P_L")


### Transpose 2019 Dataset##
transposed_data2019 <- t(2019P_L)
P_L2019t <- as.data.frame(transposed_data2019)

### Need to create a year column for every row ###
P_L2019t$year <- 2019

### Transpose 2020 Dataset##
transposed_data2020 <- t(2020P_L)
P_L2020t <- as.data.frame(transposed_data2020)

### Need to create a year column for every row ###
P_L2020t$year <- 2020

### Transpose 2021 Dataset##
transposed_data2021 <- t(2021P_L)
P_L2019t <- as.data.frame(transposed_data2019)

### Need to create a year column for every row ###
P_L2019t$year <- 2019