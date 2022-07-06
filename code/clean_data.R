################################################################################

#  Data Wrangling

################################################################################

clean_data <- function(df_data){

library(tidyverse)
library(lubridate)
library(readr)
library(stringr)
library(fastDummies)

# import data
car_df_data <- read_csv("data/Car details v3.csv", show_col_types = FALSE)

# clean data
cleaned_car_data <- car_df_data %>%
    mutate(brand = word(car_df_data$name, 1)) %>% # short name of car to first 3 letters of car brand
    mutate(engine = as.numeric(gsub("[ CC]","", as.character(engine)))) %>% # remove 'CC' from column
    mutate(mileage = as.numeric(gsub("[ kmpl|km/kg]","", as.character(mileage)))) %>% # remove 'kmpl' from column
    mutate(max_power = as.numeric(gsub("[ bhp]","", as.character(max_power)))) %>% # remove 'km/kg' from column
    filter(!grepl("LPG|CNG", fuel)) %>% # remove 'LPG' and 'CNG"
    mutate(age = (2021-year)) %>% # calculate age of car

    mutate(price = log(selling_price)) %>%
    mutate(age = log(age)) %>%
    mutate(km_driven = log(km_driven)) %>%
    mutate(mileage = log(mileage)) %>%
    mutate(seats = log(seats)) %>%
    mutate(max_power = log(max_power)) %>%
    mutate(engine = log(engine)) %>%

    filter(!grepl("-Inf", mileage)) %>% # remove -Inf

    mutate(petrol = ifelse(fuel=="Petrol", 1, 0)) %>% # dummy variable: 1 = if car is 'petrol'
    mutate(individual = ifelse(seller_type=="Individual", 1, 0)) %>% # dummy variable: 1 = if seller is 'individual'
    mutate(automatic = ifelse(transmission=="Automatic", 1, 0)) %>% # dummy variable: 1 = if car is 'manual'

dplyr::select(c(price, km_driven, age, engine, max_power, mileage, engine, seats, petrol, individual, automatic, owner)) %>%
    na.omit()


# create dummy variables for the different car brands
#cleaned_car_data <- car_data %>%
#    dummy_cols(cleaned_car_data, select_columns = c("owner"), remove_first_dummy = TRUE)

}


