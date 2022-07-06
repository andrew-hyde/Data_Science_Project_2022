
graph_2.4_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){

library(pacman)
library(recipes)
library(tidyverse)
library(lubridate)

car_data_2.4 <- car_df_data %>%
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

    #mutate(petrol = ifelse(fuel=="Petrol", 1, 0)) %>% # dummy variable: 1 = if car is 'petrol'
    #mutate(dealership = ifelse(seller_type=="Dealer", 1, 0)) %>% # dummy variable: 1 = if seller is 'dealer'
    #mutate(automatic = ifelse(transmission=="Automatic", 1, 0)) %>% # dummy variable: 1 = if car is 'manual'
    na.omit() %>%
    dplyr::select(c(age, km_driven, mileage, seats, max_power, engine)) %>%
    gather(label, value)


##############


    graph <- car_data_2.4 %>% ggplot() +

        geom_histogram(aes(x = value, fill = label), bins = 100, colour = "black") +
        #    geom_text(aes(x = number, y = value, label = value),  position = position_stack(vjust = 1)) +
        #scale_color_gradient(low="blue", high="red") +
    #geom_bar(aes(x = continent, y = value, fill = label), stat = "identity") +
    #geom_text(aes(x = continent, y = value, label = value), vjust = 0) +

    facet_wrap(~label, scales = "free_y", nrow = 2) +

        theme_bw() +

        # guides(color = FALSE, fill = FALSE, alpha = FALSE) +

        # Add titles:
        labs(title = title,
             subtitle = subtitle,
             caption = caption,
             x = xlabel,
             y = ylabel) +

        theme() +
        theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
        theme(legend.position="") +
        theme(legend.title=element_blank())

    graph

}


