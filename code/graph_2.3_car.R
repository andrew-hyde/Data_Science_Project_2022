
graph_2.3_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){


    library(tidyverse)
    library(lubridate)

    car_data_2.3 <- car_df_data %>%
        mutate(brand = word(car_df_data$name, 1)) %>% # short name of car to first 3 letters of car brand
        #mutate(engine = as.numeric(gsub("[ CC]","", as.character(engine)))) %>% # remove 'CC' from engine column
        #mutate(petrol = ifelse(fuel=="Petrol", 1, 0)) %>% # dummy variable: 1 = if car is 'petrol'
        #mutate(dealership = ifelse(seller_type=="Dealer", 1, 0)) %>% # dummy variable: 1 = if seller is 'dealer'
        #mutate(automatic = ifelse(transmission=="Automatic", 1, 0)) %>% # dummy variable: 1 = if car is 'manual'
        mutate(age = (2021-year)) %>% # calculate age of car
        mutate(log_selling_price = log(selling_price), log_km_driven = log(km_driven)) %>%
        mutate(price = log_selling_price, mileage = log_km_driven) %>% # change variable names
        na.omit() %>%
        dplyr::select(fuel)


    ##############


    graph <- car_data_2.3 %>% ggplot() +

        geom_bar(aes(x = fuel, fill = fuel), stat = "count") +

        theme_bw() +

        labs(title = title,
             subtitle = subtitle,
             caption = caption,
             x = xlabel,
             y = ylabel) +

        theme() +
        theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
        theme(legend.position="bottom") +
        theme(legend.title=element_blank())

    graph

}


