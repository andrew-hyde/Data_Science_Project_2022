
graph_2.8_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){


    library(tidyverse)
    library(lubridate)

    car_data_2.8 <- car_df_data %>%
        mutate(age = (2021-year)) %>% # calculate age of car
        mutate(brand = word(car_df_data$name, 1)) %>% # shorten, car brand name
        dplyr::select(selling_price, age) %>%
        group_by(age) %>%
        summarise(across(everything(), mean)) %>%
        na.omit()

    ##############

    graph <- car_data_2.8 %>% ggplot() +

        geom_col(aes(x = age, y=selling_price, fill = age)) +
        #geom_bar(aes(x = transmission, y = selling_price, fill = transmission), stat = "identity") +


        theme_bw() +

        labs(title = title,
             subtitle = subtitle,
             caption = caption,
             x = xlabel,
             y = ylabel) +


        theme() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(legend.position="") +
        theme(legend.title=element_blank())

    graph

}

graph_2.8_func(df_data = car_data_2.8,
               title = "Average Price of Cars by Vehicle Age",
               subtitle = "",
               caption = " ",
               xlabel = "Age",
               ylabel = "Price")


