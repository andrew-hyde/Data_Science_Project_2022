graph_2.5_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){


    library(tidyverse)
    library(lubridate)

    car_data_2.5 <- car_df_data %>%
        mutate(brand = word(car_df_data$name, 1)) %>% # short name of car to first 3 letters of car brand
        dplyr::select(seller_type) %>% na.omit()


    ##############


    graph <- car_data_2.5 %>% ggplot() +

        geom_bar(aes(x = seller_type, fill = seller_type), stat = "count") +

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



