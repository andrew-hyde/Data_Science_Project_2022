
graph_2.1_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){


    library(tidyverse)
    library(lubridate)

car_data_2.1 <- car_df_data %>%
        mutate(brand = word(car_df_data$name, 1)) %>% # short name of car to first 3 letters of car brand
        mutate(log_selling_price = log(selling_price)) %>%
        mutate(price = log_selling_price) %>% # change variable names
        na.omit() %>%
    dplyr::select(price)


##############


    graph <- car_data_2.1 %>% ggplot() +

        geom_histogram(aes(x = price), bins = 70, colour = "black", fill = "lightgreen") +

        theme_bw() +

        labs(title = title,
             subtitle = subtitle,
             caption = caption,
             x = xlabel,
             y = ylabel) +

        theme() +
        theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
        theme(legend.position="bottom") +
        theme(legend.title=element_blank())

    graph

}


