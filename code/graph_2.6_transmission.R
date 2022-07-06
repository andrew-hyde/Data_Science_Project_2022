graph_2.6_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){


    library(tidyverse)
    library(lubridate)
car_data_2.6 <- car_df_data %>%
        dplyr::select(selling_price, transmission) %>%
        group_by(transmission) %>%
        summarise(across(everything(), mean)) %>%
        na.omit()

# car_data_2.6 <- car_df_data %>%
    #     mutate(price = log(selling_price)) %>%
    #     dplyr::select(price, transmission) %>%
    #     group_by(transmission) %>%
    #     summarise(across(everything(), mean)) %>%
    #     na.omit()


##############


    graph <- car_data_2.6 %>% ggplot() +

        geom_bar(aes(x = transmission, y = selling_price, fill = transmission), stat = "identity") +

        theme_bw() +

        labs(title = title,
             subtitle = subtitle,
             caption = caption,
             x = xlabel,
             y = ylabel) +

        theme() +
        theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
        theme(legend.position="") +
        theme(legend.title=element_blank())

    graph

}

