
graph_2.7_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){


library(tidyverse)
library(lubridate)

car_data_2.7 <- car_df_data %>%
            mutate(brand = word(car_df_data$name, 1)) %>% # shorten, car brand name
            dplyr::select(selling_price, brand) %>%
            group_by(brand) %>%
            summarise(across(everything(), mean)) %>%
            na.omit()

##############

graph <- car_data_2.7 %>% ggplot() +

    geom_col(aes(x = reorder(brand, selling_price), y=selling_price, fill = brand)) +
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

