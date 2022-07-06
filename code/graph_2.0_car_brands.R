
graph_2.0_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){


library(tidyverse)
library(lubridate)

car_data_2.0 <- car_df_data %>%
    mutate(brand = word(car_df_data$name, 1)) %>% # short name of car to first 3 letters of car brand
    dplyr::select(brand) %>%
    na.omit() %>%
    count(brand)


##############


graph <- car_data_2.0 %>% ggplot() +

        geom_col(aes(x = reorder(brand, n), y=n, fill = brand)) +
        #    geom_text(aes(x = number, y = value, label = value),  position = position_stack(vjust = 1)) +
        #scale_color_gradient(low="blue", high="red") +

        theme_bw() +

        # guides(color = FALSE, fill = FALSE, alpha = FALSE) +

        # Add titles:
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



