

graph_2.9_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){

    library(tidyverse)
    library(lubridate)

car_data_2.9 <- car_df_data %>%
        dplyr::select(owner) %>%
        group_by(owner) %>%
        count(owner) %>%
        na.omit()

#############

graph <- car_data_2.9 %>% ggplot() +

        geom_bar(aes(x = reorder(owner, n), y = n, fill = owner), stat = "identity") +
        geom_text(aes(x = owner, y = n, label = n), vjust = 0) +

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


