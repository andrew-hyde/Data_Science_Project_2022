
graph_3.0_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){

    library(tidyverse)
    library(lubridate)

# IMPORT DATA
cleaned_car_data <- clean_data(df_data)

# SPLIT DATA
# set seed for reproducibility
set.seed(123)
library(rsample)
# split data 70:30, training and test set

#Test set
data_test <- testing(rsample::initial_split(cleaned_car_data, prop = 0.8, strata = "price"))
#Training set
data_train <- training(rsample::initial_split(cleaned_car_data, prop = 0.8, strata = "price"))

# Density plot of log price for the training and test sets
library(tidyverse)


car_data_3.0 <- bind_rows(

    data_test %>%
    mutate(label = "test") %>%
    dplyr::select(c(label, price)) %>%
    na.omit(),

    data_train %>%
    mutate(label = "train") %>%
    dplyr::select(c(label, price)) %>%
    na.omit())


##############

graph <- car_data_3.0 %>% ggplot() +

        geom_density(aes(x = price, colour = label),  size = 1.5, alpha = 0.5) +

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



