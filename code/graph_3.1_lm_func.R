
lm_func_1_to_10 <- function(df_data){

# Simple Linear Regression
model_1 <- lm(price ~ km_driven, data_train)
model_2 <- lm(price ~ km_driven + age, data_train)
model_3 <- lm(price ~ km_driven + age + engine, data_train)
model_4 <- lm(price ~ km_driven + age + engine + mileage, data_train)
model_5 <- lm(price ~ km_driven + age + engine + mileage + max_power, data_train)
model_6 <- lm(price ~ km_driven + age + engine + mileage + max_power + seats, data_train)
model_7 <- lm(price ~ km_driven + age + engine + mileage + max_power + seats + petrol, data_train)
model_8 <- lm(price ~ km_driven + age + engine + mileage + max_power + seats + petrol + individual, data_train)
model_9 <- lm(price ~ km_driven + age + engine + mileage + max_power + seats + petrol + individual + automatic, data_train)
model_10 <- lm(price ~ km_driven + age + engine + mileage + max_power + seats + petrol + individual + automatic + owner, data_train)

library(huxtable)
t <- huxreg(model_1, model_2, model_3, model_4, model_5, model_6, model_7, model_8, model_9, model_10)
t

}

