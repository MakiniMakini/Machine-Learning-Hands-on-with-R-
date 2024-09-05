# Helper packages
library(dplyr)    # for data manipulation
library(ggplot2)  # for awesome graphics

# Modeling packages
library(caret)    # for cross-validation, etc.
library(rsample)   # for resampling procedures
library(h2o)       # for resampling and model training
# Model interpretability packages
library(vip)      # variable importance

library(AmesHousing)

# Access the Ames dataset
ames_data <- make_ames()

# Stratified sampling with the rsample package
set.seed(123)
split <- initial_split(ames_data, prop = 0.7, 
                       strata = "Sale_Price")
ames_train  <- training(split)
ames_test   <- testing(split)

# start modeling of the data
# model 1 - simple
model1 <- lm(Sale_Price ~ Gr_Liv_Area, data = ames_train)

summary(model1) 
sigma(model1)
confint(model1, level = 0.95)

# model 2
(model2 <- lm(Sale_Price ~ Gr_Liv_Area + Year_Built, data = ames_train))
# or
(model2 <- update(model1, . ~ . + Year_Built))
summary(model2)
