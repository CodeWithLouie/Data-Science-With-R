
# House price prediction - kaggle competition
# load the dplyr and readr package
# import dataset

library(dplyr)

library(readr)

train <- read_csv("train.csv")

test <- read_csv("test.csv")

# examine the structure of the data

glimpse(train)
 
# this particular data poses a challenge since there are many variables
# knowing the variables with the most effective predictive behavior is
# a huge task, this is my second try at predicting on a continuous scale
# Took nearly 20 minutes going through each variable descriptions

# Exploratory data analysis
# view the distribution of prices

library(ggplot2)

ggplot(train, aes(SalePrice)) +
  geom_histogram()

# house price distribution is right, so we would try to normalize the data
# to fit a normal distribution, first try sort and log and use the best option

ggplot(train, aes(sqrt(SalePrice))) +
  geom_histogram() # still a bit skewed

ggplot(train, aes(log(SalePrice))) +
  geom_histogram()

# Checking the level of outliers as they are known to affect predicitons

ggplot(train, aes(SalePrice)) + 
  geom_boxplot() +
  coord_flip() # Outlier are mostly expensive houses 

# Building our model 
# first model

fmla <- as.formula("SalePrice ~ MSZoning + MSSubClass + Condition1 +
                   LotArea + Neighborhood + HouseStyle + OverallCond")

lm.model <- lm(fmla, data = train)

# second model 

fmla2 <- as.formula("SalePrice ~ MSZoning + MSSubClass + Condition1 + LotArea +
Neighborhood + OverallCond + BsmtCond + Electrical + KitchenAbvGr + 
                    OpenPorchSF + YrSold + SaleType + SaleCondition")

lm.model2 <- lm(fmla2, data = train)

# third model

fmla3 <- as.formula("SalePrice ~ OverallQual + YearBuilt")

lm.model3 <- lm(fmla3, data = train)

# fourth model

fmla4 <- as.formula("SalePrice ~ OverallQual + LotArea + Neighborhood +
                    SaleType")

lm.model4 <- lm(fmla4, data = train)

# fifth model

fmla5 <- as.formula("SalePrice ~ OverallQual + LotArea + Neighborhood +
                    SaleType + MSSubClass + BldgType + KitchenQual +
                    GarageArea + LotFrontage + ExterQual + RoofMatl + BsmtQual")

lm.model5 <- lm(fmla5, data = train)

# Examine our model performance

library(sigr) 

library(broom)


augment(lm.model) %>% 
  arrange(desc(.cooksd)) %>% 
  head()

augment(lm.model2) %>% 
  arrange(desc(.cooksd)) %>% 
  head()

augment(lm.model3) %>% 
  arrange(desc(.cooksd)) %>% 
  head()

augment(lm.model4) %>% 
  arrange(desc(.cooksd)) %>% 
  head()

augment(lm.model5) %>% 
  arrange(desc(.cooksd)) %>% 
  head()

# lm.model5 is better fitted to the data
# predicting house prices on the test data using our lm.model5

test$predictions <- predict(lm.model5, newdata = test)

submission.file <- data.frame(
  Id = 1461:2919,
  SalePrice = test$predictions
)

write.csv(submission.file, "House_file_prediction_lm.csv", row.names = FALSE)
