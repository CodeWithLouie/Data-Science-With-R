
library(readr)
library(dplyr)

car.price <- read_csv('Nigerian_Car_Prices.csv')

glimpse(car.price)

car.price %>% 
  group_by(Make, Fuel, Condition, `Engine Size`) %>% 
  summarise(Total = n()) %>% 
  arrange(desc(Total))

library(assertive)

car.price %>% 
  filter(is.na(Condition)) %>% 
  count()
