
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
  filter(!is.na(Condition)) %>% ## check for values that are not NA in the cond
  #condition column
  count()

car.price.not_NA <- car.price %>% 
  filter(!is.na(Condition)) 

# inspect the new data and check for additional NAs

View(car.price.not_NA)

# Replace NAs in Build with Not Provided
# call the stingr package 

library(stringr)

car.price.filtered <- car.price.not_NA %>% 
  mutate(New_Build = str_replace_na(Build, replacement = "Not Available"))


car.price.filtered %>% 
  filter(is.na(New_Build)) %>% 
  count()

# NAs replaced with not available 

View(car.price.filtered)

# let's see the groupings of car build, transmission, condition, make, & YOM

  car.price.filtered %>% 
    group_by(New_Build) %>% 
    count()

    car.price.filtered %>% 
    group_by(Make) %>% 
    count() %>% 
    arrange(desc(n))
    
  car.price.filtered %>% 
    group_by(`Year of manufacture`) %>% 
    count() %>% 
    arrange(desc(n))
  
  car.price.filtered %>% 
    group_by(Condition) %>% 
    count()
  
    