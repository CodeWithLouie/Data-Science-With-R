
# the titanic dataset
# predicting the chances a passenger would survive/perish
# load dataset

library(dplyr)
library(readr)

test <- read_csv("test.csv")
train <- read_csv("train.csv")

# add a survived variables to the test data 

test_combined <- test %>% 
  mutate(Survived = rep("none", nrow(test)))

# combine the test_combined and the train dataset 

train_test_combined <- rbind(train, test_combined)

# changing class of variables
# check the structure 

glimpse(train_test_combined)

train_test_combined$Pclass <- as.factor(train_test_combined$Pclass)
train_test_combined$Sex <- as.factor(train_test_combined$Sex)

train_test_combined$Survived <- as.factor(train_test_combined$Survived)

glimpse(train_test_combined)

# take a look at count of survived

train_test_combined %>% 
  count(Survived)

# distribution of passenger class

train_test_combined %>% 
  count(Pclass)

# visualise Pclass to make sense of the distribution
# load ggplot2

library(ggplot2)
library(ggthemes)

ggplot(train, aes(Pclass, fill = factor(Survived))) +
  geom_bar() +
  xlab("Passenger Class") + 
  labs(fill = "Survived") +
  ggtitle("Survival Count Across Passenger Class") +
  theme_clean()
  

# Examine the first few names of the train dataset

glimpse(train$Name)
head(train$Name)

# how many unique name do we have across the dataset 

length(unique(train_test_combined$Name))

# 1307 of 1309 obs are unique?
# check for duplicate names 

dup_name <- train_test_combined %>% 
  filter(duplicated(Name))

# A look at the titles attached to names
# is title correlated with the SibSp variable? 
# load the stringr package: time to detect some strings

library(stringr)

misses <- train_test_combined %>% 
  filter(str_detect(Name, "Miss"))

# interesting! they are 206 obs with the title of Miss.  
# Let's take a look at the first few rows

head(misses, 5)

# Miss is a title used for unmarried females
# hypothesis: does the titles correlate with age? 
# detect for Mrs. in the combined data

mrs <- train_test_combined %>% 
  filter(str_detect(Name, "Mrs"))

# 201 obs for Mrs.
# Let's take a look

head(mrs, 5)

# check out males to see if patterns continue 

males <- train_test_combined %>% 
  filter(Sex == "male") 

summary(males) 

head(males, 5)

# create a title extraction function to expand the relationship between
# Pclass and Survived 

Title_func <- function(name) {
  case_when(
    str_detect(name, "Miss.") ~ "Miss.",
    str_detect(name, "Mrs.") ~ "Mrs.",
    str_detect(name, "Mr.") ~ "Mr.",
    str_detect(name, "Master.") ~ "Master",
    TRUE ~ "other"
  )
}

# create a null vector. 

titles <- NULL

for (i in 1:nrow(train_test_combined)) {
  titles <- c(titles, Title_func(train_test_combined[i, "Name"]))
}

train_test_combined <- train_test_combined %>% 
  mutate(Title = titles) %>% 
  select(Title, Name, Sex, Age, everything())

# visualise the title variable by Pclass

ggplot(train_test_combined[1:890,], aes(Title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass) +
  labs(title = "Pclass", x = "Title", y = "Total Count", subtitle = "
       cumulative survival rate by title and passenger class", fill =
         "Survived") +
  theme_clean()
   

# what is the distribution of females to males in the combined data

summary(train_test_combined$Sex)

# visualise the relationship between sex and survival rate by Pclass

ggplot(train, aes(factor(Sex), fill = factor(Survived))) +
  geom_bar() +
  facet_wrap(~ Pclass) +
  xlab("Sex") +
  ylab("Survival count") +
  ggtitle("Pclass") +
  labs(fill = "Survived") +
  theme_classic()
  

# bring it all together
# age is an important factor here, so let's get a summary of age

summary(train_test_combined$Age)

# 263 missing values for age?

summary(train$Age)
summary(test$Age)

# most of the missing values are in the train data
# visualise age and survival rate by and Sex Pclass

ggplot(train, aes(Age, fill = factor(Survived))) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~ Sex ~ Pclass) +
  xlab("Age") + 
  ylab("Total Count") +
  theme_classic()
  

# confirm whether Master is a term for Male children

boys <- train_test_combined %>% 
  filter(Title == "Master")

summary(boys$Age)  

# male children are titled Master. the summary of boys age confirms it.
# 8 NA's
# Let's do the same for Misses.

girls <- train_test_combined %>% 
  filter(titles == "Miss.")

summary(girls$Age)

# title Miss. doesn't correlate with age,
# but it sure means an unmarried lady
# let's visualise the Misses data to understand the age pattern

misses %>% 
  filter(Survived != "none") %>% 
ggplot(aes(Age, fill = Survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~ Pclass) +
  xlab("Age") +
  ylab("Total count") +
  ggtitle("Pclass") 
  

# let's filter misses to know whether they are traveling alone
# also check age distribution

misses_alone <- misses %>% 
  filter(SibSp == 0 & Parch == 0)

summary(misses_alone$Age)

# since the max age for boy child is around 14, let's filter for 
# misses_alone with age lower than 14 to get the children misses

misses_alone %>% 
  filter(Age < 14.5) %>% 
  summarise(Children = n())

# Sibsp and Parch shows the number of siblings or spouse and parents
# It makes sense to know the survival rate by this variables
# glimpse the two variables 

glimpse(train_test_combined$SibSp)
glimpse(train_test_combined$Parch)

# both are integers whereas it's a category.
# convert to factor

train_test_combined$SibSp <- as.factor(train_test_combined$SibSp)
train_test_combined$Parch <- as.factor(train_test_combined$Parch)

glimpse(train_test_combined$SibSp)
glimpse(train_test_combined$Parch)

# visualise SibSp and survival rate by Pclass and title

train_test_combined %>% 
  filter(Survived != "none") %>% 
ggplot(aes(SibSp)) +
  geom_bar(aes(fill = Survived)) +
  facet_wrap(~ Pclass ~ Title) +
  ylim(0,300) +
  xlab("SibSp") +
  ylab("Total count") +
  ggtitle("Survival rate by SibSp, Pclass and Titles") 
  

# visualise the Parch variable 

train_test_combined %>% 
  filter(Survived != "none") %>% 
  ggplot(aes(Parch)) +
  geom_bar(aes(fill = Survived)) +
  facet_wrap(~ Pclass ~ Title) +
  ylim(0,300) +
  xlab("Parch") +
  ylab("Total count") +
  ggtitle("Survival rate by SibSp, Pclass and Titles")
  
# what about family size?
# is survival dependent on family size?
# create a new family size variables from train and test data

temp_parch <- c(train$Parch, test$Parch)
temp_sibsp <- c(train$SibSp, test$SibSp)

train_test_combined <- train_test_combined %>% 
  mutate(
    Family_size = as.factor(temp_parch + temp_sibsp + 1)
  )

# visualise survival by family size 

train_test_combined %>% 
  filter(Survived != "none") %>% 
  ggplot(aes(Family_size)) +
  geom_bar(width = 1, aes(fill = Survived)) +
  facet_wrap(~ Pclass ~ Title) +
  xlab("Family Size") +
  ylab("Total count") +
  ggtitle("Survival by Family Size by Pclass and Title")

str(train_test_combined$Ticket)

head(train_test_combined$Ticket, 5)

# ticket_first_char <- if_else(train_test_combined$Ticket == "", " ",
#                              str_sub(train_test_combined$Ticket, 1, 1))

ticket_first_char2 <- case_when(
    train_test_combined$Ticket == "" ~ " ",
    TRUE ~ str_sub(train_test_combined$Ticket, 1,1)
  )

unique(ticket_first_char2)

train_test_combined <- train_test_combined %>% 
  mutate(tkt_first_char = as.factor(ticket_first_char2))



