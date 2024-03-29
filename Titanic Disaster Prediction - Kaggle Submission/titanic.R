# Predicting Survivability Using RandomForest
# First thing first, let's load the readr and dplyr package

library(readr)
library(dplyr)
library(ggplot2)

# The Train data and Test Data

# We begin by importing the datasets needed for this analysis

# import the train data

train <- read_csv("train.csv")

# import the test data

test <- read_csv("test.csv")

# We are going to be training the `train` data and
# then run the prediction on the `test` data.

head(train)

# examine the structure

glimpse(train)

# Add a new Survived column to the `test` data.
# add a column using mutate and save as a new variable

test.survived <- test %>% 
  mutate(
    Survived = rep("none", nrow(test))
)

head(test.survived)

 
# Join the `train` and `test.survived` datasets

train.test <- rbind(train, test.survived)

head(train.test)
 
# Converting columns to the appropriate class.
# Convert Sex, Pclass, Sex, Survived, SibSp, Parch, Embrked to factors
# check the structure 

# create a function to convert variable class

convert_func <- function(x) {
  x <- as.factor(x)
}

glimpse(train.test)

# use the function to convert variables

train.test <- train.test %>% 
  mutate(
    Pclass = convert_func(Pclass),
    Sex = convert_func(Sex),
    Survived = convert_func(Survived),
    Parch = convert_func(Parch),
    SibSp = convert_func(SibSp),
    Embarked = convert_func(Embarked)
  )

glimpse(train.test)
 

# Exploratory Data Analysis

# Let's see the total number of people that survived
# or perish from the `train.test` data.

train.test %>% 
  count(Survived)
 

# They are 549 victims and 342 survivor from the `train` data. the 
# "none" paramenter is from the newly created survived column in the `test` data.

# Let's explore the data some more

train.test %>% 
  count(Pclass)

summary(train.test$Sex)

summary(train.test$SibSp)

summary(train.test$Parch)

summary(train.test$Age)
 

# Visualising the predictive behavior of each variable

ggplot(train.test[1:890, ], aes(Pclass)) + 
  geom_bar() +
  ggtitle("Pclass Distribution")
 
# There are more passengers in third class than any other class

ggplot(train.test[1:890, ], aes(Pclass, fill = factor(Survived))) +
  geom_bar() +
  xlab("Passenger Class") + 
  labs(fill = "Survived") +
  ggtitle("Pclass and Survived")
 
# Third class has the highest number of casualties.

ggplot(train.test[1:890, ], aes(Age))+
  geom_histogram() +
  ggtitle("Age Distribution")
 
# The Age distribution is more clusterd around 20-40 years.
# According to the data, children are more than likely to survive than adult.
# We will visualise this hypothesis next but only for exploration.

ggplot(train.test[1:891, ], aes(Age, fill = Survived)) + 
  geom_histogram() +
  ggtitle("Age and survived")
 

# Aha! Age is a good predictor. In addition to age, we want to look 
# at the distribution of sex and whether it is also a good predictor.

ggplot(train.test[1:891, ], aes(Sex)) +
  geom_bar() +
  ggtitle("Sex Distribution")
 
# More males onboard than female.

ggplot(train.test[1:891, ], aes(Sex, fill = Survived)) +
  geom_bar() +
  ggtitle("Sex and Survived")
 
# Oops! A large chunk of males onboard perished.

##### Multi-variate exploration

ggplot(train.test[1:891, ], aes(Age, fill = Survived)) +
  geom_histogram() + 
  facet_wrap(~ Pclass) +
  ggtitle("Age by Pclass and Survived")

# Interesting plot! survival decreases as age increases.

# The SibSp and Parch Variable

# SipSp is the number of siblings or spouses traveling together. 
# Parch shows whether a passenger is travelling with a parent or partner.

# **Question: Does a passenger travelling alone has a
# higher chance of survival?**

# To answer this, we will be looking at the distribution of each variable.

ggplot(train.test[1:891, ], aes(SibSp)) +
  geom_bar() +
  ggtitle("SibSp Distribution")
 
# Let's add the survival rate

ggplot(train.test[1:891, ], aes(SibSp, fill = Survived)) +
  geom_bar() +
  ggtitle("SibSp and Survived")

# Survival rate decreases as number of people travelling together increases.

ggplot(train.test[1:891, ], aes(Parch)) +
  geom_bar() +
  ggtitle("Parch Distribution")
 
# Hmm! looks like the same pattern. next...

ggplot(train.test[1:891, ], aes(Parch, fill = Survived)) +
  geom_bar() +
  ggtitle("Parch and Survived")

 
## Feature Engineering

### Adult Child Age Group

# Creating a Child and Adult Variable from the `train.test` data.

# create a childadult_function 

chAdult_func <- function(x) {
  case_when(
    x <= 12 ~ "Child",
    x >= 13 & x <= 19 ~ "Teenager", TRUE ~ "Adult"
  )
}

Adult_Child <- c() #create a null file

for (i in 1:length(train.test$Age)) {
  Adult_Child <- c(Adult_Child, chAdult_func(train.test[i, "Age"]))
}

# create a new column and add the Adult_Child vector

train.test <- train.test %>% 
  mutate(Age_group = Adult_Child)

# call summary on the new variable

table(train.test$Age_group)
 

# Visualise the distribution across Pclass

ggplot(train.test[1:891, ], aes(Age_group, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass) +
  ggtitle("Age Group Survival Across Pclass")
 

# Wow! this shows us more prediction than the age.

# Let's see the above chart with the Parch Var before we 
# move on to create another variables.

ggplot(train.test[1:891, ], aes(Age_group, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass ~ SibSp) +
  ggtitle("Age Group Survival by SibSp Across Pclass")

# Ouch! the pattern seems to be hard to read. Since both SIbSp 
# and Parch shows the number of dependent/spouse/siblings/parents/
# guardians traveling together, we will use this
# information to create a new **Family_Size** variable.

# create a vector of the sibsp and parch variable and add them together

train.temp <- c(train$SibSp, test$SibSp)
test.temp <- c(test$Parch, train$Parch)        

train.test <- train.test %>% 
  mutate(Family_size = as.factor(train.temp + test.temp + 1))

# Relationship between Age group, Family Size and Survival rate

ggplot(train.test[1:891, ], aes(Age_group, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Family_size) +
  ggtitle("Age vs Survived by Family Size")
 

## Training the RandomForest Algorithm

# Now that we have looked at the varibles with the most predictive nature,
# we will be training our `RandomForest` algorithm with them.

# load the randomforest package

library(randomForest)
 
# Let's create our label data for the `randomforest` model
# and also our train data.

rf.label <- as.factor(train$Survived)

rf.train1 <- train.test[1:891, c("Sex", "Pclass", "Age_group", "Family_size")]

rf.train2 <- train.test[1:891, c("Sex", "Pclass", "Age_group")]

# set the seed to 2022

set.seed(2022)

# run the model

model <- randomForest(rf.train1, rf.label, importance = TRUE, ntree = 2022)

model2 <- randomForest(rf.train2, rf.label, importance = TRUE, ntree = 2022)

# view the model accuracy

model

model2

# view the model plot

varImpPlot(model) # this has more accuracy

varImpPlot(model2)

### Running Our Model on the Test Data

set.seed(2022)

# get nrow of test data

rf.test <- train.test[892:1309, c("Sex", "Pclass", "Age_group", "Family_size")]

# run prediction on test data

prediction <- predict(model, rf.test)

# table(prediction)

# create a dataframe of PassengerId and Survived
# file to be submitted on Kaggle

submission.file <- data.frame(PassengerId = rep(892:1309), Survived = prediction) 

write.csv(submission.file, "Titanic-Randomforest-prediction-2022.csv", row.names
          = FALSE)
