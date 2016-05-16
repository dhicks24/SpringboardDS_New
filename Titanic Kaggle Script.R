# Set working directory appropriately
setwd("C:/Users/dhick/OneDrive/Documents/GitHub/SpringboardDS")

# Import the training set: "train" from URL https://www.kaggle.com/c/titanic/download/train.csv
# Store training set into appropriate working directory ("C:/Users/dhick/OneDrive/Documents/GitHub/SpringboardDS") 
# Load training set into data frame named "train"
train <- read.csv(file = "train.csv")

# Import the testing set: "test" from URL https://www.kaggle.com/c/titanic/download/test.csv
# Store testing set into appropriate working directory ("C:/Users/dhick/OneDrive/Documents/GitHub/SpringboardDS")
# Load testing set into data frame named "test"
test <- read.csv(file = "test.csv")

# Create a binary child column in each data set to analyze mortality rate of child versus adult
train$Child <- NA
train$Child[train$Age < 18] <- 1
train$Child[train$Age >= 18] <- 0

test$Child <- NA
test$Child[test$Age < 18] <- 1
test$Child[test$Age >= 18] <- 0

# Initialize a Survived column in the test data frame and set to 0
test$Survived <- 0

# Set Survived to 1 if Sex equals "female"
test$Survived[test$Sex == "female"] <- 1

# Load in the R package  
library(rpart)

# Load in the packages to build a fancy plot
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Build the decision tree
my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")

# Visualize the decision tree using plot() and text()
plot(my_tree_two)
text(my_tree_two)

# Make prediction #1 on the test set
my_prediction <- predict(my_tree_two, test, type = "class")

# Finish the data.frame() call
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Use nrow() on my_solution
nrow(my_solution)

# Finish the write.csv() call
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

# Update decision tree
my_tree_three <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class", control = rpart.control(minsplit = 50, cp = 0))

# Visualize my_tree_three
fancyRpartPlot(my_tree_three)

# Add family_size variable to train and test data sets
train_two <- train
train_two$family_size <- train_two$SibSp + train_two$Parch + 1
train <- train_two

test_two <- test
test_two$family_size <- test_two$SibSp + test_two$Parch + 1
test <- test_two

# Finish the command
my_tree_four <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size, data = train, method = "class")

# Visualize your new decision tree
fancyRpartPlot(my_tree_four)

# Add a Title column to new train and new test data frames and populate with title from Name
library(stringr)
library(gsubfn)
train_new <- train
test_new <- test

name_string_train <- as.character(train$Name)
name_string_train

train_new$Title <- gsub(pattern = "^.*, ", replacement = "", x = name_string_train)
train_new$Title

train_new$Title <- gsub(pattern = ". .*$", replacement = "", x = train_new$Title)
train_new$Title

train_new$Title <- as.factor(train_new$Title)

train_new

name_string_test <- as.character(test$Name)
name_string_test

test_new$Title <- gsub(pattern = "^.*, ", replacement = "", x = name_string_test)
test_new$Title

test_new$Title <- gsub(pattern = ". .*$", replacement = "", x = test_new$Title)
test_new$Title

test_new$Title <- as.factor(test_new$Title)

test_new

str(train_new)
str(test_new)

# Finish the command
my_tree_five <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data = train_new, method = "class")

# Visualize my_tree_five
fancyRpartPlot(my_tree_five)

# Make prediction #2
my_prediction <- predict(my_tree_five, test_new, type = "class")

# Make results ready for submission
my_solution <- data.frame(PassengerId = test_new$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

# Update the decision tree
my_tree_five <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data = train_new, method = "class")

# Visualize my_tree_five
fancyRpartPlot(my_tree_five)

# Make prediction #3
my_prediction <- predict(my_tree_five, test_new, type = "class")

# Make results ready for submission
my_solution <- data.frame(PassengerId = test_new$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

# Prepare data for analysis (ensure handled missing values, outliers, etc)
# Combine training_new and test_new sets into "all_data" (ensure variables are in same order)
str(train_new)
str(test_new)
test_new <- subset(test_new, select = c(1, 14, 2:13, 15))
test_new
all_data <- rbind(train_new, test_new)
all_data
str(all_data)

# Passenger on row 62 and 830 do not have a value for embarkment. 
# Since many passengers embarked at Southampton, we give them the value S.
all_data$Embarked[c(62, 830)] <- "S"

# Factorize embarkment codes.
all_data$Embarked <- factor(all_data$Embarked)

# Passenger on row 1044 has an NA Fare value. Let's replace it with the median fare value.
all_data$Fare[1044] <- median(all_data$Fare, na.rm = TRUE)

# How to fill in missing Age values?
# We make a prediction of a passengers Age using the other variables and a decision tree model. 
# This time you give method = "anova" since you are predicting a continuous variable.
library(rpart)
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size, data = all_data[!is.na(all_data$Age),], method = "anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])

# Split the data back into a train set and a test set
train_new <- all_data[1:891,]
test_new <- all_data[892:1309,]

# Install and load in the randomForest package
install.packages("randomForest")
library(randomForest)

# Set seed for reproducibility
set.seed(111)

# Apply the Random Forest Algorithm
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data = train_new, ntree = 1000, importance = TRUE)


# Make prediction #4 using the test set

my_prediction <- predict(my_forest, test_new, type = "class")

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerID = test_new$PassengerId, Survived = my_prediction)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

