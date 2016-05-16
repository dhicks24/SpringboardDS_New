# 01 Set working directory appropriately
setwd("C:/Users/dhick/OneDrive/Documents/GitHub/SpringboardDS")


# 02 Import data sets

  # Import the training set: "train" from URL https://www.kaggle.com/c/titanic/download/train.csv
  # Store training set into appropriate working directory ("C:/Users/dhick/OneDrive/Documents/GitHub/SpringboardDS") 
  # Load training set into data frame named "train"
train <- read.csv(file = "train.csv")

  # Import the testing set: "test" from URL https://www.kaggle.com/c/titanic/download/test.csv
  # Store testing set into appropriate working directory ("C:/Users/dhick/OneDrive/Documents/GitHub/SpringboardDS")
  # Load testing set into data frame named "test"
test <- read.csv(file = "test.csv")


# 03 Install / load packages and libraries needed for analysis  

  # Load in the packages/libraries to build a fancy plot of decision trees
library(rpart)
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
library(rattle)
library(rpart.plot)
library(RColorBrewer)

  # Load in the packages/libraries to extract Title from Name 
library(stringr)
library(gsubfn)

  # Install and load in the randomForest package for use in prediction generation
install.packages("randomForest")
library(randomForest)


# 04 Create variables needed for analysis

  # Create a binary child column in each data set to analyze mortality rate of child versus adult
train$Child <- NA
train$Child[train$Age < 18] <- 1
train$Child[train$Age >= 18] <- 0

test$Child <- NA
test$Child[test$Age < 18] <- 1
test$Child[test$Age >= 18] <- 0

  # Create a Survived column in the test data frame and set to 0
test$Survived <- 0

  # Create a family_size variable (SibSP + Parch + 1) to train and test data sets
train$family_size <- train$SibSp + train$Parch + 1
test$family_size <- test$SibSp + test$Parch + 1

  # Extract title from Name and create Title in Train and Test sets
train_new <- train
name_string_train <- as.character(train$Name)
train_new$Title <- gsub(pattern = "^.*, ", replacement = "", x = name_string_train)
train_new$Title <- gsub(pattern = ". .*$", replacement = "", x = train_new$Title)
train_new$Title <- as.factor(train_new$Title)

test_new <- test
name_string_test <- as.character(test$Name)
test_new$Title <- gsub(pattern = "^.*, ", replacement = "", x = name_string_test)
test_new$Title <- gsub(pattern = ". .*$", replacement = "", x = test_new$Title)
test_new$Title <- as.factor(test_new$Title)

  # Create a binary Cabin column to analyze the effect of having a cabin on survival

train_new$cabinBinary <- NA
train_new$cabinBinary[train_new$Cabin == ""] <- 0
train_new$cabinBinary[train_new$Cabin != ""] <- 1
train_new$cabinBinary <- as.factor(train_new$cabinBinary)

test_new$cabinBinary <- NA
test_new$cabinBinary[test_new$Cabin == ""] <- 0
test_new$cabinBinary[test_new$Cabin != ""] <- 1
test_new$cabinBinary <- as.factor(test_new$cabinBinary)

# 05 Prepare data for analysis (address missing/NA values, etc)

  # Ensure variables in train_new and test_new are in same order
test_new <- subset(test_new, select = c(1, 13, 2:12, 14:16))

  # Combine train_new and test_new sets using rbind (to aid in addressing missing, NA values)
all_data <- rbind(train_new, test_new)

  # Passenger on row 62 and 830 do not have a value for embarkment. 
  # Since many passengers embarked at Southampton, we give them the value S.
all_data$Embarked[c(62, 830)] <- "S"

  # Factorize embarkment codes.
all_data$Embarked <- factor(all_data$Embarked)

  # Passenger on row 1044 has an NA Fare value. Let's replace it with the median fare value.
all_data$Fare[1044] <- median(all_data$Fare, na.rm = TRUE)

  # We make a prediction of a passengers Age using the other variables and a decision tree model. 
  # This time you give method = "anova" since you are predicting a continuous variable.
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size, data = all_data[!is.na(all_data$Age),], method = "anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])

  # Split the data back into a train set and a test set
train_new <- all_data[1:891,]
test_new <- all_data[892:1309,]


# 06 Conduct analysis (repeat as many times as needed to generate desired Kaggle result)

  # Generate decision tree
my_tree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size + Title, data = train_new, method = "class")

  # Visualize and analyze decision tree
fancyRpartPlot(my_tree)

  # Set seed for randomForest reproducibility
set.seed(111)

  # Apply the Random Forest Algorithm (setting Survived as a Factor)
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size + Title, data = train_new, ntree = 1000, importance = TRUE)

  # Make prediction using the test set
my_prediction <- predict(my_forest, test_new, type = "class")


# 07 Prepare prediction file for submission to Kaggle

  # Create a data frame with two columns: PassengerId & Survived. Survived contains predictions by PassengerId
my_solution <- data.frame(PassengerID = test_new$PassengerId, Survived = my_prediction)

  # Create a csv file with the name my_solution.csv
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
