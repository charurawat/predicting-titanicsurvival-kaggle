
# Set working directory
setwd("F:/UVA/fall_term/sys_6018/Competitions/1_Titanic/")
getwd()

# Load necessary packages
library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(mice)
library(randomForest)

# Load data
train <- read.csv('F:/UVA/fall_term/sys_6018/Competitions/1_Titanic/train.csv',stringsAsFactors = F)
test <- read.csv('F:/UVA/fall_term/sys_6018/Competitions/1_Titanic/test.csv',stringsAsFactors = F)

titanic_data <- bind_rows(train,test) # Bind both into 1 dataset for pre-processing
#str(titanic_data)

# FEATURE ENGINEERING (add/extract/modify features)

# Extract titles from name
titanic_data$Title <- gsub('(.*, )|(\\..*)','',titanic_data$Name)
# Reassign titles
titanic_data$Title[titanic_data$Title=="Mlle"] <- "Miss"
titanic_data$Title[titanic_data$Title=="Ms"] <- "Miss"
titanic_data$Title[titanic_data$Title=="Mme"] <- "Mrs"
lctitle <- c('Capt','Col','Don','Dona','Dr','Jonkheer','Lady','Major','Rev','Sir','the Countess')
titanic_data$Title[titanic_data$Title %in% lctitle] <- "UNIMP" #titles wth low counts

# Get family size for each
titanic_data$fsize <- titanic_data$SibSp + titanic_data$Parch + 1


# Handling missing values
titanic_data$PassengerId[is.na(titanic_data$Fare)] #output 2 passenger IDs
titanic_data[1044,]
#embarkment is %, P = 3
titanic_data$Fare[1044] <- median(titanic_data[titanic_data$Pclass == '3' & titanic_data$Embarked == 'S', ]$Fare, na.rm = TRUE)

# Imputation for Age ; won't delete cus multiple NAs
sum(is.na(titanic_data$Age))
# Converting variables into factors
fv <- c('PassengerId','Pclass','Sex','Embarked','Title')
titanic_data[fv] <- lapply(titanic_data[fv],function(x) as.factor(x))
set.seed(129) # Set random seed
# Peform mice imputation,excluding less than useful variables
mice_mod <- mice(titanic_data[,!names(titanic_data) %in% c('PassengerId','Name','Ticket','Cabin','Survived')],
method ='rf')
mice_out <- complete(mice_mod)
# Plot to crosscheck
# par(mfrow=c(1,2))
# hist(titanic_data$Age,freq=F,main="orig")
# hist(mice_out$Age,freq=F,main="mice")
 # Replace age vector
titanic_data$Age <- mice_out$Age

# Classify Person Type 
titanic_data$PType[titanic_data$Age < 16] = "Child"
titanic_data$PType[titanic_data$Age >= 16 & titanic_data$Age <= 30] = "Adult"
titanic_data$PType[titanic_data$Age > 30] = "Old"
titanic_data$PType <- as.factor(titanic_data$PType)

#PREDICTION

#split the data back
train <- titanic_data[1:889,]
test <- titanic_data[890:1309,]

# Build model using Random Forest
set.seed(500) #set a random seed
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + PType +
                           Embarked  + Fare + Title  + fsize , data = train)


print(rf_model) # Print the results
plot(rf_model) # Check Model Error ;  model is better at predicting death
importance(rf_model) # Variable Importance

# Predict on Test data
prediction <- predict(rf_model,newdata = test)
solution <- data.frame(PassengerID = test$PassengerId)
solution$Survived = prediction

# Write solution to csv file
write.csv(solution,file = 'rf_model_solution.csv',row.names=F)
