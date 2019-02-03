
#--------------------------------------------------------------------------------------------------------------------------------------

current_dir = dirname(rstudioapi::getSourceEditorContext()$path) #path of current script
setwd(current_dir) #set working directory as path where script/input files are stored.

# Load necessary packages
library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(mice)
library(randomForest)

# Load data
train <- read.csv('train.csv',stringsAsFactors = F)
test <- read.csv('test.csv',stringsAsFactors = F)

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
titanic_data$PassengerId[is.na(titanic_data$Fare)] #output passenger ID wth missing fare
titanic_data[1044,]
#embarkment is %, P = 3
titanic_data$Fare[1044] <- median(titanic_data[titanic_data$Pclass == '3' & titanic_data$Embarked == 'S', ]$Fare, na.rm = TRUE)

# Imputation for Age ; won't delete cus multiple NAs
sum(is.na(titanic_data$Age)) #total rows with NAs
# Converting variables into factors
fv <- c('PassengerId','Pclass','Sex','Embarked','Title')
titanic_data[fv] <- lapply(titanic_data[fv],function(x) as.factor(x))

# Peform mice imputation,excluding less than useful variables
# and using random forest method
mice_mod <- mice(titanic_data[,!names(titanic_data) %in% c('PassengerId','Name','Ticket','Cabin','Survived')],
method ='rf')
mice_out <- complete(mice_mod)
# Plot to crosscheck
# par(mfrow=c(1,2))
# hist(titanic_data$Age,freq=F,main="orig")
# hist(mice_out$Age,freq=F,main="mice")
 # Replace age vector
titanic_data$Age <- mice_out$Age


#PREDICTION

#split the data back
train <- titanic_data[1:891,]
test <- titanic_data[892:1309,]

# Build model using Random Forest
# removed Embarked,SibSp,Parch,Ticket,Cabin
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age  +
                            Fare + Title  + fsize , data = train)
print(rf_model) # Print the results
plot(rf_model) # Check Model Error ;  model is better at predicting death
importance(rf_model) # Variable Importance

# Predict on Test data
prediction <- predict(rf_model,newdata = test)
solution <- data.frame(PassengerID = test$PassengerId)
solution$Survived = prediction

# Write solution to csv file
write.csv(solution,file = 'titanic_solution.csv',row.names=F)
