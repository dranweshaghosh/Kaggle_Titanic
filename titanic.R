library(data.table)
readData <- function(file.name, column.types, missing.types)
{
  read.csv(file.name,
        colClasses = column.types,
        na.strings=missing.types,
        stringsAsFactors = FALSE)
}
train.data.file <- "train.csv"
test.data.file <- "test.csv"
missing.types <- c("NA","")
train.column.types <- c('integer',   # PassengerId
                        'factor',    # Survived 
                        'factor',    # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
                        )
test.column.types <- train.column.types[-2]  
## No Survived Column in test.csv

train <- readData(train.data.file, train.column.types, missing.types)
test <- readData(test.data.file, test.column.types, missing.types)


## Exploring Data
summary(train)

## Missing Data
require(Amelia)
missmap(train, main = "Titanic Training Data - Missing Data", col = c("red","black"))
missmap(test, main = "Titanic Test Data - Missing Data", col = c("red","black"))

## Data Visualization
library(ggplot2)
library(ggthemes)

# PassengerId
pi1 <- ggplot(train, aes(x = PassengerId, y = Survived))
pi1 +geom_point()
# No obvious relationship found

# Pclass 
mosaicplot(train$Pclass ~ train$Survived, main = "Passenger Fate by Traveling Class", shade = FALSE, color = TRUE, xlab = "Pclass", ylab = "Survived")

# Sex - Yes
mosaicplot(train$Sex ~ train$Survived, main = "Passenger Fate by Gender", shade = FALSE, color = TRUE, xlab = "Pclass", ylab = "Survived")

# Age - Yes
boxplot(train$Age ~ train$Survived, main= "Passenger Fate by Age", xlab = "Survived", ylab = "Age")

# SibSp - Number of Siblings/Spouses Aboard - Mixed
mosaicplot(train$SibSp ~ train$Survived, main = "Passenger Fate by Siblings", shade = FALSE, color = TRUE, xlab = "SibSp", ylab = "Survived")

# Parch - Number of Parents/Children Aboard - Yes for alone vs with parents
mosaicplot(train$Parch ~ train$Survived, main = "Passenger Fate by Parents/Children", shade = FALSE, color = TRUE, xlab = "Parch", ylab = "Survived")

# Fare - yes
boxplot(train$Fare ~ train$Survived, main= "Passenger Fate by Fare", xlab = "Survived", ylab = "Fare")

# Are Fare and Pclass related? - Yes, so Passenger Class can be used as substitute for fare
boxplot(train$Fare ~ train$Pclass, main= "Fare vs Passenger Class", xlab = "Pclass", ylab = "Fare")

# Embarked - Port of Embarkation
mosaicplot(train$Embarked ~ train$Survived, main = "Passenger Fate by Port of Embarkation", shade = FALSE, color = TRUE, xlab = "Embarked", ylab = "Survived")
barplot(table(train$Embarked), names.arg = c("Cherbourg", "Queenstown", "Southampton"), main = "Embarked (Port of Emparkation)")

# Correlogram
require(corrgram)
require(plyr)
corrgram.data <- train
## change features of factor type to numeric type for inclusion on correlogram
corrgram.data$Survived <- as.numeric(corrgram.data$Survived)
corrgram.data$Pclass <- as.numeric(corrgram.data$Pclass)
corrgram.data$Embarked <- revalue(corrgram.data$Embarked, 
                                  c("C" = 1, "Q" = 2, "S" = 3))
## generate correlogram
corrgram.vars <- c("Survived", "Pclass", "Sex", "Age", 
                   "SibSp", "Parch", "Fare", "Embarked")
corrgram(corrgram.data[0:891,corrgram.vars], order=FALSE, 
         lower.panel=panel.ellipse, upper.panel=panel.pie, 
         text.panel=panel.txt, main="Titanic Training Data")

## Replacing Fate ILO Survived and revaluing Fate factor
train$Fate <- train$Survived
train$Fate <- revalue(train$Fate, c("1" = "Survived", "0" = "Perished"))



# Individual's Name
## Obtaining titles
train$Title <- gsub('(.*, )|(\\..*)', '',train$Name)
table(train$Sex, train$Title)
test$Title <- gsub('(.*, )|(\\..*)', '',test$Name)
table(test$Sex, test$Title)
## Combine all rare titles
rare_title <- c('Capt', 'Col','Don','Dona','Dr','Jonkheer', 'Lady', 'Major','Rev', 'Sir','the Countess')
## Reassignment of Mlle, Ms and Mme
train$Title[train$Title == 'Mlle'] <- 'Miss'
train$Title[train$Title == 'Ms'] <- 'Miss'
train$Title[train$Title == 'Mme'] <- 'Mrs'
train$Title[train$Title %in% rare_title] <- 'Rare'
test$Title[test$Title == 'Mlle'] <- 'Miss'
test$Title[test$Title == 'Ms'] <- 'Miss'
test$Title[test$Title == 'Mme'] <- 'Mrs'
test$Title[test$Title %in% rare_title] <- 'Rare'
table(train$Sex, train$Title )
table(test$Sex, test$Title )
## Obtaining Surnames
train$Surname <- sapply(train$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])
test$Surname <- sapply(test$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])

# Survival dependence on Family
train$Fsize = train$SibSp + train$Parch + 1
test$Fsize = test$SibSp + test$Parch + 1
train$Family <- paste(train$Surname, train$Fsize, sep='_')
test$Family <- paste(test$Surname, test$Fsize, sep='_')
ggplot(train, aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()
# Discretize family size
train$FsizeD[train$Fsize == 1] <- 'singleton'
train$FsizeD[train$Fsize < 5 & train$Fsize > 1] <- 'small'
train$FsizeD[train$Fsize > 4] <- 'large'
test$FsizeD[test$Fsize == 1] <- 'singleton'
test$FsizeD[test$Fsize < 5 & test$Fsize > 1] <- 'small'
test$FsizeD[test$Fsize > 4] <- 'large'
# Show family size by survival using a mosaic plot
mosaicplot(table(train$FsizeD, train$Survived), main='Family Size by Survival', shade=TRUE)

# Survival dependence on Age: Discretized
## Predictive Imputation of Age using MICE (Multiple Imputation Using Chained Equations)
sum(is.na(train$Age))
## Making variables factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')
train[factor_vars] <- lapply(train[factor_vars], function(x) as.factor(x))
test[factor_vars] <- lapply(test[factor_vars], function(x) as.factor(x))
## Setting a random seed
set.seed(129)
## Performing MICE imputation, excluding certain less than useful variables
library(mice)
mice_mod <- mice(train[, !names(train) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived', 'Embarked')], method='rf')
mice_output <- complete(mice_mod)
## Plotting age distributions
par(mfrow=c(1,2))
hist(train$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))
## Replacing Age variable with MICE model
train$Age <- mice_output$Age
sum(is.na(train$Age))
### Doing the same to test
mice_mod2 <- mice(test[, !names(test) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived', 'Embarked')], method='rf')
mice_output2 <- complete(mice_mod2)
## Plotting age distributions
par(mfrow=c(1,2))
hist(test$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output2$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))
## Replacing Age variable with MICE model
test$Age <- mice_output2$Age
sum(is.na(test$Age))

## Relationship with Age
plot(train$Age, train$Survived, xlab = "Age", ylab = "Survived")
survivers <- data.frame(train$Age[train$Survived == 1])
nonsurvivers <- data.frame(train$Age[train$Survived == 0])
survivers$title <- 'Survivers'
nonsurvivers$title <- 'Non-Survivers'
colnames(survivers)[1] <- "Age"
colnames(nonsurvivers)[1] <- "Age"
hist(survivers$Age, breaks = 32 ,xlim=c(0,80), ylim=c(0,40), col="red")
hist(nonsurvivers$Age,breaks = 32,  add=T, col=rgb(0,1,0,0.5))

##Discretize age
train$Agegroup[train$Age<14] <- 'child'
train$Agegroup[train$Age>=14] <- 'adult'
test$Agegroup[test$Age<14] <- 'child'
test$Agegroup[test$Age>=14] <- 'adult'
table(train$Agegroup, train$Survived)
mosaicplot(table(train$Agegroup, train$Survived), main = "Age Group by Survival", shade = TRUE)
##Slight benefit of being a child

# Combined Effect of Age and Sex
ggplot(train, aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # Including Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()

#Mothers may have survived: So maternity?
library(dplyr)
full1 <- bind_rows(select(train, Sex, Title, Age), select(test, Sex, Title, Age))
female_age <- full1 %>% filter(Sex == 'female')
plot(female_age$Title, female_age$Age)
b <- female_age[female_age$Title == 'Mrs', ]
min(b$Age)

train$Mother <- 'Not Mother'
train$Mother[train$Sex == 'female' & train$Parch > 0 & train$Age > min(b$Age) & train$Title != 'Miss'] <- 'Mother'
test$Mother <- 'Not Mother'
test$Mother[test$Sex == 'female' & test$Parch > 0 & test$Age > min(b$Age) & test$Title != 'Miss'] <- 'Mother'
table(train$Mother, train$Survived)
## Factorizing our two new factor variables
train$Agegroup  <- factor(train$Agegroup)
train$Mother <- factor(train$Mother)
test$Agegroup  <- factor(test$Agegroup)
test$Mother <- factor(test$Mother)

# Embarkment completion
table(is.na(train$Embarked))
table(is.na(test$Embarked))
## Can the data be extrapolated from Passenger Class and Fare?
## Removing the entries without Embarked Info and adding info from test data
library(dplyr)
full <- bind_rows(select(train, Embarked, Pclass, Fare), select(test, Embarked, Pclass, Fare))
embark_fare <- full %>% filter(Embarked == "NA")
library(scales)
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()
## Median = $80
train[is.na(train$Embarked),]
## Entries are 62 and 830
train$Embarked[c(62, 830)] <- 'C'
table(is.na(train$Embarked))

# Fixing Fare
table(is.na(train$Fare))
table(is.na(test$Fare))
## One entry in test does not have fare
test[is.na(test$Fare),]
## It is entry no. 1044/ test no. 153 and his Pclass is 3; Embarked is S
ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()
a <- full[full$Pclass == '3' & full$Embarked == 'S', ]
a <- a[is.na(a$Fare)==FALSE,]
median(a$Fare)
## Median is $8.05
test$Fare[153] <- median(a$Fare)

# Building Model
##set.seed(754)
##rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Agegroup + Mother, data = train)
## Show model error
##plot(rf_model, ylim = c(0,0.36))
##legend('topright', colnames(rf_model$err.rate),col=1:3, fill=1:3)

# Get importance
##importance    <- importance(rf_model)
##varImportance <- data.frame(Variables = row.names(importance), 
##                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
## rankImportance <- varImportance %>%
## mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
##ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
##                           y = Importance, fill = Importance)) +
##  geom_bar(stat='identity') + 
##  geom_text(aes(x = Variables, y = 0.5, label = Rank),
##            hjust=0, vjust=0.55, size = 4, colour = 'red') +
##  labs(x = 'Variables') +
##  coord_flip() + 
##  theme_few()
# Predict using the test set
## prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
## solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file
## write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)

# Comparing algorithms
library(caret)
library(corrplot)
library(doParallel)
library(gbm)
library(pROC)
library(xgboost)
## Testing harness with 10-fold cross validation
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
## a) linear algorithms
set.seed(7)
fit.lda <- train(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Agegroup + Mother, data=train, method="lda", metric=metric, trControl=control)
## b) nonlinear algorithms
## CART
set.seed(7)
fit.cart <- train(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Agegroup + Mother, data=train, method="rpart", metric=metric, trControl=control)
## kNN
set.seed(7)
fit.knn <- train(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Agegroup + Mother, data=train, method="knn", metric=metric, trControl=control)
## c) advanced algorithms
## SVM
set.seed(7)
fit.svm <- train(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Agegroup + Mother, data=train, method="svmRadial", metric=metric, trControl=control)
## Random Forest
set.seed(7)
fit.rf <- train(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Agegroup + Mother, data=train, method="rf", metric=metric, trControl=control)
## Generalized Boosted Regression Model (GBM)
# Setting up training control
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     allowParallel = TRUE)
grid <- expand.grid(interaction.depth = c(1,2),
                    n.trees = c(10,20),
                    shrinkage = c(0.01,0.1),
                    n.minobsinnode= 20)
set.seed(7)
# Setting up for parallel processing
registerDoParallel(4)
getDoParWorkers()
fit.bgm <- train(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Agegroup + Mother, data=train, method="gbm", metric=metric, trControl=control)

## XGBoost 
set.seed(7)
fit.xgb <- train(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Agegroup + Mother, data=train, method="xgbTree", metric=metric, trControl=control)



## Comparison of algorithms
# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf, gbm = fit.bgm, xgb=fit.xgb))
summary(results)
dotplot(results)
## XGBoost is best model
print(fit.xgb)

# Get importance
varImportance <- data.frame(varImp(fit.xgb)$importance)
varImportance$Vars <- row.names(varImportance)
varImportance[order(-varImportance$Overall),]

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(varImportance$Overall))))

# Predict using the test set
prediction <- predict(fit.xgb, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'xgb_mod_Solution.csv', row.names = F)
