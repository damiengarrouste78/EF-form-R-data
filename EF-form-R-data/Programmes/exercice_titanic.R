###############################################################################
#################### TITANIC DATA
###############################################################################


#https://www.kaggle.com/c/titanic/data
rm(list=ls())

setwd("~/R_Machine_Learning/complements/")

# Data Dictionary
# Variable	Definition	Key
# survival	Survival	0 = No, 1 = Yes
# pclass	Ticket class	1 = 1st, 2 = 2nd, 3 = 3rd
# sex	Sex	
# Age	Age in years	
# sibsp	# of siblings / spouses aboard the Titanic	
# parch	# of parents / children aboard the Titanic	
# ticket	Ticket number	
# fare	Passenger fare	
# cabin	Cabin number	
# embarked	Port of Embarkation	C = Cherbourg, Q = Queenstown, S = Southampton
# class: A proxy for socio-economic status (SES)
# 1st = Upper
# 2nd = Middle
# 3rd = Lower
# 
# age: Age is fractional if less than 1. If the age is estimated, is it in the form of xx.5
# 
# sibsp: The dataset defines family relations in this way...
# Sibling = brother, sister, stepbrother, stepsister
# Spouse = husband, wife (mistresses and fianc?s were ignored)
# 
# parch: The dataset defines family relations in this way...
# Parent = mother, father
# Child = daughter, son, stepdaughter, stepson
# Some children travelled only with a nanny, therefore parch=0 for them.


###############################################################################
#################### ARBRE CART AVEC RPART
###############################################################################
library(tidyverse)
library(rpart)
library(rpart.plot)

titanic<-read.csv(file="~/EF-form-R-data/EF-form-R-data/data/traintitanic.csv",sep=',')

glimpse(titanic)

# 38% de survivants
summary(as.factor(titanic$Survived))
mean(as.numeric(titanic$Survived))
# Est ce qu'on peut trouver des sous pop (niches) qui ont un taux de survie élevé ou faible
arbre1 <- rpart(formula = Survived ~ Age + Sex + Pclass +SibSp+Embarked+Parch+Fare, 
                data = titanic, method = "class")
arbre1
summary(arbre1)
arbre1$variable.importance

rpart.plot(arbre1, main = "Survie au naufrage ?")
rpart.plot(arbre1, main = "Survie au naufrage ?",extra=102)

arbre2 <- rpart(formula = Survived ~ Age + Sex + Pclass +SibSp+Embarked+Parch+Fare, 
                data = titanic, method = "class"
                ,control=rpart.control(cp=0.01,minsplit=25,minbucket=10))
rpart.plot(arbre2, main = "Survie au naufrage ?",extra=106)
rpart.plot(arbre2, main = "Survie au naufrage ?",extra=102)



arbre2$variable.importance
# An overall measure of variable importance is the sum of the goodness of split measures for each split for which it was the primary variable
# When rpart grows a tree it performs 10-fold cross validation on the data. Use printcp() to see the cross validation results.
printcp(arbre1)
# xerror correspond ? la cross valiadation error
# une pratique pour choisir ou arreter l'arbre est de choisir la ligne avec l'erreur de cross validation la plus faible
# ligne avec lowest xerror (la 3e ligne)
opt <- which.min(arbre2$cptable[,"xerror"])
# value de CP min
cp <- arbre2$cptable[opt, "CP"]
cp
pruned_model <- prune(arbre2,cp)
rpart.plot(pruned_model, main = "Survie au naufrage ?",extra=106)


###############################################################################
#################### ARBRE CART AVEC RPART et CARET
###############################################################################
library(caret)

names(getModelInfo())
modelLookup("rpart2")

grid <- expand.grid(maxdepth = c(2, 3, 4))
#grid <- expand.grid(maxdepth = c(1, 3, 6, 9),minsplit =c(10,20,50))
#Error: The tuning parameter grid should have columns maxdepth
summary(titanic)
# g?rer les missing value 
colSums(is.na(titanic2))

imputation = preProcess(titanic[c("Age","Sex","Pclass","SibSp","Embarked","Parch","Fare")], 
                        method = "knnImpute") # la fonction normalise les donn?es automatiquement avant l'imputation
titanic2 <- predict(imputation, titanic) 


tree <-train(as.factor(Survived) ~  Age + Sex + Pclass +SibSp+Embarked+Parch+Fare,
             data = titanic2, 
             method = "rpart2",
             tuneGrid=grid,metric='ACCURACY')

tree$bestTune

# pour controler les autres hyper param?tres, on peut passer par la fonction controle
# mais on ne peut pas optimiser
tree <- train(as.factor(Survived) ~ Sex + Pclass +SibSp+Parch+Fare, titanic, method = "rpart2", tuneGrid=grid,
              control = rpart.control(minsplit = 25, minbucket =10))

print(tree)
plot(tree)
varImp(tree)



###############################################################################
#################### RF
###############################################################################
library(randomForest)

toto<-min(table(titanic$Survived))
toto
system.time(
  rf_classifier100<-randomForest(formula = as.factor(Survived) ~ Age + Sex + Pclass +SibSp+Embarked+Parch+Fare,data=titanic2,
                                   ntree=100,mtry=3,replace=TRUE,
                                   cutoff=c(0.5,0.5),
                                   strata = as.factor(titanic$Survived),sampsize=c(100,100),
                                   nodesize=10,maxnode=12,importance=TRUE
                                   ,keep.forest=T)
) 
print(rf_classifier100)
plot(rf_classifier100)
varImp(rf_classifier100)
imp<-varImpPlot(rf_classifier100)
# this is the plot part, be sure to use reorder with the correct measure name
library(ggplot2) 
# this part just creates the data.frame for the plot part
library(dplyr)
imp <- as.data.frame(imp)
imp$varnames <- rownames(imp) # row names to column
rownames(imp) <- NULL  
imp$var_categ <- rep(1, 7) # random var category , si on veut colorier selon un groupe
ggplot(imp, aes(x=reorder(varnames, -MeanDecreaseGini), weight=MeanDecreaseGini, fill=as.factor(var_categ))) + 
  geom_bar() +
  scale_fill_discrete(name="Variable Group") +
  ylab("MeanDecreaseGini") +
  xlab("Variable Name")


str(rf_classifier100)
# moyennes des votes de la RF pour chaque indiv de lech train 
summary(rf_classifier100$votes[,2])



###############################################################################
#################### RF AVEC RPART et CARET
###############################################################################

modelLookup("rf")
modelLookup("ranger")

# D?finition des param?tres ? balayer
tgrid <- expand.grid(
  mtry = c(2,3,4,5) # Nombre de variables possibles splitt? ? chaque noeud
)
tr <- trainControl(method="cv", number = 4)

model_caret <- train(as.factor(Survived) ~ Age + Sex + Pclass +SibSp+Embarked+Parch+Fare, data = titanic2,
                     method = 'rf', # le m?thode ranger du package ranger permet de faire des rf 
                     trControl = tr,
                     tuneGrid = tgrid,
                     num.trees = 100,minbucket=16,metric='Accuracy')
print(model_caret)
model_caret$bestTune # renvoi parmi tous les tests le meilleur mod?le
plot(model_caret)
varImp(model_caret)
# mtry = 3
