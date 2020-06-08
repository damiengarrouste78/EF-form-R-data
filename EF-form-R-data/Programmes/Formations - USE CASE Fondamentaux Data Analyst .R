#----------------------------------------------------------------------------------#
#                     LES FONDAMENTAUX DATA ANALYST                                #
#                                 USE CASE                                         #
#----------------------------------------------------------------------------------#



###############################################################################
#################### Déclaration des librairies
###############################################################################

library(dplyr)
library(tidyverse)
library(devtools)
library(lubridate)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(rpart)
library(MASS)

###############################################################################
#################### Datamanagment
###############################################################################

#   chargement des données
#------------------------------------------------------------------------------#

# Import des deux fichiers de données
churn_p1 <- read.csv('C:/Users/cpe/Desktop/Data SFR Formation/churn_1.csv')
churn_p2 <- read.csv('C:/Users/cpe/Desktop/Data SFR Formation/churn_2.csv')


# Jointure des deux datasets sur la clé phone
# Vérification que la clé de jointure n'a pas de doublon
length(churn_p1$Phone) - length(unique(churn_p1$Phone))
length(churn_p2$Phone) - length(unique(churn_p2$Phone))
#=> aucun doublons

#Nombre de lignes par table
dim(churn_p1)
dim(churn_p2)

# Sur les deux tables le nombre de lignes est different, on procéde donc a une jointure gauche sur la table churn_1 (qui contient la liste des clients et leurs informations) en utilisant la clé unique Phone
# Les lignes qui ne sont pas retrouvées grâce à la clé ne seront pas prises en compte dans la jointure
churn=left_join(churn_p1, churn_p2, by = "Phone")

#Dimension du dataframe churn
dim(churn)


#   Audit des données
#------------------------------------------------------------------------------#
# Nom et format des variables
str(churn)

# recodage de la variable Area.Code
churn$Area.Code=as.factor(churn$Area.Code)



# Presence de valeurs manquantes ?
# Le summary donne pour chaque variable son nombre de valeurs manquantes
summary(churn)

#=> les variables VMail.Message, Day.Mins, Eve.Mins et Night.Charge contiennent des valeurs manquantes
#Pour VMail.Message => donnée manquante remplacée par la médiane 
#Pour les autres variables par la moyenne

#Deux méthodologies pour remplacer les données manquantes :
#Méthode 1
# Récupération des indices des lignes contenant des valeurs manquantes
case_NA=which(is.na(churn$VMail.Message))

# Remplacer les valeurs manquantes à l'aide des indices
churn$VMail.Message[case_NA]= median(churn$VMail.Message,na.rm=TRUE)

#Méthode 2
# ou directement grâce à la fonction replace_na
churn$Day.Mins=replace_na(churn$Day.Mins, mean(churn$Day.Mins,na.rm=TRUE))
churn$Eve.Mins=replace_na(churn$Eve.Mins, mean(churn$Eve.Mins,na.rm=TRUE))
churn$Night.Charge=replace_na(churn$Night.Charge, mean(churn$Night.Charge,na.rm=TRUE))

# On verifie à l'aide du summary que nous avons bien remplacé tous les NA
summary(churn)



# Présence de valeurs aberrantes ?
# En effectuant le summary il est déjà possible de se poser des questions sur les valeurs aberrantes
# pour certaines variables (Night.Calls par exemple Max = 10004.0 alors que la médiane = 100.0)

# Nous allons tenter de les visualiser à l'aide des boxplots
# En traçant les boxplots les données aberrantes se verront visuellement

ggplot(churn) + aes(x = "Boite à moustache VMail.Message", y = churn[,8]) + geom_boxplot(fill = "gold") # Box Plot pour VMail.Message
ggplot(churn) + aes(x = "Boite à moustache Night.Calls", y = churn[,16]) + geom_boxplot(fill = "gold") # Box Plot pour Night.Calls
ggplot(churn) + aes(x = "Boite à moustache Day.Charge", y = churn[,11]) + geom_boxplot(fill = "gold") # Box Plot pour Day.Charge
ggplot(churn) + aes(x = "Boite à moustache Day.Mins", y = churn[,9]) + geom_boxplot(fill = "gold") # Box Plot pour Day.Mins
ggplot(churn) + aes(x = "Boite à moustache Eve.Calls", y = churn[,13]) + geom_boxplot(fill = "gold") # Box Plot pour Eve.Calls
ggplot(churn) + aes(x = "Boite à moustache Intl.min", y = churn[,18]) + geom_boxplot(fill = "gold") # Box Plot pour Intl.min
# => Visuellement on constate que les variables Night.Calls, Day.Charge, Day.Mins et Intl.min ont des données aberrantes

# Nous décidons de supprimer toutes les lignes qui sont 2 * supérieur au 3ieme quartile pour ces variables
# Récupération des indices des lignes à supprimer
Night_Calls_sup=which(churn$Night.Calls>quantile(churn$Night.Calls)[4]*2)
Day_Charge_sup=which(churn$Day.Charge>quantile(churn$Day.Charge)[4]*2)
Day_Mins_sup=which(churn$Day.Mins>quantile(churn$Day.Mins)[4]*2)
Intl_Mins_sup=which(churn$Intl.Mins>quantile(churn$Intl.Mins)[4]*2)

# Suppression des lignes contenant les valeurs aberrantes grâce aux indices
churn <- churn[-c(Night_Calls_sup, Day_Charge_sup, Day_Mins_sup, Intl_Mins_sup),]
# => En relancant les boxplots nous constatons que les valeurs aberrantes ne sont plus présentes



# Compter le nombre de ligne par State et tri en décroissant
nbr_par_state<-as.data.frame(table(churn$State))
arrange(nbr_par_state,desc(nbr_par_state$Freq))

# On verifie que la somme est bien égale au nombre de ligne du dataset
sum(nbr_par_state[2])
dim(churn)

# Doit t'on supprimer des states ?
# => Nous remarquons que les individus sont réparties de manière homogènes dans les 
# differents états, ainsi nous n'en supprimerons pas pour ne pas perdre de données



#   Manipuler les variables
#------------------------------------------------------------------------------#
# Création de flag pour les variables à 2 modalités (Churn, Int.l.Plan, VMail.Plan)
#Modification des formats des variables
churn$Churn.<- as.character(churn$Churn.)
churn$Int.l.Plan<- as.character(churn$Int.l.Plan)
churn$VMail.Plan<- as.character(churn$VMail.Plan)

churn <- mutate(churn, Churn. = if_else(Churn. == "True.", "1","0"),
                       Int.l.Plan = if_else(Int.l.Plan == "yes", "1","0"),
                       VMail.Plan = if_else(VMail.Plan == "yes", "1","0"))
                     

#Variables passées en facteur
churn$Churn.<- as.factor(churn$Churn.)
churn$Int.l.Plan<- as.factor(churn$Int.l.Plan)
churn$VMail.Plan<- as.factor(churn$VMail.Plan)



# Formater la date de naissance 
# Format de la date de naissance
str(churn$date_naissance)

# Transformer la variabel en date
churn$date_naissance=as.Date(as.character(churn$date_naissance),format="%m/%d/%Y")

# Construction d'une variable qui contient la date du jour
today=format(Sys.Date(), "%m/%d/%Y")
today<-as.Date(today, "%m/%d/%y")

# verification du format des dates
class(churn$date_naissance)
class(today)

# Calcul de l'age par rapport à la date du jour
churn$age=round(time_length(interval(churn$date_naissance, today), "years"),0)-1
summary(churn$age)



#   Création d'indicateurs
#------------------------------------------------------------------------------#
# Création d'une nouvelle variable tranches d'ages
churn <- mutate(churn, age_classe = case_when(age < 45 ~ "moins de 45 ans",
                                              45 <= age & age <= 60 ~ "entre 45 et 60 ans",
                                              age > 60 ~ "plus de 60 ans",
                                              TRUE ~ "Non renseignée"))

# Création de tranches pour l'ancienneté en mois
churn <- mutate(churn, Account_classe = case_when( Account.Length < 75 ~ "moins de 75",
                                                   75 <= Account.Length & Account.Length <=100 ~ "entre 75 et 100",
                                                   100 < Account.Length & Account.Length <=130 ~ "entre 100 et 130",
                                                   130 < Account.Length ~ "Plus de 130", 
                                                   TRUE ~ "Non renseignée"))

# Création de la somme Mins/Calls/Charge par jour
churn$Mins_per_Days = churn$Day.Mins + churn$Eve.Mins + churn$Night.Mins
churn$Calls_per_Days = churn$Day.Calls + churn$Eve.Calls + churn$Night.Calls
churn$Charge_per_Days = churn$Day.Charge + churn$Eve.Charge + churn$Night.Charge



#   Aller plus loin
#------------------------------------------------------------------------------#
# Est-il nécessaire d'avoir l'option international pour passer des appels à l'étranger ?
# Croisement des variables Int.l.plan et Intl.Calls
table(churn$Int.l.Plan,churn$Intl.Calls)
# => Les clients sans l'option (Int.l.plan = 0) peuvent avoir passé des appels à l'étranger (Intl.Calls > 0)

# Table des clients ayant une forte activité à l'international (i.e plus de 6 appels/jour à l'étranger)
client_Inter=subset(churn, churn$Intl.Calls >= 6)




###############################################################################
#################### Statistiques univariées
###############################################################################

#   Analyse d'une variable qualitative
#------------------------------------------------------------------------------#
# Calculer la loi de distribution effectifs et frequences de la variable Area.Code
prop_area<-round(prop.table(table(churn$Area.Code)),2)

# Réaliser un diagramme circulaire de la variable Area.Code (en fréquence)
lbls <- paste(names(prop_area), "\n", prop_area, sep="")
pie(prop_area, labels=lbls, main="Repartion de la 
    variable Code.Area en frequences")
#=> La modalité 415 est la plus représentée


# Diagramme en barre de la variable classe d'âge
ggplot(churn) + aes(x = age_classe,fill = factor(age_classe)) + geom_bar()

#Autre méthode
barplot(sort(table(churn$age_classe)),col=c("green","blue","red"))
legend(x="topleft",legend=c("[0;45[","[45,60[","60;+["),fill=c("red","green","blue"))
#=> Modalité la plus représentée : Moins de 45 ans


#  Analyse d'une variable quantitative
#------------------------------------------------------------------------------#
# Histogramme de la variable Account.Length
ggplot(churn, aes(x=Account.Length)) + geom_histogram(binwidth=10,fill="pink")


#AUtre méthode
hist(churn$Account.Length,
     col="darkmagenta",
     main="Distribution de la variable Account.Length")

#=> temps d'ancienneté le plus fréquent : Autour de 100 mois d'ancienneté 




###############################################################################
#################### Statistiques bivariées
###############################################################################

#   Analyse de deux variables qualitatives
#------------------------------------------------------------------------------#
# Ancienneté et churn

#Vérification des formats des variables
class(churn$Account_classe)
class(churn$Churn.)
churn$Account_classe=as.factor(churn$Account_classe)

#Représenter sur un graphique la variable Churn en fonction de la tranche d'ancienneté
ggplot(churn) + aes(x = Account_classe,fill=Churn.) + geom_bar(position = "dodge")

# Peut on dire que les variables tranches d'ancienneté et churn sont indépendantes ?
chisq.test((table(churn$Account_classe,churn$Churn.)))
# p-value > 5% ==> variable indépendantes


#   Analyse de deux variables quantitatives
#------------------------------------------------------------------------------#
# Days.Mins et Eve.Mins 

# Nuage de point entre les variables Days.Mins et Eve.Mins
ggplot(churn, aes(x = Day.Mins, y = Eve.Mins))+ geom_point(size = 2,col="blue") 

# Test de corrélation entre les variables Days.Mins et Eve.Mins
cor.test(churn$Day.Mins,churn$Eve.Mins)
#=> Variables qui ne sont pas du tout corrélées


#   Analyse d'une variable qualitative et d'une variable quantitative
#------------------------------------------------------------------------------#

# Area.Code et Night.Calls
#Réaliser un tableau contenant la moyenne de Night.Calls par Area.Code
moy_area<-aggregate(x = churn$Night.Calls, # Specify data column
                    by = list(churn$Area.Code),      # Specify group indicator
                    FUN = mean)  
moy_area
#=> Les moyennes sont similaires quelle que soit la valeur de Area Code


# Test dégalité des moyenne par Area.Code
t.test(churn$Night.Calls[churn$Area.Code=="408"],churn$Night.Calls[churn$Area.Code=="415"])
t.test(churn$Night.Calls[churn$Area.Code=="415"],churn$Night.Calls[churn$Area.Code=="510"])
t.test(churn$Night.Calls[churn$Area.Code=="510"],churn$Night.Calls[churn$Area.Code=="408"])




###############################################################################
#################### Modèle de score
###############################################################################


#   Préambule
#------------------------------------------------------------------------------#
#Vérifier que la variable cible est bien un facteur
class(churn$Churn.)

# Conserver les variables listées pour la modélisation ainsi que l'ID client
churn<-churn[,c(4,22,6,26,21,28,7,9,19,18,12)]

#Bases d'apprentissage et de validation
#Taux de clients dans l'échantillon et graine pour l'aléatoire :
rateEch <- 0.75; set.seed(2016) 

#Extraction des lignes avec Y = 1 et construction de deux dataframes => un contenant les clients avec Y = 1 et l'autre avec les clients Y = 0 :
ligneP <- which(churn$Churn. == 1)
P <- churn[ ligneP , ]
N <- churn[ - ligneP , ]

#Extraction de 75% du dataframe P et de 75% du dataframe N:
indPTrain <- sample( 1:dim(P)[ 1 ], size = round( length(P$Churn.) * rateEch ), replace = FALSE)
indNTrain <- sample( 1:dim(N)[ 1 ], size = round( length( N$Churn.) * rateEch ), replace = FALSE)

#Construction des bases train et test :
train <- rbind(P[ indPTrain , ], N[ indNTrain , ] )           #construction de train en prenant indPTrain et indNTrain (75%)
test <- rbind( P[ - indPTrain , ], N[ - indNTrain , ] )       #construction de test en ne prenant pas indPTrain et indNTrain (25%)

#Vérification du taux de cible sur don, train et test :
table(churn$Churn.); prop.table(table(churn$Churn.))
table(train$Churn.); prop.table(table(train$Churn.))
table(test$Churn.); prop.table(table(test$Churn.))



#   Discrétiser les variables
#------------------------------------------------------------------------------#

#Extraction de toutes les variables quantitatives
ind.quant <- sapply(train, function(x) is.numeric(x) | is.integer(x))
Data.quant <- train[ ,ind.quant]
var_quanti <- colnames(Data.quant)

#Extraction d'un sous dataframe ne contenant que ces variables et l'ID client
train_SD <- train[,c("Phone",var_quanti)]

#Découpage en tranches des variables à partir du minimum, de la valeur à 25%, de la valeur à 50%, de la valeur à 75%, du maximum
DISCRETISE_DF <- function(DF) {
  nbvar <- dim(DF)[2]                                              #Nombre de variables pour boucler sur toutes les variables
  alpha1 <- c(1/4,1/2,3/4)                                                 #Bornes de découpage
  for (i in 2:nbvar) {
    nom <- paste(colnames(DF)[i],"_D",sep="")               #Nom de la nouvelle variable à partir du nom de la variable initiale
    nom_var <- colnames(DF)                                        #Nom des colonnes du data frame
    q <- quantile(DF[,i],alpha1)                                     #Recherche des valeurs associées aux bornes
    breaks <- c(min(DF[,i]),q[1],q[2],q[3],max(DF[,i]))          #Borne de découpage
    var_ <- cut(DF[,i], unique(breaks),include.lowest = TRUE)    #Découpage de la variable
    DF <- cbind(DF,var_)                                        #Ajout de la nouvelle variable dans le data frame 
    names(DF) <- c(nom_var,nom)                                #Renommer la nouvelle variable
  }
  return(DF)
}

train_SD2 <- DISCRETISE_DF(DF= train_SD)

#Vérification
summarise(group_by(train_SD2,Day.Mins_D), min = min(Day.Mins), max = max(Day.Mins))


#   Disjoncter les variables
#------------------------------------------------------------------------------#

#Extraction dans le data frame initial des variables qualitatives et de l'ID client
ind.quali <- sapply(train, function(x) is.factor(x))
Data.quali <- train[ ,ind.quali]
var_quali <- colnames(Data.quali)
train_QUALI <- train[,c("Phone",var_quali)]

#Regrouper les données qualitatives initiales et les qualitatives créées
train <- merge(train_QUALI,train_SD2[,c(1,(dim(train_SD)[2]+1) : dim(train_SD2)[2])],by="Phone")


#Création d'un sous dataframe ne contenant pas la variable cible qui ne doit pas être disjonctée
train_SS_VC <- train[,-2]

#Fonction permettant de disjoncter les variables d'un dataframe
DIJ_DF <- function(DF) {
  nbvar <- dim(DF)[2]    #Nombre de variables pour boucler sur toutes les variables
  for (i in 2:nbvar) {
    var <- DF[,i]                                                                      #Extraction de la variable
    dum <- data.frame(model.matrix( ~ var - 1, data=DF))        #Passage en dummies de la variable
    nomvar <- paste(colnames(DF)[i],"_",sep="")                #Nom de la nouvelle variable
    nom <- colnames(dum)			      #Modification du nom des colonnes de dum
    nom <- gsub( "var", nomvar, nom)
    names(dum) <- nom
    DF <- cbind(DF,dum)	                                            #Ajout au data frame de la nouvelle variable
  }
  return(DF)
}

train_DIJ <- DIJ_DF(DF= train_SS_VC) 

# Suppression des variables initiales du dataframe pour ne conserver que les nouvelles variables créées et disjonctées ainsi que la variable cible et l'ID client
train_ <- merge(train[,c("Phone","Churn.")],train_DIJ[, c(1,(dim(train)[2]): dim(train_DIJ)[2])],by="Phone")



#On conserve le nom des variables initiales dans un vecteur (sauf la varible cible et l'ID client) => utilisé au moment du khi deux
#Noms des variables remis dans le même ordre que le data frame train_

vecteur_nom <- c(colnames(train)[c(3:5)],colnames(train)[6:(dim(train)[2])])




#   Suppression de la modalité de référence
#------------------------------------------------------------------------------#
#Suppression de la modalité avec le plus petit khi deux par rapport à la variable cible
KHI_DF <- function(DF) {
  nbvar <- length(vecteur_nom)    #Nombre de variables initiales pour boucler sur toutes les variables
  vectnom <- c(rep("",each=nbvar))  #Création d'un vecteur dans lequel les variables à supprimer vont être stockées
  for (i in 1:nbvar) {
    nomvar <- colnames(DF)  #Nom des variables du data frame
    #Recherche des variables disjonctées liées à chaque variable initiale
    lignes <- which(substring(nomvar,1,nchar(vecteur_nom[i]))==vecteur_nom[i])
    SOUS_DF <- DF[,c(2,lignes)]  #Extraction de ces variables et de la variable cible
    vect <- c(rep(0,each=dim(SOUS_DF)[2]-1))   #Création d'un vecteur qui va contenir les valeurs des Khi 2
    for (j in 2:dim(SOUS_DF)[2]) {
      contingence <- table(a = SOUS_DF[,1], b = SOUS_DF[,j])  #Tableau de contingence
      res <- chisq.test(contingence)   #Calcul du Khi 2
      vect[j-1] <- res$statistic    #Récupération du Khi 2 et stockage dans le vecteur
    }
    min_khi <- which.min(vect)    #Recherche de la valeur min du Khi 2
    nomsuprr <- colnames(SOUS_DF)[min_khi+1]   #Recherche du nom de cette variable
    vectnom[i] <- nomsuprr   #Nom de la variable stocké dans le vecteur
  }
  return(vectnom)
}
vectsuppr <- KHI_DF(DF= train_)

#Suppression des variables du data frame initial
#Recherche du numéro des colonnes à supprimer et suppression
colsuppr <- which(colnames(train_) %in% vectsuppr)
train_KHI2 <- train_[,-colsuppr]


#   Corrélations
#------------------------------------------------------------------------------#
#Fonction qui permet de calculer les corrélations entre toutes les variables
#Pour supprimer les variables les plus corrélées
COR_DF <- function(DF) {
  SOUS_DF <- DF[,-c(1)]    #Data Frame sans variable cible
  nbvar <- dim(SOUS_DF)[2]    #Nombre de variables sur lesquelles faire les corrélations
  mat <- matrix(0,nbvar,nbvar)  #Création d'une matrice vide
  rownames(mat) <- colnames(SOUS_DF)  #Nom des lignes avec le nom des variables
  colnames(mat) <- colnames(SOUS_DF)  #Nom des colonnes avec le nom des variables
  for (i in 1:nbvar) {
    for (j in 1:nbvar) {
      cor <- cor.test(SOUS_DF[,i],SOUS_DF[,j],method=c("pearson")) 
      mat[i,j] <- cor$estimate
    }
  }
  return(mat)
}

#Extraction de la matrice des corrélations et analyse pour savoir quelles variables supprimées
matrice_cor <- COR_DF(DF= train_KHI2)
write.csv2(matrice_cor,file="C:/Users/krh/Documents/PROJET_SFR/DONNEES/Matrice_corr_simple.csv" , row.names = TRUE)

#Pas de suppression de variable au vu des resultats de la corrélation (seuil choisi de 0,7)
train <- train_KHI2



#   La Régression Logistique
#------------------------------------------------------------------------------#
#Fonction qui permet de calculer le lift entre chaque variable et la variable cible : pour identifier les variables avec le plus de lien avec la variable cible
SELECT_DF <- function(DF) {
  #Extraction d'une ligne du dataframe dans laquelle les résultats vont être stockés
  resultats <- DF[1,c(3:dim(DF)[2])]
  nb_clients <- dim(DF)[1] #Nombre de clients total pour calcul du taux de cible global
  nbvar <- dim(DF)[2]
  for (i in 3:nbvar) {
    resultats[1,(i-2)] <- NA
    rap1 <- dim(DF[DF[,2]==1 & DF[,i]==1,])[1] / dim(DF[DF[,i]==1,])[1] #Taux de cible de la variable
    rap2 <- dim(DF[DF[,2]==1,])[1] / nb_clients #Taux de cible global
    rap <- rap1 / rap2 #Calcul du rapport
    resultats[1,(i-2)] <- rap
  }
  return(resultats)
}
train_SELECT <- SELECT_DF(train)
colnames(train_SELECT)

#Analyse des rapports (lifts)
train_SELECT

#Suppression des variables avec un lift entre 0,8 et 1,2 => variables avec peu de lien avec la variable cible
train <- subset(train,select=-c(Intl.Calls_D_.3.4.,Intl.Calls_D_.4.6.,
                                Intl.Mins_D_.0.8.5.,Intl.Mins_D_.10.3.12.1.,
                                Eve.Mins_D_.167.202.))

#Liste des variables explicatives
X <- train[,3:dim(train)[2]]

#Passage de la variable cible en numérique
train$Churn. <- as.numeric(as.character(train$Churn.))

#Modèle Grossier 
reg0<-glm(train$Churn. ~1, family = "binomial")
summary(reg0)

#Modèle Fin
reg1 <- glm(train$Churn. ~ X[,1]+X[,2]+X[,3]+X[,4]+X[,5]+X[,6]+X[,7]+X[,8]+X[,9]+X[,10]+X[,11]+X[,12]+X[,13]+
                           X[,14]+X[,15]+X[,16]+X[,17]
            ,family = "binomial", control = list(maxit = 1000)) # Nbr max de paramètres dans le modèle


#Choix d'un modèle que l'on considère comme le meilleur (référence)
myscope<-list(upper =  ~ X[,1]+X[,2]+X[,3]+X[,4]+X[,5]+X[,6]+X[,7]+X[,8]+X[,9]+X[,10]+X[,11]+X[,12]+X[,13]+
                         X[,14]+X[,15]+X[,16]+X[,17]
              , lower = ~ 1) 

#Méthode «both» : A chaque étape on teste l'ajout d'une variable : toutes les variables sont testées en utilisant AIC, on la supprime si le modèle est dégradé
reg.AICboth <- stepAIC(reg0,direction=c("both"),scope=myscope)
summary(reg.AICboth)

#Nouveau modèle sans la variable 11 qui n'est pas significative
#Les variables 8, 16 et 17 sont aussi supprimées carmoins significatives que les autres
reg_1 <- glm(train$Churn. ~ X[,1]+X[,2]+X[,5]+X[,7]+X[,9]+X[,10]+X[,13]+X[,14]+X[,15]
            ,family = "binomial", control = list(maxit = 1000)) # Nbr max de paramètres dans le modèle
summary(reg_1)



#REAPPLICATION
#Modèle final avec uniquement les variables choisies
X2 <- train[,c("Int.l.Plan_1",
               "VMail.Plan_1",
               "Mins_per_Days_D_.642.877." ,
               "CustServ.Calls_D_.2.9.",
               "Charge_per_Days_D_.56.5.63.7.",
               "Charge_per_Days_D_.63.7.92.6.",
               "Day.Mins_D_.216.351.",
               "Intl.Calls_D_.0.3.",
               "Intl.Mins_D_.12.1.20.")]

MODELE_F <- glm(train$Churn. ~ X2[,1]+X2[,2]+X2[,3]+X2[,4]+X2[,5]+X2[,6]+X2[,7]+X2[,8]+X2[,9], 
                family = "binomial", control = list(maxit = 1000))

summary(MODELE_F)

#Pour identifier les bornes de chaque variable utilisée dans la modélisation
#summarise(group_by(train_SD2,Charge_per_Days_D), min = min(Charge_per_Days), max = max(Charge_per_Days))



############ POUR L'ECHANTILLON TEST #######################################
#Les bornes de construction pour train sont réutilisées pour test
test <- mutate(test, Mins_per_Days_D_.642.877. = if_else((642 < Mins_per_Days & Mins_per_Days <= 877), 1,0), 
               CustServ.Calls_D_.2.9. = if_else((2 < CustServ.Calls & CustServ.Calls <= 9), 1,0), 
               Charge_per_Days_D_.56.5.63.7. = if_else((56.5 < Charge_per_Days & Charge_per_Days<= 63.7), 1,0),
               Charge_per_Days_D_.63.7.92.6. = if_else((63.7 < Charge_per_Days & Charge_per_Days<= 92.6), 1,0),
               Day.Mins_D_.216.351. = if_else((216 < Day.Mins & Day.Mins<= 351), 1,0),
               Intl.Calls_D_.0.3. = if_else((0 < Intl.Calls & Intl.Calls<= 3), 1,0),
               Intl.Mins_D_.12.1.20. = if_else((12.1 < Intl.Mins & Intl.Mins<= 20), 1,0))

#Vérification de la construction pour quelques variables
summarise(group_by(test,Mins_per_Days_D_.642.877.), min = min(Mins_per_Days), max = max(Mins_per_Days))
summarise(group_by(test,CustServ.Calls_D_.2.9.), min = min(CustServ.Calls), max = max(CustServ.Calls)) 

#Les variables Int.l.Plan et V.Mail.Plan sont transformées en numériques car définies ainsi dans le modèle
test$Int.l.Plan_1=as.numeric(as.character(test$Int.l.Plan))
test$VMail.Plan_1=as.numeric(as.character(test$VMail.Plan))

#Calcul du score sur les données test
X2 <- test[,c("Int.l.Plan_1",
              "VMail.Plan_1",
              "Mins_per_Days_D_.642.877." ,
              "CustServ.Calls_D_.2.9.",
              "Charge_per_Days_D_.56.5.63.7.",
              "Charge_per_Days_D_.63.7.92.6.",
              "Day.Mins_D_.216.351.",
              "Intl.Calls_D_.0.3.",
              "Intl.Mins_D_.12.1.20.")]

res <- predict(MODELE_F,type="response", newdata = data.frame(X2))

#Ajout de la variable de prédiction dans le jeu de données test
test$scoreREG = res



#   Calcul des déciles et synthèse

#Fonction permettant le calcul des déciles
#Application sur l'échantillon test : même principe pour l'échantillon train
calcul_deciles <- function(id,score) { # id est la liste des individus / score est le score associé à la liste d'individus
  
  indiceTri <- sort(score,decreasing=T, index.return=T)   #indiceTri sort indiceTri$ix = vecteur des indices de 				               score dans l'ordre qui permet d'avoir les valeurs de score 				               dans l'ordre décroissant
  idTri <- id[indiceTri$ix]           		             # idTri = vecteur des individus dans l'ordre qui permet 				               d'avoir les valeurs de score dans l'ordre décroissant
  scoreTri <- score[indiceTri$ix]
  
  z <- seq(from=1,to=length(id),by=1)
  quantiles<-quantile(z,seq(0,1,0.1))
  
  tab <- data.frame(n=z,ID_INDIV=idTri,score=scoreTri)
  tab$decile <- NA
  tab$decile[1] <- 1
  for(i in 1:(length(quantiles)-1)){tab$decile[which((tab$n>quantiles[i] & tab$n<quantiles[i+1])|tab$n==quantiles[i+1])]=i}
  tab
}
deciles_score <- calcul_deciles(test$Phone, test$scoreREG)


#Synthèse des déciles
#Ajout de la variable cible pour calculer les indicateurs par décile
deciles_score <- left_join(deciles_score, test[, c("Phone", "Churn.")], by = c("ID_INDIV" = "Phone"))

#Variable cible passée en numérique pour les comptages
deciles_score$Churn. <- as.numeric(as.character(deciles_score$Churn.))

#Synthèse des résultats
#Comptage du nombre d'individus par décile
res_score <- aggregate(ID_INDIV ~ decile, data = deciles_score, length)

#Comptage du nombre d'individus qui valent "1" pour "Churn." par classe
res_score <- left_join(res_score, aggregate(Churn. ~ decile, data = deciles_score, sum), by = "decile") 
names(res_score) <- c("Décile", "Nb_clts_tot", "Nb_clts_cible")

#Calcul du taux de cible
res_score$Tx_cible <- res_score$Nb_clts_cible/res_score$Nb_clts_tot

#Ajout des sommes cumulées
res_score <- data.frame(res_score, apply(res_score[, c("Nb_clts_tot", "Nb_clts_cible")], 2, cumsum))
names(res_score)[5:6] <- c("Nb_clts_tot_cum", "Nb_clts_cible_cum")

#Ajout du taux de cible cumulé
res_score$Tx_cible_cum <- res_score$Nb_clts_cible_cum/res_score$Nb_clts_tot_cum

#Calcul du lift
res_score$Lift <- res_score$Tx_cible/mean(res_score$Tx_cible)

#Ajout du total et du taux de cible
res_score <- rbind(res_score, apply(res_score, 2, sum))
res_score$Décile[nrow(res_score)] <- "Total"
res_score[nrow(res_score), 4:8] <- NA
res_score[nrow(res_score), 4] <- prop.table(table(test$TOP))[2]
res_score_test <- res_score

#Export de la synthèse en CSV
write.csv2(res_score,file="C:/Users/krh/Documents/PROJET_SFR/DONNEES/Synthèse_des_déciles_simple.csv ")




############ POUR L'ECHANTILLON TRAIN #######################################

X2 <- train[,c("Int.l.Plan_1",
               "VMail.Plan_1",
               "Mins_per_Days_D_.642.877." ,
               "CustServ.Calls_D_.2.9.",
               "Charge_per_Days_D_.56.5.63.7.",
               "Charge_per_Days_D_.63.7.92.6.",
               "Day.Mins_D_.216.351.",
               "Intl.Calls_D_.0.3.",
               "Intl.Mins_D_.12.1.20.")]

MODELE_F <- glm(train$Churn. ~ X2[,1]+X2[,2]+X2[,3]+X2[,4]+X2[,5]+X2[,6]+
                  X2[,7]+X2[,8]+X2[,9], 
                family = "binomial", control = list(maxit = 1000))

#Calcul du score sur les données train

res <- predict(MODELE_F,type="response", newdata = data.frame(X2))

#Ajout de la variable de prédiction dans le jeu de données train
train$scoreREG = res


#Calcul des déciles et synthèse
deciles_score <- calcul_deciles(train$Phone, train$scoreREG)

#Ajout de la variable cible pour calculer les indicateurs par décile
deciles_score <- left_join(deciles_score, train[, c("Phone", "Churn.")], by = c("ID_INDIV" = "Phone"))

#Variable cible passée en numérique pour les comptages
deciles_score$Churn. <- as.numeric(as.character(deciles_score$Churn.))

#Synthèse des résultats
#Comptage du nombre d'individus par décile
res_score <- aggregate(ID_INDIV ~ decile, data = deciles_score, length)

#Comptage du nombre d'individus qui valent "1" pour "Churn." par classe
res_score <- left_join(res_score, aggregate(Churn. ~ decile, data = deciles_score, sum), by = "decile") 
names(res_score) <- c("Décile", "Nb_clts_tot", "Nb_clts_cible")

#Calcul du taux de cible
res_score$Tx_cible <- res_score$Nb_clts_cible/res_score$Nb_clts_tot

#Ajout des sommes cumulées
res_score <- data.frame(res_score, apply(res_score[, c("Nb_clts_tot", "Nb_clts_cible")], 2, cumsum))
names(res_score)[5:6] <- c("Nb_clts_tot_cum", "Nb_clts_cible_cum")

#Ajout du taux de cible cumulé
res_score$Tx_cible_cum <- res_score$Nb_clts_cible_cum/res_score$Nb_clts_tot_cum

#Calcul du lift
res_score$Lift <- res_score$Tx_cible/mean(res_score$Tx_cible)

#Ajout du total et du taux de cible
res_score <- rbind(res_score, apply(res_score, 2, sum))
res_score$Décile[nrow(res_score)] <- "Total"
res_score[nrow(res_score), 4:8] <- NA
res_score[nrow(res_score), 4] <- prop.table(table(train$TOP))[2]

res_score_train <- res_score


#Export de la synthèse en CSV
write.csv2(res_score,file="C:/Users/krh/Documents/PROJET_SFR/DONNEES/Synthèse_des_déciles_simple_train.csv ")


#Comparaison des synthèse des déciles sur les deux échantillons
#10% des effectifs dans chaque décile
#Taux de cible qui n'est plus décroissant pour les déciles les plus bas
#Lift avec quelques écarts entre train et test
#=> Nous allons regarder les performances au niveau du Lift mais le modèle nécessiterait des ajustements



#   Courbe de Lift
#------------------------------------------------------------------------------#
#Choix de la variable de score
score_test <- test$scoreREG
score_train <- train$scoreREG

#Utilisation de fonctions pré programmées
#Chemin du fichier contenant les fonctions
dir_fonction <- "C:/Users/cpe/Desktop/Data SFR Formation/fonctionsFormation.R"
source(dir_fonction)
nbre_point <- 50
ideal <- courbe_ideale(test$Churn.,1,nbre_point)   	#Courbe idéale
hasard <- courbe_hasard(nbre_point)			#Courbe hasard
notrelift_test <- courbe_lift(test$Churn.,score_test,nbre_point) 	#Courbe Lift pour le test
notrelift_train <- courbe_lift(train$Churn.,score_train,nbre_point) 	#Courbe Lift pour le train
plot(hasard,ideal,type='l',col='red') 
lines(hasard,notrelift_test,type='l',col='blue') 
lines(hasard,notrelift_train,type='l',col='green') 
lines(hasard,hasard,type='l')

#Les courbes de Lift sont proches mais pas tout à fait confondues
#=> Cette conclusion rejoint ce qui a été vu sur la synthèse des déciles => le modèle nécessite des ajustements
# en essayant d'ajouter / supprimer des variables



###############################################################################
#################### Segmentation
###############################################################################

#   Chargement des données
#------------------------------------------------------------------------------#
# Import du fichier churn de données
churn <- read.csv('C:/Users/cpe/Desktop/Data SFR Formation/churn.csv')


#   Construction du datamart segmentation
#------------------------------------------------------------------------------#
# Objectif : réaliser une segmentation sur les usages clients
# Construction du datamart segmentation
# Périmètre client : Nous prenons en compte l'ensemble des clients
# Variables : Nous prenons uniquement les variabels liées à l'usage + l'ID
# Nous gardons l'ensemble des variables de VMail.Message (colonne 7) à CustServ.Calls (colonne 20)

churn<-churn[,c(4,7:20)]

# Suppression des variables Charge
churn<-churn[,-which(names(churn) %in% c("Day.Charge","Eve.Charge","Night.Charge","Intl.Charge"))]

#Vérification
str(churn)


#   Réduction des dimensions
#------------------------------------------------------------------------------#
# Seulement des variables quantitatives dans le jeu de données ==> ACP pour la réduction des dimensions sur l'ensemble des variables
res_PCA<-PCA(
  churn[,c(2:11)], # Selection des variables
  scale.unit = TRUE, # Normalise les variables automatiquement pas besoin de reprendre les transformations manuellement
)

#Critère du coude et Critère de Kaiser
plot(res_PCA$eig[,1],type="l")
res_PCA$eig
#=> Sélection des 4 premières composantes principales qui portent 42% de l'inertie

# contribution des variables
res_PCA$var$contrib

# coordonnées des variables
res_PCA$var$coord

#Interprétation des axes : 10 variables => nous retenons les variables avec au moins 10% de contribution
#Pour interpréter les axes nous utilisons les variables avec les contributions les plus importantes
#Axe 1 : Axe des clients qui appellent la journée et à l'international (Day.Calls, Intl.Mins et Intl.Calls avec signe positif)
#Axe 2 : oppose les clients qui appellent la journée et la nuit (Day.Mins et Night.Calls avec signe positif), aux clients qui appellent le service client (CustServ.Calls avec signe négatif)
#Axe 3 : Axe des clients qui appellent la journée et la nuit (Night.Mins et Day.Calls avec signe positif)
#Axe 4 : Oppose les clients qui appellent en soirée et avec appels au service clients (Eve.Calls et CustServ.Calls avec signe positif), aux clients qui utilisent Vmail Message (CustServ.Calls avec signe négatif)



#   Modélisation par classification Mixte
#------------------------------------------------------------------------------#
#Récupération des 4 composantes principales sélectionnées
donnees_seg<-as.data.frame(res_PCA$ind$coord[,1:4])
names(donnees_seg)<-c("PCA1","PCA2","PCA3","PCA4")

#Modélisation par une classification mixte :
#Etape 1 : réaliser une Kmeans sur 100 centroïdes
centroides <- kmeans(donnees_seg,centers = 100,iter.max=20)

#Etape 2 : CAH sur les résultats de la Kmeans
D2 <- dist(centroides$centers, method=c("euclidean")) 
dendro <- hclust(D2,method = "ward.D" )
plot(dendro)

#Découpage en 3 classes (Interprétation du dendogramme)
CLcha <- as.data.frame(cutree(dendro,3))
names(CLcha) <- "clusterCHA"
CLcha$clusterKM <- as.numeric(rownames(CLcha)) 

#Affectation à chaque ID de la table Kmeans du cluster CAH correspondant
CLkmeans <- as.data.frame(centroides$cluster)
names(CLkmeans) <- "clusterKM" 
CLkmeans <- cbind(churn$Phone, CLkmeans)
CLkmeans <- as.data.frame(CLkmeans)
Mixte <- merge(CLcha,CLkmeans,by = "clusterKM") # On réaffecte chaque client donné par la kmeans aux classes trouvées dans la CAH
colnames(Mixte)[colnames(Mixte)=="churn$Phone"] <- "Phone"

#On ne conserve que le cluster de la Mixte (contenu dans la variable clusterCHA)
res_final <- Mixte[,c("clusterCHA","Phone")]
names(res_final) <- c("Clust_Mixte","Phone")

#Représentation des points dans le plan principal des variables ACP
var.keep2 <- c("PCA1","PCA2")
plotcluster(donnees_seg[, c("PCA1","PCA2")], res_final$Clust_Mixte)
#=> Les segments sont très mélangés : pas de découpage net entre les clusters


#   Portraits Robot
#------------------------------------------------------------------------------#
# Ajout de la segmentation à la table initiale
don_PR <- left_join(churn,res_final,by="Phone")

#Extraction de toutes les variables quantitatives excepté l'ID client
don_PR$Clust_Mixte<-as.character(as.factor(don_PR$Clust_Mixte)) # La variable de cluster est transformée en factor pour ne  pas que la variable soit prise en compte
ind.quant <- sapply(don_PR, function(x) is.numeric(x) | is.integer(x))
Data.quant <- don_PR[ ,ind.quant]
var.keep_quanti <- colnames(Data.quant)

# Variables quantitatives
# Calculer la moyenne des variables par cluster
Profil_quanti <- aggregate(don_PR[,var.keep_quanti], by=list(seg= don_PR$Clust_Mixte),mean)
Profil_quanti$seg <- as.character(Profil_quanti$seg)

# Moyenne générale
moyCLUST <- apply(don_PR[,c(var.keep_quanti)],2,mean)
moyCLUST <- c("Tous",moyCLUST)

#Regroupement de la donnée
CLUST_quanti <- rbind(Profil_quanti,moyCLUST)


#Interprétation des segments
# A noter : les segments peuvent ne pas être les mêmes que ceux présentés ci dessous => tirage aléatoire de la Kmeans qui fait que les segments peuvent être différents
# Segment 1 : 
# La variable CustServ.Calls est sur représentée => clients qui appellent le service client

# Segment 2 : 
# Les variables Day.Mins et Intl.Calls sont sur représentées => clients qui appellent en journée à l'international

# Segment 3 : 
# Les variables Eve.Mins et VMail.Message sont sur représentées => clients qui appellent en soirée et envoient des emails


#A noter : peu de variables sur représentées => les variables ne discriminent pas correctement la population
#Pistes d'amélioration de la segmentation
#Ajout d'autres variables car celles utilisées n'arrivent pas à discriminer correctement la population : trop peu de variables, certaines variables portent la même information
#En transformant les données à disposition : Il faudrait par exemple ajouter des flags d'utilisation des appels, des messages, de l'email, en journée, en soirée, la nuit,...
#En recherchant dans la base de données de nouvelles données qui pourraient être utilisées

