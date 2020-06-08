#----------------------------------------------------------------------------------#
#                     LES FONDAMENTAUX DATA ANALYST                                #
#----------------------------------------------------------------------------------#


###############################################################################
#################### Déclaration des librairies
###############################################################################
library(titanic)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(rpart)
library(FactoMineR)
library(ade4)
library(vegan)
library(fpc) 
library(cclust)
library(cluster)
library(MASS)


###############################################################################
#################### Présentation du logiciel R
###############################################################################

#   dplyr
#------------------------------------------------------------------------------#
#select() : pour la sélection de variables
#Créer une base don_tit2 ne contenant que les variables  "Pclass", "Age", et "Sex"
don_tit2 <- select(titanic_test, Pclass, Age, Sex)

#Créer une base don_tit2 en supprimant la variable  "SibSp"
don_tit2 <- select(titanic_test, -SibSp)


#filter() : pour filtrer une base. 
#Filtrer les lignes dont l'âge est compris entre 20 et 50 ans (inclus)
don_tit2 <- filter(titanic_test, 20 <= Age, Age <=50)

#Filtrer les individus qui ont embarqué à Southampton et dont l'age est renseigné
don_tit2 <- filter(titanic_test, Embarked == 'S',!is.na(Age))


#arrange() : pour ordonner une base. Attention aux levels des champs facteurs!*
#Trier par Sex et Age (en ordre croissant pour les deux variables) 
don_tit2 <- arrange(titanic_test, Sex, Age)

#Trier par Pclass et Embarked en décroissant
don_tit2 <- arrange(titanic_test, Pclass, desc(Embarked))


#distinct() : conserve uniquement les lignes distinctes des champs sélectionnés. 
#Valeurs distinctes sur Pclass et Embarked
don_tit2 <- distinct(titanic_test, Pclass, Embarked) 


#mutate() : pour créer des variables à partir des champs existants 
#Créer une variables booléenne pour les lignes avec 25 ans et plus et une autre pour les lignes avec une 1ère classe
don_tit2 <- mutate(titanic_test, top_25_ans = if_else(25 <= Age, 1,0),first_class = if_else(Pclass==1, 1,0)) 


#case_when() : pour la création de variables en tranches.  
#Créer une variable «age_tranches» : < 25 ans, 25 - 45 ans, >= 45 ans, «non renseignée»  
don_tit2 <- mutate(titanic_test, age_tranches = case_when( Age < 25 ~ "<25 ans",
                                              25 <= Age & Age <45 ~ "25 - 45 ans",
                                              45 <= Age ~ ">= 45 ans", 
                                              TRUE ~ "Non renseignée"))


#rename() : pour renommer des variables déjà existantes.
#Renommer la variable Pclass en Numero_class
don_tit2 <- rename(titanic_test, Numero_class = Pclass)


#summarise() : pour obtenir des statistiques des champs d'une base. 
#Obtenir un tableau avec la moyenne et le maximum de l'âge. 
summarise(titanic_test, age_moyenne = mean(Age, na.rm = T),max_age = max(Age, na.rm = T))


#group_by() : regroupe un data frame par rapport à certains champs.
#Regrouper la base par rapport à Pclass et Embarked
don_tit2 <- group_by(titanic_test, Pclass, Embarked)

#Obtenir le nombre de passagers et la moyenne d'age dans chacun des groupes créés
don_tit3 <- summarise(don_tit2, nombre = n(), age_moyenne = mean(Age, na.rm = T))


#inner_join() : jointure des tableaux, avec toutes les valeurs correspondantes dans les deux tableaux.  
#Jointure entre titanic_test et don_tit3
don_tit4 <- inner_join(titanic_test, don_tit3, by = c("Pclass", "Embarked"))


#L'opérateur %>% de la librairie 'magrittr'
#Exemple : conserver les lignes de la base pour les hommes, puis calculer l'age moyen par classe et par port de départ (dans cet ordre)
#Solution 1 : 
summarise( group_by( filter(titanic_test, Sex=='male'), Pclass, Embarked), 	age_moy = mean(Age, na.rm = T))

#Solution 2 :
don_tit2 <- filter(titanic_test, Sex=='male') 
don_tit3<- group_by(don_tit2, Pclass, Embarked)
summarise(don_tit3, age_moy = mean(Age, na.rm = T))  

#Solution 3 : 
titanic_test %>% filter(., Sex=='male') %>% group_by(., Pclass, Embarked) %>% summarise(., age_moy = mean(Age, na.rm = T))


#   ggplot2
#------------------------------------------------------------------------------#
#Couche 1 : données et aesthetics 
#Dans le dataframe titanic, représentation de l'âge en fonction du prix par passager. La classe sera utilisée pour la couleur.
g <- ggplot(titanic_test, aes(x = Age, y = Fare, col = Pclass)) 


#Couche 2 : géométrie
#Visualiser les données en utilisant un nuage de points et ajouter la droite de régression linéaire associée :
g2 <- g + geom_point(size = 2) + geom_smooth(method = lm)


#Couche 3 : scale
#Spécifier l'échelle des couleurs pour la variable qui colore et modifier l'échelle des axes *
g3 <- g2 + scale_colour_gradient(low = "orangered1", high = "orangered4") +
  scale_x_continuous(limits = c(0, 100), breaks = c(20, 40, 60, 80)) +
  scale_y_continuous("Prix par passager", limits = c(0, 400), breaks = c(100, 200, 300))


#Couche 4 : Facet
#Séparer le graphe en utilisant la variable Embarked :  
g4 <- g3 + facet_grid(.~ Embarked)


#Pour aller plus loin
#Ajouter un titre
g5 <- g4 + ggtitle("Représentation de l'age en fonction du prix du billet par port d'embarquement")

#Titre des axes en couleur
g6 <- g5 + theme(axis.title.x = element_text(size = 15, colour = "#6699ff"),  
                 axis.title.y = element_text(size = 15, colour = "#ff8080"))

#Mise en forme du fond
g7 <- g6 + theme(panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue",
                                    size = 0.5, linetype = "solid"))


###############################################################################
#################### Datamanagment
###############################################################################

#   Audit des données
#------------------------------------------------------------------------------#
#Création du dataframe d'exemple contenant une variable ID d'identifiant client, une variable AGE d'age en tranches, une variable NB_SMS du nombre de SMS
ID <- as.data.frame(c("ID1","ID2","ID1","ID4","ID5","ID6","ID7","ID8","ID9","ID10"))
NB_SMS <- as.data.frame(as.integer(c(5000,200,400,500,NA,150,250,200,100,50)))
AGE_TR <- c("Moins de 20 ans","Entre 20 et 30 ans","Moins de 20 ans","Moins de 20 ans","Plus de 30 ans","Plus de 30 ans","Entre 20 et 30 ans","Entre 20 et 30 ans","Plus de 30 ans","Plus de 30 ans")
Base_AUDIT <- cbind(ID,NB_SMS,AGE_TR)
names(Base_AUDIT) <- c("ID","NB_SMS","AGE_TR")


#Audit des données
summary(Base_AUDIT)
str(Base_AUDIT)

#Doublons : Vérifier si une variable contient des doublons
#Exemple : ID
length(Base_AUDIT$ID) - length(unique(Base_AUDIT$ID))

#Conserver uniquement les lignes distinctes des champs sélectionnés : 
distinct(Base_AUDIT,ID)       #Pour une variable
distinct(Base_AUDIT,ID,NB_SMS,AGE_TR)     #Pour plusieurs variables

#Tri : tri selon la variable AGE_TR en croissant et nombre de SMS en décroissant
arrange(Base_AUDIT, AGE_TR, desc(NB_SMS))

#Données extrêmes / aberrantes
#Dispersion de la variable NB_SMS - Boite à moustaches
ggplot(Base_AUDIT) + aes(x = "Boite à moustache NB SMS", y = NB_SMS) + geom_boxplot(fill = "gold") 

#Recherche des valeurs correspondant au P1 et au P99
alpha <- seq(0.0,0.99,0.01)
q <- quantile(Base_AUDIT$NB_SMS,alpha,na.rm = TRUE)

#Suppression des clients avec valeur NB SMS supérieure au P99
NB_SMS_P99 <- q[100]
Base_AUDIT_2 <- Base_AUDIT[Base_AUDIT$NB_SMS<NB_SMS_P99 | is.na(Base_AUDIT$NB_SMS),]


#Données manquantes
#Identifier les données manquantes dans la variable NB_SMS
lignes <- is.na(Base_AUDIT_2$NB_SMS)

#Remplacer la donnée manquante par la moyenne des autres valeurs
temp <- Base_AUDIT_2$NB_SMS 
temp[lignes] <- mean(Base_AUDIT_2$NB_SMS, na.rm = TRUE) #Remplacer les lignes manquantes par la moyenne
Base_AUDIT_2$NB_SMS_2 <- temp    #Construction de la variable NB_SMS_2 sans donnée manquante


#   Jointures
#------------------------------------------------------------------------------#
#Création des deux dataframe d'exemple A et B
A <- as.data.frame(c(1,2,3,4,5,6,7))
names(A) <- c("ID")
B <- as.data.frame(c(1,2,3,5,6,8))
names(B) <- c("ID")

#inner join
inner_join(A, B, by = "ID")

#left join
left_join(A, B, by = "ID")

#right join
right_join(A, B, by = "ID")

#anti join sur A
anti_join(A, B, by = "ID")


#anti join sur B
anti_join(B, A, by = "ID")

#full join
full_join(A, B, by = "ID")



#   Filtrage
#------------------------------------------------------------------------------#
#Création du dataframe d'exemple contenant une variable ID d'identifiant client, une variable AGE d'age en tranches et une variable ANC d'ancienneté en tranche
ID <- as.data.frame(c("ID1","ID2","ID3","ID4","ID5","ID6","ID7","ID8","ID9","ID10"))
NB_SMS <- as.data.frame(as.integer(c(500,200,400,500,100,150,250,200,100,50)))
AGE_TR <- c("Moins de 20 ans","Entre 20 et 30 ans","Moins de 20 ans","Moins de 20 ans","Plus de 30 ans","Plus de 30 ans","Entre 20 et 30 ans","Entre 20 et 30 ans","Plus de 30 ans","Plus de 30 ans")
Base_FILTRE <- cbind(ID,NB_SMS,AGE_TR)
names(Base_FILTRE) <- c("ID","NB_SMS","AGE_TR")


#Conserver les lignes des clients de Moins de 20 ans avec au moins 500 SMS
filter(Base_FILTRE, 500 <= NB_SMS, AGE_TR == "Moins de 20 ans")

#Supprimer la colonne AGE_TR
select(Base_FILTRE, -AGE_TR)



#   Recodage
#------------------------------------------------------------------------------#
#Création du dataframe d'exemple contenant une variable ID d'identifiant client, une variable DAT_NAIS de date de naissance, une variable NB_SMS de nombre de SMS et une variable AGE_TR d'age en tranche
ID <- as.data.frame(c("ID1","ID2","ID3","ID4","ID5","ID6","ID7","ID8","ID9","ID10"))
DAT_NAISS <- c("08/01/1980","01/02/1982","10/03/1985","20/04/1978","19/05/1983","23/06/1985","24/07/1981","02/12/1979","17/11/1984","07/10/1980")
NB_SMS <- as.data.frame(as.integer(c(500,200,400,500,100,200,250,200,100,50)))
AGE_TR <- c("Moins de 20 ans","Entre 20 et 30 ans","Moins de 20 ans","Moins de 20 ans","Plus de 30 ans","Plus de 30 ans","Entre 20 et 30 ans","Entre 20 et 30 ans","Plus de 30 ans","Plus de 30 ans")
Base_RECOD <- cbind(ID,DAT_NAISS,NB_SMS,AGE_TR)
names(Base_RECOD) <- c("ID","DAT_NAISS","NB_SMS","AGE_TR")

#Audit des données
summary(Base_RECOD)
str(Base_RECOD)

#Créer une variable DAT_NAIS2 qui transforme la variable DAT_NAIS en variable date
Base_RECOD$DAT_NAIS2 <- as.Date(as.character(Base_RECOD$DAT_NAIS),format="%Y/%m/%d")
str(Base_RECOD)


#Recodage d'une variable quantitative en qualitative
Base_RECOD <- mutate(Base_RECOD, NB_SMS_TR = case_when( NB_SMS < 200 ~ "<200",
                                                           200 <= NB_SMS & NB_SMS <400 ~ "200 - 400",
                                                           400 <= NB_SMS ~ ">= 400", 
                                                           TRUE ~ "Non renseignée"))

#Vérification de la construction
summarise(group_by(Base_RECOD,NB_SMS_TR), min = min(NB_SMS), max = max(NB_SMS))


#Recodage d'une variable qualitative en quantitative
Base_RECOD <- mutate(Base_RECOD, AGE_MOINS_20 = if_else(AGE_TR == "Moins de 20 ans", "1","0")
                     , AGE_20_30 = if_else(AGE_TR == "Entre 20 et 30 ans", "1","0")
                     , AGE_PLUS_30 = if_else(AGE_TR == "Plus de 30 ans", "1","0"))



#Vérification de la construction
table(Base_RECOD$AGE_TR,Base_RECOD$AGE_MOINS_20)
table(Base_RECOD$AGE_TR,Base_RECOD$AGE_20_30)
table(Base_RECOD$AGE_TR,Base_RECOD$AGE_PLUS_30)




#   Créer des nouveaux indicateurs
#------------------------------------------------------------------------------#
#Création du dataframe d'exemple contenant une variable ID d'identifiant client, une variable NB_SMS de nombre de SMS, une variable AGE_TR d'age en tranche
ID <- as.data.frame(c("ID1","ID1","ID1","ID1","ID2","ID2","ID2","ID2","ID3","ID3"))
JOUR <- as.data.frame(c("Lundi","Mardi","Vendredi","Samedi","Mardi","Jeudi","Vendredi","Samedi","Samedi","Dimanche"))
NB_SMS <- as.data.frame(as.integer(c(5,2,4,5,1,3,2,2,1,5)))
Base_CREA <- cbind(ID,JOUR,NB_SMS)
names(Base_CREA) <- c("ID","JOUR","NB_SMS")

#Création d'une nouvelle variable : FLAG_SEMAINE contenant les modalités SEMAINE et WEEK END selon le jour d'envoi du SMS
Base_CREA <- mutate(Base_CREA, FLAG_SEMAINE = if_else(JOUR %in% c("Samedi","Dimanche"), "WEEK END","SEMAINE"))

#Agrégation : Nombre moyen de SMS par ID et FLAG_SEMAINE
Base_CREA_AGG <- summarise(group_by(Base_CREA, ID,FLAG_SEMAINE), SMS_MOYEN = mean(NB_SMS, na.rm = T))

#Pivot : Donnée pivotée pour avoir une ligne par client
Base_CREA_PIV <- Base_CREA_AGG %>% spread(FLAG_SEMAINE, SMS_MOYEN)


###############################################################################
#################### Statistiques univariées
###############################################################################

#   Analyse statistique d'une variable qualitative
#------------------------------------------------------------------------------#

#Création du dataframe d'exemple contenant une variable ID d'identifiant client et une variable CIV de civilité
ID <- c("ID1","ID2","ID3","ID4","ID5","ID6","ID7","ID8","ID9","ID10")
CIV <- c("Mr","Mme","Mme","Mr","Mr","Mr","Mme","Mme","Mme","Mme")
Base <- cbind(ID,CIV)
Base <- as.data.frame(Base)


#Analyse de la variable CIV
#Nombre de modalité
nlevels(Base$CIV)

#Labels
levels(Base$CIV)

#Répartition en effectif
table(Base$CIV)

#Répartition en pourcentage
prop.table(table(Base$CIV))

#Représentation graphique
#Représentation batons
ggplot(Base) + aes(x = CIV,fill = factor(CIV)) + geom_bar()

#Représentation camembert
ggplot(Base, aes(x = factor(1), fill = factor(CIV))) + geom_bar(width = 1)  + coord_polar(theta = "y") 


#   Analyse statistique d'une variable quantitative
#------------------------------------------------------------------------------#

#Création du dataframe d'exemple contenant une variable ID d'identifiant client et une variable AGE d'age
ID <- c("ID1","ID2","ID3","ID4","ID5","ID6","ID7","ID8","ID9","ID10")
AGE <- as.integer(as.character(c(20,30,40,25,35,50,20,40,40,50)))
Base_2 <- cbind(ID,AGE)
Base_2 <- as.data.frame(Base_2)
Base_2$AGE <- as.integer(as.character(Base_2$AGE))


#Analyse de la variable AGE
#Résumé des principaux indicateurs
summary(Base_2$AGE)

#Moyenne
mean(Base_2$AGE,na.rm = TRUE)

#Médiane
median(Base_2$AGE,na.rm = TRUE)


#Ecart type
sd(Base_2$AGE,na.rm = TRUE)

#Quantiles : exemple pour les quartiles
alpha <- c(1/4,1/2,3/4)
q <- quantile(Base_2$AGE,alpha)
q

#Représentation graphique
#Histogramme
ggplot(Base_2, aes(x=AGE)) + geom_histogram(binwidth=1,fill="pink")

#Boîte à moustaches :
ggplot(Base_2) + aes(x = "Boite à moustache AGE", y = AGE) + geom_boxplot(fill = "gold") 

#Recodage en variable qualitative
#Discrétiser en 2 tranches [min(AGE), mean(AGE)] et [mean(AGE), max(AGE)] 
breaks <- c(min(Base_2$AGE),mean(Base_2$AGE),max(Base_2$AGE))
Base_2$AGE_TRANCHE <- cut(Base_2$AGE, breaks,include.lowest = TRUE) 


###############################################################################
#################### Statistiques bivariées
###############################################################################

#   Analyse croisée de deux variables qualitatives
#------------------------------------------------------------------------------#
#Création du dataframe d'exemple contenant une variable ID d'identifiant client, une variable AGE d'age en tranches et une variable ANC d'ancienneté en tranche
ID <- c("ID1","ID2","ID3","ID4","ID5","ID6","ID7","ID8","ID9","ID10")
AGE_TR <- c("Moins de 20 ans","Entre 20 et 30 ans","Moins de 20 ans","Moins de 20 ans","Plus de 30 ans","Plus de 30 ans","Entre 20 et 30 ans","Entre 20 et 30 ans","Plus de 30 ans","Plus de 30 ans")
ANC_TR <- c("Ancien","Ancien","Ancien","Ancien","Ancien","Récent","Ancien","Récent","Ancien","Récent")
Base_QUALI <- cbind(ID,AGE_TR,ANC_TR)
Base_QUALI <- as.data.frame(Base_QUALI)

#Tableau de contingence en effectifs
table(Base_QUALI$ANC_TR, Base_QUALI$AGE_TR) 

#Tableau de contingence en pourcentage
prop.table(table(Base_QUALI$ANC_TR, Base_QUALI$AGE_TR))

#Tableau de contingence pourcentage en ligne
prop.table(table(Base_QUALI$ANC_TR, Base_QUALI$AGE_TR),1)

#Test d'indépendance du chi2
chisq.test(prop.table(table(Base_QUALI$ANC_TR, Base_QUALI$AGE_TR)))

#Représentation batons
rep_prof <- ggplot(Base_QUALI) + aes(x = ANC_TR,fill=AGE_TR) + geom_bar(position = "dodge")

#Profil moyen
Base_QUALI$TOTAL <- "TOTAL"
rep_tot <- ggplot(Base_QUALI) + aes(x = TOTAL,fill=AGE_TR) + geom_bar(position = "dodge")
plot_grid(rep_prof, rep_tot, ncol = 2, nrow = 1)


#   Analyse croisée de deux variables quantitatives
#------------------------------------------------------------------------------#
#Création du dataframe d'exemple contenant une variable ID d'identifiant client, une variable NB_SMS de nombre de SMS et une variable NB_MIN du nombre de minutes d'appel
ID <- as.data.frame(c("ID1","ID2","ID3","ID4","ID5","ID6","ID7","ID8","ID9","ID10"))
NB_SMS <- as.data.frame(as.integer(c(50,60,40,40,120,100,80,100,80,50)))
NB_MIN <- as.data.frame(as.integer(c(25,30,20,15,70,80,50,50,30,20)))
Base_QUANTI <- cbind(ID,NB_SMS,NB_MIN)
names(Base_QUANTI) <- c("ID","NB_SMS","NB_MIN")


#Coefficient de corrélation
cor.test(Base_QUANTI$NB_SMS,Base_QUANTI$NB_MIN,method=c("pearson"))

#Nuage de points
Nuage <- ggplot(Base_QUANTI, aes(x = NB_SMS, y = NB_MIN))+ geom_point(size = 2,col="blue") + xlim(0,150) + ylim(0,100)

#Droite de régression linéaire
Nuage + geom_smooth(method = lm)


#   Analyse croisée d'une variable qualitative et quantitative
#------------------------------------------------------------------------------#
#Création du dataframe d'exemple contenant une variable ID d'identifiant client, une variable NB_SMS de nombre de SMS et une variable AGE_TR d'age en tranche
ID <- as.data.frame(c("ID1","ID2","ID3","ID4","ID5","ID6","ID7","ID8","ID9","ID10"))
NB_SMS <- as.data.frame(as.integer(c(500,200,400,500,100,150,250,200,100,50)))
AGE_TR <- c("Moins de 20 ans","Entre 20 et 30 ans","Moins de 20 ans","Moins de 20 ans","Plus de 30 ans","Plus de 30 ans","Entre 20 et 30 ans","Entre 20 et 30 ans","Plus de 30 ans","Plus de 30 ans")
Base_QUALI_QUANTI <- cbind(ID,NB_SMS,AGE_TR)
names(Base_QUALI_QUANTI) <- c("ID","NB_SMS","AGE_TR")
str(Base_QUALI_QUANTI)

#Nombre moyen de SMS par Tranche d'Age
NBSMS_AGE <- as.data.frame(summarise(group_by(Base_QUALI_QUANTI, AGE_TR), SMS_MOYEN = mean(NB_SMS, na.rm = T)))

#Nombre moyen de SMS au global
Base_QUALI_QUANTI$Total <- "Total"
NBSMS_TOT <- as.data.frame(summarise(group_by(Base_QUALI_QUANTI, Total), SMS_MOYEN = mean(NB_SMS, na.rm = T)))
names(NBSMS_TOT) <- c("AGE_TR","SMS_MOYEN")

#Regroupement des informations
rbind(NBSMS_AGE,NBSMS_TOT)

#Test de Student
t.test(Base_QUALI_QUANTI[Base_QUALI_QUANTI$AGE_TR=="Moins de 20 ans",2], Base_QUALI_QUANTI[Base_QUALI_QUANTI$Total=="Total",2])

#Boite à moustache
ggplot(Base_QUALI_QUANTI) + aes(x = AGE_TR, y = NB_SMS) + geom_boxplot(fill = "gold") 



###############################################################################
#################### Construire un modèle de scoring
###############################################################################

#   Base apprentissage / validation
#------------------------------------------------------------------------------#
#Import des données
don <- read.csv2(file = "C:/Users/cpe/Desktop/Data SFR Formation/adult_m.csv",dec=".",sep=";")
don <- don[,-c(4)]
str(don)

#On retravaille la variable income => si income == ">50K" alors inc = 1 sinon inc = 0
don <- mutate(don, inc = if_else(income == ">50K", 1,0)) 
don$income <- NULL
don$inc <- as.factor(don$inc)

#Taux de clients dans l'échantillon et graine pour l'aléatoire :
rateEch <- 0.75; set.seed(2016) 

#Extraction des lignes avec Y = 1 et construction de deux dataframes => un contenant les clients avec Y = 1 et l'autre avec les clients Y = 0 :
ligneP <- which(don$inc == 1)
P <- don[ ligneP , ]
N <- don[ - ligneP , ]

#Extraction de 75% du dataframe P et de 75% du dataframe N:
indPTrain <- sample( 1:dim(P)[ 1 ], size = round( length(P$inc) * rateEch ), replace = FALSE)
indNTrain <- sample( 1:dim(N)[ 1 ], size = round( length( N$inc) * rateEch ), replace = FALSE)

#Construction des bases train et test :
train <- rbind(P[ indPTrain , ], N[ indNTrain , ] )           #construction de train en prenant indPTrain et indNTrain (75%)
test <- rbind( P[ - indPTrain , ], N[ - indNTrain , ] )       #construction de test en ne prenant pas indPTrain et indNTrain (25%)

#Vérification du taux de cible sur don, train et test :
table(don$inc); prop.table(table(don$inc))
table(train$inc); prop.table(table(train$inc))
table(test$inc); prop.table(table(test$inc))



#   Discrétiser les variables
#------------------------------------------------------------------------------#

#Extraction des variables quantitatives utilisées : age et educational.num
#Extraction d'un sous dataframe ne contenant que ces variables
train_SD <- train[,c("age","educational.num","ID")]


#Découpage en tranches des variables à partir du minimum, de la valeur à 33%, de la valeur à 66%, du maximum
DISCRETISE_DF <- function(DF) {
  nbvar <- dim(DF)[2]-1                                               #Nombre de variables pour boucler sur toutes les variables
  alpha1 <- c(1/3,2/3)                                                 #Bornes de découpage
  for (i in 1:nbvar) {
    nom <- paste(colnames(DF)[i],"_D",sep="")               #Nom de la nouvelle variable à partir du nom de la variable initiale
    nom_var <- colnames(DF)                                        #Nom des colonnes du data frame
    q <- quantile(DF[,i],alpha1)                                     #Recherche des valeurs associées aux bornes
    breaks <- c(min(DF[,i]),q[1],q[2],max(DF[,i]))          #Borne de découpage
    var_ <- cut(DF[,i], unique(breaks),include.lowest = TRUE)    #Découpage de la variable
    DF <- cbind(DF,var_)                                               #Ajout de la nouvelle variable dans le data frame 
    names(DF) <- c(nom_var,nom)                                #Renommer la nouvelle variable
  }
  return(DF)
}
train_SD2 <- DISCRETISE_DF(DF= train_SD)

#Vérification
summarise(group_by(train_SD2,age_D), min = min(age), max = max(age))

#Extraction dans le data frame initial des variables qualitatives
ind.quali <- sapply(train, function(x) is.factor(x))
Data.quali <- train[ ,ind.quali]
var_quali <- colnames(Data.quali)
train_QUALI <- train[,c(var_quali,"ID")]


#Regrouper les données qualitatives initiales et les qualitatives créées
train <- merge(train_QUALI,train_SD2[,dim(train_SD)[2] : dim(train_SD2)[2]],by="ID")

#Suppression de la variable native.country qui a trop de modalités
train$native.country <- NULL



#   Disjoncter les variables
#------------------------------------------------------------------------------#
#Création d'un sous dataframe ne contenant pas la variable cible qui ne doit pas être disjonctée
train_SS_VC <- train[,-c(9)]

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


#Suppression des variables initiales du dataframe pour ne conserver que les nouvelles variables créées
train_ <- merge(train[,c("ID","inc")],train_DIJ[, c(1,(dim(train)[2]): dim(train_DIJ)[2])],by="ID")

#On conserve le nom des variables initiales dans un vecteur => utilisé au moment du khi deux
#Noms des variables remis dans le même ordre que le data frame train_
vecteur_nom <- c("ID","inc","workclass","education","marital.status","occupation","relationship","race","gender","age_D","educational.num_D")



#   Suppression de la modalité de référence
#------------------------------------------------------------------------------#
#Créer une fonction permettant de supprimer la modalité avec le plus petit khi deux par rapport à la variable cible
KHI_DF <- function(DF) {
  nbvar <- length(vecteur_nom)    #Nombre de variables initiales pour boucler sur toutes les variables
  vectnom <- c(rep("",each=nbvar-2))  #Création d'un vecteur dans lequel les variables à supprimer vont être stockées
  for (i in 3:nbvar) {
    nomvar <- colnames(DF)  #Nom des variables du data frame
    #Recherche des variables disjonctées liées à chaque variable initiale
    lignes <- which(substring(nomvar,1,nchar(vecteur_nom[i]))==vecteur_nom[i])
    SOUS_DF <- DF[,c(2,lignes)]  #Extraction de ces variables
    vect <- c(rep(0,each=dim(SOUS_DF)[2]-1))   #Création d'un vecteur qui va contenir les valeurs des Khi 2
    for (j in 2:dim(SOUS_DF)[2]) {
      contingence <- table(a = SOUS_DF[,1], b = SOUS_DF[,j])  #Tableau de contingence
      res <- chisq.test(contingence)   #Calcul du Khi 2
      vect[j-1] <- res$statistic    #Récupération du Khi 2 et stockage dans le vecteur
    }
    min_khi <- which.min(vect)    #Recherche de la valeur min du Khi 2
    nomsuprr <- colnames(SOUS_DF)[min_khi+1]   #Recherche du nom de cette variable
    vectnom[i-2] <- nomsuprr   #Nom de la variable stocké dans le vecteur
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
#Créer une fonction qui permet de calculer les corrélations entre toutes les variables
#Pour supprimer les variables les plus corrélées
COR_DF <- function(DF) {
  SOUS_DF <- DF[,-c(1,2)]    #Data Frame sans ID et variable cible
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
write.csv2(matrice_cor,file="C:/Users/cpe/Desktop/Matrice_corr.csv" , row.names = TRUE)

#Suppression des variables trop corrélées
train <- subset(train_KHI2,select=-c(educational.num_D_.1.9.,occupation_.,relationship_Husband))



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

#Analyse des rapports (lifts)
train_SELECT


#MODELISATION
#Suppression des variables avec un lift entre 0,8 et 1,2 => variables avec peu de lien avec la variable cible
train <- subset(train,select=-c(workclass_Federal.gov,workclass_Local.gov,workclass_Never.worked,workclass_Private,workclass_Self.emp.not.inc,workclass_State.gov,education_Assoc.voc,education_Bachelors
                                ,education_HS.grad,education_Some.college,marital.status_Married.civ.spouse,occupation_Adm.clerical,occupation_Craft.repair,occupation_Farming.fishing
                                ,occupation_Machine.op.inspct,occupation_Prof.specialty,occupation_Protective.serv,occupation_Sales 
                                ,occupation_Tech.support,occupation_Transport.moving,relationship_Wife,race_Black,race_Other,race_White,gender_Male,age_D_.44.90.,educational.num_D_.10.16.))

#Liste des variables explicatives
X <- train[,3:dim(train)[2]]

#Passage de la variable cible en numérique
train$inc <- as.numeric(as.character(train$inc))

#Modèle Grossier 
reg0<-glm(train$inc ~1, family = "binomial")

#Modèle Fin
reg1 <- glm(train$inc ~ X[,1]+X[,2]+X[,3]+X[,4]+X[,5]+X[,6]+X[,7]+X[,8]+X[,9]+X[,10]+X[,11]+X[,12]+X[,13]+X[,14]+X[,15]+X[,16]+X[,17]+X[,18]+X[,19]+X[,20]+X[,21]+X[,22]+X[,23]+X[,24]+X[,25]+X[,26]+X[,27], family = "binomial", control = list(maxit = 1000))

#Détail sur le modèle
summary(reg1)

#Choix d'un modèle que l'on considère comme le meilleur (référence)
myscope<-list(upper =  ~ X[,1]+X[,2]+X[,3]+X[,4]+X[,5]+X[,6]+X[,7]+X[,8]+X[,9]+X[,10]+X[,11]+X[,12]+X[,13]+X[,14]+X[,15]+X[,16]+X[,17]+X[,18]+X[,19]+X[,20]+X[,21]+X[,22]+X[,23]+X[,24]+X[,25]+X[,26]+X[,27], lower = ~ 1) 

#Méthode ascendante : A chaque étape on teste l'ajout d'une nouvelle variable : toutes les variables sont testées en utilisant AIC
reg.AICf <- stepAIC(reg0,direction=c("forward"), scope=myscope) 
summary(reg.AICf)
# Pour obtenir les probabilités pour chaque individu :
reg.AICf$fitted.values

#Méthode descendante : A chaque étape on teste la suppression d'une variable : toutes les variables sont testées en utilisant AIC
reg.AICb <- stepAIC(reg1,direction=c("backward"),scope=myscope)
summary(reg.AICb) 

#Méthode «both» : A chaque étape on teste l'ajout d'une variable : toutes les variables sont testées en utilisant AIC, on la supprime si le modèle est dégradé
reg.AICboth <- stepAIC(reg0,direction=c("both"),scope=myscope)
summary(reg.AICboth)


#REAPPLICATION
#Modèle final avec uniquement les variables choisies
#X2 <- train[,c("ldl_D_.1.38.2.73.","tobacco_D_.2.5.7.5.","adiposity_D_.18.8.27.")]
MODELE_F <- glm(train$inc ~ X[,1]+X[,2]+X[,3]+X[,4]+X[,5]+X[,6]+X[,7]+X[,8]+X[,9]+X[,10]+X[,11]+X[,12]+X[,13]+X[,14]+X[,15]+X[,16]+X[,17]+X[,18]+X[,19]+X[,20]+X[,21]+X[,22]+X[,23]+X[,24]+X[,25]+X[,26]+X[,27], family = "binomial", control = list(maxit = 1000))
summary(MODELE_F)

#Pour identifier les bornes de chaque variable utilisée dans la modélisation
summarise(group_by(train_SD2,age_D), min = min(age), max = max(age))

#Construction des variables disjonctées sur le jeu de données test
test <- mutate(test, workclass_.  = if_else((workclass == "?"), 1,0)
                    , workclass_Self.emp.inc  = if_else((workclass == "Self-emp-inc"), 1,0)
                    , education_10th  = if_else((education == "10th"), 1,0)
                    , education_11th  = if_else((education == "11th"), 1,0)
                , education_12th  = if_else((education == "12th"), 1,0)
                , education_1st.4th = if_else((education == "1st-4th"), 1,0)
                , education_5th.6th = if_else((education == "5th-6th"), 1,0)
                , education_7th.8th = if_else((education == "7th-8th"), 1,0)
                , education_9th = if_else((education == "9th"), 1,0)
                , education_Doctorate  = if_else((education == "Doctorate"), 1,0)
                , education_Masters  = if_else((education == "Masters"), 1,0)
                , education_Preschool  = if_else((education == "Preschool"), 1,0)
                , education_Prof.school   = if_else((education == "Prof-school"), 1,0)
                , marital.status_Divorced    = if_else((marital.status == "Divorced"), 1,0)
                , marital.status_Married.spouse.absent    = if_else((marital.status == "Married-spouse-absent"), 1,0)
                , marital.status_Never.married    = if_else((marital.status == "Never-married"), 1,0)
                , marital.status_Separated    = if_else((marital.status == "Separated"), 1,0)
                , marital.status_Widowed    = if_else((marital.status == "Widowed"), 1,0)
                , occupation_Exec.managerial     = if_else((occupation == "Exec-managerial"), 1,0)
                , occupation_Handlers.cleaners     = if_else((occupation == "Handlers-cleaners"), 1,0)
                , occupation_Other.service     = if_else((occupation == "Other-service"), 1,0)
                , occupation_Priv.house.serv     = if_else((occupation == "Priv-house-serv"), 1,0)
                , relationship_Not.in.family     = if_else((relationship == "Not-in-family"), 1,0)
                , relationship_Own.child      = if_else((relationship == "Own-child"), 1,0)
                , relationship_Unmarried      = if_else((relationship == "Unmarried"), 1,0)
                , race_Amer.Indian.Eskimo       = if_else((race == "Amer-Indian-Eskimo"), 1,0)
                , age_D_.17.31. = if_else((17 <= age & age <= 31), 1,0))


#Vérification de la construction
summarise(group_by(test,age_D_.17.31.), min = min(age), max = max(age))
table(test$race_Amer.Indian.Eskimo,test$race)


#Calcul du score sur les données test
X <- test[,16:dim(test)[2]]
res <- predict(MODELE_F,type="response", newdata = data.frame(x=X))

#Ajout de la variable de prédiction dans le jeu de données test
test$scoreREG = res

#Affectation d'une classe : à partir du taux de cible global (1,2*taux de cible habituellement)
test$scoreREG_classe <- 0
test$scoreREG_classe[test$scoreREG>0.3] <- 1

#Calcul taux d'erreur
table(test = test$scoreREG_classe,vrai = test$inc)


#   Calcul des déciles et synthèse
#------------------------------------------------------------------------------#
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
deciles_score <- calcul_deciles(test$ID, test$scoreREG)

#Ajout de la variable cible pour calculer les indicateurs par décile
deciles_score <- left_join(deciles_score, test[, c("ID", "inc")], by = c("ID_INDIV" = "ID"))

#Variable cible passée en numérique pour les comptages
deciles_score$inc <- as.numeric(as.character(deciles_score$inc))

#Synthèse des résultats
#Comptage du nombre d'individus par décile
res_score <- aggregate(ID_INDIV ~ decile, data = deciles_score, length)

#Comptage du nombre d'individus qui valent "1" pour "inc" par classe
res_score <- left_join(res_score, aggregate(inc ~ decile, data = deciles_score, sum), by = "decile") 
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

#Export de la synthèse en CSV
write.csv2(res_score,file=" Synthèse des déciles.csv ")


#   Courbe de Lift
#------------------------------------------------------------------------------#
#Choix de la variable de score
score <- test$scoreREG

#Utilisation de fonctions pré programmées
#Chemin du fichier contenant les fonctions
dir_fonction <- "C:/Users/cpe/Desktop/Data SFR Formation/fonctionsFormation.R" 
source(dir_fonction)
nbre_point <- 50
ideal <- courbe_ideale(test$inc,1,nbre_point)   	#Courbe idéale
hasard <- courbe_hasard(nbre_point)			#Courbe hasard
notrelift <- courbe_lift(test$inc,score,nbre_point) 	#Courbe Lift
plot(hasard,ideal,type='l',col='red') 
lines(hasard,notrelift,type='l',col='blue') 
lines(hasard,hasard,type='l')


#   CART
#------------------------------------------------------------------------------#
#Il est possible d'utiliser les échantillons train et test avec variables disjonctées 
#ou de reconstruire des échantillons avec les variables initiales
#Construction d'échantillons avec variables initiales
#Extraction des lignes avec Y = 1 et construction de deux dataframes => un contenant les clients avec Y = 1 et l'autre avec les clients Y = 0 :
ligneP <- which(don$inc == 1)
P <- don[ ligneP , ]
N <- don[ - ligneP , ]

#Extraction de 75% du dataframe P et de 75% du dataframe N:
indPTrain <- sample( 1:dim(P)[ 1 ], size = round( length(P$inc) * rateEch ), replace = FALSE)
indNTrain <- sample( 1:dim(N)[ 1 ], size = round( length( N$inc) * rateEch ), replace = FALSE)

#Construction des bases train et test :
train <- rbind(P[ indPTrain , ], N[ indNTrain , ] )           #construction de train en prenant indPTrain et indNTrain (75%)
test <- rbind( P[ - indPTrain , ], N[ - indNTrain , ] )       #construction de test en ne prenant pas indPTrain et indNTrain (25%)

#Vérification du taux de cible sur don, train et test :
table(don$inc); prop.table(table(don$inc))
table(train$inc); prop.table(table(train$inc))
table(test$inc); prop.table(table(test$inc))

#Passage de la variable inc en numérique
train$inc <- as.numeric(as.character(train$inc))


#Echantillon équilibré : 50/50
train_1 <- train[train$inc==1,]  #Extraction des lignes avec inc = 1
train_0_ <- train[train$inc==0,]  #Extraction des lignes avec inc = 0
train_0_lignes <- sample(1:dim(train_0_)[ 1 ], size = dim(train_1)[1], replace = FALSE)   #Extraction des lignes avec inc = 0 qui vont permettre d'avoir 50% / 50%
train_0 <- train_0_[train_0_lignes,] #Extraction des lignes avec inc = 0
train_CART <- rbind(train_1,train_0)  #Regroupement des échantillons avec inc = 1 et inc = 0

#Vérification
table(train_CART$inc)

#Modélisation
#Entrainement du modèle avec les hypers paramètres de base
arbreFit0 <- rpart(train_CART$inc ~ . , train_CART[,2:(dim(train_CART)[2]-1) ], method = "class", 
                   control = rpart.control(minsplit=1,cp=0))
plotcp(arbreFit0)			#Choix de la complexité de l'arbre
arbreFit1 <- prune(arbreFit0 , cp = 0.0001)	#Arbre découpé

#Visualisation de l'arbre
plot(arbreFit1, uniform=TRUE,branch=0.5,margin=0.1)
text(arbreFit1, all = FALSE, use.n = TRUE)

#Réapplication
#Calcul du score sur les données test et erreur de prédiction
#Prévision sous forme de probabilité
test$scoreTREE <- predict(arbreFit1, test[,2:(dim(train_CART)[2]-1) ] ,type="prob")
#Prévision sous forme de classe
test$scoreTREEC <- predict(arbreFit1, test[,2:(dim(train_CART)[2]-1) ] ,type="class")
#Erreur de prédiction
table(test = test$scoreTREEC,vrai = test$inc)   


###############################################################################
#################### Construire une segmentation
###############################################################################

#Import des données
don <- read.csv2(file = "C:/Users/cpe/Desktop/Data SFR Formation/adult_m.csv",dec=".",sep=";")
don <- don[,-c(4)]
str(don)


#   Réduction de dimension - ACP
#------------------------------------------------------------------------------#
#Extraction de toutes les variables quantitatives excepté l'ID client
ind.quant <- sapply(don, function(x) is.numeric(x) | is.integer(x))
Data.quant <- don[ ,ind.quant]
var.keep2 <- colnames(Data.quant)[-1]

#ACP
pca <- PCA( don[,var.keep2] , scale.uni=TRUE , graph=FALSE )

#Visualiser les éléments que contient l'ACP
pca

#Critère du coude
plot(pca$eig[,1],type="l")

#Critère de Kaiser
pca$eig

#Contribution des variables pour interprétation des axes
pca$var$contrib
pca$var$coord


#   Réduction de dimension - ACM
#------------------------------------------------------------------------------#
#Sélection des variables sur lesquelles faire l'ACM
Data.quali <- don[,c("relationship","race","gender","income")]

#ACM
acm<-MCA( Data.quali , method="Burt" ,graph=FALSE)

#Visualiser les éléments que contient l'ACM
acm

#Critère du coude
plot(acm$eig[,1],type="l")

#Critère de Kaiser
acm$eig

#Contribution des variables pour interprétation des axes et coordonnées pour le sens de la contribution
acm$var$contrib
acm$var$coord


#   Modélisation - Kmeans
#------------------------------------------------------------------------------#
#Dataframe contenant les composantes principales de l'ACP et de l'ACM
don_seg <- cbind(as.data.frame(pca$ind$coord[,1:2]),as.data.frame(acm$ind$coord[,1:5]))
names(don_seg) <- c("PCA1","PCA2","MCA1","MCA2","MCA3","MCA4","MCA5")

#Premier test de kmeans : On choisit de créer un modèle à partir de 3 centres et sur 10 itérations
set.seed(2016) 	#(initialisation de la graine)
SEGkmeans <- kmeans( don_seg , centers = 3, nstart = 10)

#Visualiser les éléments que contient la kmeans
summary(SEGkmeans)

#Visualiser le cluster qui est assigné aux 10 premières observations
SEGkmeans$cluster[1:10]

#Pour optimiser le nombre de clusters, on fait tourner l'algorithme des kmeans pour plusieurs tailles de classification et on les compare au sens de la statistique de Calinski-Harabasz (un score élevé indique des clusters bien séparés).
#Construction de 3 modèles de Kmeans en faisant varier le nombre de centres entre 2 et 4
plusieursSEG <- cascadeKM(don_seg, inf.gr=2, sup.gr=4, iter=100, criterion=c("calinski"))
plot(plusieursSEG)

#Tri des données en sortie pour une meilleure lecture
nom.var <- colnames(plusieursSEG$partition)
plusieursSEG$partition <- plusieursSEG$partition[order(plusieursSEG$partition[,nom.var[1]],
                                                       plusieursSEG$partition[,nom.var[2]], 						   plusieursSEG$partition[,nom.var[3]]),]
plot(plusieursSEG)

#Score de Calinski
plusieursSEG$results

#Choix de 4 clusters et on sauvegarde les résultats dans la matrice cluster
SEGkmeans2 <- kmeans( don_seg , centers = 4, nstart = 10)
cluster <- cbind(as.data.frame(don$ID),as.factor(SEGkmeans2$cluster))
cluster <- as.data.frame(cluster)
names(cluster) = c("ID","Clust_Kmeans")

#Construire une représentation des points dans le plan principal des variables de l'acp
var.keep2 <- c("PCA1","PCA2")
clusplot(don_seg[,var.keep2], cluster$Clust_Kmeans, color=TRUE, shade=TRUE, labels=2, lines=0)
plotcluster(don_seg[,c("PCA1","PCA2")], cluster$Clust_Kmeans)


#   Modélisation - CAH
#------------------------------------------------------------------------------#
#Sélection des 5K premiers clients => trop de clients dans le dataframe pour une CAH
don_seg_ <- don_seg[c(1:5000),]

#Calcul des distances par la méthode euclidienne
D <- dist(don_seg_, method=c("euclidean"))

#Construction du dendrogramme
dendro <- hclust( D ,method = "ward.D" )
plot(dendro)

#Réalisation du clustering
#Choix du découpage en 4 segments
SEGcha <- cutree(dendro,4) 

#Pour visualiser les clusters associés aux 10 premiers clients
SEGcha[1:10]

#Création de la matrice SEG.cha qui contient les clusters
SEG.cha <- as.data.frame(as.factor(SEGcha))
names(SEG.cha) <- c("Clust_CHA")

#Représentation
#Construire une représentation des points dans le plan principal des variables acp
var.keep2 <- c("PCA1","PCA2")
clusplot(don_seg_[,var.keep2], SEG.cha$Clust_CHA, color=TRUE, shade=TRUE, labels=2, lines=0)
plotcluster(don_seg_[, c("PCA1","PCA2")], SEG.cha$Clust_CHA)


#   Modélisation - Classification Mixte
#------------------------------------------------------------------------------#
#Première étape : Kmeans sur 100 centroides
centroides <- kmeans( don_seg , centers = 100, iter.max=20 )

#Deuxième étape : CAH et dendrogramme sur les centroides de le Kmeans
D2 <- dist(centroides$centers, method=c("euclidean")) 
dendro <- hclust( D2 ,method = "ward.D" )
plot(dendro)

#Découpage en 4 classes et on créé la correspondance entre cluster Kmeans et cluster CHA
CLcha <- as.data.frame(cutree(dendro,4))
names(CLcha) <- "clusterCHA"
CLcha$clusterKM <- as.numeric(rownames(CLcha)) 

#A partir de la table en sortie de la Kmeans on affecte à chaque ID son cluster de CHA qui correspond à la segmentation mixte et on récupère l'ID
CLkmeans <- as.data.frame(centroides$cluster)
names(CLkmeans) <- "clusterKM" 
CLkmeans <- cbind(don$ID, CLkmeans)
CLkmeans <- as.data.frame(CLkmeans)
CLkmeans$numligne <- as.numeric(rownames(CLkmeans))
Mixte <- merge(CLcha,CLkmeans,by = "clusterKM")

#On ne conserve que le cluster de la Mixte
Mixte2 <- Mixte[,c("clusterCHA","don$ID")]
names(Mixte2) <- c("Clust_Mixte","ID")

#On réunit les résultats de la classification Mixte avec les autres dans la table cluster
cluster <- merge(cluster, Mixte2,by="ID")

#Représentation
#On peut construire une représentation des points dans le plan principal des variables ACP
var.keep2 <- c("PCA1","PCA2")
clusplot(don_seg[,var.keep2], cluster$Clust_Mixte, color=TRUE, shade=TRUE, labels=2, lines=0)
plotcluster(don_seg[, c("PCA1","PCA2")], cluster$Clust_Mixte)


#Comparaison de la Kmeans et la Classification Mixte
table(cha= cluster$Clust_Kmeans,kmeans= cluster$Clust_Mixte)


#   Test de la robustesse du modèle
#------------------------------------------------------------------------------#
#L'affectation de nouveaux client se fera en utilisant la segmentation Kmeans

#Pour l'exemple, nous utiliserons les deux premières lignes du dataframe initial que nous considérerons comme étant des nouveaux clients
don_NEW <- don[1:2,]

#Coordonnées des nouveaux clients sur l'ACP
#Rappel : Les données des nouveaux clients doivent contenir les même variables (mêmes noms et dans le même ordre) que celles utilisées pour construire l'ACP et /ou l'ACM.
#Extraction des données quantitatives dans un nouveau dataframe
ind.quant <- sapply(don, function(x) is.numeric(x) | is.integer(x))
Data.quant <- don[ ,ind.quant]
var.keep2 <- colnames(Data.quant)[-1]
don_NEW_quanti <- don_NEW[,var.keep2]

#Projection des nouveaux individus dans l'ACP construite
don_NEW_ACP <- predict(pca, newdata = don_NEW_quanti)

#Extraction des coordonnées sur les deux premières composantes principales qui avaient été retenues
don_NEW_ACP <- don_NEW_ACP$coord[,1:2]


#Coordonnées des nouveaux clients sur l'ACM
#Rappel : Les données des nouveaux clients doivent contenir les même variables (mêmes noms et dans le même ordre) que celles utilisées pour construire l'ACP et /ou l'ACM.
#Extraction des données qualitatives dans un nouveau dataframe
don_NEW_quali <- don_NEW[,c("relationship","race","gender","income")]

#Projection des nouveaux individus dans l'ACM construite
don_NEW_ACM <- predict(acm, newdata = don_NEW_quali)

#Extraction des coordonnées sur les cinq premières composantes principales qui avaient été retenues
don_NEW_ACM <- don_NEW_ACM$coord[,1:5]


#Affectation d'un nouveau segment par distance euclidienne
#Regroupement des données ACP et ACM
don_NEW_ACP_ACM <- cbind(don_NEW[,1],don_NEW_ACP,don_NEW_ACM)
don_NEW_ACP_ACM <- as.data.frame(don_NEW_ACP_ACM)
names(don_NEW_ACP_ACM) <- c("ID","PCA1","PCA2","MCA1","MCA2","MCA3","MCA4","MCA5")


#Récupération des barycentres de la segmentation
SEGkmeans2$centers

#Distance euclidienne entre les clients et les barycentres de la segmentation
#Fonction qui calcule la distance euclidienne
maDist <- function(x, y) {
  d = sqrt(sum((x - y)^2))
  return(d)
}

#Distance par rapport à chaque barycentre
df.dist = data.frame()
for (i in 1:dim(SEGkmeans2$centers)[1]){
for (k in 1:nrow(don_NEW_ACP_ACM)) {
  df.dist[k,i] <- maDist(don_NEW_ACP_ACM[k, -c(1)], SEGkmeans2$centers[i,])
}
}

#Recherche pour chaque client du cluster affecté => celui avec la distance minimum
df.dist$SEG<- apply(df.dist,1,which.min)


#   Portraits Robot
#------------------------------------------------------------------------------#
#Ajout de la segmentation à la table initiale
don_PR <- left_join(don,cluster[,c(1:2)],by="ID")

#Extraction de toutes les variables quantitatives excepté l'ID client
ind.quant <- sapply(don_PR, function(x) is.numeric(x) | is.integer(x))
Data.quant <- don_PR[ ,ind.quant]
var.keep_quanti <- colnames(Data.quant)[-1]

#Variables quantitatives
#Calculer la moyenne des variables par cluster
Profil_quanti <- aggregate(don_PR[,var.keep_quanti], by=list(seg= don_PR$Clust_Kmeans),mean)
Profil_quanti$seg <- as.character(Profil_quanti$seg)

#Moyenne générale
moyCLUST <- apply(don_PR[,c(var.keep_quanti)],2,mean)
moyCLUST <- c("Tous",moyCLUST)

#Regroupement de la donnée
CLUST_quanti <- rbind(Profil_quanti,moyCLUST)

#Variable qualitatives
#Fonction permettant de calculer les fréquences pour les variables qualitatives choisies
FREQ <- function(var_) {
  FREQ_ <- prop.table(table(don_PR[, var_ ], cluster$Clust_Kmeans),2)
  FREQ_ <- as.data.frame(FREQ_)
  FREQ_2 <- FREQ_ %>% spread(Var1, Freq)
  names(FREQ_2)[1] <- "CLUSTER"
  FREQ_2$CLUSTER <- as.factor(FREQ_2$CLUSTER)  
  #Fréquence au global
  FREQ_TOT <- prop.table(table(don_PR[, var_]))
  FREQ_TOT <- as.data.frame(FREQ_TOT)
  FREQ_TOT2 <- FREQ_TOT %>% spread(Var1, Freq)
  FREQ_TOT2 <- cbind("Tous",FREQ_TOT2)
  names(FREQ_TOT2)[1] <- "CLUSTER"
  #Concaténer la donnée
  FREQ_VAR <- rbind(FREQ_2,FREQ_TOT2)
  return(FREQ_VAR)
}
relationship_F <- FREQ(var_ = "relationship")
race_F <- FREQ(var_ = "race")
gender_F <- FREQ(var_ = "gender")
income_F <- FREQ(var_ = "income")




