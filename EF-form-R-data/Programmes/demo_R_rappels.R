setwd("C:\\Users\\dgr\\Documents\\Formations")
rm(list=ls())

# créer une matrice
mat<-matrix(c(1,3,4,5,6,7),ncol=2)
length(mat)
dim(mat)

# accéder
nblignes=dim(mat)[1]

# data frame
df<-data.frame(mat)
class(df)
str(df)
names(df)

# renommer
names(df)<-c('v1','v2')
names(df)

df[1,2]

# créer une col
df$v3<-df$v1**2
df$v4<-c("A","B","C")

# drop
drops <- c("v3","v4")
df[ , !(names(df) %in% drops)]
# drop par index
df[,-1]

# tri
df[order(df$v1, -df$v2),]

# filtrer des lignes
subset(df, v4 == "A")
df[which(df$v4 == "A"),]

# Séectionner des colonnes : 
subset(df, select = c(v1, v2))
# Supprimer des colonnesÂ : 
subset(df, select = - c(v1))
# sélection de colonnes
df[c("v2","v3")]
df[,c("v2","v3")]
df[1:2,c("v2","v3")]
# Retrait des doublons purs: 
unique(df)
#Retrait de doublons selon une sÃ©lection de variablesÂ : 
df[!duplicated(df[,c(1,2)]),]
# Exporter dans un vecteur le numéro des lignes respectant une conditionÂ : 
X <- which(df$v4 == "A")

# indexation
# quand on fait une selection, on remarque bien que les lignes gardent leurs index
df2<-df[2:3,c("v2","v3")]

# factor
df$v1<-as.factor(df$v1)


# na
df$v2[2]<-NA
is.na(df) # returns TRUE of x is missing
df$v1[df$v4=="A"] <- NA 
mean(df$v2) # returns NA
mean(df$v2, na.rm=TRUE) # returns mean

# vect numerique
list=c(1,3,4,5,6,7)

# liste 
list=list(c(1,3,4,5,6,7))

# statistiques
mean(df$v2)
mean(scale(df$v2))

# fonction APPLY
X<-matrix(sample(1:20,20),ncol=4)
apply(X,2,mean) # 2 appliqués aux colonnes
apply(X,2,scale) # 2 appliqués aux colonnes

# group by, la variable by doit etre en liste, les x doivent etre numériques
aggregate(df[,2:3],list(df$v4),sum)

# tris à plat
prop.table(table(df$v1))

# fonction
# Une fonction retourne tout simplement le rÃ©sultat de la derniÃ¨re expression du corps de la fonction. 
# Si un résultat doit être retourné sans être à la dernière ligne de la fonction, il est nécessaire d'utiliser la fonction return. 

Monexemple <- function(A,B){
  out<-(A+B)^2
  out<-out + A
  return(out)
  
}

Monexemple <- function(A,B){
  out<-(A+B)^2
  out<-out + A
  return(list(res=out,arg1=A))
  }

Monexemple(2,3) # Résultat : 27


set.seed(0)
rnorm(10)

set.seed(5)
rnorm(10)