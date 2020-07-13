####### FORMAT DATE EN LECTURE D'UN FICHIER
setwd("~/EF-form-R-data/EF-form-R-data")
# la fonction read_delim de tidyverse infère automatiquement
# si on utilise read csv il faut ensuite transformer
fichier<-read.csv("data/orders.txt",sep='|')
str(fichier)
# caster en date la date achat
fichier$Date_achat<-as.Date(as.character(fichier$order_date),format="%Y-%m-%d")

####### FORMAT AVEC LUBRIDATE
library(lubridate)
# soit deux colonnes dates écrites en character
a <- data.frame(dates1=c("2011-01-01",'2011-02-01','2011-03-01'),
                dates2=c("2013 Mar, 1",'2013 Feb, 1','2013 Jun, 01'))
# la fonction ymd infère et transforme en date
# apply applique cette fonction à toutes les colonnes (argument 2)
df<-apply(a, 2, FUN = function(x) ymd(x))
class(df)
# en sortie c'est une matrice
df<-as.data.frame(df)
# les deux dates sont typés en numeriques
str(df)
# on les transforme avec la fonction as.date
df$dates3<-as.Date(df$dates1,origin = "1970-01-01")
# ou 
df$dates4<-as_date(df$dates2)

