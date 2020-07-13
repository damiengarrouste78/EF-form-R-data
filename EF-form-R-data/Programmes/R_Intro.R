library(tidyverse)

setwd("~/EF-form-R-data/EF-form-R-data")
# 01: Première démo de R ------------------------

2 + 2

sqrt(3)

a <- 10
a
b <- 3
b
a + b

seq(1, 10, 1)
rnorm(10)

x <- c(2, 8, 7, 4, 5, 2, 7, 9, 4, 10)
class(x)
mean(x)
class(m)

x[2]
x[2:4]
rank(x)
rank(sort(x))
plot(x)
hist(x)

### CREER UNE FONCTION QUI REPRESENTE LA DISTRIBUTION D'UNE VAR GAUSSIENNE (loi normale)

x <- rnorm(1000)
plot(density(x))

# cette fonction prend en entree l'argument var 
# elle cree une va qui suit une loi normale de taille var
# on represente la distrib hist
demo_func <- function(var) {
    x <- rnorm(var)
    hist(sort(x))
}

demo_func(1000)



# 02 : Données, objets et packages -------------

# Créations d'objets ===========

# vecteurs

myvec1 <- c(1, 5, 6)
class(myvec1)

myvec2 <- c("un", "autre", "essai")
class(myvec2)

# listes

maliste <- list(myvec1, myvec2)
class(maliste)
maliste
maliste[1]
maliste[2]
unlist(maliste)

# dataframes
mydf <- data.frame(colonne1 = myvec1,
                   colonne2 = myvec2)
names(mydf)<-c("var1","var2")
names(mydf)


# matrices
(mymat1 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3))
(mymat2 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2))
t(mymat2)

# Système d'indexation ============

# par position
mydf[, 1]
mydf[, 2]
mydf[1, 1]

# par nom
mydf$var1
mydf$var2

# on compare, ca renvoie un booléen TRUE OU FALSE
x<-c("a","b")
y<-c(2,3)
x==y

# affectation , x<-1
#x=1


# Opérateurs ===========

# arithmétique
myvec1[1] + myvec1[2]
myvec1[1] * myvec1[2]
myvec1[2] ^ myvec1[3]


# logique
myvec1[1] == myvec2[2]
myvec1[1] != myvec2[2]
myvec1[1] < myvec2[2]

isTRUE(myvec1[1] < myvec2[2])
myvec1 > 8
myvec1 > 0 & myvec1 < 5
myvec1 > 0 | myvec1 < 5

# constantes
pi
letters
LETTERS
month.name


# Fonctions de base ==============

# Statistiques / mathématiques
mean(myvec1)
sd(myvec1)
max(myvec1)
sqrt(myvec1)
cos(myvec1)


# Tri, arrondis, etc
sort(myvec1, decreasing = TRUE)
rank(myvec1)

round(myvec1 + 0.34, 1)
floor(myvec1 + 0.34)
ceiling(myvec1 + 0.34)

# Import / export de données ================
saveRDS(mydf, "mydf.rds")
rm(mydf)
mydf_imported <- readRDS("mydf.rds")

write.table(mydf, "mydf.txt")
read.table("mydf.txt")

write.table(mydf, "mydf2.txt", sep = "|")
read.table("mydf2.txt")
read.table("mydf2.txt", sep = "|")

write.csv(mydf, "mydf.csv")

xlsx::write.xlsx(mydf, "mydf.xlsx", row.names = FALSE)


# Séquences =======================

# Génération de séquences
c(1:10)
seq(1, 10, 2)
rep(1, 3)

# Séquences aléatoires

runif(3)
runif(3, min = 1, max = 100)
round(runif(3, min = 1, max = 100), 0)

sample(0:10, 10)
sample(0:10, 10, replace = TRUE)

x <- rnorm(10000, mean = 0, sd = 1)
mean(x)
sd(x)

# 03: Exploration de données --------------------------

# Fonctions numériques ==========

# summary()
x <- rnorm(10000, mean = 0, sd = 1)
summary(x)

# str()
str(x)
str(mydf)

# table()
y1 <- sample(0:100, 1000, replace = TRUE)
table(y1)

mytable <- data.frame(z1 = sample(0:10, 100, replace = TRUE),
                      z2 = sample(0:10, 100, replace = TRUE))
str(mytable)
table(mytable$z1,
      mytable$z2)
prop.table(table(mytable$z1,
                 mytable$z2))

mydf2<-mydf_imported[3,]

# STR ca veut dire STRUCTURE de l'objet
str(mydf_imported)
summary(mydf_imported)
mydf_imported$colonne2<-as.factor(mydf_imported$colonne2)
summary(mydf_imported)

table(mydf_imported$colonne2)
# FAIRE UN TRI CROISE
t<-table(titanic$Pclass,titanic$Survived)
# PROFIL LIGNES = pourcentage en lignes d'un tric croisé
prop.table(table(titanic$Pclass,titanic$Survived),margin=1)


# Fonctions graphiques ============

esquisse::esquisser()


# Exploration interactive et visuelle des données avec Radiant() =========

radiant::radiant()

# 04 : Préparation de données ------------------------------

# Base R vs dplyr =================
library(radiant.data)
data(titanic)
str(titanic)

# calcul de l'age moyen des personnes qui ont survecu
# avec base R
survived <- titanic[titanic$survived == "Yes", ]
mean(survived$age)

# avec dplyr
library(tidyverse)
titanic %>% group_by(survived) %>% summarise(age_moyen = mean(age))

# Transformations / Agrégats =================

# mutate()
titanic %>%
    mutate(last_name = sub(",.*", "", name)) %>%
    count(last_name, sort = TRUE)

# summarise()
titanic %>%
    group_by(pclass) %>%
    summarise(min_fare = min(fare),
              max_fare = max(fare))

# Sélection / filtres =================

# select()
names(titanic)
titanic %>% dplyr::select(survived, sex, age)
titanic %>% dplyr::select(2:4)
titanic %>% dplyr::select(starts_with("s"))
titanic %>% dplyr::select(ends_with("e"))
titanic %>% dplyr::select(contains("r"))

keep.vars <- c("age", "name")
titanic %>% dplyr::select(one_of(keep.vars))
titanic %>% dplyr::select(-one_of(keep.vars))

titanic %>% dplyr::select(name, everything())
titanic %>% dplyr::select(nom = name, everything())

# filter ()
titanic %>% dplyr::filter(survived == "Yes")
titanic %>% dplyr::filter(fare > 200)
titanic %>% dplyr::filter(fare > 200 & survived == "Yes")
titanic %>% dplyr::filter(fare > 200 | survived == "Yes")

# Tris et comptages

titanic %>% arrange(fare)
titanic %>% arrange(desc(fare))

titanic %>% count(sex)
titanic %>% count(pclass, sort = TRUE)

titanic %>% group_by(sex, pclass) %>% count()

# Jointures et pivots ================

df1 <- titanic %>% select(one_of(keep.vars))
df2 <- titanic %>% select(embarked)
bind_cols(df1, df2)

titanic %>% count(cabin, sort = TRUE)

df3 <- titanic %>% dplyr::filter(cabin == "G6")
df4 <- titanic %>% dplyr::filter(cabin == "F2")
bind_rows(df3, df4)

df5 <- titanic %>% sample_n(10) %>% dplyr::select(name)
inner_join(df5, titanic)

titanic %>%
    select(sex, fare, age) %>%
    pivot_longer(-sex, names_to = "variable", values_to = "value")

test <- titanic %>%
    distinct(name, pclass) %>%
    mutate(dummy = 1) %>%
    pivot_wider(names_from = pclass, values_from = dummy)
test[is.na(test)] <- 0
colSums(test > 0)

# Valeurs manquantes ===================
library(missForest)
titanic_miss <- prodNA(titanic, noNA = 0.05)
colSums(is.na(titanic_miss))

titanic_miss %>% summarise(age_median = median(age))
(age_median <- titanic_miss %>%
        summarise(age_median = median(age, na.rm = TRUE)) %>%
        t() %>% as.numeric())

temp <- titanic_miss %>%
    mutate(age = coalesce(age, age_median))
colSums(is.na(temp))
summary(temp$age)
summary(titanic$age)


# Valeurs extrêmes ==============================

(mybox <- boxplot(titanic$age))
mybox$statsr
summary(titanic$age)

(titanic_high <- titanic %>%
    dplyr::filter(age > max(mybox$stats))) %>%
    arrange(desc(age))

titanic %>% top_n(5, age)

titanic %>% top_frac(0.02, age)
titanic %>% top_frac(0.02, age) %>%
    summarise(min_age = min(age),
              max_age = max(age))

titanic %>% summarise(quants = quantile(age, probs = c(1:9) / 10))

# recodages ======================
data("titanic")
questionr::icut(titanic)
questionr::irec(titanic)


# 05: Modèles statistiques --------------------------------

# Régression linéaire  ===============
library(fastDummies)
titanic2 <- titanic %>%
    select(pclass, sex, age, sibsp, parch, fare, embarked) %>%
    dummy_cols(
        select_columns = c("pclass", "sex", "embarked"),
        remove_selected_columns = TRUE,
        remove_first_dummy = TRUE
    )

myregression <- lm(fare ~ ., titanic2)
summary(myregression)
fitted_values <- myregression$fitted.values
check_values <- data.frame(real = titanic2$fare,
                           pred = fitted_values)
plot(check_values)

# Tests statistiques ================

# Corrélation
penguins <- readRDS("data/penguins.rds")

glimpse(penguins)

plot(penguins$bill_length_mm,penguins$flipper_length_mm)
cor.test(penguins$bill_length_mm,penguins$flipper_length_mm, method="pearson")


# Khi2
M <- table(penguins$species, penguins$island)
M
(test <- chisq.test(M))


# Clusterisation ====================
complete.penguins <- penguins[complete.cases(penguins),]
mydf <- complete.penguins %>%
    select(-c("species","island","sex"))
species <- complete.penguins$species

mykm <- kmeans(mydf,3)
table(mykm$cluster, species)


# 06: Initiation à la programmation ------------------------------

# Boucles ===============================

myvec <- rnorm(5)
for (i in 1:length(myvec)) {
    print(myvec[i])
}

library(eurostat)
industry_search <- eurostat::search_eurostat("industry")
(industry_datasets <- industry_search %>% 
        select(title) %>% 
        t() %>% as.character())

check <- vector()
for (i in 1:length(industry_datasets)) {
    check[i] <- stringr::str_detect(industry_datasets[i],"income")
}
result <- industry_search[check,]

i<-1
while (i<=5){ 
    print(i)
    i<-i+1  
} 



# Création de fonctions ================
chiffres <- rnorm(100)

echelle <- function(v) {
    a <- min(v)
    b <- max(v)
    res <- (v-a)/(b-a)
    return(res)
    }
echelle(chiffres)
plot(density(chiffres))

plot(density(echelle(chiffres)))

