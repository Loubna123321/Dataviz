
##1. Population religieuse 

#Ici nous allons briévement représenter la population religieuse en France en 2003 (HDV) et en 2013 (EPIC) avec 2 questions. 

#1- Nous allons pouvoir faire un comparatif de 10 ans vis à vis de la population religieuse. 
#2- Nous ferons également un comparatif entre homme et femmes, religieux.ses entre 2003 et 2013, puis dans son ensemble.
library(foreign)
library(questionr)
library(survey)
library(R2HTML)
library(tidyverse)
library(esquisse)
library(labelled)
library(DataExplorer)
library(funModeling)
library(missForest)
library(dplyr)
library(tinytex)
library(ggplot2)


#Exploration et nettoyage de la base HDV2003: 

##La base de données HDV étant en 4 parties, 
##nous avons ouvert la premiere et la quatrieme...
hdv1<-read.csv("hdv1.csv",sep = ";")
hdv4<-read.csv("hdv4.csv",sep = ";")

##...pour récupérer les variables qui nous interressent.
hdv <- cbind(select(hdv1, c(AGEE,AGECJ)), select(hdv4, c(STATUTE,STATUTCJ,NIVE2TE,NIVE2TCJ,QUALIFE,QUALCJ,SEXEE,SEXECJ,VRELIG,VENREL,VIECOUE)))

## Puis nous nettoyons notre environnement
rm(hdv1,hdv4)

#appeler la script Recodage contenant les traitements des variables de la base hdv:

source("recodage_hdv2003.R")

#representer les données: 

head(hdv)

#les dimensions de la base hdv:

dim(hdv)
describe(hdv)
str(hdv)

#appeler le script exploration de données :

source("exploration_hdv2003.R")

## Pour la base EPIC, nous avons ouvert la base de donnees EPIC pour y recuperer ce qu'il nous faut :

epic <- select(read.csv("repondant.txt", sep="\t"),
               c(IndCOU,R_IMPREL,R_RELIGC,R_RELIGION,C_DIPLOMEC,M_DIPLOME,C_CS13C,M_CS13,SEXER,H_SEXEC_C,AGEM,H_ANAISC_C,R_MOINDIPL,R_PLUDIPL))

#appeler la script Recodage contenant les traitements des variables de la base epic:

source("recodage_epic.R")

#Representer les données :

head(epic)

#Les dimensions de la base: 

dim(epic)
str(epic)

#appeler le script exploration de données :

source("exploration_epic.R")

#Comparaison :

###############################Justine###########################:

##1. Population religieuse 

##Pour cette partie qui cherche à mettre en contexte et représenter la population religieuse g?n?rale en france, nous allons faire diff?rentes repr?sentations. 
#1.a - Nous allons en premier rep?senter la population religieuse et non religieuse en France en 2003 et en 2013. 
#Nous allons ici voir si le pourcentage de personnes religieuses ? chang? en 10 ans. 

#Pour EPIC

epic$Religiosite <- epic$Religion_enquete
epic$Religiosite <- as.factor(epic$Religiosite)

epic$Religiosite <- fct_recode(epic$Religiosite,
                               "Croyant"="Catholicisme",
                               "Croyant"="Protestantisme",
                               "Croyant"="Islam",
                               "Croyant"="Bouddhisme",
                               "Croyant"="Hindouisme",
                               "Croyant"="Judaïsme",
                               "Croyant"="Autre",
                               "Non-Croyant"="Sans religion")


freq(epic$Religiosite)

#Nombre de personne croyante et non croyante en 2013 (epic)


#Pour HDV 
hdv$Religiosite <- hdv$Rapport_religion
hdv$Religiosite <- as.factor(hdv$Religiosite) 

hdv$Religiosite <- fct_recode(hdv$Religiosite,
                                 "Croyant"="Pratique réguliere",
                                 "Croyant"="Pratique occasionnelle",
                                 "Croyant"="Sentiment d'appartenance sans pratique",
                                 "Non croyant"="Ni pratique ni appartenance",
                                 "Non croyant"="Rejet")
freq(hdv$Religiosite)


#Analyse 1.a 

##Nous pouvons ici constater qu'en 10 ans, le pourcentage de personnes croyantes et non croyantes en France est similaire (73% de croyants en 2003, et 72% en 2013). 
##En France, en 2003 et en 2013, pr?s des 3/4 de la population est croyante (72% en 2013, et 73% en 2003), contre seulemt 1/4 sont non croyants (27% en 2013 et 25% en 2003).

#1.b - En deuxi?me, nous allons voir la proportion de personne religieuse en France en fonction du sexe et en fonction de l'?ge. 
#Nous allons ici voir si il y a une diff?rence de croyance selon le sexe et en fonction de la cat?rogie d'?ge. 
# Les cat?gories d'?ge seront les suivantes : (25-40 ans), (40-55 ans), (55-70 ans) et (70+)

table(epic$AGEM)
levels(epic$AGEM) <- c("25-39 ans", "40-54 ans", "55-70 ans", "70 ans et plus")

levels(hdv$AGEE) <- c("25-39 ans", "40-54 ans", "55-70 ans", "70 ans et plus")



#1.c - En troisi?me, nous allons regarder la proportion des diff?rents types de religion recens?es en France en 2013.


###Ne fonctionne pas 
#epic %>%
filter(R_RELIGION >= 1L & R_RELIGION <= 10L) %>%
  ggplot() +
  aes(x = R_RELIGIONre) +
  geom_bar(fill = "#5475B2") +
  +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22L),
    axis.title.x = element_text(face = "italic")
  )

#Fonctionne 

labs(
  title = "Repr?sentation des religions pr?sentent en France en 2013",
  subtitle = "En pourcentage "
)

freq(epic$R_RELIGIONre)



##Analyse 1.c 

##En 2013, la religion la plus pratiqu?e en France est le catholocisme, avec plus de 60%. 
#Nous pouvons dire que plus de la moiti? des Francais sont catholiques. 
## L'islam est la deuxi?me religion la plus pratiqu?e en France en 2013, avec plus de 6%




###############################Loubna###########################:
#Dans cette partie, nous allons analyser les personnes en couple en fonction de leur réligionisité en 2003, puis en 2013
#Ensuite, nous allons pouvoir comparer les deux cas.
#En plus, nous allons analyser l'age en fonction de la réligionisité en 2003, puis en 2013
#Ensuite, nous allons pouvoir comparer les deux cas.


#2.a)Répartition des personnes en couples en fonction de la religiosité:

#Voyons en premier temps la répartition des personnes en couple:


ggplot(hdv) +
  aes(x = Vie_en_couple) +
  geom_bar(fill = "#0C4C8A") +
  labs(title = "Vie en couple") +
  theme_bw()

#On remarque que les personnes en couple sont presque deux foix plus les personnes qui ne sont pas en couple!

#Répartition des personnes en couples en fonction de la religiosité en 2003 :

hdv %>%
 filter(Vie_en_couple %in% "oui") %>%
 filter(!(Religiosite %in% "NSP")) %>%
 ggplot() +
 aes(x = Religiosite) +
 geom_bar(fill = "#0C4C8A") +
 labs(x = "Croyance et pratiques",title = "La question de pratiques et croyance réligieux pour les gens en couple en 2003") +
 theme_bw() +
 facet_wrap(vars(Vie_en_couple))

#En 2003: l'échantillon des gens réligieux (en couple) dépasse 4000 personnes alors que l'échantillon des gens non-réligieux (en couple)
#est presque de 1400 personnes

#Répartition des personnes en couples en fonction de la religiosité en 2013 :

library(dplyr)
library(ggplot2)
epic %>%
 filter(Indic_relation_amour %in% "relation amoureuse ou en couple") %>%
 filter(!(Religiosite %in% 
 "Ne souhaite pas répondre")) %>%
 ggplot() +
 aes(x = Religiosite) +
 geom_bar(fill = "#0C4C8A") +
 labs(x = "Croyance",title = "La question de croyance pour les gens en couple en 2013") +
 theme_bw() +
 facet_wrap(vars(Indic_relation_amour))

#En 2013: l'échantillon des gens réligieux en couple ou en relation dépasse 4000 personnes également, alors que l'échantillon des 
#gens non-réligieux et ne sont pas en couple est presque de 1500 personnes.

#Analyse:
#Nous pouvons remarquer que la pourcentage des personnes croyants en couple interrogés en 2003 n'a presque pas modifiée 
#en comparaison avec les gens en couple ou en relation intérrogés en 2013!  
#Nous pouvons conclure que il n ' y a pas une difference remarquable entre 2003 et 2013 en terme de gens en couple croyants.

#2.b)Répartition d'age en fonction de la religion:


#*Répartition de l'age en fonction de partiques et croyance  en 2003 :

hdv %>%
  filter(!(Religiosite %in% "NSP")) %>%
  ggplot() +
  aes(x = Religiosite, y = Age_enquete) +
  geom_jitter(size = 1.5) +
  labs(x = "Age de l'enqueté", 
       y = "Croyance et pratiques", title = "Age en fonction de partiques et croyance en 2003")

#en 2003:
#On remarque que l'age des gens interrogés se varie entre 15 et 90:
#les gens qui ont l'age entre 35 et 55 sont le plus remarquables parmi les gens déclarant croyants
#Alors que pour les gens non croyants, on remarque qu'ils sont de moins en moins nombreux à partir de 55ans! ils sont 
#beaucoup moins nombreux que les croyants 


#Voyons un autre figure pour pouvoir analyser en plus et tirer des conclusions pertinentes:
library(ggplot2)
hdv %>%
  filter(!(Religiosite %in% "NSP")) %>%
  ggplot() +
  aes(y= Age_enquete, fill = Religiosite) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_light()

#en 2003:
#on constate que les croyants  ont l'age entre 15 et 90 ans, avec:
#-Premier quartile autour de 37 ans
#-La médiane est autour de 50 ans
#-Troisieme quartile autour de 63ans

#alors que les gens non croyants ont l'age entre 15 et 85 avec des outliers, avec:
#-Premier quartile autour de 31 ans
#-La médiane est autour de 40 ans
#-Troisieme quartile autour de 53an



#*Répartition Age en fonction de croyance  en 2013 :

epic %>%
  filter(!(Religiosite %in% "Ne souhaite pas répondre")) %>%
  ggplot() +
  aes(x = Religiosite, y = Age_individu) +
  geom_jitter(size = 1.5) +
  labs(x = "Age de l'enqueté", y = "croyance",
       title = "Age en fonction de croyance en 2013") +
  theme_gray()

#en 2013:
#On remarque que l'age des gens  se varie entre 20 et 70:
#les croyants sont plus nombreux que les non croyants, et plus on monte dans l'age plus le nombre des croyants augmente
#Alors que pour les gens non croyants, on remarque qu'ils sont beaucoup moins nombreux que les croyants



#Voyons un autre figure pour pouvoir analyser en plus:

epic %>%
  filter(!(Religiosite %in% "Ne souhaite pas répondre")) %>%
  ggplot() +
  aes(fill = Religiosite, y = Age_individu) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_light()

#en 2013:
#on constate que les croyants  ont l'age entre 20 et 70 ans, avec:
#-Premier quartile autour de 39 ans
#-La médiane est autour de 48 ans
#-Troisieme quartile autour de 57 ans

#alors que les gens non croyants ont l'age entre 20 et 70, avec:
#-Premier quartile autour de 34 ans
#-La médiane est autour de 44 ans
#-Troisieme quartile autour de 52an


#Conclusion:
#En comparaison l'age en fonction de la religionisité entre 2003 et 2013:
#On remarque une petite changement entre l'age des croyantes et non croyante:
#En 2003, le nombre des personnes croyants est le plus nombreux entre l'age 35 et 55 ans.
#En 2013, plus l'age augmente plus le nombre des gens croyants augment.

#En 2003, l'age médiane des personnes croyante est autour de 50 ans
#En 2013, l'age médiane des personnes croyante est autour de 48 ans

#En 2003, l'age médiane des personnes non-croyante est autour de 40 ans
#En 2013, l'age médiane des personnes non-croyante est autour de 44 ans


###############################Mbathio###########################:


table(epic$C_DIPLOME)
##je vais croisée la variable religiosité davec le M_DIPLOME pour voir si la réligion de l'enquêté à un rapport avec son niveau de diplome 
##tableau trie a plat 
table(epic$C_DIPLOME,epic$Religiositere)
table(epic$M_DIPLOME,epic$Religiositere)

tab<-table(epic$M_DIPLOME,epic$Religiositere)
lprop(tab)
mosaicplot(tab)
##le tableau nous montre que les individue qui croient au catholisisme semble avoir le Brevet des colleges,BEPC,brevet élémentaire,DNB ou diplôme étrangers de meme niveau comme diplôme le plus élevé (64.8).




###############################Emmanuel###########################:


## Homogamie et religion 
#Repr?sentation de l'homogamie en fonction de l'appartenance ? une religion.

#Dans cette partie qui sera le corps de notre r?flexion, nous allons voir si en France il y a une homogamie religieuse.  

