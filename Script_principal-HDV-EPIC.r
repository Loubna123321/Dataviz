
##1. Population religieuse 

#Ici nous allons bri?vement repr?senter la population religieuse en France en 2003 (HDV) et en 2013 (EPIC) avec 2 questions. 

#1- Nous allons pouvoir faire un comparatif de 10 ans vis ? vis de la population religieuse. 
#2- Nous ferons ?galement un comparatif entre homme et femmes, religieux.ses entre 2003 et 2013, puis dans son ensemble.
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
#library(ggplot2)


#Exploration et nettoyage de la base HDV2003: 

##La base de donn?es HDV ?tant en 4 parties, 
##nous avons ouvert la premiere et la quatrieme...
hdv1<-read.csv2("hdv1.csv")
hdv4<-read.csv2("hdv4.csv")

# Voir le nom des colonnes
colnames(hdv1)


hdv1_select <- select(hdv1, c(AGEE,AGECJ))
hdv4_select <- select(hdv4, c(STATUTE,STATUTCJ,NIVE2TE,NIVE2TCJ,QUALIFE,QUALCJ,SEXEE,SEXECJ,VRELIG,VENREL,VIECOUE))

##...pour r?cup?rer les variables qui nous interressent via la fonction cbin qui permet de concaténer les deux BD
hdv <- cbind(hdv1_select, hdv4_select)

## Puis nous nettoyons notre environnement
rm(hdv1,hdv4)

#appeler la script Recodage contenant les traitements des variables de la base hdv:
source("recodage_hdv2003.R")

#Affichage des 05 premiéres 
head(hdv)

#les dimensions de la base hdv:
dim(hdv) # Nombre de colonnes et nombre de lignes
describe(hdv) # Statistiques sur ta bases de données
str(hdv) # Description des variables 

#appeler le script exploration de donn?es :

source("exploration_hdv2003.R")
##### appel du fichier Sokhna_hdv.R pour la création d'une nouvelle variable dans la base hdv
source("sokhna_hdv.R")

## Pour la base EPIC, nous avons ouvert la base de donnees EPIC pour y recuperer ce qu'il nous faut :

epic <- select(read.csv("repondant.txt", sep="\t"),
               c(IndCOU,R_IMPREL,R_RELIGC,R_RELIGION,C_DIPLOMEC,M_DIPLOME,C_CS13C,M_CS13,SEXER,H_SEXEC_C,AGEM,H_ANAISC_C,R_MOINDIPL,R_PLUDIPL))


head(epic)

#Les dimensions de la base: 

dim(epic)
str(epic)

#appeler le script exploration de donn?es :

source("exploration_epic.R")

#Comparaison :

###############################Justine###########################:

##1. Population religieuse 

##Pour cette partie qui cherche ? mettre en contexte et repr?senter la population religieuse g?n?rale en france, nous allons faire diff?rentes repr?sentations. 
#1.a - Nous allons en premier rep?senter la population religieuse et non religieuse en France en 2003 et en 2013. 
#Nous allons ici voir si le pourcentage de personnes religieuses ? chang? en 10 ans. 

#Pour EPIC - Nombre de personne croyante et non croyante en France en 2013

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

table(epic$Religiosite)
data_reg <-data.frame(table(epic$Religiosite))
data_reg



ggplot(epic) +
  aes(x=Religiosite,y = ..prop..,group = 1,fill= ..prop..)+
  geom_bar() +
  labs(title = "Religiosite en 2013",
       y="pourcentage") +
  theme_bw()



#Pour HDV - Nombre de personne croyante et non croyante en France en 2003 
hdv$Religiosite <- hdv$Rapport_religion
hdv$Religiosite <- as.factor(hdv$Religiosite) 

hdv$Religiosite <- fct_recode(hdv$Religiosite,
                              "Croyant"="Pratique réguliere ",
                              "Croyant"="Pratique occasionnelle",
                              "Croyant"="Sentiment d'appartenance sans pratique",
                              "Non croyant"="Ni pratique ni appartenance",
                              "Non croyant"="Rejet")
freq(hdv$Religiosite)


table(hdv$Religiosite)
data_reg <-data.frame(table(hdv$Religiosite))
data_reg



ggplot(hdv) +
  aes(x=Religiosite,y = ..prop..,group = 1,fill= ..prop..)+
  geom_bar() +
  labs(title = "Religiosite en 2003",
       y="pourcentage") +
  theme_bw()

#Analyse 1.a 

##Nous pouvons ici constater qu'en 10 ans, le pourcentage de personnes croyantes et non croyantes en France est similaire (73% de croyants en 2003, et 72% en 2013). 
##En France, en 2003 et en 2013, pr?s des 3/4 de la population est croyante (72% en 2013, et 73% en 2003), contre seulemt 1/4 sont non croyants (27% en 2013 et 25% en 2003).



#1.b - En deuxième, nous allons regarder la proportion des diff?rents types de religion recens?es en France en 2013.


freq(epic$Religion_enquete)


table(epic$Religion_enquete)
data_reg <-data.frame(table(epic$Religion_enquete))
data_reg



ggplot(epic) +
  aes(y=Religion_enquete,x = ..prop..,group = 1,fill= ..prop..)+
  geom_bar() +
  labs(title = "Types de religion en France en 2013",
       y="pourcentage") +
  theme_bw()

###############################Loubna###########################:
#Dans cette partie, nous allons analyser les personnes en couple en fonction de leur r?ligionisit? en 2003, puis en 2013
#Ensuite, nous allons pouvoir comparer les deux cas.
#En plus, nous allons analyser l'age en fonction de la r?ligionisit? en 2003, puis en 2013
#Ensuite, nous allons pouvoir comparer les deux cas.


#2.a)R?partition des personnes en couples en fonction de la religiosit?:

#Voyons en premier temps la r?partition des personnes en couple:
ggplot(hdv) +
  aes(x = Vie_en_couple) +
  geom_bar(fill = "#0C4C8A") +
  labs(title = "Vie en couple") +
  theme_bw()

#On remarque que les personnes en couple sont presque deux foix plus les personnes qui ne sont pas en couple!

#R?partition des personnes en couples en fonction de la religiosit? en 2003 :

hdv %>%
  filter(Vie_en_couple %in% "oui") %>%
  filter(!(Religiosite %in% "NSP")) %>%
  ggplot() +
  aes(x = Religiosite) +
  geom_bar(fill = "#0C4C8A") +
  labs(x = "Croyance et pratiques",title = "La question de pratiques et croyance r?ligieux pour les gens en couple en 2003") +
  theme_bw() +
  facet_wrap(vars(Vie_en_couple))

#En 2003: l'?chantillon des gens r?ligieux (en couple) d?passe 4000 personnes alors que l'?chantillon des gens non-r?ligieux (en couple)
#est presque de 1400 personnes

#R?partition des personnes en couples en fonction de la religiosit? en 2013 :

library(dplyr)
library(ggplot2)
epic %>%
  filter(Indic_relation_amour %in% "relation amoureuse ou en couple") %>%
  filter(!(Religiosite %in% 
             "Ne souhaite pas r?pondre")) %>%
  ggplot() +
  aes(x = Religiosite) +
  geom_bar(fill = "#0C4C8A") +
  labs(x = "Croyance",title = "La question de croyance pour les gens en couple en 2013") +
  theme_bw() +
  facet_wrap(vars(Indic_relation_amour))

#En 2013: l'?chantillon des gens r?ligieux en couple ou en relation d?passe 4000 personnes ?galement, alors que l'?chantillon des 
#gens non-r?ligieux et ne sont pas en couple est presque de 1500 personnes.

#Analyse:
#Nous pouvons remarquer que la pourcentage des personnes croyants en couple interrog?s en 2003 n'a presque pas modifi?e 
#en comparaison avec les gens en couple ou en relation int?rrog?s en 2013!  
#Nous pouvons conclure que il n ' y a pas une difference remarquable entre 2003 et 2013 en terme de gens en couple croyants.

#2.b)R?partition d'age en fonction de la religion:


#*R?partition de l'age en fonction de partiques et croyance  en 2003 :

hdv %>%
  filter(!(Religiosite %in% "NSP")) %>%
  ggplot() +
  aes(x = Religiosite, y = Age_enquete) +
  geom_jitter(size = 1.5) +
  labs(x = "Age de l'enquet?", 
       y = "Croyance et pratiques", title = "Age en fonction de partiques et croyance en 2003")

#en 2003:
#On remarque que l'age des gens interrog?s se varie entre 15 et 90:
#les gens qui ont l'age entre 35 et 55 sont le plus remarquables parmi les gens d?clarant croyants
#Alors que pour les gens non croyants, on remarque qu'ils sont de moins en moins nombreux ? partir de 55ans! ils sont 
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
#-La m?diane est autour de 50 ans
#-Troisieme quartile autour de 63ans

#alors que les gens non croyants ont l'age entre 15 et 85 avec des outliers, avec:
#-Premier quartile autour de 31 ans
#-La m?diane est autour de 40 ans
#-Troisieme quartile autour de 53an



#*R?partition Age en fonction de croyance  en 2013 :

epic %>%
  filter(!(Religiosite %in% "Ne souhaite pas r?pondre")) %>%
  ggplot() +
  aes(x = Religiosite, y = Age_individu) +
  geom_jitter(size = 1.5) +
  labs(x = "Age de l'enquet?", y = "croyance",
       title = "Age en fonction de croyance en 2013") +
  theme_gray()

#en 2013:
#On remarque que l'age des gens  se varie entre 20 et 70:
#les croyants sont plus nombreux que les non croyants, et plus on monte dans l'age plus le nombre des croyants augmente
#Alors que pour les gens non croyants, on remarque qu'ils sont beaucoup moins nombreux que les croyants



#Voyons un autre figure pour pouvoir analyser en plus:

epic %>%
  filter(!(Religiosite %in% "Ne souhaite pas r?pondre")) %>%
  ggplot() +
  aes(fill = Religiosite, y = Age_individu) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_light()

#en 2013:
#on constate que les croyants  ont l'age entre 20 et 70 ans, avec:
#-Premier quartile autour de 39 ans
#-La m?diane est autour de 48 ans
#-Troisieme quartile autour de 57 ans

#alors que les gens non croyants ont l'age entre 20 et 70, avec:
#-Premier quartile autour de 34 ans
#-La m?diane est autour de 44 ans
#-Troisieme quartile autour de 52an


#Conclusion:
#En comparaison l'age en fonction de la religionisit? entre 2003 et 2013:
#On remarque une petite changement entre l'age des croyantes et non croyante:
#En 2003, le nombre des personnes croyants est le plus nombreux entre l'age 35 et 55 ans.
#En 2013, plus l'age augmente plus le nombre des gens croyants augment.

#En 2003, l'age m?diane des personnes croyante est autour de 50 ans
#En 2013, l'age m?diane des personnes croyante est autour de 48 ans

#En 2003, l'age m?diane des personnes non-croyante est autour de 40 ans
#En 2013, l'age m?diane des personnes non-croyante est autour de 44 ans


###############################Mbathio###########################:
### nous allons créer une nouvelle variable qui nous permettra de voir s'il ya un homogamie de diplome
hdv$meme_diplome <- ifelse(as.character(hdv$Niveau_etude_conjoint) == as.character(hdv$Niveau_etude_enquete), "YES", "NO")

### nous allons créer une variable qui nous permettra de voir si il y'a un homogamie de diplome. 
epic$meme_diplome <- ifelse(as.character(epic$Diplome_eleve_conjoint) == as.character(epic$Diplome_eleve_enquete), "YES", "NO")



#### Homogamie et religion 
#Representation de l'homogamie en fonction de l'appartenance à une religion.
freq(epic$meme_diplome)


table(epic$meme_diplome)
data_reg <-data.frame(table(epic$meme_diplome))
data_reg


ggplot(epic) +
  aes(y=meme_diplome,x = ..prop..,group = 1,fill= ..prop..)+
  geom_bar() +
  labs(title = "Meme diplome",
       y="pourcentage") +
  theme_bw()

##Analyse 1.b 

##En 2013, la religion la plus pratiqu?e en France est le catholocisme, avec plus de 60%. 
#Nous pouvons dire que plus de la moiti? des Francais sont catholiques. 
## L'islam est la deuxi?me religion la plus pratiqu?e en France en 2013, avec plus de 6%



table(epic$meme_diplome)
##je vais croiser la variable religiosité davec meme diplome pour voir si les personnes qui se déclarent religieuses, portent moins d'attention à l'homogamie de diplome
##tableau trie a plat ( tableau de contingence)
tab_epic = table(epic$meme_diplome,epic$Religiosite)
tab_hdv = table(hdv$meme_diplome,hdv$Religiosite)
####nous avons constaté que les personnes qui se déclarent croyants porte moins attentions à l'homogamie de diplome(73.3) que les non croyant(25.4)
lprop(tab_epic)
lprop(tab_hdv)
mosaicplot(tab_epic)
library(ggmosaic)

ggplot(data = epic) +
  geom_mosaic(aes(x = product(meme_diplome, Religiosite), fill = Religiosite, na.rm = TRUE)) +
  xlab("") + ylab("") +
  theme(legend.position = "bottom")

ggplot(data = hdv) +
  geom_mosaic(aes(x = product(meme_diplome, Religiosite), fill = Religiosite, na.rm = TRUE)) +
  xlab("") + ylab("") +
  theme(legend.position = "bottom")
##le tableau nous montre que les individue qui croient au catholisisme semble avoir le Brevet des colleges,BEPC,brevet ?l?mentaire,DNB ou dipl?me ?trangers de meme niveau comme dipl?me le plus ?lev? (64.8).


###############################Emmanuel###########################:

## Homogamie et religion 
#Repr?sentation de l'homogamie en fonction de l'appartenance ? une religion.

#Dans cette partie qui sera le corps de notre r?flexion, nous allons voir si en France il y a une homogamie religieuse.  
