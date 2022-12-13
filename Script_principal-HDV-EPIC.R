
######Introduction#####

#Les sciences sociales, se sont depuis longtemps interessées à l'homogamie dans la mise en couple. 
#L'homogamie est défini par M. Bouchet-Valat et S. Grobon comme ''l'appartenance des deux conjoints à la même catégorie'' (M. Bouchet-Valat & S. Grobon, (2019). Homogames un jour, homogames toujours ? Rencontre pendant les études et proximité de diplôme et de carrière au sein des couple en France, p.137). 
#Ils entendent par même catégorie le fait d'appartenir à la même classe sociale d'origine ainsi que d'avoir le même niveau de diplome, ou proche.
#Nous savons donc que la mise en couple est dans une grande majorité basé sur une ressemblance assez proche des individus en terme de classe sociale et d'éducation, ainsi que de niveau de diplome. 
# Toutefois, nous les écrits sociologiques sur la représentation de l'homogamie en fonction de la religion sont assez rescents. 
#Nous pouvons tout de même retrouver un écrit de M. Maudet, intitulé 'Si l'amour rend aveugle, la religion lui redonne la vue'(2021), qui s'interesse à l'homogamie religieuse en France. 
#L'homogamie religieuse est définie par M. Maudet comme "le fait d'appartenir au même groupe religieux/non religieux que son ou sa conjoint.e" (2021, p.2).

#C'est pourquoi, dans le cadre de ce devoir, nous allons à partir de deux bases de données, s'interesser à l'homogamie générale des couples en France en 2003 et 2013. Puis nous allons nous plus particulirement s'attarder sur l'homogamie religieuse en 2003 et 2013. 

######Présentation des bases de données#####

##Présentation de l’enquête EPIC  
#L’enquête Étude des Parcours Individuels et Conjugaux (Epic), est la continuité d’un ensemble d’enquête réalisé entre les années 50 et 90, sur les situations matrimoniales. Nous avons l’enquête « Le choix du conjoint » de 1959, ainsi que « La formation des couples » de 1983-1984. 
#L’enquête EPIC a été réalisé en 2013-2014 par l’Institut national d’études démographiques (Ined) et l’Institut national de la statistique et des études économiques (Insee). Le but de cette enquête était de mettre en lumière les différentes manières de mise en couple et de comprendre l’ensemble de ses aspects. De plus, l’enquête EPIC a été l’une des premières à s’intéresser et à intégrer des personnes célibataires dans son étude. 
#Cette enquête a été effectué sur la base d’un tirage au sort d’un échantillon de la population. Parmi celui-ci, 16 000 logements ont été retenus et 7825 entretiens ont été exécuter auprès de personnes âgées de 26 à 65 ans.  

##Présentation de l’enquête HDV  
#L’enquête Histoire de Vie est également dans la continuité d’une enquête réalisé en 1992, intitulé « Mobilité géographique et Insertion sociale (MGIS). 
#L’enquête HDV a vu le jour en 2003, mise sur pied par l’Institut national de la statistique et des études économiques (Insee). Cette enquête porte sur la construction des identités en y comprenant le plus grand nombre d’aspect que comprend la vie sociale. 
#L’enquête HDV a été réalisé auprès de 8403 personnes habitants en France métropolitaine, âgés de 18 ans et plus. 


######Plan######

#Ce devoir sera divisé en 2 parties. 
#Dans une première partie nous introduirons brièvement la population religieuse/non religieuse en France en 2003 et 2013. 
#Dans une deuxième partie, nous nous interesserons à l'homogamie générale puis religiseuse en France, avec un comparatif de 10ans. 
#Cette partie sera divisé en 3, en passant par l'analyse des couples en fonction de leur religion, de leur diplome, puis enfin leur homogamie.



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


##Analyse 1.b 

##En 2013, la religion la plus pratiqu?e en France est le catholocisme, avec plus de 60%. 
#Nous pouvons dire que plus de la moiti? des Francais sont catholiques. 
## L'islam est la deuxi?me religion la plus pratiqu?e en France en 2013, avec plus de 6%



###############################Loubna###########################:
#Dans cette partie, nous allons analyser les personnes en couple en fonction de leur r?ligionisit? en 2003, puis en 2013.
#Puis, nous allons pouvoir comparer ces deux cas.
#Par la suite, nous allons analyser l'age en fonction de la r?ligionisit? en 2003, puis en 2013.
#Puis, nous allons pouvoir comparer ces deux cas.


#2.a)R?partition des personnes en couples en fonction de la religiosit?:

#Voyons en premier temps la r?partition des personnes en couple:
ggplot(hdv) +
  aes(x = Vie_en_couple) +
  geom_bar(fill = "#0C4C8A") +
  labs(title = "Vie en couple") +
  theme_bw()

#Nous pouvons remarquer que les personnes en couple sont pres de deux foix plus les personnes célibataires.

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

#En 2003 nous pouvons remarquer que l'échantillon des personnes religieuses et en couple dépasse 4000. Tandis que l'échantillon des personnes non religieuses et en couple est de 1400.


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

#En 2013, l'?chantillon des personnes religieuses et en couple (ou en relation) dépasse 4000. Tandis que l'échantillon des 
#personnes non religieuses et non en couple est presque de 1500 personnes.

#Analyse:
#Nous pouvons remarquer que la pourcentage des personnes croyantes et en couple interrogées en 2003 est similaire au pourcentage des personnes en couple ou en relation int?rrog?s en 2013.  
#Nous pouvons donc conclure que il n'y a pas de difference remarcable entre 2003 et 2013, concernant les personnes en couple et croyantes. 

#2.b)R?partition d'age en fonction de la religion:


#*R?partition de l'age en fonction des partiques et croyances  en 2003 :

hdv %>%
  filter(!(Religiosite %in% "NSP")) %>%
  ggplot() +
  aes(x = Religiosite, y = Age_enquete) +
  geom_jitter(size = 1.5) +
  labs(x = "Age de l'enquet?", 
       y = "Croyance et pratiques", title = "Age en fonction de partiques et croyance en 2003")

#en 2003:
#On remarque que l'age des personnes interrogées varie entre 15 ans et 90 ans:
#les personnes agées de 35 ans à 55 ans sont les personnes qui déclarent le plus être croyants. 
#On peut constater qu'à partir de 55ans, les personnes se déclarent moins croyantes, toutefois, les personnes non croyantes reste inférieur aux personnes coryantes. 



#Voyons un autre figure pour pouvoir analyser au mieux et tirer des conclusions plus pertinentes:
library(ggplot2)
hdv %>%
  filter(!(Religiosite %in% "NSP")) %>%
  ggplot() +
  aes(y= Age_enquete, fill = Religiosite) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_light()

#en 2003:
#on constate que les personnes croyantes sont agé de 15 à 90 ans, avec:
#-Premier quartile autour de 37 ans
#-La m?diane est autour de 50 ans
#-Troisieme quartile autour de 63ans

#alors que les personnes non croyantes sont agés de 15 à 85 ans avec des outliers, avec:
#-Premier quartile autour de 31 ans
#-La m?diane est autour de 40 ans
#-Troisieme quartile autour de 53an



#*R?partition Age en fonction de la croyance  en 2013 :

epic %>%
  filter(!(Religiosite %in% "Ne souhaite pas r?pondre")) %>%
  ggplot() +
  aes(x = Religiosite, y = Age_individu) +
  geom_jitter(size = 1.5) +
  labs(x = "Age de l'enquet?", y = "croyance",
       title = "Age en fonction de croyance en 2013") +
  theme_gray()

#en 2013:
#On remarque que l'age des personnes vari entre 20 et 70 ans :
#les personnes croyantes sont plus nombreuses que les non croyantes, et plus les personnes sont agés, plus elles sont croyantes.
#Alors que pour les personnes non croyantes, on remarque qu'elles sont beaucoup moins nombreuse que les croyantes.



#Voyons un autre figure pour pouvoir analyser davantage :

epic %>%
  filter(!(Religiosite %in% "Ne souhaite pas r?pondre")) %>%
  ggplot() +
  aes(fill = Religiosite, y = Age_individu) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_light()

#en 2013:
#on constate que les croyants sont agés de 20 à 70 ans, avec:
#-Premier quartile autour de 39 ans
#-La m?diane est autour de 48 ans
#-Troisieme quartile autour de 57 ans

#alors que les non croyants sont agé de 20 à 70 ans, avec:
#-Premier quartile autour de 34 ans
#-La m?diane est autour de 44 ans
#-Troisieme quartile autour de 52an


#Conclusion:
#La religiosité en fonction de l'âge entre 2003 et 2013:
#Nous pouvons remarquer un légé changement de croyance en fonction de l'age des croyants:
#En 2003, le nombre de personne croyantes est le plus élevé entre 35 et 55 ans.
#En 2013, plus l'age augmente plus le nombre des personnes croyantes augmente.

#En 2003, l'age m?diane des personnes croyantes est autour de 50 ans
#En 2013, l'age m?diane des personnes croyantes est autour de 48 ans

#En 2003, l'age m?diane des personnes non-croyantes est autour de 40 ans
#En 2013, l'age m?diane des personnes non-croyantes est autour de 44 ans


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
