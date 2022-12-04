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

##Nous allons ici travailler sur deux bases de données, à savoir : HDV - Histoire de vie (2003) ainsi que EPIC - Étude des parcours individuels et conjugaux. 

##La base de donnÃ©es HDV Ã©tant en 4 parties, 
##nous avons ouvert la premiere et la quaTrime...
hdv1<-read.dbf("hdv1.dbf")
hdv4<-read.dbf("hdv4.dbf")

##...pour rÃ©cupÃ©rer les variables qui nous interressent.
hdv <- cbind(select(hdv1, c(AGEE,AGECJ)), select(hdv4, c(STATUTE,STATUTCJ,NIVE2TE,NIVE2TCJ,QUALIFE,QUALCJ,SEXEE,SEXECJ,VRELIG,VENREL,VIECOUE)))

## Puis nous nettoyons notre environnement
rm(hdv1,hdv4)

## Pour EPIC, nous avons ouvert la base de donnÃ©es EPIC pour y recuperer ce qu'il nous faut :
epic <- select(read.csv("repondant.txt", sep="\t"),c(IndCOU,R_IMPREL,R_RELIGC,R_RELIGION,C_DIPLOMEC,M_DIPLOME,C_CS13C,M_CS13,SEXER,H_SEXEC_C,AGEM,H_ANAISC_C,R_MOINDIPL,R_PLUDIPL))


##Par soucis de lisibilitÃ©, nous allons recoder les variables et leurs modalitÃ©s.



##Recodage des variables HDV 

# Recodage de la variable Religion 
hdv$VRELIGf <- as.factor(hdv$VRELIG) 

hdv$VRELIGre <- fct_recode(hdv$VRELIGf,
                         "Pratique rÃ©guliÃ¨re"="1",
                         "Pratique occasionnelle"="2",
                         "Sentiment d'appartenance sans pratique"="3",
                         "Ni pratique ni appartenance"="4",
                         "Rejet"="5",
                         "NSP"="6")




#Recodage de la variable NIV2TE - Niveau d'Ã©tude de l'enquÃªtÃ©

hdv$NIVE2TEf <- as.factor(hdv$NIVE2TE) 

hdv$NIVE2TEre <- fct_recode(hdv$NIVE2TEf,
                          "N'a jamais fait d'Ã©tudes"="00",
                          "A arretÃ© avant la derniÃ¨re annÃ©e de primaire"="03",
                          "Arret la derniÃ¨re annÃ©e de primaire"="04",
                          "1er cycle d'enseignement gÃ©nÃ©ral"="15",
                          "2Ã¨me sycle d'enseignement gÃ©nÃ©ral"="17",
                          "Enseignement technique ou professionnel court"="20", 
                          "Enseignement technique ou professionnel long"="30", 
                          "Enseignement supÃ©rieur y compris technique supÃ©rieur"="40")



#Recodage de la variable NIVE2TCJ - NIveau d'Ã©tude du conjoint

hdv$NIVE2TCJf <- as.factor(hdv$NIVE2TCJ) 
hdv$NIVE2TCJre <- fct_recode(hdv$NIVE2TCJf,
                           "N'a jamais fait d'Ã©tudes"="00",
                           "A arretÃ© avant la derniÃ¨re annÃ©e de primaire"="03",
                           "Arret la derniÃ¨re annÃ©e de primaire"="04",
                           "1er cycle d'enseignement gÃ©nÃ©ral"="15",
                           "2Ã¨me sycle d'enseignement gÃ©nÃ©ral"="17",
                           "Enseignement technique ou professionnel court"="20", 
                           "Enseignement technique ou professionnel long"="30", 
                           "Enseignement supÃ©rieur y compris technique supÃ©rieur"="40", 
                           "Ne sait pas"="99")



#Recodage de la variable QUALIFE - Position professionnel de l'enquÃªtÃ© 

hdv$QUALIFEf <- as.factor(hdv$QUALIFE) 

hdv$QUALIFEre <- fct_recode(hdv$QUALIFEf,
                          "Manoeuvre ou ouvrier spÃ©cialisÃ©"="1",
                          "Ouvrier qualifiÃ© ou technicien.ne d'atelier"="2",
                          "Technicien.ne non cadre"="3",
                          "Agent administratif ou commerciaux"="4",
                          "IngÃ©nieur ou cadre"="5",
                          "EmployÃ© ou personnel"="6", 
                          "Autre"="7")



#Recodage de la variable QUALCJ - position professionnel du conjoint

hdv$QUALCJf <- as.factor(hdv$QUALCJ) 

hdv$QUALCJre <- fct_recode(hdv$QUALCJr,
                         "Manoeuvre ou ouvrier spÃ©cialisÃ©"="1",
                         "Ouvrier qualifiÃ© ou technicien.ne d'atelier"="2",
                         "Technicien.ne non cadre"="3",
                         "Agent administratif ou commerciaux"="4",
                         "IngÃ©nieur ou cadre"="5",
                         "EmployÃ© ou personnel"="6", 
                         "Autre"="7",
                         "NSP"="9")



#Recodage des variables de bases : SEXEE (sexe enquÃªtÃ©), SEXECJ (sexe conjoint), AGEE (age enquÃªtÃ©), AGECJ (age conjoint).

#Sexe enquÃªtÃ© 
hdv$SEXEEf <- as.factor(hdv$SEXEE) 

hdv$SEXEEre <- fct_recode(hdv$SEXEEf,
                        "Homme"="1",
                        "Femme"="2",)

table(hdv$SEXEE)

#Sexe conjoint 
hdv$SEXECJf <- as.factor(hdv$SEXECJ) 

hdv$SEXECJre <- fct_recode(hdv$SEXECJf,
                         "Homme"="1",
                         "Femme"="2",)






##Recodage des variables EPIC

## recodage des variables epic religion (R_IMPREL, R_RELIGC, R_RELIGION)

epic$R_IMPRELf<- as.factor(epic$R_IMPREL)

epic$R_IMPRELre <- fct_recode(epic$R_IMPRELf,
                            "importante ou assez importante"="1",
                            "peu importante"= "2", 
                            "sans importance"="3",
                            "NSP"="8")

epic$R_RELIGCf <- as.factor(epic$R_RELIGC)

epic$R_RELIGCre<- fct_recode(epic$R_RELIGCf, 
                           "catholicisme"="1",
                           "protestantisme"="2",
                           "islam"="3",
                           "bouddhisme"="4",
                           "hindouisme"="5",
                           "judaisme"="6",
                           "autre"="7",
                           "sans religion"="8",
                           "ne souhaite pas repondre"="98",
                           "NSP"="99")

epic$R_RELIGIONf <- as.factor(epic$R_RELIGION)
epic$R_RELIGIONre<- fct_recode(epic$R_RELIGIONf,
                             "catholicisme"="1",
                             "protestantisme"="2",
                             "islam"="3",
                             "bouddhisme"="4",
                             "hindouisme"="5",
                             "judaisme"="6",
                             "autre"="7",
                             "sans religion"="8",
                             "ne souhaite pas repondre"="98")

##A mettre si nous avons besoin de voir la population croyance générale.
epic$Religiosite <- epic$R_RELIGIONre
epic$Religiositef <- as.factor(epic$Religiosite)

epic$Religiositere <- fct_recode(epic$Religiositef,
                               "Croyant"="Catholicisme",
                               "Croyant"="Protestantisme",
                               "Croyant"="Islam",
                               "Croyant"="Bouddhisme",
                               "Croyant"="Hindouisme",
                               "Croyant"="JudaÃ¯sme",
                               "Croyant"="Autre",
                               "Non-Croyant"="Sans religion"
)



## recodage des variable homogamie

epic$C_DIPLOMECf <- as.factor(epic$C_DIPLOMEC)

epic$C_DIPLOMECre <- fct_recode(epic$C_DIPLOMECf,
                              "baccalaurÃ©at professionnel"="1",
                              "baccalaurÃ©at technologique ou diplome de ce niveau"= "2",
                              "baccalaureat general"="3",
                              "brevet superieur capacitÃ© en droit DAEU"="4",
                              "diplome de niveau bac"="5",
                              "NSP"="9")

epic$M_DIPLOMEf <- as.factor(epic$M_DIPLOME)

epic$M_DIPLOMEre <- fct_recode(epic$M_DIPLOMEf,
                             "baccalaurÃ©at professionnel"="1",
                             "baccalaurÃ©at technologique ou diplome de ce niveau"= "2",
                             "baccalaureat general"="3",
                             "brevet superieur capacitÃ© en droit DAEU"="4",
                             "diplome de niveau bac"="5",
                             "NSP"="9")

epic$C_CS13Cf <- as.factor(epic$C_CS13C)

epic$C_CS13Cre <- fct_recode(epic$C_CS13Cf,
                             "Agriculteurs"="1",
                             "Artisans, commerçants, chefs d'entreprise"="2",
                             "Cadres des entreprises"="3",
                             "Professions intellectuelles, cadres supérieurs du public, professions libérales"="4",
                             "Profession intermédiaires de l'enseignement du public"="5",
                             "Professions intermédiaires de la santé et du travail social"="6",
                             "Professions intermédiaires des entreprises"="7",
                             "Employés bureau et secteur public"="8",
                             "Employés commerce et services"="9",
                             "Ouvriers qualifiés"="10",
                             "Ouvriers non qualifiés"="11",
                             "Au foyer"="12",
                             "En études"="13",
                             "NSP"="99") 

epic$M_CS13f <- as.factor(epic$M_CS13)

epic$M_CS13re <- fct_recode(epic$M_CS13f,
                            "Agriculteurs"="1",
                            "Artisans, commerçants, chefs d'entreprise"="2",
                            "Cadres des entreprises"="3",
                            "Professions intellectuelles, cadres supérieurs du public, professions libérales"="4",
                            "Profession intermédiaires de l'enseignement du public"="5",
                            "Professions intermédiaires de la santé et du travail social"="6",
                            "Professions intermédiaires des entreprises"="7",
                            "Employés bureau et secteur public"="8",
                            "Employés commerce et services"="9",
                            "Ouvriers qualifiés"="10",
                            "Ouvriers non qualifiés"="11",
                            "Au foyer"="12",
                            "En études"="13",
                            "NSP"="99")

## recodage des variable de base

epic$SEXERf <- as.factor(epic$SEXER)
epic$SEXERre <- fct_recode(epic$SEXERf,
                         "homme"="1",
                         "femme"="2")

epic$H_SEXECf <- as.factor(epic$H_SEXEC)
epic$H_SEXECre <- fct_recode(epic$H_SEXECf,
                           "homme"="1",
                           "femme"="2",
                           "NSP"="9")



##1. Population religieuse 

##Pour cette partie qui cherche à mettre en contexte et représenter la population religieuse générale en france, nous allons faire différentes représentations. 
#1.a - Nous allons en premier repésenter la population religieuse et non religieuse en France en 2003 et en 2013. 
  #Nous allons ici voir si le pourcentage de personnes religieuses à changé en 10 ans. 

#Pour EPIC
epic$Religiosite <- epic$R_RELIGION
epic$Religiositef <- as.factor(epic$Religiosite)

epic$Religiositere <- fct_recode(epic$Religiositef,
                                 "Croyant"="1",
                                 "Croyant"="2",
                                 "Croyant"="3",
                                 "Croyant"="4",
                                 "Croyant"="5",
                                 "Croyant"="6",
                                 "Croyant"="7",
                                 "Non-Croyant"="8"
)

freq(epic$Religiositere)

#Nombre de personne croyante et non croyante en 2013 (epic)


#Pour HDV 

hdv$Vreligiosite <- as.factor(hdv$VRELIG) 

hdv$Vreligiositere <- fct_recode(hdv$Vreligiosite,
                           "Croyant"="1",
                           "Croyant"="2",
                           "Croyant"="3",
                           "Non croyant"="4",
                           "Non croyant"="5",
                           )
freq(hdv$Vreligiositere)


#Analyse 1.a 

##Nous pouvons ici constater qu'en 10 ans, le pourcentage de personnes croyantes et non croyantes en France est similaire (73% de croyants en 2003, et 72% en 2013). 
##En France, en 2003 et en 2013, près des 3/4 de la population est croyante (72% en 2013, et 73% en 2003), contre seulemt 1/4 sont non croyants (27% en 2013 et 25% en 2003).

#1.b - En deuxième, nous allons voir la proportion de personne religieuse en France en fonction du sexe et en fonction de l'âge. 
  #Nous allons ici voir si il y a une différence de croyance selon le sexe et en fonction de la catérogie d'âge. 
  # Les catégories d'âge seront les suivantes : (25-40 ans), (40-55 ans), (55-70 ans) et (70+)
table(epic$AGEM)
levels(epic$AGEM) <- c("25-39 ans", "40-54 ans", "55-70 ans", "70 ans et plus")

levels(hdv$AGEE) <- c("25-39 ans", "40-54 ans", "55-70 ans", "70 ans et plus")



#1.c - En troisième, nous allons regarder la proportion des différents types de religion recensées en France en 2013.


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
    title = "Représentation des religions présentent en France en 2013",
    subtitle = "En pourcentage "
  )
  
  freq(epic$R_RELIGIONre)
  
   

##Analyse 1.c 

##En 2013, la religion la plus pratiquée en France est le catholocisme, avec plus de 60%. 
  #Nous pouvons dire que plus de la moitié des Francais sont catholiques. 
## L'islam est la deuxième religion la plus pratiquée en France en 2013, avec plus de 6%


  
##2. Homogamie et religion 
#Représentation de l'homogamie en fonction de l'appartenance à une religion.

#Dans cette partie qui sera le corps de notre réflexion, nous allons voir si en France il y a une homogamie religieuse.  
 

#2.a 