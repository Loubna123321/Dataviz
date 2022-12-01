library(foreign)
library(questionr)
library(survey)
library(R2HTML)
library(tidyverse)
library(esquisse)

##La base de données HDV étant en 4 parties, 
##nous avons ouvert la premiere et la quatrieme...
hdv1<-read.dbf("hdv1.dbf")
hdv4<-read.dbf("hdv4.dbf")

##...pour récupérer les variables qui nous interressent.
hdv <- cbind(select(hdv1, c(AGEE,AGECJ)), select(hdv4, c(STATUTE,STATUTCJ,NIVE2TE,NIVE2TCJ,QUALIFE,QUALCJ,SEXEE,SEXECJ,VRELIG,VENREL,VIECOUE)))

## Puis nous nettoyons notre environnement
rm(hdv1,hdv4)

## Pour EPIC, nous avons ouvert la base de données EPIC pour y recuperer ce qu'il nous faut :
epic <- select(read.csv("repondant.txt", sep="\t"),c(IndCOU,R_IMPREL,R_RELIGC,R_RELIGION,C_DIPLOMEC,M_DIPLOME,C_CS13C,M_CS13,SEXER,H_SEXEC_C,AGEM,H_ANAISC_C,R_MOINDIPL,R_PLUDIPL))

##1. Population religieuse 

#Ici nous allons brièvement représenté la population religieuse en France en 2003 (HDV) et en 2013 (EPIC) avec 2 questions. 

#1- Nous allons pouvoir faire un comparatif de 10 ans vis à vis de la population religieuse. 
#2- Nous ferons également un comparatif entre homme et femmes, religieux.ses entre 2003 et 2013, puis dans son ensemble.

##Par soucis de lisibilité, nous allons recoder les variables et leurs modalités.

#Exemple à faire pour TOUT - Faire un trie à plat pour chaque variable.

#Trie à plat pour la variable VRELIG 
hdv$VRELIG <- as.factor(hdv$VRELIG) #je transforme ma variable en facteur.

hdv$VRELIG <- fct_recode(hdv$VRELIG,
                              "Pratique régulière"="1",
                              "Pratique occasionnelle"="2",
                              "Sentiment d'appartenance sans pratique"="3",
                              "Ni pratique ni appartenance"="4",
                              "Rejet"="5",
                              "NSP"="6")

table(hdv$VRELIG)


#Trie à plat pour la variable VENREL

hdv$VENREL <- as.factor(hdv$VENREL) #je transforme ma variable en facteur.

hdv$VENREL <- fct_recode(hdv$VENREL,
                         "Important que ses enfants partagent la même croyance religieuse"="1",
                         "Pas d'importance si les enfants ne partagent pas la même croyance religieuse"="2",)

table(hdv$VENREL)

#Trie à plat pour la variable NIV2TE - Niveau d'étude de l'enquêté

hdv$NIVE2TE <- as.factor(hdv$NIVE2TE) #je transforme ma variable en facteur.

hdv$NIVE2TE <- fct_recode(hdv$NIVE2TE,
                         "N'a jamais fait d'études"="00",
                         "À arreté avant la dernière année de primaire"="03",
                         "Arret la dernière année de primaire"="04",
                         "1er cycle d'enseignement général"="15",
                         "2ème sycle d'enseignement général"="17",
                         "Enseignement technique ou professionnel court"="20", 
                         "Enseignement technique ou professionnel long"="30", 
                         "Enseignement supérieur y compris technique supérieur"="40")

table(hdv$NIVE2TE)

#Trie à plat pour la variable NIVE2TCJ - NIveau d'étude du conjoint

hdv$NIVE2TCJ <- as.factor(hdv$NIVE2TCJ) #je transforme ma variable en facteur.

hdv$NIVE2TCJ <- fct_recode(hdv$NIVE2TCJ,
                          "N'a jamais fait d'études"="00",
                          "À arreté avant la dernière année de primaire"="03",
                          "Arret la dernière année de primaire"="04",
                          "1er cycle d'enseignement général"="15",
                          "2ème sycle d'enseignement général"="17",
                          "Enseignement technique ou professionnel court"="20", 
                          "Enseignement technique ou professionnel long"="30", 
                          "Enseignement supérieur y compris technique supérieur"="40", 
                          "Ne sait pas"="99")

table(hdv$NIVE2TCJ)


#Trie à plat pour la variable STATUTE - Statut ou dernier statut professionnel de l'enquêté

hdv$STATUTE <- as.factor(hdv$STATUTE) #je transforme ma variable en facteur.

hdv$STATUTE <- fct_recode(hdv$STATUTE,
                          "Salarié de l'Etat"="1",
                          "Salarié d'une collectivité locale"="2",
                          "Salarié d'une entreprise publique ou nationale"="3",
                          "Salarié du secteur privé"="4",
                          "Salarié de son entreprise ou salarié de son conjoint"="5",
                          "Non salarié : aide ou aidait un membre de sa famille dans son travail sans être salarié"="6", 
                          "Non salarié : A son compte, indépendant, employeur, gérant, associé d'une société, co-exploitant"="7")

table(hdv$STATUTE)

#Trie à plat pour la variable STATUTCJ - Statut ou dernier statut professionnel du conjoint.

hdv$STATUTCJ <- as.factor(hdv$STATUTCJ) #je transforme ma variable en facteur.

hdv$STATUTCJ <- fct_recode(hdv$STATUTCJ,
                           "Salarié de l'Etat"="1",
                           "Salarié d'une collectivité locale"="2",
                           "Salarié d'une entreprise publique ou nationale"="3",
                           "Salarié du secteur privé"="4",
                           "Salarié de son entreprise ou salarié de son conjoint"="5",
                           "Non salarié : aide ou aidait un membre de sa famille dans son travail sans être salarié"="6", 
                           "Non salarié : A son compte, indépendant, employeur, gérant, associé d'une société, co-exploitant"="7",
                           "Ne sait pas"="9")

table(hdv$STATUTCJ)

#Trie à plat de la variable QUALIFE - Position professionnel de l'enquêté 

hdv$QUALIFE <- as.factor(hdv$QUALIFE) #je transforme ma variable en facteur.

hdv$QUALIFE <- fct_recode(hdv$QUALIFE,
                          "Manoeuvre ou ouvrier spécialisé"="1",
                          "Ouvrier qualifié ou technicien.ne d'atelier"="2",
                          "Technicien.ne non cadre"="3",
                          "Agent administratif ou commerciaux"="4",
                          "Ingénieur ou cadre"="5",
                          "Employé ou personnel"="6", 
                          "Autre"="7")

table(hdv$QUALIFE)

#Trie à plat de la variable QUALCJ - position professionnel du conjoint

hdv$QUALCJ <- as.factor(hdv$QUALCJ) #je transforme ma variable en facteur.

hdv$QUALCJ <- fct_recode(hdv$QUALCJ,
                          "Manoeuvre ou ouvrier spécialisé"="1",
                          "Ouvrier qualifié ou technicien.ne d'atelier"="2",
                          "Technicien.ne non cadre"="3",
                          "Agent administratif ou commerciaux"="4",
                          "Ingénieur ou cadre"="5",
                          "Employé ou personnel"="6", 
                          "Autre"="7",
                          "NSP"="9")

table(hdv$QUALCJ)

###Il faudrait ici combiner les QUAL avec les STATUT pour former de vraies catérogies professionnelles.


#Trie à plat des variables de bases : SEXEE (sexe enquêté), SEXECJ (sexe conjoint), AGEE (age enquêté), AGECJ (age conjoint).

#Sexe enquêté 
hdv$SEXEE <- as.factor(hdv$SEXEE) #je transforme ma variable en facteur.

hdv$SEXEE <- fct_recode(hdv$SEXEE,
                         "Homme"="1",
                         "Femme"="2",)

table(hdv$SEXEE)

#Sexe conjoint 
hdv$SEXECJ <- as.factor(hdv$SEXECJ) #je transforme ma variable en facteur.

hdv$SEXECJ <- fct_recode(hdv$SEXECJ,
                        "Homme"="1",
                        "Femme"="2",)

table(hdv$SEXECJ)


#age enquêté 

table(hdv$AGEE)

#age conjoint

table(hdv$AGECJ)







addmargins(table(hdv$VRELIG)) #Donne la somme

freq(hdv$VRELIG)# Valeurs en %


table(hdv$VRELIG,hdv$SEXEE)#Croiser 2 var
