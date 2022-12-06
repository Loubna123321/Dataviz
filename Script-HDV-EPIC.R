library(foreign)
library(questionr)
library(survey)
library(R2HTML)
library(tidyverse)
library(esquisse)
"install.packages('ggplot2,...)"


##La base de données HDV étant en 4 parties, 
##nous avons ouvert la premiere et la quatrieme...
hdv1<-read.csv("hdv1.csv",sep = ";")
hdv4<-read.csv("hdv4.csv",sep = ";")

##...pour récupérer les variables qui nous interressent.
hdv <- cbind(select(hdv1, c(AGEE,AGECJ)), select(hdv4, c(STATUTE,STATUTCJ,NIVE2TE,NIVE2TCJ,QUALIFE,QUALCJ,SEXEE,SEXECJ,VRELIG,VENREL,VIECOUE)))

## Puis nous nettoyons notre environnement
rm(hdv1,hdv4)

## Pour EPIC, nous avons ouvert la base de données EPIC pour y recuperer ce qu'il nous faut :
epic <- select(read.csv("repondant.txt", sep="\t"),c(IndCOU,R_IMPREL,R_RELIGC,R_RELIGION,C_DIPLOMEC,M_DIPLOME,C_STATUTC,M_STATUT,SEXER,H_SEXEC_C,AGEM,H_ANAISC_C,R_MOINDIPL,R_PLUDIPL))


##recodage du varible M_DIPLOME diplome le plus élévé de l'enqueté
epic$M_DIPLOME <- as.factor(epic$M_DIPLOME,
                            "aucun diplome"="1",
                            "CEP (certificat d'études primaires) ou diplôme étranger de même niveau"="2",
                            "Brevet des colleges,BEPC,brevet élémentaire,DNB ou diplôme étrangers de meme niveau"="3",
                            "CAP, BEP ou diplôme de ce niveau"="4",
                            "Baccalauréat(technologique ou professionnel ou général),brevet supérieur,capacité en droit,DAEU, ou dipôme de meme niveau"="5",
                            "BTS, DEUG, DEUST, diplôme des professions sociales, diplôme de santé de niveau bac +2, autres diplôme équivalent"="6",
                            "licence, licence pro,diplôme d'infirmiere depuis 2012, maîtrise,autre diplome de niveau équivalent bac+3,bac+4"="7",
                            "Master2,DEA,DESS,diplôme de grande école de niveau bac+5(ingénieur,commerce),doctorat de médecine,pharmacie,odontologie,autre diplôme de niveau équivalent"="8",
                            "Doctorat de recherche (hors santé)"="9", 
                            "NSP"="99")

table(epic$M_DIPLOME)                            
##recodage du variable C_DIPLOME, diplôme le plus élevé du conjoint
epic$C_DIPLOME <- as.factor(epic$C_DIPLOME,
                            "aucun diplome"="1",
                            "CEP (certificat d'études primaires) ou diplôme étranger de même niveau"="2",
                            "Brevet des colleges,BEPC,brevet élémentaire,DNB ou diplôme étrangers de meme niveau"="3",
                            "CAP, BEP ou diplôme de ce niveau"="4",
                            "Baccalauréat(technologique ou professionnel ou général),brevet supérieur,capacité en droit,DAEU, ou dipôme de meme niveau"="5",
                            "BTS, DEUG, DEUST, diplôme des professions sociales, diplôme de santé de niveau bac +2, autres diplôme équivalent"="6",
                            "licence, licence pro,diplôme d'infirmiere depuis 2012, maîtrise,autre diplome de niveau équivalent bac+3,bac+4"="7",
                            "Master2,DEA,DESS,diplôme de grande école de niveau bac+5(ingénieur,commerce),doctorat de médecine,pharmacie,odontologie,autre diplôme de niveau équivalent"="8",
                            "Doctorat de recherche (hors santé)"="9", 
                            "NSP"="99")
                            
table(epic$C_DIPLOME)
##je vais croisée la variable religiosité davec le M_DIPLOME pour voir si la réligion de l'enquêté à un rapport avec son niveau de diplome 
##tableau trie a plat 
table(epic$C_DIPLOME,epic$Religiositere)
table(epic$M_DIPLOME,epic$Religiositere)

tab<-table(epic$M_DIPLOME,epic$Religiositere)
lprop(tab)
mosaicplot(tab)
##le tableau nous montre que les individue qui croient au catholisisme semble avoir le Brevet des colleges,BEPC,brevet élémentaire,DNB ou diplôme étrangers de meme niveau comme diplôme le plus élevé (64.8).
