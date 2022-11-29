
#Traitement et recodage de la base HDV2003: 

#Notre base HD2003 contient 13 variables qui sont les suivantes :


#1)La variable AGEE: Age de la personne interrogée
#2)La variable AGECJ: Age du conjoint
#3)La variable STATUTE : Statut (ou dernier statut) de la personne interrogée 
#4)La variable STATUTCJ: Statut (ou dernier statut) du conjoint 
#5)La variable NIVE2TE: Niveau d'études atteint par la personne interrogée 
#6)La variable NIVE2TCJ: Niveau d'études atteint par le conjoint 
#7)La variable QUALIFE: Position professionnelle de l'emploi (ou dernier emploi) de la personne interrogée
#8)La variable QUALCJ : Position professionnelle de l'emploi (ou dernier emploi) du conjoint 
#9)La variable SEXEE: Sexe de l'interogé : 1 pour Homme et 2 pour Femme
#10)La variable SEXECJ: sexe du conjoint : 1 pour Homme et 2 pour Femme
#11)La variable VRELIG: Aujourd'hui, diriez-vous que par rapport à la religion, vous avez :
#12)La variable VENREL: Est-il important pour vous que vos enfants partagent vos croyances religieuses ou votre position par rapport à la religion ? 1:oui   2:non
#13)La variable VIECOU: vie en couple  1 pour oui et 2 pour non



#------------------------------------------------------------------------------------------


#Renommer les variables pour plus de lisibilité : 
names(hdv)[1]<-'Age_enquete'
names(hdv)[2]<-'Age_conjoint'
names(hdv)[3]<-'Statut_profess_enquete'
names(hdv)[4]<-'Statut_profess_conjoint'
names(hdv)[5]<-'Niveau_etude_enquete'
names(hdv)[6]<-'Niveau_etude_conjoint'
names(hdv)[7]<-'Position_profess_enquete'
names(hdv)[8]<-'Position_profess_conjoint'
names(hdv)[9]<-'Sexe_enquete'
names(hdv)[10]<-'Sexe_conjoint'
names(hdv)[11]<-'Rapport_religion'
names(hdv)[12]<-'religion_enfant'
names(hdv)[13]<-'Vie_en_couple'



##Vu que ces valeurs manquantes sont toutes  pour les cas non-concernés, alors on ne peut rien faire puisque remplacer les valeurs manquantes des cas non-concernés n'a pas de sens, on ne peut non plus les supprimer! 

##Par soucis de lisibilité, nous allons recoder les variables et leurs modalités.

#Exemple à faire pour TOUT - Faire un trie à plat pour chaque variable.


#------------------------------------------------------------------------------------------


#Trie à plat pour la variable Rapport_religion :

hdv$Rapport_religion <- as.factor(hdv$Rapport_religion) #je transforme ma variable en facteur.

hdv$Rapport_religion <- fct_recode(hdv$Rapport_religion,
                                   "Pratique réguliere"="1",
                                   "Pratique occasionnelle"="2",
                                   "Sentiment d'appartenance sans pratique"="3",
                                   "Ni pratique ni appartenance"="4",
                                   "Rejet"="5",
                                   "NSP"="6")

table(hdv$Rapport_religion)

#------------------------------------------------------------------------------------------


#Trie à plat pour la variable religion_enfant

hdv$religion_enfant <- as.factor(hdv$religion_enfant) 

hdv$religion_enfant <- fct_recode(hdv$religion_enfant,
                                  "Important que ses enfants partagent la meme croyance religieuse"="1",
                                  "Pas d'importance si les enfants ne partagent pas la meme croyance religieuse"="2",)

table(hdv$religion_enfant)

#------------------------------------------------------------------------------------------


#Trie à plat pour la variable Niveau_etude_enquete - Niveau d'etude de l'enquete:

hdv$Niveau_etude_enquete <- as.factor(hdv$Niveau_etude_enquete) #je transforme ma variable en facteur.

hdv$Niveau_etude_enquete <- fct_recode(hdv$Niveau_etude_enquete,
                                       "N'a jamais fait d'etudes"="00",
                                       "a arrete avant la derniere annee de primaire"="03",
                                       "Arret la derniere annee de primaire"="04",
                                       "1er cycle d'enseignement general"="15",
                                       "2eme sycle d'enseignement general"="17",
                                       "Enseignement technique ou professionnel court"="20", 
                                       "Enseignement technique ou professionnel long"="30", 
                                       "Enseignement superieur y compris technique superieur"="40")

table(hdv$Niveau_etude_enquete)
#------------------------------------------------------------------------------------------



#Trie à plat pour la variable Niveau_etude_conjoint - Niveau d'etude du conjoint
hdv$Niveau_etude_conjoint <- as.factor(hdv$Niveau_etude_conjoint) #je transforme ma variable en facteur.

hdv$Niveau_etude_conjoint <- fct_recode(hdv$Niveau_etude_conjoint,
                                        "N'a jamais fait d'etudes"="00",
                                        "a arrete avant la derniere annee de primaire"="03",
                                        "Arret la derniere annee de primaire"="04",
                                        "1er cycle d'enseignement general"="15",
                                        "2eme sycle d'enseignement general"="17",
                                        "Enseignement technique ou professionnel court"="20", 
                                        "Enseignement technique ou professionnel long"="30", 
                                        "Enseignement superieur y compris technique superieur"="40", 
                                        "Ne sait pas"="99")

table(hdv$Niveau_etude_conjoint)
#------------------------------------------------------------------------------------------




#Trie à plat pour la variable Statut_profess_enquete - Statut ou dernier statut professionnel de l'enquete
hdv$Statut_profess_enquete <- as.factor(hdv$Statut_profess_enquete) #je transforme ma variable en facteur.

hdv$Statut_profess_enquete <- fct_recode(hdv$Statut_profess_enquete,
                                         "Salarie de l'Etat"="1",
                                         "Salarie d'une collectivite locale"="2",
                                         "Salarie d'une entreprise publique ou nationale"="3",
                                         "Salarie du secteur prive"="4",
                                         "Salarie de son entreprise ou salarie de son conjoint"="5",
                                         "Non salarie : aide ou aidait un membre de sa famille dans son travail sans etre salarie"="6", 
                                         "Non salarie : A son compte, independant, employeur, gerant, associe d'une sociate, co-exploitant"="7")

table(hdv$Statut_profess_enquete)


#------------------------------------------------------------------------------------------


#Trie à plat pour la variable Statut_profess_conjoint - Statut ou dernier statut professionnel du conjoint.
hdv$Statut_profess_conjoint <- as.factor(hdv$Statut_profess_conjoint) #je transforme ma variable en facteur.

hdv$Statut_profess_conjoint <- fct_recode(hdv$Statut_profess_conjoint,
                                          "Salarie de l'Etat"="1",
                                          "Salarie d'une collectivite locale"="2",
                                          "Salarie d'une entreprise publique ou nationale"="3",
                                          "Salarie du secteur prive"="4",
                                          "Salarie de son entreprise ou salarie de son conjoint"="5",
                                          "Non salarie : aide ou aidait un membre de sa famille dans son travail sans etre salarie"="6", 
                                          "Non salarie:A son compte, independant, employeur, gerant, associe d'une societe,co-exploitant"="7",
                                          "Ne sait pas"="9")

table(hdv$Statut_profess_conjoint)
#------------------------------------------------------------------------------------------


#Trie a plat de la variable Position_profess_enquete - Position professionnel de l'enquete 
hdv$Position_profess_enquete <- as.factor(hdv$Position_profess_enquete) #je transforme ma variable en facteur.

hdv$Position_profess_enquete <- fct_recode(hdv$Position_profess_enquete,
                                           "Manoeuvre ou ouvrier specialise"="1",
                                           "Ouvrier qualifie ou technicien.ne d'atelier"="2",
                                           "Technicien.ne non cadre"="3",
                                           "Agent administratif ou commerciaux"="4",
                                           "Ingenieur ou cadre"="5",
                                           "Employe ou personnel"="6", 
                                           "Autre"="7")

table(hdv$Position_profess_enquete)

#------------------------------------------------------------------------------------------


#Trie a plat de la variable Position_profess_conjoint - Position professionnel du conjoint: 
hdv$Position_profess_conjoint <- as.factor(hdv$Position_profess_conjoint) #je transforme ma variable en facteur.

hdv$Position_profess_conjoint <- fct_recode(hdv$Position_profess_conjoint,
                                           "Manoeuvre ou ouvrier specialise"="1",
                                           "Ouvrier qualifie ou technicien.ne d'atelier"="2",
                                           "Technicien.ne non cadre"="3",
                                           "Agent administratif ou commerciaux"="4",
                                           "Ingenieur ou cadre"="5",
                                           "Employe ou personnel"="6", 
                                           "Autre"="7",
                                           "Ne sait pas"= "9")

table(hdv$Position_profess_conjoint)


#------------------------------------------------------------------------------------------
###Il faudrait ici combiner les QUAL avec les STATUT pour former de vraies caterogies professionnelles.


#Trie a plat des variables de bases : SEXEE (sexe enquete), SEXECJ (sexe conjoint), AGEE (age enquete), AGECJ (age conjoint).
#Sexe enquete
hdv$Sexe_enquete <- as.factor(hdv$Sexe_enquete) #je transforme ma variable en facteur.

hdv$Sexe_enquete <- fct_recode(hdv$Sexe_enquete,
                               "Homme"="1",
                               "Femme"="2",)

table(hdv$Sexe_enquete)


#------------------------------------------------------------------------------------------


#Sexe conjoint 
hdv$Sexe_conjoint <- as.factor(hdv$Sexe_conjoint) #je transforme ma variable en facteur.

hdv$Sexe_conjoint <- fct_recode(hdv$Sexe_conjoint,
                                "Homme"="1",
                                "Femme"="2",)

table(hdv$Sexe_conjoint)

#------------------------------------------------------------------------------------------



#Vie en couple:
hdv$Vie_en_couple <- as.factor(hdv$Vie_en_couple) #je transforme ma variable en facteur.

hdv$Vie_en_couple <- fct_recode(hdv$Vie_en_couple,
                                "oui"="1",
                                "non"="2",)

table(hdv$Vie_en_couple)

