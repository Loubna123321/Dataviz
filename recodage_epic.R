
#Traitement et recodage de la base EPIC: 

#IndCOU: Indicateur de relation amoureuse importante lors de l'enquête
#R_IMPREL: Importance de la religion pour l'enquêté
#R_RELIGC: Religion du conjoint
#R_RELIGION: Religion de l'enquêté
#Variable construite: Religiosité

#C_CS13C: Catégorie socioprofessionnelle du conjoint
#M_CS13: Catégorie socioprofessionnelle de l'enquêté 

#SEXER: Sexe de l'enquêté   
#H_SEXEC_C: Relation actuelle : Sexe du conjoint

#AGEM: Age de l'individu (en différence de millésime)
#H_ANAISC_C: Relation actuelle : Année de naissance du conjoint
#R_MOINDIPL: Accepter l'idée d'être avec quelqu'un de nettement moins diplômé
#R_PLUDIPL: Accepter l'idée d'être avec quelqu'un de nettement plus diplômé


#------------------------------------------------------------------------------------------

#Renomer les variables :

names(epic)<-c('Indic_relation_amour',
               'Importance_religion_enquete',
               'Religion_conjoint',
               'Religion_enquete',
               'Diplome_eleve_conjoint',
               'Diplome_eleve_enquete',
               'CSP_conjoint',
               'CSP_enquete',
               'Sexe_enquete',
               'Sexe_conjoint',
               'Age_individu',
               'Annee_naissance_conjoint',
               'Accepter_etre_avec_moins_diplome',
               'Accepter_etre_avec_plus_diplome')

#------------------------------------------------------------------------------------------

#Traitement de la variable Indic_relation_amour :

epic$Indic_relation_amour <- as.factor(epic$Indic_relation_amour) #je transforme ma variable en facteur.

epic$Indic_relation_amour <- fct_recode(epic$Indic_relation_amour,
                                        "Autre situation"="0",
                                        "relation amoureuse ou en couple"="1")

#------------------------------------------------------------------------------------------

#Traitement de la variable Importance_religion_enquete :

epic$Importance_religion_enquete <- as.factor(epic$Importance_religion_enquete) #je transforme ma variable en facteur.

epic$Importance_religion_enquete <- fct_recode(epic$Importance_religion_enquete,
                                               "Importante ou assez importante"="1",
                                               "Peu importante"="2",
                                               "Sans importance"= "3",
                                               "Ne souhaite pas répondre"="8")

#------------------------------------------------------------------------------------------

#Traitement de la variable Indic_relation_amour :

epic$Religion_conjoint <- as.factor(epic$Religion_conjoint) #je transforme ma variable en facteur.

epic$Religion_conjoint <- fct_recode(epic$Religion_conjoint,
                                     "Catholicisme"="1",
                                     "Protestantisme"="2",
                                     "Islam"= "3",
                                     "Bouddhisme"="4",
                                     "Hindouisme"="5",
                                     "Judaïsme"="6",
                                     "Autre"="7",
                                     "Sans religion"="8",
                                     "Ne souhaite pas répondre"="98",
                                     "Ne sait pas"="99")

#------------------------------------------------------------------------------------------

#Traitement de la variable Religion_enquete :

epic$Religion_enquete <- as.factor(epic$Religion_enquete) #je transforme ma variable en facteur.

epic$Religion_enquete <- fct_recode(epic$Religion_enquete,
                                    "Catholicisme"="1",
                                    "Protestantisme"="2",
                                    "Islam"= "3",
                                    "Bouddhisme"="4",
                                    "Hindouisme"="5",
                                    "Judaïsme"="6",
                                    "Autre"="7",
                                    "Sans religion"="8",
                                    "Ne souhaite pas répondre"="98")

#------------------------------------------------------------------------------------------

#Traitement de la variable Diplome_eleve_conjoint :

epic$Diplome_eleve_conjoint <- as.factor(epic$Diplome_eleve_conjoint) #je transforme ma variable en facteur.

epic$Diplome_eleve_conjoint <- fct_recode(epic$Diplome_eleve_conjoint,
                                          "Aucun diplôme"="1",
                                          "Primaires"="2",
                                          "Brevet collèges ou élémentaire"= "3",
                                          "CAP, BEP"="4",
                                          "Baccalauréat ou brevet supérieur"="5",
                                          "BTS, DEUG, DEUST,niveau bac+2"="6",
                                          "Licence,bac+3, bac+4"="7",
                                          "Master2, doctorat de médecine, pharmacie, odontologie,diplôme grande école bac+5"="8",
                                          "Doctorat de recherche (hors santé)"="9",
                                          "Ne sait pas"="99")

#------------------------------------------------------------------------------------------

#Traitement de la variable Diplome_eleve_enquete :

epic$Diplome_eleve_enquete <- as.factor(epic$Diplome_eleve_enquete) #je transforme ma variable en facteur.

epic$Diplome_eleve_enquete <- fct_recode(epic$Diplome_eleve_enquete,
                                         "Aucun diplôme"="1",
                                         "Primaires"="2",
                                         "Brevet collèges ou élémentaire"= "3",
                                         "CAP, BEP"="4",
                                         "Baccalauréat ou brevet supérieur"="5",
                                         "BTS, DEUG, DEUST,niveau bac+2"="6",
                                         "Licence,bac+3, bac+4"="7",
                                         "Master2, doctorat de médecine, pharmacie, odontologie,diplôme grande école bac+5"="8",
                                         "Doctorat de recherche (hors santé)"="9",
                                         "Ne sait pas"="99")

#------------------------------------------------------------------------------------------

#Traitement de la variable CSP_conjoint :

epic$CSP_conjoint <- as.factor(epic$CSP_conjoint)

epic$CSP_conjoint <- fct_recode(epic$CSP_conjoint,
                                "Agriculteurs"="1",
                                "Artisans, commerçants, chefs d'entreprise"="2",
                                "Cadres des entreprises"="3",
                                "Professions intellectuelles, cadres superieurs du public, professions liberales"="4",
                                "Professions intermediaires de l'enseignement du public"="5",
                                "Professions intermediaires de la sante et du travail social"="6",
                                "Professions intermediaires des entreprises"="7",
                                "Employes bureau et secteur public"="8",
                                "Employes commerce et services"="9",
                                "Ouvriers qualifies"="10",
                                "Ouvriers non qualifies"="11",
                                "Au foyer"="12",
                                "En etudes"="13",
                                "Ne sait pas"="99")

#------------------------------------------------------------------------------------------

#Traitement de la variable CSP_enquete :

epic$CSP_enquete <- as.factor(epic$CSP_enquete) #je transforme ma variable en facteur.

epic$CSP_enquete <- fct_recode(epic$CSP_enquete,
                               "Agriculteurs"="1",
                               "Artisans, commerçants, chefs d'entreprise"="2",
                               "Cadres des entreprises"="3",
                               "Professions intellectuelles, cadres superieurs du public, professions liberales"="4",
                               "Professions intermediaires de l'enseignement du public"="5",
                               "Professions intermediaires de la sante et du travail social"="6",
                               "Professions intermediaires des entreprises"="7",
                               "Employes bureau et secteur public"="8",
                               "Employes commerce et services"="9",
                               "Ouvriers qualifies"="10",
                               "Ouvriers non qualifies"="11",
                               "Au foyer"="12",
                               "En etudes"="13",
                               "Ne sait pas"="99")

#------------------------------------------------------------------------------------------

#Traitement de la variable Sexe_enquete :

epic$Sexe_enquete <- as.factor(epic$Sexe_enquete) #je transforme ma variable en facteur.

epic$Sexe_enquete <- fct_recode(epic$Sexe_enquete,
                                "Homme"="1",
                                "Femme"="2")

#------------------------------------------------------------------------------------------

#Traitement de la variable Sexe_conjoint :

epic$Sexe_conjoint <- as.factor(epic$Sexe_conjoint) #je transforme ma variable en facteur.

epic$Sexe_conjoint <- fct_recode(epic$Sexe_conjoint,
                                 "Homme"="1",
                                 "Femme"="2")

#------------------------------------------------------------------------------------------

#Traitement de la variable Accepter_etre_avec_moins_diplome :

epic$Accepter_etre_avec_moins_diplome <- as.factor(epic$Accepter_etre_avec_moins_diplome) #je transforme ma variable en facteur.

epic$Accepter_etre_avec_moins_diplome <- fct_recode(epic$Accepter_etre_avec_moins_diplome,
                                                    "oui, c'est justement mon cas"="1",
                                                    "oui"="2",
                                                    "non"="3",
                                                    "Ne sait pas"="9")

#------------------------------------------------------------------------------------------

#Traitement de la variable Accepter_etre_avec_plus_diplome :

epic$Accepter_etre_avec_plus_diplome <- as.factor(epic$Accepter_etre_avec_plus_diplome) #je transforme ma variable en facteur.

epic$Accepter_etre_avec_plus_diplome <- fct_recode(epic$Accepter_etre_avec_plus_diplome,
                                                   "oui, c'est justement mon cas"="1",
                                                   "oui"="2",
                                                   "non"="3",
                                                   "Ne sait pas"="9")
