
#Traitement et recodage de la base EPIC: 




#IndCOU: Indicateur de relation amoureuse importante lors de l'enquête
#R_IMPREL: Importance de la religion pour l'enquêté
#R_RELIGC :Religion du conjoint
#R_RELIGION:Religion de l'enquêté

#C_DIPLOMEC :Diplôme le plus élevé du conjoint
#M_DIPLOME :Diplôme le plus élevé de l'enquêté

#C_STATUTC :Statut professionnel du conjoint
#M_STATUT:Statut professionnel de l'enquêté

#SEXER  :Sexe de l'enquêté   
#H_SEXEC_C :Relation actuelle : Sexe du conjoint

#AGEM:Age de l'individu (en différence de millésime)
#H_ANAISC_C:Relation actuelle : Année de naissance du conjoint
#R_MOINDIPL:Accepter l'idée d'être avec quelqu'un de nettement moins diplômé
#R_PLUDIPL :Accepter l'idée d'être avec quelqu'un de nettement plus diplômé


#------------------------------------------------------------------------------------------


names(epic)[1]<-'Indic_relation_amour'
names(epic)[2]<-'Importance_religion_enquete'
names(epic)[3]<-'Religion_conjoint'
names(epic)[4]<-'Religion_enquete'
names(epic)[5]<-'Diplome_eleve_conjoint'
names(epic)[6]<-'Diplome_eleve_enquete'
names(epic)[7]<-'Statut_profess_conjoint'
names(epic)[8]<-'Statut_profess_enquete'
names(epic)[9]<-'Sexe_enquete'
names(epic)[10]<-'Sexe_conjoint'
names(epic)[11]<-'Age_individu'
names(epic)[12]<-'Annee_naissance_conjoint'
names(epic)[13]<-'Accepter_etre_avec_moins_diplome'
names(epic)[14]<-'Accepter_etre_avec_plus_diplome'

#------------------------------------------------------------------------------------------

#Trie à plat pour la variable Indic_relation_amour :

epic$Indic_relation_amour <- as.factor(epic$Indic_relation_amour) #je transforme ma variable en facteur.

epic$Indic_relation_amour <- fct_recode(epic$Indic_relation_amour,
                                        "Autre situation"="0",
                                        "relation amoureuse ou en couple "="1",)

table(epic$Indic_relation_amour)

#------------------------------------------------------------------------------------------


#Trie à plat pour la variable Indic_relation_amour :

epic$Importance_religion_enquete <- as.factor(epic$Importance_religion_enquete) #je transforme ma variable en facteur.

epic$Importance_religion_enquete <- fct_recode(epic$Importance_religion_enquete,
                                               "Importante ou assez importante"="1",
                                               "Peu importante "="2",
                                               "Sans importance"= "3",
                                               "Ne souhaite pas répondre"="8"
)

table(epic$Importance_religion_enquete)
#------------------------------------------------------------------------------------------




#Religion_conjoint:

#Trie à plat pour la variable Indic_relation_amour :

epic$Religion_conjoint <- as.factor(epic$Religion_conjoint) #je transforme ma variable en facteur.

epic$Religion_conjoint <- fct_recode(epic$Religion_conjoint,
                                     "Catholicisme"="1",
                                     "Protestantisme "="2",
                                     "Islam"= "3",
                                     "Bouddhisme"="4",
                                     "Hindouisme"="5",
                                     "Judaïsme"="6",
                                     "Autre"="7",
                                     "Sans religion"="8",
                                     "Ne souhaite pas répondre"="98",
                                     "Ne sait pas"="99"
)

table(epic$Religion_conjoint)


#------------------------------------------------------------------------------------------



#Trie à plat pour la variable Religion_enquete :

epic$Religion_enquete <- as.factor(epic$Religion_enquete) #je transforme ma variable en facteur.

epic$Religion_enquete <- fct_recode(epic$Religion_enquete,
                                    "Catholicisme"="1",
                                    "Protestantisme "="2",
                                    "Islam"= "3",
                                    "Bouddhisme"="4",
                                    "Hindouisme"="5",
                                    "Judaïsme"="6",
                                    "Autre"="7",
                                    "Sans religion"="8",
                                    "Ne souhaite pas répondre"="98"
                                    
)

table(epic$Religion_enquete)

#------------------------------------------------------------------------------------------



#Trie à plat pour la variable Diplome_eleve_conjoint :

epic$Diplome_eleve_conjoint <- as.factor(epic$Diplome_eleve_conjoint) #je transforme ma variable en facteur.

epic$Diplome_eleve_conjoint <- fct_recode(epic$Diplome_eleve_conjoint,
                                          "Aucun diplôme"="1",
                                          " primaires "="2",
                                          "Brevet collèges ou élémentaire"= "3",
                                          "CAP, BEP"="4",
                                          "Baccalauréat ou brevet supérieur"="5",
                                          "BTS, DEUG, DEUST,niveau bac+2,"="6",
                                          "Licence,bac+3, bac+4"="7",
                                          "Master2, doctorat de médecine, pharmacie, odontologie,diplôme grande école bac+5"="8",
                                          "Doctorat de recherche (hors santé)"="9",
                                          "Ne sait pas"="99"
                                          
)

table(epic$Diplome_eleve_conjoint)


#------------------------------------------------------------------------------------------




#Trie à plat pour la variable Diplome_eleve_enquete :

epic$Diplome_eleve_enquete <- as.factor(epic$Diplome_eleve_enquete) #je transforme ma variable en facteur.

epic$Diplome_eleve_enquete <- fct_recode(epic$Diplome_eleve_enquete,
                                         "Aucun diplôme"="1",
                                         " primaires "="2",
                                         "Brevet collèges ou élémentaire"= "3",
                                         "CAP, BEP"="4",
                                         "Baccalauréat ou brevet supérieur"="5",
                                         "BTS, DEUG, DEUST,niveau bac+2,"="6",
                                         "Licence,bac+3, bac+4"="7",
                                         "Master2, doctorat de médecine, pharmacie, odontologie,diplôme grande école bac+5"="8",
                                         "Doctorat de recherche (hors santé)"="9",
                                         "Ne sait pas"="99"
                                         
)

table(epic$Diplome_eleve_enquete)

#------------------------------------------------------------------------------------------


#Statut_profess_conjoint:
#Trie à plat pour la variable Diplome_eleve_enquete :

epic$Statut_profess_conjoint <- as.factor(epic$Statut_profess_conjoint) #je transforme ma variable en facteur.

epic$Statut_profess_conjoint <- fct_recode(epic$Statut_profess_conjoint,
                                           "Salarié(e) de l'État"="1",
                                           " Salarié(e) d'une collectivité territoriale "="2",
                                           "Salarié(e) des hôpitaux publics"= "3",
                                           "Salarié(e) d' entreprise, d'artisan, d'association"="4",
                                           "Salarié(e) d'un ou plusieurs particulier"="5",
                                           "aide un membre de famille dans son travail, sans être rémunéré"="6",
                                           "Chef d'entreprise salarié, PDG, gérant(e) minoritaire, associé(e)"="7",
                                           "Indépendant(e) ou à votre compte"="8",
                                           "Ne sait pas"="99"
)

table(epic$Statut_profess_conjoint)

#------------------------------------------------------------------------------------------



#Trie à plat pour la variable Statut_profess_enquete :

epic$Statut_profess_enquete <- as.factor(epic$Statut_profess_enquete) #je transforme ma variable en facteur.

epic$Statut_profess_enquete <- fct_recode(epic$Statut_profess_enquete,
                                          "Salarié(e) de l'État"="1",
                                          " Salarié(e) d'une collectivité territoriale "="2",
                                          "Salarié(e) des hôpitaux publics"= "3",
                                          "Salarié(e) d' entreprise, d'artisan, d'association"="4",
                                          "Salarié(e) d'un ou plusieurs particulier"="5",
                                          "aide un membre de famille dans son travail, sans être rémunéré"="6",
                                          "Chef d'entreprise salarié, PDG, gérant(e) minoritaire, associé(e)"="7",
                                          "Indépendant(e) ou à votre compte"="8",
                                          "Ne sait pas"="99"
)

table(epic$Statut_profess_enquete)

#------------------------------------------------------------------------------------------



#Trie à plat pour la variable Sexe_enquete :

epic$Sexe_enquete <- as.factor(epic$Sexe_enquete) #je transforme ma variable en facteur.

epic$Sexe_enquete <- fct_recode(epic$Sexe_enquete,
                                "Homme"="1",
                                "Femme"="2",
)

table(epic$Sexe_enquete)


#------------------------------------------------------------------------------------------



#Trie à plat pour la variable Sexe_conjoint :

epic$Sexe_conjoint <- as.factor(epic$Sexe_conjoint) #je transforme ma variable en facteur.

epic$Sexe_conjoint <- fct_recode(epic$Sexe_conjoint,
                                 "Homme"="1",
                                 "Femme"="2"
)

table(epic$Sexe_conjoint)

#------------------------------------------------------------------------------------------



#Trie à plat pour la variable Accepter_etre_avec_moins_diplome :

epic$Accepter_etre_avec_moins_diplome <- as.factor(epic$Accepter_etre_avec_moins_diplome) #je transforme ma variable en facteur.

epic$Accepter_etre_avec_moins_diplome <- fct_recode(epic$Accepter_etre_avec_moins_diplome,
                                                    "oui, c'est justement mon cas"="1",
                                                    "oui"="2",
                                                    "non"="3",
                                                    "Ne sait pas"="9"
)

table(epic$Accepter_etre_avec_moins_diplome)

#------------------------------------------------------------------------------------------





#Trie à plat pour la variable Accepter_etre_avec_plus_diplome :

epic$Accepter_etre_avec_plus_diplome <- as.factor(epic$Accepter_etre_avec_plus_diplome) #je transforme ma variable en facteur.

epic$Accepter_etre_avec_plus_diplome <- fct_recode(epic$Accepter_etre_avec_plus_diplome,
                                                   "oui, c'est justement mon cas"="1",
                                                   "oui"="2",
                                                   "non"="3",
                                                   "Ne sait pas"="9"
)

table(epic$Accepter_etre_avec_plus_diplome)
