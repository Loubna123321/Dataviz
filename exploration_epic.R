#Exploration de la base EPIC: 


plot_str(epic)

#Pour visualiser la distrubtion de nos variables quantitatives :

plot_histogram(epic$Age_individu) 

describe(epic)

summary(epic)


#GÉRER LES VALEURS MANQUANTES (NA):


#Connaitre le nombre de valeurs différentes dans chaque colonnes
sapply(epic, function(x) length(unique(x)))



#Compter le nombre de valeurs manquantes dans chacune des colonnes
sapply(epic,function(x) sum(is.na(x)))


#Dans cette base également on remarque que les valeurs manquantes de chaque colonne sont pour les individus qui sont non-concernés! 



plot_missing(epic) 

#---------------------------------------------------------------------------------------------------------------

addmargins(table(epic$Indic_relation_amour)) 
freq(epic$Indic_relation_amour)
#---------------------------------------------------------------------------------------------------------------


addmargins(table(epic$Importance_religion_enquete)) 

freq(epic$Importance_religion_enquete)

#---------------------------------------------------------------------------------------------------------------


addmargins(table(epic$Religion_conjoint)) 

freq(epic$Religion_conjoint)
#---------------------------------------------------------------------------------------------------------------


addmargins(table(epic$Religion_enquete)) 

freq(epic$Religion_enquete)

#---------------------------------------------------------------------------------------------------------------

addmargins(table(epic$Diplome_eleve_conjoint)) 

freq(epic$Diplome_eleve_conjoint)
#---------------------------------------------------------------------------------------------------------------


addmargins(table(epic$Diplome_eleve_enquete)) 

freq(epic$Diplome_eleve_enquete)

#---------------------------------------------------------------------------------------------------------------


addmargins(table(epic$Statut_profess_conjoint)) 

freq(epic$Statut_profess_conjoint)

#---------------------------------------------------------------------------------------------------------------


addmargins(table(epic$Statut_profess_enquete)) 

freq(epic$Statut_profess_enquete)

#---------------------------------------------------------------------------------------------------------------

addmargins(table(epic$Sexe_enquete)) 

freq(epic$Sexe_enquete)
#---------------------------------------------------------------------------------------------------------------


addmargins(table(epic$Sexe_conjoint)) 

freq(epic$Sexe_conjoint)
#---------------------------------------------------------------------------------------------------------------


addmargins(table(epic$Accepter_etre_avec_moins_diplome))
freq(epic$Accepter_etre_avec_moins_diplome)
#---------------------------------------------------------------------------------------------------------------


addmargins(table(epic$Accepter_etre_avec_plus_diplome)) 

freq(epic$Accepter_etre_avec_plus_diplome)

