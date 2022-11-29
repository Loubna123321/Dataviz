
#Exploration de la base HDV: 


plot_str(hdv)




#Pour visualiser la distrubtion de nos variables quantitatives :

plot_histogram(hdv) 

#On remarque que l'age le plus frequent des gens interrogés est entre 50 et 60 ans
#alors que pour l'age du conjoint il est entre 45 et 55



summary(hdv)

#GÉRER LES VALEURS MANQUANTES (NA):


#Connaitre les differents modalités de chaque colonne:
sapply(hdv, function(x) length(unique(x)))


#Compter le nombre de valeurs manquantes dans chacune des colonnes
sapply(hdv,function(x) sum(is.na(x)))




 
plot_missing(hdv) 
#---------------------------------------------------------------------------------------------------------------


addmargins(table(hdv$Rapport_religion)) #Donne la somme

freq(hdv$Rapport_religion)


#Sentiment d'appartenance sans pratique est la plus frequente, en plus on voit que les deux modalités Pratique occasionnelle et Ni pratique ni appartenance ont presque la meme frequence 


#---------------------------------------------------------------------------------------------------------------

addmargins(table(hdv$religion_enfant)) 

freq(hdv$religion_enfant)


#Pas d'importance si les enfants ne partagent pas la meme croyance religieuse est la modalité la plus representée dans cette colonne

#---------------------------------------------------------------------------------------------------------------


addmargins(table(hdv$Niveau_etude_enquete)) 

freq(hdv$Niveau_etude_enquete)


#La modalité Enseignement technique ou professionnel court est la plus representée parmi les autres, ensuite, on a la modalité Enseignement superieur y compris technique superieur, la modalité (n'a jamais fait d'études est tres petite) 

#---------------------------------------------------------------------------------------------------------------



addmargins(table(hdv$Niveau_etude_conjoint))

freq(hdv$Niveau_etude_conjoint)

#La modalité(Enseignement technique ou professionnel court) est la plus fréquente dans cette colonne
#en suite, on a la modalité (Enseignement superieur y compris technique superieur) qui vienne en deuxime lieu 


#---------------------------------------------------------------------------------------------------------------



addmargins(table(hdv$Statut_profess_enquete))

freq(hdv$Statut_profess_enquete)



#La categorie avec 4692 individu est (Salarie du secteur prive), alors que les autres ctegories sont tres petites!

#---------------------------------------------------------------------------------------------------------------


addmargins(table(hdv$Statut_profess_conjoint))

freq(hdv$Statut_profess_conjoint)


#Ici également on a le secteur privé qui est dominant comme status professionel du conjoint 

#---------------------------------------------------------------------------------------------------------------


addmargins(table(hdv$Position_profess_enquete)) 

freq(hdv$Position_profess_enquete)

# Nous avons la categorie la plus frequente pour la variable Position professionnel de l'enquete est(Employe ou personnel)

#---------------------------------------------------------------------------------------------------------------




addmargins(table(hdv$Sexe_enquete)) 

freq(hdv$Sexe_enquete)



#Les femmes sont la categorie la plus interrogée avec Hommes = 3787 et Femmes = 4616 

#---------------------------------------------------------------------------------------------------------------

addmargins(table(hdv$Sexe_conjoint))

freq(hdv$Sexe_conjoint)

#Les deux categories sont presque égales avec Hommes = 3119 et Femmes = 2851  


#---------------------------------------------------------------------------------------------------------------



#plot(hdv$Vie_en_couple, col=1:2)

addmargins(table(hdv$Vie_en_couple))

freq(hdv$Vie_en_couple)

#Les personnes interrogées vivant en couple sont plus frequentes que les personnes qui ne sont pas en couple


#---------------------------------------------------------------------------------------------------------------


addmargins(table(hdv$Rapport_religion)) 

freq(hdv$Rapport_religion)

table(hdv$Rapport_religion,hdv$Sexe_enquete)#Croiser 2 var
