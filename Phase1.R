# Contexte : 

#Dans le but de mieux appréhender les caractéristiques morphologiques de ses soldats,
#l’armée américaine a lancé en 2012 une étude morphologique concernant les hommes et les femmes
#engagés. Cette étude a porté sur un échantillon (non représentatif de la population de l’armée
#américaine) constitué de 4 082 hommes et 1 936 femmes sur lesquels 93 mesures morphologiques
#ont été enregistrées dont la taille, le poids, la longueur des jambes, la circonférence de la tête, etc.; 
#ainsi que 15 variables démographiques ou administratives dont le sexe et l'âge.


# Auteurs :
# DEMEBELE Salimata - DIALLO Penda Ahmadou - GBAYE Boukola - VIALAT Élisa


# ======================================================================#

# Phase1
# --- Chargement des données ---


# 1
readLines(con = '../Data/ANSUR II FEMALE Public.csv',
          n = 10)

dataset1 = read.table(file = '../Data/ANSUR II FEMALE Public.csv',
                      sep = ",",
                      header = TRUE)

# Caractéristiques des fichiers
str(dataset1)

readLines(con = '../Data/ANSUR II MALE Public.csv',
          n = 10)

dataset2 = read.table(file = '../Data/ANSUR II MALE Public.csv',
                      sep = ",",
                      header = TRUE)
# Caractéristiques des fichiers
str(dataset2)


# 2
#Fusion verticale des deux fichiers
# Faire en sorte que les noms des colonnes soient les mêmes
colnames(dataset1) = colnames(dataset2)

dataset = rbind(dataset1, dataset2)


# 3
workdataset = subset(dataset,
                     select = c(Gender, Age, Weightlbs, Heightin,
                                handcircumference, neckcircumference, 
                                shoulderlength, wristcircumference, waistcircumference,
                                anklecircumference, chestcircumference,
                                footlength, headcircumference, thighcircumference,
                                tibialheight
                     ))

# 4
# Le poids est en livres 1kg = 2.20462livres
# La taille en pouces : 1pouce = 0.0254m
within(workdataset,
       {
         Height = Heightin *0.0254 ;
         Weight = Weightlbs/2.20462 ;
         Heightin = NULL;
         Weightlbs = NULL;
       }) -> workdataset

# 5 Correction du type des variables
# La seule variable peut-être problématique est Gender
str(workdataset)
within(workdataset,
       {
         Gender = factor(Gender)
       })->workdataset

str(workdataset)
saveRDS(workdataset, file = "../Data/ANSUR_II_SD1.rds")

