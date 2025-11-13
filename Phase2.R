# Auteurs :
# DEMEBELE Salimata - DIALLO Penda Ahmadou - GBAYE Boukola - VIALAT Élisa



# Phase 2 : 

# Recherche des trois variables morphologiques, classées par ordre décroissant d’importance,
# les plus associées à la taille d’un soldat de l’US Army

# Recherche des trois variables morphologiques, classées par ordre décroissant d’importance,
# les plus associées au poids d’un soldat de l’US Army

# Les variables d'intérêt sont : le poids (Weight) et la taille (Height)

# ---- Chargement des données ----

workdata = readRDS(file = "../Data/ANSUR_II_SD1.rds")


# ---- Traitement des données ----

# ............................................................................... #
# ... Prémilinaires ... #

# Sélection des individus ayant un âge situé entre 18 et 40 ans compris.

workset = subset(workdata,
                 subset = (Age>=18) & (Age<=40))

# Création de tableaux contenant la liste des variables et des sexes, et leurs traductions en français

lab_min = c("Gender" = "genre", 
            "Age" = "âge", 
            "Weight" = "poids", 
            "Height" = "taille",
            "handcircumference" = "circonférence de la main", 
            "neckcircumference" = "circonférence du cou", 
            "shoulderlength" = "longueur d'épaule", 
            "wristcircumference" = "circonférence du poignet", 
            "waistcircumference" = "circonférence de la taille",
            "anklecircumference" = "circonférence de la cheville", 
            "chestcircumference" = "circonférence de la poitrine",
            "footlength" = "longueur du pied", 
            "headcircumference" = "circonférence de la tête", 
            "thighcircumference" = "circonférence de la cuisse",
            "tibialheight" = "hauteur tibiale", 
            "Female" = "femmes", 
            "Male" = "hommes")

lab_max = c("Gender" = "Genre", 
            "Age" = "Âge", 
            "Weight" = "Poids", 
            "Height" = "Taille",
            "handcircumference" = "Circonférence de la main", 
            "neckcircumference" = "Circonférence du cou", 
            "shoulderlength" = "Longueur d'épaule", 
            "wristcircumference" = "Circonférence du poignet", 
            "waistcircumference" = "Circonférence de la taille",
            "anklecircumference" = "Circonférence de la cheville", 
            "chestcircumference" = "Circonférence de la poitrine",
            "footlength" = "Longueur du pied", 
            "headcircumference" = "Circonférence de la tête", 
            "thighcircumference" = "Circonférence de la cuisse",
            "tibialheight" = "Hauteur tibiale", 
            "Female" = "Femmes", 
            "Male" = "Hommes")

# Récupération des variables quantitatives

variables_quanti <- names(workset)[!names(workset) %in% c("Gender")]
print(variables_quanti)



# ............................................................................... #
# ... Nuages de dispersion et courbes de régression ... #

# Fonction pour tracer le nuage de dispersion et la courbe de régression lissée

nuage_reg = function(varx, vary, sex){
  
  # varx : variable explicative
  
  # vary : variable à expliquer
  
  # sex : sexe en question
  
  # Sélection des données concernant le sexe : sex
  workset_g = subset(workset,
                     subset = Gender == sex)
  
  Loess = loess(workset_g[[vary]] ~ workset_g[[varx]],
                span = 0.4)
  
  temp = data.frame(workset_g[, c(vary,varx)],
                    Loess = fitted(Loess))
  
  temp = temp[order(temp[[varx]]),]
  
  unitex = ifelse(varx=="Age","année", ifelse(varx=="Height", "mètre", ifelse(varx=="Weight", "kilogramme", "millimètre"))) 
  
  unitey = ifelse(vary=="Age","année", ifelse(vary=="Height", "mètre", ifelse(vary=="Weight", "kilogramme", "millimètre"))) 
  
  plot(workset_g[[vary]] ~ workset_g[[varx]],
       pch = 20,
       bg = "blue2",
       col = "blue2",
       las = 1,
       main = paste(lab_max[vary],"versus", lab_min[varx], "chez les", lab_min[sex], sep = " "),
       xlab = paste(lab_max[varx], " (exprimé en ", unitex, ")", sep=""),
       ylab = paste(lab_max[vary], " (exprimé en ", unitey, ")", sep=""))
  
  lines(temp$Loess ~ temp[[varx]],
        col = "red2",
        lwd = 2)
  
  legend(x = "topleft",
         bty = "n",
         lty = 1,
         lwd = 3,
         col = "red",
         legend = "Régression lissée")
  
  mtext("BUT 1 - Science des Données - Lisieux", side = 1, adj = 0, line = 4, cex = 0.8, col = "purple")
  mtext(expression(italic("Source: ANSUR II")), side = 1, adj = 1, line = 4, cex = 0.8, col = "orange2")
}


par(mfrow = c(1,1))

# ________________ Poids ____________________#

# Récupération des variables quantitatives autres que le poids :
variables_quantip <- names(workset)[!names(workset) %in% c("Gender","Weight")]
print(variables_quantip)

# Chez les femmes

lapply(X = variables_quantip ,
       function(x) nuage_reg(x, "Weight", "Female"))


# Chez les hommes

lapply(X = variables_quantip ,
       function(x) nuage_reg(x, "Weight", "Male"))


# ________________ Taille ____________________#

# Récupération des variables quantitatives autres que le poids :
variables_quantit <- names(workset)[!names(workset) %in% c("Gender","Height")]
print(variables_quantit)

# Chez les femmes

lapply(X = variables_quantit ,
       function(x) nuage_reg(x, "Height", "Female"))


# Chez les hommes

lapply(X = variables_quantit ,
       function(x) nuage_reg(x, "Height", "Male"))



# ............................................................................................................. #
# ... Modélisation linéaire de l’association entre chacune des variables morphologiques et les deux variables cibles ... #

# Fonction pour réaliser la modélisation linéaire

model_reg = function(varx, vary, sex){
  workset_g = subset(workset,
                     subset = Gender == sex)
  model = lm(workset_g[[vary]] ~ workset_g[[varx]])
  
  cat("\n")
  
  print(paste(lab_max[vary],"versus", lab_min[varx], "chez les", lab_min[sex], sep = " "))
  
  return(model)
}

interpretation = function(mod, varx, vary, sex){
  
  Beta_0 = coef(mod)[1]
  Beta_1 = coef(mod)[2]
  
  print(paste("Au travers de cette sortie, on remarque que :  Bêta_0 = ", Beta_0, " et Bêta_1 = ", Beta_1))
  print(paste("\nLa droite des moindres carrés a donc pour équation : ",
              lab_max[vary], "=", round(Beta_0,2),
              ifelse(Beta_1>0," + "," - "), round(abs(Beta_1),2),"*",lab_max[varx]))
  
  print(paste("Interprétaion de Bêta_1 :  Une augmentation d'une unité de la variable ",lab_min[varx],
        "entraîne en moyenne une ", ifelse(Beta_1>0,"augmentation","diminution"), " de ", round(abs(Beta_1),2),
        " de la variable ",lab_min[vary], " chez les ", lab_min[sex]))
  
  cat("\n")
  # Extraction du coefficient de détermination
  #R_squared = round(100*summary(mod)$r.squared, 2)
}

# ________________ Poids ____________________#

# Chez les femmes

resultats_p_f <- lapply(X = variables_quantip, function(x) {
  mess <- paste("Poids versus", lab_min[x], "chez les femmes", sep = " ")
  cat("\n", mess, "\n", sep="")
  
  modelv <- model_reg(x, "Weight", "Female")
  
  cat("\nRésumé du modèle :\n")
  print(summary(modelv))
  
  cat("Interprétation :\n")
  interpretation(modelv, x, "Weight", "Female")  
  cat("\n--------------------------------\n")
  
  # On retourne quand même les résultats si besoin plus tard
  list(
    message = mess,
    model = modelv,
    summary = summary(modelv)
  )
})

# Chez les hommes

resultats_p_h <- lapply(X = variables_quantip, function(x) {
  mess <- paste("Poids versus", lab_min[x], "chez les hommes", sep = " ")
  cat("\n", mess, "\n", sep="")
  
  modelv <- model_reg(x, "Weight", "Male")
  
  cat("\nRésumé du modèle :\n")
  print(summary(modelv))
  
  cat("Interprétation :\n")
  interpretation(modelv, x, "Weight", "Male")  
  cat("\n--------------------------------\n")
  
  # On retourne quand même les résultats si besoin plus tard
  list(
    message = mess,
    model = modelv,
    summary = summary(modelv)
  )
})


# ________________ Taille ____________________#

# Chez les femmes

resultats_t_f <- lapply(X = variables_quantit, function(x) {
  mess <- paste("Taille versus", lab_min[x], "chez les femmes", sep = " ")
  cat("\n", mess, "\n", sep="")
  
  modelv <- model_reg(x, "Height", "Female")
  
  cat("\nRésumé du modèle :\n")
  print(summary(modelv))
  
  cat("Interprétation :\n")
  interpretation(modelv, x, "Height", "Female")  
  cat("\n--------------------------------\n")
  
  # On retourne quand même les résultats si besoin plus tard
  list(
    message = mess,
    model = modelv,
    summary = summary(modelv)
  )
})

# Chez les hommes

resultats_t_h <- lapply(X = variables_quantit, function(x) {
  mess <- paste("Taille versus", lab_min[x], "chez les hommes", sep = " ")
  cat("\n", mess, "\n", sep="")
  
  modelv <- model_reg(x, "Height", "Male")
  
  cat("\nRésumé du modèle :\n")
  print(summary(modelv))
  
  cat("Interprétation :\n")
  interpretation(modelv, x, "Height", "Male")  
  cat("\n--------------------------------\n")
  
  # On retourne quand même les résultats si besoin plus tard
  list(
    message = mess,
    model = modelv,
    summary = summary(modelv)
  )
})




# ............................................................................... #
# ... Tracé de la droite des moindres carrés ... #


droite_sse = function(varx, vary, sex){
  
  workset_g = subset(workset,
                     subset = Gender == sex)
  nuage_reg(varx, vary, sex)
  
  fit = model_reg(varx, vary, sex)
  
  Beta_0 = coef(fit)[1]
  Beta_1 = coef(fit)[2]
  
  abline(fit,
         col = "green3",
         lwd = 2)
  
  mtext(paste(lab_max[vary], "=", round(Beta_0,2),
              ifelse(Beta_1>0," + "," -  "), round(abs(Beta_1),2),"*",lab_max[varx]),
        side = 3, 
        line = -4, 
        adj = 1)
  
  mtext(paste("Rsquared = ", round(100*summary(fit)$r.squared, 2), " %"),
        side = 3, 
        line = -4, 
        adj = 0.05)
  
  legend(x = "topright",
         bty = "n",
         lty = 1,
         lwd = 3,
         col = "green3",
         legend = "Droite des moindres carrés"
  )
}

# ________________ Poids ____________________#

# Chez les femmes

lapply(X = variables_quantip,
       function(x) droite_sse(x, "Weight", "Female"))

# Chez les hommes

lapply(X = variables_quantip,
       function(x) droite_sse(x, "Weight", "Male"))
par(mfrow = c(1,1))


# ________________ Taille ____________________#

# Chez les femmes

lapply(X = variables_quantit,
       function(x) droite_sse(x, "Height", "Female"))

# Chez les hommes

lapply(X = variables_quantit,
       function(x) droite_sse(x, "Height", "Male"))





# ............................................................................... #
# ... Classement pour chacune des variables cibles des variables morphologiques les plus pertinentes ... #

# Indicateur utilisé : Coefficient de détermination

# ________________ Poids ____________________#

# Chez les femmes

Models_coef_fp = data.frame(var = variables_quantip,
                         coef = sapply(X = variables_quantip, 
                                       FUN = function(x) round(100*(summary(model_reg(x,"Weight","Female"))$r.squared),2)
                                       )
                         )
print(Models_coef_fp)

top_poids_f = Models_coef_fp[order(Models_coef_fp$coef, decreasing = TRUE), ]

top3_poids_f = top_poids_f[1:3, ]

print(top3_poids_f)

print("Les variabls quantitatives les plus associées au poids chez les femmes sont : ")

lapply(X = top3_poids_f$var, 
       function(x) paste(lab_max[x], " avec un coefficient de détermination de : ", 
                         top3_poids_f$coef[top3_poids_f$var == x], "%"))

# Chez les hommes

Models_coef_mp = data.frame(var = variables_quantip,
                         coef = sapply(X = variables_quantip, 
                                       FUN = function(x) round(100*(summary(model_reg(x,"Weight","Male"))$r.squared),2)
                         )
)
print(Models_coef_mp)

top_poids_m = Models_coef_mp[order(Models_coef_mp$coef, decreasing = TRUE), ]

top3_poids_m = top_poids_m[1:3, ]

print(top3_poids_m)

print("Les variabls quantitatives les plus associées au poids chez les hommes sont : ")

lapply(X = top3_poids_m$var, 
       function(x) paste(lab_max[x], " avec un coefficient de détermination de : ", 
                         top3_poids_m$coef[top3_poids_m$var == x], "%"))



# ________________ Taille ____________________#

# Chez les femmes

Models_coef_ft = data.frame(var = variables_quantit,
                            coef = sapply(X = variables_quantit, 
                                          FUN = function(x) round(100*(summary(model_reg(x,"Height","Female"))$r.squared),2)
                            )
)
print(Models_coef_ft)

top_taille_f = Models_coef_ft[order(Models_coef_ft$coef, decreasing = TRUE), ]

top3_taille_f = top_taille_f[1:3, ]

print(top3_taille_f)

print("Les variabls quantitatives les plus associées à la taille chez les femmes sont : ")

lapply(X = top3_taille_f$var, 
       function(x) paste(lab_max[x], " avec un coefficient de détermination de : ", 
                         top3_taille_f$coef[top3_taille_f$var == x], "%"))

# Chez les hommes

Models_coef_mt = data.frame(var = variables_quantit,
                            coef = sapply(X = variables_quantit, 
                                          FUN = function(x) round(100*(summary(model_reg(x,"Height","Male"))$r.squared),2)
                            )
)
print(Models_coef_mt)

top_taille_m = Models_coef_mt[order(Models_coef_mt$coef, decreasing = TRUE), ]

top3_taille_m = top_taille_m[1:3, ]

print(top3_taille_m)

print("Les variabls quantitatives les plus associées à la taille chez les hommes sont : ")

lapply(X = top3_taille_m$var, 
       function(x) paste(lab_max[x], " avec un coefficient de détermination de : ", 
                         top3_taille_m$coef[top3_taille_m$var == x], "%"))

