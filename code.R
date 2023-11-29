donnees_europe = read.table("donnees_europe.txt", header=T, row.names='PAYS')

head(donnees_europe)

donnees_europe = donnees_europe[, 2:dim(donnees_europe)[2]]

head(donnees_europe)

# install.packages("gridExtra")

table_to_image <- function(tableau, filename='image.png') {
  library("gridExtra")
  png(filename, height = 50*nrow(tableau), width = 200*ncol(tableau))
  grid.table(tableau)
  dev.off()
}

# Etude univariee 

summary(donnees_europe)

# table_to_image(summary(donnees_europe), filename='summary.png')

pairs(donnees_europe)

nb_col = dim(donnees_europe)[2]


# Affichage et export des histogrammes

# par(mfrow=c(3,4))
for (i in 1:nb_col) {
  hist(donnees_europe[,i], 
       col='blue', 
       main=colnames(donnees_europe)[i], 
       xlab='Valeurs')
  # dev.print(device = png, 
              file = paste("Hist_", colnames(donnees_europe)[i],".png", sep=''), 
              width = 500)
}

# Affichage et export des boites a moustache

for (i in 1:nb_col) {
  boxplot(donnees_europe[,i], main=colnames(donnees_europe)[i])
  # dev.print(device = png, 
              file = paste("BoxPlot_", colnames(donnees_europe)[i],".png", sep=''), 
              width = 500)
}

# Matrice des correlations

correlations = round(cor(donnees_europe), digits=2)

# Export csv file (to use it in LaTeX after converting it using a Python script)
# write.csv(correlations, file='correlations.csv')
#### ACP ####

library("FactoMineR")

pca_eu = PCA(donnees_europe, ncp=5, graph=T)
pca_eu

# b) Choix du nombre de composantes 
pca_eu$eig

plot(pca_eu$eig[,2], 
     type='b', 
     main='Eboulis des valeurs propres', 
     ylab='percentage of variance')

pca_eu$var$contrib

# c) Interpreter les composantes retenues #
pca_eu$var$contrib
# Premiere composante : "Bien-etre de la population" 
# (esperances de vie h et f, pib, taux d'activite et les taux de chom)
# Deuxieme composante : "Demographie" (pop, tel)
# Troisieme composante : "Activite economique" (temp, tinf)
# Quatrieme composante : "Mariages" (mariag)

tabcos2 = pca_eu$ind$cos2
# ci^2 / distances initiales 
# seuils : 0,5 sur c1, 0,25 sur c2, 0,15 sur c3, 0,1 sur c4

tabcos2
cos2axe1=tabcos2[,1]
# bien represente sur l'axe 1 :
cos2axe1[cos2axe1>0.5]
# write.csv(cos2axe1[cos2axe1>0.5], file='Pays_rep_dim1.csv')

cos2axe2 = tabcos2[,2]
# bien represente sur l'axe 2
cos2axe2[cos2axe2>0.25]
# write.csv(cos2axe2[cos2axe2>0.25], file='Pays_rep_dim2.csv')

cos2axe3 = tabcos2[,3]
# bien represente sur l'axe 3
cos2axe3[cos2axe3>0.15]
# write.csv(cos2axe3[cos2axe3>0.15], file='Pays_rep_dim3.csv')

cos2axe4 = tabcos2[,4]
# bien represente sur l'axe 2
cos2axe4[cos2axe4>0.1]
# write.csv(cos2axe4[cos2axe4>0.1], file='Pays_rep_dim4.csv')

names(pca_eu)
names(pca_eu$var)
pca_eu$var$cor

# write.csv(pca_eu$var$cor, 'contribComp.csv') # idem que contrib finalement 
                                               # (un peu plus lisible)
# d) Generation des graphiques des individus sur les composantes retenues

library("factoextra")

fviz_pca_ind(pca_eu, 
             axes=c(1,2), 
             col.ind = "black",
             repel = TRUE)
fviz_pca_ind(pca_eu, 
             axes=c(1,3), 
             col.ind = "black",
             repel = TRUE)
fviz_pca_ind(pca_eu, 
             axes=c(1,4), 
             col.ind = "black",
             repel = TRUE)

# e) pays ayant le plus fortement contribue a la composante 1
pca_eu$ind$contrib[,1]
# write.csv(pca_eu$ind$contrib[,1], file="contrib_dim1.csv")
