setwd("~/Desktop/M2 DS/reg_lin/projet_regression_lineaire_2017")
# nettoyage environnement
rm (list=ls())

exit <- function() {
  .Internal(.invokeRestart(list(NULL, NULL), NULL))
}
# Chargement de la librairie
library(pls)
library(prospectr)

train_data = read.table("data_groupe9.csv", header=TRUE,sep = ";")

colonnes = colnames(train_data)
stepall = length(colonnes)

"""
cd = c()
pred_ok = c()
for (x1 in 1:(stepall-3)){
  for (x2 in (x1+1):(stepall-2)){
    for (x3 in (x2+1):stepall-1){
      for (x4 in (x3+1):stepall){
        ## print(train_data[colonne1])
        X1 = cbind(train_data[,x1])
        X2 = cbind(train_data[,x2])
        X3 = cbind(train_data[,x3])
        X4 = cbind(train_data[,x3])
        
        reg = lm(X1~X2 + X3 + X4)
        coefficient_determination = summary(reg)$r.squared 
        cd  = c(cd,coefficient_determination)
        
        if (coefficient_determination > 0.95) {
          print(colonnes[x1])
          print(colonnes[x2])
          print(colonnes[x3])
          print(colonnes[x4])
          
          pred_ok = c(pred_ok,c(colonnes[x1],colonnes[x2],colonnes[x3],colonnes[x4]))
        }
      }
    }
  }
}
"""

liste_indice_a_enlever = c(1,12,34,45)


train_data_modified = train_data[ , - liste_indice_a_enlever]
colonnes_modified = colnames(train_data_modified)
stepall_modified = length(colonnes_modified)

train_data_modified = data.frame(train_data_modified)
essai = data.frame(train_data_modified[,12]) + data.frame(train_data_modified[,34])


cd = c()

for (x1 in 1:(stepall-3)){
  for (x2 in (x1+1):(stepall-2)){
    for (x3 in (x2+1):stepall-1){
      X1 = cbind(train_data[,x1])
      X2 = cbind(train_data[,x2])
      X3 = cbind(train_data[,x3])

      X1_2 = c()
      for (x in X1){
        X1_2 = c(X1_2, x*x)
      }
      X1_2 = cbind(X1_2)


      X2_2 = c()
      for (x in X2){
        X2_2 = c(X2_2, x*x)
      }
      X2_2 = cbind(X2_2)


      X3_2 = c()
      for (x in X3){
        X3_2 = c(X2_2, x*x)
        }
      X3_2 = cbind(X3_2)

      X2X3 = c()
	  for (i in 1:length(X2)) {
	  	X2X3 = c(X2X3, X2[i,1]*X3[i,1])
	  }
	  X2X3 = cbind(X2X3)
      
      X2X1 = c()
	  for (i in 1:length(X2)) {
	  	X2X1 = c(X2X1, X2[i,1]*X1[i,1])
	  }
	  X2X1 = cbind(X2X1)

	  X1X3 = c()
	  for (i in 1:length(X2)) {
	  	X1X3 = c(X1X3, X1[i,1]*X3[i,1])
	  }
	  X1X3 = cbind(X1X3)

      reg = lm( X1 ~ X2 + X3 + X1_2 + X2_2 + X3_2 + X2X3 + X2X1 + X1X3)

      coefficient_determination = summary(reg)$r.squared 
      cd  = c(cd,coefficient_determination)
	  }
      
    }}

      