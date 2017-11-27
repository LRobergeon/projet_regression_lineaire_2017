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


liste_indice_a_enlever = c(1,12,34,45)


train_data_modified = train_data[ , - liste_indice_a_enlever]
colonnes_modified = colnames(train_data_modified)
stepall_modified = length(colonnes_modified)
