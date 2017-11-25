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
for (x1 in 1:(stepall-2)){
  for (x2 in (x1+1):(stepall-1)){
    for (x3 in (x2+1):stepall){
        ## print(train_data[colonne1])
        X1 = cbind(train_data[,x1])
        X2 = cbind(train_data[,x2])
        X3 = cbind(train_data[,x3])
        
        reg = lm(X1~X2 + X3)
        coefficient_determination = summary(reg)$r.squared 
        cd  = c(cd,coefficient_determination)
        
        if (coefficient_determination > 0.95) {
          print(colonnes[x1])
          print(colonnes[x2])
          print(colonnes[x3])
          
          pred_ok = c(pred_ok,c(colonnes[x1],colonnes[x2],colonnes[x3]))
      }
    }
  }
}