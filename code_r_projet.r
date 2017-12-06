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
test_data =  read.table("essai.csv", header=TRUE,sep = ";")
y_test = cbind(test_data[,2])
test_data = test_data[, - c(1,2)]
train_data = train_data[ , - c(1)]
colonnes = colnames(train_data)
colonnes_train = colnames(test_data)
stepall = length(colonnes)


cd = c()
pred_ok = c()
liste_indice_a_enlever = c()
for (x1 in 1:(stepall-3)){
  for (x2 in (x1+1):(stepall-2)){
    for (x3 in (x2+1):stepall-1){
        ## print(train_data[colonne1])
        X1 = cbind(train_data[,x1])
        X2 = cbind(train_data[,x2])
        X3 = cbind(train_data[,x3])

        
        reg = lm(X1~X2 + X3)
        coefficient_determination = summary(reg)$r.squared 
        cd  = c(cd,coefficient_determination)
        
        if (coefficient_determination > 0.85) {
          pred_ok = c(pred_ok,c(colonnes[x1],colonnes[x2],colonnes[x3]))
          if ( !(x1 %in% liste_indice_a_enlever)){
              liste_indice_a_enlever = c(liste_indice_a_enlever, x1)
          }
          }
      }
    }
  }




if (length(liste_indice_a_enlever) )
train_data_modified = train_data[ , - liste_indice_a_enlever]
test_data_modified = test_data[ , - liste_indice_a_enlever]

colonnes_modified = colnames(test_data_modified)
stepall_modified = length(colonnes_modified)

### train_data_modified = data.frame(train_data_modified)

cd = c()
for (x1 in 1:(stepall_modified-3)){
  for (x2 in (x1+1):(stepall_modified-2)){
    for (x3 in (x2+1):stepall_modified-1){
      X1 = cbind(test_data_modified[,x1])
      X2 = cbind(test_data_modified[,x2])
      X3 = cbind(test_data_modified[,x3])

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

      reg = lm( y_test ~ X1 + X2 + X3 + X2X1 + X1X3 + X2X3 )

      coefficient_determination = summary(reg)$r.squared 
      cd  = c(cd,coefficient_determination)
	  }
      
  }}
cd = sort(cd,decreasing = TRUE)
hist(cd)
