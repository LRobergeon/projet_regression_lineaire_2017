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




### ETAPE 1
liste_indice_a_enlever_etape_1 = c()
for (x1 in 1:(stepall-1)){
  for (x2 in (x1+1):(stepall)){
    
    ## print(train_data[colonne1])
    X1 = cbind(train_data[,x1])
    X2 = cbind(train_data[,x2])
    
    reg = lm(X1~X2)
    coefficient_determination = summary(reg)$r.squared 
    cd  = c(cd,coefficient_determination)
    
    if (coefficient_determination > 0.95) {
      pred_ok = c(pred_ok,c(colonnes[x1],colonnes[x2]))
      if ( !(x1 %in% liste_indice_a_enlever_etape_1)){
        liste_indice_a_enlever_etape_1 = c(liste_indice_a_enlever_etape_1, x1)
      }
    }
  }
}


if (length(liste_indice_a_enlever_etape_1) > 0){
  train_data_modified = train_data[ , - liste_indice_a_enlever_etape_1]
  test_data_modified = test_data[ , - liste_indice_a_enlever_etape_1]
  colonnes_modified = colnames(test_data_modified)
  stepall_modified = length(colonnes_modified)
} else {
  train_data_modified = train_data
  test_data_modified = test_data
  colonnes_modified = colnames(test_data_modified)
  stepall_modified = length(colonnes_modified)
}

### ETAPE 2
liste_indice_a_enlever_etape_2 = c()
for (x1 in 1:(stepall_modified-2)){
  for (x2 in (x1+1):(stepall_modified-1)){
    for (x3 in (x2+1):stepall_modified){
      ## print(train_data[colonne1])
      X1 = cbind(train_data_modified[,x1])
      X2 = cbind(train_data_modified[,x2])
      X3 = cbind(train_data_modified[,x3])
      
      
      reg = lm(X1~X2 + X3)
      coefficient_determination = summary(reg)$r.squared 
      cd  = c(cd,coefficient_determination)
      
      if (coefficient_determination > 0.95) {
        pred_ok = c(pred_ok,c(colonnes[x1],colonnes[x2],colonnes[x3]))
        if ( !(x1 %in% liste_indice_a_enlever_etape_2)){
          liste_indice_a_enlever_etape_2 = c(liste_indice_a_enlever_etape_2, x1)
        }
      }
    }
  }
}



if (length(liste_indice_a_enlever_etape_2) > 0){
  train_data_modified = train_data_modified[ , - liste_indice_a_enlever_etape_2]
  test_data_modified = test_data_modified[ , - liste_indice_a_enlever_etape_2]
  colonnes_modified = colnames(test_data_modified)
  stepall_modified = length(colonnes_modified)
  } else {
  train_data_modified = train_data_modified
  test_data_modified = test_data_modified
  colonnes_modified = colnames(test_data_modified)
  stepall_modified = length(colonnes_modified)
  }
  ### ETAPE 3
  liste_indice_a_enlever_etape_3 = c()
  for (x1 in 1:(stepall_modified-3)){
    for (x2 in (x1+1):(stepall_modified-2)){
      for (x3 in (x2+1):stepall_modified-1){
        for (x4 in (x3+1):stepall_modified){
          ## print(train_data[colonne1])
          X1 = cbind(train_data_modified[,x1])
          X2 = cbind(train_data_modified[,x2])
          X3 = cbind(train_data_modified[,x3])
          X4 = cbind(train_data_modified[,x4])
          
          reg = lm(X1~X2 + X3 + X4)
          coefficient_determination = summary(reg)$r.squared 
          cd  = c(cd,coefficient_determination)
          
          if (coefficient_determination > 0.95) {
            pred_ok = c(pred_ok,c(colonnes[x1],colonnes[x2],colonnes[x3]))
            if ( !(x1 %in% liste_indice_a_enlever_etape_3)){
              liste_indice_a_enlever_etape_3 = c(liste_indice_a_enlever_etape_3, x1)
            }
          }
        }
      }
    }
  }
  
  
  
  if (length(liste_indice_a_enlever_etape_3) > 0){
    train_data_modified = train_data_modified[ , - liste_indice_a_enlever_etape_3]
    test_data_modified = test_data_modified[ , - liste_indice_a_enlever_etape_3]
    colonnes_modified = colnames(test_data_modified)
    stepall_modified = length(colonnes_modified)
  } else {
    train_data_modified = train_data_modified
    test_data_modified = test_data_modified
    colonnes_modified = colnames(test_data_modified)
    stepall_modified = length(colonnes_modified)
  }
    cd = c()
    max_r_squared = 0
    regresseurs = c(1,2,3)
    for (x1 in 1:(stepall_modified-2)){
      for (x2 in (x1+1):(stepall_modified-1)){
        for (x3 in (x2+1):stepall_modified){
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
          if (coefficient_determination > max_r_squared) {
            regression_meilleure = reg
            max_r_squared <- coefficient_determination
            regresseurs = c(x1,x2,x3)
          }
          cd  = c(cd,coefficient_determination)
        }
        
      }}
    cd = sort(cd,decreasing = TRUE)
    hist(cd)
    for (i in regresseurs){
      print(colonnes_modified[i])
    }
    
    
    
essai_AIC = step(object = lm(y_test ~., 
                                 data = test_data_modified), 
                     direction='backward', 
                     scope=list(upper= ~ ., lower=~1),
                     k = 2)
    
essai_BIC = step(object = lm(y_test ~., 
                             data = test_data_modified), 
                 direction='backward', 
                 scope=list(upper= ~ ., lower=~1),
                 k = log(40))
    