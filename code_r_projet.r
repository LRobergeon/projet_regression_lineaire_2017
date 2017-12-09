##setwd("~/Desktop/M2 DS/reg_lin/projet_regression_lineaire_2017")
setwd("C:/Users/tangu/Documents/GitHub/projet_regression_lineaire_2017")
# nettoyage environnement
rm (list=ls())
## blabla
## hello world
exit <- function() {
  .Internal(.invokeRestart(list(NULL, NULL), NULL))
}
# Chargement de la librairie
library(pls)
library(prospectr)
library(MASS)
library(L1pack)
library(qpcR)
multiplier_deux_cbinds <- function(X,Y)
{result = c()
for (i in 1:length(X)) {
  result = c(result, X[i,1]*Y[i,1])
}
result = cbind(result)
return(result)
}

#outliers = c(17, 9)

train_data = read.table("data_groupe9.csv", header=TRUE,sep = ",")
test_data =  read.table("essai.csv", header=TRUE,sep = ";")
y_test = cbind(test_data[,2])
test_data = test_data[, - c(1,2)]
train_data = train_data[ , - c(1)]
colonnes = colnames(train_data)
colonnes_train = colnames(test_data)
stepall = length(colonnes)

#boxplot(train_data_modified)

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
    #test_data_modified = test_data_modified[- outliers,]
    #y_test = y_test[-outliers,]
    colonnes_modified = colnames(test_data_modified)
    stepall_modified = length(colonnes_modified)
  } else {
    train_data_modified = train_data_modified
    test_data_modified = test_data_modified
    #test_data_modified = test_data_modified[- outliers,]
    #y_test = y_test[-outliers,]
    colonnes_modified = colnames(test_data_modified)
    stepall_modified = length(colonnes_modified)
  }

cd = c()
max_r_squared = 0
regresseurs_L2 = c(1,2,3)
L2 = 1000000000000
PRESS_min = 100000000000
L1 = 1000000000000
for (x1 in 1:(stepall_modified-2)){
  for (x2 in (x1+1):(stepall_modified-1)){
    for (x3 in (x2+1):stepall_modified){
      X1 = cbind(test_data_modified[,x1])
      X2 = cbind(test_data_modified[,x2])
      X3 = cbind(test_data_modified[,x3])
      
      X2X3 = multiplier_deux_cbinds(X2,X3)

      X2X1 = multiplier_deux_cbinds(X1,X2)
      
      X1X3 = multiplier_deux_cbinds(X1,X3)
      
      
      
      null=lm(y_test~1)
      full = lm( y_test ~ X1 + X2 + X3 + X2X1 + X1X3 + X2X3 )
      AIC=step(null, scope=list(lower=null, upper=full), direction="backward",k=2, trace = FALSE)
      BIC=step(null, scope=list(lower=null, upper=full), direction="backward", k = log(40), trace = FALSE)
      if(length(AIC$coefficients)!=1 && length(BIC$coefficients)!=1){
        PRESS_actuel = PRESS(AIC,verbose=FALSE)$P.square
        if (PRESS_actuel < PRESS_min){
          PRESS_min <- PRESS_actuel
          regression_choisie_AIC <- AIC
          regression_choisie_BIC <- BIC
          regresseur=c(x1,x2,x3)
        }
      
      }
      
    }
    
  }}
plot(y_test,predict(regression_choisie_AIC))
y_predit=predict(regression_choisie_AIC)
colonnes_modified[regresseur]

library(L1pack)
lad(formula=full, method = "BR", subset, model = TRUE, x = FALSE, y = FALSE, contrasts = NULL)
l1fit(cbind(X1,X2,X3,X1X3,X2X1,X2X3), y_test, intercept = TRUE, tolerance = 1e-07, print.it = TRUE)

### suscpicion d'outliers 
### outliers = c(17, 9, 12)

### http://egallic.fr/l3-eco-gestion-regression-lineaire-avec-r-selection-de-modele/




###null=lm(y_test~1,data=test_data_modified)
# full=lm(y_test~.,data=test_data_modified)
# 
# AIC=step(null, scope=list(lower=null, upper=full), direction="forward",k=2)
# BIC=step(null, scope=list(lower=null, upper=full), direction="forward", k = log(40))
# 
# test_data2 =  read.table("essai.csv", header=TRUE,sep = ";")
# modele_AIC=lm(formula = reponse ~ descripteur1 + descripteur14 + descripteur71 + descripteur35 + descripteur23,data=test_data2)
# resAIC <- PRESS(modele_AIC)
# barplot(resAIC$residuals)
# 
# modele_BIC=lm(formula = reponse ~ descripteur1 + descripteur14 + descripteur71 + descripteur35,data=test_data2)
# resBIC <- PRESS(modele_BIC)
# barplot(resBICresiduals)
# 
# 
# if (resAIC$P.square>resBIC$P.square){
#   model=modele_AIC
#   meilleur_model="AIC"
# } else{
#   model=modele_BIC
#   meilleur_model="BIC"
# }
# 
# 
# library(L1pack)
# test_data_modified
# 
# 
# x=test_data2[,3:5]
# y=test_data2$reponse
# l1fit(x, y, intercept = TRUE, tolerance = 1e-07, print.it = TRUE)