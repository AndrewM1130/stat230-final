# package & data imports
library(haven)
library(tidyverse)
library(miceadds)
library(sandwich)
library(lmtest)

setwd('C:/Users/hiros/Desktop/Storage/documents/linear models/project/stat230-final')

house <- read_dta('data/PisoFirme_AEJPol-20070024_household.dta')
indiv <- read_dta('data/PisoFirme_AEJPol-20070024_individual.dta')
df = left_join(house,indiv)
dpisofirme = c('dpisofirme')

## variable definitions for household dataframe
HH_census <- grep("C_*", colnames(df),value = T)
HH_survey <- c("S_HHpeople", "S_headage", "S_spouseage", "S_headeduc", "S_spouseeduc",
               "S_rooms", "S_waterland", "S_waterhouse", "S_electricity", "S_cementfloor2000",
               "S_hasanimals", "S_animalsinside", "S_garbage", "S_washhands",
               "S_incomepc", "S_assetspc", "S_shpeoplework", "S_microenter", "S_hrsworkedpc", "S_consumptionpc" ,
               "S_cashtransfers", "S_milkprogram", "S_foodprogram")
HH_demog1 <- c("S_HHpeople", "S_headage", "S_spouseage", "S_headeduc", "S_spouseeduc")
HH_demog2 <- grep("S_dem*", colnames(df),value = T)
HH_health <- c("S_waterland", "S_waterhouse", "S_electricity", "S_hasanimals", 
               "S_animalsinside", "S_garbage", "S_washhands")
HH_econ <- c("S_incomepc","S_assetspc")
HH_social <- c("S_cashtransfers", "S_milkprogram", "S_foodprogram", "S_seguropopular")
HH_floor <- c("S_shcementfloor", "S_cementfloorkit", 
              "S_cementfloordin", "S_cementfloorbat", "S_cementfloorbed")
HH_satis <- c("S_satisfloor", "S_satishouse", "S_satislife", "S_cesds", "S_pss")
HH_robust <- c("S_instcement", "S_instsanita", "S_restsanita", "S_constceili",
               "S_restowalls", "S_improveany", "S_logrent", "S_logsell", "S_consumptionpc")

CH_survey <- c("S_age", "S_gender", "S_childma", "S_childmaage", "S_childmaeduc", 
               "S_childpa", "S_childpaage", "S_childpaeduc")
CH_demog <- c("S_HHpeople", "S_rooms", "S_age", "S_gender", "S_childma", "S_childmaage", 
              "S_childmaeduc", "S_childpa", "S_childpaage", "S_childpaeduc")
CH_health <- c("S_parcount", "S_diarrhea", "S_anemia", 
               "S_mccdts", "S_pbdypct", "S_haz", "S_whz")
CH_robust <- c("S_respira", "S_skin", "S_otherdis")
PA_robust <- c("S_malincom", "S_palincom")
dtriage = grep('dtriage?',colnames(df),value = T)
Ex_cols = c('dpisofirme','idcluster','coord_x','coord_y','idmun','idmza')

## Missing Value Imputations
Impute_df = function(df) {
  dmiss = apply(df,2,function(x) {
    return(is.na(x))
  }) %>% cbind.data.frame()
  colnames(dmiss) = paste0('dmiss_',colnames(df))
  df[is.na(df)] = 0
  return(cbind(df,dmiss))
}

imp_cols = c(HH_demog1,HH_demog2,HH_health,HH_econ,HH_social,CH_demog)
non_imp_cols = c(Ex_cols,HH_census,HH_floor,HH_satis,HH_robust,
                 CH_health,CH_robust,PA_robust,dtriage)
df_imp = cbind(df[,non_imp_cols],Impute_df(df[,imp_cols]))

HH_demog1_imp = c(HH_demog1,paste0('dmiss_',HH_demog1))
HH_demog2_imp = c(HH_demog2,paste0('dmiss_',HH_demog2))
HH_health_imp = c(HH_health,paste0('dmiss_',HH_health))
HH_econ_imp = c(HH_econ,paste0('dmiss_',HH_econ))
HH_social_imp = c(HH_social,paste0('dmiss_',HH_social))[1:5]
CH_demog_imp = c(CH_demog,paste0('dmiss_',CH_demog))

## Defining Models for Regressions
HHmodel_1_control = dpisofirme
HHmodel_2_control = c(dpisofirme,HH_demog1_imp,HH_demog2_imp,HH_health_imp)
HHmodel_3_control = c(dpisofirme,HH_demog1_imp,HH_demog2_imp,HH_health_imp,
                      HH_social_imp)
HHmodel_4_control = c(dpisofirme,HH_demog1_imp,HH_demog2_imp,HH_health_imp,
                      HH_social_imp,HH_econ_imp)

INmodel_1_control = dpisofirme
INmodel_2_control = c(dpisofirme,CH_demog_imp,dtriage,HH_health_imp)
INmodel_3_control = c(dpisofirme,CH_demog_imp,dtriage,HH_health_imp,
                      HH_social_imp)
INmodel_4_control = c(dpisofirme,CH_demog_imp,dtriage,HH_health_imp,
                      HH_social_imp,HH_econ_imp)

## Table 4: Regressions of cement floor coverage
for (i in HH_floor) {
  outcome = i
  for (j in 1:4) {
    control = get(paste0('HHmodel_',j,'_control'))
    control_form = paste(control,collapse = " + ")
    form = paste0(outcome,'~',control_form) %>% as.formula
    df2 = df_imp[,c(outcome,control,'idcluster')]
    df2 = df2[complete.cases(df2[,grep('[^dmiss]',colnames(df2))]),]
    model = lm(formula = form,
               data = df2)
    print(model$coefficients['dpisofirme'])
    cluster_model = coeftest(model,vcov=vcovCL,
                             cluster = ~idcluster)
    cat("error:", cluster_model[2,2], "\n")
    if (j == 1) {
      cat("Control Group Mean:",summary(model)$coefficients[1,1], "\n")
      cat("Control Group SD:",summary(model)$coefficients[1,2] * sqrt(nrow(house)), "\n")
    }
    assign(paste0(outcome,
                  '_HHmodel_',
                  j), 
           cluster_model)
  }
}

## Table 5: Regressions of Child Health Measures
for (i in CH_health) {
  outcome = i
  for (j in 1:4) {
    control = get(paste0('INmodel_',j,'_control'))
    control_form = paste(control,collapse = " + ")
    form = paste0(outcome,'~',control_form) %>% as.formula
    df2 = df_imp[,c(outcome,control,'idcluster')]
    df2 = df2[complete.cases(df2[,grep('[^dmiss]',colnames(df2))]),]
    
    model = lm(formula = form,
               data = df2)
    cluster_model = coeftest(model,vcov=vcovCL,
                              cluster = ~idcluster)
    
    #print(model$coefficients['dpisofirme'])
    #cat("error:", cluster_model[2,2], "\n")
    
    if (j == 1) {
      cat("Control Group Mean:",summary(model)$coefficients[1,1], "\n")
      cat("Control Group SD:",summary(model)$coefficients[1,2] * sqrt(nrow(df2)), "\n")
    }
    assign(paste0(outcome,
                  '_INmodel_',
                  j), cluster_model)
  }
}

## Table 6: Regressions of Satisfaction & Mental Health 
for (i in HH_satis) {
  outcome = i
  for (j in 1:4) {
    control = get(paste0('HHmodel_',j,'_control'))
    control_form = paste(control,collapse = " + ")
    form = paste0(outcome,'~',control_form) %>% as.formula
    df2 = df_imp[,c(outcome,control,'idcluster')]
    df2 = df2[complete.cases(df2[,grep('[^dmiss]',colnames(df2))]),]
    
    model = lm(formula = form,
               data = df2)
    cluster_model = coeftest(model,vcov=vcovCL,
                             cluster = ~idcluster)
    print(model$coefficients['dpisofirme'])
    cat("error:", cluster_model[2,2], "\n")
    if (j == 1) {
      cat("Control Group Mean:",summary(model)$coefficients[1,1], "\n")
      cat("Control Group SD:",summary(model)$coefficients[1,2] * sqrt(nrow(df2)), "\n")
    }
    assign(paste0(outcome,
                  '_HHmodel_',
                  j), cluster_model)
  }
}

## Table 7: Robustness Check Households
for (i in c(HH_robust)) {
  outcome = i
  for (j in 1:4) {
    control = get(paste0('HHmodel_',j,'_control'))
    control_form = paste(control,collapse = " + ")
    form = paste0(outcome,'~',control_form) %>% as.formula
    df2 = df_imp[,c(outcome,control,'idcluster')]
    df2 = df2[complete.cases(df2[,grep('[^dmiss]',colnames(df2))]),]
    
    model = lm(formula = form,
               data = df2)
    cluster_model = coeftest(model,vcov=vcovCL,
                             cluster = ~idcluster)
    print(model$coefficients['dpisofirme'])
    cat("error:", cluster_model[2,2], "\n")
    if (j == 1) {
      cat("Control Group Mean:",summary(model)$coefficients[1,1], "\n")
      cat("Control Group SD:",summary(model)$coefficients[1,2] * sqrt(nrow(df2)), "\n")
    }
    assign(paste0(outcome,
                  '_HHmodel_',
                  j), cluster_model)
  }
}

## Robustness Check-Individual : first 3 rows
for (i in c(CH_robust)) {
  outcome = i
  for (j in 1:4) {
    control = get(paste0('INmodel_',j,'_control'))
    control_form = paste(control,collapse = " + ")
    form = paste0(outcome,'~',control_form) %>% as.formula
    df2 = df_imp[,c(outcome,control,'idcluster')]
    df2 = df2[complete.cases(df2[,grep('[^dmiss]',colnames(df2))]),]
    
    model = lm(formula = form,
               data = df2)
    cluster_model = coeftest(model,vcov=vcovCL,
                             cluster = ~idcluster)
    print(model$coefficients['dpisofirme'])
    cat("error:", cluster_model[2,2], "\n")
    if (j == 1) {
      cat("Control Group Mean:",summary(model)$coefficients[1,1], "\n")
      cat("Control Group SD:",summary(model)$coefficients[1,2] * sqrt(nrow(df2)), "\n")
    }
    assign(paste0(outcome,
                  '_INmodel_',
                  j), cluster_model)
  }
}

## Robustness Check - PArobust - log mother income & log father income rows for Table 7
for (i in c(PA_robust)) {
  outcome = i
  for (j in 1:4) {
    control = get(paste0('INmodel_',j,'_control'))
    control_form = paste(control,collapse = " + ")
    form = paste0(outcome,'~',control_form) %>% as.formula
    df2 = df_imp[,c(outcome,control,'idcluster')]
    df2 = df2[complete.cases(df2[,grep('[^dmiss]',colnames(df2))]),]
    model = lm(formula = form,
               data = df2)
    cluster_model = coeftest(model,vcov=vcovCL,
                             cluster = ~idcluster)
    print(model$coefficients['dpisofirme'])
    cat("error:", cluster_model[2,2], "\n")
    if (j == 1) {
      cat("Control Group Mean:",summary(model)$coefficients[1,1], "\n")
      cat("Control Group SD:",summary(model)$coefficients[1,2] * sqrt(nrow(df2)), "\n")
    }
    assign(paste0(outcome,
                  '_INmodel_',
                  j), cluster_model)
  }
}

## Re-analysis







