# package & data imports
library(haven)
library(dplyr)
house <- read_dta('data/PisoFirme_AEJPol-20070024_household.dta')
indiv <- read_dta('data/PisoFirme_AEJPol-20070024_individual.dta')

## variable definitions for household dataframe
HH_census <- house[,grep("C_*", colnames(house))] 
HH_survey <- house[c("S_HHpeople", "S_headage", "S_spouseage", "S_headeduc", "S_spouseeduc",
                     "S_rooms", "S_waterland", "S_waterhouse", "S_electricity", "S_cementfloor2000",
                     "S_hasanimals", "S_animalsinside", "S_garbage", "S_washhands",
                     "S_incomepc", "S_assetspc", "S_shpeoplework", "S_microenter", "S_hrsworkedpc", "S_consumptionpc" ,
                     "S_cashtransfers", "S_milkprogram", "S_foodprogram")]
HH_demog1 <- house[c("S_HHpeople", "S_headage", "S_spouseage", "S_headeduc", "S_spouseeduc")]
HH_demog2 <- house[,grep("S_dem*", colnames(house))] 
HH_health <- house[c("S_waterland", "S_waterhouse", "S_electricity", "S_hasanimals", 
                     "S_animalsinside", "S_garbage", "S_washhands")]
HH_econ <- house[c("S_incomepc","S_assetspc")]
HH_social <- house[c("S_cashtransfers", "S_milkprogram", "S_foodprogram", "S_seguropopular")]
HH_floor <- house[c("S_shcementfloor", "S_cementfloorkit", 
                    "S_cementfloordin", "S_cementfloorbat", "S_cementfloorbed")]
HH_satis <- house[c("S_satisfloor", "S_satishouse", "S_satislife", "S_cesds", "S_pss")]
HH_robust <- house[c("S_instcement", "S_instsanita", "S_restsanita", "S_constceili",
                    "S_restowalls", "S_improveany", "S_logrent", "S_logsell", "S_consumptionpc")]

## Table 1: Description of Outcome Variables and Sample Sizes in 2005 Survey
#tabstat $HH_floor $HH_satis $HH_robust if idcluster!=., by(dpisofirme) s(count);
t1 <- cbind(house$idcluster, house$dpisofirme, HH_floor,HH_satis, HH_robust) %>%
  na.rm()

## Table 2: Difference of Means for Pre-intervention 2000 Census Variables
t2 <- 

## Table 3: Difference of Means for Independent Variables in 2005 Survey
t3

## Table 4: Cement Floor Coverage Measures
t4

## Table 5: N/A
  
## Table 6: Satisfaction and Maternal Mental Health Measures

  
## Table 7: Robustness Checks


## variable definitions for individual-level dataframe
CH_survey <- indiv[c("S_age", "S_gender", "S_childma", "S_childmaageå", "S_childmaeduc", 
                     "S_childpa", "S_childpaage", "S_childpaeduc")]
CH_demog <- indiv[c("S_HHpeople", "S_rooms", "S_age", "S_gender", "S_childma", "S_childmaage", 
              "S_childmaeduc", "S_childpa", "S_childpaage", "S_childpaeduc")]
CH_health <- indiv[c("S_parcount", "S_diarrhea", "S_anemia", 
               "S_mccdts", "S_pbdypct", "S_haz", "S_whz")]
CH_robust <- indiv[c("S_respira", "S_skin", "S_otherdis")]
PA_robust <- indiv[c("S_malincom", "S_palincom")]

## Table 1: Description of Outcome Variables and Sample Sizes in 2005 Survey
#tabstat $CH_health $CH_robust $PA_robust if idcluster!=., by(dpisofirme) s(count);
t1 <- cbind(indiv$dpisofirme, indiv$idcluster, CH_health, CH_robust, PA_robust)


## Table 2: N/A
## Table 3: Difference of Means for Independent Variables in 2005 Survey
## Table 4: Difference of Means for Independent Variables in 2005 Survey
## Table 5: Child Health Measures
## Table 6: N/A
## Table 7: Robustness Checks










