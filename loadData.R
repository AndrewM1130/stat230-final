library(haven)
library(skimr)
library(dplyr)

# loading in both household and individual-level datasets
house <- read_dta('data/PisoFirme_AEJPol-20070024_household.dta')
indiv <- read_dta('data/PisoFirme_AEJPol-20070024_individual.dta')

# rows containing missing/null/NA values
missing_house <- house[rowSums(is.na(house)) > 0,] %>% nrow()
missing_indiv <- indiv[rowSums(is.na(indiv)) > 0,] %>% nrow()

# skim-summary of both tables
tt_indiv <- skim(indiv)
tt_house <- skim(house)


# create summary dataframe for response & covariates
response = tt_indiv[c(17,18,19,20,21,22,23), c(1,2,10,11,17,13,15)]

print(response)

covariates = tt_indiv[c(2:16,69:89),
                      c(1,2,10,11,17,13,15)]
print(covariates)
