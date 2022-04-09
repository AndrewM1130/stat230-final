library(haven)
library(skimr)

house <- read_dta('114542-V1/PisoFirme_AEJPol-20070024_household.dta')
indiv <- read_dta('114542-V1/PisoFirme_AEJPol-20070024_individual.dta')


test <- house[rowSums(is.na(house)) > 0,]
test2 <- indiv[rowSums(is.na(indiv)) > 0,]


tt <- skim(indiv)
print(tt)


tt <- skim(house)
print(tt)
