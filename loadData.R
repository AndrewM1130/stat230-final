library(haven)
library(skimr)

house <- read_dta('114542-V1/PisoFirme_AEJPol-20070024_household.dta')
indiv <- read_dta('114542-V1/PisoFirme_AEJPol-20070024_individual.dta')


test <- house[rowSums(is.na(house)) > 0,]
test2 <- indiv[rowSums(is.na(indiv)) > 0,]


tt_indiv <- skim(indiv)
tt_indiv = tt_indiv[c(),
                    c(2,5,6,8:10,12)]
print(tt_indiv)


tt_house <- skim(house)
tt_house = tt_house[c(1,8,22,23,24,27,28,29,38,50,68:72),
                    c(2,5,6,8:10,12)]
print(tt_house)
