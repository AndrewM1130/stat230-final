#delimit;
clear;
version 9;
set mem 100m;
cap log close;
set matsize 800;
program drop _all;

************************************************************************************************************************************************************;
** Load Program for Regressions;
************************************************************************************************************************************************************;
program runreg, rclass;
    args outcome controls;
    reg `outcome' dpisofirme `controls', cl(idcluster); matrix b=e(b); matrix V=e(V);
    sum `outcome' if e(sample) & dpisofirme==0;
    display _newline(1) as text "Coeff              = " as res round(b[1,1]*1000)/1000;
    display _newline(1) as text "Std.Err.           = " as res round(sqrt(V[1,1])*1000)/1000;
    display _newline(1) as text "100*Coeff/ContMean = " as res round(100*b[1,1]/r(mean)*1000)/1000;
        ** Compute Moran's I Statistic for mean predicted residuals BETWEEN clusters;
        qui{;
        preserve;
        predict resid if e(sample), resid; local varsinreg: colfullnames b; 
        gen cons=1; local varsinreg: subinstr local varsinreg "_cons" "cons";
        keep idcluster coord_x coord_y resid `varsinreg'; drop if coord_x==. | resid==.;
        collapse (mean) resid coord_x coord_y `varsinreg', by(idcluster);
        order idcluster coord_x coord_y resid;
        do "$path/PisoFirme-MATA-MoransI.do";
        restore;
        };
        display _newline(1) as text "Moran's I Test Statistic = " as res moransI;
end;

************************************************************************************************************************************************************;
** PATH SETUP: Modify path to access databases here (e.g., path="C:");
************************************************************************************************************************************************************;
gl path="C:";

************************************************************************************************************************************************************;
** HOUSEHOLD LEVEL REGRESSIONS;
************************************************************************************************************************************************************;
use "$path/PisoFirme_AEJPol-20070024_household.dta", clear;

****************************************************************;
** Variables Definitions:
****************************************************************;
gl HH_census  C_blocksdirtfloor C_HHdirtfloor C_child05 C_households C_people C_rooms C_HHpersons C_waterland C_waterhouse C_waterbath C_gasheater 
              C_refrigerator C_washing C_telephone C_vehicle C_overcrowding C_poverty C_illiterate C_headeduc C_dropouts515 C_employment C_earnincome; 
gl HH_survey  S_HHpeople S_headage S_spouseage S_headeduc S_spouseeduc
              S_rooms S_waterland S_waterhouse S_electricity S_cementfloor2000
              S_hasanimals S_animalsinside S_garbage S_washhands
              S_incomepc S_assetspc S_shpeoplework S_microenter S_hrsworkedpc S_consumptionpc 
              S_cashtransfers S_milkprogram S_foodprogram;
gl HH_demog1  S_HHpeople S_headage S_spouseage S_headeduc S_spouseeduc;
gl HH_demog2  S_dem1 S_dem2 S_dem3 S_dem4 S_dem5 S_dem6 S_dem7 S_dem8;
gl HH_health  S_waterland S_waterhouse S_electricity S_hasanimals S_animalsinside S_garbage S_washhands; 
gl HH_econ    S_incomepc S_assetspc;
gl HH_social  S_cashtransfers S_milkprogram S_foodprogram S_seguropopular;
gl HH_floor   S_shcementfloor S_cementfloorkit S_cementfloordin S_cementfloorbat S_cementfloorbed;
gl HH_satis   S_satisfloor S_satishouse S_satislife S_cesds S_pss;
gl HH_robust  S_instcement S_instsanita S_restsanita S_constceili S_restowalls S_improveany S_logrent S_logsell S_consumptionpc;

****************************************************************;
** Sample Sizes and Mean Tests (Tables 1, Table 2, Table 3);
****************************************************************;
** Table 1: Description of Outcome Variables and Sample Sizes in 2005 Survey;
tabstat $HH_floor $HH_satis $HH_robust if idcluster!=., by(dpisofirme) s(count);

** Table 2: Difference of Means for Pre-intervation 2000 Census Variables;
gen mzalevel=0; bys idcluster idmza: replace mzalevel=1 if _n==1;
svyset idcluster;
foreach var of global HH_census {;
    svy : mean `var' if mzalevel==1, over(dpisofirme);
    lincom [`var']1 - [`var']0;
};

** Table 3: Difference of Means for Independent Variables in 2005 Survey;
svyset idcluster;
foreach var of global HH_survey {;
    svy : mean `var', over(dpisofirme);
    lincom [`var']1 - [`var']0;
};

****************************************************************;
** Missing Values and Regressions (Tables 4, Table 6, Table 7);
****************************************************************;
** Missing Values Imputations;
foreach x in HH_demog1 HH_demog2 HH_health HH_econ {;
    local miss=0;
    foreach y of global `x' {;
        local ++miss;
        gen dmiss_`x'_`miss'=(`y'==.);
        replace `y'=0 if `y'==.;
    };
};
gen dmiss_S_cashtransfers=(S_cashtransfers==.); replace S_cashtransfers=0 if S_cashtransfers==.;

** Define Models for Regressions;
gl HHmodel_1 ;
gl HHmodel_2 $HH_demog1 dmiss_HH_demog1* $HH_demog2 dmiss_HH_demog2* $HH_health dmiss_HH_health*;
gl HHmodel_3 $HH_demog1 dmiss_HH_demog1* $HH_demog2 dmiss_HH_demog2* $HH_health dmiss_HH_health* $HH_social dmiss_S_cash*;
gl HHmodel_4 $HH_demog1 dmiss_HH_demog1* $HH_demog2 dmiss_HH_demog2* $HH_health dmiss_HH_health* $HH_social dmiss_S_cash* $HH_econ dmiss_HH_econ*;

** Table 4: Cement Floor Coverage Measures;
foreach var of global HH_floor {;
    forvalues m = 1(1)4 {;
        display _newline(10) as err "Variable = `var' | Model = " `m';
        runreg `var' "${HHmodel_`m'}";
    };
};
** Table 6: Satisfaction and Maternal Mental Health Measures;
foreach var of global HH_satis {;
    forvalues m = 1(1)4 {;
        display _newline(10) as err "Variable = `var' | Model = " `m';
        runreg `var' "${HHmodel_`m'}";
    };
};
** Table 7: Robustness Checks;
foreach var of global HH_robust {;
    forvalues m = 1(1)4 {;
        display _newline(10) as err "Variable = `var' | Model = " `m';
        runreg `var' "${HHmodel_`m'}";
    };
};

************************************************************************************************************************************************************;
** INDIVIDUAL LEVEL REGRESSIONS;
************************************************************************************************************************************************************;
use "$path/PisoFirme_AEJPol-20070024_individual.dta", clear;

****************************************************************;
** Variables Definitions:
****************************************************************;
gl CH_survey  S_age S_gender S_childma S_childmaage S_childmaeduc S_childpa S_childpaage S_childpaeduc;
gl CH_demog   S_HHpeople S_rooms S_age S_gender S_childma S_childmaage S_childmaeduc S_childpa S_childpaage S_childpaeduc;
gl CH_health  S_parcount S_diarrhea S_anemia S_mccdts S_pbdypct S_haz S_whz;
gl CH_robust  S_respira S_skin S_otherdis;
gl PA_robust  S_malincom S_palincom;

****************************************************************;
** Sample Sizes and Mean Tests (Tables 1, Table 3);
****************************************************************;
** Table 1: Description of Outcome Variables and Sample Sizes in 2005 Survey;
tabstat $CH_health $CH_robust $PA_robust if idcluster!=., by(dpisofirme) s(count);

** Table 3: Difference of Means for Independent Variables in 2005 Survey;
svyset idcluster;
foreach var of global CH_survey {;
    svy : mean `var' if S_age<6, over(dpisofirme);
    lincom [`var']1 - [`var']0;
};

****************************************************************;
** Missing Values and Regressions (Tables 4, Table 6, Table 7);
****************************************************************;
** Missing Values Imputations;
foreach x in CH_demog HH_health HH_econ {;
    local miss=0;
    foreach y of global `x' {;
        local ++miss;
        gen dmiss_`x'_`miss'=(`y'==.);
        replace `y'=0 if `y'==.;
    };
};
gen dmiss_S_cashtransfers=(S_cashtransfers==.); replace S_cashtransfers=0 if S_cashtransfers==.;

** Define Models for Regressions;
gl INmodel_1 ;
gl INmodel_2 $CH_demog dmiss_CH_demog* dtriage* $HH_health dmiss_HH_health*;
gl INmodel_3 $CH_demog dmiss_CH_demog* dtriage* $HH_health dmiss_HH_health* $HH_social dmiss_S_cash*;
gl INmodel_4 $CH_demog dmiss_CH_demog* dtriage* $HH_health dmiss_HH_health* $HH_social dmiss_S_cash* $HH_econ dmiss_HH_econ*;

** Table 5: Child Health Measures;
foreach var of global CH_health {;
    forvalues m = 1(1)4 {;
        display _newline(2) as err "Variable = `var' | Model = " `m';
        runreg `var' "${INmodel_`m'}";
    };
};

** Table 7: Robustness Checks;
foreach var of global CH_robust {;
    forvalues m = 1(1)4 {;
        display _newline(10) as err "Variable = `var' | Model = " `m';
        runreg `var' "${INmodel_`m'}";
    };
};

foreach var of global PA_robust {;
    forvalues m = 1(1)4 {;
        display _newline(10) as err "Variable = `var' | Model = " `m';
        runreg `var' "${INmodel_`m'}";
    };
};
