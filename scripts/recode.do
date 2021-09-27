clear

cls

use "data/Omnibus_Wave9_JULY2021_without_NR_03.08.2021.dta"

// use "https://caucasusbarometer.org/downloads/NDI_2021_Jul_28.07.21_Public.dta"

svyset id [pweight=wt]

gen ownership=0
foreach var of varlist d4_* {
replace ownership = ownership + 1 if `var'== 1
}

recode d3 (-2 1/3=0) (4/5=1), gen(hied)

recode d7 (3=1)(.=.)(else=0), gen(ethnic)

recode d9 (98=5)

recode m4 (1=1 "Government")(-5 = 3 "Unaffiliated") (-2 -1 = 4 "DK/RA") (-9 -3 =.) (nonmissing=2 "Opposition"), gen(party)


quietly: ds d1-p9_7, has(type int byte long float double)

local vlist `r(varlist)'

foreach var of varlist `vlist' {
	recode `var' (-9 -3 -7 =.) (-2/-1=98) (-5 = 97)
}

quietly: label dir

local lablist `r(names)'

foreach x in `lablist' {
	lab def `x' 98 "DK/RA" 97 "Not applicable", modify
}

// recode q20 (-5=3) (-9 -3=.) (1=1) (0=0) (-2/-1=98)

recode age (18/34=1 "18-34") (35/54=2 "35-54") (nonmissing=3 "55+"), gen(agegroup)

svy: mlogit p8 i.sex i.agegroup i.stratum hied ethnic ownership i.party i.d9


// Test parameters of significant covariates

foreach x of varlist agegroup stratum party {

testparm i.`x'

}

// Export tables


foreach var of varlist p2_* p3_* p4_* p8 {

tabout `var' using "tables/frequency/`var'.csv", c(column) clab(prop) svy replace percent ptotal(none)

}

foreach var of varlist p5 p6 p7 p9_* {

tabout `var' using "tables/frequency/`var'.csv" if stratum == 1, c(column) clab(prop) svy replace percent ptotal(none)

}

foreach x in agegroup stratum party {

tabout p8 `x' using "tables/crosstabs/p8_`x'.csv", c(column) svy replace percent ptotal(none) h1(nil) h3(nil)

}

clear

cls

use "data/May17_Only_responses_10072013.dta"


quietly: ds q1-d22, has(type int byte long float double)

local vlist `r(varlist)'

foreach var of varlist `vlist' {
	recode `var' (-9 -3 -7 =.) (-2/-1=98) (-5 = 97)
}

quietly: label dir

local lablist `r(names)'

foreach x in `lablist' {
	lab def `x' 98 "DK/RA" 97 "Not applicable", modify
}


svyset psu [pweight=indwt], strata(stratum) fpc(npsuss) singleunit(certainty) || id, fpc(nhhpsu) || _n, fpc(nadhh)

svy: prop q25

foreach var of varlist q25 {

tabout `var' using "tables/frequency/p8_2013.csv", c(column) clab(prop) svy replace percent ptotal(none)

}


foreach var of varlist q5 q6 {

tabout `var' using "tables/frequency/`var'_2013.csv", c(column) clab(prop) svy replace percent ptotal(none)

}
