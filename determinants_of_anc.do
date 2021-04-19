********************************************************************************************
*					  Predictors of Antenatal Care Usage in Bangladesh                     *
********************************************************************************************
*Last Edited 04/19/21 FN

cd "C:\Users\Saiqa\Desktop\deis\master's paper"

use "C:\Users\Saiqa\Desktop\deis\master's paper\data - dhs 17-18\BDIR7RDT - individual recode\BDIR7RFL.DTA"

*********************************************************************************
*			Creating Indicators/Recoding Variables of interest			        *
*********************************************************************************

*Note: All code for creating the following indiactors come from publicly available DHS code. Changes were only made according to coutnry specifications
*If variable starts with rh, it comes from individual recode survey i.e. variable measured at mother level

*var for "Person providing assistance during antenatal care (ANC)" [rh_anc_pv]
	gen rh_anc_pv = 6 if m2a_1! = .
	replace rh_anc_pv 	= 4 	if m2f_1 == 1 | m2g_1 == 1 | m2h_1 == 1 | m2i_1 == 1 | m2j_1 == 1 | m2k_1 == 1 | m2l_1 == 1 | m2m_1 == 1
	replace rh_anc_pv 	= 3 	if m2c_1 == 1 | m2d_1 == 1 | m2e_1 == 1
	replace rh_anc_pv 	= 2 	if m2b_1 == 1
	replace rh_anc_pv 	= 1 	if m2a_1 == 1
	replace rh_anc_pv 	= 5 	if m2a_1 == 9

	label define rh_anc_pv ///
	1 "Doctor" 		///
	2 "Nurse/midwife"	///
	3 "Other health worker" ///
	4 "TBA/other/relative"		///
	5 "Missing" ///
	6 "No ANC" 
	label val rh_anc_pv rh_anc_pv
	label var rh_anc_pv "Person providing assistance during ANC"
	
*var for "ANC by skilled provider" [rh_anc_pvskill]
	recode rh_anc_pv (1/3 = 1 "Skilled provider") (4/6 = 0 "Unskilled/no one") , gen(rh_anc_pvskill)
	label var rh_anc_pvskill "Skilled assistance during ANC"	

*var for "Number of ANC visits in 4 categories" [rh_anc_numvs]
	recode m14_1 (0=0 "none") (1=1) (2 3=2 "2-3") (4/90=3 "4+") (else=9 "don't know/missing"), gen(rh_anc_numvs)
	label var rh_anc_numvs "Number of ANC visits"

*var for "4+ ANC visits (dummy)" [rh_anc_4vs]
	recode rh_anc_numvs (1 2 9=0 "no") (3=1 "yes"), gen(rh_anc_4vs)
	lab var rh_anc_4vs "Attended 4+ ANC visits"
	
*var for any type of ANC [rh_anc_any]
	recode m2n_1 (0 = 1 "any type of care") (1 = 0 "no care"), gen (rh_anc_any)
	lab var rh_anc_any "Any type of ANC"

*var for asset quintile using pcfa 
	alpha v120 v121 v122 v123 v124 v125, std item //alpha = 0.9947; PCF justified
	factor v120 v121 v122 v123 v124 v125, pcf
	factor v120 v121 v122 v123 v124 v125, pcf factors (1) //only first factor kept as it was x5 larger than rest
	rotate 
	rotate, varimax factor(1)
	predict f1
	sum f1 //All 6 assets combined into one supra variable; as a z score, it has mean = 0 and SD = 1
	clonevar rh_asset_quntile = f1 
	drop f1
	
*var for whether respondent was a teenage mother at first birth
	gen rh_teenmom = .
	replace rh_teenmom = 1 if v212 <=18
	replace rh_teenmom = 0 if v212 >18 & v212<99999
	lab var rh_teenmom "Respondent was a teenager (<=18) at time of first birth"
	label define rh_teenmom 0 "not teen mom" 1 "teen mom"
	label values rh_teenmom rh_teenmom

*var for if respondent uses modern contraceptive
	gen rh_modern_methods = (v313 == 3)
	lab var rh_modern_methods "Respondent used modern methods"
	label define rh_modern_methods 0 "did not use modern methods" 1 "used modern methods"
	label values rh_modern_methods rh_modern_methods
	
*recoded var for muslim (dummy)
	gen muslim = .
	replace muslim = 1 if v130 == 1
	replace muslim = 0 if v130 == 2 | v130 == 3 | v130 == 4
	tab muslim v130 
	
*********************************************************************************
*		Keeping variables of interest to create smaller dataset			        *
*********************************************************************************	

keep caseid v002 v005 rh_anc_pv rh_anc_pvskill rh_anc_numvs rh_anc_4vs rh_anc_any rh_asset_quntile rh_teenmom rh_modern_methods v190 v190a v106 v701 v013 v101 v102 v130 v218 v212 v313 v012
save determinants_anc_truncated, replace

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

*********************************************************************************
* Adjusting for survey weights (only 1 weight since all data is from 1 survey)  *
*********************************************************************************
*creating weight variable (applied later)
gen wt = v005/1000000

*********************************************************************************
*							Univariate Analysis									*
*********************************************************************************

*Main dependent variable: ANC from skilled provider 
tab rh_anc_pvskill [iw = wt]

*Explanatory variables 
tab v012 [iw = wt] //mother's age group
tab v101 [iw = wt] //region (similar to states in US)
tab v102 [iw = wt] //urbanicity 
tab v190 [iw = wt] //wealth quintile 
tab v106 [iw = wt] //maternal education level
tab v701 [iw = wt] //paternal/husband's education level 
tab v130 [iw = wt] //religion
su v218 [iw = wt] //number of living children (treated as continuous)
tab rh_teenmom [iw = wt] //teenage mother 
tab rh_modern_methods [iw = wt] //use of modern contraceptives 

*Explanatory cross tabs 
su v012 if rh_anc_pvskill == 1 [iw = wt] //mothers age 
su v012 if rh_anc_pvskill == 0 [iw = wt]

tab v101 rh_anc_pvskill [iw = wt], co //region
tab v106  rh_anc_pvskill [iw = wt], co //mother edu
tab v701  rh_anc_pvskill [iw = wt], co //husband edu 
tab v190  rh_anc_pvskill [iw = wt], co //wealth quintile
tab v102  rh_anc_pvskill [iw = wt], co //urbanicity 
tab rh_teenmom rh_anc_pvskill [iw = wt], co // teen mom

su v218 if rh_anc_pvskill == 1 [iw = wt] //number of living children
su v218 if rh_anc_pvskill == 0 [iw = wt]

tab rh_modern_methods  rh_anc_pvskill [iw = wt], co //modern contraceptive 
tab muslim rh_anc_pvskill [iw = wt], co //muslim 

*Creating Table for ANC from skilled provider by explanatory vars
*age group
tab v013 rh_anc_pvskill [iw=wt], row  
*region
tab v101 rh_anc_pvskill [iw=wt], row  
*maternal education
tab v106 rh_anc_pvskill [iw=wt], row  
*paternal education
tab v701 rh_anc_pvskill [iw=wt], row  
*wealth quintile
tab v190 rh_anc_pvskill [iw=wt], row  
*urbanicity
tab v102 rh_anc_pvskill [iw=wt], row  
*teen mom
tab rh_teenmom rh_anc_pvskill [iw=wt], row  
*modern contraceptive 
tab rh_modern_methods rh_anc_pvskill [iw=wt], row 
*religion 
tab v130 rh_anc_pvskill [iw=wt], row  

*output to excel
tabout v101 v106 v701 v190 v102 v130 rh_teenmom rh_modern_methods rh_anc_pvskill using Table1_ANCskilled.xls [iw=wt] , c(row) f(1) replace

*********************************************************************************
*							Bivariate Analysis									*
*********************************************************************************

ttest v012, by (rh_anc_pvskill) //age vs anc ***signficiant
tab v101 rh_anc_pvskill, chi exp cchi2 //region vs anc ***signficant
tab v106 rh_anc_pvskill, chi exp cchi2 //maternal edu vs anc ***significant
tab v701 rh_anc_pvskill, chi exp cchi2 //paternal edu vs anc ***significant 
tab v190 rh_anc_pvskill, chi exp cchi2 //wealth quintile vs anc ***significant 
tab v102 rh_anc_pvskill, chi exp cchi2 //urbanicity vs anc ***significant 
tab rh_teenmom rh_anc_pvskill, chi exp cchi2 //teen mom vs anc ***significant 
ttest v218, by (rh_anc_pvskill) //no of children vs anc ***signficiant

tab rh_modern_methods rh_anc_pvskill, chi exp cchi2 //modern contra vs anc // NOT SIGNIFICANT
tab muslim rh_anc_pvskill, chi exp cchi2 // religion vs anc // NOT SIGNIFICANT 


*********************************************************************************
*							Multivariate Logit									*
*********************************************************************************
*unrestricted 
logit rh_anc_pvskill v012 i.v101 i.v106 i.v701 i.v190 v102 rh_teenmom v218 rh_modern_methods muslim [iw=wt]
estat ic // aic: 3776.706   bic: 3939.36
estat gof, group (10)

*v1 (dropped modern methods)
logit rh_anc_pvskill v012 i.v101 i.v106 i.v701 i.v190 v102 rh_teenmom v218 muslim [iw=wt]
estat ic // 3774.708    3930.86
estat gof, group (10)

*v2 (dropped modern methods & teen mom)
logit rh_anc_pvskill v012 i.v101 i.v106 i.v701 i.v190 v102 v218 muslim [iw=wt]
estat ic // 3772.742   3922.388
estat gof, group (10)

*v3 (dropped modern methods & teen mom & muslim)
logit rh_anc_pvskill v012 i.v101 i.v106 i.v701 i.v190 v102 v218 [iw=wt]
estat ic //  3770.761   3913.901
estat gof, group (10)

*v4 (dropped modern methods & teen mom & muslim & region) <<<<<<<<<<<<<<<<<<-------------------preferred restricted model
logit rh_anc_pvskill v012 i.v106 i.v701 i.v190 v102 v218 [iw=wt]
estat ic // 3785.773   3883.368
estat gof, group (10)

*odds ratios
logistic rh_anc_pvskill v012 i.v106 i.v701 i.v190 v102 v218 [iw=wt]

*robustness analysis
probit rh_anc_pvskill v012 i.v106 i.v701 i.v190 v102 v218 [iw=wt]
