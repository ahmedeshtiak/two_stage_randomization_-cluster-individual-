/*******************************************************************************
Project:		Randomization Assignment 
Organization:	BIGD, BracU
Author:			Ahmed Eshtiak
Date created:	20/02/2026
Last edited:	23/02/2026
Last edited by: Ahmed Eshtiak
Description:	
				
	
*******************************************************************************/

**# Necessary setup
clear all
set more off
version 17
set maxvar 50000
cap log close _all
cap estimates drop _all

**# Directory setup
global randomization "E:\Books and Notes\Lectures\STATA\Randomization Johirul Bhai\assignment"
global RAW      "${randomization}\01_raw"
global CLEAN    "${randomization}\02_clean"
global DO       "${randomization}\03_do"
global RESULT   "${randomization}\04_result"

*--------------------------------------------------------------
**# Load data and drop unintended clusters
*--------------------------------------------------------------
use "${RAW}\radomization_dataset.dta", clear
destring new_cluster, replace force
gen cluster_id = zone_code*1000 + new_cluster

* Tag unique mothers per cluster
bysort cluster_id id: gen tagid = (_n == 1)
bysort cluster_id: egen cluster_mothers = total(tagid)

* Drop clusters with fewer than 16 mothers
keep if cluster_mothers >= 16
save "${CLEAN}\eligible_clusters.dta", replace

*--------------------------------------------------------------
**#Cluster-level randomisation: 150 treatment vs. 157 control
*--------------------------------------------------------------
use "${CLEAN}\eligible_clusters.dta", clear
bysort cluster_id: keep if _n == 1         // one record per cluster
set seed 12345
gen rand = runiform()
sort rand
gen treat_cluster = (_n <= 150)
keep cluster_id treat_cluster
save "${CLEAN}\cluster_assignment.dta", replace

* Merge treatment indicator back to full eligible dataset
use "${CLEAN}\eligible_clusters.dta", clear
merge m:1 cluster_id using "${CLEAN}\cluster_assignment.dta", nogen

*--------------------------------------------------------------
**#Individual-level randomisation (only within treat clusters)
*--------------------------------------------------------------
* Prepare a dataset of unique mothers in treatment clusters
use "${CLEAN}\eligible_clusters.dta", clear
merge m:1 cluster_id using "${CLEAN}\cluster_assignment.dta", nogen
keep if treat_cluster == 1                  // only treatment clusters
bysort cluster_id id: keep if _n == 1       // unique mothers

* Assign random ranks within each treatment cluster
set seed 54321
gen rand = runiform()
bysort cluster_id (rand): gen rank = _n

* Treatment Assignment
gen individual_arm = ""
bysort cluster_id: replace individual_arm = "only info" if treat_cluster == 1
bysort cluster_id: replace individual_arm = "info plus cash" if treat_cluster == 1 & rank > 8 & rank <= 12
bysort cluster_id: replace individual_arm = "info plus fees paid" if treat_cluster == 1 & rank > 12 & rank <= 16

* Save individual assignments
keep cluster_id id individual_arm
save "${CLEAN}\individual_arm_assignment.dta", replace

*--------------------------------------------------------------
**# Merge individual assignments and finalise dataset
*--------------------------------------------------------------
use "${CLEAN}\eligible_clusters.dta", clear
merge m:1 cluster_id using "${CLEAN}\cluster_assignment.dta", nogen
merge m:1 cluster_id id using "${CLEAN}\individual_arm_assignment.dta", nogen

* pure control cluster
replace individual_arm = "pure control" if treat_cluster == 0

* Order variables for clarity
order cluster_id cluster_mothers treat_cluster individual_arm id

* Save final randomised dataset
save "${CLEAN}\randomized_final_data.dta", replace


************************************************************
**# Data Cleaning and Variable Deriving for Balance Test***
************************************************************

* mother and child age
ren selected_ch_age child_age
ren res_age mother_age

destring mother_age, replace
destring res_marr, replace
rename res_marr marital_status

* mothers marrtial status
label define marital_status 1 "Never Married" 2 "Divorced" 3 "Married, living with spouse" 4"Separated" 5 "Widowed" 6 "Abandoned"

label values marital_status marital_status

gen married_mothers = (marital_status == 3)

* Generate binary variables for employment status, education level, and food availability
rename c11 employment 
rename b11_1 education_level
rename d11 food_availability 

gen employed = 0
replace employed = 1 if employment == 1 | education_level == 2 | education_level == 8 | education_level == 9

gen educated = 0 
replace educated = inrange(education_level, 5, 14)

gen yearly_food_availability  = inlist(food_availability , 3, 4)
gen monthly_food_availability  = (food_availability  == 1)

* child's sickness 
ren d21 child_sick
ren d25 child_age_birth

gen early_child_bear = 0
replace early_child_bear = 1 if child_age_birth <= 18


* daycare 
ren e111 daycare_wtp
recode daycare_wtp (-666 = .)

* Children under 9
gen under9_b6_1 = b6_1 < 9
gen under9_b6_2 = b6_2 < 9
gen under9_b6_3 = b6_3 < 9
egen under9 = rowtotal(under9_b6_1 under9_b6_2 under9_b6_3), missing

* children under 15
gen under15_b6_1 = b6_1 < 15
gen under15_b6_2 = b6_2 < 15
gen under15_b6_3 = b6_3 < 15
egen under15 = rowtotal(under15_b6_1 under15_b6_2 under15_b6_3), missing

* Household size 
destring lino_1, replace
destring lino_2, replace
destring lino_3, replace

gen hh_size = 0
replace hh_size = hh_size + (lino_1 != .)
replace hh_size = hh_size + (lino_2 != .)
replace hh_size = hh_size + (lino_3 != .)

* total income 
destring tot_income, replace
ren tot_income hh_month_hh
gen hh_per_capita =  hh_month_hh / hh_size

* Household expenditure
mdesc 
gen expenditure_household = i21 + i210 + i211 + i212 + i213 + i214 + i215 + i216 + i217 + i218 + i219

* savings 
rename j1 savings
*debt
rename j5 debt_loans

**# Principal Component Analysis
pca k_item_1 k_item_10 k_item_11 k_item_12 k_item_13
predict asset_score, score
xtile quintile = asset_score, nquantiles(5) // generate quantile
sum asset_score, detail
gen non_poor = (asset_score > `r(p50)')

fre non_poor
label define non_poor 0 "Poor" 1 "Non Poor"
label values non_poor non_poor
fre non_poor

save "${CLEAN}\randomized_final_data.dta", replace


**********************
**#Balance Test*******
**********************

* Define the treatment assignment variable
tab individual_arm, gen (treat)
ren treat1 info_cash
ren treat2 info_fees
ren treat3 info_only


* Create a list of baseline characteristics
local baseline_vars child_age mother_age marital_status employed educated yearly_food_availability monthly_food_availability child_sick child_age_birth early_child_bear daycare_wtp hh_month_hh hh_per_capita under9 under15 hh_size expenditure_household savings debt_loans non_poor

* Balance test 
foreach var of local baseline_vars {
    regress `var' i.info_cash i.info_fees i.info_only i.zone_code, cluster(cluster_id)
}

**********************************************
**#Exporting The Result of Balance Test********
**********************************************

*Initialize the Excel file 
putexcel set "$RESULT/balance_table_test.xlsx", replace sheet("Balance Test")

* Title row (row 1)
putexcel A1 = ("Balance Test Results")
putexcel A1:E1, bold overwritefmt

* Headers (row 2)
putexcel A2 = ("Baseline Characteristics")
putexcel B2 = ("Info and Cash")
putexcel C2 = ("Info and Fees")
putexcel D2 = ("Info Only")
putexcel E2 = ("Joint F-test ")

putexcel A2:E2, bold vcenter hcenter border(bottom, thin)

local row = 3

* Balance test quietly 
foreach var of local baseline_vars {
    
    * Run the regression quietly
    quietly regress `var' i.info_cash i.info_fees i.info_only i.zone_code, cluster(cluster_id)
    
    * Store the variable name (column A)
    putexcel A`row' = ("`var'")
    
    quietly test 1.info_cash //test each treatment arm (p-values)
    putexcel B`row' = (r(p))
    
    quietly test 1.info_fees
    putexcel C`row' = (r(p))
    
    quietly test 1.info_only
    putexcel D`row' = (r(p))
    
    
    quietly test 1.info_cash 1.info_fees 1.info_only //Joint F-test
    putexcel E`row' = (r(p))
    
    * Increment row
    local ++row
}

* formatting
local data_rows = `=`row'-1'
putexcel A3:A`data_rows', font("Calibri", 11) 



* Adjust Column Widths using Mata
mata:
    
    b = xl()
    
    b.load_book("$RESULT/balance_table_test.xlsx") // load file
    
    b.set_sheet("Balance Test")
    
    b.set_column_width(1, 1, 38)
    
    b.set_column_width(2, 4, 18)
    
    b.set_column_width(5, 5, 22)
    
    b.close_book()
end

save "${CLEAN}\randomized_final_data.dta", replace

