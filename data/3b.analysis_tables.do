* Project: Sundarbans 
* Started: Feb 2020 [Heng Zhu]
* Last modified: 10/26/20 [Miki Doan]
* Purpose: Analysis with tables 
* Stat v.15
	
********************************************************************************
* 0. Select the dataset
********************************************************************************
	use "$data/dataset_analysis.dta", clear 
********************************************************************************
* 1. Setup clusters 
********************************************************************************	
	egen cluster_id = group(week village)
	xtset hhid week
	order cluster_id, after(week)
	
********************************************************************************
* Figure 1: Segmented Regression
********************************************************************************

	/* Running Segmented Regression

	For this we'll need a time_after variable, so:

	| covid_d | week | time_after_covid |
	|---------|------|------------------|
	| 0	  	  | 1    | 0                |
	| 0       | 2    | 0                |
	| 1       | 3    | 1                |
	| 1       | 4    | 2                |

	*/
	
	label variable local_inc "Local Income"
	label variable total_spending "Total Expenditure"
	label variable rmt_total "Remittance Income"
	label variable aggregated_cs_essentials "Total Consumption Expenditure on Essential Items"
	label variable week "Week"

	// Creating Scatter plots with trend lines 
	local graph_outcomes "local_inc total_spending rmt_total aggregated_cs_essentials"

	foreach var of local graph_outcomes {

		egen `var'_mean = mean(`var'), by(week)

		sum `var'_mean, detail

		gen tag = inrange(`var'_mean, r(p1), r(p99)) == 1
		
		twoway 	(scatter `var'_mean week, mcolor(grey) xlab( ,format(%td))) ///
				(lfit `var'_mean week if covid_d==0 , xlab( ,format(%td)) clcolor(grey) clpattern(dash) sort) ///
			(lfit `var'_mean week if covid_d==1 , xlab( ,format(%td)) clcolor(grey) clpattern(dash) sort) ///
			, name(`var'_graph, replace) ///
			title(`: variable label `var'') ///
			xlabel(0 "Nov. 27, 2018" 13 "Feb. 26, 2019" 26 "May 28, 2019" 39 "Aug. 27, 2019" 51 "Apr. 5, 2020") ///
			legend(off)	

		drop tag

		local graphs "`graphs' `var'_graph"

		graph export "$figures/`var'_scatter.png", replace
	}
	
	
********************************************************************************
* Table 5: Marginal Impacts of COVID-19 Lockdown on Local Income,
*				Total Expenditure, and Remittance Income
********************************************************************************			

	putexcel set "$tables/analysis_tables.xlsx", sheet("table_5") modify
	
	local row = 3
	
	forval i = 1/10 {
		local row_`i' = `row' + `i'
	}

	putexcel B2=("Table 5: Marginal Impacts of COVID-19 Lockdown on Local Income, Total Expenditure, and Remittance Income"), bold 
	putexcel (B2:F2), merge hcenter vcenter 
	
	putexcel B`row'=("Compared to") C`row'=("Changes in") D`row'= ("Local income (INR)") E`row'= ("Total expenditure (INR)") F`row'= ("Remittance income (INR)"), txtwrap
 	putexcel B`row':F`row', border(bottom)
	putexcel B`row', italic
	
	putexcel B`row_1' = "Full Baseline (Nov'18-Oct'19)             NxT = 15,726", txtwrap
	putexcel (B`row_1':B`row_5'), merge vcenter
	putexcel (B`row_5':F`row_5'), border(bottom)
	putexcel B`row_6' = "Restricted Baseline (Mar'19-April'19) NxT = 2,293", txtwrap
	putexcel (B`row_6':B`row_10'), merge vcenter 

	putexcel C`row_1' = "Levels", txtwrap
	putexcel C`row_3' = "Percentage", txtwrap italic
	putexcel C`row_4' = "Baseline Mean", txtwrap italic
	putexcel C`row_5' = "R2 (within)", txtwrap italic
	
	putexcel C`row_6' = "Levels", txtwrap
	putexcel C`row_8' = "Percentage", txtwrap italic
	putexcel C`row_9' = "Baseline Mean", txtwrap italic
	putexcel C`row_10' = "R2 (within)", txtwrap italic
	
	putexcel (B`row_10':F`row_10'), border(bottom)

	// Local income 
		sum local_inc if covid_d == 0 
			putexcel D`row_4' = (r(mean)), nformat(number_sep_d2)
			scalar full_mean_inc = r(mean)
			
		sum local_inc if covid_d == 0 & MarchApril == 1 
			putexcel D`row_9' = (r(mean)), nformat(number_sep_d2)
			scalar rtd_mean_inc = r(mean)
		
		xtreg local_inc covid_d MarchApril pre_covid_full post_covid, fe cluster(cluster_id) nonest
		lincom covid_d+MarchApril
			putexcel D`row_1' = (r(estimate)), nformat(number_sep_d2)
				scalar temp = r(estimate)
				scalar temp1 = temp/full_mean_inc
			putexcel D`row_2' = (r(se)), nformat((0.00))
			putexcel D`row_3' = temp1, nformat(percent) italic
			putexcel D`row_5' = (e(r2_w)), nformat(0.000)
		
		xtreg local_inc covid_d if MarchApril == 1, fe cluster(cluster_id) nonest
			mat a = r(table)
			putexcel D`row_6' = a[1,1],  nformat(number_sep_d2)
				scalar temp = a[1,1]
				scalar temp1 = temp/rtd_mean_inc
			putexcel D`row_7' = a[2,1], nformat((0.00))
			putexcel D`row_8' = temp1, nformat(percent) italic
			putexcel D`row_10' = (e(r2_w)), nformat(0.000)
		
	// Total expenditure 
		sum total_spending if covid_d == 0 
			putexcel E`row_4' = (r(mean)), nformat(number_sep_d2)
			scalar full_mean_spending = r(mean)
			
		sum total_spending if covid_d == 0 & MarchApril == 1 
			putexcel E`row_9' = (r(mean)), nformat(number_sep_d2)
			scalar rtd_mean_spending = r(mean)
		
		xtreg total_spending covid_d MarchApril pre_covid_full post_covid, fe cluster(cluster_id) nonest
		lincom covid_d+MarchApril
			putexcel E`row_1' = (r(estimate)), nformat(number_sep_d2)
				scalar temp = r(estimate)
				scalar temp1 = temp/full_mean_spending
			putexcel E`row_2' = (r(se)), nformat((0.00))
			putexcel E`row_3' = temp1, nformat(percent) italic
			putexcel E`row_5' = (e(r2_w)), nformat(0.000)
		
		xtreg total_spending covid_d if MarchApril == 1, fe cluster(cluster_id) nonest
			mat a = r(table)
			putexcel E`row_6' = a[1,1],  nformat(number_sep_d2)
				scalar temp = a[1,1]
				scalar temp1 = temp/rtd_mean_spending
			putexcel E`row_7' = a[2,1], nformat((0.00))
			putexcel E`row_8' = temp1, nformat(percent) italic
			putexcel E`row_10' = (e(r2_w)), nformat(0.000)
			
	// Remittance income 
		sum rmt_total if covid_d == 0 
			putexcel F`row_4' = (r(mean)), nformat(number_sep_d2)
			scalar full_mean_rmt = r(mean)
			
		sum rmt_total if covid_d == 0 & MarchApril == 1 
			putexcel F`row_9' = (r(mean)), nformat(number_sep_d2)
			scalar rtd_mean_rmt = r(mean)
		
		xtreg rmt_total covid_d MarchApril pre_covid_full post_covid, fe cluster(cluster_id) nonest
		lincom covid_d+MarchApril
			putexcel F`row_1' = (r(estimate)), nformat(number_sep_d2)
				scalar temp = r(estimate)
				scalar temp1 = temp/full_mean_rmt
			putexcel F`row_2' = (r(se)), nformat((0.00))
			putexcel F`row_3' = temp1, nformat(percent) italic
			putexcel F`row_5' = (e(r2_w)), nformat(0.000)
		
		xtreg rmt_total covid_d if MarchApril == 1, fe cluster(cluster_id) nonest
			mat a = r(table)
			putexcel F`row_6' = a[1,1],  nformat(number_sep_d2)
				scalar temp = a[1,1]
				scalar temp1 = temp/rtd_mean_rmt
			putexcel F`row_7' = a[2,1], nformat((0.00))
			putexcel F`row_8' = temp1, nformat(percent) italic
			putexcel F`row_10' = (e(r2_w)), nformat(0.000)			

********************************************************************************
* Table 6: Marginal Impacts of COVID-19 Lockdown on Consumption Variables 
********************************************************************************			

	putexcel set "$tables/analysis_tables.xlsx", sheet("table_6") modify
	
	local row = 3
	
	forval i = 1/10 {
		local row_`i' = `row' + `i'
	}

	putexcel B2=("Table 6: Marginal Impacts of COVID-19 Lockdown on Consumption Variables"), bold 
	putexcel (B2:F2), merge hcenter vcenter 
	
	putexcel B`row'=("Compared to") C`row'=("Changes in") D`row'= ("Total consumption expenditure (INR)") E`row'= ("Variety of food items consumed") F`row'= ("Indicator for reduced meal portions"), txtwrap
 	putexcel B`row':F`row', border(bottom)
	putexcel B`row', italic
	
	putexcel B`row_1' = "Full Baseline (Nov'18-Oct'19)             NxT = 15,726", txtwrap
	putexcel (B`row_1':B`row_5'), merge vcenter
	putexcel (B`row_5':F`row_5'), border(bottom)
	putexcel B`row_6' = "Restricted Baseline (Mar'19-April'19) NxT = 2,293", txtwrap
	putexcel (B`row_6':B`row_10'), merge vcenter 

	putexcel C`row_1' = "Levels", txtwrap
	putexcel C`row_3' = "Percentage", txtwrap italic
	putexcel C`row_4' = "Baseline Mean", txtwrap italic
	putexcel C`row_5' = "R2 (within)", txtwrap italic
	
	putexcel C`row_6' = "Levels", txtwrap
	putexcel C`row_8' = "Percentage", txtwrap italic
	putexcel C`row_9' = "Baseline Mean", txtwrap italic
	putexcel C`row_10' = "R2 (within)", txtwrap italic
	
	putexcel (B`row_10':F`row_10'), border(bottom)
	
	// Total consumption expenditure
		sum cs_total if covid_d == 0 
			putexcel D`row_4' = (r(mean)), nformat(number_sep_d2)
			scalar full_mean_cons = r(mean)
			
		sum cs_total if covid_d == 0 & MarchApril == 1 
			putexcel D`row_9' = (r(mean)), nformat(number_sep_d2)
			scalar rtd_mean_cons = r(mean)
		
		xtreg cs_total covid_d MarchApril pre_covid_full post_covid, fe cluster(cluster_id) nonest
		lincom covid_d+MarchApril
			putexcel D`row_1' = (r(estimate)), nformat(number_sep_d2)
				scalar temp = r(estimate)
				scalar temp1 = temp/full_mean_cons
			putexcel D`row_2' = (r(se)), nformat((0.00))
			putexcel D`row_3' = temp1, nformat(percent) italic
			putexcel D`row_5' = (e(r2_w)), nformat(0.000)
		
		xtreg cs_total covid_d if MarchApril == 1, fe cluster(cluster_id) nonest
			mat a = r(table)
			putexcel D`row_6' = a[1,1],  nformat(number_sep_d2)
				scalar temp = a[1,1]
				scalar temp1 = temp/rtd_mean_cons
			putexcel D`row_7' = a[2,1], nformat((0.00))
			putexcel D`row_8' = temp1, nformat(percent) italic
			putexcel D`row_10' = (e(r2_w)), nformat(0.000)	
			
	// Variety of food items consumed
		sum cs_count if covid_d == 0 
			putexcel E`row_4' = (r(mean)), nformat(number_sep_d2)
			scalar full_mean_conscount = r(mean)
			
		sum cs_count if covid_d == 0 & MarchApril == 1 
			putexcel E`row_9' = (r(mean)), nformat(number_sep_d2)
			scalar rtd_mean_conscount = r(mean)
		
		xtreg cs_count covid_d MarchApril pre_covid_full post_covid, fe cluster(cluster_id) nonest
		lincom covid_d+MarchApril
			putexcel E`row_1' = (r(estimate)), nformat(number_sep_d2)
				scalar temp = r(estimate)
				scalar temp1 = temp/full_mean_conscount
			putexcel E`row_2' = (r(se)), nformat((0.00))
			putexcel E`row_3' = temp1, nformat(percent) italic
			putexcel E`row_5' = (e(r2_w)), nformat(0.000)
		
		xtreg cs_count covid_d if MarchApril == 1, fe cluster(cluster_id) nonest
			mat a = r(table)
			putexcel E`row_6' = a[1,1], nformat(number_sep_d2)
				scalar temp = a[1,1]
				scalar temp1 = temp/rtd_mean_conscount
			putexcel E`row_7' = a[2,1], nformat((0.00))
			putexcel E`row_8' = temp1, nformat(percent) italic
			putexcel E`row_10' = (e(r2_w)), nformat(0.000)	
			
	// Indicator for reduced meal portions
		sum fs_reducemeals if covid_d == 0 
			putexcel F`row_4' = (r(mean)), nformat(number_sep_d2)
			scalar full_mean_reducedmeal = r(mean)
			
		sum fs_reducemeals if covid_d == 0 & MarchApril == 1 
			putexcel F`row_9' = (r(mean)), nformat(number_sep_d2)
			scalar rtd_mean_reducedmeal = r(mean)
		
		xtreg fs_reducemeals covid_d MarchApril pre_covid_full post_covid, fe cluster(cluster_id) nonest
		lincom covid_d+MarchApril
			putexcel F`row_1' = (r(estimate)), nformat(number_sep_d2)
			putexcel F`row_2' = (r(se)), nformat((0.00))
			putexcel F`row_3' = "-", right
			putexcel F`row_5' = (e(r2_w)), nformat(0.000)
		
		xtreg fs_reducemeals covid_d if MarchApril == 1, fe cluster(cluster_id) nonest
			mat a = r(table)
			putexcel F`row_6' = a[1,1], nformat(number_sep_d2)
			putexcel F`row_7' = a[2,1], nformat((0.00))
			putexcel F`row_8' = "-", right 
			putexcel F`row_10' = (e(r2_w)), nformat(0.000)	
	

********************************************************************************
* Table 7: Marginal Impacts of COVID-19 Lockdown on Borrowing and
*		 Non-consumption Expenditure Using Main Specification
********************************************************************************						

	putexcel set "$tables/analysis_tables.xlsx", sheet("table_7") modify
	
	local row = 3
	
	forval i = 1/10 {
		local row_`i' = `row' + `i'
	}

	putexcel B2=("Table 7: Marginal Impacts of COVID-19 Lockdown Borrowing and Non-consumption Expenditure Using Main Specification"), bold 
	putexcel (B2:F2), merge hcenter vcenter 
	
	putexcel B`row'=("Compared to") C`row'=("Changes in") D`row'= ("Indicator for borrowing in cash") E`row'= ("Indicator for borrowing in kind") F`row'= ("Non-consumption expenditure (INR)"), txtwrap
 	putexcel B`row':F`row', border(bottom)
	putexcel B`row', italic

	
	putexcel B`row_1' = "Full Baseline (Nov'18-Oct'19)             NxT = 15,726", txtwrap
	putexcel (B`row_1':B`row_5'), merge vcenter
	putexcel (B`row_5':F`row_5'), border(bottom)
	putexcel B`row_6' = "Restricted Baseline (Mar'19-April'19) NxT = 2,293", txtwrap
	putexcel (B`row_6':B`row_10'), merge vcenter 
	
	putexcel C`row_1' = "Levels", txtwrap
	putexcel C`row_3' = "Percentage", txtwrap italic
	putexcel C`row_4' = "Baseline Mean", txtwrap italic
	putexcel C`row_5' = "R2 (within)", txtwrap italic
	
	putexcel C`row_6' = "Levels", txtwrap
	putexcel C`row_8' = "Percentage", txtwrap italic
	putexcel C`row_9' = "Baseline Mean", txtwrap italic
	putexcel C`row_10' = "R2 (within)", txtwrap italic

	putexcel (B`row_10':F`row_10'), border(bottom)
	
	// Indicator for  borrowing in cash
		sum d_br_cash if covid_d == 0 
			putexcel D`row_4' = (r(mean)), nformat(number_sep_d2)
			scalar full_mean_brcash = r(mean)
			
		sum d_br_cash if covid_d == 0 & MarchApril == 1 
			putexcel D`row_9' = (r(mean)), nformat(number_sep_d2)
			scalar rtd_mean_brcash = r(mean)
		
		xtreg d_br_cash covid_d MarchApril pre_covid_full post_covid, fe cluster(cluster_id) nonest
		lincom covid_d+MarchApril
			putexcel D`row_1' = (r(estimate)), nformat(number_sep_d2)
				scalar temp = r(estimate)
				scalar temp1 = temp/full_mean_brcash
			putexcel D`row_2' = (r(se)), nformat((0.00))
			putexcel D`row_3' = temp1, nformat(percent) italic
			putexcel D`row_5' = (e(r2_w)), nformat(0.000)
		
		xtreg d_br_cash covid_d if MarchApril == 1, fe cluster(cluster_id) nonest
			mat a = r(table)
			putexcel D`row_6' = a[1,1],  nformat(number_sep_d2)
				scalar temp = a[1,1]
				scalar temp1 = temp/rtd_mean_brcash
			putexcel D`row_7' = a[2,1], nformat((0.00))
			putexcel D`row_8' = temp1, nformat(percent) italic
			putexcel D`row_10' = (e(r2_w)), nformat(0.000)
		
	// Indicator for  borrowing in kind
		sum d_br_inkind if covid_d == 0 
			putexcel E`row_4' = (r(mean)), nformat(number_sep_d2)
			scalar full_mean_brik = r(mean)
			
		sum d_br_inkind if covid_d == 0 & MarchApril == 1 
			putexcel E`row_9' = (r(mean)), nformat(number_sep_d2)
			scalar rtd_mean_brik = r(mean)
		
		xtreg d_br_inkind covid_d MarchApril pre_covid_full post_covid, fe cluster(cluster_id) nonest
		lincom covid_d+MarchApril
			putexcel E`row_1' = (r(estimate)), nformat(number_sep_d2)
				scalar temp = r(estimate)
				scalar temp1 = temp/full_mean_brik
			putexcel E`row_2' = (r(se)), nformat((0.00))
			putexcel E`row_3' = temp1, nformat(percent) italic
			putexcel E`row_5' = (e(r2_w)), nformat(0.000)
		
		xtreg d_br_inkind covid_d if MarchApril == 1, fe cluster(cluster_id) nonest
			mat a = r(table)
			putexcel E`row_6' = a[1,1],  nformat(number_sep_d2)
				scalar temp = a[1,1]
				scalar temp1 = temp/rtd_mean_brik
			putexcel E`row_7' = a[2,1], nformat((0.00))
			putexcel E`row_8' = temp1, nformat(percent) italic
			putexcel E`row_10' = (e(r2_w)), nformat(0.000)
			
	// Non-consumption expenditure
		sum exp_total if covid_d == 0 
			putexcel F`row_4' = (r(mean)), nformat(number_sep_d2)
			scalar full_mean_noncons = r(mean)
			
		sum exp_total if covid_d == 0 & MarchApril == 1 
			putexcel F`row_9' = (r(mean)), nformat(number_sep_d2)
			scalar rtd_mean_noncons = r(mean)
		
		xtreg exp_total covid_d MarchApril pre_covid_full post_covid, fe cluster(cluster_id) nonest
		lincom covid_d+MarchApril
			putexcel F`row_1' = (r(estimate)), nformat(number_sep_d2)
				scalar temp = r(estimate)
				scalar temp1 = temp/full_mean_noncons
			putexcel F`row_2' = (r(se)), nformat((0.00))
			putexcel F`row_3' = temp1, nformat(percent) italic
			putexcel F`row_5' = (e(r2_w)), nformat(0.000)
		
		xtreg exp_total covid_d if MarchApril == 1, fe cluster(cluster_id) nonest
			mat a = r(table)
			putexcel F`row_6' = a[1,1],  nformat(number_sep_d2)
				scalar temp = a[1,1]
				scalar temp1 = temp/rtd_mean_noncons
			putexcel F`row_7' = a[2,1], nformat((0.00))
			putexcel F`row_8' = temp1, nformat(percent) italic
			putexcel F`row_10' = (e(r2_w)), nformat(0.000)			
			
			
********************************************************************************	
* Table 8: Conditional Correlations of Percentage Reduction in 
*				 Total Income and Total Expenditure from Baseline
********************************************************************************

	* Percentage reduction in total income from baseline 	
	putexcel set "$tables/analysis_tables.xlsx", sheet("table_8") modify

	putexcel B2=("Table 8: Conditional Correlations of Percentage Reduction in Total Income and Total Expenditure from Baseline"), bold 
	putexcel (B2:D2), merge hcenter vcenter border(bottom)
		
	putexcel C3=("Dependent Variables (% reduction from Baseline)"), bold 
	putexcel (C3:D3), merge hcenter vcenter border(bottom)
	
	local row = 4
	putexcel B`row'=("Variables") C`row'=("Total Income") D`row'= ("Total Expenditure") 
	putexcel B`row':D`row', hcenter border(bottom)
			
	forval i = 1/33 {
		local row_`i' = `row' + `i'
	}
		
	putexcel B`row_1' = "Total Expenditure (in Full Baseline)", txtwrap
	putexcel B`row_3' = "Total Income (in Full Baseline)", txtwrap
	putexcel B`row_5' = "Dummy if aid package received", txtwrap
	putexcel B`row_7' = "Dummy if HH in bottom 50th percentile (in Full Baseline)", txtwrap
	putexcel B`row_9' = "Household Characteristics", txtwrap
	putexcel B`row_10' = "HH size", txtwrap
	putexcel B`row_12' = "Female-headed HH", txtwrap
	putexcel B`row_14' = "HH with migrant", txtwrap
	putexcel B`row_16' = "HH involved in agriculture", txtwrap
	putexcel B`row_18' = "HH with business", txtwrap
	putexcel B`row_20' = "Impacted Income Sources", txtwrap
	putexcel B`row_21' = "Loss of local employment", txtwrap
	putexcel B`row_23' = "Loss of agricultural income", txtwrap
	putexcel B`row_25' = "Loss of remittance income", txtwrap
	putexcel B`row_27' = "Loss of business income", txtwrap
	putexcel B`row_29' = "Loss of MNREGA income", txtwrap
	putexcel B`row_31' = "Constant", txtwrap
	
	local row = `row' + 1 
	
	* Percentage reduction in total income from baseline 
	reg ave_inc_pc_drop ave_inc_pre any_aid bottom50 nb_hhmem head_female hh_migrant farm_yn business_yn ///
			loss_job_local loss_ag_inc loss_rmt loss_business_inc loss_nrega_inc if week == 1, robust 

		mat a = r(table)
		putexcel C`row_3' = a[1,1],  nformat(number_sep_d2)
		putexcel C`row_4' = a[2,1], nformat((0.00000))
		putexcel C`row_5' = a[1,2],  nformat(number_sep_d2)
		putexcel C`row_6' = a[2,2], nformat((0.00000))
		putexcel C`row_7' = a[1,3],  nformat(number_sep_d2)
		putexcel C`row_8' = a[2,3], nformat((0.000))
		putexcel C`row_10' = a[1,4],  nformat(number_sep_d2)
		putexcel C`row_11' = a[2,4], nformat((0.000))
		putexcel C`row_12' = a[1,5],  nformat(number_sep_d2)
		putexcel C`row_13' = a[2,5], nformat((0.000))
		putexcel C`row_14' = a[1,6],  nformat(number_sep_d2)
		putexcel C`row_15' = a[2,6], nformat((0.000))
		putexcel C`row_16' = a[1,7],  nformat(number_sep_d2)
		putexcel C`row_17' = a[2,7], nformat((0.000))
		putexcel C`row_18' = a[1,8],  nformat(number_sep_d2)
		putexcel C`row_19' = a[2,8], nformat((0.000))
		putexcel C`row_21' = a[1,9],  nformat(number_sep_d2)
		putexcel C`row_22' = a[2,9], nformat((0.000))
		putexcel C`row_23' = a[1,10],  nformat(number_sep_d2)
		putexcel C`row_24' = a[2,10], nformat((0.000))	
		putexcel C`row_25' = a[1,11],  nformat(number_sep_d2)
		putexcel C`row_26' = a[2,11], nformat((0.000))
		putexcel C`row_27' = a[1,12],  nformat(number_sep_d2)
		putexcel C`row_28' = a[2,12], nformat((0.000))
		putexcel C`row_29' = a[1,13],  nformat(number_sep_d2)
		putexcel C`row_30' = a[2,13], nformat((0.000))
		putexcel C`row_31' = a[1,14],  nformat(number_sep_d2)
		putexcel C`row_32' = a[2,14], nformat((0.000))


	* Percentage reduction in total spending from baseline 			
	reg ave_spending_pc_drop ave_spending_pre any_aid bottom50 nb_hhmem head_female hh_migrant farm_yn business_yn ///
			loss_job_local loss_ag_inc loss_rmt loss_business_inc loss_nrega_inc if week == 1, robust 

		mat a = r(table)
		putexcel D`row_1' = a[1,1],  nformat(number_sep_d2)
		putexcel D`row_2' = a[2,1], nformat((0.00000))
		putexcel D`row_5' = a[1,2],  nformat(number_sep_d2)
		putexcel D`row_6' = a[2,2], nformat((0.00000))
		putexcel D`row_7' = a[1,3],  nformat(number_sep_d2)
		putexcel D`row_8' = a[2,3], nformat((0.000))
		putexcel D`row_10' = a[1,4],  nformat(number_sep_d2)
		putexcel D`row_11' = a[2,4], nformat((0.000))
		putexcel D`row_12' = a[1,5],  nformat(number_sep_d2)
		putexcel D`row_13' = a[2,5], nformat((0.000))
		putexcel D`row_14' = a[1,6],  nformat(number_sep_d2)
		putexcel D`row_15' = a[2,6], nformat((0.000))
		putexcel D`row_16' = a[1,7],  nformat(number_sep_d2)
		putexcel D`row_17' = a[2,7], nformat((0.000))
		putexcel D`row_18' = a[1,8],  nformat(number_sep_d2)
		putexcel D`row_19' = a[2,8], nformat((0.000))
		putexcel D`row_21' = a[1,9],  nformat(number_sep_d2)
		putexcel D`row_22' = a[2,9], nformat((0.000))
		putexcel D`row_23' = a[1,10],  nformat(number_sep_d2)
		putexcel D`row_24' = a[2,10], nformat((0.000))	
		putexcel D`row_25' = a[1,11],  nformat(number_sep_d2)
		putexcel D`row_26' = a[2,11], nformat((0.000))
		putexcel D`row_27' = a[1,12],  nformat(number_sep_d2)
		putexcel D`row_28' = a[2,12], nformat((0.000))
		putexcel D`row_29' = a[1,13],  nformat(number_sep_d2)
		putexcel D`row_30' = a[2,13], nformat((0.000))
		putexcel D`row_31' = a[1,14],  nformat(number_sep_d2)
		putexcel D`row_32' = a[2,14], nformat((0.000))

	putexcel B`row_33'=("N"), italic 
		sum nb_hhmem if week == 1
		putexcel C`row_33' = (r(N))
		putexcel D`row_33' = (r(N))
	
	putexcel B`row_32':D`row_32', border(bottom)		
	putexcel B`row_33':D`row_33', border(bottom)		
	

********************************************************************************		
* APPENDIX 
********************************************************************************
* Table A1: Probability of Attrition Based on Baseline Household Characteristics
********************************************************************************			
	
	putexcel set "$tables/analysis_tables.xlsx", sheet("table_a1") modify

	putexcel B2=("Table A1: Probability of Attrition Based on Baseline Household Characteristics"), bold txtwrap
	putexcel (B2:C2), merge hcenter vcenter border(bottom)
		
	local row = 3
	putexcel B`row'=("Variables") C`row'=("Dependent varible: Probability of attrition") , txtwrap 
	putexcel B`row':C`row', hcenter border(bottom)
	putexcel B`row', italic
		
	forval i = 1/20 {
		local row_`i' = `row' + `i'
	}
		
	putexcel B`row_1' = "HH head age", txtwrap
	putexcel B`row_3' = "HH head years of education", txtwrap
	putexcel B`row_5' = "Female-headed HH", txtwrap
	putexcel B`row_7' = "HH size", txtwrap
	putexcel B`row_9' = "Number of Children", txtwrap
	putexcel B`row_11' = "Proportion of HH with migrant", txtwrap
	putexcel B`row_13' = "HH involved in agriculture", txtwrap
	putexcel B`row_15' = "HH with business", txtwrap
	putexcel B`row_17' = "Land owned (kathas)", txtwrap
	putexcel B`row_19' = "N", txtwrap italic
	putexcel B`row_20' = "Overall F-stat", txtwrap italic
		
	probit attrited head_age head_edu head_female nb_hhmem nb_children hh_migrant ///
			farm_yn business_yn land_own if week == 1
		
		mat a = r(table)
		forvalues i = 1(1)9 {
			local row = `row' + 1 
			local row_1 = `row' + 1
			putexcel C`row' = a[1,`i'],  nformat(number_sep_d2)
			putexcel C`row_1' = a[2,`i'], nformat((0.00))
			local row = `row' + 1 	
		}
	
		putexcel C`row_19' = (e(N)), nformat(number_sep)
		putexcel C`row_20' = (e(p)), nformat(number_sep_d2)			
		
		putexcel B`row_18':C`row_18', border(bottom)
		putexcel B`row_20':C`row_20', border(bottom)

		
********************************************************************************	
* Table A4: Marginal Impacts on Local Income, Total Expenditure, and Remittance Income
*						Using Alternative Specifications
********************************************************************************							

	putexcel set "$tables/analysis_tables.xlsx", sheet("table_a4") modify
	
	local row = 4
	
	forval i = 1/21 {
		local row_`i' = `row' + `i'
	}

	putexcel B2=("Table A4: Marginal Impacts on Local Income, Total Expenditure, and Remittance Income Using Alternative Specifications"), bold 
	putexcel (B2:F2), merge hcenter vcenter 
	putexcel B3=("(a) using time (week) fixed effects") 
	putexcel (B3:F3), merge hcenter vcenter border(bottom)
	putexcel B`row_9'=("(b) using separate indicators for March and April 2020") 
	
	putexcel B`row'=("Compared to") C`row'=("Changes in") D`row'= ("Local Income (INR)") E`row'= ("Total Expenditure (INR)")  F`row'= ("Remittance Income (INR)"), txtwrap
 	putexcel B`row':F`row', border(bottom)
	putexcel B`row', italic
	
	putexcel B`row_1' = "Full Baseline (Nov'18-Oct'19)             NxT = 15,726", txtwrap
	putexcel (B`row_1':B`row_4'), merge vcenter
	putexcel (B`row_4':F`row_4'), border(bottom)
	putexcel B`row_5' = "Restricted Baseline (Mar'19-April'19) NxT = 2,293", txtwrap
	putexcel (B`row_5':B`row_8'), merge vcenter 
	putexcel B`row_10' = "Full Baseline (Nov'18-Oct'19)             NxT = 15,108", txtwrap
	putexcel (B`row_10':B`row_18'), merge vcenter 
	putexcel (B`row_18':F`row_18'), border(bottom)
	putexcel B`row_19' = "Mean value of the dependent variable in", txtwrap italic
	putexcel B`row_20' = "		Full Baseline (Nov'18-Oct'19)", txtwrap
	putexcel B`row_21' = "		Restricted Baseline (Mar'19-April'19)", txtwrap
	
	putexcel C`row_1' = "Levels", txtwrap
	putexcel C`row_3' = "Percentage", txtwrap italic
	putexcel C`row_4' = "R2 (within)", txtwrap italic
	
	putexcel C`row_5' = "Levels", txtwrap
	putexcel C`row_7' = "Percentage", txtwrap italic
	putexcel C`row_8' = "R2 (within)", txtwrap italic
	
	putexcel C`row_10' = "Changes in March 2020", txtwrap
	putexcel C`row_11' = "Levels", txtwrap italic
	putexcel C`row_13' = "Percentage", txtwrap italic
	putexcel C`row_14' = "Changes in April 2020", txtwrap
	putexcel C`row_15' = "Levels", txtwrap italic
	putexcel C`row_17' = "Percentage", txtwrap italic
	putexcel C`row_18' = "R2 (within)", txtwrap italic
	
	putexcel (B`row_8':F`row_8'), border(bottom)
	putexcel (B`row_9':F`row_9'), merge hcenter vcenter border(bottom)
	putexcel (B`row_21':F`row_21'), border(bottom)
	
	* Mean value
		putexcel D`row_20' = full_mean_inc, nformat(number_sep_d2)
		putexcel D`row_21' = rtd_mean_inc, nformat(number_sep_d2)
		putexcel E`row_20' = full_mean_spending, nformat(number_sep_d2)
		putexcel E`row_21' = rtd_mean_spending, nformat(number_sep_d2)
		putexcel F`row_20' = full_mean_rmt, nformat(number_sep_d2)
		putexcel F`row_21' = rtd_mean_rmt, nformat(number_sep_d2)
		
	* (a) using time (week) fixed effects
		// Local income	
			xtreg local_inc covid_d MarchApril i.week, fe cluster(cluster_id) nonest
			lincom covid_d+MarchApril
				putexcel D`row_1' = (r(estimate)), nformat(number_sep_d2)
					scalar temp = r(estimate)
					scalar temp1 = temp/full_mean_inc
				putexcel D`row_2' = (r(se)), nformat((0.00))
				putexcel D`row_3' = temp1, nformat(percent) italic
				putexcel D`row_4' = (e(r2_w)), nformat(0.000)
			
			xtreg local_inc covid_d i.week if MarchApril == 1, fe cluster(cluster_id) nonest	
				mat a = r(table)
				putexcel D`row_5' = a[1,1],  nformat(number_sep_d2)
					scalar temp = a[1,1]
					scalar temp1 = temp/rtd_mean_inc
				putexcel D`row_6' = a[2,1], nformat((0.00))
				putexcel D`row_7' = temp1, nformat(percent) italic
				putexcel D`row_8' = (e(r2_w)), nformat(0.000)
			
		// Total expenditure 
			xtreg total_spending covid_d MarchApril i.week, fe cluster(cluster_id) nonest
			lincom covid_d+MarchApril
				putexcel E`row_1' = (r(estimate)), nformat(number_sep_d2)
					scalar temp = r(estimate)
					scalar temp1 = temp/full_mean_spending
				putexcel E`row_2' = (r(se)), nformat((0.00))
				putexcel E`row_3' = temp1, nformat(percent) italic
				putexcel E`row_4' = (e(r2_w)), nformat(0.000)
			
			xtreg total_spending covid_d i.week if MarchApril == 1, fe cluster(cluster_id) nonest	
				mat a = r(table)
				putexcel E`row_5' = a[1,1],  nformat(number_sep_d2)
					scalar temp = a[1,1]
					scalar temp1 = temp/rtd_mean_spending
				putexcel E`row_6' = a[2,1], nformat((0.00))
				putexcel E`row_7' = temp1, nformat(percent) italic
				putexcel E`row_8' = (e(r2_w)), nformat(0.000)
				
		// Remittance income 
			xtreg rmt_total covid_d MarchApril i.week, fe cluster(cluster_id) nonest
			lincom covid_d+MarchApril
				putexcel F`row_1' = (r(estimate)), nformat(number_sep_d2)
					scalar temp = r(estimate)
					scalar temp1 = temp/full_mean_rmt
				putexcel F`row_2' = (r(se)), nformat((0.00))
				putexcel F`row_3' = temp1, nformat(percent) italic
				putexcel F`row_4' = (e(r2_w)), nformat(0.000)
			
			xtreg rmt_total covid_d i.week if MarchApril == 1, fe cluster(cluster_id) nonest	
				mat a = r(table)
				putexcel F`row_5' = a[1,1],  nformat(number_sep_d2)
					scalar temp = a[1,1]
					scalar temp1 = temp/rtd_mean_rmt
				putexcel F`row_6' = a[2,1], nformat((0.00))
				putexcel F`row_7' = temp1, nformat(percent) italic
				putexcel F`row_8' = (e(r2_w)), nformat(0.000)		
				
	*(b) using separate indicators for March and April 2020 	
		// Local income	
			xtreg local_inc March2020 April2020 pre_covid_full post_covid, fe cluster(cluster_id) nonest
				mat a = r(table)
				putexcel D`row_11' = a[1,1],  nformat(number_sep_d2)
					scalar temp = a[1,1]
					scalar temp1 = temp/full_mean_inc
				putexcel D`row_12' = a[2,1], nformat((0.00))
				putexcel D`row_13' = temp1, nformat(percent) italic
				
				putexcel D`row_15' = a[1,2],  nformat(number_sep_d2)
					scalar temp = a[1,2]
					scalar temp1 = temp/full_mean_inc
				putexcel D`row_16' = a[2,2], nformat((0.00))
				putexcel D`row_17' = temp1, nformat(percent) italic
				putexcel D`row_18' = (e(r2_w)), nformat(0.000)
				
		// Total expenditure 
			xtreg total_spending March2020 April2020 pre_covid_full post_covid, fe cluster(cluster_id) nonest
				mat a = r(table)
				putexcel E`row_11' = a[1,1],  nformat(number_sep_d2)
					scalar temp = a[1,1]
					scalar temp1 = temp/full_mean_spending
				putexcel E`row_12' = a[2,1], nformat((0.00))
				putexcel E`row_13' = temp1, nformat(percent) italic
				
				putexcel E`row_15' = a[1,2],  nformat(number_sep_d2)
					scalar temp = a[1,2]
					scalar temp1 = temp/full_mean_spending
				putexcel E`row_16' = a[2,2], nformat((0.00))
				putexcel E`row_17' = temp1, nformat(percent) italic
				putexcel E`row_18' = (e(r2_w)), nformat(0.000)
				
		// Remittance income 
			xtreg rmt_total March2020 April2020 pre_covid_full post_covid, fe cluster(cluster_id) nonest
				mat a = r(table)
				putexcel F`row_11' = a[1,1],  nformat(number_sep_d2)
					scalar temp = a[1,1]
					scalar temp1 = temp/full_mean_rmt
				putexcel F`row_12' = a[2,1], nformat((0.00))
				putexcel F`row_13' = temp1, nformat(percent) italic
				
				putexcel F`row_15' = a[1,2],  nformat(number_sep_d2)
					scalar temp = a[1,2]
					scalar temp1 = temp/full_mean_rmt
				putexcel F`row_16' = a[2,2], nformat((0.00))
				putexcel F`row_17' = temp1, nformat(percent) italic
				putexcel F`row_18' = (e(r2_w)), nformat(0.000)

				
********************************************************************************	
* Table A5: Marginal Impacts on Consumption Using Alternative Specifications
********************************************************************************							

	putexcel set "$tables/analysis_tables.xlsx", sheet("table_a5") modify
	
	local row = 4
	
	forval i = 1/21 {
		local row_`i' = `row' + `i'
	}

	putexcel B2=("Table A5: Marginal Impacts on Consumption Using Alternative Specifications"), bold 
	putexcel (B2:F2), merge hcenter vcenter 
	putexcel B3=("(a) using time (week) fixed effects") 
	putexcel (B3:F3), merge hcenter vcenter border(bottom)
	putexcel B`row_9'=("(b) using separate indicators for March and April 2020") 
	
	putexcel B`row'=("Compared to") C`row'=("Changes in") D`row'= ("Total consumption expenditure (INR)") E`row'= ("Variety of food items consumed")  F`row'= ("Indicator for reduced meals"), txtwrap
 	putexcel B`row':F`row', border(bottom)
	putexcel B`row', italic
	
	putexcel B`row_1' = "Full Baseline (Nov'18-Oct'19)             NxT = 15,726", txtwrap
	putexcel (B`row_1':B`row_4'), merge vcenter
	putexcel B`row_5' = "Restricted Baseline (Mar'19-April'19) NxT = 2,293", txtwrap
	putexcel (B`row_5':B`row_8'), merge vcenter 
	putexcel B`row_10' = "Full Baseline (Nov'18-Oct'19)             NxT = 15,108", txtwrap
	putexcel (B`row_10':B`row_18'), merge vcenter 
	putexcel B`row_19' = "Mean value of the dependent variable in", txtwrap italic
	putexcel B`row_20' = "		Full Baseline (Nov'18-Oct'19)", txtwrap
	putexcel B`row_21' = "		Restricted Baseline (Mar'19-April'19)", txtwrap
	
	putexcel C`row_1' = "Levels", txtwrap
	putexcel C`row_3' = "Percentage", txtwrap italic
	putexcel C`row_4' = "R2 (within)", txtwrap italic
	
	putexcel C`row_5' = "Levels", txtwrap
	putexcel C`row_7' = "Percentage", txtwrap italic
	putexcel C`row_8' = "R2 (within)", txtwrap italic
	
	putexcel C`row_10' = "Changes in March 2020", txtwrap
	putexcel C`row_11' = "Levels", txtwrap italic
	putexcel C`row_13' = "Percentage", txtwrap italic
	putexcel C`row_14' = "Changes in April 2020", txtwrap
	putexcel C`row_15' = "Levels", txtwrap italic
	putexcel C`row_17' = "Percentage", txtwrap italic
	putexcel C`row_18' = "R2 (within)", txtwrap italic
	
	putexcel (B`row_8':F`row_8'), border(bottom)
	putexcel (B`row_9':F`row_9'), merge hcenter vcenter border(bottom)
	putexcel (B`row_18':F`row_18'), border(bottom)
	putexcel (B`row_21':F`row_21'), border(bottom)
	
	* Mean value
		putexcel D`row_20' = full_mean_cons, nformat(number_sep_d2)
		putexcel D`row_21' = rtd_mean_cons, nformat(number_sep_d2)
		putexcel E`row_20' = full_mean_conscount, nformat(number_sep_d2)
		putexcel E`row_21' = rtd_mean_conscount, nformat(number_sep_d2)
		putexcel F`row_20' = full_mean_reducedmeal , nformat(number_sep_d2)
		putexcel F`row_21' = rtd_mean_reducedmeal , nformat(number_sep_d2)
		
	* (a) using time (week) fixed effects
		// Total consumption expenditure	
			xtreg cs_total covid_d MarchApril i.week, fe cluster(cluster_id) nonest
			lincom covid_d+MarchApril
				putexcel D`row_1' = (r(estimate)), nformat(number_sep_d2)
					scalar temp = r(estimate)
					scalar temp1 = temp/full_mean_cons
				putexcel D`row_2' = (r(se)), nformat((0.00))
				putexcel D`row_3' = temp1, nformat(percent) italic
				putexcel D`row_4' = (e(r2_w)), nformat(0.000)
			
			xtreg cs_total covid_d i.week if MarchApril == 1, fe cluster(cluster_id) nonest	
				mat a = r(table)
				putexcel D`row_5' = a[1,1],  nformat(number_sep_d2)
					scalar temp = a[1,1]
					scalar temp1 = temp/rtd_mean_cons
				putexcel D`row_6' = a[2,1], nformat((0.00))
				putexcel D`row_7' = temp1, nformat(percent) italic
				putexcel D`row_8' = (e(r2_w)), nformat(0.000)
			
		// Variety of food items consumed
			xtreg cs_count covid_d MarchApril i.week, fe cluster(cluster_id) nonest
			lincom covid_d+MarchApril
				putexcel E`row_1' = (r(estimate)), nformat(number_sep_d2)
					scalar temp = r(estimate)
					scalar temp1 = temp/full_mean_conscount
				putexcel E`row_2' = (r(se)), nformat((0.00))
				putexcel E`row_3' = temp1, nformat(percent) italic
				putexcel E`row_4' = (e(r2_w)), nformat(0.000)
			
			xtreg cs_count covid_d i.week if MarchApril == 1, fe cluster(cluster_id) nonest	
				mat a = r(table)
				putexcel E`row_5' = a[1,1],  nformat(number_sep_d2)
					scalar temp = a[1,1]
					scalar temp1 = temp/rtd_mean_conscount
				putexcel E`row_6' = a[2,1], nformat((0.00))
				putexcel E`row_7' = temp1, nformat(percent) italic
				putexcel E`row_8' = (e(r2_w)), nformat(0.000)
				
		// Indicator for reduced meals
			xtreg fs_reducemeals covid_d MarchApril i.week, fe cluster(cluster_id) nonest
			lincom covid_d+MarchApril
				putexcel F`row_1' = (r(estimate)), nformat(number_sep_d2)
				putexcel F`row_2' = (r(se)), nformat((0.00))
				putexcel F`row_3' = "-", right
				putexcel F`row_4' = (e(r2_w)), nformat(0.000)
			
			xtreg fs_reducemeals covid_d i.week if MarchApril == 1, fe cluster(cluster_id) nonest	
				mat a = r(table)
				putexcel F`row_5' = a[1,1],  nformat(number_sep_d2)
				putexcel F`row_6' = a[2,1], nformat((0.00))
				putexcel F`row_7' = "-", right
				putexcel F`row_8' = (e(r2_w)), nformat(0.000)		
				
	*(b) using separate indicators for March and April 2020 	
		// Total consumption expenditure
			xtreg cs_total March2020 April2020 pre_covid_full post_covid, fe cluster(cluster_id) nonest
				mat a = r(table)
				putexcel D`row_11' = a[1,1],  nformat(number_sep_d2)
					scalar temp = a[1,1]
					scalar temp1 = temp/full_mean_cons
				putexcel D`row_12' = a[2,1], nformat((0.00))
				putexcel D`row_13' = temp1, nformat(percent) italic
				
				putexcel D`row_15' = a[1,2],  nformat(number_sep_d2)
					scalar temp = a[1,2]
					scalar temp1 = temp/full_mean_cons
				putexcel D`row_16' = a[2,2], nformat((0.00))
				putexcel D`row_17' = temp1, nformat(percent) italic
				putexcel D`row_18' = (e(r2_w)), nformat(0.000)
				
		// Variety of food items consumed
			xtreg cs_count March2020 April2020 pre_covid_full post_covid, fe cluster(cluster_id) nonest
				mat a = r(table)
				putexcel E`row_11' = a[1,1],  nformat(number_sep_d2)
					scalar temp = a[1,1]
					scalar temp1 = temp/full_mean_conscount
				putexcel E`row_12' = a[2,1], nformat((0.00))
				putexcel E`row_13' = temp1, nformat(percent) italic
				
				putexcel E`row_15' = a[1,2],  nformat(number_sep_d2)
					scalar temp = a[1,2]
					scalar temp1 = temp/full_mean_conscount
				putexcel E`row_16' = a[2,2], nformat((0.00))
				putexcel E`row_17' = temp1, nformat(percent) italic
				putexcel E`row_18' = (e(r2_w)), nformat(0.000)
				
		// Indicator for reduced meals
			xtreg fs_reducemeals March2020 April2020 pre_covid_full post_covid, fe cluster(cluster_id) nonest
				mat a = r(table)
				putexcel F`row_11' = a[1,1],  nformat(number_sep_d2)
				putexcel F`row_12' = a[2,1], nformat((0.00))
				putexcel F`row_13' = "-", right
				
				putexcel F`row_15' = a[1,2],  nformat(number_sep_d2)
				putexcel F`row_16' = a[2,2], nformat((0.00))
				putexcel F`row_17' = "-", right
				putexcel F`row_18' = (e(r2_w)), nformat(0.000)
						
		
********************************************************************************	
* Table A6: Marginal Impacts on Treating the Data as Dynamic Panel 
*			(using Arellano-Bond Specification)
********************************************************************************							

	putexcel set "$tables/analysis_tables.xlsx", sheet("table_a6") modify
	
	local row = 3
	
	forval i = 1/19 {
		local row_`i' = `row' + `i'
	}

	putexcel B2=("Table A6: Marginal Impacts on Treating the Data as Dynamic Panel (using Arellano-Bond Specification)"), bold 
	putexcel (B2:F2), merge hcenter vcenter 
	
	putexcel B`row'=("Compared to") C`row'=("Changes in") D`row'= ("Local income (INR)") E`row'= ("Total expenditure (INR)")  F`row'= ("Remittance income (INR)"), txtwrap
 	putexcel B`row':F`row', border(bottom)
	
	putexcel B`row_1' = "Full Baseline (Nov'18-Oct'19)             NxT = 15,108", txtwrap
	putexcel (B`row_1':B`row_7'), merge vcenter 
	putexcel (B`row_7':F`row_7')(B`row_9':F`row_9'), border(bottom)
	putexcel B`row_8' = "Mean value of the dependent variable in", txtwrap italic
	putexcel B`row_9' = "		Full Baseline (Nov'18-Oct'19)", txtwrap
	
	putexcel C`row_1' = "Levels", txtwrap
	putexcel C`row_3' = "Percentage", txtwrap italic
	putexcel C`row_4' = "Number of lags", txtwrap italic
	putexcel C`row_5' = "Arellano-Bond", txtwrap italic
	putexcel C`row_6' = "AR(1)", txtwrap italic
	putexcel C`row_7' = "AR(2)", txtwrap italic

	putexcel D`row_10'= ("Total consumption expenditure (INR)") E`row_10'= ("Variety of food items consumed")  F`row_10'= ("Indicator for reduced meals"), txtwrap
 	putexcel B`row_10':F`row_10', border(bottom)
	
	putexcel B`row_11' = "Full Baseline (Nov'18-Oct'19)             NxT = 15,108", txtwrap
	putexcel (B`row_11':B`row_17'), merge vcenter 
	putexcel (B`row_17':F`row_17')(B`row_19':F`row_19'), border(bottom)
	putexcel B`row_18' = "Mean value of the dependent variable in", txtwrap italic
	putexcel B`row_19' = "		Full Baseline (Nov'18-Oct'19)", txtwrap
	
	putexcel C`row_11' = "Levels", txtwrap
	putexcel C`row_13' = "Percentage", txtwrap italic
	putexcel C`row_14' = "Number of lags", txtwrap italic
	putexcel C`row_15' = "Arellano-Bond", txtwrap italic
	putexcel C`row_16' = "AR(1)", txtwrap italic
	putexcel C`row_17' = "AR(2)", txtwrap italic
	
	putexcel (B`row_19':F`row_19'), border(bottom)
	
	* Mean value
		putexcel D`row_9' = full_mean_inc, nformat(number_sep_d2)
		putexcel E`row_9' = full_mean_spending, nformat(number_sep_d2)
		putexcel F`row_9' = full_mean_rmt, nformat(number_sep_d2)
		putexcel D`row_19' = full_mean_cons, nformat(number_sep_d2)
		putexcel E`row_19' = full_mean_conscount, nformat(number_sep_d2)
		putexcel F`row_19' = full_mean_reducedmeal , nformat(number_sep_d2)
		
	* Local income	
		* Full BL
		xtabond local_inc covid_d MarchApril pre_covid_full post_covid, vce(robust)	
		lincom covid_d+MarchApril
			putexcel D`row_1' = (r(estimate)), nformat(number_sep_d2)
				scalar temp = r(estimate)
				scalar temp1 = temp/full_mean_inc
			putexcel D`row_2' = (r(se)), nformat((0.00))
			putexcel D`row_3' = temp1, nformat(percent) italic
		
		estat abond	
			mat a = r(arm)
			putexcel D`row_4' = "1",right
			putexcel D`row_6' = a[1,3] , nformat(0.000)
			putexcel D`row_7' = a[2,3] , nformat(0.000)
					
	// Total expenditure 
		* Full BL
		xtabond total_spending covid_d MarchApril pre_covid_full post_covid, vce(robust) lags(2)
		lincom covid_d+MarchApril
			putexcel E`row_1' = (r(estimate)), nformat(number_sep_d2)
				scalar temp = r(estimate)
				scalar temp1 = temp/full_mean_spending
			putexcel E`row_2' = (r(se)), nformat((0.00))
			putexcel E`row_3' = temp1, nformat(percent) italic
			
		estat abond	
			mat a = r(arm)
			putexcel E`row_4' = "2",right
			putexcel E`row_6' = a[1,3] , nformat(0.000)
			putexcel E`row_7' = a[2,3] , nformat(0.000)
			
	// Remittance income 
		* Full BL
		xtabond rmt_total covid_d MarchApril pre_covid_full post_covid, vce(robust)	
		lincom covid_d+MarchApril
			putexcel F`row_1' = (r(estimate)), nformat(number_sep_d2)
				scalar temp = r(estimate)
				scalar temp1 = temp/full_mean_rmt
			putexcel F`row_2' = (r(se)), nformat((0.00))
			putexcel F`row_3' = temp1, nformat(percent) italic
			
		estat abond	
			mat a = r(arm)
			putexcel F`row_4' = "1",right
			putexcel F`row_6' = a[1,3] , nformat(0.000)
			putexcel F`row_7' = a[2,3] , nformat(0.000)
				
	// Total consumption expenditure
		* Full BL
		xtabond cs_total covid_d MarchApril pre_covid_full post_covid, vce(robust)lags(3)
		lincom covid_d+MarchApril
			putexcel D`row_11' = (r(estimate)), nformat(number_sep_d2)
				scalar temp = r(estimate)
				scalar temp1 = temp/full_mean_cons
			putexcel D`row_12' = (r(se)), nformat((0.00))
			putexcel D`row_13' = temp1, nformat(percent) italic
			
		estat abond	
			mat a = r(arm)
			putexcel D`row_14' = "3",right
			putexcel D`row_16' = a[1,3] , nformat(0.000)
			putexcel D`row_17' = a[2,3] , nformat(0.000)
					
	// Variety of food items consumed 
		* Full BL
		xtabond cs_count covid_d MarchApril pre_covid_full post_covid, vce(robust) lags(3)
		lincom covid_d+MarchApril
			putexcel E`row_11' = (r(estimate)), nformat(number_sep_d2)
				scalar temp = r(estimate)
				scalar temp1 = temp/full_mean_conscount
			putexcel E`row_12' = (r(se)), nformat((0.00))
			putexcel E`row_13' = temp1, nformat(percent) italic
			
		estat abond	
			mat a = r(arm)
			putexcel E`row_14' = "3",right
			putexcel E`row_16' = a[1,3] , nformat(0.000)
			putexcel E`row_17' = a[2,3] , nformat(0.000)
			
	// Indicator of reduced meal
		* Full BL
		xtabond fs_reducemeals covid_d MarchApril pre_covid_full post_covid, vce(robust)	
		lincom covid_d+MarchApril
			putexcel F`row_11' = (r(estimate)), nformat(number_sep_d2)
				scalar temp = r(estimate)
				scalar temp1 = temp/full_mean_reducedmeal
			putexcel F`row_12' = (r(se)), nformat((0.00))
			putexcel F`row_13' = "-", right
			
		estat abond	
			mat a = r(arm)
			putexcel F`row_14' = "1",right
			putexcel F`row_16' = a[1,3] , nformat(0.000)
			putexcel F`row_17' = a[2,3] , nformat(0.000)
			
				
********************************************************************************
* Table A7: Marginal Impacts of COVID-19 Lockdown by Income Quantiles
********************************************************************************				
	
	putexcel set "$tables/analysis_tables.xlsx", sheet("table_a7") modify

	putexcel B2=("Table A7: Marginal Impacts of COVID-19 Lockdown by Income Quantiles"), bold 
	putexcel (B2:H2), merge hcenter vcenter border(bottom)
		
	local row = 3
	putexcel B`row'=("Income Percentiles") D`row'=("Total income (INR)") E`row' = ("Total expenditure (INR)") F`row' = ("Consumption expenditure (INR)") G`row' = ("Variety of food items consumed") H`row' = ("Non-consumption expenditure (INR)"), txtwrap 
	putexcel B`row':C`row', merge hcenter italic
	putexcel B`row':H`row',  border(bottom) 
			
	forval i = 1/17 {
		local row_`i' = `row' + `i'
	}
		
	putexcel B`row_1' = "35th percentile", txtwrap
	putexcel B`row_4' = "50th percentile", txtwrap
	putexcel B`row_7' = "75th percentile", txtwrap
	putexcel B`row_10' = "85th percentile", txtwrap
	
	putexcel C`row_1' = "Coefficient", txtwrap
	putexcel C`row_4' = "Coefficient", txtwrap
	putexcel C`row_7' = "Coefficient", txtwrap
	putexcel C`row_10' = "Coefficient", txtwrap 
	
	putexcel C`row_3' = "Percentage", txtwrap italic
	putexcel C`row_6' = "Percentage", txtwrap italic
	putexcel C`row_9' = "Percentage", txtwrap italic
	putexcel C`row_12' = "Percentage", txtwrap italic
	putexcel B`row_12':H`row_12', border(bottom) 
	
	putexcel B`row_13' = "Mean value of the dependent variable in", txtwrap italic
	putexcel B`row_14' = "		35th percentile", txtwrap
	putexcel B`row_15' = "		50th percentile", txtwrap
	putexcel B`row_16' = "		75th percentile", txtwrap
	putexcel B`row_17' = "		85th percentile", txtwrap
	putexcel B`row_17':H`row_17',  border(bottom) 
	
	// Generate averages first
		* Precovid income
		bys hhid: egen precovid_inc = total(local_inc) if covid_d == 0
		
		* Precovid spending
		bys hhid: egen precovid_totalexp = total(total_spending) if covid_d == 0

		* Consumption exp
		bys hhid: egen precovid_cons = total(cs_total) if covid_d == 0

		* Consumption count
		bys hhid: egen precovid_conscount=total(cs_count) if covid_d == 0

		* Non-consumption exp
		bys hhid: egen precovid_exp=total(exp_total) if covid_d == 0
	
	// Quantiles: 0.35 = 107, 0.5 = 153, 0.75=230, 0.85=261
	preserve 
			* Drop duplicates 
			duplicates drop hhid, force
			
			foreach v in inc totalexp cons conscount exp {
				qui replace precovid_`v' = precovid_`v'/49
				qui sort precovid_`v'
				qui gen r_precovid_`v' = _n
			}
					
			sum precovid_inc if r_precovid_inc < 107 
			putexcel D`row_14' = (r(mean)), nformat(number_sep_d2)
			scalar mean_inc_35 = r(mean)
			
			sum precovid_inc if r_precovid_inc >= 107 & r_precovid_inc < 153 
			putexcel D`row_15' = (r(mean)), nformat(number_sep_d2)
			scalar mean_inc_50 = r(mean)
			
			sum precovid_inc if r_precovid_inc >= 153 & r_precovid_inc < 230 
			putexcel D`row_16' = (r(mean)), nformat(number_sep_d2)
			scalar mean_inc_75 = r(mean)
			
			sum precovid_inc if r_precovid_inc >= 230 
			putexcel D`row_17' = (r(mean)), nformat(number_sep_d2)
			scalar mean_inc_85 = r(mean)
		
			sum precovid_totalexp if r_precovid_totalexp < 107 
			putexcel E`row_14' = (r(mean)), nformat(number_sep_d2)
			scalar mean_totalexp_35 = r(mean)
			
			sum precovid_totalexp if r_precovid_totalexp >= 107 & r_precovid_totalexp < 153 
			putexcel E`row_15' = (r(mean)), nformat(number_sep_d2)
			scalar mean_totalexp_50 = r(mean)
			
			sum precovid_totalexp if r_precovid_totalexp >= 153 & r_precovid_totalexp < 230 
			putexcel E`row_16' = (r(mean)), nformat(number_sep_d2)
			scalar mean_totalexp_75 = r(mean)
		
			sum precovid_totalexp if r_precovid_totalexp >= 230 
			putexcel E`row_17' = (r(mean)), nformat(number_sep_d2)
			scalar mean_totalexp_85 = r(mean)
		
			sum precovid_cons if r_precovid_cons < 107 
			putexcel F`row_14' = (r(mean)), nformat(number_sep_d2)
			scalar mean_cons_35 = r(mean)
			
			sum precovid_cons if r_precovid_cons >= 107 & r_precovid_cons < 153 
			putexcel F`row_15' = (r(mean)), nformat(number_sep_d2)
			scalar mean_cons_50 = r(mean)

			sum precovid_cons if r_precovid_cons >= 153 & r_precovid_cons < 230 
			putexcel F`row_16' = (r(mean)), nformat(number_sep_d2)
			scalar mean_cons_75 = r(mean)

			sum precovid_cons if r_precovid_cons >= 230 
			putexcel F`row_17' = (r(mean)), nformat(number_sep_d2)
			scalar mean_cons_85 = r(mean)

			sum precovid_conscount if r_precovid_conscount < 107 
			putexcel G`row_14' = (r(mean)), nformat(number_sep_d2)
			scalar mean_conscount_35 = r(mean)

			sum precovid_conscount if r_precovid_conscount >= 107 & r_precovid_conscount < 153 
			putexcel G`row_15' = (r(mean)), nformat(number_sep_d2)
			scalar mean_conscount_50 = r(mean)

			sum precovid_conscount if r_precovid_conscount >= 153 & r_precovid_conscount < 230 
			putexcel G`row_16' = (r(mean)), nformat(number_sep_d2)
			scalar mean_conscount_75 = r(mean)

			sum precovid_conscount if r_precovid_conscount >= 230 
			putexcel G`row_17' = (r(mean)), nformat(number_sep_d2)
			scalar mean_conscount_85 = r(mean)

			sum precovid_exp if r_precovid_exp < 107 
			putexcel H`row_14' = (r(mean)), nformat(number_sep_d2)
			scalar mean_exp_35 = r(mean)

			sum precovid_exp if r_precovid_exp >= 107 & r_precovid_exp < 153 
			putexcel H`row_15' = (r(mean)), nformat(number_sep_d2)
			scalar mean_exp_50 = r(mean)

			sum precovid_exp if r_precovid_exp >= 153 & r_precovid_exp < 230 
			putexcel H`row_16' = (r(mean)), nformat(number_sep_d2)
			scalar mean_exp_75 = r(mean)
	
			sum precovid_exp if r_precovid_exp >= 230 
			putexcel H`row_17' = (r(mean)), nformat(number_sep_d2)
			scalar mean_exp_85 = r(mean)
		
	restore 
	
	// Total income  	
		* 0.35
		qreg2 local_inc covid_d MarchApril pre_covid_full post_covid, quantile(0.35) cluster(cluster_id)
			mat a = r(table)
			putexcel D`row_1' = a[1,1],  nformat(number_sep_d2)
			putexcel D`row_2' = a[2,1], nformat((0.00))
			scalar temp = a[1,1]
			scalar temp1 = a[1,1]/mean_inc_35
			putexcel D`row_3' = temp1, nformat(percent) italic
		
		* 0.5
		qreg2 local_inc covid_d MarchApril pre_covid_full post_covid, quantile(0.5) cluster(cluster_id)
			mat a = r(table)
			putexcel D`row_4' = a[1,1],  nformat(number_sep_d2)
			putexcel D`row_5' = a[2,1], nformat((0.00))
			scalar temp = a[1,1]
			scalar temp1 = a[1,1]/mean_inc_50
			putexcel D`row_6' = temp1, nformat(percent) italic
			
		* 0.75
		qreg2 local_inc covid_d MarchApril pre_covid_full post_covid, quantile(0.75) cluster(cluster_id)
			mat a = r(table)
			putexcel D`row_7' = a[1,1],  nformat(number_sep_d2)
			putexcel D`row_8' = a[2,1], nformat((0.00))
			scalar temp = a[1,1]
			scalar temp1 = a[1,1]/mean_inc_75
			putexcel D`row_9' = temp1, nformat(percent) italic
			
		* 0.85
		qreg2 local_inc covid_d MarchApril pre_covid_full post_covid, quantile(0.85) cluster(cluster_id)
			mat a = r(table)
			putexcel D`row_10' = a[1,1],  nformat(number_sep_d2)
			putexcel D`row_11' = a[2,1],nformat((0.00))
			scalar temp = a[1,1]
			scalar temp1 = a[1,1]/mean_inc_85
			putexcel D`row_12' = temp1, nformat(percent) italic
			
	// Total expenditure  
		* 0.35
		qreg2 total_spending covid_d MarchApril pre_covid_full post_covid, quantile(0.35) cluster(cluster_id)
			mat a = r(table)
			putexcel E`row_1' = a[1,1],  nformat(number_sep_d2)
			putexcel E`row_2' = a[2,1], nformat((0.00))
			scalar temp = a[1,1]
			scalar temp1 = a[1,1]/mean_totalexp_35
			putexcel E`row_3' = temp1, nformat(percent) italic
			
		* 0.5
		qreg2 total_spending covid_d MarchApril pre_covid_full post_covid, quantile(0.5) cluster(cluster_id)
			mat a = r(table)
			putexcel E`row_4' = a[1,1],  nformat(number_sep_d2)
			putexcel E`row_5' = a[2,1], nformat((0.00))
			scalar temp = a[1,1]
			scalar temp1 = a[1,1]/mean_totalexp_50
			putexcel E`row_6' = temp1, nformat(percent) italic
			
		* 0.75
		qreg2 total_spending covid_d MarchApril pre_covid_full post_covid, quantile(0.75) cluster(cluster_id)
			mat a = r(table)
			putexcel E`row_7' = a[1,1],  nformat(number_sep_d2)
			putexcel E`row_8' = a[2,1], nformat((0.00))
			scalar temp = a[1,1]
			scalar temp1 = a[1,1]/mean_totalexp_75
			putexcel E`row_9' = temp1, nformat(percent) italic
			
		* 0.85
		qreg2 total_spending covid_d MarchApril pre_covid_full post_covid, quantile(0.85) cluster(cluster_id)
			mat a = r(table)
			putexcel E`row_10' = a[1,1],  nformat(number_sep_d2)
			putexcel E`row_11' = a[2,1], nformat((0.00))
			scalar temp = a[1,1]
			scalar temp1 = a[1,1]/mean_totalexp_85
			putexcel E`row_12' = temp1, nformat(percent) italic
					
	// Total consumption expenditure   
		* 0.35
		qreg2 cs_total covid_d MarchApril pre_covid_full post_covid, quantile(0.35) cluster(cluster_id)
			mat a = r(table)
			putexcel F`row_1' = a[1,1],  nformat(number_sep_d2)
			putexcel F`row_2' = a[2,1], nformat((0.00))
			scalar temp = a[1,1]
			scalar temp1 = a[1,1]/mean_cons_35
			putexcel F`row_3' = temp1, nformat(percent) italic
			
		* 0.5
		qreg2 cs_total covid_d MarchApril pre_covid_full post_covid, quantile(0.5) cluster(cluster_id)
			mat a = r(table)
			putexcel F`row_4' = a[1,1],  nformat(number_sep_d2)
			putexcel F`row_5' = a[2,1], nformat((0.00))
			scalar temp = a[1,1]
			scalar temp1 = a[1,1]/mean_cons_50
			putexcel F`row_6' = temp1, nformat(percent) italic
			
		* 0.75
		qreg2 cs_total covid_d MarchApril pre_covid_full post_covid, quantile(0.75) cluster(cluster_id)
			mat a = r(table)
			putexcel F`row_7' = a[1,1],  nformat(number_sep_d2)
			putexcel F`row_8' = a[2,1], nformat((0.00))
			scalar temp = a[1,1]
			scalar temp1 = a[1,1]/mean_cons_75
			putexcel F`row_9' = temp1, nformat(percent) italic
			
		* 0.85
		qreg2 cs_total covid_d MarchApril pre_covid_full post_covid, quantile(0.85) cluster(cluster_id)
			mat a = r(table)
			putexcel F`row_10' = a[1,1],  nformat(number_sep_d2)
			putexcel F`row_11' = a[2,1], nformat((0.00))
			scalar temp = a[1,1]
			scalar temp1 = a[1,1]/mean_cons_85
			putexcel F`row_12' = temp1, nformat(percent) italic
			
	// Variety of consumption items 
		* 0.35
		qreg2 cs_count covid_d MarchApril pre_covid_full post_covid, quantile(0.35) cluster(cluster_id)
			mat a = r(table)
			putexcel G`row_1' = a[1,1],  nformat(number_sep_d2)
			putexcel G`row_2' = a[2,1], nformat((0.00))
			scalar temp = a[1,1]
			scalar temp1 = a[1,1]/mean_conscount_35
			putexcel G`row_3' = temp1, nformat(percent) italic
			
		* 0.5
		qreg2 cs_count covid_d MarchApril pre_covid_full post_covid, quantile(0.5) cluster(cluster_id)
			mat a = r(table)
			putexcel G`row_4' = a[1,1],  nformat(number_sep_d2)
			putexcel G`row_5' = a[2,1], nformat((0.00))
			scalar temp = a[1,1]
			scalar temp1 = a[1,1]/mean_conscount_50
			putexcel G`row_6' = temp1, nformat(percent) italic
			
		* 0.75
		qreg2 cs_count covid_d MarchApril pre_covid_full post_covid, quantile(0.75) cluster(cluster_id)
			mat a = r(table)
			putexcel G`row_7' = a[1,1],  nformat(number_sep_d2)
			putexcel G`row_8' = a[2,1], nformat((0.00))
			scalar temp = a[1,1]
			scalar temp1 = a[1,1]/mean_conscount_75
			putexcel G`row_9' = temp1, nformat(percent) italic
			
		* 0.85
		qreg2 cs_count covid_d MarchApril pre_covid_full post_covid, quantile(0.85) cluster(cluster_id)
			mat a = r(table)
			putexcel G`row_10' = a[1,1],  nformat(number_sep_d2)
			putexcel G`row_11' = a[2,1], nformat((0.00))
			scalar temp = a[1,1]
			scalar temp1 = a[1,1]/mean_conscount_85
			putexcel G`row_12' = temp1, nformat(percent) italic
		
	// Total non-consumption expenditure 
		* 0.35
		qreg2 exp_total covid_d MarchApril pre_covid_full post_covid, quantile(0.35) cluster(cluster_id)
			mat a = r(table)
			putexcel H`row_1' = a[1,1],  nformat(number_sep_d2)
			putexcel H`row_2' = a[2,1], nformat((0.00))
			scalar temp = a[1,1]
			scalar temp1 = a[1,1]/mean_exp_35
			putexcel H`row_3' = temp1, nformat(percent) italic
			
		* 0.5
		qreg2 exp_total covid_d MarchApril pre_covid_full post_covid, quantile(0.5) cluster(cluster_id)
			mat a = r(table)
			putexcel H`row_4' = a[1,1],  nformat(number_sep_d2)
			putexcel H`row_5' = a[2,1], nformat((0.00))
			scalar temp = a[1,1]
			scalar temp1 = a[1,1]/mean_exp_50
			putexcel H`row_6' = temp1, nformat(percent) italic
			
		* 0.75
		qreg2 exp_total covid_d MarchApril pre_covid_full post_covid, quantile(0.75) cluster(cluster_id)
			mat a = r(table)
			putexcel H`row_7' = a[1,1],  nformat(number_sep_d2)
			putexcel H`row_8' = a[2,1], nformat((0.00))
			scalar temp = a[1,1]
			scalar temp1 = a[1,1]/mean_exp_75
			putexcel H`row_9' = temp1, nformat(percent) italic
			
		* 0.85
		qreg2 exp_total covid_d MarchApril pre_covid_full post_covid, quantile(0.85) cluster(cluster_id)
			mat a = r(table)
			putexcel H`row_10' = a[1,1],  nformat(number_sep_d2)
			putexcel H`row_11' = a[2,1], nformat((0.00))
			scalar temp = a[1,1]
			scalar temp1 = a[1,1]/mean_exp_85
			putexcel H`row_12' = temp1, nformat(percent) italic
			

********************************************************************************
* Table A8: Average Weekly Comparisons of Total Income and Total Expenditure (INR) by Percentiles
********************************************************************************				
	
	putexcel set "$tables/analysis_tables.xlsx", sheet("table_a8") modify
	
	local row = 4
	
	forval i = 1/18 {
		local row_`i' = `row' + `i'
	}

	putexcel B2=("Table A8: Average Weekly Comparisons of Total Income and Total Expenditure (INR) by Percentiles"), bold 
	putexcel (B2:F2), merge hcenter vcenter 
	putexcel C3=("Full Baseline (Nov'18-Oct'19)") 
	putexcel (C3:D3), merge hcenter vcenter border(bottom)
	putexcel E3=("Post Lockdown (Mar'20-Apr'20)") 
	putexcel (E3:F3), merge hcenter vcenter border(bottom)
	
	putexcel B`row'=("Full Baseline Percentiles") C`row'=("Total Income") D`row'= ("Total Expenditure") E`row'=("Total Income") F`row'= ("Total Expenditure"), txtwrap
 	putexcel B`row':F`row', border(bottom)
	putexcel B`row', italic
	
	putexcel B`row_1' = "Above 50th percentile (N = 152)", txtwrap
	putexcel B`row_3' = "Below 50th percentile (N = 155)", txtwrap
	putexcel B`row_4':F`row_4', border(bottom)
	
	putexcel B`row_5' = "Difference", txtwrap
	putexcel B`row_7' = "t-value", txtwrap italic
	
	* Full Baseline Income   
	ttest ave_inc_pre if week == 1, by(bottom50)
		putexcel C`row_1' = (r(mu_1)), nformat(number_sep_d2)
		putexcel C`row_2' = (r(sd_1)), nformat((0.00))
		putexcel C`row_3' = (r(mu_2)), nformat(number_sep_d2)
		putexcel C`row_4' = (r(sd_2)), nformat((0.00))
		putexcel C`row_5' = (r(mu_1)-r(mu_2)), nformat(number_sep_d2)
		putexcel C`row_6' = (r(se)), nformat((0.00))
		putexcel C`row_7' = (r(t)), nformat(number_sep_d2)
	
	* Full Baseline Expenditure  
	ttest ave_spending_pre if week == 1, by(bottom50)
		putexcel D`row_1' = (r(mu_1)), nformat(number_sep_d2)
		putexcel D`row_2' = (r(sd_1)), nformat((0.00))
		putexcel D`row_3' = (r(mu_2)), nformat(number_sep_d2)
		putexcel D`row_4' = (r(sd_2)), nformat((0.00))
		putexcel D`row_5' = (r(mu_1)-r(mu_2)), nformat(number_sep_d2)
		putexcel D`row_6' = (r(se)), nformat((0.00))
		putexcel D`row_7' = (r(t)), nformat(number_sep_d2)
	
	* Post Lockdown Income   
	ttest ave_inc_post if week == 1, by(bottom50)
		putexcel E`row_1' = (r(mu_1)), nformat(number_sep_d2)
		putexcel E`row_2' = (r(sd_1)), nformat((0.00))
		putexcel E`row_3' = (r(mu_2)), nformat(number_sep_d2)
		putexcel E`row_4' = (r(sd_2)), nformat((0.00))
		putexcel E`row_5' = (r(mu_1)-r(mu_2)), nformat(number_sep_d2)
		putexcel E`row_6' = (r(se)), nformat((0.00))
		putexcel E`row_7' = (r(t)), nformat(number_sep_d2)
	
	* Post Lockdown Expenditure  
	ttest ave_spending_post if week == 1, by(bottom50)
		putexcel F`row_1' = (r(mu_1)), nformat(number_sep_d2)
		putexcel F`row_2' = (r(sd_1)), nformat((0.00))
		putexcel F`row_3' = (r(mu_2)), nformat(number_sep_d2)
		putexcel F`row_4' = (r(sd_2)), nformat((0.00))
		putexcel F`row_5' = (r(mu_1)-r(mu_2)), nformat(number_sep_d2)
		putexcel F`row_6' = (r(se)), nformat((0.00))
		putexcel F`row_7' = (r(t)), nformat(number_sep_d2)
			
			
********************************************************************************
* Table A9: Weekly Marginal Impacts on Consumption Expenditure Using Equation (A1)
********************************************************************************	
	
	putexcel set "$tables/analysis_tables.xlsx", sheet("table_a9") modify

	putexcel B2=("Table A9: Weekly Marginal Impacts on Consumption Expenditure using equation A1"), bold txtwrap
	putexcel (B2:C2), merge hcenter vcenter border(bottom)
		
	local row = 3
	putexcel B`row'=("Variables") C`row'=("Dependent varible: Total consumption expenditure (INR)"), txtwrap 
	putexcel B`row':C`row', hcenter border(bottom)
	putexcel B`row', italic
			
	forval i = 1/17 {
		local row_`i' = `row' + `i'
	}
		
	putexcel B`row_1' = "Post-lockdown week dummy", txtwrap
	putexcel B`row_3' = "March-April week dummy", txtwrap
	putexcel B`row_5' = "Pre-lockdown time trend", txtwrap
	putexcel B`row_7' = "Post-lockdown week 2 dummy", txtwrap
	putexcel B`row_9' = "Post-lockdown week 3 dummy", txtwrap
	putexcel B`row_11' = "Post-lockdown week 4 dummy", txtwrap
	putexcel B`row_13' = "Constant", txtwrap
	putexcel B`row_15' = "NxT", txtwrap italic
	putexcel B`row_16' = "R2 (within)", txtwrap italic
	putexcel B`row_17' = "Overall F-stat", txtwrap italic
		
	xtreg cs_total covid_d MarchApril pre_covid_full d_post_covid_w2 d_post_covid_w3 ///
			d_post_covid_w4, fe cluster(cluster_id) nonest
			
		mat a = r(table)
		forvalues i = 1(1)7 {
			local row = `row' + 1 
			local row_1 = `row' + 1
			putexcel C`row' = a[1,`i'],  nformat(number_sep_d2)
			putexcel C`row_1' = a[2,`i'], nformat((0.00))
			local row = `row' + 1 	
		}
		
		putexcel C`row_15' = (e(N)), nformat(number_sep)
		putexcel C`row_16' = (e(r2_w)), nformat(0.000)	
		putexcel C`row_17' = (e(F)), nformat(number_sep_d2)
		
	putexcel B`row_14':C`row_14', border(bottom)
	putexcel B`row_17':C`row_17', border(bottom)	

