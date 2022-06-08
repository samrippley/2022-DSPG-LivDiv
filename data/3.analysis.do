* Project: Sundarbans 
* Started: Feb 2020 [Heng Zhu]
* Last modified: 10/26/20 [Miki Doan]
* Purpose: Analysis
* Stat v.15

* set scheme plotplain

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
* Table 5: Marginal Impacts of COVID-19 Lockdown on Total Income,
*				Total Expenditure, and Remittance Income 
********************************************************************************			
	
	foreach v in local_inc total_spending rmt_total {
		// Full BL
			xtreg `v' covid_d MarchApril pre_covid_full post_covid, fe cluster(cluster_id) nonest
			lincom covid_d+MarchApril
			sum `v' if covid_d == 0 
			
		// Restricted BL
			xtreg `v' covid_d if MarchApril == 1, fe cluster(cluster_id) nonest
			sum `v' if covid_d == 0 & MarchApril == 1 
	}

********************************************************************************
* Table 6: Marginal Impacts of COVID-19 Lockdown on Consumption Variables 
********************************************************************************			
	
	foreach v in cs_total cs_count fs_reducemeals {
		// Full BL
			xtreg `v' covid_d MarchApril pre_covid_full post_covid, fe cluster(cluster_id) nonest
			lincom covid_d+MarchApril
			sum `v' if covid_d == 0 
			
		// Restricted BL
			xtreg `v' covid_d if MarchApril == 1, fe cluster(cluster_id) nonest
			sum `v' if covid_d == 0 & MarchApril == 1 
	}
	
********************************************************************************
* Table 7: Marginal Impacts of COVID-19 Lockdown on Borrowing and
*		 Non-consumption Expenditure Using Main Specification
********************************************************************************			
	
	foreach v in d_br_cash d_br_inkind exp_total {
		// Full BL 
			xtreg `v' covid_d MarchApril pre_covid_full post_covid, fe cluster(cluster_id) nonest
			lincom covid_d+MarchApril
			sum `v' if covid_d == 0 
			
		// Restricted BL
			xtreg `v' covid_d if MarchApril == 1, fe cluster(cluster_id) nonest
			sum `v' if covid_d == 0 & MarchApril == 1 
	}
	
	
********************************************************************************	
* Table 8: Conditional Correlations of Percentage Reduction in 
*				 Total Income and Total Expenditure from Baseline
********************************************************************************

	// Percentage reduction in total income (local income + remittance from baseline 
	reg ave_inc_pc_drop ave_inc_pre any_aid bottom50 nb_hhmem head_female hh_migrant farm_yn business_yn ///
			loss_job_local loss_ag_inc loss_rmt loss_business_inc loss_nrega_inc if week == 1, robust 

	// Percentage reduction in total spending from baseline 			
	reg ave_spending_pc_drop ave_spending_pre any_aid bottom50 nb_hhmem head_female hh_migrant farm_yn business_yn ///
			loss_job_local loss_ag_inc loss_rmt loss_business_inc loss_nrega_inc if week == 1, robust 

			
********************************************************************************		
* APPENDIX 
********************************************************************************
* Table A1: Probability of Attrition Based on Baseline Household Characteristics
********************************************************************************		
	
	probit attrited head_age head_edu head_female nb_hhmem nb_children hh_migrant ///
			farm_yn business_yn land_own if week == 1
	
********************************************************************************	
* Table A4: Marginal Impacts on Total Income, Total Expenditure, and Remittance Income
*						Using Alternative Specifications
********************************************************************************				

	foreach v in local_inc total_spending rmt_total {
		// Using time (week) fixed effects
			* Full BL
				xtreg `v' covid_d MarchApril i.week, fe cluster(cluster_id) nonest
				lincom covid_d+MarchApril
			
			* Restricted BL
				xtreg `v' covid_d i.week if MarchApril == 1, fe cluster(cluster_id) nonest	
				
		// Using separate indicators for March and April 2020 
			xtreg `v' March2020 April2020 pre_covid_full post_covid, fe cluster(cluster_id) nonest
	}
	
********************************************************************************	
* Table A5: Marginal Impacts on Consumption Using Alternative Specifications
********************************************************************************				

	foreach v in cs_total cs_count fs_reducemeals {
		// Using time (week) fixed effects
			* Full BL
				xtreg `v' covid_d MarchApril i.week, fe cluster(cluster_id) nonest
				lincom covid_d+MarchApril
			
			* Restricted BL
				xtreg `v' covid_d i.week if MarchApril == 1, fe cluster(cluster_id) nonest	
				
		// Using separate indicators for March and April 2020 
			xtreg `v' March2020 April2020 pre_covid_full post_covid, fe cluster(cluster_id) nonest
	}			
			
********************************************************************************	
* Table A6: Marginal Impacts on Treating the Data as Dynamic Panel 
* 				 (using Arellano-Bond Specification)
********************************************************************************				
	
	// 1 lag (Full BL)
	foreach v in local_inc rmt_total fs_reducemeals {
		xtabond `v' covid_d MarchApril pre_covid_full post_covid, vce(robust)
		lincom covid_d+MarchApril
		estat abond		
	}
	
	// 2 lags (Full BL) 
		xtabond total_spending covid_d MarchApril pre_covid_full post_covid, vce(robust) lags(2)
		lincom covid_d+MarchApril
		estat abond
					
	// 3 lags (Full BL) 
	foreach v in cs_total cs_count {
		xtabond `v' covid_d MarchApril pre_covid_full post_covid, vce(robust) lags(3)
		lincom covid_d+MarchApril
		estat abond
	}	

********************************************************************************
* Table A7: Marginal Impacts of COVID-19 Lockdown by Income Quantiles
********************************************************************************	
	
	preserve 
	foreach j in local_inc total_spending cs_total cs_count exp_total {
		foreach v in 0.35 0.5 0.75 0.85 {
			qreg2 `j' covid_d MarchApril pre_covid_full post_covid, quantile(`v') cluster(cluster_id)
		}
	}

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

		* Drop duplicates 
		duplicates drop hhid, force
	
	// Quantiles: 0.35 = 107, 0.5 = 153, 0.75=230, 0.85=261
		foreach v in inc totalexp cons conscount exp {
			qui replace precovid_`v' = precovid_`v'/49
			qui sort precovid_`v'
			qui gen r_precovid_`v' = _n
			*0.35 for `v'
			sum precovid_`v' if r_precovid_`v' < 107 
			*0.5 for `v'
			sum precovid_`v' if r_precovid_`v' >= 107 & r_precovid_`v' < 153 
			*0.75 for `v'
			sum precovid_`v' if r_precovid_`v' >= 153 & r_precovid_`v' < 230 
			*0.85 for `v'
			sum precovid_`v' if r_precovid_`v' >= 230 
		}
	restore	
	
	
********************************************************************************
* Table A8: Average Weekly Comparisons of Total Income and Total Expenditure (in INR) by Percentiles
********************************************************************************				
			
	ttest ave_inc_pre if week == 1, by(bottom50)
	ttest ave_spending_pre if week == 1, by(bottom50)
	ttest ave_inc_post if week == 1, by(bottom50)
	ttest ave_spending_post if week == 1, by(bottom50)
			
			
********************************************************************************
* Table A9: Weekly Marginal Impacts on Consumption Expenditure Using Equation (A1)
********************************************************************************
	
	xtreg cs_total covid_d MarchApril pre_covid_full d_post_covid_w2 d_post_covid_w3 ///
			d_post_covid_w4, fe cluster(cluster_id) nonest
			




	
