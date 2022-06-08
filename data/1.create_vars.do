* Project: Sundarbans 
* Started: Feb 2020 [Heng Zhu]
* Last modified: 10/26/20 [Miki Doan]
* Purpose: Create variables 
* Stat v.15

********************************************************************************
* 0. Select the dataset
********************************************************************************
	use "$data/dataset.dta", clear	
		
********************************************************************************
* 1. Create relevant variables 
********************************************************************************		
	* COVID dummy, in paper COVID_t
	gen covid_d = 0 
		replace covid_d = 1 if week == 50 | week == 51 | week == 52 | week == 53
	
	label variable covid_d "COVID dummy (week 50-53)" 
	
	* MarchApril dummy, in paper MarchApril_t
	gen MarchApril = 0 
		replace MarchApril = 1 if week == 18 | week == 19 | week == 20 | week == 21
		replace MarchApril = 1 if week == 50 | week == 51 | week == 52 | week == 53
		
	label variable MarchApril "MarchApril dummy (week 18-21 and week 50-53)" 
	
	* March2020 dummy
	gen March2020 = 0
		replace March2020 = 1 if week == 50 | week == 51 
		
	label variable March2020 "March 2020 dummy (week 50-51)" 
		
	* April2020 dummy	
	gen April2020 = 0
		replace April2020 = 1 if week == 52 | week == 53
		
	label variable MarchApril "April2020 dummy (week 52-53)" 
		
	* Pre-COVID trend, in paper PreCOVID_t
	gen pre_covid_full = week 
		replace pre_covid_full = 0 if week >= 50 
	
	label variable pre_covid_full "Pre-COVID trend for full baseline" 
	
	* Pre-COVID trend for restricted baseline
	gen pre_covid_rtd = 0 
		replace pre_covid_rtd = 1 if week == 18
		replace pre_covid_rtd = 2 if week == 19
		replace pre_covid_rtd = 3 if week == 20
		replace pre_covid_rtd = 4 if week == 21
		
	label variable pre_covid_rtd "Pre-COVID trend for restricted baseline" 	
		
	* Post-COVID trend, in paper PostCOVID_t	
	gen post_covid = 0 
		replace post_covid = 1 if week == 50
		replace post_covid = 2 if week == 51
		replace post_covid = 3 if week == 52
		replace post_covid = 4 if week == 53
		
	label variable post_covid "Post-COVID trend" 	
		
	* Generating post-COVID weekly dummies
	forvalues i = 1(1)4 {
		gen d_post_covid_w`i' = 0
		replace d_post_covid_w`i' = 1 if post_covid == `i' 
		label variable d_post_covid_w`i' "Post-COVID week `i' dummy"
	}	
	
	order covid_d-d_post_covid_w4, after(date)
	
	* Generating monthly time trend based on calendar weeks (week_num)
	gen month_t = 11     if week_num == 0  | week_num == 1  | week_num == 2						       	           /*NOV 2018*/
	replace month_t = 12 if week_num == 3  | week_num == 4  | week_num == 5  | week_num == 6 				       /*DEC 2018*/
	replace month_t = 1  if week_num == 7  | week_num == 8  | week_num == 9  | week_num == 10  | week_num == 11    /*JAN 2019*/
	replace month_t = 2  if week_num == 12  | week_num == 13  | week_num == 14  | week_num == 15  		           /*FEB 2019*/
	replace month_t = 3  if week_num == 16  | week_num == 17  | week_num == 18  | week_num == 19 		           /*MAR 2019*/
	replace month_t = 4  if week_num == 20  | week_num == 21  | week_num == 22  | week_num == 23 		           /*APR 2019*/
	replace month_t = 5  if week_num == 24  | week_num == 25  | week_num == 26  | week_num == 27  | week_num == 28 /*MAY 2019*/
	replace month_t = 6  if week_num == 29  | week_num == 30  | week_num == 31  | week_num == 32		           /*JUN 2019*/	
	replace month_t = 7  if week_num == 33  | week_num == 34  | week_num == 35  | week_num == 36  | week_num == 37 /*JUL 2019*/
	replace month_t = 8  if week_num == 38  | week_num == 39  | week_num == 40  | week_num == 41		           /*AUG 2019*/
	replace month_t = 9  if week_num == 42  | week_num == 43  | week_num == 44  | week_num == 45		           /*SEP 2019*/	
	replace month_t = 10 if week_num == 46  | week_num == 47  | week_num == 48  | week_num == 49		           /*OCT 2019*/
	replace month_t = 0  if month_t == .
	lab variable month_t "Monthly Trend"

	* Average spending percentage drop
	sort hhid week 
	bys hhid: egen ave_spending_pre = mean(total_spending) if covid_d == 0 
		replace ave_spending_pre = ave_spending_pre[_n-1] if ave_spending_pre == . 
	bys hhid: egen ave_spending_post = mean(total_spending) if covid_d == 1
		sort hhid ave_spending_post 
		replace ave_spending_post = ave_spending_post[_n-1] if ave_spending_post == . 
	gen ave_spending_pc_drop = (ave_spending_pre-ave_spending_post)/ave_spending_pre
	
	label variable ave_spending_pre "Average household spending pre-COVID"
	label variable ave_spending_post "Average household spending post-COVID"
	label variable ave_spending_pc_drop "Percentage drop in average household spending post_COVID"
	
	order ave_spending_pre-ave_spending_pc_drop, after(total_spending)
	
	* Average total income percentage drop 
	sort hhid week 
	bys hhid: egen ave_inc_pre = mean(total_inc) if covid_d == 0 
		replace ave_inc_pre = ave_inc_pre[_n-1] if ave_inc_pre == . 
	bys hhid: egen ave_inc_post = mean(total_inc) if covid_d == 1
		sort hhid ave_inc_post 
		replace ave_inc_post = ave_inc_post[_n-1] if ave_inc_post == . 
	gen ave_inc_pc_drop = (ave_inc_pre-ave_inc_post)/ave_inc_pre
	
	label variable ave_inc_pre "Average household total income pre-COVID (local income and remittances)"
	label variable ave_inc_post "Average household total income post-COVID (local income and remittances)"
	label variable ave_inc_pc_drop "Percentage drop in average household total income post_COVID"
		
	* Poverty level
	bys hhid: gen ave_inc_pre_perindiv = ave_inc_pre/nb_hhmem if covid_d == 0
			
		* Indicate living below 204 INR per week per person in rural india 
		bys hhid: gen below_204inr_pre = (ave_inc_pre_perindiv<204) if covid_d == 0
			replace below_204inr_pre = . if ave_inc_pre_perindiv == . 
			
		* Indicate household at the bottom 50% from 2019 (Full Baseline) 
		sort ave_inc_pre
		sum ave_inc_pre, detail
		gen bottom50 = (ave_inc_pre <= r(p50))
								
		* Label variables
		label variable ave_inc_pre_perindiv "Mean household income during 12m FD divided by household size" 
		label variable below_204inr_pre "Dummy for households who live below 204 per week per person (Indian povety line for rural India)"
		label variable bottom50 "Dummy for households at the bottom 50% from 2019 full BL"
		
		order ave_inc_pre-bottom50, after(total_inc)

********************************************************************************
* 3. Save the dataset
********************************************************************************		
	save "$data/dataset_analysis.dta", replace 
