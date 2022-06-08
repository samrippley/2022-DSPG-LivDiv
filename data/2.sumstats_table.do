* Project: Sundarbans 
* Started: Feb 2020 [Heng Zhu]
* Last modified: 10/26/20 [Miki Doan]
* Purpose: Summary stats
* Stat v.15

********************************************************************************
* 0. Select the dataset
********************************************************************************
	use "$data/dataset_analysis.dta", clear	 

********************************************************************************
* Table 1: Summary of Household Demographics 
********************************************************************************

	putexcel set "$tables/summary_tables.xlsx", sheet("table_1") modify
	
	
	putexcel B2=("Table 1: Summary of Household Demographics"), bold 
	putexcel (B2:D2), merge hcenter vcenter border(bottom)
		
	local row = 3
	putexcel B`row'=("Variables") C`row'=("Mean") D`row'= ("Standard Deviation") 
	putexcel B`row':D`row', hcenter border(bottom)
	putexcel D`row', italic
			
	forval i = 1/10 {
		local row_`i' = `row' + `i'
	}
		
	putexcel B`row_1' = "HH head age", txtwrap
	putexcel B`row_2' = "HH head years of education", txtwrap
	putexcel B`row_3' = "Female headed household", txtwrap
	putexcel B`row_4' = "HH size", txtwrap
	putexcel B`row_5' = "Number of children", txtwrap
	putexcel B`row_6' = "Proportion of HH with migrant", txtwrap
	putexcel B`row_7' = "Proportion of HH involved in agriculture", txtwrap
	putexcel B`row_8' = "Proportion of HH with businesses", txtwrap
	putexcel B`row_9' = "Land owned (in kathas)", txtwrap
	putexcel B`row_10' = "Proportion of HH below the Indian poverty line", txtwrap

	local row = `row' + 1 
	foreach var of varlist head_age head_edu head_female nb_hhmem nb_children hh_migrant farm_yn business_yn land_own  {
		sum `var' if week == 1
		*putexcel C`row' = (r(mean)),nformat(number_d2)
		*putexcel D`row' = (r(sd)),nformat(0.00) italic 
		*local row = `row' + 1 	
	}	

	putexcel B`row'=("N"), italic 
		sum nb_hhmem if week == 1
		putexcel C`row' = (r(N))
		putexcel (C`row':D`row'), merge hcenter
	
	putexcel B`row':D`row', border(bottom)		
	putexcel B`row_10':D`row_10', border(bottom)	
		
********************************************************************************
* Table 2: Summary of Financial Diaries   
********************************************************************************		

	putexcel set "$tables/summary_tables.xlsx", sheet("table_2") modify

	putexcel B2=("Table 2: Summary of Financial Diaries"), bold 
	putexcel (B2:E2), merge hcenter vcenter border(bottom)
		
	local row = 3
	putexcel C`row'=("Full Baseline (Nov'18-Oct'19)") D`row'= ("Restricted Baseline (Mar'19-April'19)") E`row'=("Post-Lockdown (Mar'20-April'20)"), txtwrap
	putexcel B`row':E`row', hcenter border(bottom)
		
	forval i = 1/13 {
		local row_`i' = `row' + `i'
	}
		
	putexcel B`row_1' = "Local income (INR)", txtwrap
	putexcel B`row_3' = "Remittances (INR)", txtwrap
	putexcel B`row_5' = "Proportion of HH who borrowed", txtwrap
	putexcel B`row_7' = "Loan size (INR)", txtwrap
	putexcel B`row_9' = "Consumption (INR)", txtwrap
	putexcel B`row_11' = "Non-cons expenditures (INR)", txtwrap
	putexcel B`row_13' = "NxT",txtwrap italic
	
	pctrim br_amt, p(2 98) mark(o0)
		gen br_amt_trim = br_amt
		replace br_amt_trim = . if o0 == 1

	foreach var of varlist local_inc rmt_total d_br br_amt_trim cs_total exp_total {
		local row = `row' + 1 
		local row_1 = `row' + 1
		sum `var' if week < 50
			putexcel C`row' = (r(mean)),nformat(number_d2)
			putexcel C`row_1' = (r(sd)),nformat((0.00)) italic
		sum `var' if inrange(week,18,21)
			putexcel D`row' = (r(mean)),nformat(number_d2)
			putexcel D`row_1' = (r(sd)),nformat((0.00)) italic
		sum `var' if inrange(week,50,53)
			putexcel E`row' = (r(mean)),nformat(number_d2)
			putexcel E`row_1' = (r(sd)),nformat((0.00)) italic		
		local row = `row' + 1 		
	}

	sum local_inc if week < 50
		putexcel C`row_13' = (r(N))
	sum local_inc if inrange(week,18,21)
		putexcel D`row_13' = (r(N))
	sum local_inc if inrange(week,50,53)
		putexcel E`row_13' = (r(N))
		
	putexcel B`row':E`row', border(bottom)		
	putexcel B`row_13':E`row_13', border(bottom)		
		
*************************************************************************************************************
* Table 3: Summary of Household-level Loss of Income Sources, Adjustments, and Migrant Members After Lockdown
*************************************************************************************************************	* Panel (a): Loss of Income Sources
	
	putexcel set "$tables/summary_tables.xlsx", sheet("table_3a") modify

	putexcel B2=("Table 3a: Loss of Income Sources After Lockdown")
	putexcel (B2:D2), merge hcenter vcenter border(bottom) bold 
		
	local row = 3
	putexcel B`row'=("Variables") C`row'=("Mean") D`row'= ("Standard Deviation") 
	putexcel B`row':D`row', hcenter border(bottom)
	putexcel D`row', italic
	
	forval i = 1/5 {
		local row_`i' = `row' + `i'
	}
	
	putexcel B`row_1' = "Proportion of HH...", txtwrap
	putexcel B`row_2' = "		with local job loss", txtwrap
	putexcel B`row_3' = "		with loss of agricultural income", txtwrap
	putexcel B`row_4' = "		with loss of remittance income", txtwrap
	putexcel B`row_5' = "		with loss of MNREGA income", txtwrap
	
	local row = `row' + 2
	foreach var of varlist loss_job_local loss_ag_inc loss_rmt loss_nrega_inc  {
		sum `var' if week == 1
		putexcel C`row' = (r(mean)),nformat(number_d2)
		putexcel D`row' = (r(sd)),nformat(0.00) italic 
		local row = `row' + 1 
	}

	putexcel B`row'=("N"), italic 
	sum attrited if week == 1 & attrited == 0
	putexcel C`row' = (r(N))
	putexcel (C`row':D`row'), merge hcenter
	
	putexcel B`row':D`row', border(bottom)		
	putexcel B`row_5':D`row_5', border(bottom)	
	
  * Panel (b): Adjustments on consumption and assets
	
	putexcel set "$tables/summary_tables.xlsx", sheet("table_3b") modify

	putexcel B2=("Table 3b: Household-level Adjustments on Consumption and Assets After Lockdown")
	putexcel (B2:D2), merge hcenter vcenter border(bottom) bold 
		
	local row = 3
	putexcel B`row'=("Variables") C`row'=("Mean") D`row'= ("Standard Deviation") 
	putexcel B`row':D`row', hcenter border(bottom)
	putexcel D`row', italic
	
	forval i = 1/5 {
		local row_`i' = `row' + `i'
	}
	
	putexcel B`row_1' = "Proportion of HH who...", txtwrap
	putexcel B`row_2' = "		increased consumption from own production", txtwrap
	putexcel B`row_3' = "		foraged", txtwrap
	putexcel B`row_4' = "		postponed buying essential household items", txtwrap
	putexcel B`row_5' = "		sold assets", txtwrap
	
	local row = `row' + 2
	foreach var of varlist ld_cons_prod ld_forage ld_postpone_hhitem ld_sell_assets  {
		sum `var' if week == 1
		putexcel C`row' = (r(mean)),nformat(number_d2)
		putexcel D`row' = (r(sd)),nformat(0.00) italic 
		local row = `row' + 1 
	}

	putexcel B`row'=("N"), italic 
	sum attrited if week == 1 & attrited == 0
	putexcel C`row' = (r(N))
	putexcel (C`row':D`row'), merge hcenter
	
	putexcel B`row':D`row', border(bottom)		
	putexcel B`row_5':D`row_5', border(bottom)	
	
  * Panel (c): Migrant Members
  
	putexcel set "$tables/summary_tables.xlsx", sheet("table_3c") modify

	putexcel B2=("Table 3c: Summary of Migrant Members After Lockdown")
	putexcel (B2:D2), merge hcenter vcenter border(bottom) bold 
		
	local row = 3
	putexcel B`row'=("Variables") C`row'=("Mean") D`row'= ("Standard Deviation") 
	putexcel B`row':D`row', hcenter border(bottom)
	putexcel D`row', italic
	
	forval i = 1/5 {
		local row_`i' = `row' + `i'
	}
	
	putexcel B`row_1' = "Proportion of migrant members at destination who...", txtwrap
	putexcel B`row_2' = "		has work", txtwrap
	putexcel B`row_3' = "		lost jobs temporarily", txtwrap
	putexcel B`row_4' = "		lost jobs permanently", txtwrap
	putexcel B`row_5' = "Money amount from HH to migrant (INR)", txtwrap
	
	local row = `row' + 2
	foreach var of varlist migrant_job_y-migrant_job_loss send_migrant_rmt {
		sum `var' if week == 1
		putexcel C`row' = (r(mean)),nformat(number_d2)
		putexcel D`row' = (r(sd)),nformat(0.00) italic 
		local row = `row' + 1 
	}

	putexcel B`row'=("N"), italic 
	sum send_migrant_rmt if week == 1
	putexcel C`row' = (r(N))
	putexcel (C`row':D`row'), merge hcenter
	
	putexcel B`row':D`row', border(bottom)		
	putexcel B`row_5':D`row_5', border(bottom)		
	
********************************************************************************
* Table 4: Summary of Local Borrowing and Loans 
********************************************************************************	

	putexcel set "$tables/summary_tables.xlsx", sheet("table_4") modify

	putexcel B2=("Table 4: Summary of Local Borrowing and Loans"), bold 
	putexcel (B2:E2), merge hcenter vcenter border(bottom)
		
	local row = 4
	putexcel C3 =("Full Baseline (Nov'18-Oct'19)") D3= ("Restricted Baseline (Mar'19-April'19)") E3=("Post-Lockdown (Mar'20-April'20)"), txtwrap
	putexcel B3:E3, hcenter border(bottom)	
	
	putexcel B4=("(a) Cash loans") 
	putexcel (B4:E4), merge hcenter vcenter border(bottom)
	
	forval i = 1/18 {
		local row_`i' = `row' + `i'
	}
	
	putexcel B`row_9'=("(b) In-kind loans") 
	putexcel (B`row_9':E`row_9'), merge hcenter vcenter border(bottom)	
		
		
	putexcel B`row_1' = "Probability of getting a cash loan", txtwrap
	putexcel B`row_3' = "Loan size", txtwrap
	putexcel B`row_5' = "Markup", txtwrap
	putexcel B`row_7' = "Duration (days)", txtwrap
	
	putexcel B`row_10' = "Probability of getting an in-kind loan", txtwrap
	putexcel B`row_12' = "Loan size", txtwrap
	putexcel B`row_14' = "Markup", txtwrap
	putexcel B`row_16' = "Duration (days)", txtwrap
	
	putexcel B`row_18' = "NxT",txtwrap italic

	pctrim br_cash_amt, p(2 98) mark(o1)
		gen br_cash_amt_trim = br_cash_amt
		replace br_cash_amt_trim = . if o1 == 1
		
	gen br_cash_markup = br_cash_amtdue-br_cash_amt	
	pctrim br_cash_markup, p(2 98) mark(o2)
		gen br_cash_markup_trim = br_cash_markup
		replace br_cash_markup_trim = . if o2 == 1	
		
	pctrim br_cash_duedays, p(2 98) mark(o3)
		gen br_cash_duedays_trim = br_cash_duedays
		replace br_cash_duedays_trim = . if o3 == 1		
	
	foreach var of varlist d_br_cash br_cash_amt_trim br_cash_markup_trim br_cash_duedays_trim {
		local row = `row' + 1 
		local row_1 = `row' + 1
		sum `var' if week < 50
			putexcel C`row' = (r(mean)),nformat(number_d2)
			putexcel C`row_1' = (r(sd)),nformat((0.00)) italic
		sum `var' if inrange(week,18,21)
			putexcel D`row' = (r(mean)),nformat(number_d2)
			putexcel D`row_1' = (r(sd)),nformat((0.00)) italic
		sum `var' if inrange(week,50,53)
			putexcel E`row' = (r(mean)),nformat(number_d2)
			putexcel E`row_1' = (r(sd)),nformat((0.00)) italic		
		local row = `row' + 1 		
	}

	pctrim br_inkind_amt, p(2 98) mark(o4)
		gen br_inkind_amt_trim = br_inkind_amt
		replace br_inkind_amt_trim = . if o4 == 1

	gen br_inkind_markup = br_inkind_amtdue-br_inkind_amt		
	pctrim br_inkind_markup, p(2 98) mark(o5)
		gen br_inkind_markup_trim = br_inkind_markup
		replace br_inkind_markup_trim = . if o5 == 1	
		
	pctrim br_inkind_duedays, p(2 98) mark(o6)
		gen br_inkind_duedays_trim = br_inkind_duedays
		replace br_inkind_duedays_trim = . if o6 == 1		
	
	drop o0 o1 o2 o3 o4 o5 o6 
	
	local row = `row' + 1 
	foreach var of varlist d_br_inkind br_inkind_amt_trim br_inkind_markup_trim br_inkind_duedays_trim {
		local row = `row' + 1 
		local row_1 = `row' + 1
		sum `var' if week < 50
			putexcel C`row' = (r(mean)),nformat(number_d2)
			putexcel C`row_1' = (r(sd)),nformat((0.00)) italic
		sum `var' if inrange(week,18,21)
			putexcel D`row' = (r(mean)),nformat(number_d2)
			putexcel D`row_1' = (r(sd)),nformat((0.00)) italic
		sum `var' if inrange(week,50,53)
			putexcel E`row' = (r(mean)),nformat(number_d2)
			putexcel E`row_1' = (r(sd)),nformat((0.00)) italic		
		local row = `row' + 1 		
	}
	
	sum d_br_inkind if week < 50
		putexcel C`row_18' = (r(N))
	sum d_br_inkind if inrange(week,18,21)
		putexcel D`row_18' = (r(N))
	sum d_br_inkind if inrange(week,50,53)
		putexcel E`row_18' = (r(N))	
	
	putexcel B`row_8':E`row_8', border(bottom)	
	putexcel B`row_17':E`row_17', border(bottom)	
	putexcel B`row_18':E`row_18', border(bottom)	
	
	
********************************************************************************		
* APPENDIX 
********************************************************************************
* Table A2: Summary of Reasons for Borrowing 
********************************************************************************		

	putexcel set "$tables/summary_tables.xlsx", sheet("table_a2") modify

	putexcel B2=("Table A2: Summary of Reasons for Borrowing"), bold 
	putexcel (B2:E2), merge hcenter vcenter border(bottom)
		
	local row = 3
	putexcel C`row'=("Post-Lockdown (Mar'20-April'20)") D`row'=("Full Baseline (Nov'18-Oct'19)") E`row'= ("Restricted Baseline (Mar'19-April'19)"), txtwrap
	putexcel B`row':E`row', hcenter border(bottom)
		
	forval i = 1/9 {
		local row_`i' = `row' + `i'
	}
		
	putexcel B`row_1' = "Consumption", txtwrap
	putexcel B`row_3' = "Non-consumption expenditures", txtwrap
	putexcel B`row_5' = "Loan payback", txtwrap
	putexcel B`row_7' = "Agriculture investment", txtwrap
	putexcel B`row_9' = "NxT",txtwrap italic

	foreach var of varlist br_purpose_cons br_purpose_exp br_purpose_loan br_purpose_ag {
		local row = `row' + 1 
		local row_1 = `row' + 1
		sum `var' if inrange(week,50,53)
			putexcel C`row' = (r(mean)),nformat(number_d2)
			putexcel C`row_1' = (r(sd)),nformat((0.00)) italic	
		sum `var' if week < 50
			putexcel D`row' = (r(mean)),nformat(number_d2)
			putexcel D`row_1' = (r(sd)),nformat((0.00)) italic
		sum `var' if inrange(week,18,21)
			putexcel E`row' = (r(mean)),nformat(number_d2)
			putexcel E`row_1' = (r(sd)),nformat((0.00)) italic
	
		local row = `row' + 1 		
	}

	sum br_purpose_cons if inrange(week,50,53)
		putexcel C`row_9' = (r(N))
	sum br_purpose_cons if week < 50
		putexcel D`row_9' = (r(N))
	sum br_purpose_cons if inrange(week,18,21)
		putexcel E`row_9' = (r(N))
	
	putexcel B`row_9':E`row_9', border(bottom)		
	putexcel B`row_8':E`row_8', border(bottom)	
	
********************************************************************************
* Table A3: Desired volumes of loans
********************************************************************************	

	putexcel set "$tables/summary_tables.xlsx", sheet("table_a3") modify

	putexcel B2=("Table A3: Desired Volumes of Loans in Weeks Post Lockdown"), bold 
	putexcel (B2:F2), merge hcenter vcenter border(bottom)
		
	local row = 3
	putexcel C3 =("Week 1") D3= ("Week 2") E3=("Week 3") F3= ("Week 4"), txtwrap
	putexcel B3:F3, hcenter border(bottom)	
	
	forval i = 1/8 {
		local row_`i' = `row' + `i'
	}	
		
	putexcel B`row_1' = "Cash loans desired", txtwrap
	putexcel B`row_3' = "Percentage able to borrow in cash", txtwrap
	putexcel B`row_5' = "In-kind loans desired", txtwrap
	putexcel B`row_7' = "Percentage able to borrow in kind" , txtwrap
	
	gen br_cash_obtained_prop = br_cash_amt/br_cash_wanted
	gen br_inkind_obtained_prop = br_inkind_amt/br_inkind_wanted
	
	foreach var of varlist br_cash_wanted br_cash_obtained_prop br_inkind_wanted br_inkind_obtained_prop {
		local row = `row' + 1 
		local row_1 = `row' + 1
		sum `var' if week == 50
			putexcel C`row' = (r(mean)),nformat(number_d2)
			putexcel C`row_1' = (r(sd)),nformat((0.00)) italic
		sum `var' if week == 51
			putexcel D`row' = (r(mean)),nformat(number_d2)
			putexcel D`row_1' = (r(sd)),nformat((0.00)) italic	
		sum `var' if week == 52
			putexcel E`row' = (r(mean)),nformat(number_d2)
			putexcel E`row_1' = (r(sd)),nformat((0.00)) italic
		sum `var' if week == 53
			putexcel F`row' = (r(mean)),nformat(number_d2)
			putexcel F`row_1' = (r(sd)),nformat((0.00)) italic
		local row = `row' + 1 	
	}
	
