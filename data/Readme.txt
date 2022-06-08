*****************************INTRODUCTION************************************
AJAE paper "Economic Impacts of the COVID-19 Lockdown in a Remittance-dependent Region"
Authors: Anubhab Gupta, Heng Zhu, Miki Khanh Doan, Aleksandr Michuda, and Binoy Majumder
Corresponding Author Email: anubhab@vt.edu
****************************FOLDER INFORMATION*******************************
The "Data and codes" folder has:
	1. 1 datasets: "dataset.dta"
	2. 4 do files: "master.do", "1.create_vars.do", "2.sumstats_table.do", "3.analysis.do" & "3b.analysis_tables.do"
****************************DATA DESCRIPTION**********************************
Data Description:
	"dataset.dta" is raw data
	A cleaned dataset "dataset_analysis.dta" is created with additionally variables for analysis
************************************ANALYSIS**********************************
Do Files:
	"master.do" runs all the other do files and produces all the results in the paper and the appendix
	"1.create_vars.do" creates variables for anaylsis from raw data, "dataset.dta" and saves the clean and final data, "dataset_analysis.dta"
	"2.sumstats_table.do" produces the summary tables in the paper (Tables 1-4) and the appendix (Appendix Tables A2 and A3)
	"3.analysis.do" produces Figure 1 and all analysis tables in the paper (Tables 5-8) and the appendix (Appendix Tables A1, A4-A9)
	"3b.analysis_tables.do" outputs all the tables in excel spreadsheets