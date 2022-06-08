* Project: Sundarbans 
* Started: Feb 2020 [Heng Zhu]
* Last modified: 10/26/20 [Miki Doan]
* Purpose: Master file for Sundarbans Covid paper  
* Stat v.15

* Clean slate
	clear all
	set more off, perm
	set matsize 5000

********************************************************************************
* General setup
********************************************************************************
* Users can change their initials
* All subsequent files are referred to using dynamic, absolute filepaths

* Set this value to the user currently using this file
    global user "ag"

********************************************************************************
* Set root folder globals
********************************************************************************
	if "$user" == "md" {
		global myDocs "/Users/Miki/Dropbox/Shared/India_Sundarbans/2019-20 COVID study/AJAE Revise n Resubmit"
    }
	
	if "$user" == "ag" {
		global myDocs "C:/Users/gupbh/Dropbox/India Projects/India_Sundarbans/2019-20 COVID study/AJAE Revise n Resubmit"
    }
********************************************************************************
* Set sub-folder globals
********************************************************************************
	global projectFolder          "$myDocs/Sundarbans"
    global data		              "$projectFolder/data"
	global do_files		          "$projectFolder/do_files"
	global output                 "$projectFolder/output"
	global tables                 "$output/tables"
	global figures                "$output/figures"

********************************************************************************
* Create file structure
********************************************************************************
	* Overall analysis
	qui: capture mkdir "$projectFolder/output"
	qui: capture mkdir "$projectFolder/output/tables"
	qui: capture mkdir "$projectFolder/output/figures"

********************************************************************************
* Run do files
********************************************************************************	
	* 1. Create relevant variables  
		do "$do_files/1.create_vars.do"
	
	* 2. Summary stats 
		do "$do_files/2.sumstats_table.do"
		
	* 3. Analysis
		*do "$do_files/3.analysis.do"
		
	* 3b. Analysis with tables: Same as the do file above. Only select to generate analysis tables 
		do "$do_files/3b.analysis_tables.do" 
