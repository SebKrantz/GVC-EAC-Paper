
clear
global data EM

* Load Base Data
if "$data" == "EORA" {
	cd "/Users/sebastiankrantz/Documents/Data/EORA"
	global years 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021
	global nyears 22	
	global countries UGA TZA KEN RWA BDI SSD COD SSA MEA EUU ECA NAC SAS ASE CHN ROA LAC OCE 
	global nsec 26 
} 
if "$data" == "EM" {
	cd "/Users/sebastiankrantz/Documents/Data/EMERGING"
	global years 2010 2015 2016 2017 2018 2019
	global nyears 6	
	global countries LAC SAS SSA NAC ECA MEA ROA OCE EUU BDI ASE CHN COD KEN RWA SSD TZA UGA 
	global nsec 134
} 

global nctry 18
// global nsec 17



*** KWW Decomposition (Corrected) ***

clear 
local nobs = $nyears * $nctry
set obs `nobs'

* ID Variables
gen year = .
gen country = ""

* Generate Additional Names
foreach var in gexp dc dva vax ref ddc fc fva fdc {
	gen `var' = .
}

foreach y in $years {
	
	di "Year: `y'"
	
	* Load ICIO Table
	icio_clean
	icio_load, iciot(user, userp("ICIO_CSV") tablen(${data}_`y'.csv) countrylist(${data}_countrylist.csv))  
	icio, info
	
	qui foreach c in $countries {
		icio, exporter(`c') perspective(world) approach(sink)
		// matlist r(detailed)
		mat res = r(detailed)
		local j = `j'+1
		replace year = `y' in `j'
		replace country = "`c'" in `j'
		replace gexp = res[1, 1] in `j'
		replace dc = res[2, 1] in `j'
		replace dva = res[3, 1] in `j'
		replace vax = res[4, 1] in `j'
		replace ref = res[5, 1] in `j'
		replace ddc = res[6, 1] in `j'
		replace fc = res[7, 1] in `j'
		replace fva = res[8, 1] in `j'
		replace fdc = res[9, 1] in `j'
	}
}

outsheet using "ICIO_CSV/${data}_GVC_KWW_BM19.csv", comma replace



*** Detailed Bilateral-Sector Decomposition ***
/* Note: Cannot have importing country sector 
		 because final demand is not available at
		 the sector level 
*/

clear 
local nobs = $nyears * $nctry * $nsec * ($nctry-1)
set obs `nobs'

* ID Variables
gen year = .
gen from_region = ""
gen from_sector = .
gen to_region = ""

* Generate Additional Names
foreach var in gexp dc dva vax davax ref ddc fc fva fdc gvc gvcb gvcf {
	gen `var' = .
}
	
foreach y in $years {
	
	di "Year: `y'"
	
	* Load ICIO Table
	icio_clean
	icio_load, iciot(user, userp("ICIO_CSV") tablen(${data}_`y'.csv) countrylist(${data}_countrylist.csv))  
	icio, info

	* Detailed Decomposition
	foreach ci in $countries {
		foreach ce in $countries {
			if "`ce'" == "`ci'" {
				continue	
			} 
			qui icio, exporter(`ce', all) importer(`ci') perspective(exporter) approach(source) output(detailed)
			// matlist r(detailed)
			mat res = r(detailed)
			local iter = `iter'+1
			local start = (`iter'-1)*$nsec + 1
			local end = `start' + $nsec - 1
			* di "EXP: `ce', IMP: `ci'-`si', Rows: `start'-`end'"
			local i = 0
			qui forvalues j = `start'/`end' {
				local i = `i'+1
				replace year = `y' in `j'
				replace from_region = "`ce'" in `j'
				replace from_sector = `i' in `j'
				replace to_region = "`ci'" in `j'
				replace gexp = res[1, `i'] in `j'
				replace dc = res[2, `i'] in `j'
				replace dva = res[3, `i'] in `j'
				replace vax = res[4, `i'] in `j'
				replace davax = res[5, `i'] in `j'
				replace ref = res[6, `i'] in `j'
				replace ddc = res[7, `i'] in `j'
				replace fc = res[8, `i'] in `j'
				replace fva = res[9, `i'] in `j'
				replace fdc = res[10, `i'] in `j'
				replace gvc = res[11, `i'] in `j'
				replace gvcb = res[12, `i'] in `j'
				replace gvcf = res[13, `i'] in `j'
			}			
		}
}
}


outsheet using "ICIO_CSV/${data}_GVC_BIL_SEC_BM19.csv", comma replace




/* Miscellaneous / Experimental


icio, origin(all) destination(all) save("supply_demand.xlsx")

icio, origin(all) importer(KEN) output(va) save(oKEN.xlsx ) 


forvalues i = 1/17 {
	forvalues j = 1/17 {
			icio, origin(all,`i') destination(all,`j') save("supply_demand.xls", modify sheet("sector_`i'_`j'"))
	}
}

*/
