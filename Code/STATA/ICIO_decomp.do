
clear
global data EM

* Load Base Data
if "$data" == "EORA" {
	cd "/Users/sebastiankrantz/Documents/Data/EORA"
	global years 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021
	global nyears 22	
	* global countries UGA TZA KEN RWA BDI SSD COD SSA MEA EUU ECA NAC SAS ASE CHN ROA LAC OCE 
	global countries AFG ALB DZA AND AGO ATG ARG ARM ABW AUS AUT AZE BHS BHR BGD BRB BLR BEL BLZ BEN BMU BTN BOL BIH BWA BRA VGB BRN BGR BFA BDI KHM CMR CAN CPV CYM CAF TCD CHL CHN COL COG CRI HRV CUB CYP CZE CIV PRK COD DNK DJI DOM ECU EGY SLV ERI EST ETH FJI FIN FRA PYF GAB GMB GEO DEU GHA GRC GRL GTM GIN GUY HTI HND HKG HUN ISL IND IDN IRN IRQ IRL ISR ITA JAM JPN JOR KAZ KEN KWT KGZ LAO LVA LBN LSO LBR LBY LIE LTU LUX MAC MDG MWI MYS MDV MLI MLT MRT MUS MEX MCO MNG MNE MAR MOZ MMR NAM NPL NLD NCL NZL NIC NER NGA NOR PSE OMN PAK PAN PNG PRY PER PHL POL PRT QAT KOR MDA ROU RUS RWA WSM SMR STP SAU SEN SRB SYC SLE SGP SVK SVN SOM ZAF SSD ESP LKA SDN SUR SWZ SWE CHE SYR TWN TJK THA MKD TGO TTO TUN TUR TKM UGA UKR ARE GBR TZA USA URY UZB VUT VEN VNM YEM ZMB ZWE
	global nctry 187
	* global nsec 26 
} 
if "$data" == "EM" {
	cd "/Users/sebastiankrantz/Documents/Data/EMERGING"
	global years 2010 2015 2016 2017 2018 2019
	global nyears 6	
	* global countries LAC SAS SSA NAC ECA MEA ROA OCE EUU BDI ASE CHN COD KEN RWA SSD TZA UGA 
	global countries ABW AFG AGO AIA ALB AND ANT BES CUW ARE ARG ARM ASM ATA ATF ATG AUS AUT AZE BDI BEL BEN BFA BGD BGR BHR BHS BIH BLM BLR BLZ BMU BOL BRA BRB BRN BTN BWA BVT CAF CAN CCK CHE CHL CHN CIV CMR COD COG COK COL COM CPV CRI CUB CXR CYM CYP CZE DEU DJI DMA DNK DOM DZA ECU EGY ERI ESH ESP EST TWN ETH LIE FIN FJI FLK FRA FRO FSM GAB GBR GEO GHA GIB GIN GLP GMB GNB GNQ GRC GRD GRL GTM GUM GUF GUY HKG HMD HND HRV HTI HUN IDN IND IOT IRL IRN IRQ ISL ISR ITA JAM JOR JPN KAZ KEN KGZ KHM KIR KNA KOR KWT LAO LBN LBR LBY LCA LKA LSO LTU LUX LVA MAC MAR MCO MDA MDG MDV MEX MHL MKD MLI MLT MMR MNG MNP MNE MOZ MRT MSR MTQ MUS MWI MYS MYT NAM NCL NER NFK NGA NIC NIU NLD NOR NPL NRU NZL OMN PAK PAN PER PCN PHL PLW PNG POL PRI PRK PRT PRY PSE PYF QAT REU ROU RUS RWA SAU SDN SHN SEN SRB SGP SGS SJM SLB SLE SLV SMR SOM SPM SSD STP SUR SVK SVN SWE SWZ SXM SYC SYR TCA TCD TGO THA TJK TKL TKM TLS TON TTO TUN TUR TUV TZA UGA UKR UMI URY USA UZB VAT VCT VEN VGB VIR VNM VUT WLF WSM YEM ZAF ZMB ZWE
	global nctry 245
	* global nsec 134
} 

* global nctry 18
global nsec 5 // 17



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

outsheet using "GVC_5_Sectors/${data}_GVC_KWW_BM19.csv", comma replace


*** Exporter-Sector Level Decomposition ***

clear 
local nobs = $nyears * $nctry * $nsec
set obs `nobs'

* ID Variables
gen year = .
gen from_region = ""
gen from_sector = .

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
	foreach ce in $countries {
		qui icio, exporter(`ce', all) perspective(exporter) approach(source) output(detailed)
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


outsheet using "GVC_5_Sectors/${data}_GVC_SEC_BM19.csv", comma replace



*** Detailed Bilateral-Sector Decomposition ***
/* Note: Cannot have importing country sector 
		 because final demand is not available at
		 the sector level 
   Also Note: Computations can take extremely long
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


outsheet using "GVC_5_Sectors/${data}_GVC_BIL_SEC_BM19.csv", comma replace




/* Miscellaneous / Experimental


icio, origin(all) destination(all) save("supply_demand.xlsx")

icio, origin(all) importer(KEN) output(va) save(oKEN.xlsx ) 


forvalues i = 1/17 {
	forvalues j = 1/17 {
			icio, origin(all,`i') destination(all,`j') save("supply_demand.xls", modify sheet("sector_`i'_`j'"))
	}
}

*/
