* Info on tables
icio_load, iciotable(eora) year(2015) info
* get EORA 2015 Table.
icio_load, iciotable(eora) year(2015)


* Display available country and sector codes for the loaded table
        icio, info

*  Supply-final demand linkages:
*  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

*  What is the GDP (value-added) produced by each country?
        icio, origin(all)

 *  How much value-added does each country produces in a given sector?
        icio, origin(all,19)

 *  What is the aggregate final demand of each country?
        icio, destination(all)

 *  What is the value-added originated in Germany and absorbed in China?
        icio, origin(deu) destination(chn)

 *  Where the value-added produced in the Italian sector 19 is absorbed?
        icio, origin(ita,19) destination(all)

 *  Which final demand sectors in China are the most important for the absorption of US-made value-added?
        icio, origin(usa) destination(chn,all)

 *  Where the GDP produced in each country is absorbed (and save the output as "supply_demand.xls" in the current working directory)?
        icio, origin(all) destination(all) save("supply_demand.xls")

 *  How much USMCA (former NAFTA) countries' final demand in sector 20 is satisfied by Chinese productions?
        icio, origin(chn) destination(usmca,20) groups(usa, mex, can, "usmca")

 *  Value-added trade and GVC participation:
 *  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

 *  Which part of a country’s total exports is home produced, i.e. is domestic GDP?
        icio, exporter(deu) output(dva)

 *  Which part of a country’s total exports can be traced back to other countries GDP?
        icio, exporter(deu) output(fva)

 *  Where the foreign value-added in German exports is produced?
        icio, origin(all) exporter(deu) output(fva)

 *  Considering the bilateral exports from Italy to Germany, where the Italian GDP (domestic VA) re-exported by Germany is absorbed?
        icio, exporter(ita) importer(deu) destination(all) output(dva)

 *  How can be obtained the complete breakdown by origin and destination of the value-added (both domestic and foreign) for Chinese exports to the US?
        icio, origin(all) exporter(chn) importer(usa) destination(all) output(va) save("CHN_to_USA.xls")

 *  Which share of the German exports is related to GVC, i.e. cross more than one border?
        icio, exporter(deu) output(gvc)

 *  Which share of the German exports is related to backward and forward GVC?
        icio, exporter(deu) output(gvcb)
        icio, exporter(deu) output(gvcf)

 *  Note that it is possible to get a detailed assessment of trade in value-added and GVC participation regarding a certain trade flow running:
        icio, exporter(deu)
        icio, exporter(usa) importer(chn)
        icio, exporter(deu,19) importer(chn)

 *  How can the (corrected) Koopman et al. (2014) decomposition be retrieved using icio?
        icio, exporter(deu) perspective(world) approach(sink)

 *  Trade policy analysis:
 *  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

 *  Which is the Chinese GDP that at any point in time, passes through a certain bilateral trade flow, say Chinese exports to the US? In other terms, what is the Chinese GDP potentially exposed to US tariffs on imports from China?
        icio, exp(chn) imp(usa) persp(bilat) output(dva)

 *  Which is the German GDP potentially exposed to US tariffs on all imports?
        icio, origin(deu) imp(usa) persp(importer) output(va)

 *  Which is the German GDP that could be affected by US tariffs on imports in sector 20?
        icio, origin(deu) imp(usa,20) persp(sectimp) output(va)

 *  Which is the exposure of US GDP to a Chinese tariff on US imports in sector 17?
        icio, exp(usa,17) imp(chn) persp(sectbil) output(dva)

 *  To what extent are Italian sectors exposed to a shock on German's exports in sector 20?
        icio, origin(ita,all) exp(deu,20) persp(sectexp) output(va)

 *  Miscellaneous:
 *  -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

 *  Store results in an excel file
        icio, exporter(usa,all) save("USA_exports_decomp.xls")

 *  Display (and save) large dimension results
        icio, exporter(usa,all) save("USA_exports_decomp.xls")
        matlist r(detailed)

        icio, origin(all) destination(all) save("supply_demand.xls")
        matlist r(vby)

 *  Load a user-created table
        icio_load, iciot(user, userp("path_to_the_table_folder") tablen(ADB_2011.csv) countrylist(adb_countrylist.csv))

 *  Computes the detailed decomposition for a list of countries and stores the results in two matrices (dollar values and shares).
        loc countries "ita deu fra esp"
        foreach c of local countries {
             qui icio, exp(`c')
             m st_matrix("total_exports", st_matrix("r(detailed)")[.,1])
             mat results_dollars = nullmat(results_dollars), total_exports
             m st_matrix("total_exports_shares", st_matrix("r(detailed)")[.,2])
             mat results_shares = nullmat(results_shares), total_exports_shares
        }
        matlist results_dollars
        matlist results_shares

