library(fastverse)
set_collapse(mask = c("manip", "helper", "special"))
fastverse_extend(ggplot2)
vec_ptype2.factor.factor <- function(x, y, ...) x

EAC <- c("UGA", "TZA", "KEN", "RWA", "BDI", "SSD", "COD")

####################################
# UN Comtrade, Accessed Through OTS
####################################

# devtools::install_github("ropensci/tradestatistics")
fastverse_extend(tradestatistics)
View(ots_countries)

# Product-Level Bilateral Trade Flows
EAC_DATA_YRPC <- ots_create_tidy_data(years = 2005:2015,
                                      reporters = tolower(EAC), 
                                      partners = tolower(EAC), 
                                      table = "yrpc")

# Aggregating to section level 
EAC_DATA_YRPC_USD15_SEC <- EAC_DATA_YRPC |> 
  subset(!is.na(section_code)) |> 
  ots_gdp_deflator_adjustment(reference_year = 2015) |>  
  mutate(trade_value_usd = trade_value_usd_imp + trade_value_usd_exp) |> 
  join(ots_commodities, on = "commodity_code", drop = TRUE) |> 
  select(-conversion_year, -gdp_deflator, -observation) |> 
  collap( ~ year + reporter_iso + partner_iso + section_code, "fsum_uw", w = ~ trade_value_usd) 
  
descr(EAC_DATA_YRPC_USD15_SEC)
# Aggregate over Time
EAC_DATA_USD15_SEC |> 
  collap(~ reporter_iso + partner_iso + section_code, "fsum_uw", w = ~ trade_value_usd) |> View()


# Product-Level Aggregate Trade Flows
EAC_DATA_YRC <- ots_create_tidy_data(years = 2005:2015,
                                     reporters = tolower(EAC), 
                                     table = "yrc")

# Aggregating to section level 
EAC_DATA_YRC_USD15_SEC <- EAC_DATA_YRC |> 
  subset(!is.na(section_code)) |> 
  ots_gdp_deflator_adjustment(reference_year = 2015) |>  
  mutate(trade_value_usd = trade_value_usd_imp + trade_value_usd_exp) |> 
  join(ots_commodities, on = "commodity_code", drop = TRUE) |> 
  select(-conversion_year, -gdp_deflator, -observation) |> 
  collap( ~ year + reporter_iso + section_code, "fsum_uw", w = ~ trade_value_usd) 


# Exploratory....

# library(fastverse)
# ots_data = qs::qread("/Users/sebastiankrantz/Documents/IFW Kiel/Africa-Macro-Stability/Data/Trade/ots_data.qs")
# ots_data[]
# tradestatistics::ots_sections # Cannot join to data...
#

# => Julian Hinz says their methodology is not inocuous

####################################
# CEPII BACI
####################################

BACI_2d <- qs::qread("/Users/sebastiankrantz/Documents/Data/CEPII BACI 2021/BACI.qs") |> 
           mutate(code_2d = trunc(k / 10000L)) |> 
           collap(v + q ~ t + i + j + code_2d, fsum, fill = TRUE, keep.col.order = FALSE)

# Adding Product Codes and aggregating to 2-digit level: Still too detailed...
product_codes <- fread("/Users/sebastiankrantz/Documents/Data/CEPII BACI 2021/product_codes_HS07_V202102.csv")
# H3 = HS 2007, see: https://wits.worldbank.org/product_concordance.html
HS <- readxl::read_xls("~/Documents/Data/Classifications/HSProducts.xls", sheet = "H3 Nomenclature") |> 
      janitor::clean_names()
# product_codes |> join(HS, on = c("code" = "product_code")) |> View()
product_codes_2d <- product_codes |> mutate(code_2d = trunc(code / 10000L)) |> 
  join(subset(HS, tier == 1L) |> mutate(product_code = as.integer(product_code)), 
       on = c("code_2d" = "product_code")) |> 
  select(code_2d, product_description) |> 
  unique(cols = "code_2d") |> 
  mutate(product_description = qF(product_description))

# Add OTS sections
ots_commodities_2d <- ots_commodities |> 
  mutate(code_2d = trunc(as.integer(commodity_code) / 10000L), 
         section_code = as.integer(section_code),
         section_fullname_english = qF(section_fullname_english)) |> 
  subset(is.finite(code_2d) & code_2d > 0, code_2d, section_code, section_fullname_english) |> 
  unique()

BACI_2d %<>% join(join(product_codes_2d, ots_commodities_2d)) 
rm(product_codes, product_codes_2d, HS, ots_commodities_2d)

# Adding Countries
country_codes <- fread("/Users/sebastiankrantz/Documents/Data/CEPII BACI 2021/country_codes_V202102.csv") |> 
                 compute(i = country_code, iso3 = qF(iso_3digit_alpha))
BACI_2d <- BACI_2d |> transform(
  i = country_codes$iso3[ckmatch(i, country_codes$i)],   
  j = country_codes$iso3[ckmatch(j, country_codes$i)]   
) |> rename(t = year, i = iso3_o, j = iso3_d, v = value, q = quantity)
rm(country_codes)

# Saving 
BACI_2d |> qs::qsave("/Users/sebastiankrantz/Documents/Data/CEPII BACI 2021/BACI_2d.qs") 
BACI_2d <- qs::qread("/Users/sebastiankrantz/Documents/Data/CEPII BACI 2021/BACI_2d.qs") 

# Aggregating: EAC and ROW
EAC_BACI_SEC <- BACI_2d |> 
  subset(iso3_o %in% africamonitor::am_countries_wld$ISO3 & iso3_d %in% africamonitor::am_countries_wld$ISO3) |> 
  mutate(iso3_o = iif(iso3_o %in% EAC, as.character(iso3_o), "ROW"), 
         iso3_d = iif(iso3_d %in% EAC, as.character(iso3_d), "ROW")) |> 
  collap(value + quantity + section_fullname_english ~ year + iso3_o + iso3_d + section_code, 
         fsum, flast, na.rm = FALSE) |> 
  subset(iso3_o != iso3_d)

EAC_BACI_MIG <- EAC_BACI_SEC |> group_by(iso3_o, iso3_d, year) |> select(value, quantity) |> fsum()

EAC_BACI_AGG <- EAC_BACI_MIG |> 
  subset(between(year, 2010, 2015)) |> 
  group_by(iso3_o, iso3_d) |> 
  select(value, quantity) |> fmean() |> 
  transformv(c(value, quantity), `/`, 1e6) |> qDT()

# Ratios: ROW to EAC Trade
# Imports
EAC_BACI_AGG[iso3_o == "ROW", sum(value)] / EAC_BACI_AGG[iso3_o %in% EAC & iso3_d %in% EAC, sum(value)]
# Exports
EAC_BACI_AGG[iso3_d == "ROW", sum(value)] / EAC_BACI_AGG[iso3_o %in% EAC & iso3_d %in% EAC, sum(value)]
# Total
EAC_BACI_AGG[iso3_o == "ROW" | iso3_d == "ROW", sum(value)] / EAC_BACI_AGG[iso3_o %in% EAC & iso3_d %in% EAC, sum(value)]

# Trade Flow Diagram
# With ROW
migest::mig_chord(EAC_BACI_AGG) # Millions of current USD
dev.copy(pdf, "Figures/REV/BACI_MIG_2010_15_ROW.pdf", width = 5, height = 5)
dev.off()
# Without ROW
migest::mig_chord(subset(EAC_BACI_AGG, iso3_o != "ROW" & iso3_d != "ROW")) # Millions of current USD
dev.copy(pdf, "Figures/REV/BACI_MIG_2010_15.pdf", width = 5, height = 5)
dev.off()

# Also See Alluvial Plots: https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html
# library(ggalluvial)
# Similar to 


####################################
# IMF DOTS
####################################

library(rdbnomics)
rdb_ds = rdb_dimensions("IMF", "DOT", simplify = TRUE)
View(unlist2d(rdb_ds))
EAC_ISO2 = c("UG", "TZ", "KE", "RW", "BI", "SS", "CD")
EAC_DOT = rdb("IMF", "DOT", dimensions = list(REF_AREA = c(EAC_ISO2, "W00"),
                                              COUNTERPART_AREA = c(EAC_ISO2, "W00"),
                                              INDICATOR = c("TXG_FOB_USD", "TMG_CIF_USD"), 
                                              FREQ = "A"))
EAC_DOT <- EAC_DOT |> janitor::clean_names() |> 
  pivot(c("ref_area", "counterpart_area", "original_period"), check.dups = TRUE,
        names = "indicator_2", labels = "indicator", values = "value", how = "w", fill = 0) |> 
  rename(original_period = year) |> 
  mutate(year = as.integer(year)) |> 
  roworderv()

# Bring data int MIG form
EAC_DOT_MIG <- EAC_DOT |> 
  subset(ref_area != counterpart_area) |> 
  pivot(1:3) |> 
  transform(iso3_o = iif(variable == "TXG_FOB_USD", ref_area, counterpart_area),
            iso3_d = iif(variable == "TMG_CIF_USD", ref_area, counterpart_area)) |> 
  select(iso3_o, iso3_d, year, value) |> 
  collapv(1:3, fsum) |> 
  transform(iso3_o = c(EAC, "ROW")[ckmatch(iso3_o, c(EAC_ISO2, "W00"))],
            iso3_d = c(EAC, "ROW")[ckmatch(iso3_d, c(EAC_ISO2, "W00"))]) |> 
  pivot(c("iso3_d", "year"), names = "iso3_o", how = "w") |> 
  mutate(ROW = ROW - psum(UGA, TZA, KEN, RWA, BDI, SSD, COD, na.rm = TRUE)) |> 
  pivot("year", names = list("iso3_d", "iso3_o"), how = "r") |> 
  mutate(ROW = ROW - psum(UGA, TZA, KEN, RWA, BDI, SSD, COD, na.rm = TRUE)) |> 
  pivot(c("iso3_o", "year"), names = list("iso3_d", "value"), na.rm = TRUE) |> 
  colorder(iso3_o, iso3_d)


EAC_DOT_MIG_AGG <- EAC_DOT_MIG |> 
  subset(between(year, 2010, 2015)) |> 
  collap(value ~ iso3_o + iso3_d) |> 
  mutate(value = value / 1000) |> 
  as_character_factor() |> qDT()

# Ratios: ROW to EAC Trade
# Imports
EAC_DOT_MIG_AGG[iso3_o == "ROW", sum(value)] / EAC_DOT_MIG_AGG[iso3_o %in% EAC & iso3_d %in% EAC, sum(value)]
# Exports
EAC_DOT_MIG_AGG[iso3_d == "ROW", sum(value)] / EAC_DOT_MIG_AGG[iso3_o %in% EAC & iso3_d %in% EAC, sum(value)]
# Total
EAC_DOT_MIG_AGG[iso3_o == "ROW" | iso3_d == "ROW", sum(value)] / EAC_DOT_MIG_AGG[iso3_o %in% EAC & iso3_d %in% EAC, sum(value)]

# Trade Flow Diagram
# library(migest)
# With ROW
migest::mig_chord(EAC_DOT_MIG_AGG) # Billions of Current USD
dev.copy(pdf, "Figures/REV/DOT_MIG_2010_15_ROW.pdf", width = 5, height = 5)
dev.off()

# Without ROW
migest::mig_chord(subset(EAC_DOT_MIG_AGG, iso3_o != "ROW" & iso3_d != "ROW")) # Billions of Current USD
dev.copy(pdf, "Figures/REV/DOT_MIG_2010_15.pdf", width = 5, height = 5)
dev.off()

####################################
# EORA
####################################

EORA <- new.env()
load("Code/EAC_EORA_2021_data.RData", envir = EORA)

EAC_EORA <- EORA$decomps |> get_elem("ESR") |> 
  unlist2d("year", "country_sector") |> 
  group_by(year, iso3_o = substr(country_sector, 1, 3)) |> 
  num_vars() |> fsum(fill = TRUE) |> 
  pivot(1:2, names = list("iso3_d", "value"))

EAC_EORA_MIG <- EAC_EORA |> 
  transform(iso3_o = iif(iso3_o %in% EAC, as.character(iso3_o), "ROW"), 
            iso3_d = iif(iso3_d %in% EAC, as.character(iso3_d), "ROW"),
            year = as.integer(year)) |> 
  collap(value ~ year + iso3_o + iso3_d, fsum) |> 
  subset(iso3_o != iso3_d)

EAC_EORA_MIG_AGG <- EAC_EORA_MIG |> 
  subset(between(year, 2010, 2015)) |> 
  collap(value ~ iso3_o + iso3_d) |> 
  mutate(value = value / 1e6) |> 
  as_character_factor() |> qDT()

# Ratios: ROW to EAC Trade
# Imports
EAC_EORA_MIG_AGG[iso3_o == "ROW", sum(value)] / EAC_EORA_MIG_AGG[iso3_o %in% EAC & iso3_d %in% EAC, sum(value)]
# Exports
EAC_EORA_MIG_AGG[iso3_d == "ROW", sum(value)] / EAC_EORA_MIG_AGG[iso3_o %in% EAC & iso3_d %in% EAC, sum(value)]
# Total
EAC_EORA_MIG_AGG[iso3_o == "ROW" | iso3_d == "ROW", sum(value)] / EAC_EORA_MIG_AGG[iso3_o %in% EAC & iso3_d %in% EAC, sum(value)]

# Trade Flow Diagram
# library(migest)
# With ROW
migest::mig_chord(EAC_EORA_MIG_AGG) # USD Billions at Basic Prices
dev.copy(pdf, "Figures/REV/EORA_MIG_2010_15_ROW.pdf", width = 5, height = 5)
dev.off()

# Without ROW
migest::mig_chord(subset(EAC_EORA_MIG_AGG, iso3_o != "ROW" & iso3_d != "ROW"))
dev.copy(pdf, "Figures/REV/EORA_MIG_2010_15.pdf", width = 5, height = 5)
dev.off()


####################################
# Trade Flow Time Series
####################################

EAC_BACI_MIG |> 
  subset(iso3_o != "ROW" & iso3_d != "ROW") |> 
  ggplot(aes(x = year, y = value, colour = iso3_d)) +
  geom_line() + # geom_vline(xintercept = 2015) +
  facet_wrap(~ iso3_o, scales = "free_y")

EAC_DOT_MIG |> 
  subset(iso3_o != "ROW" & iso3_d != "ROW") |> 
  ggplot(aes(x = year, y = value, colour = iso3_d)) +
  geom_line() + # geom_vline(xintercept = 2015) +
  facet_wrap(~ iso3_o, scales = "free_y")

EAC_EORA_MIG |> 
  subset(iso3_o != "ROW" & iso3_d != "ROW") |> 
  ggplot(aes(x = year, y = value, colour = iso3_d)) +
  geom_line() + geom_vline(xintercept = 2015) +
  facet_wrap(~ iso3_o, scales = "free_y")

# ROW to Inner EAC Trade According to Different Databases

rowbind(BACI = EAC_BACI_MIG |> select(-quantity), 
        DOT = EAC_DOT_MIG, 
        EORA = EAC_EORA_MIG, idcol = "data") |> 
  group_by(data, year, inner_eac = iso3_o %in% EAC & iso3_d %in% EAC) |> 
  num_vars() |> fsum() |> 
  pivot(1:2, names = "inner_eac", how = "w") |> 
  mutate(ratio = `FALSE`/`TRUE`) |> 
  subset(year >= 2000) |> 
  ggplot(aes(x = year, y = ratio, colour = data)) + geom_line() +
    theme_bw() + labs(y = "EAC-ROW Trade / Inner-EAC Trade", 
                      x = "Year", colour = "Database")

ggsave("Figures/REV/ROW_EAC_Trade_Ratios.pdf", width = 8, height = 4)  
  
