library(fastverse)
set_collapse(mask = c("manip", "helper", "special"))
fastverse_extend(ggplot2, scales, RColorBrewer, readxl, qs, install = TRUE)
source("Code/helpers.R")

EAC <- c("UGA", "TZA", "KEN", "RWA", "BDI", "SSD", "COD")
EAC5 <- c("UGA", "TZA", "KEN", "RWA", "BDI")

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
# ots_data = qread("/Users/sebastiankrantz/Documents/IFW Kiel/Africa-Macro-Stability/Data/Trade/ots_data.qs")
# ots_data[]
# tradestatistics::ots_sections # Cannot join to data...
#

# => Julian Hinz says their methodology is not inocuous

####################################
# CEPII BACI
####################################

EM_ISO3 <- read_xlsx("~/Documents/Data/EMERGING/Country_EMERGING.xlsx")$ISO3
BACI_2d <- qread("/Users/sebastiankrantz/Documents/Data/CEPII BACI 2023/BACI_HS96_V202301/BACI_HS96_2d.qs") |> 
           subset(iso3_o %in% EM_ISO3 | iso3_d %in% EM_ISO3) 

# Aggregating: EAC and ROW
EAC_BACI_SEC <- BACI_2d |> 
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

EAC5_BACI_AGG <- EAC_BACI_AGG |> 
  group_by(iso3_o = iif(iso3_o %in% EAC5, iso3_o, "ROW"),
           iso3_d = iif(iso3_d %in% EAC5, iso3_d, "ROW")) |> fsum() |> qDT()

# Ratios: ROW to EAC Trade
# Imports
EAC_BACI_AGG[iso3_o == "ROW", sum(value)] / EAC_BACI_AGG[iso3_o %in% EAC & iso3_d %in% EAC, sum(value)]
EAC5_BACI_AGG[iso3_o == "ROW", sum(value)] / EAC5_BACI_AGG[iso3_o %in% EAC5 & iso3_d %in% EAC5, sum(value)]
# Exports
EAC_BACI_AGG[iso3_d == "ROW", sum(value)] / EAC_BACI_AGG[iso3_o %in% EAC & iso3_d %in% EAC, sum(value)]
EAC5_BACI_AGG[iso3_d == "ROW", sum(value)] / EAC5_BACI_AGG[iso3_o %in% EAC5 & iso3_d %in% EAC5, sum(value)]
# Total
EAC_BACI_AGG[iso3_o == "ROW" | iso3_d == "ROW", sum(value)] / EAC_BACI_AGG[iso3_o %in% EAC & iso3_d %in% EAC, sum(value)]
EAC5_BACI_AGG[iso3_o == "ROW" | iso3_d == "ROW", sum(value)] / EAC5_BACI_AGG[iso3_o %in% EAC5 & iso3_d %in% EAC5, sum(value)]

# Trade Flow Diagram
# With ROW
migest::mig_chord(EAC_BACI_AGG) # Billions of current USD
dev.copy(pdf, "Figures/REV/BACI_MIG_2010_15_ROW.pdf", width = 5, height = 5)
dev.off()
# Without ROW
migest::mig_chord(subset(EAC_BACI_AGG, iso3_o != "ROW" & iso3_d != "ROW")) # Billions of current USD
dev.copy(pdf, "Figures/REV/BACI_MIG_2010_15.pdf", width = 5, height = 5)
dev.off()

# Also See Alluvial Plots: https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html
# library(ggalluvial)

# Now looking at specific sectors
# EAC_BACI_SEC |> gvr("section") |> count() |> View()
# BACI_2d |> select(code_2d, product_description, section_code, section_fullname_english) |> count() |> View()
# broad_sec <- list(AGR = 1:4, MIN = 5, MAN = 6:20)
EAC_BACI_BSEC <- EAC_BACI_SEC |> 
  mutate(broad_sec = nif(section_code %in% 1:2, "AGR", 
                         section_code %in% 3:4, "FBE", 
                         section_code == 5, "MIN", 
                         section_code %in% 6:20, "MAN", 
                         default = "OTH")) |> 
  group_by(year, iso3_o, iso3_d, broad_sec) |> 
  select(value, quantity) |> 
  fsum(fill = TRUE)

sec = "FBE"
EAC_BACI_BSEC |> 
  subset(broad_sec == sec & between(year, 2010, 2015)) |> 
  # subset(iso3_o != "ROW" & iso3_d != "ROW") |> 
  group_by(iso3_o, iso3_d) |> 
  select(value, quantity) |> 
  fmean() |> 
  mutate(value = value / 1e6) |> 
  migest::mig_chord()
  # ggplot(aes(y = value, axis1 = iso3_o, axis2 = iso3_d, fill = iso3_o)) +
  #   ggalluvial::geom_alluvium() # +

dev.copy(pdf, sprintf("Figures/REV/BACI_MIG_%s_2010_15_ROW.pdf", sec), width = 5, height = 5)
dev.off()

####################################
# IMF DOTS
####################################

# library(rdbnomics)
# # rdb_ds = rdb_dimensions("IMF", "DOT", simplify = TRUE)
# # View(unlist2d(rdb_ds))
EAC_ISO2 = c("UG", "TZ", "KE", "RW", "BI", "SS", "CD")
# EAC_DOT = rdb("IMF", "DOT", dimensions = list(REF_AREA = c(EAC_ISO2, "W00"),
#                                               COUNTERPART_AREA = c(EAC_ISO2, "W00"),
#                                               INDICATOR = c("TXG_FOB_USD", "TMG_CIF_USD"), 
#                                               FREQ = "A"))
# EAC_DOT <- EAC_DOT |> janitor::clean_names() |> 
#   pivot(c("ref_area", "counterpart_area", "original_period"), check.dups = TRUE,
#         names = "indicator_2", labels = "indicator", values = "value", how = "w", fill = 0) |> 
#   rename(original_period = year) |> 
#   mutate(year = as.integer(year)) |> 
#   roworderv()
# 
# saveRDS(EAC_DOT, "Data/EAC_IMF_DOT.RData")

EAC_DOT <- readRDS("Data/EAC_IMF_DOT.RData")

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

EAC5_DOT_MIG_AGG <- EAC_DOT_MIG_AGG |> 
  group_by(iso3_o = iif(iso3_o %in% EAC5, iso3_o, "ROW"),
           iso3_d = iif(iso3_d %in% EAC5, iso3_d, "ROW")) |> fsum() |> qDT()

# Ratios: ROW to EAC Trade
# Imports
EAC_DOT_MIG_AGG[iso3_o == "ROW", sum(value)] / EAC_DOT_MIG_AGG[iso3_o %in% EAC & iso3_d %in% EAC, sum(value)]
EAC5_DOT_MIG_AGG[iso3_o == "ROW", sum(value)] / EAC5_DOT_MIG_AGG[iso3_o %in% EAC5 & iso3_d %in% EAC5, sum(value)]
# Exports
EAC_DOT_MIG_AGG[iso3_d == "ROW", sum(value)] / EAC_DOT_MIG_AGG[iso3_o %in% EAC & iso3_d %in% EAC, sum(value)]
EAC5_DOT_MIG_AGG[iso3_d == "ROW", sum(value)] / EAC5_DOT_MIG_AGG[iso3_o %in% EAC5 & iso3_d %in% EAC5, sum(value)]
# Total
EAC_DOT_MIG_AGG[iso3_o == "ROW" | iso3_d == "ROW", sum(value)] / EAC_DOT_MIG_AGG[iso3_o %in% EAC & iso3_d %in% EAC, sum(value)]
EAC5_DOT_MIG_AGG[iso3_o == "ROW" | iso3_d == "ROW", sum(value)] / EAC5_DOT_MIG_AGG[iso3_o %in% EAC5 & iso3_d %in% EAC5, sum(value)]

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
load("Data/EAC_EORA_2021_data_broad_sec.RData", envir = EORA)

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

EAC5_EORA_MIG_AGG <- EAC_EORA_MIG_AGG |> 
  group_by(iso3_o = iif(iso3_o %in% EAC5, iso3_o, "ROW"),
           iso3_d = iif(iso3_d %in% EAC5, iso3_d, "ROW")) |> fsum() |> qDT()

# Ratios: ROW to EAC Trade
# Imports
EAC_EORA_MIG_AGG[iso3_o == "ROW", sum(value)] / EAC_EORA_MIG_AGG[iso3_o %in% EAC & iso3_d %in% EAC, sum(value)]
EAC5_EORA_MIG_AGG[iso3_o == "ROW", sum(value)] / EAC5_EORA_MIG_AGG[iso3_o %in% EAC5 & iso3_d %in% EAC5, sum(value)]
# Exports
EAC_EORA_MIG_AGG[iso3_d == "ROW", sum(value)] / EAC_EORA_MIG_AGG[iso3_o %in% EAC & iso3_d %in% EAC, sum(value)]
EAC5_EORA_MIG_AGG[iso3_d == "ROW", sum(value)] / EAC5_EORA_MIG_AGG[iso3_o %in% EAC5 & iso3_d %in% EAC5, sum(value)]
# Total
EAC_EORA_MIG_AGG[iso3_o == "ROW" | iso3_d == "ROW", sum(value)] / EAC_EORA_MIG_AGG[iso3_o %in% EAC & iso3_d %in% EAC, sum(value)]
EAC5_EORA_MIG_AGG[iso3_o == "ROW" | iso3_d == "ROW", sum(value)] / EAC5_EORA_MIG_AGG[iso3_o %in% EAC5 & iso3_d %in% EAC5, sum(value)]

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

# Now looking at specific sectors
EAC_EORA_BSEC <- EORA$decomps |> get_elem("ESR") |> 
  unlist2d("year", "country_sector") |> 
  transform(iso3_o = substr(country_sector, 1, 3),
            sector = substr(country_sector, 5, 7),
            country_sector = NULL) |> 
  pivot(c("year", "iso3_o", "sector"), names = list("iso3_d", "value")) |> # with(unique(sector))
  transform(iso3_o = iif(iso3_o %in% EAC, as.character(iso3_o), "ROW"), 
            iso3_d = iif(iso3_d %in% EAC, as.character(iso3_d), "ROW"),
            year = as.integer(year), 
            broad_sec = nif(sector == "AFF", "AGR", # %in% c("AGR", "FIS")
                            sector == "MIN", "MIN", 
                            sector == "FBE", "FBE",
                            sector %in% c("TEX", "WAP", "PCM", "MPR", "ELM", "TEQ", "MAN"), "MAN", 
                            default = "SRV")) |> 
  collap(value ~ year + iso3_o + iso3_d + broad_sec, fsum) |> 
  subset(iso3_o != iso3_d)
  
for (sec in c("AGR", "FBE", "MIN", "MAN", "SRV")) {
EAC_EORA_BSEC |> 
  subset(broad_sec == sec & between(year, 2010, 2015)) |> 
  # subset(iso3_o != "ROW" & iso3_d != "ROW") |> 
  group_by(iso3_o, iso3_d) |> 
  select(value) |> 
  fmean() |> 
  mutate(value = value / 1e6) |> 
  migest::mig_chord()

dev.copy(pdf, sprintf("Figures/REV/EORA_MIG_%s_2010_15_ROW.pdf", sec), width = 5, height = 5)
dev.off()
}

####################################
# EMERGING: Same Code as EORA
####################################

EM <- new.env()
load("Data/EAC_EMERGING_data_broad_sec.RData", envir = EM)

EAC_EM <- EM$decomps |> get_elem("ESR") |> 
  unlist2d("year", "country_sector") |> 
  group_by(year, iso3_o = substr(country_sector, 1, 3)) |> 
  num_vars() |> fsum(fill = TRUE) |> 
  pivot(1:2, names = list("iso3_d", "value"))

EAC_EM_MIG <- EAC_EM |> 
  transform(iso3_o = iif(iso3_o %in% EAC, as.character(iso3_o), "ROW"), 
            iso3_d = iif(iso3_d %in% EAC, as.character(iso3_d), "ROW"),
            year = as.integer(year)) |> 
  collap(value ~ year + iso3_o + iso3_d, fsum) |> 
  subset(iso3_o != iso3_d)

EAC_EM_MIG_AGG <- EAC_EM_MIG |> 
  subset(between(year, 2010, 2015)) |> 
  collap(value ~ iso3_o + iso3_d) |> 
  mutate(value = value / 1e3) |> 
  as_character_factor() |> qDT()

EAC5_EM_MIG_AGG <- EAC_EM_MIG_AGG |> 
  group_by(iso3_o = iif(iso3_o %in% EAC5, iso3_o, "ROW"),
           iso3_d = iif(iso3_d %in% EAC5, iso3_d, "ROW")) |> fsum() |> qDT()

# Ratios: ROW to EAC Trade
# Imports
EAC_EM_MIG_AGG[iso3_o == "ROW", sum(value)] / EAC_EM_MIG_AGG[iso3_o %in% EAC & iso3_d %in% EAC, sum(value)]
EAC5_EM_MIG_AGG[iso3_o == "ROW", sum(value)] / EAC5_EM_MIG_AGG[iso3_o %in% EAC5 & iso3_d %in% EAC5, sum(value)]
# Exports
EAC_EM_MIG_AGG[iso3_d == "ROW", sum(value)] / EAC_EM_MIG_AGG[iso3_o %in% EAC & iso3_d %in% EAC, sum(value)]
EAC5_EM_MIG_AGG[iso3_d == "ROW", sum(value)] / EAC5_EM_MIG_AGG[iso3_o %in% EAC5 & iso3_d %in% EAC5, sum(value)]
# Total
EAC_EM_MIG_AGG[iso3_o == "ROW" | iso3_d == "ROW", sum(value)] / EAC_EM_MIG_AGG[iso3_o %in% EAC & iso3_d %in% EAC, sum(value)]
EAC5_EM_MIG_AGG[iso3_o == "ROW" | iso3_d == "ROW", sum(value)] / EAC5_EM_MIG_AGG[iso3_o %in% EAC5 & iso3_d %in% EAC5, sum(value)]

# Trade Flow Diagram
# library(migest)
# With ROW
migest::mig_chord(EAC_EM_MIG_AGG) # USD Billions at Basic Prices
dev.copy(pdf, "Figures/REV/EM_MIG_2010_15_ROW.pdf", width = 5, height = 5)
dev.off()

# Without ROW
migest::mig_chord(subset(EAC_EM_MIG_AGG, iso3_o != "ROW" & iso3_d != "ROW"))
dev.copy(pdf, "Figures/REV/EM_MIG_2010_15.pdf", width = 5, height = 5)
dev.off()

# Now looking at specific sectors
EAC_EM_BSEC <- EM$decomps |> get_elem("ESR") |> 
  unlist2d("year", "country_sector") |> 
  transform(iso3_o = substr(country_sector, 1, 3),
            sector = substr(country_sector, 5, 7),
            country_sector = NULL) |> 
  pivot(c("year", "iso3_o", "sector"), names = list("iso3_d", "value")) |> # with(unique(sector))
  transform(iso3_o = iif(iso3_o %in% EAC, as.character(iso3_o), "ROW"), 
            iso3_d = iif(iso3_d %in% EAC, as.character(iso3_d), "ROW"),
            year = as.integer(year), 
            broad_sec = nif(sector == "AFF", "AGR", # %in% c("AGR", "FIS")
                            sector == "MIN", "MIN", 
                            sector == "FBE", "FBE",
                            sector %in% c("TEX", "WAP", "PCM", "MPR", "ELM", "TEQ", "MAN"), "MAN", 
                            default = "SRV")) |> 
  collap(value ~ year + iso3_o + iso3_d + broad_sec, fsum) |> 
  subset(iso3_o != iso3_d)

for (sec in c("AGR", "FBE", "MIN", "MAN", "SRV")) {
  EAC_EM_BSEC |> 
    subset(broad_sec == sec & between(year, 2010, 2015)) |> 
    subset(iso3_o != "ROW" & iso3_d != "ROW") |> 
    group_by(iso3_o, iso3_d) |> 
    select(value) |> 
    fmean() |> 
    mutate(value = value / 1e3) |> 
    migest::mig_chord()
  
  dev.copy(pdf, sprintf("Figures/REV/EM_MIG_%s_2010_15.pdf", sec), width = 5, height = 5)
  dev.off()
}


####################################
# Trade Flow Time Series
####################################

EAC_BACI_MIG |> 
  subset(iso3_o != "ROW" & iso3_d != "ROW") |> 
  mutate(value = BY(value, list(iso3_o, iso3_d), frollmean, 5)) |> 
  ggplot(aes(x = year, y = value, colour = iso3_d)) +
  geom_line() + # geom_vline(xintercept = 2015) +
  facet_wrap(~ iso3_o, scales = "free_y")

EAC_DOT_MIG |> 
  subset(iso3_o != "ROW" & iso3_d != "ROW") |> 
  mutate(value = BY(value, list(iso3_o, iso3_d), frollmean, 5)) |> 
  ggplot(aes(x = year, y = value, colour = iso3_d)) +
  geom_line() + # geom_vline(xintercept = 2015) +
  facet_wrap(~ iso3_o, scales = "free_y")

EAC_EORA_MIG |> 
  subset(iso3_o != "ROW" & iso3_d != "ROW") |> 
  mutate(value = BY(value, list(iso3_o, iso3_d), frollmean, 5)) |> 
  ggplot(aes(x = year, y = value, colour = iso3_d)) +
  geom_line() + geom_vline(xintercept = 2015) +
  facet_wrap(~ iso3_o, scales = "free_y")

EAC_EM_MIG |> 
  subset(iso3_o != "ROW" & iso3_d != "ROW") |> 
  ggplot(aes(x = year, y = value, colour = iso3_d)) +
  geom_line() + geom_vline(xintercept = 2015) +
  facet_wrap(~ iso3_o, scales = "free_y")

# ROW to Inner EAC Trade According to Different Databases

# Total Trade
rowbind(BACI = EAC_BACI_MIG |> select(-quantity), 
        DOTS = EAC_DOT_MIG, 
        EORA = EAC_EORA_MIG,
        # EMERGING = EAC_EM_MIG, 
        idcol = "data") |> 
  subset(iso3_o %in% EAC5 | iso3_d %in% EAC5) |> 
  group_by(data, year, inner_eac = iso3_o %in% EAC5 & iso3_d %in% EAC5) |> 
  num_vars() |> fsum() |> 
  pivot(1:2, names = "inner_eac", how = "w") |> 
  group_by(data) |> 
  mutate(ratio = `FALSE`/`TRUE`,
         ratio_ma = frollmean(ratio, 5, na.rm = TRUE), 
         ratio_ma_post = frollmean(`FALSE`, 5, na.rm = TRUE)/frollmean(`TRUE`, 5, na.rm = TRUE)) |> 
  ungroup() |> 
  subset(year >= 2000 & year <= 2021) |> 
  ggplot(aes(x = year, y = ratio_ma, colour = data)) + 
    geom_line() +
    # geom_smooth(se = FALSE, linewidth = 0.5, linetype = 2) +
    # scale_y_continuous(limits = c(10, 30)) + 
    theme_bw() + labs(y = "EAC-ROW/Inner-EAC Trade, 5 Year MA", 
                      x = "Year", colour = "Database")

ggsave("Figures/REV/ROW_EAC5_Trade_Ratios_5YMA.pdf", width = 8, height = 4)  
  

# Exports and Imports
EAC5_TRADE_Share <- rowbind(
  DOTS = EAC_DOT_MIG,
  BACI = EAC_BACI_MIG |> select(-quantity),
  # EORA = EAC_EORA_MIG,
  idcol = "source") %>% 
  subset(iso3_o %in% EAC5 | iso3_d %in% EAC5) %>% {
    rowbind(
      `Export Destination` = rowbind(
        mutate(., value = fsum(value, list(source, iso3_o, year), TRA = "/")) |>
          subset(iso3_o %in% EAC5 & iso3_d %in% EAC5, -iso3_d) |> 
          group_by(source, country = iso3_o, year) |> select(value) |> fsum(),
        group_by(., source, iso3_o = iso3_o %in% EAC5, iso3_d = iso3_d %in% EAC5, year) |> fsum() |> 
          mutate(value = fsum(value, list(source, iso3_o, year), TRA = "/")) |> 
          subset(iso3_o & iso3_d, -iso3_o, -iso3_d) |> 
          mutate(country = "EAC")
      ),
      `Import Origin` = rowbind(
        mutate(., value = fsum(value, list(source, iso3_d, year), TRA = "/")) |>
          subset(iso3_o %in% EAC5 & iso3_d %in% EAC5, -iso3_o) |> 
          group_by(source, country = iso3_d, year) |> select(value) |> fsum(),
        group_by(., source, iso3_o = iso3_o %in% EAC5, iso3_d = iso3_d %in% EAC5, year) |> fsum() |> 
          mutate(value = fsum(value, list(source, iso3_d, year), TRA = "/")) |> 
          subset(iso3_o & iso3_d, -iso3_o, -iso3_d) |> 
          mutate(country = "EAC")
      ),
      idcol = "share"
    )}

# Disaggregated Plot
EAC5_TRADE_Share |> 
  roworderv() |> 
  mutate(value = BY(value, list(share, source, country), frollmean, 5, na.rm = TRUE),
         country = factor(country, levels = c(EAC5, "EAC"))) |> 
  subset(year >= 2000) |> 
  ggplot(aes(x = year, y = value, color = country, linetype = source)) +
  geom_line() + 
  facet_wrap(~ share, scales = "fixed") + 
  scale_y_continuous(labels = percent, limits = c(0, NA),
                     breaks = extended_breaks(7)) + 
  scale_x_continuous(n.breaks = 6) +
  guides(color = guide_legend(title = "Country:  ", nrow = 1)) +
  scale_color_manual(values = c(brewer.pal(5, "Dark2"), "black")) +
  labs(x = NULL, y = "EAC Share, 5-Year MA", linetype = " Source:  ") +
  theme_bw() + pretty_plot 

ggsave("Figures/REV/GT_EAC5_shares_ts.pdf", width = 8.5, height = 4)


# ROW to Inner EAC Trade According to Different Databases: Sector Level

rowbind(BACI = EAC_BACI_BSEC |> select(-quantity), 
        EORA = EAC_EORA_BSEC,
        EMERGING = EAC_EM_BSEC, idcol = "data") |> 
  subset(iso3_o %in% EAC5 | iso3_d %in% EAC5) |> 
  group_by(data, broad_sec, year, 
           inner_eac = iso3_o %in% EAC5 & iso3_d %in% EAC5) |> 
  num_vars() |> fsum() |>
  pivot(1:3, names = "inner_eac", how = "w") |> 
  mutate(ratio = replace_outliers(`FALSE`/`TRUE`, 30, NA, "max")) |> # View()
  extract(data != "EMERGING", ratio := frollmean(ratio, 5, na.rm = TRUE), by = .(data, broad_sec)) |> 
  subset(year >= 2000 & year <= 2021 & broad_sec %in% c("AGR", "FBE", "MAN")) |> # "MIN"
  ggplot(aes(x = year, y = ratio, colour = data)) + 
    geom_line() + 
    # geom_smooth(se = FALSE, linewidth = 0.5, linetype = 2) +
    facet_wrap(~broad_sec, scales = "fixed", nrow = 1) +
    scale_colour_brewer(palette = "Paired", direction = -1) +
    scale_y_continuous(n.breaks = 10) +
    theme_bw() + labs(y = "EAC-ROW/Inner-EAC Trade, 5 Year MA", 
                      x = "Year", colour = "Database:    ") +
    theme(legend.position = "top")

ggsave("Figures/REV/ROW_EAC_Trade_Ratios_Sec_5YMA.pdf", width = 8, height = 4)  


# Separately by Country: Only BACI
EAC_BACI_BSEC_SH <- EAC_BACI_BSEC |> 
  subset(iso3_o %in% EAC5 | iso3_d %in% EAC5) |> 
  mutate(inner_eac = iso3_o %in% EAC5 & iso3_d %in% EAC5) %>% { 
    join(
      group_by(., broad_sec, year, inner_eac, country = iso3_o) |> 
        summarise(exports = fsum(value)) %>%
        rowbind(subset(., country %in% EAC5) |> 
                collap(exports ~ broad_sec + year + inner_eac, fsum) |> 
                mutate(country = "EAC")),      
      group_by(., broad_sec, year, inner_eac, country = iso3_d) |> 
        summarise(imports = fsum(value)) %>% 
        rowbind(subset(., country %in% EAC5) |> 
                collap(imports ~ broad_sec + year + inner_eac, fsum) |> 
                mutate(country = "EAC")),
      how = "full") 
  } |> 
  # subset(GRPN(list(data, broad_sec, year, country)) == 2L) |> 
  mutate(total = psum(exports, imports, na.rm = TRUE),
         across(c(exports, imports, total), list(share = fsum), 
                list(broad_sec, year, country), TRA = "/", .names = TRUE)) |> 
  subset(inner_eac & broad_sec %in% c("AGR", "AFF", "FBE", "MAN"), -inner_eac) |> 
  roworder(broad_sec, country) |> 
  mutate(across(c(exports_share, imports_share, total_share), list(ma = BY), 
                list(broad_sec, country), frollmean, 5, na.rm = TRUE, .names = TRUE)) |> 
  pivot(c("broad_sec", "year", "country"), na.rm = TRUE) 

# Plot
EAC_BACI_BSEC_SH |> 
  subset(year >= 2000 & year <= 2021 & country %in% c(EAC5, "EAC") & 
           variable %in% c("exports_share_ma", "imports_share_ma")) |> 
  mutate(variable = set_attr(variable, "levels", recode_char(levels(variable), 
         exports_share_ma = "Exports", imports_share_ma = "Imports")), 
         country = factor(country, levels = c(EAC5, "EAC"))) |> 
  
  ggplot(aes(x = year, y = value, colour = country)) + 
  geom_line() + 
  facet_grid(variable ~ broad_sec, scales = "free_y") +
  guides(color = guide_legend(title = "Country:  ", nrow = 1)) +
  scale_color_manual(values = c(brewer.pal(5, "Dark2"), "black")) +
  scale_y_continuous(n.breaks = 10) +
  theme_bw() + labs(y = "EAC Trade Share, 5 Year MA", 
                    x = "Year", colour = "Country:    ") +
  theme(legend.position = "top")
  
ggsave("Figures/REV/GT_EAC5_shares_sec_ts.pdf", width = 8.5, height = 5)

# Examine: 
EAC_BACI_BSEC_SH |> 
  subset((year == 2000 | year == 2020) & country %in% c(EAC5, "EAC") & 
           variable %in% c("exports_share_ma", "imports_share_ma")) |> 
  pivot(c("broad_sec", "variable"), names = c("country", "year"), how = "w") |> 
  roworderv()

# Stats:
EAC_BACI_BSEC_SH |> 
  subset(year >= 2010 & year <= 2020 & country %in% c(EAC5, "EAC") & 
           variable %in% c("exports_share", "imports_share")) |> View()
  collap(value ~ broad_sec + country + variable) |> 
  pivot(c("broad_sec", "variable"), names = "country", how = "w")
