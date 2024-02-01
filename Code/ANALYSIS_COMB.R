##########################################
# GVC Analysis for EAC Countries Using
# EORA 2021 and EMERGING Databases and
# GVC Indicators from STATA's ICIO Module
##########################################

# Load Libraries and Functions -----------------------
library(fastverse)
set_collapse(mask = c("manip", "helper", "special"), nthreads = 4, sort = TRUE)
fastverse_extend(ggplot2, ggh4x, scales, RColorBrewer, readxl, pheatmap, qs, decompr, install = TRUE)
source("Code/helpers.R")

# Load GVC Data ----------------------------------------------------
EAC <- c("UGA", "TZA", "KEN", "RWA", "BDI", "SSD", "COD")
EAC6 <- c("UGA", "TZA", "KEN", "RWA", "BDI", "COD")
EAC5 <- c("UGA", "TZA", "KEN", "RWA", "BDI")
ROW <- c("SSA", "MEA", "EUU", "ECA", "NAC", "SAS", "ASE", "CHN", "ROA", "LAC", "OCE")
REG <- c("UGA", "TZA", "KEN", "RWA", "BDI", "COD", ROW) #, "SSD")
MAN <- c("FBE", "TEX", "WAP", "PCM", "MPR", "ELM", "TEQ", "MAN")


trade_class <- read_xlsx("/Users/sebastiankrantz/Documents/Data/EORA/trade classification.xlsx", range = "A1:B220")
sec_class <- read_xlsx("/Users/sebastiankrantz/Documents/Data/EORA/trade classification.xlsx", sheet = "Sectors") |> 
             mutate(id = seq_along(code)) |> rename(tolower)
SEC <- unique(sec_class$broad_sector_code)

WDR_POS <- fread("/Users/sebastiankrantz/Documents/Data/WDR2020GVCdata/WDR2020_gvc_data.csv") |> 
  relabel(cntry = "Country name",
          sect = "Sector number",
          t = "Year",
          source = "ICIO table",
          gexp = "Gross exports",
          dc = "Domestic content",
          dva = "Domestic Value-Added", 
          vax = "Value-Added absorbed in foreign countries",
          ref = "Reflection",
          ddc = "Domestic double-counting",
          fc = "Foreign content",
          fva = "Foreign Value-Added",
          fdc = "Foreign double-counting",
          gvc = "GVC exports (gvcb+gvcf)",
          gvcb = "GVC backward participation",
          gvcf = "GVC forward participation",
          sect_name = "Sector name") |> 
  join(haven::read_dta("/Users/sebastiankrantz/Documents/Data/GVCPosition/position_full.dta") |> 
       select(country, sect, t, source, upstreamness, downstreamness),
       on = c("cntry" = "country", "sect", "t", "source")) |> 
  rename(t = year) |> 
  mutate(cntry = recode_char(cntry, SUD = "SDN", SDS = "SSD", ANT = "ATG")) |> 
  join(trade_class, on = c("cntry" = "iso3c")) |> 
  subset(!is.na(trade) & source == "eora", -source, -cntry) |> 
  group_by(year, trade, sect) |> 
  summarise(across(gexp:gvcf, fsum), 
            sect_name = fmode(sect_name), 
            across(c(upstreamness, downstreamness), fmean, w = gexp)) |> 
  # collap(~ year + trade + sect, fsum) |> 
  transform(source = qF("WDR_EORA"), 
            trade = factor(trade, levels = REG)) |> 
  colorder(source, year, country = trade, sect, sect_name)

WDR_POS_SEC <- WDR_POS |> 
  join(sec_class |> select(id, broad_sector_code), 
       on = c("sect" = "id")) |> 
  group_by(source, year, country, sector = broad_sector_code) |> 
  summarise(across(gexp:gvcf, fsum), 
            across(c(upstreamness, downstreamness), fmean, w = gexp))

WDR_POS_AGG <- WDR_POS |> group_by(source, year, country) |> 
  summarise(across(gexp:gvcf, fsum), 
            across(c(upstreamness, downstreamness), fmean, w = gexp))


EM_SEC <- read_xlsx("~/Documents/Data/EMERGING/Sector_EMERGING.xlsx") |> mutate(id = Code) |> rename(tolower)
EM_CTRY <- read_xlsx("~/Documents/Data/EMERGING/Country_EMERGING.xlsx") |> 
           transform(Detailed_Region_Code = iif(ISO3 %in% EAC6, ISO3, Detailed_Region_Code),
                     Detailed_Region = nif(ISO3 %in% EAC6, "East African Community", 
                                           Detailed_Region_Code == "SSA", "Sub-Saharan Africa (Excluding EAC)", 
                                           default = Detailed_Region)) |> 
           rename(tolower) 

SEC_ALL <- list(EORA = sec_class, EMERGING = EM_SEC) |> 
  lapply(select, id, broad_sector_code) |> 
  rowbind(idcol = "source") |> 
  mutate(id = as.integer(id))

BIL_SEC <- rowbind(EMERGING = fread("/Users/sebastiankrantz/Documents/Data/EMERGING/GVC_Regions/EM_GVC_BIL_SEC_BM19.csv"),
                   EORA = fread("/Users/sebastiankrantz/Documents/Data/EORA/GVC_Regions/EORA_GVC_BIL_SEC_BM19.csv") |> 
                          transformv(is.double, `*`, 1/1000), 
                   idcol = "source") |> 
           # Aggregating to Broad Sectors
           join(SEC_ALL |> rename(broad_sector_code = from_sector), 
                on = c("source", "from_sector" = "id"), drop = "x") |> 
           group_by(source, year, from_region, from_sector, to_region) |> 
           fsum()

BIL_AGG <- BIL_SEC |> group_by(source, year, from_region, to_region) |> select(-from_sector) |> fsum()

AGG <- BIL_AGG |> group_by(source, year, country = from_region) |> select(-from_region, -to_region) |> fsum()

# These are computed from ICIO using 
REG_SEC <- rowbind(EMERGING = fread("/Users/sebastiankrantz/Documents/Data/EMERGING/GVC_Countries_Agg_Sectors/EM_GVC_SEC_BM19.csv") |> 
                      rm_stub("from_") |> 
                      mutate(sector = structure(sector, levels = c("AFF", "FBE", "MAN", "MIN", "SRV"), class = "factor"), 
                             sector = factor(sector, levels = c("AFF", "MIN", "FBE", "MAN", "SRV"))),
                    EORA = fread("/Users/sebastiankrantz/Documents/Data/EORA/GVC_Countries_Agg_Sectors/EORA_GVC_SEC_BM19.csv") |> 
                      transformv(is.double, `*`, 1/1000) |> rm_stub("from_") |> 
                      mutate(sector = structure(sector, levels = c("AFF", "MIN", "FBE", "MAN", "SRV"), class = "factor")), 
                    idcol = "source") |> 
            join(select(EM_CTRY, iso3, region = detailed_region_code), on = c("region" = "iso3"), drop = "x") |> 
            group_by(source, year, region, sector) |> fsum()

REG_AGG <- REG_SEC |> group_by(source, year, region) |> num_vars() |> fsum()

# Load Raw Decomposition Data -------------------------------------------

# # EAC + 11 World Regions, and 17 broad sectors
# EORA <- new.env()
# load("Data/EAC_EORA_2021_data_broad_sec.RData", envir = EORA)
# 
# EM <- new.env()
# load("Data/EAC_EMERGING_data_broad_sec.RData", envir = EM)
# EM$y <- colnames(EM$out_ag)

# 5 broad sectors with full country resolution: for Forward GVC Participation
EM <- new.env()
load("Data/EAC_EMERGING_data_Countries_Agg_Sectors.RData", envir = EM)

EORA <- new.env()
load("Data/EAC_EORA_data_Countries_Agg_Sectors.RData", envir = EORA)

# EAC + 11 World Regions, but with full sector resolution
EORA_DET <- new.env()
load("Data/EAC_EORA_2021_data.RData", envir = EORA_DET)

EM_DET <- new.env()
load("Data/EAC_EMERGING_data.RData", envir = EM_DET)

# Basic Comparison ------------------------------------------------------

BIL_SEC[source == "EORA" & between(year, 2000, 2015)] |> group_by(source, from_region, year) |> select(gexp, gvc) |> fsum() 
WDR_POS[between(year, 2000, 2015)] |> group_by(source, country, year) |> select(gexp, gvc) |> fsum() 
KWW[source == "EORA" & between(year, 2000, 2015)] |> roworder(source, country, year)

rowbind(WDR_POS[country %in% EAC6, lapply(.SD, sum), by = .(source, country, year), 
                .SDcols = .c(gexp, gvc, gvcb, gvcf)],
        BIL_SEC[from_region %in% EAC6, lapply(.SD, sum), by = .(source, country = from_region, year), 
                .SDcols = .c(gexp, gvc, gvcb, gvcf)]) |> 
  transformv(c(gvc, gvcb, gvcf), `/`, gexp) |> 
  # rowbind(VS_df_ag %>% slt(-i2e) %>% 
  #  av(VS1_df_ag %>% slt(E2R)) %>%
  #  sbt(Country %in% EAC6) %>%
  #  tfm(source = "OLD_EORA") %>%
  #  rnm(Country = country, Year = year, I2E = gvcb, E2R = gvcf), fill = TRUE) |> 
  
  ggplot(aes(x = year, y = gvcb, colour = source, linetype = source)) +
    geom_line() +
    facet_wrap(~country) + 
    theme_bw() + pretty_plot + rbsc2

# Note: with higher level of sectoral aggregation, GVC indicators are different!


#############################
# Gross Flows
#############################

# EM_Raw <- qread("~/Documents/Data/EMERGING/EMERGING_EAC_Regions.qs")
EM_Agg <- qread("~/Documents/Data/EMERGING/EMERGING_EAC_Regions_Broad_Sectors.qs")

# Heatmaps of Aggregated Gross Flows

EM_T_ag_15_19 <- EM_Agg |> get_elem("T") |> extract(as.character(2015:2019)) |> pmean() |> mat_agg() |> extract(REG, REG)
log10(EM_T_ag_15_19) |> 
  pheatmap(color = colorRampPalette(brewer.pal(n = 7, name ="YlOrRd"))(100),
           cluster_rows = FALSE, cluster_cols = FALSE, border_color = NA,
           legend_breaks = 0:7, # legend_labels = 10^(0:7),
           display_numbers = TRUE)

dev.copy(pdf, "Figures/REV/EM_heatmap_2015_19_AG.pdf", width = 7, height = 6)
dev.off()

# Share of VA
(diag(EM_T_ag_15_19) / colSums(`diag<-`(EM_T_ag_15_19, 0)))[EAC6] |> mean()
# Including regional trade
sum(EM_T_ag_15_19[EAC6, EAC6]) / sum(EM_T_ag_15_19[ROW, EAC6])
sum(EM_T_ag_15_19[EAC6, EAC6]) / sum(EM_T_ag_15_19[EAC6, ROW])

# Total Intermediates Trade Ratio
(sum(EM_T_ag_15_19[ROW, EAC6]) + sum(EM_T_ag_15_19[EAC6, ROW])) / sum(`diag<-`(EM_T_ag_15_19[EAC6, EAC6], 0))
# Input Ratio
sum(EM_T_ag_15_19[ROW, EAC6]) / sum(`diag<-`(EM_T_ag_15_19[EAC6, EAC6], 0))
# Output Ratio
sum(EM_T_ag_15_19[EAC6, ROW]) / sum(`diag<-`(EM_T_ag_15_19[EAC6, EAC6], 0))


# Summary of Gross Flows
EM_FD_ag_15_19 <- EM_Agg |> get_elem("FD") |> extract(as.character(2015:2019)) |> pmean() |> mat_agg() |> extract(REG, REG)
E_shares <- get_E_shares(EM_T_ag_15_19, EM_FD_ag_15_19) 

# Aggregate
E_shares$Aggregate |> 
  qDF("country") |> 
  pivot("country") |> 
  subset(country %in% EAC6) |> 
  mutate(country = factor(country, levels = EAC6)) |> 
  ggplot(aes(x = country, y = value)) +
    facet_wrap( ~ variable, scales = "free_y") +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = percent) +
    labs(y = "Percent", x = "EAC Member") +
    theme_bw() + pretty_plot

ggsave("Figures/REV/EM_gross_shares_ag.pdf", width = 10, height = 4)

# Export / Import Shares
E_shares$Shares |> 
  unlist2d("Variable", "Country") |> 
  pivot(1:2, names = list("Flow", "Value")) |> 
  subset(Country %in% EAC6) |> 
  mutate(Country = factor(Country, levels = EAC6)) |> 
  ggplot(aes(x = Country, y = Value, fill = Flow)) +
  facet_wrap( ~ Variable, scales = "free_y") +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  scale_y_continuous(labels = percent) +
  labs(y = "EAC6 Share", x = "EAC Member") +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() + pretty_plot + theme(legend.position = "right")

ggsave("Figures/REV/EM_gross_trade_shares_ag.pdf", width = 10, height = 4)

# Largest Sector-Level intermediate flows. 
EM_T_DT_Agg <- EM_Agg$DATA[as.character(2015:2019)] |> 
  get_elem("T") |> pmean() |> qDT("from") |> 
  transform(set_names(tstrsplit(from, ".", fixed = TRUE), c("from_region", "from_sector"))) |> 
  pivot(c("from", "from_region", "from_sector"), names = list("to", "value")) |> 
  transform(set_names(tstrsplit(to, ".", fixed = TRUE), c("to_region", "to_sector"))) |> 
  colorder(value, pos = "end") |> 
  subset(from_region != to_region & (from_region %in% EAC6 | to_region %in% EAC6))
  
# Largest 50 Flows: Broad Sectors
add_vars(
  EM_T_DT_Agg |> 
    subset(from_region != "COD" & to_region != "COD") |> 
    select(from, to, value) |> 
    roworder(-value) |> 
    head(50),
  EM_T_DT_Agg |> 
    subset(from_region %in% EAC5 & to_region %in% EAC5) |> 
    select(from, to, value) |> 
    roworder(-value) |> 
    head(50)
) |> 
  xtable::xtable() |> print(booktabs = TRUE)
  
  
#############################
# Backward GVC Participation
#############################

# Backward: Overall Time Series
WDR_POS_AGG |> 
  rowbind(AGG[!(year > 2015 & country == "BDI" & source == "EORA")], fill = TRUE) |> 
  mutate(source = factor(source, levels = c("EMERGING", "EORA", "WDR_EORA"))) |> 
  subset(country %in% EAC6 & year >= 2000) |> 
  transformv(c(gvc, gvcb, gvcf), `/`, gexp) |> 
  ggplot(aes(x = year, y = gvcb, colour = source, linetype = source)) +
  geom_line() +
  scale_y_continuous(labels = percent) +
  facet_wrap(~country) + 
  labs(y = "Backward GVC Participation (VS)", x = NULL, 
       colour = "Source:   ", linetype = "Source:   ") +
  theme_bw() + pretty_plot + rbsc2

ggsave("Figures/REV/VA_shares_ag_ts.pdf", width = 10, height = 5)

# Decomposition by Source
vs_agg_share <- AGG[source == "EMERGING" & year >= 2015, .(VSS = mean(gvcb/gexp)), by = country] |> 
  with(set_names(VSS, country))

VS_EAC_BIL <- list(EORA = EORA_DET, EMERGING = EM_DET) |> 
  lapply(function(x) {
    sapply(x$y, function(i) mat_agg(leontief(x$decomps[[i]], long = FALSE))[, EAC6], # fsum(x$VB_ag[, EAC6, i], TRA = "/"), 
           simplify = FALSE) |> 
    unlist2d("year", "share", DT = TRUE) |> 
    mutate(year = as.integer(year)) |> 
    pivot(1:2, names = list("country", "value")) 
  }) |> rowbind(idcol = "source") 
  
VS_EAC_BIL |> 
  subset((between(year, 2010, 2015) & source == "EORA") | (year >= 2015 & source == "EMERGING")) |> 
  mutate(share = factor(setv(share, "SSD", "SSA"), levels = REG),
         value = fsum(value, list(source, year, country), TRA = "/")) |> 
  group_by(source, share, country) |> 
  select(value) |> fmean() |> 
  subset(as.character(country) != as.character(share)) |> 
  # # mutate(value = fsum(value, list(source, country), TRA = "/")) |> 
  # subset(share %!in% EAC6 & source == "EMERGING") |> 
  # roworder(-value)
  mutate(country = set_attr(country, "levels", paste0(levels(country), " (", round(vs_agg_share[levels(country)]*100,1), "%)"))) |> 
  ggplot(aes(x = source, y = value, fill = share)) +
    geom_bar(stat = "identity", position = "fill", alpha = 0.8) +
    facet_wrap( ~ country, nrow = 1) + # , scales = "free_y" 
    guides(fill = guide_legend(ncol = 1)) + 
    scale_y_continuous(labels = percent, 
                       breaks = extended_breaks(10), expand = c(0,0), limits = c(0, 1)) +  
    labs(x = "Database", y = "Share of Foreign Exports Content", fill = "Source") +
    theme_bw() + pretty_plot + rbsc2 + theme(legend.position = "right")

ggsave("Figures/REV/VA_shares_ctry.pdf", width = 12, height = 5)

# Sector-Level decomposition
VS_shares_sec <- BIL_SEC |> 
  subset(from_region %in% EAC6) |> 
  group_by(source, year, country = from_region, sector = from_sector) |> 
  select(gvcb, gexp) |> fsum() |> 
  mutate(value = gvcb / gexp) |> 
  subset((between(year, 2010, 2015) & source == "EORA") | (year >= 2015 & source == "EMERGING")) |> 
  group_by(source, country, sector) |> 
  select(value) |> fmean() |> 
  transform(country = factor(country, levels = EAC6),
            sector = factor(sector, levels = SEC)) |> 
  subset(source == "EMERGING") |> 
  na_omit() 

# Figure
VS_shares_sec %>% {
  ggplot(., aes(x = sector, y = value, colour = country)) +
      geom_point(size = 3) + # geom_line() +
      scale_color_brewer(palette = "Dark2") + 
      geom_line(aes(x = sector, y = value, linetype = Function, group = Function), colour = "black",
                 data = collap(., value ~ sector, list(Mean = fmean, Median = fmedian), return = "long"), 
                 linewidth = 1, inherit.aes = FALSE) + 
      scale_y_continuous(labels = percent, breaks = extended_breaks(10)) +
      labs(x = "Database", y = "Foreign Content Share", 
           colour = "Country", linetype = "Aggregate") +
      theme_bw() + pretty_plot +
      theme(legend.position = "right")
  }

ggsave("Figures/REV/VA_shares_sec.pdf", width = 10, height = 5)

# Sectoral foreign source shares
VS_shares_sec_origin <- list(EORA = EORA_DET, EMERGING = EM_DET) |> 
  lapply(function(x) {
    sapply(x$decomps, function(d) leontief(d, post = "exports") |> 
           subset(Using_Country %iin% EAC5) |> 
           mutate(Same_Country = Source_Country == Using_Country) |> 
           collap(FVAX ~ Source_Country + Same_Country + Using_Industry, fsum, na.rm = FALSE) |> 
           mutate(VAS = fsum(FVAX, Using_Industry, TRA = "/")), 
           simplify = FALSE) |> 
      rowbind(idcol = "year") |> 
      mutate(year = as.integer(levels(year))[year])
  }) |> rowbind(idcol = "source") |> 
  subset((between(year, 2010, 2015) & source == "EORA") | (year >= 2015 & source == "EMERGING")) |> 
  mutate(Source_Country = factor(setv(as.character(Source_Country), "SSD", "SSA"), levels = REG)) |> 
  join(rowbind(gvr(sec_class, "code$"), gvr(EM_SEC, "code$")) |> 
         compute(Using_Industry = qF(broad_sector_code, sort = FALSE), code = code), 
       on = c("Using_Industry" = "code"), drop = "x") |> 
  group_by(source, year, Source_Country, Same_Country, Using_Industry) |> 
  summarise(VAS = fmean(VAS), FVAX = fsum(FVAX)) |> 
  group_by(source, Source_Country, Same_Country, Using_Industry) |> fmean() |> 
  mutate(VAS_PostAgg = fsum(FVAX, list(source, Using_Industry), TRA = "/")) |> 
  subset(!Same_Country, -Same_Country)

# Note: Post_Agg is better, Similar to BIL_SEC-based figures
VS_shares_sec_origin |>
  subset(source %==% "EMERGING") |> 
  ggplot(aes(x = Using_Industry, y = VAS_PostAgg, fill = Source_Country)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.8) +
  guides(fill = guide_legend(ncol = 1)) + 
  scale_y_continuous(labels = percent, 
                     breaks = extended_breaks(10), expand = c(0,0), limits = c(0, 1)) +  
  labs(x = "Sector", y = "Share of Foreign Exports Content", fill = "Source") +
  theme_bw() + pretty_plot + rbsc2 + theme(legend.position = "right")

ggsave("Figures/REV/VA_shares_sec_ctry.pdf", width = 10, height = 5)

# Show EAC percentage share
VS_shares_sec_origin |>
  subset(source %==% "EMERGING") |> 
  mutate(share = fsum(VAS_PostAgg, Using_Industry, TRA = "/")) |> 
  subset(Source_Country %in% EAC6) %$%
  fsum(share, Using_Industry) |> multiply_by(100) |> 
  # extract(MAN) |> mean()
  sort(decreasing = TRUE)

# Table Including them...
VS_shares_sec %>%
  pivot("sector", "value", "country", how = "w", sort = TRUE) %>%
  transform(Mean = pmean(num_vars(.), na.rm = TRUE), 
            Median = fmedian(transpose(num_vars(.)))) %>% 
  join(VS_shares_sec_origin |> 
         subset(source %==% "EMERGING") |>
         group_by(sector = Using_Industry) |> 
         summarise(EAC = fsum(VAS_PostAgg))) |> 
  transformv(is.numeric, multiply_by, 100) %>% 
  xtable::xtable(digits = 1) %>% print(booktabs = TRUE, include.r = FALSE)


#############################
# Forward GVC Participation
#############################

WDR_POS_AGG |> 
  rowbind(AGG, fill = TRUE) |> 
  mutate(source = factor(source, levels = c("EMERGING", "EORA", "WDR_EORA"))) |> 
  subset(country %in% EAC6 & year >= 2000) |> 
  transformv(c(gvc, gvcb, gvcf), `/`, gexp) |> 
  ggplot(aes(x = year, y = gvcf, colour = source, linetype = source)) +
  geom_line() +
  facet_wrap( ~ country) + 
  labs(y = "Forward GVC Participation (VS1)", x = "Year", 
       colour = "Source:   ", linetype = "Source:   ") +
  theme_bw() + pretty_plot + rbsc2
# Problem: Reduced ICIO (Regions) gives attenuated forward GVC participation

# Computing Classical VS1 Manually: Double counted components are likely small

for (x in list(EM, EORA)) {  
  # = The sum of value added going into other countries exports, divided by own exports
  x$E <- lapply(x$decomps, with, E) |> value2df("E")
  x$E_AGG <- collap(x$E, E ~ year + country, fsum)
  x$VS1_SEC <- lapply(x$decomps, with, rowSums(Vc * Bm %r*% E)) |> value2df("VS1") |> join(x$E)
  x$VS1_AGG <-  collap(x$VS1_SEC, E + VS1 ~ year + country, fsum)
  x$VS1_BIL_SEC <- lapply(x$decomps, with, t(fsum(t(Vc * Bm %r*% E), x$g))) |> value2df() |> 
                   pivot(1:3, names = list("importer", "VS1")) |> 
                   join(select(EM_CTRY, iso3, detailed_region_code), on = c("importer" = "iso3")) |> 
                   group_by(year, country, sector, importer = detailed_region_code) |> 
                   select(VS1) |> fsum() |> 
                   mutate(importer = factor(importer, levels = REG)) |> 
                   join(x$E)
  x$VS1_BIL <- x$VS1_BIL_SEC |> collap(VS1 ~ year + country + importer, fsum) |> join(x$E_AGG)
  for (i in grep("VS1", names(x), value = TRUE)) x[[i]] %<>% mutate(VS1_Share = VS1 / E)
}
rm(x, i)

# Joint Plot (Traditional VS1 Measure)
rowbind(EMERGING = EM$VS1_AGG |> select(year, country, E2R = VS1_Share),
        EORA = EORA$VS1_AGG |> select(year, country, E2R = VS1_Share),
        WDR_EORA = WDR_POS_AGG |> compute(E2R = gvcf / gexp, keep = .c(year, country)),
        idcol = "source") |> 
  subset(country %in% EAC6 & year >= 2000) |> 
  ggplot(aes(x = year, y = E2R, colour = source, linetype = source)) +
  geom_line() +
  scale_y_continuous(labels = percent) +
  facet_wrap( ~ country) + 
  labs(y = "Forward GVC Participation (VS1)", x = "Year", 
       colour = "Source:   ", linetype = "Source:   ") +
  theme_bw() + pretty_plot + rbsc2

ggsave("Figures/REV/VS1_shares_ag_ts.pdf", width = 10, height = 5)

# Time Series Area Plot (Traditional VS1)
EORA$VS1_BIL |> 
  subset(country %in% EAC6) %>% 
  ggplot(aes(x = year, y = VS1_Share, fill = importer)) +
  geom_area(position = "stack", alpha = 0.8) +
  facet_wrap( ~ country, scales = "free_y") + 
  guides(fill = guide_legend(ncol = 1)) + 
  scale_y_continuous(labels = percent, breaks = extended_breaks(10)) +
  scale_x_continuous(expand = c(0,0)) + rbsc2 +
  theme_minimal() + pretty_plot + theme(legend.position = "right")

# Now Joint Plot Using ICIO Data (STATA) (Country-sector level decomposition with full country 5-sector tables)
rowbind(REG_AGG |> compute(E2R = gvcf / gexp, keep = .c(source, year, region)),
        WDR_POS_AGG |> compute(E2R = gvcf / gexp, region = country, keep = .c(source, year))) |> 
  subset(region %in% EAC6 & year >= 2000) |> 
  mutate(region = factor(region, levels = EAC6)) |> 
  ggplot(aes(x = year, y = E2R, colour = source, linetype = source)) +
  geom_line() +
  scale_y_continuous(labels = percent) +
  facet_wrap( ~ region) + 
  labs(y = "Forward GVC Participation (VS1)", x = NULL, 
       colour = "Source:   ", linetype = "Source:   ") +
  theme_bw() + pretty_plot + rbsc2

ggsave("Figures/REV/GVCF_shares_ag_ts.pdf", width = 10, height = 5)

# Correct Country-Level metric (EM Average)
GVCF_EM_correct <- REG_AGG[source == "EMERGING" & year >= 2015, .(E2R = mean(gvcf / gexp)), by = region] |> qM(1) |> drop()

# Barplot with both sources
rowbind(EMERGING = EM$VS1_BIL |> select(year, country, importer, E2R = VS1_Share),
        EORA = EORA$VS1_BIL |> select(year, country, importer, E2R = VS1_Share),
        idcol = "source") |> 
  subset((between(year, 2010, 2015) & source == "EORA") | (year >= 2015 & source == "EMERGING")) |> 
  collap(E2R ~ source + country + importer) |> 
  subset(country %in% EAC6) |> 
  # pivot("importer", "E2R", c("source", "country"), how = "w") |> 
  # transformv(-1, function(x) proportions(x)*100) |> gvr("importer|EMERGING")
  mutate(country = factor(country, levels = EAC6),
         country = setlevels(country, new = paste0(levels(country), "\nE2R: ",
     round(fsum(E2R[source == "EMERGING"], country[source == "EMERGING"])[levels(country)]*100,1), 
         "%\nBM: ", round(GVCF_EM_correct[levels(country)]*100,1), "%"))) |>
  ggplot(aes(x = source, y = E2R, fill = importer)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.8) +
  facet_wrap( ~ country, nrow = 1) + # , scales = "free_y" 
  guides(fill = guide_legend(ncol = 1)) + 
  scale_y_continuous(labels = percent, 
                     breaks = extended_breaks(10), expand = c(0,0), limits = c(0, 1)) +  
  labs(x = "Database", y = "Share of Re-Exported Exports Content", fill = "Partner") +
  theme_bw() + pretty_plot + rbsc2 + theme(legend.position = "right")

ggsave("Figures/REV/VS1_shares_ctry.pdf", width = 12, height = 5.3)

# Bilateral Sector-Level Breakdown: Uganda and Kenya
VS1_UGA_KEN_SEC <- EM$VS1_BIL_SEC |> 
  join(EM$E_AGG, on = c("year", "country"), suffix = "_AGG") |> 
  mutate(VS1_AGG_Share = VS1 / E_AGG,
         Share_in_Country_VS1 = fsum(VS1, list(year, country), TRA = "/"),
         Share_in_Sector_VS1 = fsum(VS1, list(year, sector, country), TRA = "/")) |> 
  subset(year >= 2015 & country %in% EAC6 & importer %in% EAC6) |> 
  group_by(country, sector, importer) |> fmean() |> 
  subset(country %in% c("UGA", "KEN") & importer %in% c("UGA", "KEN") & 
         as.character(country) != importer) |> 
  mutate(Sectoral_Partner_Share = fsum(VS1, importer, TRA = "/"))

VS1_UGA_KEN_SEC |> group_by(country) |> 
  summarise(VS1 = fsum(VS1),
            E_AGG = fmean(E_AGG), 
            VS1_AGG_Share = fsum(VS1_AGG_Share),
            Share_in_Country_VS1 = fsum(Share_in_Country_VS1))

# Breakdown of individual countries and the EAC
VS1_EAC_SEC <- EM$VS1_BIL_SEC |> 
  join(EM$E_AGG, on = c("year", "country"), suffix = "_AGG") |> 
  mutate(VS1_AGG_Share = VS1 / E_AGG,
         Share_in_Country_VS1 = fsum(VS1, list(year, country), TRA = "/"),
         Share_in_Sector_VS1 = fsum(VS1, list(year, sector, country), TRA = "/")) |> 
  subset(year >= 2015 & country %in% EAC6 & importer %in% EAC6) |> 
  group_by(country, sector, importer) |> fmean() |> 
  collap( ~ country + sector, custom = list( # Note: country is exporter
    fsum = .c(VS1, VS1_Share, VS1_AGG_Share, Share_in_Country_VS1, Share_in_Sector_VS1),
    fmean = .c(E, E_AGG)
  )) 

# Presentation:
VS1_EAC_SEC |> 
  mutate(Share_in_Sector_VS1 = Share_in_Sector_VS1 * 100) |> 
  pivot("country", values = c("VS1", "Share_in_Sector_VS1"), names = "sector", how = "w") |> 
  mutate(country = factor(country, levels = EAC6)) |> 
  roworder(country) |> 
  join(VS1_EAC_SEC |> # Adding totals
         collap(VS1 + Share_in_Country_VS1 + VS1_AGG_Share ~ country, fsum, keep.col.order = FALSE) |> 
         transformv(c(Share_in_Country_VS1, VS1_AGG_Share), `*`, 100)) |> 
  colorder(VS1_SRV, VS1, Share_in_Country_VS1, VS1_AGG_Share, pos = "after") |> 
  xtable::xtable() |> print(booktabs = TRUE, include.r = FALSE)
  

# Sector-level Re-Export Shares
REG_SEC[source == "EMERGING", .(year, country = region, sector, VS1 = gvcf, E = gexp)] |> 
# EM$VS1_SEC |> 
  subset(year >= 2015 & country %in% EAC6) |> 
  group_by(country, sector) |> 
  select(VS1, E) |> fsum() %>%
  rowbind(
    subset(., country %in% EAC5) |> 
    group_by(sector) |> 
    select(VS1, E) |> fsum() |> 
    mutate(country = "EAC")
  ) |> 
  mutate(VS1_Share = VS1 / E, 
         country = factor(country, levels = c(EAC6, "EAC"))) |> 
  pivot("sector", "VS1_Share", "country", how = "w") |> 
  replace_outliers(c(0, 1)) %>%
  transform(Mean = pmean(select(., -sector, -EAC), na.rm = TRUE), 
            Median = fmedian(transpose(select(., -sector, -EAC)))) |> 
  colorder(EAC, pos = "end") |> 
  transformv(-1, `*`, 100) |> 
  xtable::xtable(digits = 1) |> print(booktabs = TRUE, include.r = FALSE)

# GVC Partners for EAC Sector-level Re-Exports
EM$VS1_BIL_SEC |> 
  # mutate(VS1 = fsum(VS1, list(year, sector), TRA = "/")) |> # Normalize years (optional)
  subset(year >= 2015 & country %in% EAC5) |> 
  group_by(importer, sector) |> 
  summarise(VS1 = fsum(VS1)) |> # collap(VS1 ~ sector, fsum)
  # pivot("importer", "VS1", "sector", how = "w") |> 
  # tfmv(-1, proportions) |> sbt(importer %in% EAC6, -importer) |> fsum()
  mutate(sector = factor(sector, levels = c("AFF", "MIN", "FBE", "MAN", "SRV"))) |> 

  ggplot(aes(x = sector, y = VS1, fill = importer)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.8) +
  # guides(fill = guide_legend(ncol = 1)) + 
  scale_y_continuous(labels = percent, 
                     breaks = extended_breaks(10), expand = c(0,0)) +  # , limits = c(0, 1)
  labs(y = "Share of Re-Exported Content (VS1)", x = "Broad Sector (5-Sector Aggregation)", fill = "Partner") +
  theme_bw() + pretty_plot + rbsc2 + theme(legend.position = "right")

dev.copy(pdf, "Figures/REV/VS1_shares_sec_ctry.pdf", width = 8, height = 5)
dev.off()

# Correct Levels
REG_SEC[source == "EMERGING" & region %in% EAC5 & year >= 2015, 
        .(VS1 = sum(gvcf)), by = sector] |> 
  ggplot(aes(x = sector, y = VS1)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8)


#############################
# Regional Integration
#############################

# Regional Integration in VA Trade 

# exports_VA <- lapply(EM_DET$decomps, leontief) %>% 
#   unlist2d("Year", DT = TRUE) %>% 
#   tfmv(is.character, qF, sort = FALSE) %>% 
#   tfm(Year = as.integer(as_numeric_factor(Year)))

# (1) EAC Share in VS
VS_EAC <- VS_EAC_BIL |> 
  subset(country %in% EAC5 & share != country) |> 
  group_by(source, year, country, EAC_share = share %in% EAC5) |> 
  summarise(value = fsum(value)) %>% {
  rowbind(
    mutate(., value = fsum(value, list(source, year, country), TRA = "/")) |> 
      subset(EAC_share, -EAC_share),
    collap(., value ~ source + year + EAC_share, fsum) |> 
      mutate(value = fsum(value, list(source, year), TRA = "/"), 
             country = "EAC5") |> 
      subset(EAC_share, -EAC_share)
  )}

# (2) EAC Share in VS1
VS1_EAC <- rowbind(EMERGING = EM$VS1_BIL,
                   EORA = EORA$VS1_BIL, idcol = "source") |> 
  subset(country %in% EAC5 & as.character(country) != importer) |> 
  group_by(source, year, country, EAC_importer = importer %in% EAC5) |> 
  summarise(VS1 = fsum(VS1)) %>% {
  rowbind(
    mutate(., value = fsum(VS1, list(source, country, year), TRA = "/")) |> 
    subset(EAC_importer, -EAC_importer, -VS1),
    collap(., VS1 ~ source + year + EAC_importer, fsum) |> 
    mutate(value = fsum(VS1, list(source, year), TRA = "/"), 
           country = "EAC5") |> 
    subset(EAC_importer, -EAC_importer, -VS1)
  )}

# Value Added in exports to the EAC by country origin
exports_EAC_VA <- list(EORA = EORA_DET, EMERGING = EM_DET) %>% 
  lapply(function(X) {
    lapply(X$decomps, function(o) with(o, (Vc * B) %*% ESR[, EAC5]) %>% fsum(X$g)) %>% 
      unlist2d("year", "exporter", DT = TRUE) %>%
      mutate(year = as.integer(year)) %>% 
      pivot(1:2, names = list("importer", "value")) 
  }) |> rowbind(idcol = "source")

# Value Added in final goods exports to the EAC by country origin
FD_exports_EAC_VA <- list(EORA = EORA_DET, EMERGING = EM_DET) %>% 
  lapply(function(X) {
    lapply(X$decomps, function(o) with(o, (Vc * B) %*% Efd[, EAC5]) %>% fsum(X$g)) %>% 
      unlist2d("year", "exporter", DT = TRUE) %>%
      mutate(year = as.integer(year)) %>% 
      pivot(1:2, names = list("importer", "value")) 
  }) |> rowbind(idcol = "source")


# TODO: Could also compute using DVA in bilateral exports following BM (DAVAX), but does not work for final imports
.c(VAI_EAC, VAFI_EAC) %=% lapply(
  list(exports_EAC_VA, FD_exports_EAC_VA), function(data) {data |> 
  subset(importer %in% EAC5 & exporter != importer) |> 
  group_by(source, year, importer, EAC_exporter = exporter %in% EAC5) |> 
  summarise(value = fsum(value)) %>% {
    rowbind(
      mutate(., value = fsum(value, list(source, year, importer), TRA = "/")) |> 
        subset(EAC_exporter, -EAC_exporter),
      collap(., value ~ source + year + EAC_exporter, fsum) |> 
        mutate(value = fsum(value, list(source, year), TRA = "/"), 
               importer = "EAC5") |> 
        subset(EAC_exporter, -EAC_exporter)
    )} |> 
  rename(importer = country)})


EAC_GVC_DATA <- rowbind(VS = VS_EAC, 
                        VS1 = VS1_EAC, 
                        VAI = VAI_EAC, 
                        VAFI = VAFI_EAC, idcol = "variable")
# Plot as in current paper
EAC_GVC_DATA |>
  mutate(country = factor(country, levels = c(EAC5, "EAC5"))) |>
  subset(source == "EMERGING") |> 
  # subset(source == "EORA" & between(year, 2005, 2015)) |>

  ggplot(aes(x = year, y = value, color = variable)) +
  geom_line() +
  facet_wrap( ~ country, scales = "free_y") +
  scale_y_continuous(labels = percent, breaks = extended_breaks(7)) +
  guides(color = guide_legend(title = NULL)) +
  theme_minimal() + pretty_plot

# Adding weighted linear trend
EAC_GVC_DATA <- EAC_GVC_DATA |> 
  mutate(country = factor(country, levels = c(EAC5, "EAC5")), 
         weight = nif(source == "EMERGING" & year == 2010L, 2, source == "EORA" & year > 2015, 0.1, default = 1)) |> 
  group_by(variable, source, country) |> 
  mutate(as.list(set_names(coef(lm(value ~ year, weights = weight)), c("icpt", "slope")))) |>
  ungroup() |> 
  mutate(trend = icpt + year * slope)

# Improved plot
EAC_GVC_DATA |> 
  rename(value = Value, trend = "Linear Trend") |> 
  pivot(1:4, values = c("Value", "Linear Trend"), names = list("measure", "value")) |> 
  ggplot(aes(x = year, y = value, color = country, linetype = measure)) +
  geom_line() + 
  facet_grid2(source ~ variable, scales = "free", independent = "all") + 
  scale_y_continuous(labels = percent, limits = c(0, NA),
                     breaks = extended_breaks(7)) + 
  scale_x_continuous(n.breaks = 4) +
  guides(color = guide_legend(title = "Country:  ", nrow = 1)) +
  scale_color_manual(values = c(brewer.pal(5, "Dark2"), "black")) +
  labs(x = NULL, y = NULL, linetype = "        Measure:  ") +
  theme_bw() + pretty_plot 

ggsave("Figures/REV/VA_EAC5_shares_ts.pdf", width = 11.69, height = 6)

# Plotting slope coefficients
EAC_GVC_DATA |> 
  group_by(variable, source, country) |>
  select(slope) |> ffirst() |> 
  ggplot(aes(x = variable, y = slope, fill = country)) +
    geom_bar(stat = "identity", position = position_dodge(0.9)) + 
    geom_hline(yintercept = 0) +
    geom_text(aes(label = round(slope*100, 2), vjust = iif(slope > 0, -0.3, 1.3)), size = 2, position = position_dodge(0.9)) +
    facet_wrap( ~ source, scales = "free") + 
    scale_y_continuous(labels = percent, # limits = c(0, NA),
                       breaks = extended_breaks(7)) + 
    guides(fill = guide_legend(title = "Country:  ", nrow = 1)) +
    scale_fill_manual(values = c(brewer.pal(5, "Dark2"), "black")) +
    labs(x = NULL, y = NULL) +
    theme_bw() + pretty_plot 

ggsave("Figures/REV/VA_EAC5_shares_slope_bar.pdf", width = 8, height = 3.5)


# Analysis at the sector-level using broad-sector ICIO's: 

# (1) EAC Share in VS
VS_EAC_BIL_SEC <- 
  list(EORA = EORA, EMERGING = EM) |> 
  lapply(function(x) lapply(x$decomps, function(z) leontief(z) |> 
      subset(Using_Country %iin% EAC5) |> 
      collap(FVAX ~ Source_Country + Using_Country + Using_Industry, fsum, na.rm = FALSE) 
  )) |> 
  unlist2d(c("source", "year"), DT = TRUE) |> 
  mutate(year = as.integer(year)) |> 
  rename(Source_Country = share, Using_Country = country, Using_Industry = sector, FVAX = value)

VS_EAC_SEC <- VS_EAC_BIL_SEC |> 
  subset(country %in% EAC5 & share != country) |> 
  group_by(source, year, country, sector, EAC_share = share %in% EAC5) |> 
  summarise(value = fsum(value)) %>% {
    rowbind(
      mutate(., value = fsum(value, list(source, year, country, sector), TRA = "/")) |> 
        subset(EAC_share, -EAC_share),
      collap(., value ~ source + year + sector + EAC_share, fsum) |> 
        mutate(value = fsum(value, list(source, year, sector), TRA = "/"), 
               country = "EAC5") |> 
        subset(EAC_share, -EAC_share)
    )}

# (2) EAC Share in VS1
VS1_EAC_SEC <- rowbind(EMERGING = EM$VS1_BIL_SEC,
                       EORA = EORA$VS1_BIL_SEC, idcol = "source") |> 
  subset(country %in% EAC5 & as.character(country) != importer) |> 
  group_by(source, year, country, sector, EAC_importer = importer %in% EAC5) |> 
  summarise(VS1 = fsum(VS1)) %>% {
    rowbind(
      mutate(., value = fsum(VS1, list(source, country, sector, year), TRA = "/")) |> 
        subset(EAC_importer, -EAC_importer, -VS1),
      collap(., VS1 ~ source + year + sector + EAC_importer, fsum) |> 
        mutate(value = fsum(VS1, list(source, year, sector), TRA = "/"), 
               country = "EAC5") |> 
        subset(EAC_importer, -EAC_importer, -VS1)
    )}

# Value Added in exports to the EAC by country origin
exports_EAC_VA_SEC <- list(EORA = EORA, EMERGING = EM) %>% 
  lapply(function(X) {
    lapply(X$decomps, function(o) with(o, (Vc * B) %*% ESR[, EAC5])) |> 
      value2df() |> 
      rename(country = exporter) |> 
      pivot(1:3, names = list("importer", "value")) 
  }) |> rowbind(idcol = "source")

# Value Added in final goods exports to the EAC by country origin
FD_exports_EAC_VA_SEC <- list(EORA = EORA, EMERGING = EM) %>% 
  lapply(function(X) {
    lapply(X$decomps, function(o) with(o, (Vc * B) %*% Efd[, EAC5])) |> 
      value2df() |> 
      rename(country = exporter) |> 
      pivot(1:3, names = list("importer", "value")) 
  }) |> rowbind(idcol = "source")


# TODO: Could also compute using DVA in bilateral exports following BM (DAVAX), but does not work for final imports
.c(VAI_EAC_SEC, VAFI_EAC_SEC) %=% lapply(
  list(exports_EAC_VA_SEC, FD_exports_EAC_VA_SEC), function(data) {data |> 
      subset(importer %in% EAC5 & as.character(exporter) != importer) |> 
      group_by(source, year, importer, sector, EAC_exporter = exporter %in% EAC5) |> 
      summarise(value = fsum(value)) %>% {
        rowbind(
          mutate(., value = fsum(value, list(source, year, importer, sector), TRA = "/")) |> 
            subset(EAC_exporter, -EAC_exporter),
          collap(., value ~ source + year + sector + EAC_exporter, fsum) |> 
            mutate(value = fsum(value, list(source, year, sector), TRA = "/"), 
                   importer = "EAC5") |> 
            subset(EAC_exporter, -EAC_exporter)
        )} |> 
      rename(importer = country)})


EAC_GVC_DATA_SEC <- rowbind(VS = VS_EAC_SEC, 
                            VS1 = VS1_EAC_SEC, 
                            VAI = VAI_EAC_SEC, 
                            VAFI = VAFI_EAC_SEC, idcol = "variable") |> 
                    na_omit() |> 
                    subset(GRPN(list(variable, source, country, sector)) > 4L)


# Plot as in current paper
EAC_GVC_DATA_SEC |>
  mutate(country = factor(country, levels = c(EAC5, "EAC5"))) |>
  subset(source == "EMERGING") |> # & between(year, 2005, 2015)) |>
  
  ggplot(aes(x = year, y = value, color = variable)) +
  geom_line() +
  facet_grid2(sector ~ country, scales = "free_y", independent = "y") +
  scale_y_continuous(labels = percent, breaks = extended_breaks(7)) +
  guides(color = guide_legend(title = NULL)) +
  theme_minimal() + pretty_plot


# Adding weighted linear trend
EAC_GVC_DATA_SEC <- EAC_GVC_DATA_SEC |> 
  mutate(country = factor(country, levels = c(EAC5, "EAC5")),
         weight = nif(source == "EMERGING" & year == 2010L, 2, source == "EORA" & year > 2015, 0.1, default = 1)) |> 
  group_by(variable, source, country, sector) |> 
  mutate(as.list(set_names(coef(lm(value ~ year, weights = weight)), c("icpt", "slope")))) |> 
  ungroup() |> 
  mutate(trend = icpt + year * slope)

# Improved plot
EAC_GVC_DATA_SEC |> 
  subset(source == "EMERGING") |> 
  rename(value = Value, trend = "Linear Trend") |> 
  pivot(1:5, values = c("Value", "Linear Trend"), names = list("measure", "value")) |> 
  ggplot(aes(x = year, y = value, color = country, linetype = measure)) +
  geom_line() + 
  facet_grid2(sector ~ variable, scales = "free", independent = "all") + 
  scale_y_continuous(labels = percent, limits = c(0, NA),
                     breaks = extended_breaks(7)) + 
  scale_x_continuous(n.breaks = 4) +
  guides(color = guide_legend(title = "Country:  ", nrow = 1)) +
  scale_color_manual(values = c(brewer.pal(5, "Dark2"), "black")) +
  labs(x = NULL, y = NULL, linetype = "        Measure:  ") +
  theme_bw() + pretty_plot 

ggsave("Figures/REV/EM_VA_EAC5_shares_ts_sec.pdf", width = 11.69, height = 10)

# Plotting slope coefficients

EAC_GVC_DATA_SEC |> 
  subset(source == "EMERGING" & sector != "MIN") |> 
  group_by(variable, source, country, sector) |> 
  select(slope) |> ffirst() |> 
  mutate(slope_tr = nif(slope > 0.01, 0.01 + (slope-0.01) * 0.1, slope < -0.01, -0.01 + (slope+0.01) * 0.1, default = slope),
         sector = recode_char(as.character(sector),
                              AFF = "Agriculture & Livestock", 
                              FBE = "Foods & Beverages", 
                              MAN = "Manufactured Goods",
                              SRV = "Services")) |> 
  
  ggplot(aes(x = variable, y = slope_tr, fill = country)) +
    geom_bar(stat = "identity", position = position_dodge(0.9)) + 
    geom_hline(yintercept = 0) + 
    geom_hline(yintercept = c(-0.01, 0.01), linetype = "dotted") + 
    geom_text(aes(label = round(slope*100, 2), vjust = iif(slope > 0, -0.3, 1.3)), size = 2, position = position_dodge(0.9)) +
    facet_wrap("sector", scales = "fixed") + 
    scale_y_continuous(labels = percent, limits = c(-0.013, 0.013), expand = c(0,0),
                       breaks = extended_breaks(7)) + 
    guides(fill = guide_legend(title = "Country:  ", nrow = 1)) +
    scale_fill_manual(values = c(brewer.pal(5, "Dark2"), "black")) +
    labs(x = NULL, y = NULL) +
    theme_bw() + pretty_plot 
  
ggsave("Figures/REV/EM_VA_EAC5_shares_slope_bar_sec.pdf", width = 9, height = 5.5)





#############################
# KWW Decomposition
#############################

KWW <- rowbind(EMERGING = fread("/Users/sebastiankrantz/Documents/Data/EMERGING/GVC_Regions/EM_GVC_KWW_BM19.csv"),
               EORA = fread("/Users/sebastiankrantz/Documents/Data/EORA/GVC_Regions/EORA_GVC_KWW_BM19.csv") |> 
                 transformv(is.double, `*`, 1/1000), 
               idcol = "source")

KWW_BSEC <- rowbind(EMERGING = fread("/Users/sebastiankrantz/Documents/Data/EMERGING/GVC_Countries_Agg_Sectors/EM_GVC_KWW_BM19.csv"),
                    EORA = fread("/Users/sebastiankrantz/Documents/Data/EORA/GVC_Countries_Agg_Sectors/EORA_GVC_KWW_BM19.csv") |> 
                           transformv(is.double, `*`, 1/1000), 
                    idcol = "source")

KWW |> with(gexp / psum(vax, ref, ddc, fva, fdc)) |> descr() |> print(digits = 4)
# Source-Based Decomposition
AGG |> with(gexp / psum(davax, vax-davax, ref, ddc, fva, fdc)) |> descr() |> print(digits = 4)
AGG |> with(gvcf / (dva - davax)) |> descr() |> print(digits = 4) # Check definition of forward gvc integration

KWW_EAC6 <- KWW |> 
  subset(country %in% EAC6) |> 
  join(compute(AGG, davax_ratio = davax / vax, keep = .c(source, year, country))) |> 
  mutate(davax = vax * davax_ratio, 
         ndavax = vax - davax, 
         country = factor(country, levels = EAC6)) 

KWW_EAC6 |> 
  # transformv(c("davax", "ndavax", "ref", "ddc", "fva", "fdc"), `/`, gexp) |> subset(year >= 2015) |> collap(~ source)
  # pivot(1:3, values = c("davax", "ndavax", "ref", "ddc", "fva", "fdc")) |> 
  pivot(1:3, values = c("davax", "ndavax", "ref", "ddc", "fva", "fdc")) |> 
  mutate(variable = set_attr(variable, "levels", toupper(levels(variable)))) |> 
  
  ggplot(aes(x = year, y = value, fill = variable)) +
      geom_area(position = "fill", alpha = 0.8) +
      facet_grid2(source ~ country, scales = "free", independent = "x") +
      scale_y_continuous(labels = percent, breaks = extended_breaks(7), 
                         expand = c(0, 0)) +
      scale_x_continuous(n.breaks = 6, expand = c(0, 0)) + rbsc2 +
      labs(x = NULL, y = "VA Share in Gross Exports", fill = "Component") +
      theme_linedraw() + pretty_plot +
      theme(legend.position = "right", 
            axis.text.x = element_text(angle = 315, vjust = 0))

ggsave("Figures/REV/KWW_DEC_NEW.pdf", width = 11.69, height = 5)    
    

##################################
# Upstreamness and Downstreamness
##################################

# Linear Trends
KWW_EAC6_USDS <- KWW_EAC6 |> 
  transform(upstreamness = (ndavax + ddc) / dc,
            downstreamness = fva / fc) |>  # Imperfect !! -> should be FVA in final vs. intermediate goods
  pivot(1:3, values = c("upstreamness", "downstreamness")) |> 
  mutate(weight = nif(source == "EMERGING" & year == 2010L, 2, source == "EORA" & year > 2015, 0.1, default = 1)) |> 
  group_by(source, variable, country) |> 
  mutate(as.list(set_names(coef(lm(value ~ year, weights = weight)), c("icpt", "slope")))) |> 
  ungroup() |> 
  mutate(trend = icpt + year * slope)

KWW_EAC6_USDS |> 
  ggplot(aes(x = year, y = value, colour = variable, linetype = source)) +
  geom_line() +
  facet_wrap(~ country, scales = "fixed", nrow = 1) +
  scale_y_continuous(labels = percent, breaks = extended_breaks(7)) +
  scale_x_continuous(n.breaks = 6, expand = c(0, 0)) + 
  scale_color_brewer(palette = "Dark2") +
  labs(x = NULL, y = "Upstreamness and Downstreamness", colour = "Indicator:  ") +
  theme_bw() + pretty_plot +
  theme(axis.text.x = element_text(angle = 315, vjust = 0))

KWW_EAC6_USDS |> 
  ggplot(aes(x = year, y = value, colour = variable)) +
  geom_line() +
  geom_line(aes(y = trend), linetype = "dotted") +
  facet_grid2(source ~ country, scales = "free_x", independent = "x") +
  scale_y_continuous(labels = percent, breaks = extended_breaks(7)) +
  scale_x_continuous(n.breaks = 6, expand = c(0, 0)) + 
  scale_color_brewer(palette = "Dark2") +
  labs(x = NULL, y = "Upstreamness and Downstreamness", colour = "Indicator:  ") +
  theme_bw() + pretty_plot +
  theme(axis.text.x = element_text(angle = 315, vjust = 0))

ggsave("Figures/REV/UP_DOWN_ag_ts.pdf", width = 11.69, height = 8.27)

# Plotting slope coefficients
KWW_EAC6_USDS |> 
  group_by(variable, source, country) |> 
  select(slope) |> ffirst() |> 
  ggplot(aes(x = variable, y = slope, fill = country)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) + 
  geom_hline(yintercept = 0) +
  geom_text(aes(label = round(slope*100, 2), vjust = iif(slope > 0, -0.3, 1.3)), size = 2, position = position_dodge(0.9)) +
  facet_wrap( ~ source, scales = "free") + 
  scale_y_continuous(labels = percent, # limits = c(0, NA),
                     breaks = extended_breaks(7)) + 
  guides(fill = guide_legend(title = "Country:  ", nrow = 1)) +
  scale_fill_manual(values = c(brewer.pal(5, "Dark2"), "black")) +
  labs(x = NULL, y = NULL) +
  theme_bw() + pretty_plot 

ggsave("Figures/REV/VA_EAC5_shares_slope_bar.pdf", width = 8, height = 4)



# Now plotting Mancini et al's measures. 
WDR_POS_AGG |> 
  subset(country %in% EAC6 & year >= 2000) |> 
  mutate(country = factor(country, levels = EAC6), 
         position = upstreamness / downstreamness) |> 
  pivot(c("year", "country"), c("upstreamness", "downstreamness", "position")) |> 
  
  ggplot(aes(x = year, y = value, colour = variable)) +
  geom_line() +
  facet_wrap(~ country, scales = "fixed", nrow = 1) +
  scale_y_continuous(breaks = extended_breaks(7)) +
  scale_x_continuous(n.breaks = 6, expand = c(0, 0)) + 
  scale_color_brewer(palette = "Dark2") +
  labs(x = NULL, y = "Upstreamness and Downstreamness", colour = "Indicator:  ") +
  theme_bw() + pretty_plot +
  theme(axis.text.x = element_text(angle = 315, vjust = 0))
  

# Upstreamness following Antras et al. 2013: (no inventory correction yet as in mancini et al GVC Positioning database)
U <- list(EORA = EORA, EMERGING = EM) %>% 
  lapply(function(X) {
    lapply(X$decomps, function(o) {
      tmp = o$B %*% rowSums(o$Y)
      cbind(U = drop((o$B %*% tmp) %/=% tmp), E = o$E)
    }) |> value2df()
  }) |> rowbind(idcol = "source")

U_DET <- list(EORA = EORA_DET, EMERGING = EM_DET) %>% 
  lapply(function(X) {
    lapply(X$decomps, function(o) {
      tmp = o$B %*% rowSums(o$Y)
      cbind(U = drop((o$B %*% tmp) %/=% tmp), E = o$E)
    }) |> value2df()
  }) |> rowbind(idcol = "source")

# Comparison
U_DET_ALL <- WDR_POS |> 
  select(source, year, country, sector = sect, U = upstreamness, E = gexp) |> 
  rowbind(U_DET)

U_DET_ALL |> 
  group_by(source, year, country) |> 
  num_vars() |> fmean(E, keep.w = FALSE) |> 
  subset(country %in% EAC6 & year >= 2000) |> 
  mutate(country = factor(country, levels = EAC6)) |> 
  
  ggplot(aes(x = year, y = U, colour = source, linetype = source)) +
  geom_line() +
  facet_wrap(~ country, scales = "fixed", nrow = 2) +
  labs(y = "Upstreamness Index", x = NULL, 
       colour = "Source:   ", linetype = "Source:   ") +
  theme_bw() + pretty_plot + rbsc2

ggsave("Figures/REV/Upstreamness_ag_ts.pdf", width = 10, height = 5)


# At the Broad Sector Level
U_DET_ALL_BSEC <- U_DET_ALL |> 
  join(rowbind(sbt(SEC_ALL, source == "EMERGING"), 
               sbt(SEC_ALL, source == "EORA") |> mtt(source = "WDR_EORA"),
               mtt(slt(sec_class, id = code, broad_sector_code), source = "EORA")), 
       on = c("source", "sector" = "id")) |> 
  mutate(sector = NULL) |> 
  group_by(source, year, country, sector = broad_sector_code) |> 
  select(U, E) |> fmean(E)

# Detailed Plot
U_DET_ALL_BSEC |> 
  subset(country %in% EAC6 & year >= 2000) |> 
  mutate(country = factor(country, levels = EAC6), 
         sector = factor(sector, levels = SEC)) |> 
  
  ggplot(aes(x = year, y = U, colour = source, linetype = source)) +
  geom_line() +
  facet_grid2(sector ~ country, scales = "free", independent = "y") +
  labs(y = "Upstreamness Index", x = NULL, 
       colour = "Source:   ", linetype = "Source:   ") +
  theme_bw() + pretty_plot + rbsc2

ggsave("Figures/REV/Upstreamness_sec_ts.pdf", width = 10, height = 15)

# Aggregate Sectors
U_DET_ALL_BSEC |> 
  subset(country %in% EAC6 & year >= 2000) |> 
  mutate(country = factor(country, levels = EAC6), 
         sector = nif(sector == "AFF", "AGR", # %in% c("AGR", "FIS")
                      sector == "MIN", "MIN", 
                      sector == "FBE", "FBE",
                      sector %in% c("TEX", "WAP", "PCM", "MPR", "ELM", "TEQ", "MAN"), "MAN", 
                      default = "SRV")) |> 
  group_by(source, year, country, sector) |> fmean(sum.E) |> 
  
  ggplot(aes(x = year, y = U, colour = source, linetype = source)) +
  geom_line() +
  facet_grid2(sector ~ country, scales = "free", independent = "y") +
  labs(y = "Upstreamness Index", x = NULL, 
       colour = "Source:   ", linetype = "Source:   ") +
  theme_bw() + pretty_plot + rbsc2

ggsave("Figures/REV/Upstreamness_broad_sec_ts.pdf", width = 10, height = 8)

# Aggregating the Time Dimension: Simple changes 2010-2014 to 2015-2019 by sector
U_DET_ALL_BSEC |> 
  subset(country %in% EAC6 & year >= 2000) |> 
  group_by(source, country, sector, 
           year = nif(year < 2010, "2000-2009", year < 2015, "2010-2014", year < 2020, "2015-2019")) |> 
  fmean(sum.E) |> 
  subset(source != "EORA" & !is.na(year)) |> 
  group_by(source, country, sector) |> 
  fdiff(t = year) |> 
  ungroup() |> 
  subset(year == "2015-2019") |> 
  pivot(c("source", "sector"), "U", "country", how = "w")

# Both Sectors and Time Dimension: Changes + Growth Rate: Reported in Paper
U_DET_ALL_BSEC |> 
  subset(country %in% EAC6 & year >= 2000) |> 
  mutate(country = factor(country, levels = EAC6), 
         sector = nif(sector == "AFF", "AFF", 
                      sector == "MIN", "MIN", 
                      sector == "FBE", "FBE",
                      sector %in% c("TEX", "WAP", "PCM", "MPR", "ELM", "TEQ", "MAN"), "MAN", 
                      # sector == "TRA", "TRA",
                      # sector == "PTE", "PTE",
                      # sector == "FIB", "FIB",
                      default = "SRV")) |> 
  group_by(source, year, country, sector) |> fmean(sum.E) %>%
  rowbind( # Adding EAC5
    subset(., country %in% EAC5) |> 
    collap(U ~ source + year + sector, w = ~ sum.sum.E) |> 
    mutate(country = "EAC5")
  ) |> 
  mutate(year = nif(year < 2010, "2000-2009", year < 2015, "2010-2014", year < 2020, "2015-2019")) |> 
  group_by(source, year, country, sector) |> fmedian() |> 
  subset(!is.na(year) & source != "EORA") |> 
  pivot(c("sector", "year"), "U", c("source", "country"), how = "w") |> 
  subset(year != "2000-2009") %>%
  rowbind(G(., by = ~ sector, t = ~ year, stub = FALSE) |> 
          subset(year == "2015-2019") |> 
          mutate(year = "Growth Rate")) |> 
  roworder(sector, year) |> 
  gvr("COD", invert = TRUE) |> 
  xtable::xtable() |> print(booktabs = TRUE, include.r = FALSE)



# Check Global Average Sectoral Changes in Upstreamness
U_DET_ALL_BSEC |> 
  mutate(sector = nif(sector == "AFF", "AFF", 
                      sector == "MIN", "MIN", 
                      sector == "FBE", "FBE",
                      sector %in% c("TEX", "WAP", "PCM", "MPR", "ELM", "TEQ", "MAN"), "MAN", 
                      # sector == "TRA", "TRA",
                      default = "SRV")) |> 
  group_by(source, year, sector) |> num_vars() |> fmean(sum.E) |> 
  # subset(between(year, 2015, 2019)) |> 
  # group_by(source, sector) |> num_vars() |> fmedian() |> 
  mutate(year = nif(year < 2010, "2000-2009", year < 2015, "2010-2014", year < 2020, "2015-2019")) |> 
  group_by(source, year, sector) |> fmedian() |> 
  subset(year != "2000-2009" & source != "EORA") |>
  pivot(c("sector", "year"), "U", "source", how = "w") %>%
  rowbind(G(., by = ~ sector, t = ~ year, stub = FALSE) |> 
            subset(year == "2015-2019") |> 
            mutate(year = "Growth Rate")) 


# Downstreamness following Antras et al. 2013: (no inventory correction yet as in mancini et al GVC Positioning database)
D <- list(EORA = EORA, EMERGING = EM) %>% 
  lapply(function(X) {
    lapply(X$decomps, function(o) cbind(D = fsum(o$B), E = o$E)) |> value2df()
  }) |> rowbind(idcol = "source")

D_DET <- list(EORA = EORA_DET, EMERGING = EM_DET) %>% 
  lapply(function(X) {
    lapply(X$decomps, function(o) cbind(D = fsum(o$B), E = o$E)) |> value2df()
  }) |> rowbind(idcol = "source")

# Comparison
WDR_POS |> 
  select(source, year, country, sector = sect, D = downstreamness, E = gexp) |> 
  rowbind(D_DET) |> 
  group_by(source, year, country) |> 
  num_vars() |> fmean(E, keep.w = FALSE) |> 
  subset(country %in% EAC6 & year >= 2000) |> 
  
  ggplot(aes(x = year, y = D, colour = source, linetype = source)) +
  geom_line() +
  facet_wrap(~ country, scales = "fixed", nrow = 2) +
  labs(y = "Downstreamness Index", x = "Year", 
       colour = "Source:   ", linetype = "Source:   ") +
  theme_bw() + pretty_plot + rbsc2



# Note: they are positively correlated !!!
join(U, slt(D, -E), verbose = 0) %$% pwcor(U, D)
join(U, slt(D, -E), verbose = 0) |> STD(~source+country+sector, stub =FALSE) %$% pwcor(U, D)

join(U_DET, slt(D_DET, -E), verbose = 0) %$% pwcor(U, D)
join(U_DET, slt(D_DET, -E), verbose = 0) |> STD(~source+country+sector, stub =FALSE) %$% pwcor(U, D)


#######################################
# (New) Revealed Comparative Advantage
#######################################

# -> EAC_BACI_SEC is calculated in TRADE_FLOWS.R

# Standard Comparative Advantage with BACI
BACI_RCA <- EAC_BACI_SEC |> 
  group_by(year, country = iso3_o, sector = broad_sector_code) |> 
  select(value) |> fsum() %>% 
  rowbind(subset(., country %in% EAC5) |> 
          collap(value ~ year + sector, fsum) |> 
          mutate(country = "EAC5")) |> 
  mutate(SSH = fsum(value, list(year, country), TRA = "/"),          # Share of Sector in country exports
         SWSH = fsum(value, list(year, sector), TRA = "fill") %/=%   # Share of Sector in World Exports
                fsum(value, year, TRA = "fill"),
         RCA = SSH / SWSH, # Revealed Comparative Advantage
         country = factor(country, levels = c(EAC, "EAC5", "ROW")),
         sector = factor(sector, levels = SEC)) |> 
  droplevels() |> 
  mutate(source = qF("BACI")) |> colorder(source)

# Revealed Comparative Advantage With ICIO Data
RCA <- rowbind(WDR_POS_SEC |> select(source, year, country, sector, gexp),
               BIL_SEC |> select(source, year, country = from_region, sector = from_sector, gexp)) |> 
  group_by(source, year, country, sector) |> 
  select(value = gexp) |> fsum() %>% 
  rowbind(subset(., country %in% EAC5) |> 
            collap(value ~ source + year + sector, fsum) |> 
            mutate(country = "EAC5")) |> 
  mutate(SSH = fsum(value, list(source, year, country), TRA = "/"),        # Share of Sector in country exports
         SWSH = fsum(value, list(source, year, sector), TRA = "fill") %/=% # Share of Sector in World Exports
           fsum(value, list(source, year), TRA = "fill"),
         RCA = SSH / SWSH,
         country = factor(country, levels = c(EAC, "EAC5", ROW)),
         sector = factor(sector, levels = SEC)) |> 
  droplevels()

# New Revealed Comparative Advantage based on DVA in Exports
NRCA <- rowbind(WDR_POS_SEC |> select(source, year, country, sector, dva),
                BIL_SEC |> select(source, year, country = from_region, sector = from_sector, dva)) |> 
  group_by(source, year, country, sector) |> 
  select(value = dva) |> fsum() %>% 
  rowbind(subset(., country %in% EAC5) |> 
            collap(value ~ source + year + sector, fsum) |> 
            mutate(country = "EAC5")) |> 
  mutate(SSH = fsum(value, list(source, year, country), TRA = "/"),        # Share of Sector in country exports
         SWSH = fsum(value, list(source, year, sector), TRA = "fill") %/=% # Share of Sector in World Exports
                fsum(value, list(source, year), TRA = "fill"),
         RCA = SSH / SWSH,
         country = factor(country, levels = c(EAC, "EAC5", ROW)),
         sector = factor(sector, levels = SEC)) |> 
  droplevels()

# Combining  
RCA_ALL <- rowbind(GX = RCA, 
                   GX = BACI_RCA, 
                   VAX = NRCA, idcol = "type") 
RCA_ALL |> 
  subset(between(year, 2010, 2019) & country %in% c(EAC5, "EAC5") & RCA > 0) |> 
  collap(RCA ~ type + source + country + sector, fmedian, na.rm = TRUE) |> # with(range(RCA))
  mutate(source = factor(source, levels = c("WDR_EORA", "EORA", "EMERGING", "BACI"))) |> 
  subset(type == "VAX" | source %in% c("EMERGING", "BACI")) |> 
  # # Print Numbers (Appendix)
  # pivot(c("country", "source", "type"), "RCA", "sector", how = "w") |>
  #     xtable::xtable() |> print(booktabs = TRUE, include.r = FALSE)
  # # Print Correlations (Appendix)
  # pivot(names = c("source", "type"), values = "RCA", how = "w", sort = "names") |>
  #     num_vars() |> pwcor() |>
  #     print(digits = 3, return = TRUE) |>
  #     xtable::xtable() |> print(booktabs = TRUE)
  mutate(RCA = replace_outliers(RCA, c(0.01, 30), "clip")) |> 
  
  ggplot(aes(x = RCA, y = sector, colour = type, shape = source)) +
  geom_vline(xintercept = 1) + geom_point(alpha = 0.8) +
  facet_wrap( ~ country, scales = "free_y") +
  scale_x_continuous(trans = "log10", breaks = log_breaks(10), limits = c(0.01, 30),
                     expand = c(0, 0.02), labels = function(x) signif(x, 3)) +
  scale_y_discrete(limits = rev) +
  theme_bw() + pretty_plot + 
  labs(colour = "Flow: ", shape = "Source: ", y = "Sector", x = "(New) Revealed Comparative Advantage") +
  scale_color_manual(values = c( "blue", "red")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 3.5)), 
        plot.margin = margin(r = 10))
  
ggsave("Figures/REV/NRCA_EAC5_ALL.pdf", width = 10, height = 7)

  
# RCA relative to EAC
EAC_RCA_ALL <- RCA_ALL |> 
  subset(country %in% EAC5) |> 
  mutate(SSH = fsum(value, list(type, source, year, country), TRA = "/"),        # Share of Sector in country exports
         SWSH = fsum(value, list(type, source, year, sector), TRA = "fill") %/=% # Share of Sector in EAC exports
                fsum(value, list(type, source, year), TRA = "fill"),
         RCA = SSH / SWSH,
         country = factor(country, levels = EAC),
         sector = factor(sector, levels = SEC)) |> 
  droplevels()

# RCA for inner-EAC Trade 
RCA_IEAC_ALL <- rowbind(
  GX = BIL_SEC[from_region %in% EAC5 & to_region %in% EAC5, .(value = sum(gexp)), 
               by = .(source, year, country = from_region, sector = from_sector)], 
  GX = EAC_BACI_SEC[iso3_o %in% EAC5 & iso3_d %in% EAC5, .(value = sum(value)), 
                    by = .(year, country = iso3_o, sector = broad_sector_code)][, source := qF("BACI")], 
  VAX = BIL_SEC[from_region %in% EAC5 & to_region %in% EAC5, .(value = sum(dva)), 
                by = .(source, year, country = from_region, sector = from_sector)], 
  idcol = "type") |> 
  mutate(SSH = fsum(value, list(type, source, year, country), TRA = "/"),        # Share of Sector in country exports
         SWSH = fsum(value, list(type, source, year, sector), TRA = "fill") %/=% # Share of Sector in inner-EAC exports
                fsum(value, list(type, source, year), TRA = "fill"),
         RCA = SSH / SWSH,
         country = factor(country, levels = EAC5),
         sector = factor(sector, levels = SEC)) |> 
  droplevels()


# Joint Plot
rowbind("Relative to EAC Exports" = EAC_RCA_ALL,
        "In Inner-EAC Trade" = RCA_IEAC_ALL, 
        idcol = "measure") |> 
  subset(between(year, 2010, 2019) & RCA > 0) |> 
  collap(RCA ~ measure + type + source + country + sector, fmedian, na.rm = TRUE) |> # with(range(RCA))
  subset(type == "VAX" | source %in% c("EMERGING", "BACI")) |> 
  # pivot(c("measure", "country", "source", "type"), "RCA", "sector", how = "w") |> # View()
  #       subset(measure == "In Inner-EAC Trade", -measure) |> 
  #       xtable::xtable() |> print(booktabs = TRUE, include.r = FALSE)
  mutate(source = factor(source, levels = c("WDR_EORA", "EORA", "EMERGING", "BACI")), 
         RCA = replace_outliers(RCA, c(0.03, 30), "clip")) |> 
  
  ggplot(aes(x = RCA, y = sector, colour = type, shape = source)) +
  geom_vline(xintercept = 1) + 
  geom_point(alpha = 0.8) +
  facet_grid(measure ~ country) +
  scale_x_continuous(trans = "log10", breaks = log_breaks(10), limits = c(0.03, 30),
                     expand = c(0, 0.02), labels = function(x) signif(x, 3)) +
  scale_y_discrete(limits = rev) +
  theme_bw() + pretty_plot + 
  labs(colour = "Flow: ", shape = "Source: ", y = "Sector", 
       x = "(New) Revealed Comparative Advantage") +
  scale_color_manual(values = c( "blue", "red")) +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0), 
        plot.margin = margin(r = 10))

ggsave("Figures/REV/EAC_NRCA_EAC5_ALL.pdf", width = 12, height = 7)


# Shifts in RCA 

rowbind("Overall" = RCA_ALL,
        "Relative to EAC" = EAC_RCA_ALL,
        "In Inner-EAC Trade" = RCA_IEAC_ALL,
        idcol = "measure") |> 
  subset(country %in% c(EAC5, "EAC5") & source %in% c("EMERGING", "BACI") & (type == "VAX" | source == "BACI")) |> 
  group_by(measure, type, source, country, sector, 
           period = nif(between(year, 2006, 2010), "2006-2010", between(year, 2015, 2019), "2015-2019")) |> 
  select(RCA) |> fmedian() |> na_omit(cols = "period") |> # select(type, source) |> table()
  subset(GRPN(list(measure, type, source, country, sector)) == 2L) %>%
  # rowbind(
  #   G(., by = RCA ~ measure + type + source + country + sector, t = ~ period, stubs = FALSE) |> 
  #     na_omit() |> mutate(period = "Growth Rate")    
  # ) |> pivot(c("measure", "country", "source", "type", "period"), "RCA", "sector", how = "w") |> 
  # roworder(measure, source, type, country, period) |> 
  # subset(measure == "In Inner-EAC Trade", -measure) |> 
  # xtable::xtable() |> print(booktabs = TRUE, include.r = FALSE)
  mutate(RCA = replace_outliers(RCA, c(0.03, 30), "clip")) |> 
  
  ggplot(aes(x = RCA, y = sector, colour = period, shape = source)) +
  geom_vline(xintercept = 1) + 
  geom_point(alpha = 0.7) +
  facet_grid(country ~ measure) +
  scale_x_continuous(trans = "log10", breaks = log_breaks(10), limits = c(0.03, 30),
                     expand = c(0, 0.02), labels = function(x) signif(x, 3)) +
  scale_y_discrete(limits = rev) +
  scale_shape_manual(values = c("circle", "plus")) +
  theme_bw() + pretty_plot + 
  labs(colour = "Flow: ", shape = "Source: ", y = "Sector", x = "(New) Revealed Comparative Advantage") +
  scale_color_brewer(palette = "Paired") +
  # scale_color_manual(values = c( "blue", "red")) +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0), 
        plot.margin = margin(r = 10))

ggsave("Figures/REV/NRCA_EAC5_DIFF_ALL.pdf", width = 10, height = 14)


# Growth Rates

rowbind("Overall" = RCA_ALL,
        "Relative to EAC" = EAC_RCA_ALL,
        "In Inner-EAC Trade" = RCA_IEAC_ALL,
        idcol = "measure") |> 
  subset(country %in% c(EAC5, "EAC5") & source %in% c("EMERGING", "BACI") & (type == "VAX" | source == "BACI")) |> 
  group_by(measure, type, source, country, sector, 
           period = nif(between(year, 2006, 2010), "2006-2010", between(year, 2015, 2019), "2015-2019")) |> 
  select(RCA) |> fmedian() |> na_omit(cols = "period") |> # select(type, source) |> table()
  subset(GRPN(list(measure, type, source, country, sector)) == 2L) |> 
  G(by = RCA ~ measure + type + source + country + sector, t = ~ period, stubs = FALSE) |> 
  na_omit(cols = "RCA") |> # descr(cols = "RCA")
  replace_outliers(c(-100, 100), "clip") |> 
  
  ggplot(aes(x = RCA, y = sector, colour = measure, shape = source)) +
  geom_vline(xintercept = 1) + 
  geom_point(alpha = 0.7) +
  facet_wrap( ~ country, nrow = 2) +
  scale_shape_manual(values = c("circle", "plus")) +
  scale_y_discrete(limits = rev) +
  theme_bw() + pretty_plot + 
  labs(colour = "Measure: ", shape = "Source: ", y = "Sector", x = "Growth Rate of (N)RCA (Percent)") +
  scale_color_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0), 
        plot.margin = margin(r = 10))

ggsave("Figures/REV/NRCA_EAC5_ALL_Growth.pdf", width = 10, height = 7)

  

########################################################
# GVC's and Industrial Development (Regression Analysis)
########################################################
  

# GVC Instrument following Kummritz (2016) ------------------------------------------------------------

# Using ESCAP Database: https://www.unescap.org/resources/escap-world-bank-trade-cost-database
# -> Theoretically derived trade costs as tariff equivalent (in percent)

ESCAP <- rowbind(
  readxl::read_xlsx("/Users/sebastiankrantz/Documents/Data/ESCAP/20230505-ESCAP-WB-tradecosts-dataset-1995-2010.xlsx", sheet = "GTT"),
  readxl::read_xlsx("/Users/sebastiankrantz/Documents/Data/ESCAP/20230505-ESCAP-WB-tradecosts-dataset-2011-2021.xlsx", sheet = "GTT")) |> 
  transformv(c(reporter, partner), \(x) vswitch(x, c("ZAR", "TMP", "ROM", "MNT"), c("COD", "TLS", "ROU", "MNE"), default = x))
descr(ESCAP)
fndistinct(ESCAP$sector)

# Data is unique but contains mirror flows
ESCAP |> select(reporter, partner, year, sector) |> any_duplicated()

# ESCAP |> subset(reporter %in% EAC5) |> psmat(tij ~ reporter + partner, ~year) |> plot()
ESCAP |> group_by(reporter, partner) |> 
  summarise(cor = cor(tij, year, use = "na.or.complete")) |> 
  with(fmean(abs(cor))) # Could do linear interpretation, but better use locf...

mean_impfun <- function(x) frollmean(x, 3L, align = "center", na.rm = TRUE, hasNA = TRUE) |> na_locf(TRUE) |> na_focb(TRUE)

ESCAP <- ESCAP |>
  roworder(reporter, partner, year) |>
  group_by(reporter, partner) |>
  mutate(across(c(tij, geometric_avg_tariff, nontariff_tij), list(imp = mean_impfun), .names = TRUE)) |>
  ungroup()

# select(ESCAP, tij, geometric_avg_tariff, nontariff_tij) %<>% add_stub("_imp", FALSE)

# Plot:
ESCAP |>
  subset(reporter %in% EAC5 & partner %in% EAC5 & year >= 2000) |>
  pivot(c("reporter", "partner", "year"), c("tij", "tij_imp"), na.rm = TRUE) |>
  mutate(variable = set_attr(variable, "levels", c("Raw", "3-Year MA + LOCF"))) |>
  ggplot(aes(x = year, y = value, colour = partner)) +
    geom_line() +
    facet_grid(variable ~ reporter) +
    labs(colour = "Partner:  ", y = "Tariff Equivalent Total Trade Cost (Percent)", x = NULL) +
    theme_bw() + pretty_plot + rbsc2

# ggsave("Figures/REV/ESCAP_EAC5_Trade_Costs.pdf", width = 10, height = 5)

BACI_BIL_AGG <- qread("/Users/sebastiankrantz/Documents/Data/CEPII BACI 2023/BACI_HS96_V202301/BACI_HS96_2d.qs") |> 
                group_by(iso3_o, iso3_d, year) |> summarise(exports = fsum(value, fill = TRUE))

# Imputing
BACI_BIL_AGG <- expand.grid(iso3_o = unique(BACI_BIL_AGG$iso3_o, sort = TRUE),
                            iso3_d = unique(BACI_BIL_AGG$iso3_d, sort = TRUE),
                            year = unique(BACI_BIL_AGG$year, sort = TRUE)) |> 
                join(BACI_BIL_AGG) |> 
                roworderv(1:3) |> group_by(1:2) |> 
                mutate(exports = mean_impfun(exports)) |> 
                ungroup()

# Trade-Weighted aggregation
ESCAP_REG <- ESCAP |> 
  rename(reporter = iso3_o, partner = iso3_d) |> 
  join(select(EM_CTRY, iso3_o = iso3, region_o = detailed_region_code)) |> 
  join(select(EM_CTRY, iso3_d = iso3, region_d = detailed_region_code)) |> 
  join(BACI_BIL_AGG) |>
  subset(is.finite(exports) & is.finite(tij_imp)) |> 
  collap(tij_imp + geometric_avg_tariff_imp + nontariff_tij_imp ~ region_o + region_d + year, 
         w = ~exports, keep.col.order = FALSE) |> 
  subset(region_o != region_d)

# Inner-EAC Trade Cost Matrix
ESCAP_REG |> 
  subset(region_o %in% EAC5 & region_d %in% EAC5 & between(year, 2010, 2020)) %$%
  table(region_o, region_d, w = tij_imp, wFUN = fmean)

# Panel-Variation
ESCAP_REG |> 
  subset(region_o %in% EAC5 & region_d %in% EAC5) |> 
  qsu(pid = tij_imp ~ region_o + region_d)

# Plot
ESCAP_REG |> 
  subset(region_o %in% EAC5 & region_d %in% EAC5) |> 
  psmat(tij_imp ~ region_o + region_d, ~ year) |> plot(legend = TRUE)

# Now Kummritz Instrument: Exports weighted trade costs excluding bilateral partner
w_mean_excl <- function(x, w) vapply(seq_along(x), function(i) fmean.default(x[-i], w = w[-i]), 1)
  
ESCAP_REG <- ESCAP_REG |>  
  group_by(region_o, year) |> 
  mutate(tij_imp_instr = w_mean_excl(tij_imp, exports)) |> 
  ungroup()

# Instrument for Traditional Trade (Total and Final Goods) -----------------------------------------------

rem_SSD <- function(x) factor(copyv(as.character(x), "SSD", "SSA"), levels = REG)

gen_trade_instrument <- function(source = "ESR", name = "E") {
  list(EORA = EORA_DET, EMERGING = EM_DET) |> 
    lapply(function(X) X$decomps |> 
             lapply(extract2, source) |> value2df()) |> 
    rowbind(idcol = "source") |> 
    # subset(country %iin% EAC5) |> 
    pivot(1:4, names = list("importer", "E")) |> 
    subset(country != importer) |> 
    mutate(importer = rem_SSD(importer)) |> 
    collap(E ~ source + year + country + sector + importer, fsum) |> 
    join(select(ESCAP_REG, region_o, region_d, year, tij_imp), 
         on = c("country" = "region_o", "importer" = "region_d", "year"), 
         how = "inner") |> 
    group_by(source, country, sector, year) |> 
    mutate(tij_imp_instr = w_mean_excl(tij_imp, E)) |> 
    ungroup() |> 
    na_omit(cols = "tij_imp_instr") |> 
    rename(E = name, 
           tij_imp_instr = paste0("tij_imp_instr_", name), 
           .nse = FALSE)
}

# Zero Stage Estimation
TRADE_BIL <- gen_trade_instrument()
mod_E <- feols(log(E+1) ~ log(tij_imp_instr_E) | country^sector + country^year + sector^year, data = TRADE_BIL, split = ~ source)
etable(mod_E)
TRADE_BIL[, E_hat := exp(fitted(feols(log(E+1) ~ log(tij_imp_instr_E) | country^sector + country^year + sector^year, data = .SD))), by = source] # -1: avoid zeros

TRADE_BIL_FD <- gen_trade_instrument("Efd", "Efd")
mod_Efd <- feols(log(Efd+1) ~ log(tij_imp_instr_Efd) | country^sector + country^year + sector^year, data = TRADE_BIL_FD, split = ~ source)
etable(mod_Efd)
TRADE_BIL_FD[, Efd_hat := exp(fitted(feols(log(Efd+1) ~ log(tij_imp_instr_Efd) | country^sector + country^year + sector^year, data = .SD))), by = source] # -1: avoid zeros

# Exporting 
etable(mod_E, mod_Efd)
esttex(mod_E, mod_Efd, digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE)
rm(mod_E, mod_Efd)

# Using BACI
BACI_2d_reg_EAC5 <- qread("/Users/sebastiankrantz/Documents/Data/CEPII BACI 2023/BACI_HS96_V202301/BACI_HS96_2d.qs") |> 
  subset(iso3_o %in% EM_ISO3 | iso3_d %in% EM_ISO3) |> 
  join(select(EM_CTRY, iso3_o = iso3, region_o = detailed_region_code)) |> 
  join(select(EM_CTRY, iso3_d = iso3, region_d = detailed_region_code)) |> 
  mutate(across(c(region_o, region_d), rem_SSD)) |> 
  collap(value ~ year + region_o + region_d + code_2d + product_description, fsum, 
         fill = TRUE, keep.col.order = FALSE) |> 
  subset(!(is.na(region_o) | is.na(region_d)) & region_o != region_d & year >= 2000) # |> 
# subset(region_o %in% EAC5)

# BACI instrument
BACI_2d_reg_EAC5 <- BACI_2d_reg_EAC5 |>
  join(select(ESCAP_REG, region_o, region_d, year, tij_imp), 
       on = c("region_o", "region_d", "year")) |> 
  subset(GRPN(list(year, region_o, code_2d, year)) >= 2L) |> 
  group_by(year, region_o, code_2d, year) |> 
  mutate(tij_imp_instr = w_mean_excl(tij_imp, value)) |> 
  ungroup() |> 
  na_omit(cols = "tij_imp_instr") |> 
  rename(value = E) 

# Zero Stage Estimation
mod <- feols(log(E+1) ~ log(tij_imp_instr) | region_o^code_2d + region_o^year + code_2d^year, data = BACI_2d_reg_EAC5)
etable(mod)
BACI_2d_reg_EAC5[, E_hat := exp(fitted(feols(log(E+1) ~ log(tij_imp_instr) | region_o^code_2d + region_o^year + code_2d^year, data = BACI_2d_reg_EAC5)))] # -1: avoid zeros

# Aggregation BACI
BACI_2d_reg_EAC5_AGG <- BACI_2d_reg_EAC5 |> 
  group_by(year, country = region_o, sector = code_2d) |> 
  summarise(across(c(E, E_hat), fsum),
            tij_imp = fmean(tij_imp, E),
            tij_imp_instr = fmean(tij_imp_instr, E))

# Joining 
TRADE_BIL %<>% join(TRADE_BIL_FD |> select(-tij_imp), how = "full")
rm(TRADE_BIL_FD)

# Aggregation
TRADE_BIL_AGG <- TRADE_BIL |> 
  group_by(source, year, country, sector) |> 
  summarise(across(c(E, Efd, E_hat, Efd_hat), fsum),
            tij_imp = fmean(tij_imp, E),
            tij_imp_instr_E = fmean(tij_imp_instr_E, E), 
            tij_imp_instr_Efd = fmean(tij_imp_instr_Efd, Efd))



# Adding Upstreamness and Downstreamness ---------------------------------------------------------------------------

# # Examine Panel-Variation
# U_DET |> qsu(U ~ source, pid = ~ country + sector) |> aperm()
# D_DET |> qsu(D ~ source, pid = ~ country + sector) |> aperm()
# # -> Substantially more between-variation, but there is some within variation
# # EAC vs ROW
# U_DET |> mutate(EAC = country %in% EAC) |> qsu(U ~  source + EAC, pid = ~ sector) #  + sector
# U_DET |> collap(U ~ source + year + sector) |> qsu(U ~ source, ~sector)
# # 
# U_DET |> collap(U ~ source + country + sector) |> subset(source == "EORA") |> with(fvar(W(U, sector))/fvar(B(U, sector)))
# # Between within sectors
# U_DET |> collap(U ~ source + year + sector) |> subset(source == "EORA") |> with(fvar(W(U, sector))/fvar(B(U, sector)))
# 
# # -> Not much between sector variation...

rem_SSD_IMP <- function(x, imp = TRUE) {
  col <- if(anyv(names(x), "U")) "U" else "D"
  ids <- c("source", "country", "sector", "year")
  x <- x |> 
    mutate(country = rem_SSD(country)) |> 
    collapv(ids, cols = col, w = "E", sort = TRUE, keep.col.order = FALSE) 
  if(!imp) return(x)
  # # Sector-level rolling average to smooth change... only method that has enough signal...
  # x <- transformv(x, col, BY, gv(x, ids[1:3]), mean_impfun)
  # # TODO: use simple average?? (technology in poor countries may be better represented)
  # # -> Kummritz uses simple average across countries and years: I do the same here... lets EAC have more weight
  # # Problem: having bilateral country information on source and using industry might induce endogeneity
  # fmean(x[[col]], gv(x, ids[-2L]), NULL, "fill", set = TRUE) # X$E
  
  # New solution: time invariant, but at the bilateral sector-level
  fmean(x[[col]], gv(x, ids[1:3]), NULL, "fill", set = TRUE)
  x
}

U_ALL_BIL <- select(rem_SSD_IMP(U_DET), -E) |> rename(U = U_source) |> 
  join(select(rem_SSD_IMP(D_DET), -E) |> rename(D = D_using), 
       on = c("source", "year"), 
       suffix = c("_source", "_using"), multiple = TRUE) |> # gvr("source|using|year") |> any_duplicated()
  join(select(rem_SSD_IMP(U_DET), source, country_using = country, sector_using = sector, year, U_using = U)) |> 
  colorder(source, country_source, sector_source, country_using, sector_using, year) |> 
  transformv(U_source:U_using, replace_outliers, c(1, 10), set = TRUE) |> 
  subset(is.finite(U_source) & is.finite(D_using)) |> 
  roworderv(1:6) 
  
# Aggregate Version: Collapsed as in Kummritz (2016)
U_AGG_BIL <- join(
  rem_SSD_IMP(U_DET, FALSE)[source == "EMERGING" | year <= 2015, .(U_source = mean(U, na.rm = TRUE)), by = .(source, sector)],
  rem_SSD_IMP(D_DET, FALSE)[source == "EMERGING" | year <= 2015, .(D_using = mean(D, na.rm = TRUE)), by = .(source, sector)],
  on = "source", suffix = c("_source", "_using"), multiple = TRUE
) |> 
  join(rem_SSD_IMP(U_DET, FALSE)[source == "EMERGING" | year <= 2015, .(U_using = mean(U, na.rm = TRUE)), 
                                 by = .(source, sector_using = sector)]) |> 
  subset(is.finite(U_source) & is.finite(D_using))
  
# Constructing instrument
U_ALL_BIL <- U_ALL_BIL |> 
  join(U_AGG_BIL, on = c("source", "sector_source", "sector_using"), suffix = "_tiv") |> 
  join(select(ESCAP_REG, country_source = region_o, country_using = region_d, year, tij_imp, tij_imp_instr))

# This captures all the cases
U_ALL_BIL %$% any(is.na(tij_imp_instr) & country_source != country_using)

# renaming
setrename(U_ALL_BIL,
          country_source = source_country,
          sector_source = source_sector,
          country_using = using_country, 
          sector_using = using_sector)

table(U_ALL_BIL$source)

# Now adding GVC data (FVAX)
U_ALL_BIL <- U_ALL_BIL |> join(
  list(EORA = EORA_DET, EMERGING = EM_DET) |> 
  lapply(function(X) X$decomps |> 
    lapply(leontief, post = "exports") |> 
    rowbind(idcol = "year")) |> 
  rowbind(idcol = "source") |> 
  rename(tolower) |> rename(sub, pat = "industry", rep = "sector") |> 
  transformv(c(source_country, using_country), rem_SSD) |> 
  group_by(source:using_sector) |> fsum(na.rm = FALSE) |> 
  mutate(year = as.integer(levels(year))[year])
) |> 
  subset(is.finite(tij_imp_instr) & source_country != using_country & is.finite(fvax) & fvax >= 0)

# rm(EORA_DET, EM_DET)
gc()

if(anyNA(U_ALL_BIL)) stop("Missing values")

# Also Adding Traditional Trade Instrument (Sector-Level)
U_ALL_BIL |> varying(tij_imp_instr ~ source + source_country + using_country + year)
U_ALL_BIL %<>% join(TRADE_BIL |> select(source:importer, E, tij_imp_instr_sec = tij_imp_instr_E), 
                    on = c("source", "year", "source_country" = "country", "source_sector" = "sector", "using_country" = "importer")) # |> 
# with(pwcor(tij_imp, tij_imp_sec)) # is 1

# Check correlations
U_ALL_BIL %$% pwcor(tij_imp_instr, tij_imp_instr_sec)
U_ALL_BIL %$% pwcor(log(fvax+1), log(tij_imp_instr))
U_ALL_BIL %$% pwcor(log(fvax+1), log(tij_imp_instr_sec))
# -> Less correlated, first stages are weaker

# U_ALL_BIL %$% fwithin(list(fvax = log(fvax), tij = tij_imp_instr), source_all) %$% cor(fvax, tij)
# U_ALL_BIL %$% fwithin(list(fvax = fvax, tij = tij_imp_instr), list(source_all, using_country, using_sector)) %$% cor(fvax, tij)

# Now constructing the instrument
fastverse_extend(fixest, install = TRUE)
# setFixest_nthreads(4)

predict_fvax <- function(data, stub) {
  fml <- sprintf("log(fvax+1) ~ log(instrument) | %s_country^%s_sector + %s_country^year + %s_sector^year", stub, stub, stub, stub)
  feols(as.formula(fml), data = data, fixef.tol = 1e-7, mem.clean = FALSE) %>% fitted() %>% exp() %-=% 1
}
SDcols <- function(stub) c("fvax", "instrument", paste0(stub, "_country"), paste0(stub, "_sector"), "year")


# Estimation
# # TODO: Better Estimate with subset ??
# U_ALL_BIL %<>% subset(using_country %in% EAC5) # I2E
# U_ALL_BIL %<>% subset(using_country %in% EAC5) # E2R
# # -> Also results in weaker first stages

gc()
U_ALL_BIL[, instrument := tij_imp_instr / (U_source * D_using)]
U_ALL_BIL[is.finite(instrument), i2e_hat := predict_fvax(.SD, "using"), by = source, .SDcols = SDcols("using")]
U_ALL_BIL[is.finite(instrument), e2r_hat := predict_fvax(.SD, "source"), by = source, .SDcols = SDcols("source")]
# U_ALL_BIL[, instrument := tij_imp_instr / (U_source / U_using)] # -> Predictive power inferior to above
# U_ALL_BIL[, i2e_hat_U := predict_fvax(.SD, "using"), by = source, .SDcols = SDcols("using")]
# U_ALL_BIL[, e2r_hat_U := predict_fvax(.SD, "source"), by = source, .SDcols = SDcols("source")]
U_ALL_BIL[, instrument := tij_imp_instr / (U_source_tiv * D_using_tiv)]
U_ALL_BIL[, i2e_hat_tiv := predict_fvax(.SD, "using"), by = source, .SDcols = SDcols("using")]
U_ALL_BIL[, e2r_hat_tiv := predict_fvax(.SD, "source"), by = source, .SDcols = SDcols("source")]
# U_ALL_BIL[, instrument := tij_imp_instr / (U_source_tiv / U_using_tiv)] # -> Predictive power inferior to above
# U_ALL_BIL[, i2e_hat_U_tiv := predict_fvax(.SD, "using"), by = source, .SDcols = SDcols("using")]
# U_ALL_BIL[, e2r_hat_U_tiv := predict_fvax(.SD, "source"), by = source, .SDcols = SDcols("source")]
U_ALL_BIL[, instrument := NULL]
gc()

# Intelligible estimates

est_i2e <- feols(log(fvax+1) ~ log(instrument) | using_country^using_sector + using_country^year + using_sector^year, 
                 data = U_ALL_BIL, split = ~ source, # subset = ~ using_country %in% c("UGA", "TZA", "KEN", "RWA", "BDI"), 
                 fixef.tol = 1e-7, mem.clean = TRUE) 
esttable(est_i2e)

est_e2r <- feols(log(fvax+1) ~ log(instrument) | source_country^source_sector + source_country^year + source_sector^year, 
                 data = U_ALL_BIL, split = ~ source, # subset = ~ source_country %in% c("UGA", "TZA", "KEN", "RWA", "BDI"),
                 fixef.tol = 1e-7, mem.clean = TRUE)
esttable(est_e2r)

esttex(est_i2e, est_e2r, digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE)

# Saving
qsave(U_ALL_BIL, "Data/U_ALL_BIL.qs", compress_level = 5L)
# U_ALL_BIL <- qread("Data/U_ALL_BIL.qs")

# Compute GVC indicators:
GVC_INSTR_DATA <- U_ALL_BIL |>
  group_by(source, using_country, using_sector, year) |>
  gvr("^fvax$|i2e_") |> fsum() |> rename(fvax = i2e) |> rm_stub("using_") |> 
  join(validate = "1:1", how = "full", column = TRUE,
    U_ALL_BIL |>
      group_by(source, source_country, source_sector, year) |>
      gvr("^fvax$|e2r_") |> fsum() |> rename(fvax = e2r) |> rm_stub("source_")
  )

qsave(GVC_INSTR_DATA, "Data/GVC_INSTR_DATA.qs", compress_level = 5L)


# First stages -> Weak IV for EAC, maybe need to run EAC specific regressions
feols(log(i2e) ~ log(i2e_hat) + log(i2e_hat_tiv) | country^sector + country^year + sector^year, 
      data = GVC_INSTR_DATA[source == "EORA" & country %in% EAC5]) |> fitstat( ~ f + wf + wald)
feols(log(e2r) ~ log(e2r_hat) + log(e2r_hat_tiv) | country^sector + country^year + sector^year, 
      data = GVC_INSTR_DATA[source == "EORA" & country %in% EAC5]) |> fitstat( ~ f + wf + wald)
feols(log(i2e) ~ log(i2e_hat) + log(i2e_hat_tiv) | country^sector + country^year + sector^year, 
      data = GVC_INSTR_DATA[source == "EMERGING" & country %in% EAC5]) |> fitstat( ~ f + wf + wald)
feols(log(e2r) ~ log(e2r_hat) + log(e2r_hat_tiv) | country^sector + country^year + sector^year, 
      data = GVC_INSTR_DATA[source == "EMERGING" & country %in% EAC5]) |> fitstat( ~ f + wf + wald)


### Previous Effort: Running estimation by source contry-sector: too detailed, not what Kummritz does...
#
# # Computing industry distances
# settransform(U_ALL_BIL,
# inddist = 1 / (U_source * D_using),
# inddist_U = 1 / (U_source / U_using),
# inddist_tiv = 1 / (U_source_tiv * D_using_tiv),
# inddist_U_tiv = 1 / (U_source_tiv / U_using_tiv),
# source_all = interaction(source, source_country, source_sector, sort = FALSE) # In appearance order
# )
# # Split sample: Can get coefficient table, but seems to remove obs...
# U_ALL_BIL[, instrument := log(inddist * tij_imp_instr)]
# mods <- feols(log(fvax+1) ~ instrument | using_country^using_sector + using_country^year + using_sector^year, 
#               split = ~ source_all, data = U_ALL_BIL, verbose = 0) # lean = TRUE, mem.clean = TRUE)
# mods %<>% get_elem("obs_selection", invert = TRUE) %>% lapply(set_class, "fixest")
# coeftable <- mods |> lapply(coeftable) |> unlist2d("sample", "variable")
# descr(coeftable)
# fitted_vals <- mods |> lapply(fitted) |> rename(sub, pat = "sample.var: source_all; sample: ", rep = "", fixed = TRUE)
# rm(mods); gc()
# fitted_vals %<>% extract(vlengths(fitted_vals) > 0)
# U_ALL_BIL %<>% subset(source_all %in% names(fitted_vals)) %>% droplevels()
# gc()
# if(!identical(levels(U_ALL_BIL$source_all), names(fitted_vals))) stop("name mismatch")
# U_ALL_BIL[, fvax_fit := exp(unlist(fitted_vals, use.names = FALSE)) %-=% 1]
# rm(fitted_vals); gc()
# # All in one go (more efficient and takes along all obs)
# predict_fvax <- function() {
#   feols(log(fvax+1) ~ 0 | source_all[instrument] + source_all^using_country^using_sector + source_all^using_country^year + source_all^using_sector^year,
#         data = U_ALL_BIL, fixef.tol = 1e-7, mem.clean = TRUE) %>% fitted() %>% exp() %-=% 1
# }
# U_ALL_BIL[, instrument := log(tij_imp_instr / (U_source * D_using))]
# U_ALL_BIL[, fvax_hat := predict_fvax()]
# U_ALL_BIL[, instrument := log(tij_imp_instr / (U_source / U_using))]
# U_ALL_BIL[, fvax_hat_U := predict_fvax()]
# U_ALL_BIL[, instrument := log(tij_imp_instr / (U_source_tiv * D_using_tiv))]
# U_ALL_BIL[, fvax_hat_tiv := predict_fvax()]
# # Same as above
# # U_ALL_BIL[, instrument := log(tij_imp_instr / (U_source_tiv / U_using_tiv))]
# # U_ALL_BIL[, fvax_hat_U_tiv := predict_fvax()]
# U_ALL_BIL[, instrument := NULL]
# U_ALL_BIL[, lapply(.SD, cor, fvax), .SDcols = fvax_hat:fvax_hat_tiv] |> raise_to_power(2)
# U_ALL_BIL |> 
#   group_by(source, using_country, using_sector, year) |> 
#   select(fvax, fvax_hat, fvax_hat_U, fvax_hat_tiv) |> 
#   fsum() |> num_vars() |> pwcor() |> raise_to_power(2)
# # -> Too high R^2. Kummritz does not put in source-sector FE and runs two different estimations




# Data Construction ---------------------------------------------------------------


# Checks:
EORA_DET %$% sapply(y, function(i) all.equal(va[, i], decomps[[i]]$X * decomps[[i]]$Vc))

# sapply(y, function(i) all.equal(sbt(VS_df, Year == as.integer(i), i2e)[[1]], 
#                                 unattrib(collapv(leontief(decomps[[i]]), 1:2, fsum, cols = "FVAX")[[3]])))

RM_EORA_SEC <- c("REC", "REI", "FIB", "EGW", "OTH", "PHH")  
EM_MAN <- EM_SEC |> subset(broad_sector_code %in% MAN, code) |> unlist(use.names = FALSE) |> as.integer()

# Now constructing datasets at full sector resolution.   
EORA21_VA <- EORA_DET$decomps |> lapply(with, X * Vc) |> value2df("VA") |> mutate(VA = VA / 1000)

# EORA21_DATA <- fread("/Users/sebastiankrantz/Documents/Data/EORA/GVC_Regions/EORA_GVC_BIL_SEC_BM19.csv") |> 
#   transformv(is.double, `*`, 1/1000) |> 
#   join(select(sec_class, id, from_sector = code), on = c("from_sector" = "id"), drop = "x") |> # from_sector = broad_sector_code for broad sector sample
#   group_by(year, country = from_region, sector = from_sector) |> 
#   num_vars() |> fsum() |> 
#   join(EORA21_VA) |> 
#   colorder(country, sector, VA, pos = "after") |> 
#   transform(I2E = gvcb / gexp, 
#             E2R = gvcf / gexp) |> 
#   # subset(sector %!in% RM_EORA_SEC) |> 
#   droplevels()

# WDR_EORA15_DATA <- WDR_POS |> # Use WDR_POS_SEC for broad sectors
#   join(select(sec_class, id, sector = code), on = c("sect" = "id")) |> 
#   mutate(sect = NULL) |> 
#   subset(year >= 2000) |> 
#   join(EORA21_VA) |> 
#   colorder(country, sector, sector_name = sect_name, VA, pos = "after") |> 
#   transform(I2E = gvcb / gexp, 
#             E2R = gvcf / gexp) |> 
#   # subset(sector %!in% RM_EORA_SEC) |> 
#   droplevels()

EM_VA <- EM_DET$decomps |> lapply(with, X * Vc) |> value2df("VA")

# EM_DATA <- fread("/Users/sebastiankrantz/Documents/Data/EMERGING/GVC_Regions/EM_GVC_BIL_SEC_BM19.csv") |> 
#   # BIL_SEC[source == "EMERGING"] |> 
#   group_by(year, country = from_region, sector = from_sector) |> 
#   select(gexp:gvcf) |> fsum() |> 
#   join(EM_VA) |> 
#   colorder(country, sector, VA, pos = "after") |> 
#   transform(I2E = gvcb / gexp, 
#             E2R = gvcf / gexp)



# Plot Data ----------------------------------

EORA21_DATA[country %in% EAC5] |> with({
  # Histograms: https://stackoverflow.com/questions/3541713/how-to-plot-two-histograms-together-in-r
  oldpar <- par(mfrow = c(1, 3), mar = c(2.5, 4, 2.1, 0), lwd = 0.5) # bottom, left, top, right
  # VA
  VA <- replace_outliers(VA, 1, "clip", single = "min")
  hist(log10(VA), xlab = NA, main = expression('log'[10](VA))) # , breaks = seq(2.5,7.5,0.2), xlim = c(2.5, 7.5))
  hist(log10(VA[sector %in% MAN]), xlab = NA, # , breaks = seq(2.5,7.5,0.2), xlim = c(2.5, 7.5), 
       col = "orange", add = TRUE)
  abline(v = fmedian(log10(VA)), lwd = 1.5)
  abline(v = fmedian(log10(VA[sector %in% MAN])), col = "red", lwd = 1.5)
  legend("topleft", c("Overall Median", "Manufacturing Median"), lty = 1, lwd = 1.5,
         col = c("black", "red"), bty = "n", y.intersp = 2, seg.len = 1)
  # I2E
  I2E <- replace_outliers(I2E, c(0,1))
  hist(I2E, breaks = seq(0, 1, 0.025), xlab = NA, main = expression('Backward GVC Integration (VS/I2E)'))
  # axis(side = 1, at = seq(0, 0.1, 0.10)) # https://stackoverflow.com/questions/25997337/in-r-how-to-set-the-breaks-of-x-axis
  hist(I2E[sector %in% MAN], breaks = seq(0,1,0.025), xlab = NA, xlim = c(0, 0.75), 
       col = "orange", add = TRUE)
  abline(v = fmedian(I2E), lwd = 1.5)
  abline(v = fmedian(I2E[sector %in% MAN]), col = "red", lwd = 1.5)
  # E2R
  E2R <- replace_outliers(E2R, c(0,0.75))
  hist(E2R, breaks = seq(0,0.75,0.025), xlab = NA, main = expression('Forward GVC Integration (VS1/E2R)'))
  hist(E2R[sector %in% MAN], breaks = seq(0,0.75,0.025), xlab = NA, xlim = c(0, 0.75), 
       col = "orange", add = TRUE)
  abline(v = fmedian(E2R), lwd = 1.5)
  abline(v = fmedian(E2R[sector %in% MAN]), col = "red", lwd = 1.5)
  par(oldpar)
})

dev.copy(pdf, "Figures/REV/EORA21_GROWTH_REG_Hists.pdf", width = 10.27, height = 4)
dev.off()


# TS Charts
EORA21_DATA[country %in% EAC5] |> index_by(country, sector, year)  |> with({
  oldpar <- par(mfrow = c(1, 3), mar = c(4.5, 2.5, 2.1, 1.5)) # bottom, left, top, right
  mat <- psmat(log10(VA))
  man_sec <- substr(rownames(mat), 5, 7) %in% MAN
  colour <- ifelse(man_sec, "orange", "grey")
  plot(mat, xlab = "Year", ylab = NA, main = expression('log'[10](VA)), colours = colour)  
  fmedian(mat) %>% lines(as.integer(names(.)), ., lwd = 1.5)
  fmedian(mat[man_sec, ]) %>% lines(as.integer(names(.)), ., col = "red", lwd = 1.5)
  legend("topleft", c("Overall Median", "Manufacturing Median"), lty = 1, lwd = 1.5,
         col = c("black", "red"), bty = "n", y.intersp = 1.5, seg.len = 1)
  mat <- psmat(I2E) |> replace_outliers(c(0, 1))
  plot(mat, xlab = "Year", ylab = NA, main = expression('Backward GVC Integration (VS/I2E)'), colours = colour)  
  fmedian(mat) %>% lines(as.integer(names(.)), ., lwd = 1.5)
  fmedian(mat[man_sec, ]) %>% lines(as.integer(names(.)), ., col = "red", lwd = 1.5)
  mat <- psmat(E2R) |> replace_outliers(c(0, 1))
  plot(mat, xlab = "Year", ylab = NA, main = expression('Forward GVC Integration (VS1/E2R)'), colours = colour)  
  fmedian(mat) %>% lines(as.integer(names(.)), ., lwd = 1.5)
  fmedian(mat[man_sec, ]) %>% lines(as.integer(names(.)), ., col = "red", lwd = 1.5)
  par(oldpar)
  rm(mat, man_sec, colour)
})

dev.copy(pdf, "Figures/REV/EORA21_GROWTH_REG_TS.pdf", width = 10.27, height = 5)
dev.off()




# Join VA to Trade Data -----------------------------------------------------------------------

TRADE_BIL_AGG %<>% join(rowbind(EORA21_VA, EM_VA))

# Need EMERGING VA here...
BACI_2d_reg_EAC5_AGG %<>% mutate(sector = as.integer(sector)) %>% 
  join(EM_VA %>% mutate(sector = as.integer(sector))) %>% 
  subset(is.finite(VA))

# Gross Trade
models_trade <- list(
  full_sample = list(
    OLS_EORA21 = feols(log(VA) ~ log(E) | country^sector + country^year + sector^year, 
                       data = TRADE_BIL_AGG[country %in% EAC5 & source == "EORA"], vcov = DK ~ year),
    IV_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(E) ~ log(E_hat), 
                       data = TRADE_BIL_AGG[country %in% EAC5 & source == "EORA"], vcov = DK ~ year),
    OLS_EORA15 = feols(log(VA) ~ log(E) | country^sector + country^year + sector^year, 
                       data = TRADE_BIL_AGG[country %in% EAC5 & source == "EORA" & year <= 2015], vcov = DK ~ year),
    IV_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(E) ~ log(E_hat), 
                       data = TRADE_BIL_AGG[country %in% EAC5 & source == "EORA" & year <= 2015], vcov = DK ~ year),
    OLS_EM = feols(log(VA) ~ log(E) | country^sector + country^year + sector^year,
                    data = TRADE_BIL_AGG[country %in% EAC5 & source == "EMERGING"], vcov = DK ~ year),
    IV_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(E) ~ log(E_hat),
                   data = TRADE_BIL_AGG[country %in% EAC5 & source == "EMERGING"], vcov = DK ~ year),
    OLS_BACI = feols(log(VA) ~ log(E) | country^sector + country^year + sector^year,
                   data = BACI_2d_reg_EAC5_AGG[country %in% EAC5], vcov = DK ~ year),
    IV_BACI = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(E) ~ log(E_hat),
                  data = BACI_2d_reg_EAC5_AGG[country %in% EAC5], vcov = DK ~ year)
  ),
  manufacturing_sample = list(
    OLS_EORA21 = feols(log(VA) ~ log(E) | country^sector + country^year + sector^year, 
                       data = TRADE_BIL_AGG[country %in% EAC5 & source == "EORA" & sector %in% MAN], vcov = DK ~ year),
    IV_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(E) ~ log(E_hat), 
                      data = TRADE_BIL_AGG[country %in% EAC5 & source == "EORA" & sector %in% MAN], vcov = DK ~ year),
    OLS_EORA15 = feols(log(VA) ~ log(E) | country^sector + country^year + sector^year, 
                       data = TRADE_BIL_AGG[country %in% EAC5 & source == "EORA" & year <= 2015 & sector %in% MAN], vcov = DK ~ year),
    IV_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(E) ~ log(E_hat), 
                      data = TRADE_BIL_AGG[country %in% EAC5 & source == "EORA" & year <= 2015 & sector %in% MAN], vcov = DK ~ year),
    OLS_EM = feols(log(VA) ~ log(E) | country^sector + country^year + sector^year,
                   data = TRADE_BIL_AGG[country %in% EAC5 & source == "EMERGING" & sector %in% EM_MAN], vcov = DK ~ year),
    IV_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(E) ~ log(E_hat),
                  data = TRADE_BIL_AGG[country %in% EAC5 & source == "EMERGING" & sector %in% EM_MAN], vcov = DK ~ year),
    OLS_BACI = feols(log(VA) ~ log(E) | country^sector + country^year + sector^year,
                     data = BACI_2d_reg_EAC5_AGG[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    IV_BACI = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(E) ~ log(E_hat),
                    data = BACI_2d_reg_EAC5_AGG[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year)
  )
)

etable(models_trade$full_sample, 
       headers = names(models_trade$full_sample), # stage = 1,
       fitstat = ~ . + wh.p + kpr + ivwald1.p) #  + sargan.p

etable(models_trade$manufacturing_sample, 
       headers = names(models_trade$manufacturing_sample), 
       fitstat = ~ . + wh.p + kpr + ivwald1.p) #  + sargan.p

# Exporting
esttex(models_trade$full_sample[1:4], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(models_trade$full_sample[1:4]), fitstat = ~ . + wh.p + kpr + ivwald1.p)

esttex(models_trade$manufacturing_sample[1:4], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(models_trade$manufacturing_sample[1:4]), fitstat = ~ . + wh.p + kpr + ivwald1.p)

# First Stages
esttex(models_trade$full_sample[c(2, 4, 6, 8)], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, stage = 1,
       headers = names(models_trade$full_sample[c(2, 4, 6, 8)]), fitstat = ~ . + wh.p + kpr + ivwald1.p)

esttex(models_I2E$manufacturing_sample[c(2, 4, 6, 8)], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, stage = 1,
       headers = names(models_trade$manufacturing_sample[c(2, 4, 6, 8)]), fitstat = ~ . + wh.p + kpr + ivwald1.p)



# Trade in Final Goods
models_fg_trade <- list(
  full_sample = list(
    OLS_EORA21 = feols(log(VA) ~ log(Efd) | country^sector + country^year + sector^year, 
                       data = TRADE_BIL_AGG[country %in% EAC5 & source == "EORA"], vcov = DK ~ year),
    IV_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(Efd) ~ log(Efd_hat), 
                      data = TRADE_BIL_AGG[country %in% EAC5 & source == "EORA"], vcov = DK ~ year),
    OLS_EORA15 = feols(log(VA) ~ log(Efd) | country^sector + country^year + sector^year, 
                       data = TRADE_BIL_AGG[country %in% EAC5 & source == "EORA" & year <= 2015], vcov = DK ~ year),
    IV_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(Efd) ~ log(Efd_hat), 
                      data = TRADE_BIL_AGG[country %in% EAC5 & source == "EORA" & year <= 2015], vcov = DK ~ year),
    OLS_EM = feols(log(VA) ~ log(Efd) | country^sector + country^year + sector^year,
                   data = TRADE_BIL_AGG[country %in% EAC5 & source == "EMERGING"], vcov = DK ~ year),
    IV_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(Efd) ~ log(Efd_hat),
                  data = TRADE_BIL_AGG[country %in% EAC5 & source == "EMERGING"], vcov = DK ~ year)
  ),
  manufacturing_sample = list(
    OLS_EORA21 = feols(log(VA) ~ log(Efd) | country^sector + country^year + sector^year, 
                       data = TRADE_BIL_AGG[country %in% EAC5 & source == "EORA" & sector %in% MAN], vcov = DK ~ year),
    IV_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(Efd) ~ log(Efd_hat), 
                      data = TRADE_BIL_AGG[country %in% EAC5 & source == "EORA" & sector %in% MAN], vcov = DK ~ year),
    OLS_EORA15 = feols(log(VA) ~ log(Efd) | country^sector + country^year + sector^year, 
                       data = TRADE_BIL_AGG[country %in% EAC5 & source == "EORA" & year <= 2015 & sector %in% MAN], vcov = DK ~ year),
    IV_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(Efd) ~ log(Efd_hat), 
                      data = TRADE_BIL_AGG[country %in% EAC5 & source == "EORA" & year <= 2015 & sector %in% MAN], vcov = DK ~ year),
    OLS_EM = feols(log(VA) ~ log(Efd) | country^sector + country^year + sector^year,
                   data = TRADE_BIL_AGG[country %in% EAC5 & source == "EMERGING" & sector %in% EM_MAN], vcov = DK ~ year),
    IV_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(Efd) ~ log(Efd_hat),
                  data = TRADE_BIL_AGG[country %in% EAC5 & source == "EMERGING" & sector %in% EM_MAN], vcov = DK ~ year)
  )
)

etable(models_fg_trade$full_sample, 
       headers = names(models_fg_trade$full_sample), # stage = 1,
       fitstat = ~ . + wh.p + kpr + ivwald1.p) #  + sargan.p

etable(models_fg_trade$manufacturing_sample, 
       headers = names(models_fg_trade$manufacturing_sample), 
       fitstat = ~ . + wh.p + kpr + ivwald1.p) #  + sargan.p

# Exporting
esttex(models_fg_trade$full_sample[1:4], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(models_fg_trade$full_sample[1:4]), fitstat = ~ . + wh.p + kpr + ivwald1.p)

esttex(models_fg_trade$manufacturing_sample[1:4], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(models_fg_trade$manufacturing_sample[1:4]), fitstat = ~ . + wh.p + kpr + ivwald1.p)

# First Stages
esttex(models_fg_trade$full_sample[c(2, 4)], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, stage = 1,
       headers = names(models_fg_trade$full_sample[c(2, 4)]), fitstat = ~ . + wh.p + kpr + ivwald1.p)

esttex(models_fg_trade$manufacturing_sample[c(2, 4)], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, stage = 1,
       headers = names(models_fg_trade$manufacturing_sample[c(2, 4)]), fitstat = ~ . + wh.p + kpr + ivwald1.p)


# Combined Exports:
esttex(c(models_trade$full_sample[1:4], models_fg_trade$full_sample[1:4]), 
       digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(c(models_trade$full_sample[1:4], models_fg_trade$full_sample[1:4])), 
       fitstat = ~ . + wh.p + kpr + ivwald1.p)


esttex(c(models_trade$manufacturing_sample[1:4], models_fg_trade$manufacturing_sample[1:4]), 
       digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(c(models_trade$manufacturing_sample[1:4], models_fg_trade$manufacturing_sample[1:4])), 
       fitstat = ~ . + wh.p + kpr + ivwald1.p)



# Regressions: OLS + Lags ---------------------------------------------------------------

fastverse_extend(fixest, robustbase)

.c(WDR_EORA15_DATA, EORA21_DATA, EM_DATA) %=% lapply(list(WDR_EORA15_DATA, EORA21_DATA, EM_DATA), index_by, country, sector, year)

# Normal FEOLS
feols(log(VA) ~ L(log(gvcb), 0:2) + L(log(gvcf), 0:2) | year^country + year^sector + country^sector, 
      data = WDR_EORA15_DATA[country %in% EAC5])

feols(Dlog(VA) ~ L(Dlog(gvcb), 0:2) + L(Dlog(gvcf), 0:2), # | year^country + year^sector,
      data = EORA21_DATA[country %in% EAC5])

feols(log(VA) ~ L(log(I2E), 0:2) + L(log(E2R), 0:2) | year^country + year^sector + country^sector, 
      data = EORA21_DATA[country %in% EAC5])

feols(Dlog(VA) ~ L(Dlog(I2E), 0:2) + L(Dlog(E2R), 0:2), #  | country^sector 
      data = EORA21_DATA[country %in% EAC5])

# Robust Difference Estimates

lmrob(VA ~ gvcb + L1.gvcb + L2.gvcb + gvcf + L1.gvcf + L2.gvcf, #  | country^sector # TODO: 
      data = EORA21_DATA[country %in% EAC5, .(VA, gvcb, gvcf)] |> fdiff(log = TRUE) |> L(0:2), setting = "KS2014", k.max = 10000)

lmrob(VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, #  | country^sector 
      data = EORA21_DATA[country %in% EAC5, .(VA, I2E, E2R)] |> fdiff(log = TRUE) |> L(0:2) |> replace_inf(), setting = "KS2014", k.max = 10000)

# Now EMERGING

# No Dynamics

feols(log(VA) ~ log(gvcb) + log(gvcf) | year^country + year^sector + country^sector, 
      data = EM_DATA[country %in% EAC5])

feols(Dlog(VA) ~ Dlog(gvcb) + Dlog(gvcf), 
      data = EM_DATA[country %in% EAC5])

feols(log(VA) ~ log(I2E) + log(E2R) | year^country + year^sector + country^sector, 
      data = EM_DATA[country %in% EAC5])

feols(Dlog(VA) ~ Dlog(I2E) + Dlog(E2R), 
      data = EM_DATA[country %in% EAC5])


# With Dynamics
  
feols(log(VA) ~ L(log(gvcb), 0:2) + L(log(gvcf), 0:2) | year^country + year^sector + country^sector, 
      data = EM_DATA[country %in% EAC5])

feols(Dlog(VA) ~ L(Dlog(gvcb), 0:2) + L(Dlog(gvcf), 0:2), 
      data = EM_DATA[country %in% EAC5])

feols(log(VA) ~ L(log(I2E), 0:2) + L(log(E2R), 0:2) | year^country + year^sector + country^sector, 
      data = EM_DATA[country %in% EAC5])

feols(Dlog(VA) ~ L(Dlog(I2E), 0:2) + L(Dlog(E2R), 0:2), 
      data = EM_DATA[country %in% EAC5])
  


# Robust Difference Estimates

lmrob(VA ~ gvcb + L1.gvcb + L2.gvcb + gvcf + L1.gvcf + L2.gvcf, setting = "KS2014", k.max = 10000,
      data = EM_DATA[country %in% EAC5, L(fdiff(list(VA = VA, gvcb = gvcb, gvcf = gvcf), log = TRUE), 0:2)] |> replace_inf())

lmrob(VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, setting = "KS2014", k.max = 10000, 
      data = EM_DATA[country %in% EAC5, L(fdiff(list(VA = VA, I2E = I2E, E2R = E2R), log = TRUE), 0:2)] |> replace_inf())





# Regressions: IV ---------------------------------------------------------------
fastverse_extend(fixest)

GVC_INSTR_DATA <- qread("Data/GVC_INSTR_DATA.qs") |> 
  join(mutate(EORA21_VA, source = "EORA"), on = c("source", "country", "sector", "year")) |> 
  join(mutate(EM_VA, source = "EMERGING"), on = c("source", "country", "sector", "year")) |> 
  transform(VA = pfirst(VA, VA_y), VA_y = NULL, .join = NA)

# EORA21_DATA %<>% join(subset(GVC_INSTR_DATA, source == "EORA", -source, -.join), on = c("country", "sector", "year"), how = "full", column = TRUE) %>%
#                 transform(VA = pfirst(VA, VA_y), VA_y = NULL)
# WDR_EORA15_DATA %<>% join(subset(GVC_INSTR_DATA, source == "EORA", -source, -.join), on = c("country", "sector", "year"), how = "full", column = TRUE) %>%
#                      transform(VA = pfirst(VA, VA_y), VA_y = NULL) %>% subset(year <= 2015)
# EM_DATA %<>% join(subset(GVC_INSTR_DATA, source == "EMERGING", -source, -.join) |> 
#                    mutate(sector = as.integer(levels(sector))[sector]), 
#                  on = c("year", "country", "sector"), how = "full", column = TRUE) %>%  # , how = "full", column = TRUE |> View()
#              transform(VA = pfirst(VA, VA_y), VA_y = NULL)
# # EORA21_DATA |> num_vars() |> pwcor()
# EORA21_DATA[country %in% EAC5, pwcor(gvcf, e2r)]

EORA21_DATA <- GVC_INSTR_DATA[source == "EORA"]
WDR_EORA15_DATA <- GVC_INSTR_DATA[source == "EORA" & year <= 2015]
EM_DATA <- GVC_INSTR_DATA[source == "EMERGING"]

# First stages -> Weak IV for EAC, maybe need to run EAC specific regressions
feols(log(i2e) ~ log(i2e_hat) + log(i2e_hat_tiv) | country^sector + country^year + sector^year, 
      data = EORA21_DATA[country %in% EAC5])
feols(log(e2r) ~ log(e2r_hat) + log(e2r_hat_tiv) | country^sector + country^year + sector^year, 
      data = EORA21_DATA[country %in% EAC5])

feols(log(gvcb) ~ log(i2e_hat) + log(i2e_hat_tiv) | country^sector + country^year + sector^year, 
      data = WDR_EORA15_DATA[country %in% EAC5])
feols(log(gvcf) ~ log(e2r_hat) + log(e2r_hat_tiv) | country^sector + country^year + sector^year, 
      data = WDR_EORA15_DATA[country %in% EAC5])

feols(log(i2e) ~ log(i2e_hat) + log(i2e_hat_tiv) | country^sector + country^year + sector^year, 
      data = EM_DATA[country %in% EAC5])


# FULL 2SLS Results --------------------------------------------------------------------------------------------------

# No Dynamics: Backward GVC Integration
models_I2E <- list(
  full_sample = list(
    OLS_I2E_EORA21 = feols(log(VA) ~ log(i2e) | country^sector + country^year + sector^year, 
                          data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_K_I2E_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ log(i2e_hat_tiv), 
                            data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_CS_I2E_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ log(i2e_hat), 
                             data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_B_I2E_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ log(i2e_hat) + log(i2e_hat_tiv), 
                            data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    OLS_I2E_EORA15 = feols(log(VA) ~ log(i2e) | country^sector + country^year + sector^year, 
                           data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_K_I2E_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ log(i2e_hat_tiv), 
                            data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_CS_I2E_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ log(i2e_hat), 
                            data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_B_I2E_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ log(i2e_hat) + log(i2e_hat_tiv), 
                            data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    OLS_I2E_EM = feols(log(VA) ~ log(i2e) | country^sector + country^year + sector^year,
                         data = EM_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_K_I2E_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ log(i2e_hat_tiv),
                          data = EM_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_CS_I2E_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ log(i2e_hat),
                          data = EM_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_B_I2E_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ log(i2e_hat) + log(i2e_hat_tiv),
                          data = EM_DATA[country %in% EAC5], vcov = DK ~ year)
  ),
  manufacturing_sample = list(
    OLS_I2E_EORA21 = feols(log(VA) ~ log(i2e) | country^sector + country^year + sector^year, 
                           data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_K_I2E_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ log(i2e_hat_tiv), 
                            data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_CS_I2E_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ log(i2e_hat), 
                            data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_B_I2E_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ log(i2e_hat) + log(i2e_hat_tiv), 
                            data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    OLS_I2E_EORA15 = feols(log(VA) ~ log(i2e) | country^sector + country^year + sector^year, 
                           data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_K_I2E_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ log(i2e_hat_tiv), 
                            data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_CS_I2E_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ log(i2e_hat), 
                            data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_B_I2E_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ log(i2e_hat) + log(i2e_hat_tiv), 
                            data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    OLS_I2E_EM = feols(log(VA) ~ log(i2e) | country^sector + country^year + sector^year,
                       data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    IV_K_I2E_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ log(i2e_hat_tiv),
                        data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    IV_CS_I2E_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ log(i2e_hat),
                        data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    IV_B_I2E_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ log(i2e_hat) + log(i2e_hat_tiv),
                        data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year)
  )
)

etable(models_I2E$full_sample[1:8], 
       headers = names(models_I2E$full_sample[1:8]), # stage = 1,
       fitstat = ~ . + wh.p + kpr + ivwald1.p) #  + sargan.p

etable(models_I2E$manufacturing_sample[1:8], 
       headers = names(models_I2E$manufacturing_sample[1:8]), 
       fitstat = ~ . + wh.p + kpr + ivwald1.p) #  + sargan.p

# Exporting
esttex(models_I2E$full_sample[1:8], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(models_I2E$full_sample[1:8]), fitstat = ~ . + wh.p + kpr + ivwald1.p)

esttex(models_I2E$manufacturing_sample[1:8], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(models_I2E$manufacturing_sample[1:8]), fitstat = ~ . + wh.p + kpr + ivwald1.p)

# First Stages
esttex(models_I2E$full_sample[c(2:4, 6:8)], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, stage = 1,
       headers = names(models_I2E$full_sample[c(2:4, 6:8)]), fitstat = ~ . + wh.p + kpr + ivwald1.p)

esttex(models_I2E$manufacturing_sample[c(2:4, 6:8)], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, stage = 1,
       headers = names(models_I2E$manufacturing_sample[c(2:4, 6:8)]), fitstat = ~ . + wh.p + kpr + ivwald1.p)



# No Dynamics: Forward GVC Integration
models_E2R <- list(
  full_sample = list(
    OLS_E2R_EORA21 = feols(log(VA) ~ log(e2r) | country^sector + country^year + sector^year, 
                           data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_K_E2R_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ log(e2r_hat_tiv), 
                            data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_CS_E2R_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ log(e2r_hat), 
                             data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_B_E2R_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ log(e2r_hat) + log(e2r_hat_tiv), 
                            data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    OLS_E2R_EORA15 = feols(log(VA) ~ log(e2r) | country^sector + country^year + sector^year, 
                           data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_K_E2R_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ log(e2r_hat_tiv), 
                            data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_CS_E2R_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ log(e2r_hat), 
                             data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_B_E2R_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ log(e2r_hat) + log(e2r_hat_tiv), 
                            data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    OLS_E2R_EM = feols(log(VA) ~ log(e2r) | country^sector + country^year + sector^year,
                       data = EM_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_K_E2R_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ log(e2r_hat_tiv),
                        data = EM_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_CS_E2R_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ log(e2r_hat),
                         data = EM_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_B_E2R_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ log(e2r_hat) + log(e2r_hat_tiv),
                        data = EM_DATA[country %in% EAC5], vcov = DK ~ year)
  ),
  manufacturing_sample = list(
    OLS_E2R_EORA21 = feols(log(VA) ~ log(e2r) | country^sector + country^year + sector^year, 
                           data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_K_E2R_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ log(e2r_hat_tiv), 
                            data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_CS_E2R_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ log(e2r_hat), 
                             data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_B_E2R_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ log(e2r_hat) + log(e2r_hat_tiv), 
                            data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    OLS_E2R_EORA15 = feols(log(VA) ~ log(e2r) | country^sector + country^year + sector^year, 
                           data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_K_E2R_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ log(e2r_hat_tiv), 
                            data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_CS_E2R_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ log(e2r_hat), 
                             data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_B_E2R_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ log(e2r_hat) + log(e2r_hat_tiv), 
                            data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    OLS_E2R_EM = feols(log(VA) ~ log(e2r) | country^sector + country^year + sector^year,
                       data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    IV_K_E2R_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ log(e2r_hat_tiv),
                        data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    IV_CS_E2R_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ log(e2r_hat),
                         data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    IV_B_E2R_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ log(e2r_hat) + log(e2r_hat_tiv),
                        data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year)
  )
)

etable(models_E2R$full_sample[1:8], headers = names(models_E2R$full_sample[1:8]), 
       fitstat = ~ . + wh.p + kpr + ivwald1.p) #  + sargan.p
etable(models_E2R$manufacturing_sample[1:8], headers = names(models_E2R$manufacturing_sample[1:8]), 
       fitstat = ~ . + wh.p + kpr + ivwald1.p) #  + sargan.p

# Exporting
esttex(models_E2R$full_sample[1:8], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(models_E2R$full_sample[1:8]),
       fitstat = ~ . + wh.p + kpr + ivwald1.p)

esttex(models_E2R$manufacturing_sample[1:8], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(models_E2R$manufacturing_sample[1:8]),
       fitstat = ~ . + wh.p + kpr + ivwald1.p)


# First Stages
esttex(models_E2R$full_sample[c(2:4, 6:8)], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, stage = 1,
       headers = names(models_E2R$full_sample[c(2:4, 6:8)]), fitstat = ~ . + wh.p + kpr + ivwald1.p)

esttex(models_E2R$manufacturing_sample[c(2:4, 6:8)], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, stage = 1,
       headers = names(models_E2R$manufacturing_sample[c(2:4, 6:8)]), fitstat = ~ . + wh.p + kpr + ivwald1.p)





# Regional Integration -----------------------------------------------------

# Gross Trade ---------------------------------------------

# Aggregation
TRADE_RI_BIL_AGG <- TRADE_BIL |> 
  subset(importer %in% EAC5) |> 
  group_by(source, year, country, sector) |> 
  summarise(across(c(E, Efd, E_hat, Efd_hat), list(reg = fsum), .names = TRUE)) |> 
  join(TRADE_BIL_AGG) %>% 
  transform(select(., E_reg:Efd_hat_reg) %c/% select(., E:Efd_hat) %>% add_stub("_sh", FALSE))

# Estimations
RI_model_est <- function(data = TRADE_RI_BIL_AGG,
                         form_OLS, form_IV) {
  list(
    full_sample = list(
      OLS_EORA21 = feols(form_OLS, data = data[country %in% EAC5 & source == "EORA"], vcov = DK ~ year),
      IV_EORA21 = feols(form_IV, data = data[country %in% EAC5 & source == "EORA"], vcov = DK ~ year),
      OLS_EORA15 = feols(form_OLS, data = data[country %in% EAC5 & source == "EORA" & year <= 2015], vcov = DK ~ year),
      IV_EORA15 = feols(form_IV, data = data[country %in% EAC5 & source == "EORA" & year <= 2015], vcov = DK ~ year),
      OLS_EM = feols(form_OLS, data = data[country %in% EAC5 & source == "EMERGING"], vcov = DK ~ year),
      IV_EM = feols(form_IV, data = data[country %in% EAC5 & source == "EMERGING"], vcov = DK ~ year)
    ),
    manufacturing_sample = list(
      OLS_EORA21 = feols(form_OLS, data = data[country %in% EAC5 & source == "EORA" & sector %in% MAN], vcov = DK ~ year),
      IV_EORA21 = feols(form_IV, data = data[country %in% EAC5 & source == "EORA" & sector %in% MAN], vcov = DK ~ year),
      OLS_EORA15 = feols(form_OLS, data = data[country %in% EAC5 & source == "EORA" & year <= 2015 & sector %in% MAN], vcov = DK ~ year),
      IV_EORA15 = feols(form_IV, data = data[country %in% EAC5 & source == "EORA" & year <= 2015 & sector %in% MAN], vcov = DK ~ year),
      OLS_EM = feols(form_OLS, data = data[country %in% EAC5 & source == "EMERGING" & sector %in% EM_MAN], vcov = DK ~ year),
      IV_EM = feols(form_IV, data = data[country %in% EAC5 & source == "EMERGING" & sector %in% EM_MAN], vcov = DK ~ year)
    )
  )
}


# Grosss Trade ---------
models_reg_trade <- RI_model_est(form_OLS = log(VA) ~ log(E) + log(E_reg) | country^sector + country^year + sector^year,
                                 form_IV = log(VA) ~ 0 | country^sector + country^year + sector^year | log(E) + log(E_reg)  ~ log(E_hat) + log(E_hat_reg))
# Shares
models_reg_trade <- RI_model_est(form_OLS = log(VA) ~ log(E) * E_reg_sh | country^sector + country^year + sector^year,
                                 form_IV = log(VA) ~ 0 | country^sector + country^year + sector^year | log(E) * E_reg_sh  ~ log(E_hat) * E_hat_reg_sh)
# Shares Reduced: Better (more sensible)
models_reg_trade <- RI_model_est(form_OLS = log(VA) ~ log(E) + log(E):E_reg_sh | country^sector + country^year + sector^year,
                                 form_IV = log(VA) ~ 0 | country^sector + country^year + sector^year | log(E) + log(E):E_reg_sh  ~ log(E_hat) + log(E_hat):E_hat_reg_sh) # log(E_hat) * E_hat_reg_sh

# Final Goods Trade ---------
models_reg_fg_trade <- RI_model_est(form_OLS = log(VA) ~ log(Efd) + log(Efd_reg) | country^sector + country^year + sector^year,
                                 form_IV = log(VA) ~ 0 | country^sector + country^year + sector^year | log(Efd) + log(Efd_reg)  ~ log(Efd_hat) + log(Efd_hat_reg))
# Shares
models_reg_fg_trade <- RI_model_est(form_OLS = log(VA) ~ log(Efd) * Efd_reg_sh | country^sector + country^year + sector^year,
                                 form_IV = log(VA) ~ 0 | country^sector + country^year + sector^year | log(Efd) * Efd_reg_sh  ~ log(Efd_hat) * Efd_hat_reg_sh)
# Shares Reduced: Better (more sensible)
models_reg_fg_trade <- RI_model_est(form_OLS = log(VA) ~ log(Efd) + log(Efd):Efd_reg_sh | country^sector + country^year + sector^year,
                                 form_IV = log(VA) ~ 0 | country^sector + country^year + sector^year | log(Efd) + log(Efd):Efd_reg_sh  ~ log(Efd_hat) + log(Efd_hat):Efd_hat_reg_sh) # log(Efd_hat) * Efd_hat_reg_sh

# Results
etable(c(models_reg_trade$full_sample[1:4], models_reg_fg_trade$full_sample[1:4]), 
       headers = names(c(models_reg_trade$full_sample[1:4], models_reg_fg_trade$full_sample[1:4])), # stage = 1,
       fitstat = ~ . + wh.p + ivwald1.p) #  + kpr + sargan.p

etable(c(models_reg_trade$manufacturing_sample[1:4], models_reg_fg_trade$manufacturing_sample[1:4]), 
       headers = names(c(models_reg_trade$manufacturing_sample[1:4], models_reg_fg_trade$manufacturing_sample[1:4])), 
       fitstat = ~ . + wh.p + ivwald1.p) #  + sargan.p

# Exporting
esttex(c(models_reg_trade$full_sample[1:4], models_reg_fg_trade$full_sample[1:4]), 
       digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(c(models_reg_trade$full_sample[1:4], models_reg_fg_trade$full_sample[1:4])), 
       fitstat = ~ . + wh.p + ivwald1.p) # + kpr


esttex(c(models_reg_trade$manufacturing_sample[1:4], models_reg_fg_trade$manufacturing_sample[1:4]), 
       digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(c(models_reg_trade$manufacturing_sample[1:4], models_reg_fg_trade$manufacturing_sample[1:4])), 
       fitstat = ~ . + wh.p + ivwald1.p) # + kpr


# # First Stages
# esttex(models_reg_trade$full_sample[c(2, 4, 6, 8)], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, stage = 1,
#        headers = names(models_reg_trade$full_sample[c(2, 4, 6, 8)]), fitstat = ~ . + wh.p + kpr + ivwald1.p)
# 
# esttex(models_reg_trade$manufacturing_sample[c(2, 4, 6, 8)], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, stage = 1,
#        headers = names(models_reg_trade$manufacturing_sample[c(2, 4, 6, 8)]), fitstat = ~ . + wh.p + kpr + ivwald1.p)
# 


# GVC Related Trade ---------------------------------------------

GVC_RI_INSTR_DATA <- U_ALL_BIL |>
  subset(source_country %in% EAC5) |> 
  group_by(source, using_country, using_sector, year) |>
  gvr("^fvax$|i2e_hat") |> add_stub("_reg", FALSE) |> fsum() |> 
  rename(fvax_reg = i2e_reg) |> rm_stub("using_") |> 
  join(validate = "1:1", how = "full", 
       U_ALL_BIL |>
         subset(using_country %in% EAC5) |> 
         group_by(source, source_country, source_sector, year) |>
         gvr("^fvax$|e2r_hat") |> add_stub("_reg", FALSE) |> fsum() |> 
         rename(fvax_reg = e2r_reg) |> rm_stub("source_")
  ) |> 
  join(mutate(GVC_INSTR_DATA, .join = NULL)) %>%
  transform(gvr(., "^i2e") %c+% gvr(., "^e2r") %>% rename(substr, 4, 100) %>% add_stub("gvc")) %>%
  transform(gvr(., "_reg") %c/% gv(., sub("_reg", "", gvr(., "_reg", return = "names"))) %>% add_stub("_sh", FALSE))


# I2E
models_reg_I2E <- RI_model_est(data = GVC_RI_INSTR_DATA,
                               form_OLS = log(VA) ~ log(i2e) + log(i2e_reg) | country^sector + country^year + sector^year,
                               form_IV = log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) + log(i2e_reg)  ~ log(i2e_hat) + log(i2e_hat_reg))
# Shares
models_reg_I2E <- RI_model_est(data = GVC_RI_INSTR_DATA,
                               form_OLS = log(VA) ~ log(i2e) * i2e_reg_sh | country^sector + country^year + sector^year,
                               form_IV = log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) * i2e_reg_sh  ~ log(i2e_hat) * i2e_hat_reg_sh)
# Shares Reduced: Better (more sensible)
models_reg_I2E <- RI_model_est(data = GVC_RI_INSTR_DATA,
                               form_OLS = log(VA) ~ log(i2e) + log(i2e):i2e_reg_sh | country^sector + country^year + sector^year,
                               form_IV = log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) + log(i2e):i2e_reg_sh  ~ log(i2e_hat) + log(i2e_hat):i2e_hat_reg_sh) # log(i2e_hat) * i2e_hat_reg_sh

# E2R
models_reg_E2R <- RI_model_est(data = GVC_RI_INSTR_DATA,
                               form_OLS = log(VA) ~ log(e2r) + log(e2r_reg) | country^sector + country^year + sector^year,
                               form_IV = log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) + log(e2r_reg)  ~ log(e2r_hat) + log(e2r_hat_reg))
# Shares
models_reg_E2R <- RI_model_est(data = GVC_RI_INSTR_DATA,
                               form_OLS = log(VA) ~ log(e2r) * e2r_reg_sh | country^sector + country^year + sector^year,
                               form_IV = log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) * e2r_reg_sh  ~ log(e2r_hat) * e2r_hat_reg_sh)
# Shares Reduced: Better (more sensible)
models_reg_E2R <- RI_model_est(data = GVC_RI_INSTR_DATA,
                               form_OLS = log(VA) ~ log(e2r) + log(e2r):e2r_reg_sh | country^sector + country^year + sector^year,
                               form_IV = log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) + log(e2r):e2r_reg_sh  ~ log(e2r_hat) + log(e2r_hat):e2r_hat_reg_sh) # log(e2r_hat) * e2r_hat_reg_sh

# GVC
models_reg_GVC <- RI_model_est(data = GVC_RI_INSTR_DATA,
                               form_OLS = log(VA) ~ log(gvc) + log(gvc_reg) | country^sector + country^year + sector^year,
                               form_IV = log(VA) ~ 0 | country^sector + country^year + sector^year | log(gvc) + log(gvc_reg)  ~ log(gvc_hat) + log(gvc_hat_reg))
# Shares
models_reg_GVC <- RI_model_est(data = GVC_RI_INSTR_DATA,
                               form_OLS = log(VA) ~ log(gvc) * gvc_reg_sh | country^sector + country^year + sector^year,
                               form_IV = log(VA) ~ 0 | country^sector + country^year + sector^year | log(gvc) * gvc_reg_sh  ~ log(gvc_hat) * gvc_hat_reg_sh)
# Shares Reduced: Better (more sensible)
models_reg_GVC <- RI_model_est(data = GVC_RI_INSTR_DATA,
                               form_OLS = log(VA) ~ log(gvc) + log(gvc):gvc_reg_sh | country^sector + country^year + sector^year,
                               form_IV = log(VA) ~ 0 | country^sector + country^year + sector^year | log(gvc) + log(gvc):gvc_reg_sh  ~ log(gvc_hat) + log(gvc_hat):gvc_hat_reg_sh) # log(gvc_hat) * gvc_hat_reg_sh



# Results
etable(c(models_reg_I2E$full_sample[1:4], models_reg_E2R$full_sample[1:4]), 
       headers = names(c(models_reg_I2E$full_sample[1:4], models_reg_E2R$full_sample[1:4])), # stage = 1,
       fitstat = ~ . + wh.p + ivwald1.p) #  + kpr + sargan.p

etable(c(models_reg_I2E$manufacturing_sample[1:4], models_reg_E2R$manufacturing_sample[1:4]), 
       headers = names(c(models_reg_I2E$manufacturing_sample[1:4], models_reg_E2R$manufacturing_sample[1:4])), 
       fitstat = ~ . + wh.p + ivwald1.p) #  + sargan.p

# Exporting
esttex(c(models_reg_I2E$full_sample[1:4], models_reg_E2R$full_sample[1:4]), 
       digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(c(models_reg_I2E$full_sample[1:4], models_reg_E2R$full_sample[1:4])), 
       fitstat = ~ . + wh.p + ivwald1.p) # + kpr

esttex(c(models_reg_I2E$manufacturing_sample[1:4], models_reg_E2R$manufacturing_sample[1:4]), 
       digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(c(models_reg_I2E$manufacturing_sample[1:4], models_reg_E2R$manufacturing_sample[1:4])), 
       fitstat = ~ . + wh.p + ivwald1.p) # + kpr









###########################
# Rather Experimental Stuff
###########################

# TODO: Dnyamic instrument regression with separate dynamic first and final stages



# Dynamic Instrument Models ------------------------------
# -> Experimental to see if first stage fit can be increased, but does not really work...
# Literature suggests LIML works better fith weak IV's, but where do I get a FE LIML estimator??

.c(WDR_EORA15_DATA, EORA21_DATA, EM_DATA) %=% lapply(list(WDR_EORA15_DATA, EORA21_DATA, EM_DATA), index_by, country, sector, year)

# Backward GVC Integration
models_I2E_dynIV <- list(
  full_sample = list(
    OLS_I2E_EORA21 = feols(log(VA) ~ log(i2e) | country^sector + country^year + sector^year, 
                           data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_K_I2E_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ L(log(i2e_hat_tiv), 0:1), 
                            data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_CS_I2E_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ L(log(i2e_hat), 0:1), 
                             data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_B_I2E_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ L(log(i2e_hat), 0:1) + L(log(i2e_hat_tiv), 0:1), 
                            data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    OLS_I2E_EORA15 = feols(log(VA) ~ log(i2e) | country^sector + country^year + sector^year, 
                           data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_K_I2E_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ L(log(i2e_hat_tiv), 0:1), 
                            data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_CS_I2E_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ L(log(i2e_hat), 0:1), 
                             data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_B_I2E_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ L(log(i2e_hat), 0:1) + L(log(i2e_hat_tiv), 0:1), 
                            data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year) # ,
    # OLS_I2E_EM = feols(log(VA) ~ log(i2e) | country^sector + country^year + sector^year,
    #                    data = EM_DATA[country %in% EAC5], vcov = DK ~ year),
    # IV_K_I2E_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ L(log(i2e_hat_tiv), 0:1),
    #                     data = EM_DATA[country %in% EAC5], vcov = DK ~ year),
    # IV_CS_I2E_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ L(log(i2e_hat), 0:1),
    #                      data = EM_DATA[country %in% EAC5], vcov = DK ~ year),
    # IV_B_I2E_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ L(log(i2e_hat), 0:1) + L(log(i2e_hat_tiv), 0:1),
    #                     data = EM_DATA[country %in% EAC5], vcov = DK ~ year)
  ),
  manufacturing_sample = list(
    OLS_I2E_EORA21 = feols(log(VA) ~ log(i2e) | country^sector + country^year + sector^year, 
                           data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_K_I2E_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ L(log(i2e_hat_tiv), 0:1), 
                            data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_CS_I2E_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ L(log(i2e_hat), 0:1), 
                             data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_B_I2E_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ L(log(i2e_hat), 0:1) + L(log(i2e_hat_tiv), 0:1), 
                            data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    OLS_I2E_EORA15 = feols(log(VA) ~ log(i2e) | country^sector + country^year + sector^year, 
                           data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_K_I2E_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ L(log(i2e_hat_tiv), 0:1), 
                            data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_CS_I2E_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ L(log(i2e_hat), 0:1), 
                             data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_B_I2E_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ L(log(i2e_hat), 0:1) + L(log(i2e_hat_tiv), 0:1), 
                            data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year) #,
    # OLS_I2E_EM = feols(log(VA) ~ log(i2e) | country^sector + country^year + sector^year,
    #                    data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    # IV_K_I2E_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ L(log(i2e_hat_tiv), 0:1),
    #                     data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    # IV_CS_I2E_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ L(log(i2e_hat), 0:1),
    #                      data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    # IV_B_I2E_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(i2e) ~ L(log(i2e_hat), 0:1) + L(log(i2e_hat_tiv), 0:1),
    #                     data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year)
  )
)

etable(models_I2E_dynIV$full_sample[1:8], headers = names(models_I2E_dynIV$full_sample[1:8]), # stage = 1,
       fitstat = ~ . + wh.p + kpr + ivwald1.p)#  + sargan.p
etable(models_I2E_dynIV$manufacturing_sample[1:8], headers = names(models_I2E_dynIV$manufacturing_sample[1:8]), 
       fitstat = ~ . + wh.p + kpr + ivwald1.p) #  + sargan.p

# Exporting
esttex(models_I2E_dynIV$full_sample[1:8], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(models_I2E_dynIV$full_sample[1:8]), fitstat = ~ . + wh.p + kpr + ivwald1.p)

esttex(models_I2E_dynIV$manufacturing_sample[1:8], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(models_I2E_dynIV$manufacturing_sample[1:8]), fitstat = ~ . + wh.p + kpr + ivwald1.p)

# First Stages
esttex(models_I2E_dynIV$full_sample[c(2:4, 6:8)], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, stage = 1,
       headers = names(models_I2E_dynIV$full_sample[c(2:4, 6:8)]), fitstat = ~ . + wh.p + kpr + ivwald1.p)

esttex(models_I2E_dynIV$manufacturing_sample[c(2:4, 6:8)], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, stage = 1,
       headers = names(models_I2E_dynIV$manufacturing_sample[c(2:4, 6:8)]), fitstat = ~ . + wh.p + kpr + ivwald1.p)


# Forward GVC Integration
models_E2R_dynIV <- list(
  full_sample = list(
    OLS_E2R_EORA21 = feols(log(VA) ~ log(e2r) | country^sector + country^year + sector^year, 
                           data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_K_E2R_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ L(log(e2r_hat_tiv), 0:1), 
                            data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_CS_E2R_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ L(log(e2r_hat), 0:1), 
                             data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_B_E2R_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ L(log(e2r_hat), 0:1) + L(log(e2r_hat_tiv), 0:1), 
                            data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    OLS_E2R_EORA15 = feols(log(VA) ~ log(e2r) | country^sector + country^year + sector^year, 
                           data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_K_E2R_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ L(log(e2r_hat_tiv), 0:1), 
                            data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_CS_E2R_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ L(log(e2r_hat), 0:1), 
                             data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_B_E2R_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ L(log(e2r_hat), 0:1) + L(log(e2r_hat_tiv), 0:1), 
                            data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year) #,
    # OLS_E2R_EM = feols(log(VA) ~ log(e2r) | country^sector + country^year + sector^year,
    #                    data = EM_DATA[country %in% EAC5], vcov = DK ~ year),
    # IV_K_E2R_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ L(log(e2r_hat_tiv), 0:1),
    #                     data = EM_DATA[country %in% EAC5], vcov = DK ~ year),
    # IV_CS_E2R_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ L(log(e2r_hat), 0:1),
    #                      data = EM_DATA[country %in% EAC5], vcov = DK ~ year),
    # IV_B_E2R_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ L(log(e2r_hat), 0:1) + L(log(e2r_hat_tiv), 0:1),
    #                     data = EM_DATA[country %in% EAC5], vcov = DK ~ year)
  ),
  manufacturing_sample = list(
    OLS_E2R_EORA21 = feols(log(VA) ~ log(e2r) | country^sector + country^year + sector^year, 
                           data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_K_E2R_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ L(log(e2r_hat_tiv), 0:1), 
                            data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_CS_E2R_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ L(log(e2r_hat), 0:1), 
                             data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_B_E2R_EORA21 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ L(log(e2r_hat), 0:1) + L(log(e2r_hat_tiv), 0:1), 
                            data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    OLS_E2R_EORA15 = feols(log(VA) ~ log(e2r) | country^sector + country^year + sector^year, 
                           data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_K_E2R_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ L(log(e2r_hat_tiv), 0:1), 
                            data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_CS_E2R_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ L(log(e2r_hat), 0:1), 
                             data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_B_E2R_EORA15 = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ L(log(e2r_hat), 0:1) + L(log(e2r_hat_tiv), 0:1), 
                            data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year) #,
    # OLS_E2R_EM = feols(log(VA) ~ log(e2r) | country^sector + country^year + sector^year,
    #                    data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    # IV_K_E2R_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ L(log(e2r_hat_tiv), 0:1),
    #                     data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    # IV_CS_E2R_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ L(log(e2r_hat), 0:1),
    #                      data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    # IV_B_E2R_EM = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(e2r) ~ L(log(e2r_hat), 0:1) + L(log(e2r_hat_tiv), 0:1),
    #                     data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year)
  )
)

etable(models_E2R_dynIV$full_sample[1:8], headers = names(models_E2R_dynIV$full_sample[1:8]), 
       fitstat = ~ . + wh.p + kpr + ivwald1.p) #  + sargan.p
# -> These look better most of the time, but Wu-Hausmann r-value suggests no endogeneity ...
etable(models_E2R_dynIV$manufacturing_sample[1:8], headers = names(models_E2R_dynIV$manufacturing_sample[1:8]), 
       fitstat = ~ . + wh.p + kpr + ivwald1.p) #  + sargan.p

# Exporting
esttex(models_E2R_dynIV$full_sample[1:8], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(models_E2R_dynIV$full_sample[1:8]),
       fitstat = ~ . + wh.p + kpr + ivwald1.p)

esttex(models_E2R_dynIV$manufacturing_sample[1:8], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(models_E2R_dynIV$manufacturing_sample[1:8]),
       fitstat = ~ . + wh.p + kpr + ivwald1.p)


# First Stages
esttex(models_E2R_dynIV$full_sample[c(2:4, 6:8)], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, stage = 1,
       headers = names(models_E2R_dynIV$full_sample[c(2:4, 6:8)]), fitstat = ~ . + wh.p + kpr + ivwald1.p)

esttex(models_E2R_dynIV$manufacturing_sample[c(2:4, 6:8)], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, stage = 1,
       headers = names(models_E2R_dynIV$manufacturing_sample[c(2:4, 6:8)]), fitstat = ~ . + wh.p + kpr + ivwald1.p)



.c(WDR_EORA15_DATA, EORA21_DATA, EM_DATA) %=% lapply(list(WDR_EORA15_DATA, EORA21_DATA, EM_DATA), unindex)


# First-Difference Models --------------------------------------------------------------------------------------------------

.c(WDR_EORA15_DATA, EORA21_DATA, EM_DATA) %=% lapply(list(WDR_EORA15_DATA, EORA21_DATA, EM_DATA), 
   function(x) add_vars(x, index_by(x, country, sector, year) |> num_vars() |> fdiff(log = TRUE) |> 
                          replace_inf() |> add_stub("dlog_") |> unindex()))

# No Dynamics: Backward GVC Integration
models_I2E_diff <- list(
  full_sample = list(
    OLS_I2E_EORA21 = feols(dlog_VA ~ dlog_i2e, # | country^year + sector^year, 
                           data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_K_I2E_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_i2e ~ dlog_i2e_hat_tiv, 
                            data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_CS_I2E_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_i2e ~ dlog_i2e_hat, 
                             data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_B_I2E_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_i2e ~ dlog_i2e_hat + dlog_i2e_hat_tiv, 
                            data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    OLS_I2E_EORA15 = feols(dlog_VA ~ dlog_i2e | 0, 
                           data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_K_I2E_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_i2e ~ dlog_i2e_hat_tiv, 
                            data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_CS_I2E_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_i2e ~ dlog_i2e_hat, 
                             data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_B_I2E_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_i2e ~ dlog_i2e_hat + dlog_i2e_hat_tiv, 
                            data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    OLS_I2E_EM = feols(dlog_VA ~ dlog_i2e | 0,
                       data = EM_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_K_I2E_EM = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_i2e ~ dlog_i2e_hat_tiv,
                        data = EM_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_CS_I2E_EM = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_i2e ~ dlog_i2e_hat,
                         data = EM_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_B_I2E_EM = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_i2e ~ dlog_i2e_hat + dlog_i2e_hat_tiv,
                        data = EM_DATA[country %in% EAC5], vcov = DK ~ year)
  ),
  manufacturing_sample = list(
    OLS_I2E_EORA21 = feols(dlog_VA ~ dlog_i2e | 0, 
                           data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_K_I2E_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_i2e ~ dlog_i2e_hat_tiv, 
                            data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_CS_I2E_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_i2e ~ dlog_i2e_hat, 
                             data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_B_I2E_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_i2e ~ dlog_i2e_hat + dlog_i2e_hat_tiv, 
                            data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    OLS_I2E_EORA15 = feols(dlog_VA ~ dlog_i2e | 0, 
                           data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_K_I2E_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_i2e ~ dlog_i2e_hat_tiv, 
                            data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_CS_I2E_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_i2e ~ dlog_i2e_hat, 
                             data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_B_I2E_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_i2e ~ dlog_i2e_hat + dlog_i2e_hat_tiv, 
                            data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    OLS_I2E_EM = feols(dlog_VA ~ dlog_i2e | 0,
                       data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    IV_K_I2E_EM = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_i2e ~ dlog_i2e_hat_tiv,
                        data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    IV_CS_I2E_EM = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_i2e ~ dlog_i2e_hat,
                         data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    IV_B_I2E_EM = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_i2e ~ dlog_i2e_hat + dlog_i2e_hat_tiv,
                        data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year)
  )
)

etable(models_I2E_diff$full_sample[1:8], headers = names(models_I2E_diff$full_sample[1:8]), # stage = 1,
       fitstat = ~ . + wh.p + kpr + ivwald1.p)#  + sargan.p
etable(models_I2E_diff$manufacturing_sample[1:8], headers = names(models_I2E_diff$manufacturing_sample[1:8]), 
       fitstat = ~ . + wh.p + kpr + ivwald1.p) #  + sargan.p

# Exporting
esttex(models_I2E_diff$full_sample[1:8], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(models_I2E_diff$full_sample[1:8]), fitstat = ~ . + wh.p + kpr + ivwald1.p)

esttex(models_I2E_diff$manufacturing_sample[1:8], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(models_I2E_diff$manufacturing_sample[1:8]), fitstat = ~ . + wh.p + kpr + ivwald1.p)

# First Stages
esttex(models_I2E_diff$full_sample[c(2:4, 6:8)], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, stage = 1,
       headers = names(models_I2E_diff$full_sample[c(2:4, 6:8)]), fitstat = ~ . + wh.p + kpr + ivwald1.p)

esttex(models_I2E_diff$manufacturing_sample[c(2:4, 6:8)], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, stage = 1,
       headers = names(models_I2E_diff$manufacturing_sample[c(2:4, 6:8)]), fitstat = ~ . + wh.p + kpr + ivwald1.p)



# No Dynamics: Forward GVC Integration
models_E2R_diff <- list(
  full_sample = list(
    OLS_E2R_EORA21 = feols(dlog_VA ~ dlog_e2r | 0, 
                           data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_K_E2R_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_e2r ~ dlog_e2r_hat_tiv, 
                            data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_CS_E2R_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_e2r ~ dlog_e2r_hat, 
                             data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_B_E2R_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_e2r ~ dlog_e2r_hat + dlog_e2r_hat_tiv, 
                            data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    OLS_E2R_EORA15 = feols(dlog_VA ~ dlog_e2r | 0, 
                           data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_K_E2R_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_e2r ~ dlog_e2r_hat_tiv, 
                            data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_CS_E2R_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_e2r ~ dlog_e2r_hat, 
                             data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_B_E2R_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_e2r ~ dlog_e2r_hat + dlog_e2r_hat_tiv, 
                            data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    OLS_E2R_EM = feols(dlog_VA ~ dlog_e2r | 0,
                       data = EM_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_K_E2R_EM = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_e2r ~ dlog_e2r_hat_tiv,
                        data = EM_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_CS_E2R_EM = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_e2r ~ dlog_e2r_hat,
                         data = EM_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_B_E2R_EM = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_e2r ~ dlog_e2r_hat + dlog_e2r_hat_tiv,
                        data = EM_DATA[country %in% EAC5], vcov = DK ~ year)
  ),
  manufacturing_sample = list(
    OLS_E2R_EORA21 = feols(dlog_VA ~ dlog_e2r | 0, 
                           data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_K_E2R_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_e2r ~ dlog_e2r_hat_tiv, 
                            data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_CS_E2R_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_e2r ~ dlog_e2r_hat, 
                             data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_B_E2R_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_e2r ~ dlog_e2r_hat + dlog_e2r_hat_tiv, 
                            data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    OLS_E2R_EORA15 = feols(dlog_VA ~ dlog_e2r | 0, 
                           data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_K_E2R_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_e2r ~ dlog_e2r_hat_tiv, 
                            data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_CS_E2R_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_e2r ~ dlog_e2r_hat, 
                             data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_B_E2R_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_e2r ~ dlog_e2r_hat + dlog_e2r_hat_tiv, 
                            data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    OLS_E2R_EM = feols(dlog_VA ~ dlog_e2r | 0,
                       data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    IV_K_E2R_EM = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_e2r ~ dlog_e2r_hat_tiv,
                        data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    IV_CS_E2R_EM = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_e2r ~ dlog_e2r_hat,
                         data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    IV_B_E2R_EM = feols(dlog_VA ~ 0 | country^year + sector^year | dlog_e2r ~ dlog_e2r_hat + dlog_e2r_hat_tiv,
                        data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year)
  )
)

etable(models_E2R_diff$full_sample[1:8], headers = names(models_E2R_diff$full_sample[1:8]), 
       fitstat = ~ . + wh.p + kpr + ivwald1.p) #  + sargan.p
etable(models_E2R_diff$manufacturing_sample[1:8], headers = names(models_E2R_diff$manufacturing_sample[1:8]), 
       fitstat = ~ . + wh.p + kpr + ivwald1.p) #  + sargan.p

# Exporting
esttex(models_E2R_diff$full_sample[1:8], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(models_E2R_diff$full_sample[1:8]),
       fitstat = ~ . + wh.p + kpr + ivwald1.p)

esttex(models_E2R_diff$manufacturing_sample[1:8], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(models_E2R_diff$manufacturing_sample[1:8]),
       fitstat = ~ . + wh.p + kpr + ivwald1.p)


# First Stages
esttex(models_E2R_diff$full_sample[c(2:4, 6:8)], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, stage = 1,
       headers = names(models_E2R_diff$full_sample[c(2:4, 6:8)]), fitstat = ~ . + wh.p + kpr + ivwald1.p)

esttex(models_E2R_diff$manufacturing_sample[c(2:4, 6:8)], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, stage = 1,
       headers = names(models_E2R_diff$manufacturing_sample[c(2:4, 6:8)]), fitstat = ~ . + wh.p + kpr + ivwald1.p)


# Dynamic First-Difference Models --------------------------------------------------------------------------------------------------

# Backward GVC Integration
models_I2E_dyn_diff <- list(
  full_sample = list(
    OLS_I2E_EORA21 = feols(dlog_VA ~ L(dlog_i2e, 0:2), # | country^year + sector^year, 
                           data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_K_I2E_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_i2e, 0:2) ~ L(dlog_i2e_hat_tiv, 0:2), 
                            data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_CS_I2E_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_i2e, 0:2) ~ L(dlog_i2e_hat, 0:2), 
                             data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_B_I2E_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_i2e, 0:2) ~ L(dlog_i2e_hat, 0:2) + L(dlog_i2e_hat_tiv, 0:2), 
                            data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    OLS_I2E_EORA15 = feols(dlog_VA ~ L(dlog_i2e, 0:2) | 0, 
                           data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_K_I2E_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_i2e, 0:2) ~ L(dlog_i2e_hat_tiv, 0:2), 
                            data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_CS_I2E_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_i2e, 0:2) ~ L(dlog_i2e_hat, 0:2), 
                             data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_B_I2E_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_i2e, 0:2) ~ L(dlog_i2e_hat, 0:2) + L(dlog_i2e_hat_tiv, 0:2), 
                            data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    OLS_I2E_EM = feols(dlog_VA ~ L(dlog_i2e, 0:2) | 0,
                       data = EM_DATA[country %in% EAC5], vcov = DK ~ year) #,
    # IV_K_I2E_EM = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_i2e, 0:2) ~ L(dlog_i2e_hat_tiv, 0:2),
    #                     data = EM_DATA[country %in% EAC5], vcov = DK ~ year),
    # IV_CS_I2E_EM = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_i2e, 0:2) ~ L(dlog_i2e_hat, 0:2),
    #                      data = EM_DATA[country %in% EAC5], vcov = DK ~ year),
    # IV_B_I2E_EM = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_i2e, 0:2) ~ L(dlog_i2e_hat, 0:2) + L(dlog_i2e_hat_tiv, 0:2),
    #                     data = EM_DATA[country %in% EAC5], vcov = DK ~ year)
  ),
  manufacturing_sample = list(
    OLS_I2E_EORA21 = feols(dlog_VA ~ L(dlog_i2e, 0:2) | 0, 
                           data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_K_I2E_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_i2e, 0:2) ~ L(dlog_i2e_hat_tiv, 0:2), 
                            data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_CS_I2E_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_i2e, 0:2) ~ L(dlog_i2e_hat, 0:2), 
                             data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_B_I2E_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_i2e, 0:2) ~ L(dlog_i2e_hat, 0:2) + L(dlog_i2e_hat_tiv, 0:2), 
                            data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    OLS_I2E_EORA15 = feols(dlog_VA ~ L(dlog_i2e, 0:2) | 0, 
                           data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_K_I2E_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_i2e, 0:2) ~ L(dlog_i2e_hat_tiv, 0:2), 
                            data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_CS_I2E_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_i2e, 0:2) ~ L(dlog_i2e_hat, 0:2), 
                             data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_B_I2E_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_i2e, 0:2) ~ L(dlog_i2e_hat, 0:2) + L(dlog_i2e_hat_tiv, 0:2), 
                            data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    OLS_I2E_EM = feols(dlog_VA ~ L(dlog_i2e, 0:2) | 0,
                       data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year) #,
    # IV_K_I2E_EM = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_i2e, 0:2) ~ L(dlog_i2e_hat_tiv, 0:2),
    #                     data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    # IV_CS_I2E_EM = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_i2e, 0:2) ~ L(dlog_i2e_hat, 0:2),
    #                      data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    # IV_B_I2E_EM = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_i2e, 0:2) ~ L(dlog_i2e_hat, 0:2) + L(dlog_i2e_hat_tiv, 0:2),
    #                     data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year)
  )
)

etable(models_I2E_dyn_diff$full_sample[1:8], headers = names(models_I2E_dyn_diff$full_sample[1:8]), # stage = 1,
       fitstat = ~ . + wh.p + kpr + ivwald1.p)#  + sargan.p
etable(models_I2E_dyn_diff$manufacturing_sample[1:8], headers = names(models_I2E_dyn_diff$manufacturing_sample[1:8]), 
       fitstat = ~ . + wh.p + kpr + ivwald1.p) #  + sargan.p

# Exporting
esttex(models_I2E_dyn_diff$full_sample[1:8], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(models_I2E_dyn_diff$full_sample[1:8]), fitstat = ~ . + wh.p + kpr + ivwald1.p)

esttex(models_I2E_dyn_diff$manufacturing_sample[1:8], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(models_I2E_dyn_diff$manufacturing_sample[1:8]), fitstat = ~ . + wh.p + kpr + ivwald1.p)

# First Stages
esttex(models_I2E_dyn_diff$full_sample[c(2:4, 6:8)], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, stage = 1,
       headers = names(models_I2E_dyn_diff$full_sample[c(2:4, 6:8)]), fitstat = ~ . + wh.p + kpr + ivwald1.p)

esttex(models_I2E_dyn_diff$manufacturing_sample[c(2:4, 6:8)], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, stage = 1,
       headers = names(models_I2E_dyn_diff$manufacturing_sample[c(2:4, 6:8)]), fitstat = ~ . + wh.p + kpr + ivwald1.p)


# Forward GVC Integration
models_E2R_dyn_diff <- list(
  full_sample = list(
    OLS_E2R_EORA21 = feols(dlog_VA ~ L(dlog_e2r, 0:2) | 0, 
                           data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_K_E2R_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_e2r, 0:2) ~ L(dlog_e2r_hat_tiv, 0:2), 
                            data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_CS_E2R_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_e2r, 0:2) ~ L(dlog_e2r_hat, 0:2), 
                             data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_B_E2R_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_e2r, 0:2) ~ L(dlog_e2r_hat, 0:2) + L(dlog_e2r_hat_tiv, 0:2), 
                            data = EORA21_DATA[country %in% EAC5], vcov = DK ~ year),
    OLS_E2R_EORA15 = feols(dlog_VA ~ L(dlog_e2r, 0:2) | 0, 
                           data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_K_E2R_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_e2r, 0:2) ~ L(dlog_e2r_hat_tiv, 0:2), 
                            data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_CS_E2R_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_e2r, 0:2) ~ L(dlog_e2r_hat, 0:2), 
                             data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    IV_B_E2R_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_e2r, 0:2) ~ L(dlog_e2r_hat, 0:2) + L(dlog_e2r_hat_tiv, 0:2), 
                            data = WDR_EORA15_DATA[country %in% EAC5], vcov = DK ~ year),
    OLS_E2R_EM = feols(dlog_VA ~ L(dlog_e2r, 0:2) | 0,
                       data = EM_DATA[country %in% EAC5], vcov = DK ~ year) #,
    # IV_K_E2R_EM = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_e2r, 0:2) ~ L(dlog_e2r_hat_tiv, 0:2),
    #                     data = EM_DATA[country %in% EAC5], vcov = DK ~ year),
    # IV_CS_E2R_EM = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_e2r, 0:2) ~ L(dlog_e2r_hat, 0:2),
    #                      data = EM_DATA[country %in% EAC5], vcov = DK ~ year),
    # IV_B_E2R_EM = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_e2r, 0:2) ~ L(dlog_e2r_hat, 0:2) + L(dlog_e2r_hat_tiv, 0:2),
    #                     data = EM_DATA[country %in% EAC5], vcov = DK ~ year)
  ),
  manufacturing_sample = list(
    OLS_E2R_EORA21 = feols(dlog_VA ~ L(dlog_e2r, 0:2) | 0, 
                           data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_K_E2R_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_e2r, 0:2) ~ L(dlog_e2r_hat_tiv, 0:2), 
                            data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_CS_E2R_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_e2r, 0:2) ~ L(dlog_e2r_hat, 0:2), 
                             data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_B_E2R_EORA21 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_e2r, 0:2) ~ L(dlog_e2r_hat, 0:2) + L(dlog_e2r_hat_tiv, 0:2), 
                            data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    OLS_E2R_EORA15 = feols(dlog_VA ~ L(dlog_e2r, 0:2) | 0, 
                           data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_K_E2R_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_e2r, 0:2) ~ L(dlog_e2r_hat_tiv, 0:2), 
                            data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_CS_E2R_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_e2r, 0:2) ~ L(dlog_e2r_hat, 0:2), 
                             data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    IV_B_E2R_EORA15 = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_e2r, 0:2) ~ L(dlog_e2r_hat, 0:2) + L(dlog_e2r_hat_tiv, 0:2), 
                            data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year),
    OLS_E2R_EM = feols(dlog_VA ~ L(dlog_e2r, 0:2) | 0,
                       data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year) # ,
    # IV_K_E2R_EM = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_e2r, 0:2) ~ L(dlog_e2r_hat_tiv, 0:2),
    #                     data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    # IV_CS_E2R_EM = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_e2r, 0:2) ~ L(dlog_e2r_hat, 0:2),
    #                      data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year),
    # IV_B_E2R_EM = feols(dlog_VA ~ 0 | country^year + sector^year | L(dlog_e2r, 0:2) ~ L(dlog_e2r_hat, 0:2) + L(dlog_e2r_hat_tiv, 0:2),
    #                     data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = DK ~ year)
  )
)

etable(models_E2R_dyn_diff$full_sample[1:8], headers = names(models_E2R_dyn_diff$full_sample[1:8]), 
       fitstat = ~ . + wh.p + kpr + ivwald1.p) #  + sargan.p
etable(models_E2R_dyn_diff$manufacturing_sample[1:8], headers = names(models_E2R_dyn_diff$manufacturing_sample[1:8]), 
       fitstat = ~ . + wh.p + kpr + ivwald1.p) #  + sargan.p

# Exporting
esttex(models_E2R_dyn_diff$full_sample[1:8], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(models_E2R_dyn_diff$full_sample[1:8]),
       fitstat = ~ . + wh.p + kpr + ivwald1.p)

esttex(models_E2R_dyn_diff$manufacturing_sample[1:8], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, 
       headers = names(models_E2R_dyn_diff$manufacturing_sample[1:8]),
       fitstat = ~ . + wh.p + kpr + ivwald1.p)


# First Stages
esttex(models_E2R_dyn_diff$full_sample[c(2:4, 6:8)], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, stage = 1,
       headers = names(models_E2R_dyn_diff$full_sample[c(2:4, 6:8)]), fitstat = ~ . + wh.p + kpr + ivwald1.p)

esttex(models_E2R_dyn_diff$manufacturing_sample[c(2:4, 6:8)], digits.stats = 4, fixef_sizes = TRUE, fixef_sizes.simplify = TRUE, stage = 1,
       headers = names(models_E2R_dyn_diff$manufacturing_sample[c(2:4, 6:8)]), fitstat = ~ . + wh.p + kpr + ivwald1.p)


## Experimental ----------------------------------------------------------------------------------------------------


mod = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(gvcb) ~ log(i2e_hat) + log(i2e_hat_tiv), 
            data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year)
summary(mod, stage = 1:2)
etable(summary(mod, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)

mod = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(gvcf) ~ log(e2r_hat) + log(e2r_hat_tiv), 
            data = EORA21_DATA[country %in% EAC5 & sector %in% MAN], vcov = DK ~ year)
summary(mod, stage = 1:2)
etable(summary(mod, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)

mod = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(gvcb) ~ log(i2e_hat) * log(i2e_hat_tiv), 
            data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = "threeway") # DK ~ year
summary(mod, stage = 1:2)
etable(summary(mod, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)

mod = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(gvcf) ~ log(e2r_hat) * log(e2r_hat_tiv), 
            data = WDR_EORA15_DATA[country %in% EAC5 & sector %in% MAN], vcov = "threeway")
summary(mod, stage = 1:2)
etable(summary(mod, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)

mod = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(gvcb) ~ log(i2e_hat) * log(i2e_hat_tiv), 
            data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = "threeway") # DK ~ year
summary(mod, stage = 1:2)
etable(summary(mod, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)

mod = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(gvcf) ~ log(e2r_hat) * log(e2r_hat_tiv), 
            data = EM_DATA[country %in% EAC5 & sector %in% EM_MAN], vcov = "threeway")
summary(mod, stage = 1:2)
etable(summary(mod, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)



# With Dynamics
mod = feols(Dlog(VA) ~ 0 | country^year + sector^year | L(Dlog(gvcb), 0:2) ~ L(Dlog(i2e_hat), 0:2) + L(Dlog(i2e_hat_tiv), 0:2), 
            data = EORA21_DATA[country %in% EAC5], vcov = "threeway") # DK ~ year
summary(mod, stage = 1:2)
etable(summary(mod, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)

mod = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | L(log(gvcf), 0:1) ~ L(log(e2r_hat), 0:1) + L(log(e2r_hat_tiv), 0:1), 
            data = EORA21_DATA[country %in% EAC5], vcov = "threeway")
summary(mod, stage = 1:2)
etable(summary(mod, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)

mod = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | L(log(gvcb), 0:1) ~ L(log(i2e_hat), 0:1) + L(log(i2e_hat_tiv), 0:1), 
            data = EM_DATA[country %in% EAC5], vcov = "threeway") # DK ~ year
summary(mod, stage = 1:2)
etable(summary(mod, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)

mod = feols(log(VA) ~ 0 | country^sector + country^year + sector^year | log(gvcf) ~ log(e2r_hat) + log(e2r_hat_tiv), 
            data = EM_DATA[country %in% EAC5], vcov = "threeway")
summary(mod, stage = 1:2)
etable(summary(mod, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)

# plot(fixef(mod))
