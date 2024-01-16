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
  mutate(source = qF("WDR_EORA")) |> 
  colorder(source, year, country = trade, sect, sect_name)

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

EORA <- new.env()
load("Data/EAC_EORA_2021_data_broad_sec.RData", envir = EORA)

EORA_DET <- new.env()
load("Data/EAC_EORA_2021_data.RData", envir = EORA_DET)

EM <- new.env()
load("Data/EAC_EMERGING_data_broad_sec.RData", envir = EM)
EM$y <- colnames(EM$out_ag)

EM_DET <- new.env()
load("Data/EAC_EMERGING_data.RData", envir = EM_DET)

EM_Raw <- qread("~/Documents/Data/EMERGING/EMERGING_EAC_Regions.qs")
EM_Agg <- qread("~/Documents/Data/EMERGING/EMERGING_EAC_Regions_Broad_Sectors.qs")

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

# Heatmaps of Aggregated Gross Flows

EM_T_ag_15_19 <- EM$T_ag[,, as.character(2015:2019)] |> rowMeans(dims = 2) |> extract(REG, REG)
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
EM_FD_ag_15_19 <- EM$FD_ag[,, as.character(2015:2019)] |> rowMeans(dims = 2) |> extract(REG, REG)
E_shares <- get_E_shares(EM_T_ag_15_19, EM_FD_ag_15_19) 

# Aggregate
E_shares$Aggregate |> 
  qDF("country") |> 
  pivot("country") |> 
  subset(country %in% EAC6) |> 
  ggplot(aes(x = country, y = value)) +
    facet_wrap( ~ variable, scales = "free_y") +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = percent) +
    labs(y = "Percent", x = "EAC Member") +
    theme_bw() + pretty_plot

dev.copy(pdf, "Figures/REV/EM_gross_shares_ag.pdf", width = 10, height = 4)
dev.off()

# Export / Import Shares
E_shares$Shares |> 
  unlist2d("Variable", "Country") |> 
  pivot(1:2, names = list("Flow", "Value")) |> 
  subset(Country %in% EAC6) |> 
  ggplot(aes(x = Country, y = Value, fill = Flow)) +
  facet_wrap( ~ Variable, scales = "free_y") +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  scale_y_continuous(labels = percent) +
  labs(y = "EAC Share", x = "EAC Member") +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() + pretty_plot + theme(legend.position = "right")

dev.copy(pdf, "Figures/REV/EM_gross_trade_shares_ag.pdf", width = 10, height = 4)
dev.off()

# Largest Sector-Level intermediate flows. 
EM_T_DT_Agg <- EM_Agg$DATA[as.character(2015:2019)] |> 
  get_elem("T") |> pmean() |> qDT("from") |> 
  transform(set_names(tstrsplit(from, ".", fixed = TRUE), c("from_region", "from_sector"))) |> 
  pivot(c("from", "from_region", "from_sector"), names = list("to", "value")) |> 
  transform(set_names(tstrsplit(to, ".", fixed = TRUE), c("to_region", "to_sector"))) |> 
  colorder(value, pos = "end") |> 
  subset(from_region != to_region & (from_region %in% EAC6 | to_region %in% EAC6))
  
# Largest 20 Flows: Broad Sectors
add_vars(
  EM_T_DT_Agg |> 
    subset(from_region != "COD" & to_region != "COD") |> 
    select(from, to, value) |> 
    roworder(-value) |> 
    head(20),
  EM_T_DT_Agg |> 
    subset(from_region %in% EAC6 & to_region %in% EAC6) |> 
    select(from, to, value) |> 
    roworder(-value) |> 
    head(20)
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
  facet_wrap(~country) + 
  labs(y = "Backward GVC Participation (VS)", x = "Year", 
       colour = "Source:   ", linetype = "Source:   ") +
  theme_bw() + pretty_plot + rbsc2

dev.copy(pdf, "Figures/REV/VA_shares_ag_ts.pdf", width = 10, height = 5)
dev.off()

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

dev.copy(pdf, "Figures/REV/VA_shares_ctry.pdf", width = 12, height = 5)
dev.off()

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

dev.copy(pdf, "Figures/REV/VA_shares_sec.pdf", width = 10, height = 5)
dev.off()

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
  labs(x = "Database", y = "Share of Foreign Exports Content", fill = "Source") +
  theme_bw() + pretty_plot + rbsc2 + theme(legend.position = "right")

dev.copy(pdf, "Figures/REV/VA_shares_sec_ctry.pdf", width = 10, height = 5)
dev.off()

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
EM <- new.env()
load("Data/EAC_EMERGING_data_Countries_Agg_Sectors.RData", envir = EM)
EORA <- new.env()
load("Data/EAC_EORA_data_Countries_Agg_Sectors.RData", envir = EORA)

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

# Joint Plot
rowbind(EMERGING = EM$VS1_AGG |> select(year, country, E2R = VS1_Share),
        EORA = EORA$VS1_AGG |> select(year, country, E2R = VS1_Share),
        WDR_EORA = WDR_POS_AGG |> compute(E2R = gvcf / gexp, keep = .c(year, country)),
        idcol = "source") |> 
  subset(country %in% EAC6 & year >= 2000) |> 
  ggplot(aes(x = year, y = E2R, colour = source, linetype = source)) +
  geom_line() +
  facet_wrap( ~ country) + 
  labs(y = "Forward GVC Participation (VS1)", x = "Year", 
       colour = "Source:   ", linetype = "Source:   ") +
  theme_bw() + pretty_plot + rbsc2

dev.copy(pdf, "Figures/REV/VS1_shares_ag_ts.pdf", width = 10, height = 5)
dev.off()

# Time Series Area Plot
EORA$VS1_BIL |> 
  subset(country %in% EAC6) %>% 
  ggplot(aes(x = year, y = VS1_Share, fill = importer)) +
  geom_area(position = "stack", alpha = 0.8) +
  facet_wrap( ~ country, scales = "free_y") + 
  guides(fill = guide_legend(ncol = 1)) + 
  scale_y_continuous(labels = percent, breaks = extended_breaks(10)) +
  scale_x_continuous(expand = c(0,0)) + rbsc2 +
  theme_minimal() + pretty_plot + theme(legend.position = "right")

# Now Joint Plot Using ICIO Data (Country-sector level decomposition with full country 5-sector tables)
rowbind(REG_AGG |> compute(E2R = gvcf / gexp, keep = .c(source, year, region)),
        WDR_POS_AGG |> compute(E2R = gvcf / gexp, region = country, keep = .c(source, year))) |> 
  subset(region %in% EAC6 & year >= 2000) |> 
  mutate(region = factor(region, levels = EAC6)) |> 
  ggplot(aes(x = year, y = E2R, colour = source, linetype = source)) +
  geom_line() +
  facet_wrap( ~ region) + 
  labs(y = "Forward GVC Participation (VS1)", x = "Year", 
       colour = "Source:   ", linetype = "Source:   ") +
  theme_bw() + pretty_plot + rbsc2

dev.copy(pdf, "Figures/REV/GVCF_shares_ag_ts.pdf", width = 10, height = 5)
dev.off()

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

dev.copy(pdf, "Figures/REV/VS1_shares_ctry.pdf", width = 12, height = 5.3)
dev.off()

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
  labs(y = "Share of Re-Exported Content (VS1)", x = "Sector", fill = "Partner") +
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
             country = "EAC") |> 
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
           country = "EAC") |> 
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
               importer = "EAC") |> 
        subset(EAC_exporter, -EAC_exporter)
    )} |> 
  rename(importer = country)})


EAC_GVC_DATA <- rowbind(VS = VS_EAC, 
                        VS1 = VS1_EAC, 
                        VAI = VAI_EAC, 
                        VAFI = VAFI_EAC, idcol = "variable")
# Plot as in current paper
EAC_GVC_DATA |>
  mutate(country = factor(country, levels = c(EAC5, "EAC"))) |>
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
  mutate(country = factor(country, levels = c(EAC5, "EAC")), 
         weight = nif(source == "EMERGING" & year == 2010L, 2, source == "EORA" & year > 2015, 0.1, default = 1)) |> 
  group_by(variable, source, country) |> 
  mutate(as.list(set_names(coef(lm(value ~ year, weights = weight)), c("icpt", "slope")))) |>
  ungroup() |> 
  mutate(trend = icpt + year * slope)

# Improved plot
EAC_GVC_DATA |> 
  rename(value = Value, trend = "MM Trend") |> 
  pivot(1:4, values = c("Value", "MM Trend"), names = list("measure", "value")) |> 
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

dev.copy(pdf, "Figures/REV/VA_EAC5_shares_ts.pdf", width = 11.69, height = 6)
dev.off()

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

dev.copy(pdf, "Figures/REV/VA_EAC5_shares_slope_bar.pdf", width = 8, height = 4)
dev.off()


# Analysis at the sector-level using broad-sector ICIO's: ----------------------------

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
               country = "EAC") |> 
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
               country = "EAC") |> 
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
                   importer = "EAC") |> 
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
  mutate(country = factor(country, levels = c(EAC5, "EAC"))) |>
  subset(source == "EMERGING") |> # & between(year, 2005, 2015)) |>
  
  ggplot(aes(x = year, y = value, color = variable)) +
  geom_line() +
  facet_grid2(sector ~ country, scales = "free_y", independent = "y") +
  scale_y_continuous(labels = percent, breaks = extended_breaks(7)) +
  guides(color = guide_legend(title = NULL)) +
  theme_minimal() + pretty_plot


# Adding weighted linear trend
EAC_GVC_DATA_SEC <- EAC_GVC_DATA_SEC |> 
  mutate(country = factor(country, levels = c(EAC5, "EAC")),
         weight = nif(source == "EMERGING" & year == 2010L, 2, source == "EORA" & year > 2015, 0.1, default = 1)) |> 
  group_by(variable, source, country, sector) |> 
  mutate(as.list(set_names(coef(lm(value ~ year, weights = weight)), c("icpt", "slope")))) |> 
  ungroup() |> 
  mutate(trend = icpt + year * slope)

# Improved plot
EAC_GVC_DATA_SEC |> 
  subset(source == "EMERGING") |> 
  rename(value = Value, trend = "MM Trend") |> 
  pivot(1:5, values = c("Value", "MM Trend"), names = list("measure", "value")) |> 
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

dev.copy(pdf, "Figures/REV/EM_VA_EAC5_shares_ts_sec.pdf", width = 11.69, height = 10)
dev.off()

# Plotting slope coefficients

EAC_GVC_DATA_SEC |> 
  subset(source == "EMERGING" & sector != "MIN") |> 
  group_by(variable, source, country, sector) |> 
  select(slope) |> ffirst() |> 
  mutate(slope_tr = nif(slope > 0.01, 0.01 + (slope-0.01) * 0.1, slope < -0.01, -0.01 + (slope+0.01) * 0.1, default = slope)) |> 
  
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
  
dev.copy(pdf, "Figures/REV/EM_VA_EAC5_shares_slope_bar_sec.pdf", width = 9, height = 7)
dev.off()





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
    

# Upstreamness and Downstreamness ------------------------------------------

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

dev.copy(pdf, "Figures/REV/VA_EAC5_shares_slope_bar.pdf", width = 8, height = 4)
dev.off()

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
  

# Upstreamness following Antras et al. 2013: (no nnventory correction yet as in mancini et al GVC Positioning database)
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
  labs(y = "Upstreamness Index", x = "Year", 
       colour = "Source:   ", linetype = "Source:   ") +
  theme_bw() + pretty_plot + rbsc2

dev.copy(pdf, "Figures/REV/Upstreamness_ag_ts.pdf", width = 10, height = 5)
dev.off()


# At the Broad Sector Level
U_DET_ALL_BSEC <- U_DET_ALL |> 
  join(rowbind(sbt(SEC_ALL, source == "EMERGING"), 
               sbt(SEC_ALL, source == "EORA") |> mtt(source = "WDR_EORA"),
               mtt(slt(sec_class, id = code, broad_sector_code), source = "EORA")), 
       on = c("source", "sector" = "id")) |> 
  mutate(sector = NULL) |> 
  group_by(source, year, country, sector = broad_sector_code) |> 
  select(U, E) |> fmean(E)

U_DET_ALL |> 
  group_by(source, year, country) |> 
  num_vars() |> fmean(E, keep.w = FALSE) |> 
  subset(country %in% EAC6 & year >= 2000) |> 
  mutate(country = factor(country, levels = EAC6)) |> 
  
  ggplot(aes(x = year, y = U, colour = source, linetype = source)) +
  geom_line() +
  facet_wrap(~ country, scales = "fixed", nrow = 2) +
  labs(y = "Upstreamness Index", x = "Year", 
       colour = "Source:   ", linetype = "Source:   ") +
  theme_bw() + pretty_plot + rbsc2

dev.copy(pdf, "Figures/REV/Upstreamness_ag_ts.pdf", width = 10, height = 5)
dev.off()


# Downstreamness following Antras et al. 2013: (no nnventory correction yet as in mancini et al GVC Positioning database)
# Problem: missing values (NA's) being generated, particularly for EMERGING!!!
# -> better compute from scratch!!
D <- list(EORA = EORA, EMERGING = EM) %>% 
  lapply(function(X) {
    lapply(X$decomps, function(o) {
      A = solve(o$B)
      diag(A) = diag(A) - 1
      A %*=% -1
      T = A %r*% o$X
      B = T / o$X # This is not the leontief inverse
      BB = solve(diag(ncol(B)) %-=% B)
      # if(anyNA(BB)) stop("errors")
      tmp = BB %*% (o$Vc * o$X)
      cbind(D = drop((BB %*% tmp) %/=% tmp), E = o$E)
    }) |> value2df()
  }) |> rowbind(idcol = "source")

D_DET <- list(EORA = EORA_DET, EMERGING = EM_DET) %>% 
  lapply(function(X) {
    lapply(X$decomps, function(o) {
      A = solve(o$B)
      diag(A) = diag(A) - 1
      A %*=% -1
      T = A %r*% o$X
      B = T %c/% o$X # This is not the leontief inverse
      BB = solve(diag(ncol(B)) %-=% B)
      tmp = BB %*% (o$Vc * o$X)
      cbind(D = drop((BB %*% tmp) %/=% tmp), E = o$E)
    }) |> value2df()
  }) |> rowbind(idcol = "source")

# Comparison
WDR_POS |> 
  select(source, year, country, sector = sect, D = downstreamness, E = gexp) |> 
  rowbind(D) |> 
  group_by(source, year, country) |> 
  num_vars() |> fmean(E, keep.w = FALSE) |> 
  subset(country %in% EAC6 & year >= 2000) |> 
  
  ggplot(aes(x = year, y = D, colour = source, linetype = source)) +
  geom_line() +
  facet_wrap(~ country, scales = "fixed", nrow = 2) +
  labs(y = "Downstreamness Index", x = "Year", 
       colour = "Source:   ", linetype = "Source:   ") +
  theme_bw() + pretty_plot + rbsc2
