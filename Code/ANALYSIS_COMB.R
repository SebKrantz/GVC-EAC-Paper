##########################################
# GVC Analysis for EAC Countries Using
# EORA 2021 and EMERGING Databases and
# GVC Indicators from STATA's ICIO Module
##########################################

# Load Libraries and Functions -----------------------
library(fastverse)
set_collapse(mask = c("manip", "helper", "special"), nthreads = 4, sort = TRUE)
fastverse_extend(ggplot2, scales, RColorBrewer, readxl, pheatmap, qs, decompr, install = TRUE)
vec_ptype2.factor.factor <- function(x, y, ...) x

pretty_plot <-
  theme(
    axis.title.x = element_text(size = 14, margin = ggplot2::margin(t = 10, b = 5)),
    axis.title.y = element_text(size = 14, margin = ggplot2::margin(r = 10, l = 5)),
    # axis.text.x = element_text(
    #   angle = 315,
    #   hjust = 0,
    #   margin = ggplot2::margin(t = 0)
    # ),
    legend.position = "top",
    strip.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.text = element_text(size = 12)
  )

rbsc <- discrete_scale(c("colour", "fill"), "hue", rainbow, na.value = "grey50") # hcl.colors

crbpal <- gradient_n_pal(recode_char(rainbow(26L),  
                                     "#00FF14" = "#00CC66", # Dark Green 
                                     "#FF003B" = "#FF0000")) # Red / Magenta
rbsc2 <- discrete_scale(c("colour", "fill"), "gradientn", 
                        function(x) crbpal(seq(0, 1, 1/x)), na.value = "grey50")

# function to get the country name
cr <- function(x) {
  if(is.character(x)) return(substr(x, 1L, 3L))
  names(x) <- substr(names(x), 1L, 3L)
  x
}
crm <- function(x) {
  if(is.character(x)) return(substr(x, 5L, 10000L))
  names(x) <- substr(names(x), 5L, 10000L)
  x
}

mat_agg <- function(x) {
  gr <- qF(cr(rownames(x)), sort = FALSE, na.exclude = FALSE)
  gc <- qF(cr(colnames(x)), sort = FALSE, na.exclude = FALSE)
  x |> fsum(gr) |> t() |> fsum(gc) |> t()
}

get_E_shares <- function(T_ag, FD_ag, countries = EAC6, return.E = FALSE, VAS = TRUE) {
  
  nam <- rownames(T_ag)
  out <- rowSums(T_ag) + rowSums(FD_ag)
  
  if(!all_identical(nam, colnames(T_ag), rownames(FD_ag)))
    stop("Matrices not or nor equally named")
  
  T_ag_exp <- T_ag
  diag(T_ag_exp) <- 0
  FD_ag_exp <- FD_ag
  diag(FD_ag_exp) <- 0
  
  E <- cbind(T_ag_exp, FD_ag_exp) 
  
  if(return.E) return(E)
  ckmatch(countries, nam)
  ckmatch(countries, colnames(FD_ag))
  
  imp_sh_ctry <- cbind(Intermediate = colSums(T_ag_exp[countries, ]) / colSums(T_ag_exp),
                       Final = colSums(FD_ag_exp[countries, ]) / colSums(FD_ag_exp))
  imp_sh <- colSums(T_ag_exp) / colSums(T_ag)  # What proportion of inputs was imported ?
  
  # Export share (Proportion of Output Exported)
  exp_sh_ctry <- cbind(Intermediate = rowSums(T_ag_exp[, countries]) / rowSums(T_ag_exp),
                       Final = rowSums(FD_ag_exp[, countries]) / rowSums(FD_ag_exp))
  exp_sh <- rowSums(E) / out
  
  if(!all_obj_equal(names(imp_sh), names(exp_sh), rownames(imp_sh_ctry), rownames(exp_sh_ctry)))
    stop("Names mismatch")
  
  list(Aggregate = cbind(`Value Added` = if(VAS) {1 - colSums(T_ag) / out} else NULL,
                         `Percent of Inputs Imported` = imp_sh, 
                         `Percent of Output Exported` = exp_sh),
       Shares = list(Imports = imp_sh_ctry, 
                     Exports = exp_sh_ctry))
}

value2df <- function(l, nam = NULL) {
  res <- lapply(l, qDT, "sector") %>% 
    unlist2d("year", DT = TRUE) %>% 
    transform(country = qF(cr(sector), sort = FALSE), 
              sector = qF(crm(sector), sort = FALSE),
              year = as.integer(year)) %>%
    colorder(year, country, sector) 
  if(!is.null(nam)) set_names(res, c("year", "country", "sector", nam)) else res
}

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
  collap(~ year + trade + sect, fsum) |> 
  mutate(source = qF("WDR_EORA")) |> 
  colorder(source, year, country = trade, sect, sect_name)

WDR_POS_AGG <- WDR_POS |> group_by(source, year, country) |> select(-sect, -sect_name) |> fsum()


EM_SEC <- read_xlsx("~/Documents/Data/EMERGING/Sector_EMERGING.xlsx") |> mutate(id = Code) |> rename(tolower)
EM_CTRY <- read_xlsx("~/Documents/Data/EMERGING/Country_EMERGING.xlsx") |> 
           transform(Detailed_Region_Code = iif(ISO3 %in% EAC6, ISO3, Detailed_Region_Code),
                     Detailed_Region = nif(ISO3 %in% EAC6, "East African Community", 
                                           Detailed_Region_Code == "SSA", "Sub-Saharan Africa (Excluding EAC)", 
                                           default = Detailed_Region)) |> 
           rename(tolower) 


KWW <- rowbind(EMERGING = fread("/Users/sebastiankrantz/Documents/Data/EMERGING/GVC_Regions/EM_GVC_KWW_BM19.csv"),
               EORA = fread("/Users/sebastiankrantz/Documents/Data/EORA/GVC_Regions/EORA_GVC_KWW_BM19.csv") |> 
                      transformv(is.double, `*`, 1/1000), 
               idcol = "source")

BIL_SEC <- rowbind(EMERGING = fread("/Users/sebastiankrantz/Documents/Data/EMERGING/GVC_Regions/EM_GVC_BIL_SEC_BM19.csv"),
                   EORA = fread("/Users/sebastiankrantz/Documents/Data/EORA/GVC_Regions/EORA_GVC_BIL_SEC_BM19.csv") |> 
                          transformv(is.double, `*`, 1/1000), 
                   idcol = "source") |> 
           # Aggregating to Broad Sectors
           join(list(EORA = sec_class, EMERGING = EM_SEC) |> 
                lapply(select, id, from_sector = broad_sector_code) |> 
                rowbind(idcol = "source"), 
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
EM_DET$y <- colnames(EM_DET$out_ag)

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

list(EORA = EORA_DET, EMERGING = EM_DET) |> 
  lapply(function(x) {
    sapply(x$y, function(i) x$VB_ag[, EAC6, i], # mat_agg(leontief(x$decomps[[i]], long = FALSE))[, EAC6],
           simplify = FALSE) |> 
    unlist2d("year", "share", DT = TRUE) |> 
    mutate(year = as.integer(year)) |> 
    pivot(1:2, names = list("country", "value"))
  }) |> rowbind(idcol = "source") |> 
  subset((between(year, 2010, 2015) & source == "EORA") | (year >= 2015 & source == "EMERGING")) |> 
  mutate(share = factor(setv(share, "SSD", "SSA"), levels = REG)) |> 
  group_by(source, share, country) |> 
  select(value) |> fmean() |> 
  subset(as.character(country) != as.character(share)) |> 
  # # mutate(value = fsum(value, list(source, country), TRA = "/")) |> 
  # subset(share %!in% EAC6 & source == "EMERGING") |> 
  # roworder(-value)
  mutate(country = setlevels(country, new = paste0(levels(country), " (", round(vs_agg_share[levels(country)]*100,1), "%)"))) |> 
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
