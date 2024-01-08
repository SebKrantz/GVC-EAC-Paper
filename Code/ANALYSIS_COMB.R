##########################################
# GVC Analysis for EAC Countries Using
# EORA 2021 and EMERGING Databases and
# GVC Indicators from STATA's ICIO Module
##########################################

# Load Libraries and Functions -----------------------
library(fastverse)
set_collapse(mask = c("manip", "helper", "special"), nthreads = 4, sort = TRUE)
fastverse_extend(ggplot2, scales, RColorBrewer, readxl, pheatmap, qs, install = TRUE)
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

# Load GVC Data ----------------------------------------------------
EAC <- c("UGA", "TZA", "KEN", "RWA", "BDI", "SSD", "COD")
EAC6 <- c("UGA", "TZA", "KEN", "RWA", "BDI", "COD")

trade_class <- read_xlsx("/Users/sebastiankrantz/Documents/Data/EORA/trade classification.xlsx", range = "A1:B220")
sec_class <- read_xlsx("/Users/sebastiankrantz/Documents/Data/EORA/trade classification.xlsx", sheet = "Sectors")

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

KWW <- rowbind(EMERGING = fread("/Users/sebastiankrantz/Documents/Data/EMERGING/ICIO_CSV/EM_GVC_KWW_BM19.csv"),
               EORA = fread("/Users/sebastiankrantz/Documents/Data/EORA/ICIO_CSV/EORA_GVC_KWW_BM19.csv") |> 
                      transformv(is.double, `*`, 1/1000), 
               idcol = "source")

BIL_SEC <- rowbind(EMERGING = fread("/Users/sebastiankrantz/Documents/Data/EMERGING/ICIO_CSV/EM_GVC_BIL_SEC_BM19.csv"),
                   EORA = fread("/Users/sebastiankrantz/Documents/Data/EORA/ICIO_CSV/EORA_GVC_BIL_SEC_BM19.csv") |> 
                          transformv(is.double, `*`, 1/1000), 
                   idcol = "source")

# Load Raw Decomposition Data -------------------------------------------

EORA <- new.env()
load("Data/EAC_EORA_2021_data_broad_sec.RData", envir = EORA)

EM <- new.env()
load("Data/EAC_EMERGING_data.RData", envir = EM)
EM_Raw <- qread("~/Documents/Data/EMERGING/EMERGING_EAC_Regions.qs")
EM_Agg <- qread("~/Documents/Data/EMERGING/EMERGING_EAC_Regions_Agg.qs")

# Basic Comparison ------------------------------------------------------

BIL_SEC[source == "EORA" & between(year, 2000, 2015)] |> group_by(source, from_region, year) |> select(gexp, gvc) |> fsum() 
KWW[source == "EORA" & between(year, 2000, 2015)] |> roworder(source, country, year)
WDR_POS[between(year, 2000, 2015)] |> group_by(source, country, year) |> select(gexp, gvc) |> fsum() 

rowbind(WDR_POS[country %in% EAC6, lapply(.SD, sum), by = .(source, country, year), 
                .SDcols = .c(gexp, gvc, gvcb, gvcf)],
        BIL_SEC[from_region %in% EAC6, lapply(.SD, sum), by = .(source, country = from_region, year), 
                .SDcols = .c(gexp, gvc, gvcb, gvcf)]) |> 
  transformv(c(gvc, gvcb, gvcf), `/`, gexp) |> 
  rowbind(VS_df_ag %>% slt(-i2e) %>% 
   av(VS1_df_ag %>% slt(E2R)) %>%
   sbt(Country %in% EAC6) %>%
   tfm(source = "OLD_EORA") %>%
   rnm(Country = country, Year = year, I2E = gvcb, E2R = gvcf), fill = TRUE) |> 
  
  ggplot(aes(x = year, y = gvcb, colour = source, linetype = source)) +
    geom_line() +
    facet_wrap(~country) + 
    theme_bw() + pretty_plot + rbsc2

# Note: with higher level of sectoral aggregation, GVC indicators are different!

# Heatmaps of Aggregated Gross Flows ------------------------------------

ROW <- c("SSA", "MEA", "EUU", "ECA", "NAC", "SAS", "ASE", "CHN", "ROA", "LAC", "OCE")
REG <- c("UGA", "TZA", "KEN", "RWA", "BDI", "COD", ROW) #, "SSD")

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
  
  
