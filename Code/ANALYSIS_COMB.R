##########################################
# GVC Analysis for EAC Countries Using
# EORA 2021 and EMERGING Databases and
# GVC Indicators from STATA's ICIO Module
##########################################

# Load Libraries and Functions -----------------------
library(fastverse)
set_collapse(mask = "all", remove = "between", nthreads = 4, sort = TRUE)
fastverse_extend(ggplot2, scales, readxl)
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

# Load GVC Data --------------------------------------
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

# Basic Comparison -----------------------------

BIL_SEC[source == "EORA" & between(year, 2000, 2015)] |> group_by(source, from_region, year) |> select(gexp, gvc) |> sum() 
KWW[source == "EORA" & between(year, 2000, 2015)] |> roworder(source, country, year)
WDR_POS[between(year, 2000, 2015)] |> group_by(source, country, year) |> select(gexp, gvc) |> sum() 

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

# Problem: do to higher level of aggregation, GVC indicators are different!