library(fastverse)
set_collapse(mask = c("manip", "helper", "special"))
fastverse_extend(ggplot2)
vec_ptype2.factor.factor <- function(x, y, ...) x

EAC <- c("UGA", "TZA", "KEN", "RWA", "BDI", "SSD", "COD")


####################################
# World Development Report GVC Data
####################################

labels <- c(
  cntry = "Country name",
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
  sect_name = "Sector name"
)

WDR <- fread("Data/WDR 2020 Chapter 1/WDR2020_gvc_data.csv") 
vlabels(WDR)[names(labels)] <- labels
# WDR |> setrename(t = year, cntry = ISO3) |> setrename(toupper, cols = is.double)

EAC %in% unique(WDR$cntry)

WDR[cntry %in% EAC, lapply(.SD, sum), by = .(cntry, year = t), .SDcols = gexp:gvcf] |> 
  transformv(c(gvcb, gvcf), `/`, gexp) |> 
  subset(year >= 1990, cntry, year, gvcb, gvcf) |> 
  melt(1:2) |> 
  ggplot(aes(x = year, y = value, colour = variable)) + 
  geom_line() + facet_wrap(~ cntry)
  
  
######################################
# Upstreamness and Downstreamness Data
# Mancini M., Montalbano P., Nenci S., Vurchio D., 2022, Positioning in Global Value Chains: World Map and Indicators. A new dataset available for GVC analyses.
# https://www.tradeconomics.com/position/
######################################

POS <- haven::read_dta("/Users/sebastiankrantz/Documents/Data/GVCPosition/position_full.dta")

POS |> 
  mutate(position = upstreamness / downstreamness) |> 
  collap(position ~ country + t + source) |> 
  subset(country %in% EAC & t >= 1995) |> 
  ggplot(aes(x = t, y = position, colour = country)) +
    geom_line() 

####################################
# WITS GVC DATA
# From: https://wits.worldbank.org/gvc/gvc-data-download.html
####################################

# Trade Data: Very Large, but source data from various databases
WITS <- fread("unzip -p /Users/sebastiankrantz/Documents/Data/WITSGVCData/gvc_trade.zip")
qsu(WITS)
WITS |> qsu(t ~ source, stable = FALSE)
rm(WITS); gc()

# Standard GVC Data
GVC <- fread("unzip -p /Users/sebastiankrantz/Documents/Data/WITSGVCData/gvc-output.zip")
GVC |> qsu(t ~ source, stable = FALSE)
rm(GVC); gc()
