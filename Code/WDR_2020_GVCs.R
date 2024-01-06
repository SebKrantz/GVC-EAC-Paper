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
######################################