
# setwd("C:/Users/Sebastian Krantz/Documents/Data/EORA")

rm(list = ls())
gc()

load("Code/EAC_EORA_data.RData")

# Libraries, Functions, Global Variables -----------------------------------------

library(collapse)
library(magrittr)
library(data.table)
library(ggplot2)
library(scales)
library(RColorBrewer)

mfac <- function(x, ordered = TRUE) factor(x, levels = funique(x), ordered = ordered)
mds <- function(x) dollar(round(x), scale = 1/1000, suffix = " M")
mds2 <- function(x) dollar(round(x), scale = 1, suffix = " M")


value2df <- function(l, nam = NULL) {
  res <- lapply(l, qDT, "Sector") %>% 
    unlist2d("Year", DT = TRUE) %>% 
    tfm(Country = mfac(cr(Sector)), 
        Sector = mfac(crm(Sector)),
        Year = as.integer(Year)) %>%
    colorder(Year, Country, Sector) 
  if(!is.null(nam)) setNames(res, c("Year", "Country", "Sector", nam)) else res
}

pretty_plot <-
  theme(
    # axis.text = element_text(face="bold", size = 10),
    axis.title.x = element_text(size = 14, margin = ggplot2::margin(t = 10, b = 5)),
    axis.title.y = element_text(size = 14, margin = ggplot2::margin(r = 10, l = 5)),
    axis.text.x = element_text(
      angle = 315,
      hjust = 0,
      margin = ggplot2::margin(t = 0)
    ),
    legend.position = "top",
    # strip.background = element_rect(fill = "grey30", colour = NA),
    strip.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # plot.margin = ggplot2::margin(r = 70),
    # unit(c(1, 1, 0.5, 0.5), "lines"),,
    legend.text = element_text(size = 12)
  )

rbsc <- discrete_scale(c("colour", "fill"), "hue", rainbow, na.value = "grey50") # hcl.colors

setNames(rainbow(26L), i)
crbpal <- gradient_n_pal(recode_char(rainbow(26L),  
                                     "#00FF14" = "#00CC66", # Dark Green 
                                     "#FF003B" = "#FF0000")) # Red / Magenta
#FF0033
rbsc2 <- discrete_scale(c("colour", "fill"), "gradientn", 
                        function(x) crbpal(seq(0, 1, 1/x)), na.value = "grey50")

dec15 <- decomps$`2015`


####################################
# Part 0: Data Checks: GDP by Sector
####################################

g_sec <- GRP(crm(rownam), sort = FALSE)
va_ag_sec <- fsum(va, g_sec)

# Global GDP by Sector
va_ag_sec %>% 
  qDT("Sector") %>% 
  melt(1L, NULL, "Year", "VA") %>% 
  as_numeric_factor %>% 
  tfm(Sector = factor(Sector, levels = i)) %>%
  ggplot(aes(x = Year, y = VA, fill = Sector)) +
  geom_area(alpha = 0.8) + rbsc2 + 
  scale_x_continuous(breaks = 2005:2015, expand = c(0,0.02)) +
  scale_y_continuous(labels = mds, breaks = extended_breaks(10)) +
  guides(fill = guide_legend(ncol = 1L)) + 
  theme_minimal(base_size = 14) + pretty_plot +
  theme(legend.position = "right")

dev.copy(pdf, "../Figures/global_GDP_sector.pdf", width = 9, height = 7.5)
dev.off()


# Global GDP by World Region
va_ag %>% 
  qDT("Region") %>% 
  tfm(Region = fifelse(Region %in% EAC, "EAC", Region)) %>%
  gby(Region, sort = FALSE) %>% fsum %>% # nv %>% fsum(TRA = "%")
  melt(1L, NULL, "Year", "VA") %>% 
  as_numeric_factor %>% 
  tfm(Region = mfac(Region)) %>%
  ggplot(aes(x = Year, y = VA, fill = Region)) +
  geom_area(alpha = 0.8) + rbsc2 + 
  scale_x_continuous(breaks = 2005:2015, expand = c(0,0.02)) +
  scale_y_continuous(labels = mds, breaks = extended_breaks(10)) +
  guides(fill = guide_legend(ncol = 1L)) + 
  theme_minimal(base_size = 14) + pretty_plot +
  theme(legend.position = "right")

dev.copy(pdf, "../Figures/global_GDP_region.pdf", width = 9, height = 5)
dev.off()


# EAC GDP by Sector: Stacked Area
lapply(decomps, with, Vc * X) %>% 
  value2df("VA") %>%
  sbt(Country %in% EAC) %>%

  ggplot(aes(x = Year, y = VA, fill = Sector)) +
  geom_area(position = "stack", alpha = 0.8) +
  facet_wrap( ~ Country, scales = "free_y") + 
  guides(fill = guide_legend(ncol = 1)) + 
  scale_y_continuous(labels = mds, breaks = extended_breaks(7)) + 
  scale_x_continuous(breaks = 2005:2015) + rbsc2 +
  theme_minimal() + pretty_plot +
  theme(legend.position = "right")

dev.copy(pdf, "../Figures/EAC_GDP_sector.pdf", width = 11.69, height = 8.27)
dev.off()

###############################
# Part 1: Analyzing Gross Flows
###############################

# Heatmap of the Matrices -----------------------------------------------------
library(pheatmap)
# Intermediate Flows
T[eac, eac, "2015"] %>% log10 %>% 
  pheatmap(color = colorRampPalette(brewer.pal(n = 7, name ="YlOrRd"))(100),
           cluster_rows = FALSE, cluster_cols = FALSE, 
           border_color = NA, legend_breaks = -4:6, 
           fontsize_row = 3.2, fontsize_col = 3.2)  # legend_labels = 10^(0:7))

dev.copy(pdf, "../Figures/heatmap_EAC.pdf", width = 9, height = 8)
dev.off()

T_ag[, , "2015"] %c/% 1000 %>% log10 %>% .[r, r] %>% 
  pheatmap(color = colorRampPalette(brewer.pal(n = 7, name ="YlOrRd"))(100),
           cluster_rows = FALSE, cluster_cols = FALSE, border_color = NA,
           legend_breaks = 0:7, # legend_labels = 10^(0:7),
           display_numbers = TRUE)

dev.copy(pdf, "../Figures/heatmap_AG.pdf", width = 7, height = 6)
dev.off()


# Largest EAC flows -----------------------
eacfl <- T[eac, eac, "2015"] # add eac in rows bracket to get inter eac only
for(i in 1:6) eacfl[eac[, i], eac[, i]] <- 0 # Removing own country flows
eacfl <- unlist(lapply(mrtl(eacfl, TRUE), setNames, colnames(eacfl)))
# eacfl[substr(names(eacfl), 1L, 3L) %in% EAC] <- 0 #removing flows from EAC, comment out for inter-eac flows
eacfl <- round(sort(eacfl, decreasing = TRUE))

eaclfl <- head(eacfl, 20) %c/% 1000 %>% 
  qDT(TRUE) %>% 
  add_vars(head(eacfl[-grep("KEN", names(eacfl))], 20) %c/% 1000 %>% 
             qDT(TRUE)) %>% 
  setNames(c("Largest Flow", "Value", "Largest Non-KEN Flow", "Value"))

settfmv(eaclfl, is.character, `substr<-`, 8L, 8L, value = ">")
settfmv(eaclfl, is.character, function(x) sub(">", " -> ", x))

stargazer::stargazer(eaclfl, summary = FALSE)

# Largest EAC-WLD flows -----------------------
weacfl <- T[, , "2015"] # add eac in rows bracket to get inter eac only
weacfl[eac, eac] <- 0   # Removing inner EAC flows
weacfl[-eac, -eac] <- 0 # Removing other world flows
weacfl <- unlist(lapply(mrtl(weacfl, TRUE), setNames, colnames(weacfl)))
weacfl <- round(sort(weacfl, decreasing = TRUE))

weaclfl <- head(weacfl, 20) %c/% 1000 %>% 
  qDT(TRUE) %>% 
  add_vars(head(weacfl[-grep("KEN", names(weacfl))], 20) %c/% 1000 %>% 
             qDT(TRUE)) %>% 
  setNames(c("Largest Flow", "Value", "Largest Non-KEN Flow", "Value"))

settfmv(weaclfl, is.character, `substr<-`, 8L, 8L, value = ">")
settfmv(weaclfl, is.character, function(x) sub(">", " -> ", x))

stargazer::stargazer(weaclfl, summary = FALSE)

# Direct Value Added ----------------------

# Value Added:

va[eac, "2015"] %>% 
  qDF(TRUE) %>%
  setNames(.c(Sector, DVA)) %>%
  tfm(Country = mfac(cr(Sector)), Sector = mfac(crm(Sector))) %>%
  {
    ggplot(., aes(x = Sector, y = DVA, fill = Country)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(DVA)),
                vjust = ifelse(.$DVA > 0, -0.3, 1.3),
                size = 2.5
      ) +
      facet_wrap(~Country, scales = "fixed", ncol = 1L) +
      scale_fill_brewer(palette = "Set1") +
      guides(fill = FALSE) +
      scale_y_continuous(labels = mds, expand = c(0.2, 0)) +
      theme_minimal() +
      pretty_plot
  }

dev.copy(pdf, "../Figures/output_DVA_tot.pdf", width = 10, height = 8.27)
dev.off()

# Direct Value added content of Output (Problematic)
dec15$Vc[eac] %>% replace_outliers(0, NA, "min") %>%
  qDF(TRUE) %>%
  setNames(.c(Sector, DVA)) %>%
  tfm(Country = mfac(cr(Sector)), Sector = mfac(crm(Sector))) %>%
  {
    ggplot(., aes(x = Sector, y = DVA, fill = Country)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(DVA * 100, 1L)),
                vjust = ifelse(.$DVA > 0, -0.3, 1.3),
                size = 2.5
      ) +
      facet_wrap(~Country, scales = "fixed", ncol = 1L) +
      scale_fill_brewer(palette = "Set1") +
      guides(fill = FALSE) +
      scale_y_continuous(labels = percent, expand = c(0.2, 0)) +
      theme_minimal() +
      pretty_plot
  }

dev.copy(pdf, "../Figures/output_DVA.pdf", width = 10, height = 8.27)
dev.off()

# Exports ----------------------------------------------
# Uganda's export decomposition
barplot(crm(dec15$E[eac[, "UGA"]]), col = "orange", border = NA)

# Gross Exports:
exports <- lapply(decomps, with, E) %>% value2df("Exports")

# Total EAC Exports: Long

  exports %>% 
    sbt(Year == 2015L & Country %in% EAC) %>%
  {
    ggplot(., aes(x = Sector, y = Exports, fill = Country)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(Exports)),
                vjust = ifelse(.$Exports > 0, -0.3, 1.3),
                size = 2.5
      ) +
      facet_wrap(~Country, scales = "fixed", ncol = 1L) +
      scale_fill_brewer(palette = "Set1") +
      guides(fill = FALSE) +
      scale_y_continuous(labels = mds, trans = "sqrt") +
      theme_minimal() +
      pretty_plot
  }

dev.copy(pdf, "../Figures/exports.pdf", width = 10, height = 8.27)
dev.off()


# Total EAC Exports: Stacked
exports %>% 
  sbt(Year == 2015L & Country %in% EAC) %>%
  
  ggplot(aes(x = Country, y = Exports, fill = Sector)) +
  geom_bar(stat = "identity") +
  guides(fill = guide_legend(ncol = 1)) +
  scale_y_continuous(labels = mds, breaks = extended_breaks(7)) + rbsc2 +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())

dev.copy(pdf, "../Figures/exports_stacked.pdf", width = 8.27, height = 7.5)
dev.off()

# Total EAC Exports: Stacked Area
exports %>% 
  sbt(Country %in% EAC) %>%
  
  ggplot(aes(x = Year, y = Exports, fill = Sector)) +
  geom_area(position = "stack", alpha = 0.8) +
  facet_wrap( ~ Country, scales = "free_y") + 
  guides(fill = guide_legend(ncol = 1)) + 
  scale_y_continuous(labels = mds, breaks = extended_breaks(7)) + 
  scale_x_continuous(breaks = 2005:2015) + rbsc2 +
  theme_minimal() + pretty_plot +
  theme(legend.position = "right")

dev.copy(pdf, "../Figures/exports_stacked_ts.pdf", width = 11.69, height = 8.27)
dev.off()


# EAC Exports to EAC Partner States
dec15$ESR[eac, EAC] %>%
  rowSums() %>%
  qDF(TRUE) %>%
  setNames(.c(Sector, Exports)) %>%
  tfm(Country = mfac(cr(Sector)), Sector = mfac(crm(Sector))) %>%
  {
    ggplot(., aes(x = Sector, y = Exports, fill = Country)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(Exports, 1)),
                vjust = ifelse(.$Exports > 0, -0.3, 1.3),
                size = 2.5
      ) +
      facet_wrap(~ Country, scales = "fixed", ncol = 1L) +
      scale_fill_brewer(palette = "Set1") +
      guides(fill = FALSE) +
      scale_y_continuous(labels = mds, trans = "sqrt") +
      theme_minimal() +
      pretty_plot
  }

dev.copy(pdf, "../Figures/exports_EAC.pdf", width = 10, height = 8.27)
dev.off()

# Exports to EAC Partner States: Stacked
dec15$ESR[eac, EAC] %>%
  rowSums() %>%
  qDF(TRUE) %>%
  setNames(.c(Sector, Exports)) %>%
  tfm(Country = mfac(cr(Sector)), Sector = mfac(crm(Sector))) %>%
  ggplot(aes(x = Country, y = Exports, fill = Sector)) +
  geom_bar(stat = "identity") +
  guides(fill = guide_legend(ncol = 1)) +
  scale_y_continuous(labels = mds, breaks = extended_breaks(7)) + rbsc2 +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())

dev.copy(pdf, "../Figures/exports_EAC_stacked.pdf", width = 8.27, height = 7.5)
dev.off()

# Exports Share to EAC Partner States: Stacked
dec15$ESR[eac, EAC] %>%
  rowSums() %c/% fsum(rowSums(dec15$ESR[eac, ]), cr(rownam[eac]), TRA = 1L) %>% 
  qDF(TRUE) %>%
  setNames(.c(Sector, Exports)) %>%
  tfm(Country = mfac(cr(Sector)), Sector = mfac(crm(Sector))) %>%
  ggplot(aes(x = Country, y = Exports, fill = Sector)) +
  geom_bar(stat = "identity") +
  guides(fill = guide_legend(ncol = 1)) +
  scale_y_continuous(labels = percent, breaks = extended_breaks(7)) + rbsc2 +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())

dev.copy(pdf, "../Figures/exports_EAC_perc_stacked.pdf", width = 8.27, height = 7.5)
dev.off()


# Exports Share to EAC Partner States: Stacked Area
lapply(decomps, function(x) x$ESR[eac, EAC] %>% 
         rowSums %c/% 
         fsum(rowSums(dec15$ESR[eac, ]), cr(rownam[eac]), TRA = 1L) %>%
         qDF(TRUE) %>% setNames(.c(Sector, Exports))) %>% 
  unlist2d("Year") %>% 
  tfm(Country = mfac(cr(Sector)), 
      Sector = mfac(crm(Sector)),
      Year = as.integer(Year)) %>% 
  ggplot(aes(x = Year, y = Exports, fill = Sector)) +
  geom_area(position = "stack", alpha = 0.8) +
  facet_wrap( ~ Country, scales = "free_y") + 
  guides(fill = guide_legend(ncol = 1)) + 
  scale_y_continuous(labels = percent, breaks = extended_breaks(7)) + 
  scale_x_continuous(breaks = 2005:2015) + rbsc2 +
  theme_minimal() + pretty_plot +
  theme(legend.position = "right")

dev.copy(pdf, "../Figures/exports_EAC_perc_stacked_ts.pdf", width = 11.69, height = 8.27)
dev.off()

  
# Detailed Decomposition ----------------------------------------------

# Import share (Proportion of Output that was Imported Inputs)

get_E_shares <- function(T, FD, countries = EAC, return.E = FALSE, VAS = TRUE) {

  nam <- rownames(T)
  out <- rowSums(T) + rowSums(FD)
  GN <- nrow(T)
  G <- fndistinct(cr(nam))
  N <- GN / G
  Nfdc <- ncol(FD) / G
    
  if(!all_identical(nam, colnames(T), rownames(FD)))
    stop("Matrices not or nor equally named")
  
  `Value Added     ` <- if(VAS) {1 - colSums(T) / out} else NULL
  
  E <- cbind(T, FD) 
  
  for (j in 1:G) {
    m <- 1L + (j - 1L) * N
    n <- N + (j - 1L) * N
    s <- GN + 1L + (j - 1L) * Nfdc
    r <- GN + Nfdc + (j - 1L) * Nfdc
    E[m:n, m:n] <- 0  ## intermediate demand for domestic goods
    E[m:n, s:r] <- 0  ## final demand for domestic goods
  }
  
  if(return.E) return(E)
  
  ckmatch(countries, cr(nam))
  ckmatch(countries, cr(colnames(FD)))
  
  ictry <- which(cr(nam) %in% countries)
  ictry_FD <- which(cr(colnames(FD)) %in% countries)
  iT <- seq_len(GN)
  
  imp_sh <- colSums(E[, iT])
  imp_sh_eac <- colSums(E[ictry, iT]) / imp_sh # Further normalization: Relative to total imports
  imp_sh <- imp_sh / out # What proportion of output was imported ?
  
  # Export share (Proportion of Output Exported)
  exp_sh <- rowSums(E)
  exp_sh_eac <- rowSums(E[, c(ictry, ictry_FD + GN)]) / exp_sh
  exp_sh <- exp_sh / out
  
  if(!all_obj_equal(names(imp_sh), names(exp_sh), names(imp_sh_eac), names(exp_sh_eac)))
    stop("Names mismatch")
  
  shares <- cbind(`Value Added     `,
                  `Percent of Inputs Imported (VS)     ` = imp_sh, 
                  `Percent of Output Exported      ` = exp_sh, 
                  `Percent of Imports from EAC      ` = imp_sh_eac, 
                  `Percent of Exports to EAC      ` = exp_sh_eac)
  

  if(nrow(shares) != nrow(T)) stop("Dimension mismatch")
  
  return(shares)
}

# Choosing colors
# cols = c("#00CA28", #000099","#0000CC","#0000FF","#3366CC","#3399CC","#33CCFF","#660000","#990000","#CC0000","#FF0000","#FF3300","#FF6600","#FF9900","#FFCC00")
# barplot(setNames(rep(1, length(cols)), cols), col = cols)

# Import / Export share to / from EAC...
get_E_shares(T[,, "2015"], FD[,, "2015"])[eac, ] %>% qDT("Sector") %>%
  tfm(Country = mfac(cr(Sector)), Sector = mfac(crm(Sector))) %>%
  colorderv(5:4, pos = "exchange") %>%
  melt(.c(Country, Sector), value.name = "Value", na.rm = TRUE) %>% 
  ggplot(aes(x = Sector, y = Value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(Value * 100)),
            vjust = -0.3, size = 1.8, position = position_dodge(0.9)) +
  facet_wrap( ~ Country, scales = "fixed", ncol = 1L) +
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  scale_fill_manual(values = c("#00B52F", "#0000CC", "#3399CC", "#CC0000", "#FF9900")) +
  guides(fill = guide_legend(title = NULL)) + 
  theme_minimal() +
  pretty_plot # + theme(legend.position = "right")

dev.copy(pdf, "../Figures/output_shares.pdf", width = 13.69, height = 8.27)
dev.off()

# Aggregate:

# First check:
get_E_shares(T_ag[, , "2015"], FD_ag[, , "2015"], return.E = TRUE) %>% View


    lapply(setNames(y, y), 
       function(j) get_E_shares(T_ag[, , j], FD_ag[, , j])) %>% 
    unlist2d("Year", "Country", DT = TRUE) %>% 
      colorderv(6:5, pos = "exchange") %>%
    sbt(Country %in% EAC) %>% # Uncomment this to get the full picture..
    melt(1:2, variable.name = "Metric", value.name = "Value") %>%
    tfm(Year = as.integer(Year), 
        Country = mfac(Country)) %>%
      
    ggplot(aes(x = Year, y = Value, color = Metric)) +
      geom_line() +
      facet_wrap( ~ Country, scales = "free_y") + 
      guides(fill = guide_legend(ncol = 1)) + 
      scale_y_continuous(labels = percent, breaks = extended_breaks(7)) + 
      scale_x_continuous(breaks = 2005:2015) + 
      scale_color_manual(values = c("#00B52F", "#0000CC", "#3399CC", "#CC0000", "#FF9900")) +
      guides(color = guide_legend(title = NULL)) + 
      theme_minimal() + pretty_plot + theme(legend.text = element_text(size = 10))
    
    dev.copy(pdf, "../Figures/output_shares_ag_ts.pdf", width = 11.69, height = 8.27)
    dev.off()
    



###################################
# Part 2: Decomposed Flows
###################################

round(range(dec15$B), 5)
    
# Examining VB for EAC:
round(VB_ag[EAC, EAC, ]*100, 2)

# Heatmap of the VB Matrices --------------------------------------------------------
library(pheatmap)

pheatmap(VB_ag[,, "2015"] * 100,
         color = colorRampPalette(brewer.pal(n = 7, name ="YlOrRd"))(100),
         cluster_rows = FALSE, cluster_cols = FALSE, border_color = NA,
         display_numbers = TRUE)

dev.copy(pdf, "../Figures/heatmap_AG_VB.pdf", width = 9, height = 8)
dev.off()

# Disaggregated EAC heatmap: Does not look cool
pheatmap(VB[eac, eac, "2015"] * 100, 
         color = colorRampPalette(brewer.pal(n = 7, name ="YlOrRd"))(100),
         cluster_rows = FALSE, cluster_cols = FALSE, 
         border_color = NA, # legend_breaks = -4:6, 
         fontsize_row = 3.2, fontsize_col = 3.2)  

# dev.copy(pdf, "../Figures/heatmap_EAC_VB", width = 9, height = 8)
# dev.off()

# This gets disaggregated EAC VA Shares:
VB_eac_ag <- VB[, eac, "2015"] %c*% 100 %>% fsum(g)

VB_eac_ag <- table(cr(colnames(VB_eac_ag)))[EAC] %>% cumsum %>%
  lapply(function(i) VB_eac_ag[, seq(i-25L, i)] %>%
           setColnames(crm(colnames(.)))) 

# This plots individual country FULL heatmaps...
for(i in names(VB_eac_ag)) {
  
  pheatmap(VB_eac_ag[[i]],
           color = colorRampPalette(brewer.pal(n = 7, name ="YlOrRd"))(100),
           cluster_rows = FALSE, cluster_cols = FALSE, border_color = NA,
           display_numbers = TRUE)
  
  dev.copy(pdf, paste0("../Figures/heatmap_VB_AG_", i, ".pdf"), width = 10, height = 4)
  dev.off()
  
}

# This plots a combined EAC chart...
unlist2d(VB_eac_ag, "Country", "Source", DT = TRUE, id.factor = TRUE) %>% 
  sbt(Source %in% EAC) %>%
  replace_outliers(0, NA, "min") %>%
  melt(1:2, variable.name = "Sector", value.name = "VA") %>%
  ggplot(aes(x = Sector, y = Source, fill = VA)) + geom_tile() +
  geom_text(aes(label = format(round(VA, 2))), size = 2) +
  facet_wrap( ~ Country, ncol = 1L) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1L) + 
  theme_minimal() + theme(axis.text.x = element_text(
    angle = 315,
    hjust = 0,
    margin = ggplot2::margin(t = 0)
  ))

dev.copy(pdf, "../Figures/heatmap_VB_AG_EAC.pdf", width = 10.69, height = 7.27)
dev.off()


# Decomposed Flows -----------------------------------------------------------------------------------


EAC_VA_shares <- lapply(setNames(y, y), function(i) VB_ag[, EAC, i]) %>% 
  unlist2d("Year", "Share", DT = TRUE) %>% 
  tfm(Year = as.integer(Year),
      Share = mfac(Share)) %>% 
  melt(1:2, NULL, "Country", "Value") 
  

# Plot of VA Shares:  
EAC_VA_shares %>%
  sbt(as.character(Country) != as.character(Share)) %>%
  ggplot(aes(x = Year, y = Value, fill = Share)) +
  geom_area(position = "stack", alpha = 0.8) +
  facet_wrap( ~ Country, scales = "free_y") + 
  guides(fill = guide_legend(ncol = 1)) + 
  scale_y_continuous(labels = percent, #trans = "sqrt",
                     breaks = extended_breaks(10)) + # limits = c(0, 1), 
  scale_x_continuous(breaks = 2005:2015, expand = c(0,0)) + rbsc2 +
  #scale_color_brewer(palette = "Set1") +
  # guides(fill = guide_legend(title = NULL, nrow = 1)) + 
  theme_minimal() + pretty_plot + theme(legend.position = "right")

dev.copy(pdf, "../Figures/VA_shares_ag_ts_area.pdf", width = 11.69, height = 8.27)
dev.off()

# Annualized Growth of VA shares
EAC_VA_shares %>%  
  gby(Share, Country) %>% 
  collapg(list(ffirst, flast), return = "long") %>%
  gby(Share, Country) %>% 
  tfm(Year = qF(Year), 
      Max = fmax(Value, GRP(.), "replace_fill"),
      Growth_A = flast(fgrowth(Value, 10, 1, GRP(.), Year, 
                       power = 1/10), GRP(.), "replace_fill")) %>% 
  fungroup %>% 
  tfm(Country_new = fifelse(as.character(Country) == as.character(Share), 
                    paste0(Country, ' (', round(Growth_A, 1), '%)'), NA_character_)) %>%
  tfm(Country_new = mfac(ffirst(Country_new, Country, "replace_fill"))) %>%
  sbt(as.character(Country) != as.character(Share)) %>%

  ggplot(aes(x = Share, y = Value, fill = Year)) +
  geom_bar(stat = "identity", position = position_dodge()) +
      geom_text(aes(label = paste0(round(Growth_A, 1), '%'), y = Max), 
                vjust = -0.3, size = 2) +
  facet_wrap( ~ Country_new, scales = "free_y") + 
  guides(fill = guide_legend(ncol = 1)) + 
  scale_y_continuous(labels = percent, #trans = "sqrt",
                     breaks = extended_breaks(10)) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() + pretty_plot + theme(legend.position = "right")

dev.copy(pdf, "../Figures/VA_shares_ag_ts_bar.pdf", width = 11.69, height = 8.27)
dev.off()


# Value Added Exports: ------------------------------------------------------------

# Decompositions of Gross Exports -------------------------------------------------
# Gives the VA sources of Each country-industries (using) exports: 
# Note that this has nothing to do with the destination of those exports!! We're just tracing the value-added origins!!
exports_VA <- lapply(decomps, leontief) %>% 
  unlist2d("Year", DT = TRUE) %>% 
  tfmv(is.character, mfac) %>% 
  tfm(Year = as.integer(as_numeric_factor(Year)))

# EAC Total Value Added contained in Global Gross Exports by Source Country-Industry: Stacked Area
exports_VA %>% 
  sbt(Source_Country %in% EAC) %>% 
  gby(Year, Source_Country, Source_Industry) %>% 
  num_vars %>% fsum %>% rm_stub("Source_") %>% 
  
  ggplot(aes(x = Year, y = FVAX, fill = Industry)) +
  geom_area(position = "stack", alpha = 0.8) +
  facet_wrap( ~ Country, scales = "free_y") + 
  guides(fill = guide_legend(ncol = 1)) + 
  scale_y_continuous(labels = mds, breaks = extended_breaks(7)) + 
  scale_x_continuous(breaks = 2005:2015) + rbsc2 +
  theme_minimal() + pretty_plot +
  theme(legend.position = "right")

dev.copy(pdf, "../Figures/VA_exports_stacked_ts.pdf", width = 11.69, height = 8.27)
dev.off()

# EAC Total Value Added contained in Domestic Gross Exports by Using/Exporting Country-Industry: Stacked Area
exports_VA %>% 
  sbt(Source_Country %in% EAC & 
   Source_Country == Using_Country) %>%
   gby(Year, Using_Country, Using_Industry) %>% # This gets rid of innner-country supplies
   num_vars %>% fsum %>% rm_stub("Using_") %>%

  ggplot(aes(x = Year, y = FVAX, fill = Industry)) +
  geom_area(position = "stack", alpha = 0.8) +
  facet_wrap( ~ Country, scales = "free_y") + 
  guides(fill = guide_legend(ncol = 1)) + 
  scale_y_continuous(labels = mds, breaks = extended_breaks(7)) + 
  scale_x_continuous(breaks = 2005:2015) + rbsc2 +
  theme_minimal() + pretty_plot +
  theme(legend.position = "right")

dev.copy(pdf, "../Figures/VA_DOM_exports_stacked_ts.pdf", width = 11.69, height = 8.27)
dev.off()

# EAC Value Added Share in Global Gross Exports to the EAC, excluding any domestic value added in exports absorbed at home   
  VB[, , "2015"] %*% dec15$ESR # Gives country-industry VA origin of exports to each country
  
  exports_VA_Dest <- 
    lapply(decomps, with, copyAttrib(diag(Vc) %*% B %*% ESR, ESR)) %>%
    value2df %>%
    melt(1:3, NULL, "Destination", "FVAX")
  
  exports_VA_Dest %>%
    # Problem: We don't know whether those exports are actually directly exported from the EAC, 
    # we only know the value added origins and the final destination. 
    sbt(Country %in% EAC & as.character(Country) != as.character(Destination)) %>% # Problem: Some of the exports are ultimately absorbed at home, ...
    tfm(FVAX_tot = fsum(FVAX, list(Year, Country), 
                        TRA = "replace_fill")) %>%
    tfm(FVAX_share = FVAX / FVAX_tot) %>%
    sbt(Destination %in% EAC) %>% 
    collapv(1:3, fsum, cols = .c(FVAX, FVAX_share)) %>%
  
  ggplot(aes(x = Year, y = FVAX_share, fill = Sector)) +
  geom_area(position = "stack", alpha = 0.8) +
  facet_wrap( ~ Country, scales = "free_y") + 
  guides(fill = guide_legend(ncol = 1)) + 
  scale_y_continuous(labels = percent, breaks = extended_breaks(7)) + 
  scale_x_continuous(breaks = 2005:2015) + rbsc2 +
  theme_minimal() + pretty_plot +
  theme(legend.position = "right")

dev.copy(pdf, "../Figures/VA_EAC_exports_stacked_ts.pdf", width = 11.69, height = 8.27)
dev.off()

# EAC Value Added Share in Domestic Gross Exports to the EAC, excluding any domestic value added in exports absorbed at home   

lapply(decomps, with, copyAttrib(diag(Vc) %*% B %*% diag(rowSums(ESR[, EAC])), B)[eac, eac] %>% colSums) %>%
  value2df("FVAX") %>% # ...

  ggplot(aes(x = Year, y = FVAX_share, fill = Sector)) +
  geom_area(position = "stack", alpha = 0.8) +
  facet_wrap( ~ Country, scales = "free_y") + 
  guides(fill = guide_legend(ncol = 1)) + 
  scale_y_continuous(labels = percent, breaks = extended_breaks(7)) + 
  scale_x_continuous(breaks = 2005:2015) + rbsc2 +
  theme_minimal() + pretty_plot +
  theme(legend.position = "right")

dev.copy(pdf, "../Figures/VA_EAC_exports_stacked_ts.pdf", width = 11.69, height = 8.27)
dev.off()


# Value Added in EAC exports by country origin
EAC_exports_VA <- 
  exports_VA %>% 
  sbt(Using_Country %in% EAC) %>% 
  gby(Year, Source_Country, Using_Country) %>% 
  num_vars %>% fsum %>% frename(FVAX = EAC_FVAX, Using_Country = Country) # Country is Exporting Country

# Value Added in exports to the EAC by country origin
exports_EAC_VA <- 
  lapply(decomps, with, diag(Vc) %*% B %*% ESR[, EAC] %>% fsum(g)) %>% 
  unlist2d("Year", "Source_Country", DT = TRUE) %>%
  melt(1:2, NULL, "Country", "FVAX_EAC", variable.factor = FALSE) %>%  # Country is Receiving Country
  tfm(Year = as.integer(Year)) 

# EAC shares in EAC Exports and Exports to the EAC, Excluding Domestically Absorbed Exports
merge(EAC_exports_VA, exports_EAC_VA) %>%
  sbt(Source_Country != Country) %>% # Excluding situations where the Exporting or receiving country is the value added origin country (= EAC).. 
  tfm(gvr(.,"FVAX") %>% fsum(list(Year, Country), TRA = "replace_fill") %>% 
        add_stub("_tot", FALSE)) %>%
  sbt(Source_Country %in% EAC) %>%
  tfm(EAC_FVAX_share = EAC_FVAX / EAC_FVAX_tot,
      FVAX_EAC_share = FVAX_EAC / FVAX_EAC_tot) %>%
  gby(Year, Country) %>% 
  gvr("share") %>% fsum %>% rm_stub("_share", FALSE) %>%
  melt(1:2, NULL, "Variable", "Share") %>%
  tfm(Country = factor(Country, levels = EAC)) %>%
  
  ggplot(aes(x = Year, y = Share, color = Variable)) +
  geom_line() +
  facet_wrap( ~ Country, scales = "free_y") + 
  scale_y_continuous(labels = percent, breaks = extended_breaks(7)) + 
  scale_x_continuous(breaks = 2005:2015) + ylab("EAC Share") +
  guides(color = guide_legend(title = NULL)) +
  theme_minimal() + pretty_plot 

dev.copy(pdf, "../Figures/VA_EAC_shares_ts.pdf", width = 11.69, height = 8.27)
dev.off()




# Decompositions of Final Exports -------------------------------------------------

# VA origin of Global final exports 
FD_exports_VA <- 
  lapply(decomps, with, copyAttrib(diag(Vc) %*% B %*% rowSums(Efd), E)) %>%
  value2df("FVAFX") 

# EAC Total Value Added contained in Global Final Exports by Source Country-Industry: Stacked Area
# Note: This still includes exports ultimately consumed at home
FD_exports_VA %>% 
  sbt(Country %in% EAC) %>% 

  ggplot(aes(x = Year, y = FVAFX, fill = Sector)) +
  geom_area(position = "stack", alpha = 0.8) +
  facet_wrap( ~ Country, scales = "free_y") + 
  guides(fill = guide_legend(ncol = 1)) + 
  scale_y_continuous(labels = mds, breaks = extended_breaks(7)) + 
  scale_x_continuous(breaks = 2005:2015) + rbsc2 +
  theme_minimal() + pretty_plot +
  theme(legend.position = "right")

dev.copy(pdf, "../Figures/VA_FD_exports_stacked_ts.pdf", width = 11.69, height = 8.27)
dev.off()

# Value Added in EAC final goods exports by country origin
EAC_FD_exports_VA_orig <- 
  lapply(decomps, with, copyAttrib(diag(Vc) %*% B %*% diag(rowSums(Efd)), B)[, eac] %>% 
         fsum(g) %>% qDT("VA_Origin_Country") %>% 
         melt(1, NULL, "Country", "EAC_FVAFX", variable.factor = FALSE)) %>%  # Here Country is the exporting country
  unlist2d("Year", DT = TRUE) %>% 
  tfm(Country = cr(Country), 
      Year = as.integer(Year)) %>% 
  gby(1:3) %>% fsum

# Value Added in final goods exports to the EAC by country origin
FD_exports_EAC_VA_orig <- 
  lapply(decomps, with, diag(Vc) %*% B %*% Efd[, EAC] %>% fsum(g)) %>% 
  unlist2d("Year", "VA_Origin_Country", DT = TRUE) %>%
  melt(1:2, NULL, "Country", "FVAFX_EAC", variable.factor = FALSE) %>% # Here country is the receiving country
  tfm(Year = as.integer(Year)) 

# EAC shares in EAC Final Exports and Final Exports to the EAC, Excluding Domestically Absorbed Final Exports
merge(EAC_FD_exports_VA_orig, FD_exports_EAC_VA_orig) %>%
  sbt(VA_Origin_Country != Country) %>% # Excluding situations where the Exporting or receiving country is the value added origin country (= EAC).. 
  tfm(gvr(.,"FVAFX") %>% fsum(list(Year, Country), TRA = "replace_fill") %>% 
      add_stub("_tot", FALSE)) %>%
  sbt(VA_Origin_Country %in% EAC) %>%
  tfm(EAC_FVAFX_share = EAC_FVAFX / EAC_FVAFX_tot,
      FVAFX_EAC_share = FVAFX_EAC / FVAFX_EAC_tot) %>%
  gby(Year, Country) %>% 
  gvr("share") %>% fsum %>% rm_stub("_share", FALSE) %>%
  melt(1:2, NULL, "Variable", "Share") %>%
  tfm(Country = factor(Country, levels = EAC)) %>%
 
  ggplot(aes(x = Year, y = Share, color = Variable)) +
  geom_line() +
  facet_wrap( ~ Country, scales = "free_y") + 
  scale_y_continuous(labels = percent, breaks = extended_breaks(7)) + 
  scale_x_continuous(breaks = 2005:2015) + ylab("EAC Share") +
  guides(color = guide_legend(title = NULL)) +
  theme_minimal() + pretty_plot 

dev.copy(pdf, "../Figures/VA_FD_EAC_shares_ts.pdf", width = 11.69, height = 8.27)
dev.off()







# Now some more GVC Indicators ----------------------------------------------------------
library(gvc)    
library(decompr)

    
# Vertical Specialization (VS): The Import share of Exports / output: Backward GVC Integration
VS <- lapply(decomps, with, colSums(Vc * Bm))
# Same thing (I checked it):
VS_df <- lapply(decomps, function(x) tfm(i2e(leontief(x)), I2E = i2e / x$E)) %>%
         unlist2d("Year", DT = TRUE) %>% tfm(Year = as.integer(Year)) %>%
         frename(country = Country, sector = Sector)
# Aggregate Version:
VS_df_ag <- VS_df[exports, on = .(Year, Country, Sector)] %>% 
  gby(Year, Country) %>% nv %>% fmean(Exports, keep.w = FALSE) 

# VS1: Export to Re-Export Share: Forward GVC Integration
VS1 <- lapply(decomps, with, rowSums(Vc * Bm %r*% E) / E) # = The sum of value added going into other countries exports, divided by own exports
# Same thing (I checked it): all.equal(unattrib(unlist(VS1)), unattrib(VS1_df$E2R))
VS1_df <- lapply(decomps, function(x) tfm(e2r(leontief(x)), E2R = e2r / x$E)) %>%
          unlist2d("Year", DT = TRUE) %>% tfm(Year = as.integer(Year)) %>%
          frename(country = Country, sector = Sector)
# Aggregate VS1:
VS1_df_ag <- VS1_df[exports, on = .(Year, Country, Sector)] %>% 
  gby(Year, Country) %>% nv %>% fmean(Exports, keep.w = FALSE) 



# Aggregate Plot (ts)
VS_df_ag %>% slt(-i2e) %>% 
  av(VS1_df_ag %>% slt(E2R)) %>%
  frename(I2E = "I2E (VS)    ", E2R = "E2R (VS1)") %>%
  melt(1:2, NULL, "Variable", "Value") %>% 
  sbt(Country %in% EAC) %>%
  tfm(Country = factor(Country, levels = EAC)) %>%

  ggplot(aes(x = Year, y = Value, color = Variable)) +
  geom_line() +
  facet_wrap( ~ Country, scales = "fixed") + 
  guides(fill = guide_legend(ncol = 1)) + 
  scale_y_continuous(labels = percent, 
                     breaks = extended_breaks(10)) + 
  scale_x_continuous(breaks = 2005:2015) + # rbsc +
  scale_color_brewer(palette = "Set1") +
  guides(color = guide_legend(title = NULL, nrow = 1)) + 
  theme_minimal() + pretty_plot 

dev.copy(pdf, "../Figures/VS_ag_ts.pdf", width = 11.69, height = 8.27)
dev.off()

# Sector Level Plot: 2015
VS_df %>% slt(-i2e) %>% 
  av(VS1_df %>% slt(E2R)) %>%
  frename(I2E = "I2E (VS)    ", E2R = "E2R (VS1)") %>%
  melt(1:3, NULL, "Variable", "Value") %>% 
  sbt(Country %in% EAC) %>%
  collapv(2:4, flast, cols = "Value", sort = FALSE) %>% 
  tfm(Country = factor(Country, levels = EAC),
      Sector = mfac(Sector)) %>%
  replace_outliers(c(0, 1)) %>%
  
  ggplot(aes(x = Sector, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(Value * 100)),
            vjust = -0.3, size = 3, position = position_dodge(0.9)) +
  facet_wrap( ~ Country, ncol = 1) + 
  scale_y_continuous(labels = percent, 
                     breaks = extended_breaks(7)) + 
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(title = NULL, nrow = 1)) + 
  theme_minimal() + pretty_plot 

dev.copy(pdf, "../Figures/VS.pdf", width = 13.69, height = 8.27)
dev.off()


# Sector Level Plot: Average Growth Rate
VS_df %>% slt(-i2e) %>% 
  av(VS1_df %>% slt(E2R)) %>%
  frename(I2E = "I2E (VS)    ", E2R = "E2R (VS1)") %>%
  melt(1:3, NULL, "Variable", "Value") %>% 
  sbt(Country %in% EAC) %>%
  gby(2:4) %>%
  fgrowth(10, 1, Year, power = 1/10, scale = 1) %>% 
  fungroup %>% na_omit %>%
  tfm(Country = factor(Country, levels = EAC),
      Sector = mfac(Sector)) %>%
  replace_outliers(c(-1, 1)) %>% {
  
  ggplot(., aes(x = Sector, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  # geom_text(aes(label = round(Value * 100, 1)),
  #           vjust = -fifelse(.$Value >= 0, -0.3, 1.3), 
  #           size = 3, position = position_dodge(width = 1)) +
  facet_wrap( ~ Country, ncol = 1, scales = "free_y") + 
  scale_y_continuous(labels = function(x) percent(x, accuracy = 1), 
                     breaks = extended_breaks(7)) + 
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(title = NULL, nrow = 1)) + 
  theme_minimal() + pretty_plot 
}
dev.copy(pdf, "../Figures/VS_growth.pdf", width = 13.69, height = 8.27)
dev.off()

# Regional Integration in VA Trade ----------------------------------------------------------

# (1) EAC Share in VS
VS_EAC <- 
  exports_VA %>% 
  # Excluding Own country VAE
  sbt(Using_Country %in% EAC & Source_Country != Using_Country) %>% 
  # Computing Total foreign VAE
  tfm(FVAX_tot = fsum(FVAX, list(Year, Using_Country, Using_Industry), TRA = 1L)) %>% 
  # Getting EAC source and aggregating
  sbt(Source_Country %in% EAC) %>% 
  gby(Year, Using_Country, Using_Industry) %>% nv %>% 
  frename(FVAX = FVAX_EAC) %>%
  collapg(custom = list(fsum = "FVAX_EAC", flast = "FVAX_tot")) %>%
  # Computing VS_EAC
  tfm(VS_EAC = FVAX_EAC / FVAX_tot)

VS_EAC_ag <- VS_EAC %>% gby(1:2) %>% gvr("FVAX") %>% fsum %>%
                tfm(VS_EAC = FVAX_EAC / FVAX_tot)

# (2) EAC Share in VS1
VS1_EAC <- 
  exports_VA %>% 
  # Excluding Own country VAE
  sbt(Source_Country %in% EAC & Source_Country != Using_Country) %>% 
  # Computing Total foreign VAE
  tfm(tot_FVAX = fsum(FVAX, list(Year, Source_Country, Source_Industry), TRA = 1L)) %>% 
  # Getting EAC using and aggregating
  sbt(Using_Country %in% EAC) %>% 
  gby(Year, Source_Country, Source_Industry) %>% nv %>% 
  frename(FVAX = EAC_FVAX) %>%
  collapg(custom = list(fsum = "EAC_FVAX", flast = "tot_FVAX")) %>%
  # Computing VS1_EAC
  tfm(VS1_EAC = EAC_FVAX / tot_FVAX)

VS1_EAC_ag <- VS1_EAC %>% gby(1:2) %>% gvr("FVAX") %>% fsum %>%
                 tfm(VS1_EAC = EAC_FVAX / tot_FVAX)


# Value Added in exports to the EAC by country origin
exports_EAC_VA <- 
  lapply(decomps, with, diag(Vc) %*% B %*% ESR[, EAC] %>% fsum(g)) %>% 
  unlist2d("Year", "Source_Country", DT = TRUE) %>%
  melt(1:2, NULL, "Country", "FVAX_EAC", variable.factor = FALSE) %>%  # Country is Receiving Country
  tfm(Year = as.integer(Year)) 

VAI_EAC <- 
  exports_EAC_VA %>% 
  sbt(Source_Country != Country) %>%
  tfm(FVAX_EAC_tot = fsum(FVAX_EAC, list(Year, Country), TRA = 1)) %>%
  sbt(Source_Country %in% EAC) %>% 
  gby(Year, Country) %>% 
  collapg(custom = list(fsum = "FVAX_EAC", flast = "FVAX_EAC_tot")) %>%
  tfm(VAI = FVAX_EAC / FVAX_EAC_tot)
  

# Value Added in final goods exports to the EAC by country origin
FD_exports_EAC_VA <- 
  lapply(decomps, with, diag(Vc) %*% B %*% Efd[, EAC] %>% fsum(g)) %>% 
  unlist2d("Year", "Source_Country", DT = TRUE) %>%
  melt(1:2, NULL, "Country", "FVAFX_EAC", variable.factor = FALSE) %>% # Country is Receiving Country
  tfm(Year = as.integer(Year)) 


VAFI_EAC <- 
  FD_exports_EAC_VA %>% 
  sbt(Source_Country != Country) %>%
  tfm(FVAFX_EAC_tot = fsum(FVAFX_EAC, list(Year, Country), TRA = 1)) %>%
  sbt(Source_Country %in% EAC) %>% 
  gby(Year, Country) %>% 
  collapg(custom = list(fsum = "FVAFX_EAC", flast = "FVAFX_EAC_tot")) %>%
  tfm(VAFI = FVAFX_EAC / FVAFX_EAC_tot)


merge(VS_EAC_ag %>% rm_stub("Using_"), 
      VS1_EAC_ag %>% rm_stub("Source_")) %>%
  gvr("Year|Country|VS") %>% rm_stub("_EAC", FALSE) %>%
  merge(VAI_EAC %>% gvr("Year|Country|VAI")) %>%
  merge(VAFI_EAC %>% gvr("Year|Country|VAFI")) %>%
  melt(1:2, NULL, "Variable", "Share") %>%
  tfm(Country = factor(Country, levels = EAC)) %>%
  
  ggplot(aes(x = Year, y = Share, color = Variable)) +
  geom_line() +
  facet_wrap( ~ Country, scales = "free_y") + 
  scale_y_continuous(labels = percent, breaks = extended_breaks(7)) + 
  scale_x_continuous(breaks = 2005:2015) + ylab("EAC Share") +
  guides(color = guide_legend(title = NULL)) +
  theme_minimal() + pretty_plot 

dev.copy(pdf, "../Figures/VA_EAC_shares_ts.pdf", width = 11.69, height = 8.27)
dev.off()


















# KWW Decomposition ------------------------------------------------------------------------------

kww2 <- function(x, ...) wwz2kww(wwz(x), )

KWW_ts <- lapply(decomps, kww) %>% unlist2d("Year") %>%
          tfm(Year = as.integer(Year)) %>% qDT %>%
          melt(1:2, variable.name = "Term")

KWW_ts %>% 
  sbt(Country %in% EAC) %>%

  ggplot(aes(x = Year, y = value, fill = Term)) +
  geom_area(position = "fill", alpha = 0.8) +
  facet_wrap( ~ Country, scales = "free_y") +
  guides(fill = guide_legend(ncol = 1)) +
  scale_y_continuous(labels = percent, breaks = extended_breaks(7), 
                     expand = c(0, 0.02)) +
  scale_x_continuous(breaks = 2005:2015, expand = c(0, 0)) + rbsc2 +
  ylab("VA Share in Gross Exports") +
  theme_minimal() + pretty_plot +
  theme(legend.position = "right")
  
  # ggplot(aes(x = Year, y = value, fill = Term)) +
  # geom_area(position = "stack", alpha = 0.8) +
  # facet_wrap( ~ Country, scales = "free_y") +
  # guides(fill = guide_legend(ncol = 1)) +
  # scale_y_continuous(labels = mds, breaks = extended_breaks(7)) +
  # scale_x_continuous(breaks = 2005:2015) + rbsc2 + ylab("Value Added") +
  # theme_minimal() + pretty_plot +
  # theme(legend.position = "right")

dev.copy(pdf, "../Figures/KWW_fill_ts.pdf", width = 11.69, height = 8.27)
dev.off()


KWW_ts_det <- lapply(decomps, kww2) %>% unlist2d("Year") %>%
              tfm(Year = as.integer(Year)) %>% qDT %>%
              melt(1:4, variable.name = "Term")

# Only EAC
KWW_ts_det %>% 
  sbt(Exporting_Country %in% EAC & 
      Importing_Country %in% EAC) %>%
  gby(Year, Exporting_Country, Term) %>%
  num_vars %>% fsum %>%
  rm_stub("Exporting_") %>%
  
  ggplot(aes(x = Year, y = value, fill = Term)) +
  geom_area(position = "fill", alpha = 0.8) +
  facet_wrap( ~ Country, scales = "free_y") +
  guides(fill = guide_legend(ncol = 1)) +
  scale_y_continuous(labels = percent, breaks = extended_breaks(7), 
                     expand = c(0, 0.02)) +
  scale_x_continuous(breaks = 2005:2015, expand = c(0, 0)) + rbsc2 +
  ylab("VA Share in Gross Exports to the EAC") +
  theme_minimal() + pretty_plot +
  theme(legend.position = "right")

# ggplot(aes(x = Year, y = value, fill = Term)) +
# geom_area(position = "stack", alpha = 0.8) +
# facet_wrap( ~ Country, scales = "free_y") +
# guides(fill = guide_legend(ncol = 1)) +
# scale_y_continuous(labels = mds, breaks = extended_breaks(7)) +
# scale_x_continuous(breaks = 2005:2015) + rbsc2 + ylab("Value Added") +
# theme_minimal() + pretty_plot +
# theme(legend.position = "right")

dev.copy(pdf, "../Figures/KWW_fill_ts_EAC.pdf", width = 11.69, height = 8.27)
dev.off()

# Sector 2015
KWW_ts_det %>% 
  sbt(Exporting_Country %in% EAC & 
      Year == 2015L) %>%
  gby(Exporting_Country, Exporting_Industry, Term) %>%
  slt(value) %>% fsum %>%
  rm_stub("Exporting_") %>% 
  replace_outliers(0, NA, "min") %>%
  
  ggplot(aes(x = value, y = factor(Industry, levels = rev(levels(Industry))), fill = Term)) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.8) +
  facet_wrap( ~ Country, scales = "free_y") +
  guides(fill = guide_legend(ncol = 1)) +
  scale_x_continuous(labels = percent, breaks = extended_breaks(7), 
                     expand = c(0, 0.02)) +
  scale_y_discrete(expand = c(0, 0)) + rbsc2 +
  labs(x = "VA Share in Gross Exports", y = "Value") +
  theme_minimal() + pretty_plot +
  theme(legend.position = "right", 
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 3.5)))

# ggplot(aes(x = Year, y = value, fill = Term)) +
# geom_area(position = "stack", alpha = 0.8) +
# facet_wrap( ~ Country, scales = "free_y") +
# guides(fill = guide_legend(ncol = 1)) +
# scale_y_continuous(labels = mds, breaks = extended_breaks(7)) +
# scale_x_continuous(breaks = 2005:2015) + rbsc2 + ylab("Value Added") +
# theme_minimal() + pretty_plot +
# theme(legend.position = "right")

dev.copy(pdf, "../Figures/KWW_fill_sec.pdf", width = 11.69, height = 8.27)
dev.off()

# Upstreamness and Downstramness based in KWW Decomposition ------------------------------------------------------------------------------


# Aggregate  ------------------------------------------------
KWW_ts %>% 
  sbt(Country %in% EAC) %>%
  dcast(Year + Country ~ Term) %>% 
  fcompute(Year = Year, Country = Country,
           `Upstreamness       ` = (DVA_INT + DVA_INTrex + DDC) / (DVA_FIN + DVA_INT + DVA_INTrex + RDV_FIN + RDV_INT + DDC),
           Downstreamness = FVA_FIN / (FVA_FIN + FVA_INT + FDC)) %>% # Why ??
  melt(1:2, variable.name = "Variable", value.name = "Value") %>% 
  
  ggplot(aes(x = Year, y = Value, color = Variable)) +
  geom_line() +
  facet_wrap( ~ Country, scales = "fixed") + 
  guides(fill = guide_legend(ncol = 1)) + 
  scale_y_continuous(labels = percent, 
                     breaks = extended_breaks(10)) + 
  scale_x_continuous(breaks = 2005:2015) + # rbsc +
  scale_color_brewer(palette = "Dark2") +
  guides(color = guide_legend(title = NULL, nrow = 1)) + 
  theme_minimal() + pretty_plot 

dev.copy(pdf, "../Figures/UP_DOWN_ag_ts.pdf", width = 11.69, height = 8.27)
dev.off()


KWW_ts %>% 
  sbt(Country %in% EAC) %>%
  dcast(Year + Country ~ Term) %>% 
  fcompute(Year = Year, Country = Country,
           `Upstreamness       ` = (DVA_INT + DVA_INTrex + DDC) / (DVA_FIN + DVA_INT + DVA_INTrex + RDV_FIN + RDV_INT + DDC),
           Downstreamness = FVA_FIN / (FVA_FIN + FVA_INT + FDC)) %>% # Why ??
  melt(1:2, variable.name = "Variable", value.name = "Value") %>% 
  gby(Country, Variable) %>% fdiff(10, 1, Year) %>% 
  fungroup %>% na_omit %>% {
    
    ggplot(., aes(x = Country, y = Value, fill = Variable)) +
      geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8) +
      geom_text(aes(label = percent(Value, 0.1)), 
                vjust = ifelse(.$Value > 0, -0.3, 1.3), position = position_dodge(0.9)) +
      scale_y_continuous(labels = percent, 
                         breaks = extended_breaks(10)) + 
      scale_fill_brewer(palette = "Dark2") +
      guides(fill = guide_legend(title = NULL, nrow = 1)) + 
      theme_minimal() + pretty_plot + theme(axis.text.x = element_text(angle = 0))
  }

dev.copy(pdf, "../Figures/UP_DOWN_ag_growth.pdf", width = 8, height = 4.27)
dev.off()

# Disaggregated  ------------------------------------------------
KWW_ts_det %>% 
  sbt(Exporting_Country %in% EAC) %>%
  gby(-Importing_Country, -value) %>% 
  nv %>% fsum %>% rm_stub("Exporting_") %>%
  dcast(... ~ Term, value.var = "value") %>% 
  fcompute(Year = Year, Country = Country, Industry = factor(Industry, levels = rev(levels(Industry))), 
           Upstreamness = (DVA_INT + DVA_INTrex + DDC) / (DVA_FIN + DVA_INT + DVA_INTrex + RDV_FIN + RDV_INT + DDC),
           Downstreamness = FVA_FIN / (FVA_FIN + FVA_INT + FDC)) %>% # Why ??
  collapv(2:3, list(ffirst, flast), return = "long") %>% 
  slt(-Function) %>%
  melt(1:3, variable.name = "Variable", value.name = "Value") %>% 
  tfm(Text = recode_char(paste(Variable, Year), "Downstreamness 2015" = "Downstreamness 2015             ")) %>%  

  ggplot(aes(x = Value, y = Industry, color = Text)) +
  geom_point() +
  facet_wrap( ~ Country, scales = "free_y") + 
  scale_x_continuous(labels = function(x) percent(x, 1.0), 
                     breaks = extended_breaks(10)) + 
  scale_color_brewer(palette = "Paired") +
  guides(color = guide_legend(title = NULL, nrow = 1)) + 
  theme_minimal() + pretty_plot 

dev.copy(pdf, "../Figures/UP_DOWN_sec.pdf", width = 11.69, height = 8.27)
dev.off()


# NRCA ------------------------------------------------------------------------------------------------

KWW_ts_det_wide <- lapply(decomps, kww2) %>% unlist2d("Year") %>%
                   tfm(Year = as.integer(Year)) %>% qDT %>%
                   gby(Year, Exporting_Country, Exporting_Industry) %>%
                   num_vars %>% fsum


settransform(KWW_ts_det_wide, VAE = DVA_FIN + DVA_INT + DVA_INTrex + RDV_FIN + RDV_INT) # = GDP in exports, not matter where they are absorbed
settransform(KWW_ts_det_wide, VAE_s = VAE / fsum(VAE, list(Year, Exporting_Country), TRA = "replace_fill"))
settransform(KWW_ts_det_wide, VAE_Ws = fsum(VAE, list(Year, Exporting_Industry), TRA = "replace_fill") / 
                                       fsum(VAE, Year, TRA = "replace_fill"))

settransform(KWW_ts_det_wide, NRCA = VAE_s / VAE_Ws)

KWW_ts_det_wide %>% 
  rm_stub("Exporting_") %>% 
  sbt(Year == 2015L & Country %in% EAC, Year:Industry, NRCA) %>% 
  tfm(Industry = factor(Industry, levels = rev(levels(Industry)))) %>%

  ggplot(aes(y = Industry, x = NRCA)) + # , fill = Industry
  geom_bar(stat = "identity", alpha = 0.8) +
  facet_wrap( ~ Country, scales = "free_y") +
  guides(fill = FALSE) +
  scale_x_continuous(trans = "log10", breaks = log_breaks(10), limits = c(0.01, 100),
                     expand = c(0, 0.02), labels = function(x) signif(x, 3)) +
  scale_y_discrete(expand = c(0, 0)) + xlab("NRCA") + # rbsc2 +
  theme_minimal() + pretty_plot +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 3.5))) #,
        # panel.spacing.x = unit(2, "lines"))
        

dev.copy(pdf, "../Figures/NRCA.pdf", width = 11.69, height = 8.27)
dev.off()

KWW_ts_det_wide %>% 
  rm_stub("Exporting_") %>% 
  sbt(Country %in% EAC, Year:Industry, NRCA) %>% 
  
  ggplot(aes(x = Year, y = NRCA, colour = Industry)) +
  geom_line() +
  facet_wrap( ~ Country, scales = "fixed") +
  guides(colour = guide_legend(ncol = 1)) +
  scale_y_continuous(trans = "log10", breaks = log_breaks(10), 
                     expand = c(0, 0.02), labels = function(x) signif(x, 3)) +
  scale_x_continuous(breaks = 2005:2015, expand = c(0, 0)) + rbsc2 +
  theme_minimal() + pretty_plot +
  theme(legend.position = "right") 

dev.copy(pdf, "../Figures/NRCA_ts.pdf", width = 11.69, height = 8.27)
dev.off()

# NRCA relative to EAC

KWW_ts_det_wide[Exporting_Country %in% EAC, 
                VAE_EACs := fsum(VAE, list(Year, Exporting_Industry), TRA = "replace_fill") / 
                            fsum(VAE, Year, TRA = "replace_fill")]

KWW_ts_det_wide[, NRCA_EAC := VAE_s / VAE_EACs]

KWW_ts_det_wide %>% 
  rm_stub("Exporting_") %>% 
  sbt(Year == 2015L & Country %in% EAC, Year:Industry, NRCA_EAC) %>% 
  tfm(Industry = factor(Industry, levels = rev(levels(Industry)))) %>%
  
  ggplot(aes(y = Industry, x = NRCA_EAC)) + # , fill = Industry
  geom_bar(stat = "identity", alpha = 0.8) +
  facet_wrap( ~ Country, scales = "free_y") +
  guides(fill = FALSE) +
  scale_x_continuous(trans = "log10", breaks = log_breaks(6), limits = c(0.03, 30),
                     expand = c(0, 0.02), labels = function(x) signif(x, 3)) +
  scale_y_discrete(expand = c(0, 0)) + xlab("NRCA") + # rbsc2 +
  theme_minimal() + pretty_plot +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 3.5))) #,
  

dev.copy(pdf, "../Figures/NRCA_EAC.pdf", width = 11.69, height = 8.27)
dev.off()

KWW_ts_det_wide %>% 
  rm_stub("Exporting_") %>% 
  sbt(Country %in% EAC, Year:Industry, NRCA_EAC) %>% 
  
  ggplot(aes(x = Year, y = NRCA_EAC, colour = Industry)) +
  geom_line() +
  facet_wrap( ~ Country, scales = "fixed") +
  guides(colour = guide_legend(ncol = 1)) +
  scale_y_continuous(trans = "log10", breaks = log_breaks(10), limits = c(0.03, 20), 
                     expand = c(0, 0.02), labels = function(x) signif(x, 3)) +
  scale_x_continuous(breaks = 2005:2015, expand = c(0, 0)) + rbsc2 +
  theme_minimal() + pretty_plot +
  theme(legend.position = "right") 

dev.copy(pdf, "../Figures/NRCA_EAC_ts.pdf", width = 11.69, height = 8.27)
dev.off()

# NRCA in Inner-EAC Trade

KWW_ts_det_EAC <- lapply(decomps, kww2) %>% unlist2d("Year") %>%
                  tfm(Year = as.integer(Year)) %>% 
                  sbt(Exporting_Country %in% EAC & Importing_Country %in% EAC) %>% 
                  gby(Year, Exporting_Country, Exporting_Industry) %>%
                  num_vars %>% fsum %>% setDT 
                  

settransform(KWW_ts_det_EAC, VAE = DVA_FIN + DVA_INT + DVA_INTrex + RDV_FIN + RDV_INT) # = GDP in exports, not matter where they are absorbed
settransform(KWW_ts_det_EAC, VAE_s = VAE / fsum(VAE, list(Year, Exporting_Country), TRA = "replace_fill"))
settransform(KWW_ts_det_EAC, VAE_Ws = fsum(VAE, list(Year, Exporting_Industry), TRA = "replace_fill") / 
                                      fsum(VAE, Year, TRA = "replace_fill"))

settransform(KWW_ts_det_EAC, NRCA = VAE_s / VAE_Ws)


KWW_ts_det_EAC %>% 
  rm_stub("Exporting_") %>% 
  sbt(Year == 2015L, Year:Industry, NRCA) %>% 
  tfm(Industry = factor(Industry, levels = rev(levels(Industry)))) %>%
  
  ggplot(aes(y = Industry, x = NRCA)) + # , fill = Industry
  geom_bar(stat = "identity", alpha = 0.8) +
  facet_wrap( ~ Country, scales = "free_y") +
  guides(fill = FALSE) +
  scale_x_continuous(trans = "log10", breaks = log_breaks(6), limits = c(0.01, 100),
                     expand = c(0, 0.02), labels = function(x) signif(x, 3)) +
  scale_y_discrete(expand = c(0, 0)) + xlab("NRCA") + # rbsc2 +
  theme_minimal() + pretty_plot +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 3.5))) #,

dev.copy(pdf, "../Figures/NRCA_IEAC.pdf", width = 11.69, height = 8.27)
dev.off()

KWW_ts_det_EAC %>% 
  rm_stub("Exporting_") %>% 

  ggplot(aes(x = Year, y = NRCA, colour = Industry)) +
  geom_line() +
  facet_wrap( ~ Country, scales = "fixed") +
  guides(colour = guide_legend(ncol = 1)) +
  scale_y_continuous(trans = "log10", breaks = log_breaks(10), # limits = c(0.03, 20), 
                     expand = c(0, 0.02), labels = function(x) signif(x, 3)) +
  scale_x_continuous(breaks = 2005:2015, expand = c(0, 0)) + rbsc2 +
  theme_minimal() + pretty_plot +
  theme(legend.position = "right") 

dev.copy(pdf, "../Figures/NRCA_IEAC_ts.pdf", width = 11.69, height = 8.27)
dev.off()





##########################################
# Part 3: GVC's and Industrial Development
##########################################
# (a) The Effects of GVC Integration on GDP at the sector level: -----------------------------------------------

names(y) <- y
data <- lapply(y, function(i) va[, i]) %>% # Same as: lapply(decomps, with, Vc * X) (I checked)
        value2df("VA") %>%
        merge(value2df(lapply(y, function(i) VA[, , i]))) %>% # Disaggregated VA
        tfm(VA_SUM = COE + TAX + SUB + NOS + NMI + COF, 
            COE_NOS = COE + NOS) %>%
        tfm(slt(., COE, TAX, SUB, NOS, NMI, COF, COE_NOS) %c/% 
            VA_SUM %>% add_stub("_S", FALSE)) %>% # Adding Shares
        merge(VS_df) %>% 
        merge(VS1_df) %>%
        merge(exports) %>%
        tfm(DVA_Exports = Exports - i2e)

# Saving so does not get overwritten
VA_array <- VA

data %>% sbt(Country %in% EAC) %>% 
  gby(Country, Sector) %>% slt(-Year) %>%
  W(keep.group_vars = FALSE) %>% pwcor(P = TRUE)

data %>% sbt(Country %in% EAC) %>% 
  tfmv(is.character, qF) %>% 
  HDW(~ Country:Sector + Country:Year + Sector:Year) %>% pwcor(P = TRUE)

data %>% sbt(Country %in% EAC) %>% 
  G(1,1, ~ Country + Sector, ~ Year, keep.ids = FALSE) %>% pwcor(P = TRUE)

data %>% sbt(Country %in% EAC) %>% 
  G(1, 1, ~ Country + Sector, ~ Year) %>% 
  tfmv(is.character, qF) %>% 
  HDW(~ Country:Sector + Country:Year + Sector:Year) %>% pwcor(P = TRUE)


data %>% sbt(Country %in% EAC) %>% 
  G(1,1, ~ Country + Sector, ~ Year, stubs = FALSE) %>% 
  na_omit %>%
  .[, qDT(cor(cbind(VA, I2E, E2R, Exports)), "Variable"), 
    by = .(Country, Sector)] -> data_G
# Plot Correlations
data_G %>% melt(1:3) %>%
  ggplot(aes(x = variable, y = Variable, fill = value)) + 
  geom_tile() + 
  facet_grid(Country ~ Sector) + pretty_plot

data %>% sbt(Country %in% EAC) %>% 
  # tfm(VA = log(VA), DVA_Exports = log(DVA_Exports)) %>%
  G(1,1, ~ Country + Sector, ~ Year, stubs = FALSE) %>% 
  na_omit %>% # sbt(I2E > 0.1) %>%
  .[, mrtl(flm(DVA_Exports[-1], cbind(Intercept = 1, L(I2E, 0:1), L(E2R, 0:1))[-1, ]), TRUE), # , e2r, Exports # , Exports
    by = .(Country, Sector)] -> data_VS_effect

fmedian(nv(data_VS_effect))
# Check robustness to country FE's and different specifications ??? -> just do one panel-regression in FD. 
# See also Kumritz specification...

# Plot Effects
data_VS_effect %>% sbt(Country != "SSD", -Intercept) %>% 
  #replace_outliers(c(-5, 5)) %>%
  melt(1:2, NULL, "Variable", "VA") %>%
  ggplot(aes(x = Sector, y = Variable, fill = VA)) + 
  geom_tile() + 
  geom_text(aes(label = format(round(VA, 2))), size = 2) +
  facet_wrap( ~ Country, ncol = 1L) +
  scale_fill_gradient2(low = "blue", high = "red") + 
  # scale_fill_distiller(palette = "YlOrRd", direction = 1L) + 
  theme_minimal() + pretty_plot

fmedian(nv(data_VS_effect), w = data[Country %in% EAC, last(Exports), by = .(Country, Sector)][, V1])

# Now doing the real thing: -----------------------------------------------------------------------------

library(plm)
library(data.table)
between <- data.table::between
library(collapse)
library(magrittr)
library(stargazer)

setwd("GVC EAC Paper")

data %>% sbt(Country %in% setdiff(EAC, "SSD") & (!between(I2E, 0, 1) | !between(E2R, 0, 1))) %T>% 
  View %>% gby(Country, Sector) %>% smr(N = fnobs(Year), I2E = fmedian(I2E), E2R = fmedian(E2R))

data %>% sbt(Country %in% setdiff(EAC, "SSD") & Sector %!in% c("REC", "REI", "FIB", "EGW") & (!between(I2E, 0, 1) | !between(E2R, 0, 1))) %T>% 
  View %>% gby(Country, Sector) %>% smr(N = fnobs(Year), I2E = fmedian(I2E), E2R = fmedian(E2R))

# Excluded Sectors Table:
data %>% sbt(Country %in% setdiff(EAC, "SSD") & 
             (Sector %in% c("REC", "REI", "FIB", "EGW") |
              (Sector %in% c("OTH", "PHH") & Country == "KEN"))) %>%
  qsu(I2E + E2R ~ Sector, array = FALSE) %>% 
  unlist2d("GVC Measure", "Sector") %T>%
  stargazer(summary = FALSE, 
            rownames = FALSE,
            out = "Tables/EXCL_SEC.tex") %>% print

# Analysis data frame

pserify <- function(pdata) {
  ix <- attr(pdata, "index")
  nam <- attr(pdata, "row.names")
  mkps <- function(x) {
    names(x) <- nam
    attr(x, "index") <- ix
    oldClass(x) <- c("pseries", class(x))
    x
  }
  dapply(pdata, mkps)
}
ss_pseries <- function(x, i, drop = TRUE) { # `[.pseries`
  xi <- .subset(x, i)
  ss_ix <- ss(attr(x, "index"), i)
  attr(xi, "index") <- if(drop) fdroplevels.data.frame(ss_ix) else ss_ix
  oldClass(xi) <- oldClass(x)
  xi
}
# rm(`[.pseries`)
# print.pdata.frame <- function(x, ...) {
#   attr(x, "index") <- NULL
#   class(x) <- "data.frame"
#   rm_ps <- function(z) {
#     if(!inherits(z, "pseries")) return(z)
#     clz <- oldClass(z)
#     attr(z, "index") <- NULL
#     oldClass(z) <- clz[clz != "pseries"]
#     z
#   }
#   x <- dapply(x, rm_ps)
#   if (any(duplicated(rownames(x)))) {
#     print("Note: pdata.frame contains duplicated row names, thus original row names are not printed")
#     rownames(x) <- NULL
#   }
#   print(x, ...)
# }

moddat <- data %>% sbt(Country %in% setdiff(EAC, "SSD") & 
                       Sector %!in% c("REC", "REI", "FIB", "EGW") &
                       !(Sector %in% c("OTH", "PHH") & Country == "KEN")) %>%
           tfm(cs = finteraction(Country, Sector), 
               cy = finteraction(Country, Year),
               sy = finteraction(Sector, Year)) %>%
             frename(DVA_Exports = DVA_EX) %>%
             fdroplevels %>%
             pdata.frame(index = c('cs', 'Year')) %>% pserify


# rm(list = names(moddat))
# detach(moddat)
# attach(moddat)

# Summary statistics
moddat %>% slt(VA, DVA_EX, I2E, E2R) %>% 
  dapply(unclass) %>% qsu(array = FALSE) %>% 
  tfm(VA = round(VA), DVA_EX = round(DVA_EX)) %>%
  unlist2d("Variable", "Trans") %T>%
  stargazer(summary = FALSE, 
            rownames = FALSE,
            digits = 2, digits.extra = 0,
            out = "Tables/SUMMARY_VA_GROWTH.tex") %>% print

MAN <- .c(FBE, TEX, WAP, PCM, MPR, ELM, TEQ, MAN)

with(moddat, {
# Histograms: https://stackoverflow.com/questions/3541713/how-to-plot-two-histograms-together-in-r
oldpar <- par(mfrow = c(1, 3), mar = c(2.5, 4, 2.1, 0), lwd = 0.5) # bottom, left, top, right
# VA
hist(log10(VA), breaks = seq(2.5,7.5,0.2), xlab = NA, main = expression('log'[10](VA)), xlim = c(2.5, 7.5))
hist(log10(VA[Sector %in% MAN]), breaks = seq(2.5,7.5,0.2), xlab = NA, xlim = c(2.5, 7.5), 
     col = "orange", add = TRUE)
abline(v = fmedian(log10(VA)), lwd = 1.5)
abline(v = fmedian(log10(VA[Sector %in% MAN])), col = "red", lwd = 1.5)
legend("topleft", c("Overall", "Manufacturing"), lty = 1, lwd = 1.5,
       col = c("black", "red"), bty = "n", y.intersp = 2, seg.len = 1)
# I2E
hist(I2E, breaks = seq(0,0.75,0.025), xlab = NA, main = expression('I2E'), xaxt = 'n')
axis(side = 1, at = seq(0, 0.70, 0.10)) # https://stackoverflow.com/questions/25997337/in-r-how-to-set-the-breaks-of-x-axis
hist(I2E[Sector %in% MAN], breaks = seq(0,0.75,0.025), xlab = NA, xlim = c(0, 0.75), 
     col = "orange", add = TRUE)
abline(v = fmedian(I2E), lwd = 1.5)
abline(v = fmedian(I2E[Sector %in% MAN]), col = "red", lwd = 1.5)
# E2R
hist(E2R, breaks = seq(-0.06,0.65,0.025), xlab = NA, main = expression('E2R'))
hist(E2R[Sector %in% MAN], breaks = seq(-0.06,0.65,0.025), xlab = NA, xlim = c(0, 0.75), 
     col = "orange", add = TRUE)
abline(v = fmedian(E2R), lwd = 1.5)
abline(v = fmedian(E2R[Sector %in% MAN]), col = "red", lwd = 1.5)
par(oldpar)
})
dev.copy(pdf, "Figures/GROWTH_REG_Hists.pdf", width = 10.27, height = 4)
dev.off()


# TS Charts
with(moddat, {
oldpar <- par(mfrow = c(1, 3), mar = c(4.5, 2.5, 2.1, 1.5)) # bottom, left, top, right
mat <- psmat(log10(VA))
man_sec <- substr(rownames(mat), 5, 7) %in% MAN
colour <- ifelse(man_sec, "orange", "grey")
plot(mat, xlab = "Year", ylab = NA, main = expression('log'[10](VA)), colours = colour)  
fmedian(mat) %>% lines(as.integer(names(.)), ., lwd = 1.5)
fmedian(mat[man_sec, ]) %>% lines(as.integer(names(.)), ., col = "red", lwd = 1.5)
legend("topleft", c("Overall Median", "Manufacturing Median"), lty = 1, lwd = 1.5,
       col = c("black", "red"), bty = "n", y.intersp = 1.5, seg.len = 1)
mat <- psmat(I2E)
plot(mat, xlab = "Year", ylab = NA, main = expression('I2E'), colours = colour)  
fmedian(mat) %>% lines(as.integer(names(.)), ., lwd = 1.5)
fmedian(mat[man_sec, ]) %>% lines(as.integer(names(.)), ., col = "red", lwd = 1.5)
mat <- psmat(E2R)
plot(mat, xlab = "Year", ylab = NA, main = expression('E2R'), colours = colour)  
fmedian(mat) %>% lines(as.integer(names(.)), ., lwd = 1.5)
fmedian(mat[man_sec, ]) %>% lines(as.integer(names(.)), ., col = "red", lwd = 1.5)
par(oldpar)
rm(mat, man_sec, colour)
})
dev.copy(pdf, "Figures/GROWTH_REG_TS.pdf", width = 10.27, height = 5)
dev.off()


# Model Selection ----------------------------------------------------------------------------------------------
options(plm.fast = TRUE)
settfm(moddat, log_VA = log(VA))

# Reject random effects
phtest(log_VA ~ L(I2E, 0:2) + L(E2R, 0:2), moddat) 
# Robust version (same conclusion)
temp <- L(moddat, 0:2, cols = c("log_VA", "I2E", "E2R"))
phtest(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, 
       temp, method = "aux", vcov = vcovHC) 

# Reject that CS FE is sufficient
temp <- qDF(moddat) %>% tfmv(c('log_VA', 'I2E', 'E2R'), fwithin, g = cs, apply = FALSE) %>% 
        pdata.frame(index = c("cy", "Sector"))
phtest(log_VA ~ L(I2E, 0:2) + L(E2R, 0:2), data = temp)
# Robust version (same conclusion)
phtest(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, method = "aux", vcov = vcovHC,
       data = tfmv(temp, c("log_VA", "I2E", "E2R"), flag.data.frame, n = 1:2, g = cs, t = Year, apply = FALSE))
# Test shether FD model is sifficint without cy FE...
phtest(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, method = "aux", vcov = vcovHC,
       data = moddat %>% tfmv(c('log_VA', 'I2E', 'E2R'), fdiff, apply = FALSE) %>%
         tfmv(c("log_VA", "I2E", "E2R"), flag, n = 1:2, apply = FALSE) %>%
         qDF %>% pdata.frame(index = c("cy", "Sector")))


# Reject that CS and CY FE is sufficient
temp <- qDF(moddat) %>% tfmv(c('log_VA', 'I2E', 'E2R'), fhdwithin, 
                             fl = list(cs, cy), fill = TRUE, apply = FALSE) %>% 
                         pdata.frame(index = c("sy", "Country"))
phtest(log_VA ~ L(I2E, 0:2) + L(E2R, 0:2), data = temp)
# Robust version (don't reject !!)
phtest(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, method = "aux", vcov = vcovHC,
       data = tfmv(temp, c("log_VA", "I2E", "E2R"), flag.data.frame, n = 1:2, g = cs, t = Year, apply = FALSE))

phtest(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, # method = "aux", vcov = vcovHC,
       data = moddat %>% tfmv(c('log_VA', 'I2E', 'E2R'), fdiff, apply = FALSE) %>%
         tfmv(c('log_VA', 'I2E', 'E2R'), fwithin.data.frame, g = cy, apply = FALSE) %>% 
         tfmv(c("log_VA", "I2E", "E2R"), flag, n = 1:2, apply = FALSE) %>%
         qDF %>% pdata.frame(index = c("sy", "Country")))


# Estimation:
library(fixest)

FE_mod <- feols(log_VA ~ l(I2E, 0:2) + l(E2R, 0:2), data = moddat, 
             panel.id = c("cs", "Year"), fixef = c("cs", "cy", "sy"))

summary(FE_mod)
# Residual ACF
index(moddat)[-FE_mod$obsRemoved, ] %$% psacf(resid(FE_mod), cs, Year) 
test_mod <- index(moddat)[-FE_mod$obsRemoved, ] %$% lm(resid(FE_mod) ~ L(resid(FE_mod), 1, cs, Year)) 
car::linearHypothesis(test_mod, "L(resid(FE_mod), 1, cs, Year) = -0.1") # -1/(11-1) = -0.1, see Wooldridge sec. 10.5.4

FE_mod2 <- feols(log_VA ~ l(I2E, 0:2) + l(E2R, 0:2), data = moddat, 
                 panel.id = c("cs", "Year"), fixef = c("cs", "cy"))
# Residual ACF
index(moddat)[-FE_mod2$obsRemoved, ] %$% psacf(resid(FE_mod2), cs, Year) 
index(moddat)[-FE_mod2$obsRemoved, ] %$% lm(resid(FE_mod2) ~ L(resid(FE_mod2), 1, cs, Year)) %>% summary

# FD Model
FD_mod <- lm(log_VA ~ L(I2E, 0:2) + L(E2R, 0:2), data = D(moddat, stubs = FALSE))
index(moddat)[-FD_mod$na.action, ] %$% psacf.default(resid(FD_mod), cs, Year) 
index(moddat)[-FD_mod$na.action, ] %$% lm(resid(FD_mod) ~ flag.default(resid(FD_mod), 1, cs, Year)) %>% summary
# Same as this test:
pwfdtest(log_VA ~ L(I2E, 0:1) + L(E2R, 0:1), moddat, h0 = "fd")
# This tests against rho_FD != -0.5
pwfdtest(log_VA ~ L(I2E, 0:1) + L(E2R, 0:1), moddat, h0 = "fe")
# Tests autocorrelation of residuals of fixed-effects model not equal to -1/(11-1) = -0.1, Wooldridge sec. 10.5.4
pwartest(log_VA ~ L(I2E, 0:1) + L(E2R, 0:1), moddat)
# Same
FD_mod <- plm(log_VA ~ L(I2E, 0:2) + L(E2R, 0:2), data = moddat, model = "fd")
coef(FD_mod)
# Fazit: FD model is better

# Model exports Analysis ----------------------------------------------------------------------------
settfmv(moddat, c('I2E', 'E2R'), flag, n = 1:2, apply = FALSE)
vars <- c("log_VA", "I2E", "L1.I2E", "L2.I2E", "E2R", "L1.E2R", "L2.E2R")


FD_mod <- lm(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, data = D(moddat, cols = vars, stubs = FALSE))
  # jtools::summ(FD_mod, robust = TRUE, digits = 5)
  # performance::check_model(FD_mod)
  # performance::check_autocorrelation(FD_mod)
  # performance::check_heteroscedasticity(FD_mod)
  # obs <- names(head(sort(cooks.distance(FD_mod), decreasing = TRUE))) # Note: row numbers in original data
  # qsu(-dfbeta(FD_mod))
  # -dfbeta(FD_mod)[obs, ] %r+% coef(FD_mod)
  # lm(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R,
  #    data = D(moddat, stubs = FALSE), subset = rownames(moddat) %!in% obs) %>% summary

# coef(lmrob(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, data =
#      moddat %>% tfmv(is.numeric, fdiff, apply = FALSE) %>% qDF %>% HDW(~cy+sy, stub = FALSE))) %>% round(4)


# FD_mod <- plm(formula(FD_mod), moddat, model = "fd")  
FD_mod <- feols(formula(FD_mod), D(moddat, cols = vars, stubs = FALSE), cluster = "cs")  

library(robustbase)
FD_mod_r <- lmrob(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, data = D(moddat, cols = vars, stubs = FALSE))
  
FD_mod_log <- lm(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, 
                 data = moddat %>% tfmv(vars[-1], log) %>% gv(vars) %>% fdiff)
  # jtools::summ(FD_mod_log, robust = TRUE, digits = 5)
  # performance::check_model(FD_mod_log)
  # qsu(-dfbeta(FD_mod_log))
  # lm(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R,
  #    data = moddat %>% tfm(gvr(., "I2E|E2R") %>% lapply(log)) %>% D(stubs = FALSE), 
  #    subset = rownames(moddat) %!in% obs)
  
FD_mod_log <- feols(formula(FD_mod_log), moddat %>% tfmv(vars[-1], log) %>% tfmv(vars, fdiff, apply = FALSE), cluster = "cs")  

FD_mod_log_r <- lmrob(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, 
                 data = moddat %>% tfmv(vars[-1], log) %>% gv(vars) %>% fdiff, 
                 k.max = 10000, refine.tol = 1e-16)

  
FE_mod <- feols(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, data = moddat, 
                panel.id = c("cs", "Year"), fixef = c("cs", "cy", "sy"), se = "threeway")

temp <- qDF(moddat) %>% HDW(~ qF(cs) + qF(cy) + qF(sy), cols = vars, stub = FALSE)
# summary(lm(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, temp))

FE_mod_r <- lmrob(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, temp)

FD_FE_mod <- feols(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, 
                   data = tfmv(moddat, vars, fdiff, apply = FALSE), 
                   panel.id = c("cs", "Year"), fixef = c("cy", "sy"), cluster = c("cs", "cy", "sy"))

temp <- tfmv(moddat, vars, fdiff, apply = FALSE) %>% qDF %>% HDW( ~ qF(cy) + qF(sy), cols = vars, stub = FALSE)
FD_FE_mod_r <- lmrob(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, temp)

# FE_mod2 <- feols(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, data = moddat, 
#                  panel.id = c("cs", "Year"), fixef = c("cs", "cy"))

FE_mod_log <- feols(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, 
                    data = moddat %>% tfmv(vars[-1], log), 
                    panel.id = c("cs", "Year"), fixef = c("cs", "cy", "sy"), se = "threeway")


FD_FE_mod_log <- feols(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, 
                       data = moddat %>% tfmv(vars[-1], log) %>% tfmv(vars, fdiff, apply = FALSE), 
                       panel.id = c("cs", "Year"), fixef = c("cy", "sy"), cluster = c("cs", "cy", "sy"))

# FE_mod_log2 <- feols(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, data = moddat %>% tfm(gvr(., "I2E|E2R") %>% lapply(log)), 
#                      panel.id = c("cs", "Year"), fixef = c("cs", "cy"))

temp <- moddat %>% tfmv(vars[-1], log) %>% tfmv(vars, fdiff, apply = FALSE) %>% qDF %>% 
          HDW( ~ qF(cy) + qF(sy), cols = vars, stub = FALSE)
FD_FE_mod_log_r <- lmrob(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, temp)


temp <- qDF(moddat) %>% tfmv(vars[-1], log) %>% HDW(~ qF(cs) + qF(cy) + qF(sy), cols = vars, stub = FALSE)
# summary(lm(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, temp))

FE_mod_log_r <- lmrob(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, temp)

modlist <- list(FD = FD_mod,
                `FD-TFE` = FD_FE_mod,
                FE = FE_mod,
                #`FE NoSY` = FE_mod2,
                `FD Elas` = FD_mod_log,
                `FD-TFE Elas` = FD_FE_mod_log,
                `FE Elas` = FE_mod_log
                #`FE NoSY Elas` = FE_mod_log2
)

etable(modlist) # %>% lapply(unlist) %>% qDF
# coefplot(FD_mod, FD_FE_mod, FE_mod, FD_mod_log, FD_FE_mod_log, FE_mod_log)
esttex(modlist, digits = 4,
       fixef_sizes = TRUE, fixef_sizes.simplify = FALSE, # postprocess.tex = TRUE, #drop.section = "fixef",
       style.tex = style.tex(fixef_sizes.prefix = "N ", 
                             line.top = "", 
                             tablefoot.title = "" ,
                             line.bottom = ""), 
       subtitles = names(modlist),
       file = "Tables/GROWTH_EST_TFE.tex", replace = TRUE)

# esttex(FE_mod, FE_mod_log)

# library(modelsummary)
# 
# # selist <- list(FD = sqrt(diag(plm::vcovHC(FD_mod, type = "HC3"))), 
# #                `FD-TFE` = se(FD_FE_mod, "twoway"),
# #                FE = se(FE_mod, "threeway"),
# #                # `FE NoSY` = se(FE_mod2, "threeway"),
# #                `FD Elas` = sqrt(diag(plm::vcovHC(FD_mod_log, type = "HC3"))),
# #                `FD-TFE Elas` = se(FD_FE_mod_log, "twoway"),
# #                `FE Elas` = se(FE_mod_log, "threeway")
# #                #`FE NoSY Elas` = se(FE_mod_log2, "twoway")
# #                )
# 
# modelsummary(modlist, fmt = 4, 
#              output = "Tables/GROWTH_EST_TFE.tex", stars = TRUE, 
#              gof_omit = "Pseudo|Lik|IC")
# 
# modelsummary(modlist, fmt = 4, 
#              output = "huxtable", stars = TRUE, 
#              gof_omit = "Pseudo|Lik|IC") %>% huxtable::quick_xlsx(file = "Tables/GROWTH_EST_TFE.xlsx")


modlist_r <- list(FD = FD_mod_r, 
                `FD-TFE` = FD_FE_mod_r, 
                FE = FE_mod_r,
                `FD Elas` = FD_mod_log_r,
                `FD-TFE Elas` = FD_FE_mod_log_r,
                `FE Elas` = FE_mod_log_r) 

stargazer(modlist_r, df = FALSE, digits = 4, out = "Tables/GROWTH_EST_rob.tex")

modelsummary(modlist_r, fmt = 4, 
             output = "huxtable", stars = TRUE, 
             gof_omit = "Pseudo|Lik|IC") %>% huxtable::quick_xlsx(file = "Tables/GROWTH_EST_rob.xlsx")


# Old: mixed coefficient tables
# 
# modlist <- list(FD = FD_mod, 
#                 `FD-MM` = FD_mod_r, 
#                 FE = FE_mod,
#                 `FE-MM` = FE_mod_r,
#                 #`FE NoSY` = FE_mod2,
#                 `FD Elas` = FD_mod_log,
#                 `FD-MM Elas` = FD_mod_log_r,
#                 `FE Elas` = FE_mod_log, 
#                 `FE-MM Elas` = FE_mod_log_r
#                 #`FE NoSY Elas` = FE_mod_log2
# )
# 
# selist <- list(FD = sqrt(diag(plm::vcovHC(FD_mod, type = "HC3"))), 
#                `FD-MM` = sqrt(diag(vcov(FD_mod_r))),
#                FE = se(FE_mod, "twoway"),
#                `FE-MM` = sqrt(diag(vcov(FE_mod_r))),
#                # `FE NoSY` = se(FE_mod2, "twoway"),
#                `FD Elas` = sqrt(diag(plm::vcovHC(FD_mod_log, type = "HC3"))),
#                `FD-MM Elas` = summary(FD_mod_log_r)$coefficients[, 2],
#                `FE Elas` = se(FE_mod_log, "twoway"), 
#                `FE-MM Elas` = sqrt(diag(vcov(FE_mod_log_r))) 
#                #`FE NoSY Elas` = se(FE_mod_log2, "twoway")
# )
# 
# 
# modelsummary(modlist[names(selist)], fmt = 4, 
#              statistic_override = selist,
#              output = "Tables/GROWTH_EST_rob.tex", stars = TRUE, 
#              gof_omit = "Pseudo|Lik|IC")
# 
# modelsummary(modlist[names(selist)], fmt = 4, 
#              statistic_override = selist,
#              output = "huxtable", stars = TRUE, 
#              gof_omit = "Pseudo|Lik") %>% huxtable::quick_xlsx(file = "Tables/GROWTH_EST_rob.xlsx")
# 




# Manufacturing -------------------------------------------------
moddat_MAN <- moddat %>% sbt(Sector %in% MAN) %>% qDF %>% fdroplevels %>% 
              pdata.frame(index = c("cs", "Year"))
modlist_MAN <- list()
modlist_MAN$FD <- lm(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, data = D(moddat_MAN, cols = vars, stubs = FALSE))
  # m <- modlist_MAN$FD
  # jtools::summ(m, robust = TRUE, digits = 5)
  # performance::check_model(m)
  # performance::check_autocorrelation(m)
  # performance::check_heteroscedasticity(m)
  # obs <- names(head(sort(cooks.distance(m), decreasing = TRUE))) # Note: row numbers in original data
  # qsu(-dfbeta(m))
  # -dfbeta(m)[obs, ] %r+% coef(m)
  # lm(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R,
  #    data = D(moddat_MAN, stubs = FALSE), subset = rownames(moddat_MAN) %!in% obs) %>% summary

# modlist_MAN$FD <- plm(formula(modlist_MAN$FD), moddat_MAN, model = "fd")  

modlist_MAN$FD <- feols(formula(modlist_MAN$FD), D(moddat_MAN, cols = vars, stubs = FALSE), cluster = "cs")  

modlist_MAN$`FD Elas` <- lm(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, 
                         data = moddat_MAN %>% tfmv(vars[-1], log) %>% gv(vars) %>% fdiff)
  # jtools::summ(modlist_MAN$`FD Elas`, robust = TRUE, digits = 5)
  # performance::check_model(modlist_MAN$`FD Elas`)
  # qsu(-dfbeta(modlist_MAN$`FD Elas`))
  # lm(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R,
  #    data = moddat_MAN %>% tfm(gvr(., "I2E|E2R") %>% lapply(log)) %>% D(stubs = FALSE), 
  #    subset = rownames(moddat_MAN) %!in% obs) %>% summary


# modlist_MAN$`FD Elas` <- plm(formula(modlist_MAN$`FD Elas`), moddat_MAN %>% tfm(gvr(., "I2E|E2R") %>% lapply(log)), model = "fd")  
modlist_MAN$`FD Elas` <- feols(formula(modlist_MAN$`FD Elas`), 
                               moddat_MAN %>% tfmv(vars[-1], log) %>% tfmv(vars, fdiff, apply = FALSE), cluster = "cs")  

modlist_MAN$`FD-TFE` <- feols(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, 
                              tfmv(moddat_MAN, vars, fdiff, apply = FALSE),
                              fixef = c("cy", "sy"), cluster = c("cs", "cy", "sy"))  


modlist_MAN$FE <- feols(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, data = moddat_MAN, 
                        fixef = c("cs", "cy", "sy"), se = "threeway")

# modlist_MAN$`FE NoSY` <- feols(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, data = moddat_MAN, 
#                                fixef = c("cs", "cy"))

modlist_MAN$`FD-TFE Elas` <- feols(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, 
                                   moddat_MAN %>% tfmv(vars[-1], log) %>% tfmv(vars, fdiff, apply = FALSE),
                                   fixef = c("cy", "sy"), cluster = c("cs", "cy", "sy"))  


modlist_MAN$`FE Elas` <- feols(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, 
                            data = moddat_MAN %>% tfmv(vars[-1], log), 
                            fixef = c("cs", "cy", "sy"), se = "threeway")

# modlist_MAN$`FE NoSY Elas` <- feols(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, 
#                              data = moddat_MAN %>% tfm(gvr(., "I2E|E2R") %>% lapply(log)), 
#                              panel.id = c("cs", "Year"), fixef = c("cs", "cy"))

etable(modlist_MAN[names(modlist)])
esttex(modlist_MAN[names(modlist)], digits = 4,
       fixef_sizes = TRUE, fixef_sizes.simplify = FALSE, # postprocess.tex = TRUE, #drop.section = "fixef",
       style.tex = style.tex(fixef_sizes.prefix = "N ", 
                             line.top = "", 
                             tablefoot.title = "" ,
                             line.bottom = ""), 
       subtitles = names(modlist_MAN),
       file = "Tables/GROWTH_EST_MAN_TFE.tex", replace = TRUE)


modelsummary(modlist_MAN[names(modlist)], fmt = 4, 
             output = "huxtable", stars = TRUE, 
             gof_omit = "Pseudo|Lik|IC") %>% huxtable::quick_xlsx(file = "Tables/GROWTH_EST_MAN_TFE.xlsx")


# Old way using modelsummary package...
# selist_MAN <- list(FD = sqrt(diag(plm::vcovHC(modlist_MAN$FD, type = "HC3"))), 
#                    FE = se(modlist_MAN$FE, "twoway"),
#                    `FE NoSY` = se(modlist_MAN$`FE NoSY`, "twoway"),
#                    `FD Elas` = sqrt(diag(plm::vcovHC(modlist_MAN$`FD Elas`, type = "HC3"))),
#                    `FE Elas` = se(modlist_MAN$`FE Elas`, "twoway"), 
#                    `FE NoSY Elas` = se(modlist_MAN$`FE NoSY Elas`, "twoway"))
# 
# modelsummary(modlist_MAN[names(selist_MAN)], fmt = 4, 
#              statistic_override = selist_MAN,
#              output = "Tables/GROWTH_EST_MAN.tex", stars = TRUE, 
#              gof_omit = "Pseudo|Lik")
# 
# modelsummary(modlist_MAN[names(selist_MAN)], fmt = 4,
#              statistic_override = selist_MAN,
#              output = "huxtable", stars = TRUE,
#              gof_omit = "Pseudo|Lik") %>% huxtable::quick_xlsx(file = "Tables/GROWTH_EST_MAN.xlsx")


modlist_MAN_r <- list()
modlist_MAN_r$`FD` <- lmrob(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, data = D(moddat_MAN, cols = vars, stubs = FALSE))
modlist_MAN_r$`FD-TFE` <- lmrob(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, 
                                 data = moddat_MAN %>% tfmv(vars, fdiff, apply = FALSE) %>% qDF %>% HDW(~ cy + sy, cols = vars, stub = FALSE))
modlist_MAN_r$`FE` <- lmrob(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, 
                                    data = moddat_MAN %>% qDF %>% HDW(~ cs + cy + sy, cols = vars, stub = FALSE))
modlist_MAN_r$`FD Elas` <- lmrob(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, 
                             data = moddat_MAN %>% gv(vars) %>% tfmv(vars[-1], log) %>% fdiff)
modlist_MAN_r$`FD-TFE Elas` <- lmrob(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, 
                                 data = moddat_MAN %>% tfmv(vars[-1], log) %>% tfmv(vars, fdiff, apply = FALSE) %>% 
                                   qDF %>% HDW(~ cy + sy, cols = vars, stub = FALSE), k.max = 500)
modlist_MAN_r$`FE Elas` <- lmrob(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, 
                             data = moddat_MAN %>% tfmv(vars[-1], log) %>% qDF %>% HDW(~ cs + cy + sy, cols = vars, stub = FALSE), k.max = 500)

stargazer(modlist_MAN_r, df = FALSE, digits = 4, out = "Tables/GROWTH_EST_MAN_rob.tex")

modelsummary(modlist_MAN_r, fmt = 4, 
             output = "huxtable", stars = TRUE, 
             gof_omit = "Pseudo|Lik|IC") %>% huxtable::quick_xlsx(file = "Tables/GROWTH_EST_MAN_rob.xlsx")





# DVA Exports -----------------------------------------------------------------------------
modlist_DVA_EX <- list()

settfm(moddat, log_DVA_EX = log(DVA_EX))
modlist_DVA_EX$FD <- plm(log_DVA_EX ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, moddat, model = "fd")  

modlist_DVA_EX$`FD Elas` <- plm(log_DVA_EX ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, 
                                moddat %>% tfm(gvr(., "I2E|E2R") %>% lapply(log)), model = "fd")  

modlist_DVA_EX$FE <- feols(log_DVA_EX ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, data = moddat, 
                           panel.id = c("cs", "Year"), fixef = c("cs", "cy", "sy"))

modlist_DVA_EX$`FE Elas` <- feols(log_DVA_EX ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, 
                                  data = moddat %>% tfm(gvr(., "I2E|E2R") %>% lapply(log)), 
                                  panel.id = c("cs", "Year"), fixef = c("cs", "cy", "sy"))

# Manufacturing
settfm(moddat_MAN, log_DVA_EX = log(DVA_EX))
modlist_DVA_EX$`FD MAN` <- plm(log_DVA_EX ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, moddat_MAN, model = "fd")  

modlist_DVA_EX$`FD Elas MAN` <- plm(log_DVA_EX ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, 
                             moddat_MAN %>% tfm(gvr(., "I2E|E2R") %>% lapply(log)), model = "fd")  

modlist_DVA_EX$`FE MAN` <- feols(log_DVA_EX ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, data = moddat_MAN, 
                           panel.id = c("cs", "Year"), fixef = c("cs", "cy", "sy"))

modlist_DVA_EX$`FE Elas MAN` <- feols(log_DVA_EX ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, 
                               data = moddat_MAN %>% tfm(gvr(., "I2E|E2R") %>% lapply(log)), 
                               panel.id = c("cs", "Year"), fixef = c("cs", "cy", "sy"))

etable(modlist_DVA_EX, se = "twoway")

selist_DVA_EX <- list(FD = sqrt(diag(plm::vcovHC(modlist_DVA_EX$FD, type = "HC3"))), 
                   FE = se(modlist_DVA_EX$FE, "twoway"),
                   `FD Elas` = sqrt(diag(plm::vcovHC(modlist_DVA_EX$`FD Elas`, type = "HC3"))),
                   `FE Elas` = se(modlist_DVA_EX$`FE Elas`, "twoway"),
                   `FD MAN` = sqrt(diag(plm::vcovHC(modlist_DVA_EX$`FD MAN`, type = "HC3"))), 
                   `FE MAN` = se(modlist_DVA_EX$`FE MAN`, "twoway"),
                   `FD Elas MAN` = sqrt(diag(plm::vcovHC(modlist_DVA_EX$`FD Elas MAN`, type = "HC3"))),
                   `FE Elas MAN` = se(modlist_DVA_EX$`FE Elas MAN`, "twoway"))

modelsummary(modlist_DVA_EX[names(selist_DVA_EX)], fmt = 4, 
             statistic_override = selist_DVA_EX,
             output = "Tables/GROWTH_EST_DVA_EX.tex", stars = TRUE, 
             gof_omit = "Pseudo|Lik")

modelsummary(modlist_DVA_EX[names(selist_DVA_EX)], fmt = 4, 
             statistic_override = selist_DVA_EX,
             output = "huxtable", stars = TRUE, 
             gof_omit = "Pseudo|Lik") %>% huxtable::quick_xlsx(file = "Tables/GROWTH_EST_DVA_EX.xlsx")


# attach(FD_moddat)
# VA_FD <- lm(log(VA) ~ L(I2E, 0:1) + L(E2R, 0:1)) 
# plot(VA_FD, which = c(1,3,4,5))
# car::dfbetaPlots(VA_FD)
# obs <- names(head(sort(cooks.distance(VA_FD), decreasing = TRUE))) # Note: row numbers in original data
# -dfbeta(VA_FD)[obs, ] %c+% coef(VA_FD)
# mod0 <- lm(log(VA) ~ L(I2E, 0:2) + L(E2R, 0:2))
# mod1 <- lm(log(VA) ~ L(I2E, 0:2) + L(E2R, 0:2), subset = -as.integer(obs))
# range(dfbeta(VA_FD))
# performance::check_model(VA_FD)

# library(fixest)
# data %>% sbt(Country %in% EAC) %>%
#   tfm(id = finteraction(Country, Sector)) %>%
#   feols(log(VA) ~ l(I2E, 0:1) + l(E2R, 0:1), 
#         panel.id = c("id", "Year"), 
#         fixef = c("Country", "Year"), data = .)
# 

# -> Conclusion: Overall in the World, i2e decreases DVA whereas e2r increases it. So it's quite mechanical, nothing interesting here...



# (b) Composition of VA --------------------------------------------------------


moddat <- data %>% sbt(Country %in% setdiff(EAC, "SSD") & 
                       Sector %!in% c("REC", "REI", "FIB", "EGW") &
                       !(Sector %in% c("OTH", "PHH") & Country == "KEN")) %>% # &
                       # between(COE_S, 0, 1) & between(NOS_S, 0, 1)) %>% # between(COE_NOS_S, 0, 1) -> too restrictive..
  tfm(cs = finteraction(Country, Sector), 
      cy = finteraction(Country, Year),
      sy = finteraction(Sector, Year)) %>%
  frename(DVA_Exports = DVA_EX) %>%
  fdroplevels %>%
  pdata.frame(index = c('cs', 'Year')) %>% pserify

# rm(list = names(moddat))
detach(moddat)
attach(moddat)

# Excluded Sectors Table: NaH, just keep them..
data %>% sbt(Country %in% setdiff(EAC, "SSD") & 
             Sector %!in% c("REC", "REI", "FIB", "EGW") &
             !(Sector %in% c("OTH", "PHH") & Country == "KEN") &
            (COE_S < 0 | COE_S > 1 | NOS_S < 0 | NOS_S > 1), 
            Year:Sector, VA_SUM, SUB, COE_S, NOS_S) %>% qsu
  tfm(VA_SUM = round(VA_SUM), SUB = round(SUB)) %>%
  stargazer(summary = FALSE, rownames = FALSE,
            digits = 4, digits.extra = 0,
            out = "Tables/VADEC_EXCL.tex") 


# Summary statistics
moddat %>% gvr("COE|NOS") %>% 
  dapply(unclass) %>% qsu(array = FALSE) %>% 
  tfm(COE = round(COE), 
      NOS = round(NOS),
      COE_NOS = round(COE_NOS)) %>%
  unlist2d("Variable", "Trans") %>% 
  tfm(`N/T` = as.integer(`N/T`)) %T>%
  stargazer(summary = FALSE, 
            rownames = FALSE,
            digits = 4, digits.extra = 0,
            out = "Tables/SUMMARY_VADEC.tex") %>% print

MAN <- .c(FBE, TEX, WAP, PCM, MPR, ELM, TEQ, MAN)
COE_S <- replace_outliers(COE_S, c(0, 1)) %>% ss_pseries(Country %!in% "TZA")
NOS_S <- replace_outliers(NOS_S, c(0, 1)) %>% ss_pseries(Country %!in% "TZA")
COE_NOS_S <- replace_outliers(COE_NOS_S, c(0, 1)) %>% ss_pseries(Country %!in% "TZA")

oldpar <- par(mfrow = c(1, 3), mar = c(2.5, 4, 2.1, 0), lwd = 0.5) # bottom, left, top, right
# COE
hist(COE_S, breaks = seq(0,0.9,0.025), xlab = NA, main = expression('COE'['Share']), xaxt = 'n')
axis(side = 1, at = seq(0,1,0.1)) 
hist(COE_S[Sector %in% MAN], breaks = seq(0,0.9,0.025), xlab = NA, 
     col = "orange", add = TRUE)
abline(v = fmedian(COE_S), lwd = 1.5)
abline(v = fmedian(COE_S[Sector %in% MAN]), col = "red", lwd = 1.5)
legend("topleft", c("Overall", "Manufacturing"), lty = 1, lwd = 1.5,
       col = c("black", "red"), bty = "n", y.intersp = 2, seg.len = 1)
# NOS
hist(NOS_S, breaks = seq(0,1,0.025), xlab = NA, main = expression('NOS'['Share']), xaxt = 'n')
axis(side = 1, at = seq(0,1,0.1)) 
hist(NOS_S[Sector %in% MAN], breaks = seq(0,1,0.025), xlab = NA, 
     col = "orange", add = TRUE)
abline(v = fmedian(NOS_S), lwd = 1.5)
abline(v = fmedian(NOS_S[Sector %in% MAN]), col = "red", lwd = 1.5)
# COE + NOS
hist(COE_NOS_S, breaks = seq(0.65,1,0.025), xlab = NA, main = expression('COE'['Share'] + 'NOS'['Share']))
hist(COE_NOS_S[Sector %in% MAN], breaks = seq(0.65,1,0.025), xlab = NA, xlim = c(0, 0.75), 
     col = "orange", add = TRUE)
abline(v = fmedian(COE_NOS_S), lwd = 1.5)
abline(v = fmedian(COE_NOS_S[Sector %in% MAN]), col = "red", lwd = 1.5)
par(oldpar)

dev.copy(pdf, "Figures/VADEC_REG_Hists_NOTZ.pdf", width = 10.27, height = 4)
dev.off()


# TS Charts
oldpar <- par(mfrow = c(1, 3), mar = c(4.5, 2.5, 2.1, 1.5)) # bottom, left, top, right
mat <- psmat(COE_S)
man_sec <- substr(rownames(mat), 5, 7) %in% MAN
colour <- ifelse(man_sec, "orange", "grey")
plot(mat, xlab = "Year", ylab = NA, main = expression('COE'['Share']), colours = colour)  
fmedian(mat) %>% lines(as.integer(names(.)), ., lwd = 1.5)
fmedian(mat[man_sec, ]) %>% lines(as.integer(names(.)), ., col = "red", lwd = 1.5)
legend("topleft", c("Overall Median", "Manufacturing Median"), lty = 1, lwd = 1.5,
       col = c("black", "red"), bty = "n", y.intersp = 1.5, seg.len = 1)
mat <- psmat(NOS_S)
plot(mat, xlab = "Year", ylab = NA, main = expression('NOS'['Share']), colours = colour)  
fmedian(mat) %>% lines(as.integer(names(.)), ., lwd = 1.5)
fmedian(mat[man_sec, ]) %>% lines(as.integer(names(.)), ., col = "red", lwd = 1.5)
mat <- psmat(COE_NOS_S)
plot(mat, xlab = "Year", ylab = NA, main = expression('COE'['Share'] + 'NOS'['Share']), colours = colour)  
fmedian(mat) %>% lines(as.integer(names(.)), ., lwd = 1.5)
fmedian(mat[man_sec, ]) %>% lines(as.integer(names(.)), ., col = "red", lwd = 1.5)
par(oldpar)
rm(mat, man_sec, colour)

dev.copy(pdf, "Figures/VADEC_REG_TS_NOTZ.pdf", width = 10.27, height = 5)
dev.off()



modlist_COE <- list()
library(MASS)
library(robustbase)
settfmv(moddat, c('I2E', 'E2R'), flag, n = 1:2, apply = FALSE)

form <- NOS_S ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R
FD_mod <- lm(form, data = Dlog(moddat, stubs = FALSE))
FD_mod_r <- rlm(form, data = Dlog(moddat, stubs = FALSE))
FD_mod_r2 <- rlm(form, data = Dlog(moddat, stubs = FALSE), method = "MM")
FD_mod_r3 <- rlm(form, data = Dlog(moddat, stubs = FALSE), psi = psi.bisquare)
FD_mod_r4 <- rlm(form, data = Dlog(moddat, stubs = FALSE), method = "MM", psi = psi.bisquare)
FD_mod_r5 <- lmrob(form, data = Dlog(moddat, stubs = FALSE), setting = "KS2014") # 5 and 6 give the same !!
FD_mod_r6 <- lmrob(form, data = Dlog(moddat, stubs = FALSE), method = "SMDM") 
FD_mod_r7 <- lmrob(form, data = Dlog(moddat, stubs = FALSE), setting = "KS2014", method = "M") 
# FD_mod_r8 <- ltsReg(form, data = Dlog(moddat, stubs = FALSE)) 

modelsummary::msummary(list(FD_mod, FD_mod_r, FD_mod_r2, FD_mod_r3, FD_mod_r4, FD_mod_r5, FD_mod_r6, FD_mod_r7), 
                       fmt = 6, output = "huxtable")

# jtools::summ(FD_mod, robust = TRUE, digits = 5)
# performance::check_model(FD_mod)
# performance::check_autocorrelation(FD_mod)
# performance::check_heteroscedasticity(FD_mod)
# obs <- names(head(sort(cooks.distance(FD_mod), decreasing = TRUE))) # Note: row numbers in original data
# qsu(-dfbeta(FD_mod))
# -dfbeta(FD_mod)[obs, ] %r+% coef(FD_mod)
# lm(log_VA ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R,
#    data = D(moddat, stubs = FALSE), subset = rownames(moddat) %!in% obs) %>% summary

library(modelsummary)
library(stargazer)
rob_fits <- function(form, data) {
        list(LM = lm(form, data = data),
             Huber = rlm(form, data = data, psi = psi.huber),
             # `MM-light` = rlm(form, data = data, method = "MM"),
             `MM` = lmrob(form, data = data, setting = "KS2014") 
             # `MM2` = robust::lmRob(form, data = data)
             # ltsReg(form, data = Dlog(dapply(moddat, unclass), stubs = FALSE)) # no outreg...
             )
#                  
}
# Regressions in FD:
form <- function(y) as.formula(paste(y, "~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R"))
temp <- D(moddat, stubs = FALSE)

res <- c(rob_fits(form("COE_S"), temp), 
         rob_fits(form("NOS_S"), temp),
         rob_fits(form("COE_NOS_S"), temp))

stargazer(res, out = "Tables/VADEC_REG_Shares.tex", df = FALSE)
msummary(res, output = "huxtable", fmt = 6) %>% 
  huxtable::quick_xlsx(file = "Tables/VADEC_REG_Shares.xlsx")


temp <- Dlog(moddat, stubs = FALSE)

res <- c(rob_fits(form("COE_S"), temp), 
         rob_fits(form("NOS_S"), temp),
         rob_fits(form("COE_NOS_S"), temp))

stargazer(res, out = "Tables/VADEC_REG_Shares_Elas.tex", df = FALSE)
msummary(res, output = "huxtable", fmt = 6) %>% 
  huxtable::quick_xlsx(file = "Tables/VADEC_REG_Shares_Elas.xlsx")

temp <- moddat %>% tfm(log_COE = log(COE), 
                       log_NOS = log(NOS), 
                       log_COE_NOS = log(COE_NOS)) %>% D(stubs = FALSE)

res <- c(rob_fits(form("log_COE"), temp), 
         rob_fits(form("log_NOS"), temp),
         rob_fits(form("log_COE_NOS"), temp))

stargazer(res, out = "Tables/VADEC_REG_LogVal_Shares.tex", df = FALSE)
msummary(res, output = "huxtable", fmt = 6) %>% 
  huxtable::quick_xlsx(file = "Tables/VADEC_REG_LogVal_Shares.xlsx")

temp <- Dlog(moddat, stubs = FALSE)

res <- c(rob_fits(form("COE"), temp), 
         rob_fits(form("NOS"), temp),
         rob_fits(form("COE_NOS"), temp))

stargazer(res, out = "Tables/VADEC_REG_LogVal_Elas.tex", df = FALSE)
msummary(res, output = "huxtable", fmt = 6) %>% 
  huxtable::quick_xlsx(file = "Tables/VADEC_REG_LogVal_Elas.xlsx")













msummary(rob_fits(form, Dlog(moddat, stubs = FALSE)))

modlist_COE$FD <- plm(COE ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, moddat, model = "fd")  

modlist_COE$`FD Elas` <- plm(COE ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, 
                                moddat %>% tfm(gvr(., "COE|I2E|E2R") %>% lapply(log)), model = "fd")  

# Manufacturing
moddat_MAN <- moddat %>% sbt(Sector %in% MAN) %>% qDF %>% fdroplevels %>% 
              pdata.frame(index = c("cs", "Year"))

modlist_COE$`FD MAN` <- plm(COE ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, moddat_MAN, model = "fd")  

modlist_COE$`FD Elas MAN` <- plm(COE ~ I2E + L1.I2E + L2.I2E + E2R + L1.E2R + L2.E2R, 
                                    moddat_MAN %>% tfm(gvr(., "COE|I2E|E2R") %>% lapply(log)), model = "fd")  


selist_COE <- list(FD = sqrt(diag(plm::vcovHC(modlist_COE$FD, type = "HC3"))), 
                   FE = se(modlist_COE$FE, "twoway"),
                      `FD Elas` = sqrt(diag(plm::vcovHC(modlist_COE$`FD Elas`, type = "HC3"))),
                      `FE Elas` = se(modlist_COE$`FE Elas`, "twoway"),
                      `FD MAN` = sqrt(diag(plm::vcovHC(modlist_COE$`FD MAN`, type = "HC3"))), 
                      `FE MAN` = se(modlist_COE$`FE MAN`, "twoway"),
                      `FD Elas MAN` = sqrt(diag(plm::vcovHC(modlist_COE$`FD Elas MAN`, type = "HC3"))),
                      `FE Elas MAN` = se(modlist_COE$`FE Elas MAN`, "twoway"))

modelsummary(modlist_COE[names(selist_COE)], fmt = 4, 
             statistic_override = selist_COE,
             output = "Tables/GROWTH_EST_COE.tex", stars = TRUE, 
             gof_omit = "Pseudo|Lik")

modelsummary(modlist_COE[names(selist_COE)], fmt = 4, 
             statistic_override = selist_COE,
             output = "huxtable", stars = TRUE, 
             gof_omit = "Pseudo|Lik") %>% huxtable::quick_xlsx(file = "Tables/VADEC_EST_COE.xlsx")









# Export shares (Upgrading following Foster 2015) ------------------------------------------------------------------

COMTRADE <- readRDS("~/R/Macro-Data-Portal/Macro Data Portal DB/OTS_UN_COMT.rda")
EX_shares <- collap(COMTRADE, ~ Community + Year, cols = 12:15) %>% 
             tfmv(-(1:2), fsum, Year, TRA = "%") %>% setDT

EX_shares %>% psmat(EX_VAL_CT + IM_VAL_CT ~ Community, ~ Year) %>% plot(colours = TRUE)

# Uninformative about upgrading...
EX_shares %>% D(by = EX_VAL_CT + IM_VAL_CT ~ Community, t = ~ Year, stubs = FALSE) %>%
  collap(EX_VAL_CT + IM_VAL_CT ~ Community, fsum)

 EX_shares %>% 
   tfm(EX_shares[, lapply(.SD, frollmean, n = 10L), by = Community, .SDcols = 3:6]) %>%
   melt(1:2, na.rm = TRUE) %>%
   ggplot(aes(x = Year, y = value, colour = Community)) + 
   geom_line(size = 1) + facet_wrap("variable") + 
   guides(colour = guide_legend(ncol = 1)) + theme_bw()
 
# Bernhard and Milberg (2011): Economic upgrading if increased export unit value (greater product quality) 
# and higher market share (maintained international competitivness). Measured over the course of a decade. 


# Further to do: GVC's and economic complexity (production and/or exports, herfindahl index).
# TODO: Could do weighted normalized Herfindahl index, weighting export shares by Receiving Country GDP to measure concentration of complex exports ??
 
# Herfindahl Index Approach -----------------------------------------------------------------------
data %>% 
  tfm(DVA_Exports = Exports - i2e) %>%
  gby(Year, Country) %>%
  tfm(EX_Share = fsum(Exports, GRP(.), TRA = "/"), 
      DVA_EX_Share = fsum(DVA_Exports, GRP(.), TRA = "/")) %>%
  tfm(HI = EX_Share^2,
      HI_DVA = DVA_EX_Share^2) %>% 
  collapg(w = Exports,
          custom = list(fsum_uw = .c(VA, DVA_Exports, i2e, e2r, HI, HI_DVA),
                        fmean = .c(I2E, E2R))) %>%
  tfm(HIN = (HI - 1/26)/(1-1/26), 
      HIN_DVA = (HI_DVA - 1/26)/(1-1/26)) -> data_HI_ag

pwcor(nv(data_HI_ag), P = TRUE)
pwcor(nv(D(data_HI_ag, by = ~ Country)), P = TRUE) %>% print(digits = 4)

ctry <- "RWA"

sbt(data_HI_ag, Country %in% EAC) %>% 
  psmat(HIN + HIN_DVA + I2E + E2R ~ Country, ~ Year) %>% 
  plot(legend = TRUE)

 m1 <- lm(HIN ~ log(i2e) + log(Exports), sbt(data_HI_ag, Country == ctry)) 
 m2 <- lm(HIN ~ I2E + log(Exports), sbt(data_HI_ag, Country == ctry)) 
 m3 <- lm(Dlog(HIN) ~ Dlog(i2e) + Dlog(Exports), sbt(data_HI_ag, Country == ctry))
 m4 <- lm(D(HIN) ~ D(I2E) + Dlog(Exports), sbt(data_HI_ag, Country == ctry))

 m5 <- lm(HIN_DVA ~ log(i2e) + log(DVA_Exports), sbt(data_HI_ag, Country == ctry))
 m6 <- lm(HIN_DVA ~ I2E + log(DVA_Exports), sbt(data_HI_ag, Country == ctry)) 
 m7 <- lm(Dlog(HIN_DVA) ~ Dlog(i2e) + Dlog(DVA_Exports), sbt(data_HI_ag, Country == ctry)) 
 m8 <- lm(D(HIN_DVA) ~ D(I2E) + Dlog(DVA_Exports), sbt(data_HI_ag, Country == ctry))
 
 plot_coefs(m1, m2, m3, m4, m5, m6, m7, m8)
 stargazer::stargazer(m1, m2, m3, m4, m5, m6, m7, m8, 
                      out = "GVC EAC Paper/Tables/HHI.tex", df = FALSE)

# -> Only significant effects for Uganda.

library(fixest)
feols(HIN_DVA ~ I2E + log(DVA_Exports), 
      data = sbt(data_HI_ag, Country %in% setdiff(EAC, "SSD")), 
      fixef = c("Country", "Year"), panel.id = c("Country", "Year"))

feols(HIN_DVA ~ E2R + log(DVA_Exports), 
      data = sbt(data_HI_ag, Country %in% setdiff(EAC, "SSD")), 
      fixef = c("Country", "Year"), panel.id = c("Country", "Year"))

# FD Estimators
feols(d(HIN_DVA) ~ d(I2E) + d(log(DVA_Exports)), 
      data = sbt(data_HI_ag, Country %in% setdiff(EAC, "SSD")), 
      panel.id = c("Country", "Year"))

feols(d(HIN_DVA) ~ d(E2R) + d(log(DVA_Exports)), 
      data = sbt(data_HI_ag, Country %in% setdiff(EAC, "SSD")), 
      panel.id = c("Country", "Year"))


# Also following Foster 2015: Take Exports and determine tech intensity as GDP weighted NRCA...
# Also look at Decomposition of VA? COE and NOS?? Does it say something about profitability or social upgrading??
 
 

# Also: Add ES findings...

