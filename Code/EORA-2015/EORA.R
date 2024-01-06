rm(list = ls())
gc()
library(collapse)
library(data.table)
library(magrittr)
## Full 20 26 Sector Mapping
# F2S <- fread("~/Data/EORA/conc_full2simplified.txt")
## Labels
Lab_FD <- fread("~/Data/EORA/indices/labels_FD.txt")
# Lab_Q <- fread("~/Data/EORA/indices/labels_Q.txt")
Lab_T <- fread("~/Data/EORA/indices/labels_T.txt")
# Lab_VA <- fread("~/Data/EORA/indices/labels_VA.txt")

# Dimensions:
# # Countries
# Gv <- funique(Lab_FD[[1]])
# G <- length(Gv) - 1L # ROW Sector
# # Sectors
# Nv <- funique(Lab_T[[4]])
# N <- length(Nv) - 1L
# # Final Uses
# Uv <- funique(Lab_FD[[4]])
# U <- length(Uv)
# # Value Added (Income side GDP)
# VAv <- Lab_VA[[2]]
# VA <- length(VAv)
# 
# # So the ICIO should be
# G * N # rows
# G * N + G * U # columns

## Data
# final demand block FD
# FD <- fread("~/Data/EORA/Eora26_2015_bp/Eora26_2015_bp_FD.txt") %>% qM %>% setDimnames(NULL)
# transactions matrix T
# T <- fread("~/Data/EORA/Eora26_2015_bp/Eora26_2015_bp_T.txt") %>% qM %>% setDimnames(NULL)
# primary inputs (also called value added)
# VA <- fread("~/Data/EORA/Eora26_2015_bp/Eora26_2015_bp_VA.txt") %>% qM %>% setDimnames(NULL)

# Other / Simplified:
T <-
  fread(
    "~/Data/EORA/Eora26_2015_bp/import/miranda/Eoras/Phase199/Loop082/simplified/Eora26_2015_bp_T.txt"
  ) %>%
  qM() %>%
  setDimnames(NULL)
FD <-
  fread(
    "~/Data/EORA/Eora26_2015_bp/import/miranda/Eoras/Phase199/Loop082/simplified/Eora26_2015_bp_FD.txt"
  ) %>%
  qM() %>%
  setDimnames(NULL)
# Not Currently needed
# VA <-
#   fread(
#     "~/Data/EORA/Eora26_2015_bp/import/miranda/Eoras/Phase199/Loop082/simplified/Eora26_2015_bp_VA.txt"
#   ) %>%
#   qM() %>%
#   setDimnames(NULL)

# ICIO = cbind(T, FD)

# # Checks...
# all.equal(colSums(T) + rowSums(FD), colSums(VA))
# cor(colSums(T) + rowSums(FD), colSums(VA))
#
# ## satellite accounts (also called environmental extensions or stressors):
# # Q for emissions associated with production
# Q <- fread("~/Data/EORA/Eora26_2015_bp/Eora26_2015_bp_Q.txt") %>% qM %>% setDimnames(NULL)
# # QY for direct emissions by final consumers
# QY <- fread("~/Data/EORA/Eora26_2015_bp/Eora26_2015_bp_QY.txt") %>% qM %>% setDimnames(NULL)
#
#   fread("~/Data/EORA/conc_full2simplified.txt")

library(decompr)
library(matrixStats)

crm <- function(x) {
  # function to remove the country name
  if (is.character(x)) {
    return(substr(x, 5L, 10000L))
  }
  names(x) <- substr(names(x), 5L, 10000L)
  x
}
cr <- function(x) {
  # function to get the country name
  if (is.character(x)) {
    return(substr(x, 1L, 3L))
  }
  names(x) <- substr(names(x), 1L, 3L)
  x
}


# Full Table (Discarded) ---------------------------------------------------------------------

# GN <- nrow(T) - 1L
# seqGN <- 1:GN
# out <- rowSums2(T, rows = seqGN) + rowSums2(FD, rows = seqGN) # , cols = -ncol(T)
# v <- colSums2(VA, cols = seqGN)
# k <- setdiff(unique(Lab_T[[1]]), "ROW")
# i <- setdiff(unique(Lab_T[[4]]), "TOTAL")
# # FDC <- unique(Lab_FD[[4]])
# 
# # Check dimensions match
# all_identical(
#   nrow(T) - 1L,
#   ncol(T) - 1L,
#   nrow(FD) - 1L,
#   length(out),
#   length(v),
#   length(k) * length(i)
# )
# 
# # Check that table is cartesian product:
# all.equal(
#   unattrib(ss(Lab_T, -fnrow(Lab_T), c(1, 4))),
#   unattrib(expand.grid(i, k, stringsAsFactors = FALSE))[2:1]
# )


# # create the decompr object
# dec <- load_tables_vectors(
#   x = T[seqGN, seqGN],
#   # No ROW
#   y = FD[seqGN, seq_len(length(k) * length(FDC))],
#   k = k,
#   i = i,
#   o = out,
#   v = v
# )
# names(dec)

# Aggregating to EAC + World Regions ---------------------------------------------

n <- nrow(T)
ind <- Lab_T[[4L]][-n]
k <- funique(Lab_T[[1L]][-n])
i <- funique(ind)

# Replacing industries with codes:
sec_class <- readxl::read_excel("trade classification.xlsx", sheet = "Sectors")
i_long <- i
i <- sec_class$code[ckmatch(i, sec_class$sector)] 
ind_long <- ind
ind <- sec_class$code[ckmatch(ind, sec_class$sector)]

# Check dimensions match
all_identical(
  n - 1L,
  ncol(T) - 1L,
  nrow(FD) - 1L,
  length(k) * length(i)
)

# Check that table is cartesian product:
all.equal(
  unattrib(ss(Lab_T, -n, c(1L, 4L))),
  unattrib(expand.grid(i_long, k, stringsAsFactors = FALSE))[2:1]
)

# Aggregating T -----------------------------------
gind <- GRP(ind, sort = FALSE)
identical(GRPnames(gind), i)

# First Expanding ROW row and column:
# ROW Rows
TT <- T[-n, -n]
T_ag <- TT %>% fsum(gind) %>% fsum(TRA = "/") %r*% 
        T[n, -n] %>% {
          rbind(TT, unname(.))
         }

# ROW Matrix
  # (nrow(T)-1L) / length(k) == length(i)
  # starts <- seq_along(k)  * 26L - 25L
  # cdiag_T <- lapply(starts, function(i) T[i:(i + 25L), i:(i + 25L)]) %>% simplify2array
  # cdiag_T_ag <- apply(cdiag_T, 1:2, sum)
  # cdiag_T_ag <- cdiag_T_ag / sum(cdiag_T_ag)
  # rm(starts, cdiag_T)

T[n, n] # This is 0, so this is actually redundant..

T_dg <- TT %>% fsum(gind) %>% t %>% fsum(gind) %>% `/`(sum(.))
sum(T_dg)

# ROW Cols
T_ag <- TT %>% t %>% fsum(gind) %>%
  fsum(TRA = "/") %r*% T[-n, n] %>%
  t %>% {
    cbind(T_ag, unname(.) %>% rbind(unname(T_dg) * T[n, n]))
  }
rm(TT, T_dg)
gc()

# Checks:
dim(T_ag)
dim(T) - 1L + 26L
identical(T[1:(n - 1L), 1:(n - 1L)], T_ag[1:(n - 1L), 1:(n - 1L)])
anyNA(T_ag)
T_ag[is.na(T_ag)] <- 0
all.equal(colSums2(T_ag, rows = n:nrow(T_ag))[1:(n - 1L)], T[n, ][1:(n - 1L)])
all.equal(rowSums2(T_ag, cols = n:ncol(T_ag))[1:(n - 1L)], T[, n][1:(n - 1L)])

# Now Aggregating to the classification...
trade_class <-
  readxl::read_excel("trade classification.xlsx", range = "A1:B220")
k[k %!in% trade_class$iso3c]

# This is UNDP Classification: https://en.wikipedia.org/wiki/List_of_UNDP_country_codes
Lab_T[[1L]] <-
  recode_char(
    Lab_T[[1L]],
    SUD = "SDN",
    SDS = "SSD",
    USR = "ROW",
    ANT = "ATG"
  )
k <- funique(Lab_T[[1L]])
k[k %!in% trade_class$iso3c]

identical(Lab_T[[1L]], trade_class$iso3c[match(Lab_T[[1L]], trade_class$iso3c)])
Lab_T[[5L]] <- trade_class$trade[ckmatch(Lab_T[[1L]], trade_class$iso3c)]

reg <- c(Lab_T[[5L]], rep("ROW", 25L))
all(length(reg) == dim(T_ag))
regi <- paste(reg, c(ind[-n], i), sep = ".")
gregi <- GRP(regi, sort = FALSE)

r <- funique(reg)
EAC <- c("UGA", "TZA", "KEN", "RWA", "BDI", "SSD")
r <- c(c(EAC, "SSA"), setdiff(r, c(EAC, "SSA")))
ri <- as.vector(t(outer(r, i, paste, sep = ".")))

# This aggregates T:
T_ag %<>% fsum(gregi) %>% t %>% fsum(gregi) %>% t %>% .[ri, ri]

# Aggregating FD -----------------------------------
FD_ag <- FD[-n, ] %>%
  fsum(gind) %>%
  fsum(TRA = "/") %r*% FD[n, ] %>% {
    rbind(FD[-n, ], unname(.))
  }

# Checks:
dim(FD_ag)
dim(T) - 1L + 26L
identical(FD[1:(n - 1L), ], FD_ag[1:(n - 1L), ])
anyNA(FD_ag)
FD_ag[is.na(FD_ag)] <- 0
all.equal(colSums2(FD_ag, rows = n:nrow(FD_ag))[1:(n - 1L)], FD[n, ][1:(n - 1L)])

# Getting classification again
fdc <- funique(Lab_FD[[4L]])
Lab_FD[[1L]] <-
  recode_char(
    Lab_FD[[1L]],
    SUD = "SDN",
    SDS = "SSD",
    USR = "ROW",
    ANT = "ATG"
  )
all(funique(Lab_FD[[1L]]) %in% trade_class$iso3c)
Lab_FD[[5L]] <- trade_class$trade[ckmatch(Lab_FD[[1L]], trade_class$iso3c)]

regfdc <- paste(Lab_FD[[5L]], Lab_FD[[4L]], sep = ".")
rf <- as.vector(t(outer(r, fdc, paste, sep = ".")))

# This aggregates FD:
FD_ag %<>% fsum(gregi) %>%
  t() %>%
  fsum(regfdc) %>%
  t() %>%
  .[ri, rf]

# Creating decmpr Object --------------------------------------------
all_identical(rownames(T_ag), 
              colnames(T_ag), 
              rownames(FD_ag),
              ri)

# Now creating the decompr object
dec <- load_tables_vectors(
  x = T_ag,
  y = FD_ag,
  k = r,
  i = i
)
names(dec)

out <- rowSums(T_ag) + rowSums(FD_ag)

library(ggplot2)
library(scales)
library(RColorBrewer)

mfac <- function(x, ordered = TRUE) factor(x, levels = funique(x), ordered = ordered)
mds <- function(x) dollar(round(x), scale = 1/1000, suffix = " M")


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
    plot.margin = ggplot2::margin(r = 70),
    # unit(c(1, 1, 0.5, 0.5), "lines"),,
    legend.text = element_text(size = 12)
  )

rbsc <-
  discrete_scale(c("colour", "fill"), "hue", rainbow, na.value = "grey50") # hcl.colors

uga <- which(startsWith(dec$rownam, "UGA"))
tza <- which(startsWith(dec$rownam, "TZA"))
ken <- which(startsWith(dec$rownam, "KEN"))
rwa <- which(startsWith(dec$rownam, "RWA"))
bdi <- which(startsWith(dec$rownam, "BDI"))
ssd <- which(startsWith(dec$rownam, "SSD"))
eac <- c(uga, tza, ken, rwa, bdi, ssd)

###############################
# Part 1: Analyzing Gross Flows
###############################

# Heatmap of the Matrices -----------------------------------------------------
library(pheatmap)
# Intermediate Flows
T_ag[eac, eac] %>% log10 %>% 
         pheatmap(color = colorRampPalette(brewer.pal(n = 7, name ="YlOrRd"))(100),
                  cluster_rows = FALSE, cluster_cols = FALSE, 
                  border_color = NA, legend_breaks = -4:6, 
                  fontsize_row = 3.2, fontsize_col = 3.2)  # legend_labels = 10^(0:7))
# heatmap(log(T_ag[eac, eac]), Rowv = NA, Colv = NA, scale = "none", revC = TRUE, cexRow = 0.3, cexCol = 0.3)
dev.copy(pdf, "GVC EAC Paper/Figures/heatmap_EAC.pdf", width = 9, height = 8)
dev.off()

# pheatmap::pheatmap(log10(T_ag) %>% setDimnames(lapply(dimnames(.), cr)), cluster_rows = FALSE, cluster_cols = FALSE, 
#                     scale = "none")
# heatmap(log(T_ag) %>% setDimnames(lapply(dimnames(.), cr)), Rowv = NA, Colv = NA, 
#         scale = "none", revC = TRUE, cexRow = 1.9, cexCol = 1.9)
# dev.copy(pdf, "GVC EAC Paper/Figures/heatmap_FULL.pdf", width = 9, height = 9)
# dev.off()

T_ag %>% fsum(cr(rownames(.))) %>% t %>% fsum(cr(rownames(.))) %>% 
  t %c/% 1000 %>% log10 %>% .[r, r] %>% 
    pheatmap(color = colorRampPalette(brewer.pal(n = 7, name ="YlOrRd"))(100),
             cluster_rows = FALSE, cluster_cols = FALSE, border_color = NA,
             legend_breaks = 0:7, # legend_labels = 10^(0:7),
             display_numbers = TRUE)
# heatmap(Rowv = NA, Colv = NA, scale = "none", revC = TRUE, cexRow = 1.9, cexCol = 1.9)
dev.copy(pdf, "GVC EAC Paper/Figures/heatmap_AG.pdf", width = 7, height = 6)
dev.off()

# TODO: Now double counting in ROW category?? i.e. former USSR ?? 

# Value Added ----------------------

# Value Added:
all_obj_equal(unattrib(dec$Vc * dec$X), 
              unattrib(out - colSums(T_ag)), 
              unattrib(diag(dec$Vc) %*% dec$B %*% rowSums(dec$Y)))

with(dec, Vc[eac] * X[eac]) %>% 
  qDF(TRUE) %>%
  setNames(.c(Sector, DVA)) %>%
  tfm(Country = mfac(cr(Sector)), Sector = mfac(crm(Sector))) %>%
  {
    ggplot(., aes(x = Sector, y = DVA, fill = Country)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(DVA / 1000)),
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

dev.copy(pdf, "GVC EAC Paper/Figures/output_DVA_tot.pdf", width = 10, height = 8.27)
dev.off()

# Direct Value added content of Output (Problematic)
dec$Vc[eac] %>% replace_outliers(0, NA, "min") %>%
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

dev.copy(pdf, "GVC EAC Paper/Figures/output_DVA.pdf", width = 10, height = 8.27)
dev.off()

# Exports --------------------------
barplot(crm(dec$E[uga]), col = "orange", border = NA)


# Total Exports: Long
dec$E[eac] %>%
  qDF(TRUE) %>%
  setNames(.c(Sector, Exports)) %>%
  tfm(Country = mfac(cr(Sector)), 
      Sector = mfac(crm(Sector))) %>%
  {
    ggplot(., aes(x = Sector, y = Exports, fill = Country)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(Exports/1000)),
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

dev.copy(pdf, "GVC EAC Paper/Figures/exports.pdf", width = 10, height = 8.27)
dev.off()


# Total Exports: Stacked
dec$E[eac] %>%
  qDF(TRUE) %>%
  setNames(.c(Sector, Exports)) %>%
  tfm(Country = mfac(cr(Sector)), Sector = mfac(crm(Sector))) %>%
  ggplot(aes(x = Country, y = Exports, fill = Sector)) +
  geom_bar(stat = "identity") +
  guides(fill = guide_legend(ncol = 1)) +
  scale_y_continuous(labels = mds, breaks = extended_breaks(7)) + rbsc +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())

dev.copy(pdf, "GVC EAC Paper/Figures/exports_stacked.pdf", width = 8.27, height = 7.5)
dev.off()


# Exports to EAC Partner States
dec$ESR[eac, EAC] %>%
  rowSums() %>%
  qDF(TRUE) %>%
  setNames(.c(Sector, Exports)) %>%
  tfm(Country = mfac(cr(Sector)), Sector = mfac(crm(Sector))) %>%
  {
    ggplot(., aes(x = Sector, y = Exports, fill = Country)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(Exports/1000, 1)),
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

dev.copy(pdf, "GVC EAC Paper/Figures/exports_EAC.pdf", width = 10, height = 8.27)
dev.off()

# Exports to EAC Partner States: Stacked
dec$ESR[eac, EAC] %>%
  rowSums() %>%
  qDF(TRUE) %>%
  setNames(.c(Sector, Exports)) %>%
  tfm(Country = mfac(cr(Sector)), Sector = mfac(crm(Sector))) %>%
  ggplot(aes(x = Country, y = Exports, fill = Sector)) +
  geom_bar(stat = "identity") +
  guides(fill = guide_legend(ncol = 1)) +
  scale_y_continuous(labels = mds, breaks = extended_breaks(7)) + rbsc +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())

dev.copy(pdf, "GVC EAC Paper/Figures/exports_EAC_stacked.pdf", width = 8.27, height = 7.5)
dev.off()

# Exports Share to EAC Partner States: Stacked
dec$ESR[eac, EAC] %>%
  rowSums() %c/% fsum(rowSums(dec$ESR[eac, ]), cr(dec$rownam[eac]), TRA = 1L) %>% 
  qDF(TRUE) %>%
  setNames(.c(Sector, Exports)) %>%
  tfm(Country = mfac(cr(Sector)), Sector = mfac(crm(Sector))) %>%
  ggplot(aes(x = Country, y = Exports, fill = Sector)) +
  geom_bar(stat = "identity") +
  guides(fill = guide_legend(ncol = 1)) +
  scale_y_continuous(labels = percent, breaks = extended_breaks(7)) + rbsc +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())

dev.copy(pdf, "GVC EAC Paper/Figures/exports_EAC_perc_stacked.pdf", width = 8.27, height = 7.5)
dev.off()


# Import share (Proportion of Output that was Imported Inputs)
N <- dec$N
G <- dec$G
GN <- dec$GN
Nfdc <- length(fdc)
E <- cbind(T_ag, FD_ag) # This is also expensive with large matrices. Would be nice if it could be avoided
for (j in 1:G) {
  m <- 1L + (j - 1L) * N
  n <- N + (j - 1L) * N
  s <- GN + 1L + (j - 1L) * Nfdc
  r <- GN + Nfdc + (j - 1L) * Nfdc
  E[m:n, m:n] <- 0  ## intermediate demand for domestic goods
  E[m:n, s:r] <- 0  ## final demand for domestic goods
}
imp_sh <- colSums(E[, seq_col(T_ag)])
imp_sh_eac <- colSums(E[eac, seq_col(T_ag)]) / imp_sh # Further normalization: Relative to total imports
imp_sh <- imp_sh / out # What proportion of output was imported ?

# Export share (Proportion of Output Exported)
exp_sh <- rowSums(E)
exp_sh_eac <- rowSums(E[, cr(colnames(E)) %in% EAC]) / exp_sh
exp_sh <- exp_sh / out

all_obj_equal(names(imp_sh), names(exp_sh), names(imp_sh_eac), names(exp_sh_eac))
shares <- cbind(`Percent of Inputs Imported (VS)     ` = imp_sh, 
                `Percent of Output Exported      ` = exp_sh, 
                `Percent of Imports from EAC      ` = imp_sh_eac, 
                `Percent of Exports to EAC      ` = exp_sh_eac)
rm(imp_sh, exp_sh, imp_sh_eac, exp_sh_eac)
nrow(shares) == nrow(T_ag)
gc()

# Choosing colors
cols = c("#000099","#0000CC","#0000FF","#3366CC","#3399CC","#33CCFF","#660000","#990000","#CC0000","#FF0000","#FF3300","#FF6600","#FF9900","#FFCC00")
barplot(setNames(rep(1, length(cols)), cols), col = cols)

# Import / Export share to / from EAC...
shares[eac, ] %>% qDT("Sector") %>%
  tfm(Country = mfac(cr(Sector)), Sector = mfac(crm(Sector))) %>%
  melt(.c(Country, Sector), value.name = "Value", na.rm = TRUE) %>% 
    ggplot(aes(x = Sector, y = Value, fill = variable)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_text(aes(label = round(Value * 100)),
                vjust = -0.3, size = 1.8, position = position_dodge(0.9)) +
      facet_wrap( ~ Country, scales = "fixed", ncol = 1L) +
      scale_y_continuous(limits = c(0, 1), labels = percent) +
      scale_fill_manual(values = c("#0000CC", "#CC0000", "#3399CC", "#FF9900")) +
      guides(fill = guide_legend(title = NULL)) + 
      theme_minimal() +
      pretty_plot # + theme(legend.position = "right")

dev.copy(pdf, "GVC EAC Paper/Figures/output_shares.pdf", width = 13.69, height = 8.27)
dev.off()


###################################
# Part 2: Decomposed Flows
###################################

range(dec$B)

# Final value added matrices:
VB <- dec$Vc * dec$B
FVAX <- qM(leontief(dec, post = "output", long = FALSE))
# Checks
all.equal(unattrib(FVAX), unattrib(VB %*% diag(dec$X)))
all.equal(colSums(FVAX), out)
identical(dimnames(FVAX), dimnames(T_ag))

FVAE <- qM(leontief(dec, post = "exports", long = FALSE))
FVAFD <- copyAttrib(VB %*% diag(rowSums(FD_ag)), VB)


# Heatmap of the Matrices ------------------------------
library(pheatmap)
cmatscale <- function(x, i) switch(i, VB = x * 100, log10(x / 1000))
for(i in .c(VB, FVAE, FVAFD, FVAX)) {
  print(i)
  cal <- call("[", as.symbol(i), quote(eac), quote(eac))
  
  eval(cal) %>% cmatscale(i) %>%
  pheatmap(color = colorRampPalette(brewer.pal(n = 7, name ="YlOrRd"))(100),
           cluster_rows = FALSE, cluster_cols = FALSE, 
           border_color = NA, # legend_breaks = -4:6, 
           fontsize_row = 3.2, fontsize_col = 3.2)  
  dev.copy(pdf, paste0("GVC EAC Paper/Figures/heatmap_EAC_", i, ".pdf"), width = 9, height = 8)
  dev.off()
  
  temp <- (if(i == "VB") FVAFD else get(i)) %>% fsum(cr(rownames(.))) %>% t %>% 
                         fsum(cr(rownames(.))) %>% t 
  temp <- if(i == "VB") temp %r/% colSums(temp) * 100 else cmatscale(temp, i) 
  temp %>% .[r, r] %>% 
  pheatmap(color = colorRampPalette(brewer.pal(n = 7, name ="YlOrRd"))(100),
           cluster_rows = FALSE, cluster_cols = FALSE, border_color = NA,
           # legend_breaks = 0:7, # legend_labels = 10^(0:7),
           display_numbers = TRUE)
  dev.copy(pdf, paste0("GVC EAC Paper/Figures/heatmap_AG_", i, ".pdf"), width = 7, height = 6)
  dev.off()
  rm(temp)
}

VB_eac_ag <- VB[eac, eac] %c*% 100 %>% rowsum(cr(rownames(.)), reorder = FALSE) 

VB_eac_ag <- table(cr(colnames(VB_eac_ag)))[EAC] %>% cumsum %>%
  lapply(function(i) VB_eac_ag[, seq(i-25L, i)] %>%
           setColnames(crm(colnames(.)))) 

# This plots individual heatmaps...
for(i in names(VB_eac_ag)) {

pheatmap(VB_eac_ag[[i]],
         color = colorRampPalette(brewer.pal(n = 7, name ="YlOrRd"))(100),
         cluster_rows = FALSE, cluster_cols = FALSE, border_color = NA,
         display_numbers = TRUE)

dev.copy(pdf, paste0("GVC EAC Paper/Figures/heatmap_VB_AG_", i, ".pdf"), width = 10, height = 4)
dev.off()

}

# This plots a combined chart...
unlist2d(VB_eac_ag, "Country", "Source", DT = TRUE, id.factor = TRUE) %>% 
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

dev.copy(pdf, "GVC EAC Paper/Figures/heatmap_VB_AG_EAC.pdf", width = 10.69, height = 7.27)
dev.off()





# Decompositions: ---------------------------------------------------

lFD <- leontief(dec, post = "final_demand") # same as using decomp
lE <- leontief(dec, post = "exports")

lE %>%
  sbt(Source_Country == "UGA") %>%
  tfm(FVAX = fsum(FVAX, TRA = "%")) %>%
  View()

wwz <- wwz(dec)

# Value added origin of exports by country
head(lE)
lE %>% sbt(Using_Country %in% EAC) %>%
 collap(FVAX ~ Source_Country + Using_Country + Using_Industry, fsum) %>%
  tfm(VAOR = fsum(FVAX, list(Using_Country, Using_Industry), TRA = "/")) %>%
  sbt(Source_Country %in% EAC) %>%
  ggplot(aes(x = Using_Industry, y = VAOR, fill = Source_Country)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    facet_wrap( ~ Using_Country, scales = "fixed", ncol = 1L) +
    scale_fill_brewer(palette = "Set1") +
    theme_minimal() + pretty_plot
  


library(gvc)

# Vertical Specialization: Hummels (2001) ----------------------

# Simple (gross flows)

# Checking Am:
with(dec, all.equal((Am %r*% X)[Am > 0], T_ag[Am > 0]))

with(dec, rep(1, GN) %*% Am %*% E / sum(E)) # Formula in the paper
with(dec, fmean(Am %*% E / E, w = E))
with(dec, fmean(colSums(Am %r*% E) / E, w = E))

# Not equal:
all.equal(unattrib(with(dec, colSums(Am %r*% E) / E)), 
          unattrib(with(dec, Am %*% E / E)))
# This is the key to the formula in the paper: At the aggregate level it doesn't matter
all.equal(unattrib(with(dec, sum(Am %r*% E) / sum(E))), 
          unattrib(with(dec, rep(1, GN) %*% Am %*% E / sum(E))))

# THIS SHOULD BE IT...
all.equal(with(dec, colSums(Am %r*% E) / E), with(dec, colSums(Am)))

VS_gr <- colSums(dec$Am) # This is the easiest way to compute it !!!
fmean(VS_gr, w = dec$E) # Aggregate version (same as above)

all.equal(imp_sh, VS_gr) # Vertical specialization is actually equivalent to this one compute above !!!

# Now leontief VS: (same but n)
all.equal(with(dec, colSums(Vc * Bm * outer(rep.int(1L, GN), E))), # with(dec, colSums(Am %r*% (L %*% E))), # drop(with(dec, Am %*% (B %*% E))), #
          with(i2e(lE), setNames(i2e, paste(country, sector, sep = "."))))

# This is also true, so the simples way to calculate the ratio is using colSums(Vc * Bm)
with(dec, all.equal(colSums(Vc * Bm * outer(rep.int(1L, GN), E)) / E, 
                    colSums(Vc * Bm)))

VS_va <- with(dec, colSums(Vc * Bm))

# Hummels VS1 = Export to Re-export:
e2r(lE)
all.equal(with(dec, rowSums(Vc * Bm * outer(rep.int(1L, GN), E))), # with(dec, colSums(Am %r*% (L %*% E))), # drop(with(dec, Am %*% (B %*% E))), #
          with(e2r(lE), setNames(e2r, paste(country, sector, sep = "."))))

VS1_va <- with(dec, rowSums(Vc * Bm * outer(rep.int(1L, GN), E)) / E)


# Check: Matches dec$E ??
qDT(i2e(lE))[qDT(e2r(lE)), on = .(country, sector)] %>% 
 tfm(i2e = i2e / dec$E, 
     e2r = e2r / dec$E,
     sector = mfac(sector),
     country = mfac(country)) %>% 
  sbt(country %in% EAC) %>%
  melt(1:2) %>% tfm(value = replace_outliers(value, c(0, 1))) %>%
  frename(tools::toTitleCase) %>%
  ggplot(aes(x = Sector, y = Value, fill = Variable)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  geom_text(aes(label = round(Value * 100)),
            vjust = -0.3, size = 3, position = position_dodge(0.9)) +
  facet_wrap( ~ Country, scales = "fixed", ncol = 1L) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = percent) +
  theme_minimal() + pretty_plot

dev.copy(pdf, "GVC EAC Paper/Figures/VS.pdf", width = 13.69, height = 8.27)
dev.off()

# Same at aggregate level:
cbind(i2e = VS_va, e2r = VS1_va) %>% 
  qDT("country") %>% 
  tfm(country = mfac(cr(country))) %>%
  gby(country, sort = FALSE) %>%
  fmean(w = dec$E) %>%
  melt(1L) %>% frename(tools::toTitleCase) %>%
  ggplot(aes(x = Country, y = Value, fill = Variable)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  geom_text(aes(label = round(Value * 100)),
            vjust = -0.3, size = 3, position = position_dodge(0.9)) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = percent) +
  theme_minimal() + pretty_plot

dev.copy(pdf, "GVC EAC Paper/Figures/VS_aggregate.pdf", width = 13.69, height = 5.27)
dev.off()
  


res <- list()

# Domestic Final Demand Domestic Value Added
res$dfddva <- dfddva(lFD)
res$dfddva_ag <- dfddva(lFD, aggregate = TRUE)

res$dfddva <-
  qDT(lFD) %>%
  gby(Importing_Country, Source_Industry) %>%
  slt(Final_Demand) %>%
  fsum() %>%
  merge(qDT(res$dfddva), all.x = TRUE) %>%
  tfm(ratio = )

# Domestic Final Demand Foreign Value Added
res$dfdfva <- dfdfva(lFD)
res$dfdfva_ag <- dfdfva(lFD, aggregate = TRUE)

# Foreign Final Demand Domestic Value Added
res$ffddva <- ffddva(lFD)
res$ffddva_ag <- ffddva(lFD, aggregate = TRUE)

lFD_UGA <- fsubset(lFD, Source_Country == "UGA")
lFD_UGA_ag <- collap(lFD_UGA, Final_Demand ~ Source_Industry, fsum)
dfddva_ag %>% fsubset(country == "UGA")
fsum(lFD_UGA$Final_Demand)
dfdfva_ag %>% fsubset(Importing_Country == "UGA")

library(ggplot2)
EAC <- .c(UGA, TZA, KEN, RWA, BDI, SSD)
dfddva_ag %>%
  fsubset(country %in% EAC) %>%
  add_vars(dfdfva_ag %>% fsubset(Importing_Country %in% EAC)) %>%
  tfm(dfva = dfddva / dfdfva) %>%
  ggplot(aes(x = country, y = dfva)) +
  geom_bar(stat = "identity")

dfddva %>%
  fsubset(Importing_Country == "UGA") %>%
  ggplot(aes(x = Source_Industry, y = dfdfva)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45))

# Downstreamness
ds <- downstream(dec)
gc()
# Upstreamness
us <- upstream(dec)

# Exporting to Re-export
e2r(lE)
# Import to Export
i2e(lE) # gvc::vertical_specialisation(lE) # Same thing
# manually calculating this:
nonUGA <- which(Lab_T$V1[-length(Lab_T$V1)] != "UGA")
nonUGAFD <- which(Lab_FD$V1 != "UGA")
str(rowSums(T[-nonUGA, nonUGA]) + rowSums(FD[-nonUGA, nonUGAFD]))
# str(dec$Efd[, "UGA"]) [-nonUGA])
# New Revealed Comparative Advantage
nrca(lE)

# Also look at NTB'S in Uganda !!!
