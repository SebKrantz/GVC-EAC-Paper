#########################################
## Aggregating EORA 26 2005-2015 for EAC
#########################################

setwd("C:/Users/Sebastian Krantz/Documents/Data/EORA")

rm(list = ls())
gc()

library(collapse)
library(data.table)
library(magrittr)
library(matrixStats)
library(readxl)

# function to remove the country name
crm <- function(x) {
  if(is.character(x)) return(substr(x, 5L, 10000L))
  names(x) <- substr(names(x), 5L, 10000L)
  x
}
# function to get the country name
cr <- function(x) {
  if(is.character(x)) return(substr(x, 1L, 3L))
  names(x) <- substr(names(x), 1L, 3L)
  x
}

## Labels
Lab_FD <- fread("Eora26_2005_bp/labels_FD.txt")
Lab_VA <- fread("Eora26_2005_bp/labels_VA.txt")
v <- .c(COE, TAX, SUB, NOS, NMI, COF)
Lab_T <- fread("Eora26_2005_bp/labels_T.txt")

# Check Equality:
for(i in 2006:2015) {
  if(!identical(Lab_FD, fread(paste0("Eora26_",i,"_bp/labels_FD.txt")))) stop(i)
  if(!identical(Lab_VA, fread(paste0("Eora26_",i,"_bp/labels_VA.txt")))) stop(i)
  if(!identical(Lab_T, fread(paste0("Eora26_",i,"_bp/labels_T.txt")))) stop(i)
}

## Dimensions
n <- 4915L
dT <- c(n, n)
dFD <- c(n, 1140L)
dVA <- c(n, 6L)

## Countries and Industries
ind <- Lab_T[[4L]]
ctry <- Lab_T[[2L]]
FDctry <- Lab_FD[[2L]]
k <- funique(ctry[-n])
i <- funique(ind[-n])

# Check that table is cartesian product
all.equal(
  unattrib(ss(Lab_T, -n, c(2L, 4L))),
  unattrib(expand.grid(i, k, stringsAsFactors = FALSE))[2:1]
)

# Replacing industries with 3-Character codes
sec_class <- read_xlsx("trade classification.xlsx", sheet = "Sectors")
i_long <- i
i <- sec_class$code[ckmatch(i, sec_class$sector)] 
ind_long <- ind
ind[-n] <- sec_class$code[ckmatch(ind[-n], sec_class$sector)]
ind[n] <- "TOT"

## Getting classification for aggregation: EAC and world Regions:
trade_class <- read_xlsx("trade classification.xlsx", range = "A1:B220")
k[k %!in% trade_class$iso3c]

# This is UNDP Classification: https://en.wikipedia.org/wiki/List_of_UNDP_country_codes
ctry <- recode_char(ctry, SUD = "SDN", SDS = "SSD", ANT = "ATG")
T_USR_rm <- which(ctry != "USR") # removing USSR
ctry <- ctry[T_USR_rm] 
ind <- ind[T_USR_rm] 
FDctry <- recode_char(FDctry, SUD = "SDN", SDS = "SSD", ANT = "ATG")
FD_USR_rm <- which(FDctry != "USR") # removing USSR
FDctry <- FDctry[FD_USR_rm] 
k <- recode_char(k, SUD = "SDN", SDS = "SSD", ANT = "ATG")
k <- setdiff(k, "USR") # removing USSR
all(k %in% trade_class$iso3c)
all(k %in% ctry)
all(FDctry %in% c(k, "ROW"))

reg <- trade_class$trade[ckmatch(ctry, trade_class$iso3c)]
funique(reg)
EAC <- c("UGA", "TZA", "KEN", "RWA", "BDI", "SSD")
r <- c(EAC, "SSA", "MEA", "EUU", "ECA", "NAC", "SAS", "ASE", "CHN", "ROA", "LAC", "OCE")
# r_long <- ... No long names...
setdiff(funique(reg), r)
FDreg <- trade_class$trade[ckmatch(FDctry, trade_class$iso3c)]
regi <- paste(reg, ind, sep = ".")

all_identical(length(reg), length(ind), length(ctry))
all_identical(length(FDreg), length(FDctry))

# Grouping regi
flast(regi)
regi_lev <- c(t(outer(r, i, paste, sep = ".")), "ROW.TOT") # c(r, "ROW")
all(regi %in% regi_lev)
regi <- factor(regi, levels = regi_lev)
class(regi) <- c("factor", "na.included")

# Grouping FDreg
FDreg <- factor(FDreg, levels = c(r, "ROW"))
class(FDreg) <- c("factor", "na.included")


# Now Aggregating:
y <- as.character(2005:2015)

T <- lapply(y, function(j) {
  file <- paste0("Eora26_",j,"_bp/Eora26_",j,"_bp_T.txt")
  Tj <- fread(file, data.table = FALSE) 
  if(!identical(fdim(Tj), dT)) stop("Dimension mismatch in T: ", j)
  Tj[T_USR_rm, T_USR_rm] %>% fsum(regi) %>% qM %>% t %>% fsum(regi) %>% t # We first aggregate the rows of the data frame before converting to matrix
}) 

names(T) <- y
str(T)

T <- simplify2array(T)
str(T)

FD <- lapply(y, function(j) {
  file <- paste0("Eora26_",j,"_bp/Eora26_",j,"_bp_FD.txt")
  FDj <- fread(file, data.table = FALSE) 
  if(!identical(fdim(FDj), dFD)) stop("Dimension mismatch in FD: ", j)
  FDj[T_USR_rm, FD_USR_rm] %>% fsum(regi) %>% qM %>% t %>% fsum(FDreg) %>% t # We first aggregate the rows of the data frame before converting to matrix
}) 

names(FD) <- y
str(FD)

FD <- simplify2array(FD)
str(FD)

VA <- lapply(y, function(j) {
  file <- paste0("Eora26_",j,"_bp/Eora26_",j,"_bp_VA.txt")
  VAj <- transpose(fread(file, data.table = FALSE)) %>% setNames(v)
  if(!identical(fdim(VAj), dVA)) stop("Dimension mismatch in VA: ", j)
  VAj[T_USR_rm, ] %>% fsum(regi) %>% qM
}) 

names(VA) <- y
str(VA)

VA <- simplify2array(VA)
str(VA)


gc()

# Computing output and value added 
out <- apply(T, c(1L, 3L), sum) + apply(FD, c(1L, 3L), sum)
va <- out - apply(T, 2:3, sum)
va_VA <- apply(VA, c(1L, 3L), sum)

# Getting rid of ROW category
T_ROW_rm <- grep("ROW", rownames(T))
T[T_ROW_rm, T_ROW_rm, ]
T <- T[-T_ROW_rm, -T_ROW_rm, ]
FD <- FD[-T_ROW_rm, -grep("ROW", colnames(FD)), ]
VA <- VA[-T_ROW_rm, , ]
out <- out[-T_ROW_rm, ]
va <- va[-T_ROW_rm, ]
va_VA <- va_VA[-T_ROW_rm, ]
rm(T_ROW_rm)


# Creating decomposition Objects:
library(decompr)
decomps <- lapply(y, function(j) {
  load_tables_vectors(x = T[, , j],
                      y = FD[, , j],
                      k = r,
                      i = i,
                      o = out[, j],
                      v = va[, j])
  
})

names(decomps) <- y
str(decomps)

VB <- lapply(decomps, leontief, post = "none", long = FALSE) %>% 
       simplify2array
str(VB)

# Check: Close to 1 (exempting ROW category)
colMeans(apply(VB, 2:3, sum))

# Defining group object to aggregate to country level
rownam <- decomps[[1L]]$rownam
g <- GRP(cr(rownam), sort = FALSE)

T_ag <- lapply(setNames(y, y), function(i) T[, , i] %>% fsum(g) %>% t %>% fsum(g) %>% t) %>% 
        simplify2array

str(T_ag)

FD_ag <- lapply(setNames(y, y), function(i) FD[, , i] %>% fsum(g)) %>% 
         simplify2array

str(FD_ag)

VA_ag <- lapply(setNames(y, y), function(i) VA[, , i] %>% fsum(g)) %>% 
  simplify2array

str(VA_ag)

out_ag <- fsum(out, g)
va_ag <- fsum(va, g)
va_VA_ag <- fsum(va_VA, g)

VB_ag <- lapply(decomps, leontief, post = "output", long = FALSE) %>%
         lapply(function(x) x %>% fsum(g) %>% t %>% fsum(g) %>% t) %r/% 
         mctl(out_ag) %>% simplify2array

str(VB_ag)
colMeans(apply(VB_ag, 2:3, sum))

eac <- seq_len(length(i) * length(EAC)) 
dim(eac) <- c(length(i), length(EAC))
dimnames(eac) <- list(i, EAC)

rm(list = setdiff(ls(), .c(decomps, T, FD, VA, out, va, va_VA, VB, T_ag, FD_ag, VA_ag, 
                           out_ag, va_ag, va_VA_ag, VB_ag, 
                           r, EAC, eac, i, i_long, v, rownam, y, g, cr, crm)))

save.image("EAC_EORA_data.RData")

