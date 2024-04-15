library(fastverse)
fastverse_extend(qs, decompr)

EM <- qread("/Users/sebastiankrantz/Documents/Data/EMERGING/EMERGING_5_Sectors.qs")

k = EM$Regions$ISO3 # $Detailed_Region_Code
i = as.character(EM$Sectors$Agg_Sector_Code) # $Code)
y = names(EM$DATA)

decomps <- lapply(EM$DATA, function(x) {
  load_tables_vectors(x = x$T,
                      y = x$FD,
                      k = k,
                      i = i,
                      o = x$X,
                      v = x$VA)
})

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

VB <- lapply(decomps, leontief, post = "none", long = FALSE) |> simplify2array()
str(VB)

# Check: Close to 1 (exempting ROW category)
colMeans(apply(VB, 2:3, sum))

# Defining group object to aggregate to country level
rownam <- decomps[[1L]]$rownam
g <- GRP(cr(rownam), sort = FALSE)

T_ag <- lapply(EM$DATA, function(x) x$T %>% fsum(g) %>% t %>% fsum(g) %>% t) %>% 
  simplify2array()

str(T_ag)

FD_ag <- lapply(EM$DATA, function(x) x$FD %>% fsum(g)) %>% 
  simplify2array()

str(FD_ag)

VA_ag <- lapply(EM$DATA, function(x) x$VA %>% fsum(g)) %>% 
  simplify2array()

str(VA_ag)

out_ag <- lapply(EM$DATA, function(x) x$X %>% fsum(g)) %>% 
  simplify2array()

str(out_ag)

VB_ag <- lapply(decomps, leontief, post = "output", long = FALSE) %>%
  lapply(function(x) x %>% fsum(g) %>% t %>% fsum(g) %>% t) %r/% 
  mctl(out_ag) %>% simplify2array

str(VB_ag)
colMeans(apply(VB_ag, 2:3, sum))

EAC <- c("UGA", "TZA", "KEN", "RWA", "BDI", "SSD", "COD")
eac <- seq_len(length(i) * length(EAC)) 
dim(eac) <- c(length(i), length(EAC))
dimnames(eac) <- list(i, EAC)

# Removing temporary files
rm(list = setdiff(ls(), .c(decomps, T, FD, VA, out, va, va_VA, VB, T_ag, FD_ag, VA_ag, 
                           out_ag, va_ag, va_VA_ag, VB_ag, 
                           r, EAC, eac, i, i_long, v, rownam, y, g, cr, crm)))

save.image("Data/EMERGING_data_5_Sectors.RData")
