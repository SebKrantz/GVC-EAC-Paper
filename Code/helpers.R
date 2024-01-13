
library(fastverse)
library(ggplot2)
library(scales)

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
