library(decompr)

# load the data 
data(leather)
# list the objects in the data set 
ls()

# inter = IO matrix: each row shows the use of a country-industries output
# each column shows inputs to production of that country-industry
# so each entry is a payment of the column country-industry to the row country-industry,
# or equivalently the derivery of goods for that value from the row country-industry for production in the column country-industry

# final shows the final use of goods produced by each country-industry (row) in each country (column)

# Output is thus the total use of each country-industries (rows) goods for either intermediate production or final use 
# Check
all.equal(rowSums(final) + rowSums(inter), out)

# Value added conversely must be equal to output minus intermediate inputs:
va <- out - colSums(inter)

# Now exports are is the output (row) used for foreign intermediate inputs and foreign final demand: 
G <- length(countries)
N <- length(industries)
GN <- G * N # number of country industries
fdc <- dim(final)[2]/G # final demand categories (in each country)

E <- cbind(inter, final)
for (j in 1:G) {
  m <- 1 + (j - 1) * N
  n <- N + (j - 1) * N
  s <- GN + 1 + (j - 1) * fdc
  r <- GN + fdc + (j - 1) * fdc
  E[m:n, m:n] <- 0
  E[m:n, s:r] <- 0
}
replace(E, E == 0, NA) # see what this does
E <- rowSums(E)
E


# create the decompr object 
decompr_object <- load_tables_vectors(x = inter, 
                                      y = final, 
                                      k = countries, 
                                      i = industries, 
                                      o = out)
ls(decompr_object)

# Check out this function
x = inter; y = final; k = countries; i = industries; o = out
function (x, y, k, i, o, v = NULL, null_inventory = FALSE) 
{
  G <- length(k)
  N <- length(i)
  GN <- G * N
  rownam <- as.vector(t(outer(k, i, paste, sep = "."))) # This creates country-industry names
  fdc <- dim(y)[2]/G # This computes the number of final demand categories (in each country)
  if (null_inventory == TRUE) { # when the inventory (last FDC) should be set to zero
    y[, fdc * (1:G)] <- 0
  }
  Bd <- Ad <- matrix(0, nrow = GN, ncol = GN) # GN x GN matrices
  Yd <- ESR <- Eint <- Efd <- Y <- matrix(0, nrow = GN, ncol = G) # GN x G matrices
  if (is.null(v)) { # Computing VA if needed
    v <- o - colSums(x)
  }
  A <- x * outer(rep.int(1L, length(o)), o) # t(t(x)/o) # or sweep(x, 1, o, "/") # or use for loop # row-normalized IO matrix: shares of total country-industry output (row) used in production of column countr-industry
  A[!is.finite(A)] <- 0 
  Am <- A # Am is imported coefficient matrix. 
  II <- diag(GN) # GN x GN identity matrix
  Bm <- B <- solve(II - A) # Leontief inverse: o = A o + y -> o = (II - A)^(-1)y where y is final demand -> production in each country-industry to satisfy final demand, so B contains final demand multipiliers...
  for (j in 1:G) {
    m = 1 + (j - 1) * N
    n = N + (j - 1) * N
    Ad[m:n, m:n] <- A[m:n, m:n] # Ad = domestic IO matrix
    Bd[m:n, m:n] <- B[m:n, m:n] # Bd = domestic leontief inverse matrix
    Bm[m:n, m:n] <- 0           # Bm = imported goods leotief inverse matrix
    Am[m:n, m:n] <- 0           # Am = imported goods IO matrix
  }
  L <- solve(II - Ad) # Domestic goods only leontief inverses -> production to satisfy domestic final demand
  Vc <- v/o           # Value added content (share) of output
  Vc[!is.finite(Vc)] <- 0
  if (fdc > 1) { # If more than one final demand category per country, sum over those categories
    for (j in 1:G) {
      m <- 1 + (j - 1) * fdc
      n <- fdc + (j - 1) * fdc
      Y[, j] <- rowSums(y[, m:n])
    }
  } else if (fdc == 1) { # Only one final demand category per country, Y is final demand y
    Y <- y
  }
  Ym <- Y # foreign (imported) final demand
  E <- cbind(x, y) # Getting exports: output not used in domestic intermediates of domestic final demand
  for (j in 1:G) {
    m <- 1 + (j - 1) * N
    n <- N + (j - 1) * N
    s <- GN + 1 + (j - 1) * fdc
    r <- GN + fdc + (j - 1) * fdc
    E[m:n, m:n] <- 0
    E[m:n, s:r] <- 0
    Yd[m:n, j] <- Y[m:n, j] # Domestic final demand
    Ym[m:n, j] <- 0         # Foreign final demand
  }
  z <- E # Saving export matrix 
  E <- as.matrix(rowSums(E)) # This computes exports
  for (j in 1:G) {
    m <- 1 + (j - 1) * N
    n <- N + (j - 1) * N
    s <- GN + 1 + (j - 1) * fdc
    r <- GN + fdc + (j - 1) * fdc
    if (s == r) { # if only once final demand category
      Efd[, j] <- z[, s:r] # Export final demand # put only s or r
    }
    else {
      Efd[, j] <- rowSums(z[, s:r])
    }
    Eint[, j] <- rowSums(z[, m:n])           # Foreign demanded intermediate goods
    ESR[, j] <- rowSums(z[, m:n]) + Efd[, j] # Total exports to each country
  }
  names(Vc) <- rownam
  names(o) <- rownam
  rownames(Y) <- rownam
  rownames(ESR) <- rownam
  names(E) <- rownam
  dimnames(B) <- dimnames(A)
  dimnames(Bm) <- dimnames(A)
  dimnames(Bd) <- dimnames(A)
  dimnames(Am) <- dimnames(A)
  dimnames(L) <- dimnames(A)
  colnames(ESR) <- k
  colnames(Y) <- k
  dimnames(Ym) <- dimnames(Y)
  dimnames(Eint) <- dimnames(ESR)
  dimnames(Efd) <- dimnames(ESR)
  out <- list(Am = Am, B = B, Bd = Bd, Bm = Bm, L = L, E = E, 
              ESR = ESR, Eint = Eint, Efd = Efd, Vc = Vc, G = G, GN = GN, 
              i = i, k = k, N = N, rownam = rownam, X = o, Y = Y, 
              Yd = Yd, Ym = Ym)
  class(out) <- "decompr"
  return(out)
}


lt <- leontief(decompr_object)

wwz <- wwz(decompr_object)