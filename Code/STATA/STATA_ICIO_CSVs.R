library(fastverse)
fastverse_extend(qs, decompr)

#######################
# EMERGING
#######################

path <- "/Users/sebastiankrantz/Documents/Data/EMERGING"

EM <- qread(paste(path, "EMERGING_5_Sectors.qs", sep = "/")) # _Agg : Need sectoral detail for accurate GVC indicators

ICIO_path <- paste(path, "ICIO_CSV", sep = "/")
list.files(ICIO_path)
unlink(ICIO_path, recursive = TRUE)
dir.create(ICIO_path)

# Write countrylist
EM$Regions |> get_vars(1) |> 
  fwrite(paste(ICIO_path, "EM_countrylist.csv", sep = "/"), col.names = FALSE)

# Now loop across datasets
years <- names(EM$DATA)

for (y in years) {
  cat(y, fill = TRUE)
  x <- EM$DATA[[y]]
  qDF(setRownames(cbind(x$T, x$FD))) |> 
    fwrite(paste0(ICIO_path, "/EM_", y, ".csv"), col.names = FALSE)
}

#######################
# EORA 
#######################

path <- "/Users/sebastiankrantz/Documents/Data/EORA"

EORA <- new.env()
load("Data/EAC_EORA_2021_data.RData", envir = EORA) # _broad_sec

ICIO_path <- paste(path, "ICIO_CSV", sep = "/")
unlink(ICIO_path, recursive = TRUE)
dir.create(ICIO_path)

# Write countrylist
data.frame(Detailed_Region_Code = EORA$r) |> 
  fwrite(paste(ICIO_path, "EORA_countrylist.csv", sep = "/"), col.names = FALSE)


# Now loop across datasets
years <- EORA$y

for (y in years) {
  cat(y, fill = TRUE)
  qDF(setRownames(cbind(EORA$T[, , y], EORA$FD[, , y]))) |> 
    fwrite(paste0(ICIO_path, "/EORA_", y, ".csv"), col.names = FALSE)
}

# Now Execute ICIO_decomp.do

#######################
# Template Data
#######################

# Create Template Data Frame: Not really needed !
GVC_grid <- expand.grid(from_sector = EM$Sectors$Broad_Sector_Code,
                        from_region = EM$Regions$Detailed_Region_Code,
                        to_sector = EM$Sectors$Broad_Sector_Code,
                        to_region = EM$Regions$Detailed_Region_Code) |> 
            colorder(from_region, from_sector, to_region, to_sector) |> 
            subset(from_region %!=% to_region)

GVC_grid |> fwrite(paste(ICIO_path, "GVC_grid.csv", sep = "/"))

