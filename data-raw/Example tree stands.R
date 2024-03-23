# Recovering data from subdirectories.

# Example stand data.
direc <- "C:\\Roberto\\Ecosystem Modelling Facility\\IPM\\Subset of IFN2 and IFN3 data for fpm R package\\"
maxdbh <- load(paste0(direc, "maxdbh.Rdata"))
seedlings <- load(paste0(direc, "seedlings.Rdata"))
saplings <- load(paste0(direc, "saplings.Rdata"))
trees <- load(paste0(direc, "trees.Rdata"))

# Example climate data.
direc <- "C:\\Roberto\\Ecosystem Modelling Facility\\IPM\\Nuevos ajustes funciones fpm\\"
climateSpain <- load(paste0(direc, "climateSpain.Rdata"))

# Save it.
usethis::use_data(maxdbh,
                  seedlings,
                  saplings,
                  trees,
                  climateSpain,
                  overwrite = TRUE)
