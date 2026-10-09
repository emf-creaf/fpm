# Example datasets (partly fake, partly not) for use in functions.
path <- "C://Roberto//Ecosystem Modelling Facility//IPM//Subset of IFN2 and IFN3 data for fpm R package"

# Example stand data.

load(file.path(path, "maxdbh.Rdata"))
load(file.path(path, "seedlings.Rdata"))
load(file.path(path, "saplings.Rdata"))
load(file.path(path, "trees.Rdata"))

# Example climate data.
path <- "C:\\Roberto\\Ecosystem Modelling Facility\\IPM\\Nuevos ajustes funciones fpm\\"
load(file.path(path, "climateSpain.Rdata"))

# Match idplot.
idplot <- unique(trees$idplot, saplings$idplot, seedlings$idplot)
idplot <- intersect(idplot, climateSpain$idplot)

# Filter seedlings, saplings and trees to match plots in climateSpain.
seedlings <- seedlings[seedlings$idplot %in% idplot, ]
saplings <- saplings[saplings$idplot %in% idplot, ]
treesIFN <- trees[trees$idplot %in% idplot, ]
climateSpain <- climateSpain[climateSpain$idplot %in% idplot, ]

# Save data for use within the package.
usethis::use_data(maxdbh, overwrite = TRUE)
usethis::use_data(seedlings, overwrite = TRUE)
usethis::use_data(saplings, overwrite = TRUE)
usethis::use_data(treesIFN, overwrite = TRUE)
usethis::use_data(climateSpain, overwrite = TRUE)

