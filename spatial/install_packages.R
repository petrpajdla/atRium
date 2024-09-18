# Install required packages ----
required_packages <- c("terra", "sf", "tmap", "tmaptools", "geodata",
                       "dplyr", "tidyr", "spData", "grid")

if(!all(required_packages %in% installed.packages())) {
  packages_to_install <- which(!required_packages %in% installed.packages())
  install.packages(required_packages[packages_to_install],
                   dependencies = TRUE,
                   repos = "https://ftp.gwdg.de/pub/misc/cran/")
}