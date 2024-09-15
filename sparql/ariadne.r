# Script with some example queries to ARIADNE KB using glitter package
# By Petr Pajdla, Brno 2024-09-15
# For 'atRium' training school, https://www.aiscr.cz/atRium

# Packages 
# You will need to install `glitter` package from GitHub
# remotes::install_github("lvaudor/glitter")

library(glitter)
library(dplyr)
library(ggplot2)
library(sf)

# ARAIDNE KB SPARQL ednpoint
endpoint <- "https://graphdb.ariadne.d4science.org/repositories/ariadneplus-pr01"
