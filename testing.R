#------------------------------------------------------------------#
# R-Projekt: Gotta Read Em All (GREA)
# Authors: Stanislaus Stadlmann
# Module: Statistical Programming with R (University of Göttingen)
#------------------------------------------------------------------#

# Delete everything
rm(list = ls())

# Set WD as the location from Document
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
rm(path)

# Trying out stuff
library(readr)
library(foreign)

# Reading SPSS
spss <- read.spss("data/spss_SAQ.sav", to.data.frame = TRUE)


## erstmal alles lesen, dann gucken wie man . (trenner) rauskriegt, 
# dann funktion schreiben die alle lesen kann

