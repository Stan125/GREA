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
library(readxl)
library(R.matlab)

## Function

GREA_fun <- function(filelocation) {
  
  if (is.null(filelocation)) 
    return(NULL)
  
  # Split string
  str <- strsplit(filelocation, "[.]")[[1]]
  
  # File ending
  filetype <- str[length(str)]
  
  # ------ Files without any other options ------ #
  
  # STATA: .dta
  if (filetype == "dta")
    data <- read.dta(file = filelocation)
  
  # MATLAB: .mat
  else if (filetype == "mat")
    data <- readMat(con = filelocation)
  
  # ------ Files with sep, header, dec, NA options ------ #
  
  # raw, csv, txt, asc, dat
  
  # ------ Files with other options ------ #
  
  # SPSS: .sav
  else if (filetype == "sav")
    data <- read.spss(file = filelocation, to.data.frame = TRUE)
  
  # Excel: .xls, .xlsx
  else if (any(filetype == c("xls", "xlsx")))
    data <- read_excel(path = filelocation)
  
  
  
  # Give back DF
  return(data)
}




