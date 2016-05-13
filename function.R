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
library(data.table)
library(readxl)

## Function

# Inputs: file location, name of dataset
filelocation <- "data/excel_aachen.xlsx"
name <- "testdata"

GREA_fun <- function(input_loc) {
  
  if (is.null(input_loc)) 
    return(NULL)
  
  # Split string
  str <- strsplit(input_loc, "[.]")[[1]]
  
  # File ending
  filetype <- str[length(str)]
  
  # SPSS: sav
  if (filetype == "sav")
    data <- read.spss(file = input_loc, to.data.frame = TRUE)
  
  # STATA: dta
  else if (filetype == "dta")
    data <- read.dta(file = input_loc)
  
  # Excel: xls, xlsx
  else if (any(filetype == c("xls", "xlsx")))
    data <- read_excel(path = input_loc)
  
  # Give back DF
  return(data)
}




