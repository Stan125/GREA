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

## Function: obtain_filetype obtains the filetype of a file in a string
obtain_filetype <- function(filelocation) {
  if (is.null(filelocation)) 
    return(NULL)
  
  # Split string
  str <- strsplit(filelocation, "[.]")[[1]]
  
  # File ending
  filetype <- str[length(str)]
  
  return(filetype)
}


## Function: GREA_fun

GREA_fun <- function(filelocation, header = FALSE, sep = "", dec = ".",
                     into.dataframe = TRUE, sheetIndex = 1) {
  
  # Wrap TryCatch around to specify error messages
  tryCatch({
  
  if (is.null(filelocation)) 
    return(NULL)
  
  filetype <- obtain_filetype(filelocation)
  
  # ------ Files without any options ------ #
  
  # STATA: .dta
  if (filetype == "dta")
    data <- read.dta(file = filelocation)
  
  # MATLAB: .mat
  else if (filetype == "mat")
    data <- readMat(con = filelocation)
  
  # ------ Files with sep, header, dec, NA options ------ #
  
  # raw, csv, txt, asc, dat
  else if (any(filetype == c("raw", "csv", "txt", "asc", "dat")))
    data <- read.table(file = filelocation, header = header, sep = sep, dec = dec)
  
  # ------ Files with other options ------ #
  
  # SPSS: .sav
  else if (filetype == "sav")
    data <- read.spss(file = filelocation, to.data.frame = into.dataframe)
  
  # Excel: .xls, .xlsx
  else if (any(filetype == c("xls", "xlsx")))
    data <- read_excel(path = filelocation, sheet = sheetIndex)
  
  # Give back DF
  return(data)
  },
  
  # ------ Files with other options ------ #
  error = function(err) {
    message("1. Wrong options for generating df, or") 
    message("2. Outcome is not a df (for Excel and SPSS reader functions)")
  },
  warning = function(war) {
    message("1. Wrong options for generating df, or") 
    message("2. Outcome is not a df (for Excel and SPSS reader functions)")
  })
}