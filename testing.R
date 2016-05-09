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

# Reading SPSS
spss <- read.spss("data/spss_SAQ.sav", to.data.frame = TRUE)

# Reading Excel
xlsx <- read_excel("data/excel_aachen.xlsx")

# Reading Stata Files
dta <- read.dta("data/stata_africa.dta")

# Reading csv
csv <- read_csv2(file = "data/csv_food_supply.csv")

## Function

# Inputs: file location, name of dataset
input_loc <- "data/excel_aachen.xlsx"
data_name <- "testdata"

# Split string
str <- strsplit(input_loc, "[.]")[[1]]

# File ending
filetype <- str[length(str)]

if (filetype == "sav") {
  assign(data_name,
         read.spss(file = input_loc, to.data.frame = TRUE))
} else if (filetype == "dta") {
  assign(data_name,
         read.dta(file = input_loc))
} else if (any(filetype == c("xls", "xlsx"))) {
  asign(data_name,
        read_excel(path = input_loc))
}

