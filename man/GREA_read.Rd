% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Read.R
\name{GREA_read}
\alias{GREA_read}
\title{Read Data Wrapper}
\usage{
GREA_read(filelocation, header = FALSE, sep = " ", dec = ".",
  sheetIndex = 1, na.values, skip = 0, encoding = "unknown")
}
\arguments{
\item{filelocation}{A single string with the location and name of the file, e.g. \dQuote{data/blabla.csv}}

\item{header}{A logical indicating whether a text-delimited file contains a header row.}

\item{sep}{A character string specifying a column separator.}

\item{dec}{A character string specifying a decimal separator.}

\item{sheetIndex}{A numerical value to indicate which sheet to import (Excel formats).}

\item{na.values}{A character string specifying which values to convert to NA's.
while importing. Can be a vector of values when reading text-delimited files (\code{\link{read.table}}),
and should be a single value when importing Excel files (\code{\link[readxl]{read_excel}}).}

\item{skip}{How many rows should be skipped? Only for text-delimited files and Excel files.}

\item{encoding}{Encoding of text-delimited files (see \code{\link{read.table}})}
}
\value{
A dataframe, containing the read-in data plus the code to read the data as an attribute.
}
\description{
Reads Data of many different formats. Currently: .dta (STATA), .sav (SPSS), .mat (MATLAB), .xls/.xlsx (Excel), and .raw, .csv, .txt, .asc, .dat. Is the basis-function for the GREA add-in.
}

