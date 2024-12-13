% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReplaceMissingValue.R
\name{handle_missing}
\alias{handle_missing}
\title{Replace Missing Value with Column Means
This function replaces missing value (NA) in numeric columns of a data frame
with the means of the respective column.
Title}
\usage{
handle_missing(
  data,
  method = c("remove", "replace", "summary"),
  replacement = NULL
)
}
\arguments{
\item{replacement}{}
}
\value{
A data frame where NA values in numeric columns are replaced with the column means.
}
\description{
Replace Missing Value with Column Means
This function replaces missing value (NA) in numeric columns of a data frame
with the means of the respective column.
Title
}
\examples{
data <- c(1, 2, NA, 4, NA, 6)
print(missing_summary)
}
