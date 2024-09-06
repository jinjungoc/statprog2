# Functionality: R package for automatically clean, summarize and impute missing values in datasets

# Load devtools
install.packages("devtools")
library(devtools)

# Create a new package called 'dataCleaner'
create("dataCleaner")

#' Clean Data Function
#' 
#' This function removes rows with NA values from a given dataset.
#' @param data A data frame to be cleaned.
#' @return A cleaned data frame without NA values.
#' @example
#' clean_data(iris)
#' @export
clean_data <- function(data) {
  return(na.omit(data))
}


#' Summarize Data Function
#' 
#' This function returns basic summary statistics of numeric columns.
#' @param data A data frame.
#' @return Summary statistics for each numeric column.
#' @example
#' summarize_data(iris)
#' @export
summarize_data <- function(data) {
  return(summary(data))
}


#' Impute Missing Values Function
#' 
#' This function imputes missing values with column means.
#' @param data A data frame with missing values.
#' @return A data frame with missing values imputed.
#' @example
#' impute_missing(iris)
#' @export
impute_missing <- function(data) {
  numeric_cols <- sapply(data, is.numeric)
  data[numeric_cols] <- lapply(data[numeric_cols], function(x) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    return(x)
  })
  return(data)
}

roxygen2::roxygenise()

devtools::document()

devtools::check()

