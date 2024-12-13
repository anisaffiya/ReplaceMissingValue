#' Replace Missing Value with Column Means
#' This function replaces missing value (NA) in numeric columns of a data frame
#' with the means of the respective column.
#' Title
#'
#'
#' @param data
#' @param method
#' @param replacement
#'
#' @return A data frame where NA values in numeric columns are replaced with the column means.
#' @export
#'
#' @examples
#' data <- c(1, 2, NA, 4, NA, 6)
#' print(missing_summary)
handle_missing <- function(data, method = c("remove", "replace", "summary"), replacement = NULL) {
  method <- match.arg(method)

  if (method == "remove") {
    return(na.omit(data))
  } else if (method == "replace") {
    if (is.null(replacement)) {
      stop("Please provide a replacement value.")
    }
    return(ifelse(is.na(data), replacement, data))
  } else if (method == "summary") {
    return(list(
      total_missing = sum(is.na(data)),
      percent_missing = mean(is.na(data)) * 100,
      missing_indices = which(is.na(data))
    ))
  }
}


data <- c(1, 2, NA, 4, NA, 6)

cleaned_data <- handle_missing(data, method = "remove")
cat("Cleaned Data:", cleaned_data, "\n")

replaced_data <- handle_missing(data, method = "replace", replacement = 0)
cat("Replaced Data:", replaced_data, "\n")

missing_summary <- handle_missing(data, method = "summary")
print(missing_summary)
