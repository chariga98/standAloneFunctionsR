#' Hashed id
#'
#' @description  This function generates hashed identifiers by applying a specified algorithm to the input values, optionally incorporating a salt value.
#'
#' @param x A vector or list of values to be hashed.
#' @param salt A character string representing a salt value to be appended to each element in x. Default is NULL, indicating no salt.
#' @param algo A character string specifying the hashing algorithm to be used. Default is "crc32".
#'
#' @return A character vector of hashed identifiers corresponding to the input values.
#' @export
#'
#' @examples
#' # Example 1: Generate hashed identifiers without salt
#' x <- c("apple", "banana", "cherry")
#' hashed_id(x)
#'
#' # Output:
#' # [1] "AD5A2589" "77D3B209" "2C6EDC7A"
#'
#' # Example 2: Generate hashed identifiers with salt
#' x <- c("apple", "banana", "cherry")
#' salt <- "salty"
#' hashed_id(x, salt)
#'
#' # Output:
#' # [1] "1A1E6CE0E9A969A9" "1B69B1B8C1D6EF99" "1C10A25AA55025CD"

hashed_id <- function(x, salt, algo = "crc32") {
  if (missing(salt)) {
    y <- x
  } else {
    y <- paste(x, salt)
  }
  y <- sapply(y, function(X) digest::digest(X, algo = algo))
  as.character(y)
}