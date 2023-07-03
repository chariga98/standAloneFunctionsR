#' Drop Unused Levels
#' 
#' @description
#' This function drops unused levels from specified columns in a data frame.
#'
#' @param data A data frame.
#' @param columns A character vector specifying the column names in `dat` from which unused levels should be dropped.
#'
#' @return A modified version of the input data frame `data` with unused levels dropped from the specified columns.
#' 
#' @export
#'
#' @examples
#' # Create a data frame with a factor column
#' df <- data.frame(
#'   x = factor(c("A", "B", "C", "D")),
#'   y = c(1, 2, 3, 4)
#' )
#' 
#' # Drop unused levels from the 'x' column
#' df_modified <- drop_unused_levels(df, "x")
#' levels(df_modified$x)
#' # Output: "A" "B" "C" "D"
#' 
#' # Drop unused levels from multiple columns
#' df_modified <- drop_unused_levels(df, c("x", "y"))
#' levels(df_modified$x)
#' # Output: "A" "B" "C" "D"
#' 
#' levels(df_modified$y)
#' # Output: 1 2 3 4 
#' 
#' 
drop_unused_levels <- function(data, columns) {
  for(i in seq_along(columns)) {
    if(is.factor(data[[columns[i]]])) data[[columns[i]]] <- droplevels(data[[columns[i]]])
  }
  return(dat)
}
