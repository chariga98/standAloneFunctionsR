#' Title
#'
#' @param prefix 
#' @param existing_names 
#' @param include_index 
#' @param start_index 
#'
#' @return
#' @export
#'
#' @examples
next_default_item = function(prefix, existing_names = c(), include_index = FALSE, start_index = 1) {
  if(!is.character(prefix)) stop("prefix must be of type character")
  
  if(!include_index) {
    if(!prefix %in% existing_names) return(prefix)
  }
  
  item_name_exists = TRUE
  start_index = 1
  while(item_name_exists) {
    out = paste0(prefix,start_index)
    if(!out %in% existing_names) {
      item_name_exists = FALSE
    }
    start_index = start_index + 1
  }
  return(out)
}