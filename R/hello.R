#' print hello world + some text
#'
#' @param x A dataframe with at least one column
#' @param col the name of the column that needs to be separated
#' @param separator a string that denotes the separator for separating columns
#' @return A dataframe
#' @export
#'
#' @examples
#' x <- data.frame(c("apple-mango", "x-223"))
#' colnames(x) = "c1"
#' hello(x, col = "c1", separator = "+")
hello <- function(x, col, separator) {
  x = x %>% tidyr::separate(col, into = c("c1","c2"), sep = separator)
  return(x)
}
