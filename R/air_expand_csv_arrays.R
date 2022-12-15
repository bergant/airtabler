#' Expand arrays stored in CSVs
#'
#' This function helps users work with airtable data that has been exported to CSVs.
#' Because airtable uses nested data structures (json arrays), the data must be
#' flattened to be stored in a csv. The standard way to store arrays in a csv
#' is to wrap the array in quotes and separate each item with commas. So an array
#' stored in a csv would look like "item 1,item 2,...,item n". This function will
#' convert arrays stored in csvs to either a list or a vector and removes the
#' surrounding quotes.
#'
#'
#' @param x Character. likely a vector or field in a dataframe.
#' @param simplify_to_vector Logical. Should expanded arrays be converted from
#' lists to vectors? For lists with multiple elements at a given position, the length of the output
#' may be greater than the length of the input. See [tidyr::unnest()] for expanding
#' list columns.
#'
#'
#' @return A vector or list of expanded arrays.
#' @export air_expand_csv_arrays
#'
#' @examples
#'
#'
#' # example vector data
#' x <- c("item 1,item 2,item 3","apple,orange,banana","1,2,3","")
#'
#' # to list
#' air_expand_csv_arrays(x)
#'
#' # to vector
#' air_expand_csv_arrays(x,simplify_to_vector =  TRUE)
#'
#'
air_expand_csv_arrays <- function(x,simplify_to_vector = FALSE){

  if(!is.character(x)){
    rlang::abort("x must be class character. x is class {class(x)}")
  }

  split_x  <- stringr::str_split(x,",")

  x_out <- purrr::map(.x = split_x,.f = stringr::str_replace_all,pattern = "^\"|\"$",replacement = "")

  if(simplify_to_vector){
    x_out <-  purrr::as_vector(x_out,.type = "character")
  }

  return(x_out)

}
