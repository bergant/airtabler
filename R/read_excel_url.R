#' Read an excel file from URL
#'
#' Extends \code{readxl::read_excel} to allow for reading from a URL.
#' @param url String. Url for file
#' @param fileext String. File extension for temp file
#' @param ... additional arguments to pass to \code{read_excel}
#'
#' @return tibble
#' @export read_excel_url
#'
#' @examples
read_excel_url <- function(url, fileext= ".xslx",...){
  tmp <- tempfile(fileext = ".xslx")
  curl::curl_download(url, tmp )
  readxl::read_excel(tmp,...)
}
