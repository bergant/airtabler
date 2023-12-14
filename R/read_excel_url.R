#' Read an excel file from URL
#'
#' Extends \code{readxl::read_excel} to allow for reading from a URL.
#'
#' @param url String. Url for file
#' @param fileext String. File extension for temp file
#' @param ... additional arguments to pass to \code{read_excel}
#' @param parse_all_sheets Logical. Should all sheets be parsed?
#'
#' @return tibble or list of tibbles if parse_all_sheets = TRUE
#' @export read_excel_url
#'
read_excel_url <- function(url, fileext= ".xslx",parse_all_sheets = FALSE,...){
  tmp <- tempfile(fileext = ".xslx")
  curl::curl_download(url, tmp )
  if(parse_all_sheets){
  sheets <- readxl::excel_sheets(tmp)
  xl_list <- purrr::map(sheets,function(x){
    readxl::read_excel(path = tmp,sheet = x,...)
  })

  names(xl_list) <- sheets

  return(xl_list)
  }

  readxl::read_excel(path = tmp,...)


}
