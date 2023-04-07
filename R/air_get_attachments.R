#' Get Airtable file attachments
#'
#' Extract the contents of an attachment stored in Airtable. Currently only setup
#' to work with Excel files. Planned expansion to other file types.
#' For excel files, returns a named list.
#'
#' @seealso \code{air_download_attachments}
#'
#' @param base String. ID for the base or app to be fetched
#' @param table_name String. Name of the table to be fetched from the base
#' @param field String. Name of field with file attachments in base
#' @param extract_type String. File type to be extracted.
#' Should be one of: excel
#' @param extract_field String. Name of extract field that will be created
#' @param ... Additional arguments to pass to \code{air_get}
#' @param download_file Logical. Should files be downloaded?
#' @param dir_name String. Where should files be downloaded to?
#' Will create the folder if it does not exist.
#' @param skip Numeric. How many lines should be skipped? See \code{readxl::read_excel} skip.
#' @param parse_all_sheets Logical. Should all sheets in spreadsheet be parsed?
#'
#' @return named list of data frames
#' @export air_get_attachments
#'
#' @examples
#'
#' \dontrun{
#'
#' base <- "appXXXXXXXXX"
#' table_name <- "table with excel attachments"
#'
#'  table_with_attachments <- air_get_attachments(base,table_name, field = "attachment_field" )
#'
#' }
#'
#'
air_get_attachments <- function(base, table_name, field, download_file = FALSE, dir_name = "downloads", extract_type ="excel", extract_field ="excel_extract", skip = 0, parse_all_sheets = FALSE, ...){

  # get data
  x <- fetch_all(base,table_name,...)

  ### subset to necessary records ----

  # get files
  xfield <- purrr::pluck(x,field)

  ### get files ----

  if(download_file){
    x <- air_download_attachments(x,field = field,dir_name = dir_name)
  }

  ### extract excel ----

  if(extract_type == "excel"){

  xlist <- purrr::map(xfield,function(x){
    if(is.null(x$url)){
      ID <- x$id
      warning(sprintf("Record ID %s is null",ID))
      return(NULL)
    }
    read_excel_url(x$url, skip = skip,parse_all_sheets = parse_all_sheets) ## need to be able to pass additional arguments
  })

  ## add extract to data frame ----
  x[[extract_field]] <- xlist
  }

  return(x)
}




