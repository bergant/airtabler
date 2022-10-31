#' Get Airtable file attachments
#'
#' Get an attachment stored in air tables. For excel files, returns a named list.
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
#'
#' @return named list of data frames
#' @export air_get_attachments
#'
#' @examples
air_get_attachments <- function(base, table_name, field, download_file = FALSE, dir_name = "downloads", extract_type ="excel", extract_field ="excel_extract", skip = 0, ...){
  #browser()
  # get data
  x <- fetch_all(base,table_name,...)

  ### subset to necessary records ----

  # get files
  xfield <- purrr::pluck(x,field)

  ### get files ----

  if(download_file){
    dir.create(dir_name)

    xlist <- purrr::map(xfield, function(x){

      if(is.null(x$url)){
        ID <- x$id
        warning(sprintf("Record ID %s is null",ID))
        return(NULL)
      }

      dest <- sprintf("%s/%s", dir_name,x$filename)

     utils::download.file(url = x$url,destfile = dest)
    })

    message("downloaded files in ./downloads")

  }

  ### extract excel ----

  if(extract_type == "excel"){

  xlist <- purrr::map(xfield,function(x){
    if(is.null(x$url)){
      ID <- x$id
      warning(sprintf("Record ID %s is null",ID))
      return(NULL)
    }
    read_excel_url(x$url, skip = skip) ## need to be able to pass additional arguments
  })

  ## add extract to data frame ----
  x[[extract_field]] <- xlist
  }

  return(x)
}




