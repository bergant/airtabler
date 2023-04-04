#' Download Airtable file attachments
#'
#' Download an attachment stored in air tables. Returns original dataframe
#' with an additional field called attachment_file_paths. The attachment_file_paths
#' field is of class list so it can handle multiple attachments per record.
#'
#' @param x Data frame. Output from air_get or fetch_all.
#' @param field String. Name of field with file attachments in base
#' @param dir_name String. Where should files be downloaded to?
#' Will create the folder if it does not exist. Folders created are recursively.
#' @param ... reserved for additional arguments.
#'
#' @return Returns x with an additional field called attachment_file_paths
#' @export air_download_attachments
#'
#' @examples
air_download_attachments <- function(x, field, dir_name = "downloads",...){
  #browser()

  if(!is.data.frame(x)){
    rlang::abort("x is not a dataframe")
  }

  if(!field %in% names(x)){
    error_msg <- glue::glue("{field} not found in names(x). Check the name of the column
                            used to store attachments in airtable")

    rlang::abort(error_msg)
  }

  if(!is.list(x[,field])){
    error_msg <- glue::glue("{field} is not of class list. Verify the name of
    the column used to store attachments in airtable")
    rlang::warn(error_msg)

    field_file_paths <- sprintf("%s_file_paths",field)

    x$file_path <- NA

    # using dynamic names in case a base has multiple file attachment
    # columns
    x <- dplyr::rename(x,{{field_file_paths}} := file_path)
    return(x)
  }

  ### subset to necessary records ----

  # get files
  xfield <- purrr::pluck(x,field)

  ### get files ----
    dir.create(path = dir_name,recursive = TRUE)

    xlist <- purrr::map(xfield, function(x){

      if(is.null(x$url)){
        ID <- x$id
        warning(sprintf("Record ID %s is null",ID))
        return(NULL)
      }

      # prepending attachment id in case the file naming convention
      # of the user does not preclude duplicate file names for files
      # with different contents - e.g. original file generation was
      # structured like sample_1234/fasta.file sample_1235/fasta.file

      dest <- sprintf("%s/%s_%s", dir_name,x$id,x$filename)

       a <- utils::download.file(url = x$url,destfile = dest)
       print(a)

      return(dest)
    })

    down_load_message <- glue::glue("Files downloaded here {dir_name}")

    message(down_load_message)

    field_file_paths <- sprintf("%s_file_paths",field)

    x$file_path <- xlist

    # using dynamic names in case a base has multiple file attachment
    # columns
    x <- dplyr::rename(x,{{field_file_paths}} := file_path)
    return(x)

}




