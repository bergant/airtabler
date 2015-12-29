#' airtabler: Interface to the Airtable API
#'
#' Provides access to the Airtable API (\url{http://airtable.com/api}).
#'
#' After you've created and configured the schema of an Airtable base from the
#' graphical interface, your Airtable base will provide its own API to create,
#' read, update, and destroy records (from \url{http://airtable.com/api})
#'
#' @section API key: \pkg{airtabler} functions will read the API key from
#'   environment variable \code{AIRTABLE_API_KEY}. To start R session with the
#'   initialized environvent variable create an .Renviron file in your R home
#'   with a line like this:
#'
#'   \code{AIRTABLE_API_KEY=************}
#'
#'   To check where your R home is, try \code{normalizePath("~")}.
#' @section Usage:
#'   Use \code{\link{air_get}}, \code{\link{air_insert}},
#'   \code{\link{air_update}} and \code{\link{air_delete}} to access your
#'   airtable data.
#' @docType package
#' @name airtabler-package
#' @aliases airtabler
NULL

air_url <- "https://api.airtable.com/v0"

air_api_key <- function() {
  key <- Sys.getenv("AIRTABLE_API_KEY")
  if(key == "") {
    stop("AIRTABLE_API_KEY environment variable is empty. See ?airtabler for help.")
  }
  key
}

to_url_params <- function(param_list) {
  # Convert parameters to url string
  # Example:
  #   to_url_params(list(a=1, b="2", c="An apple"))
  #   to_url_params(list(a=1, b="2", c="An apple"))
  # Result:
  #   #> [1] "a=1&b=2"
  for(i in seq_along(param_list)) {
    if(class(param_list[[i]]) == "character") {
      param_list[[i]] <- gsub(" ", "%20", param_list[[i]])
    }
  }
  res <-
    paste(
      sapply(names(param_list), function(n) {
        sprintf("%s=%s",n, param_list[n])
      }),
      collapse = "&"
    )
  URLencode(res)
}

#' Get a list of records or retreive a single
#'
#' You can retrieve records in an order of a view by providing the name or ID of
#' the view in the view query parameter. The results will include only records
#' visible in the order they are displayed.
#'
#' @param base Airtable base
#' @param table_name Table name
#' @param record_id (optional) Use record ID argument to retrieve an existing
#'   record details
#' @param limit (optional) A limit on the number of records to be returned.
#'   Limit can range between 1 and 100.
#' @param offset (optional) Page offset returned by the previous list-records
#'   call. Note that this is represented by a record ID, not a numerical offset.
#' @param view (optional) The name or ID of the view
#' @param sortField (optional) The field name to use for sorting
#' @param sortDirection (optional) "asc" or "desc". The sort order in which the
#'   records will be returned. Defaults to asc.
#' @param combined_result If TRUE (default) all data is returned in the same data
#'   frame. If FALSE table fields are returned in separate \code{fields} element.
#' @return A data frame with records or a list with record details if
#'   \code{record_id} is specified.
#' @export
air_get <- function(base, table_name, record_id = NULL,
                   limit = NULL, offset = NULL,
                   view = NULL,
                   sortField = NULL, sortDirection = NULL,
                   combined_result = TRUE) {

  search_path <- table_name
  if(!missing(record_id)) {
    search_path <- paste0(search_path, "/", record_id)
  }
  request_url <- sprintf("%s/%s/%s?", air_url, base, search_path)
  request_url <- URLencode(request_url)

  # append parameters to URL:
  param_list <- as.list(environment())[c(
    "limit", "offset", "view", "soetField", "sortDirection")]
  param_list <- param_list[!sapply(param_list, is.null)]
  request_url <- paste0(request_url, to_url_params(param_list) )

  # call service:
  res <- httr::GET(
    url = request_url,
    config = httr::add_headers(Authorization = paste("Bearer", air_api_key()))
  )
  air_validate(res)      # throws exception (stop) if error
  ret <- air_parse(res)  # returns R object
  if(combined_result && is.null(record_id)) {
    # combine ID, Fields and CreatedTime in the same data frame:
    ret <-
      cbind(
        id = ret$id, ret$fields, createdTime = ret$createdTime,
        stringsAsFactors =FALSE
      )
  }
  ret
}

air_validate <- function(res) {

  if(!inherits(res, "response")) {
    stop("Not a HTTP response object")
  }
  if(res$status_code >= 400) {
    err_message <- httr::content(res, as = "text")
    if(err_message != "") {
      error_info <- jsonlite::fromJSON(err_message)
    } else {
      error_info <- ""
    }
    if(is.list(error_info)) {
      err_message <- paste(error_info$error, collapse = "\n")
    } else {
      err_message <- error_info
    }
    stop("HTTP error: ", res$status_code, "\n", err_message, call. = FALSE)
  }
  if(substr(res$headers$`content-type`, 1, 16) != "application/json") {
    stop("Returned message is not a json", call. = FALSE)
  }
}

air_parse <- function(res) {
  res_obj <- jsonlite::fromJSON(httr::content(res, as = "text"))
  if(!is.null(res_obj$records)) {
    res <- res_obj$records
  } else {
    res <- res_obj
  }
  res
}




#' Insert a new record
#'
#' Creates a new record and returns the created record object if the call
#' succeeded, including a record ID which will uniquely identify the record
#' within the table.
#'
#' @param base Airtable base
#' @param table_name Table name
#' @param record_data Named list of values. You can include all, some, or none
#'   of the field values
#' @export
air_insert <- function(base, table_name, record_data) {

  if(inherits(record_data, "data.frame")) {
    return( air_insert_data_frame(base, table_name, record_data))
  }

  record_data <- air_prepare_record(record_data)
  json_record_data <- jsonlite::toJSON(list(fields = record_data))

  request_url <- sprintf("%s/%s/%s", air_url, base, table_name)

  # call service:
  res <- httr::POST(
    request_url,
    httr::add_headers(
      Authorization = paste("Bearer", air_api_key()),
      `Content-type` = "application/json"
    ),
    body = json_record_data
  )

  air_validate(res)  # throws exception (stop) if error
  air_parse(res)     # returns R object
}

air_insert_data_frame <- function(base, table_name, records) {
  lapply(seq_len(nrow(records)), function(i) {
    record_data <-
      unlist(as.list(records[i,]), recursive = FALSE)
    air_insert(base = base, table_name = table_name, record_data = record_data)
  })
}

air_update_data_frame <- function(base, table_name, record_ids, records) {
  lapply(seq_len(nrow(records)), function(i) {
    record_data <-
      unlist(as.list(records[i,]), recursive = FALSE)
    air_update(base = base,
               table_name = table_name,
               record_id = ifelse(is.null(record_ids), record_data$id, record_ids[i]),
               record_data = record_data)
  })
}


#' @rdname air_insert
#' @param x Object to be marked as a multiple value field
#' @export
multiple <- function(x) {
  class(x) <- c("air_multiple", class(x))
  x
}

air_prepare_record <- function(x) {
  # unbox all 1-sized elements which are not marked with "air_multiple" class:

  for(i in seq_along(x)) {
    if(inherits(x[[i]], "air_multiple")) {
      class(x[[i]]) <- class(x[[i]])[-1]
    } else {
      if(length(x[[i]]) == 1 ) {
        x[[i]] <- jsonlite::unbox(x[[i]])
      }
    }
  }
  x
}

#' Delete a record
#'
#' Deletes a record and returns the deleted record id if the call
#' succeeded.
#'
#' @param base Airtable base
#' @param table_name Table name
#' @param record_id Id of the record to be deleted
#' @export
air_delete <- function(base, table_name, record_id) {

  if(length(record_id) > 1) {
    return(air_delete_vec(base, table_name, record_id))
  }

  request_url <- sprintf("%s/%s/%s/%s", air_url, base, table_name, record_id)

  # call service:
  res <- httr::DELETE(
    request_url,
    httr::add_headers(
      Authorization = paste("Bearer", air_api_key())
    )
  )

  air_validate(res)  # throws exception (stop) if error
  air_parse(res)     # returns R object
}

air_delete_vec <- Vectorize(air_delete, vectorize.args = "record_id", SIMPLIFY = FALSE)





#' Update a record
#'
#' Updates a new record. Any fields that are not included will not be updated.
#'
#' @param base Airtable base
#' @param table_name Table name
#' @param record_id An id of the record
#' @param record_data Named list of values. You can include all, some, or none
#'   of the field values
#' @export
air_update <- function(base, table_name, record_id, record_data) {

  if(inherits(record_data, "data.frame")) {
    return(air_update_data_frame(base, table_name, record_id, record_data))
  }
  record_data <- air_prepare_record(record_data)
  json_record_data <- jsonlite::toJSON(list(fields = record_data))

  request_url <- sprintf("%s/%s/%s/%s", air_url, base, table_name, record_id)

  # call service:
  res <- httr::PATCH(
    request_url,
    httr::add_headers(
      Authorization = paste("Bearer", air_api_key()),
      `Content-type` = "application/json"
    ),
    body = json_record_data
  )

  air_validate(res)  # throws exception (stop) if error
  air_parse(res)     # returns R object
}

#' Get airtable base object
#'
#' Creates airtable object with tables and functions
#'
#' @param base Airtable base
#' @param tables Table names in the airtable base (character vector)
#' @return Airtable base object
#' @export
air_base <- function(base, tables) {
  res <- lapply(tables, function(x) air_table_funs(base, x))
  names(res) <- tables
  class(res) <- "airtable.base"
  attr(res, "base") <- base
  res
}

#' @keywords internal
#' @export
print.airtable.base <- function(x, ...) {
  cat("Airtable base object", attr(x, "base"), "\n")
  cat("Tables:", names(x), sep = "\n  ")
}

air_table_funs <- function(base, table_name) {

  res_list <- list()
  res_list[["get"]] <-
    function(
      record_id = NULL,
      limit = NULL, offset = NULL,
      view = NULL,
      sortField = NULL, sortDirection = NULL,
      combined_result = TRUE
    ){
      air_get(base, table_name, record_id, limit, offset, view, sortField, sortDirection, combined_result)
    }
  res_list[["insert"]] <-
    function(record_data) {
      air_insert(base, table_name, record_data)
    }
  res_list[["update"]] <-
    function(record_id, record_data) {
      air_update(base, table_name, record_id, record_data)
    }
  res_list[["delete"]] <-
    function(record_id) {
      air_delete(base, table_name, record_id)
    }

  res_list
}
