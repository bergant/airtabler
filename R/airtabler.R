#' airtabler: Interface to the Airtable API
#'
#' Provides access to the Airtable API (\url{http://airtable.com/api}).
#'
#' @section Setup:
#'   Create and configure the schema of an Airtable base on (\url{http://airtable.com})
#'   and check the API on \url{http://airtable.com/api}.
#'
#' @section API key:
#'   Generate the airtable API key from your Airtable account page
#'   (http://airtable.com/account).
#'
#'   \pkg{airtabler} functions will read the API key from
#'   environment variable \code{AIRTABLE_API_KEY}. To start R session with the
#'   initialized environment variable create an \code{.Renviron} file in your R home
#'   with a line like this:
#'
#'   \code{AIRTABLE_API_KEY=************}
#'
#'   To check where your R home is, try \code{normalizePath("~")}.
#' @section Usage:
#'   Use \code{\link{airtable}} function to get airtable base object
#'   or just call primitives \code{\link{air_get}}, \code{\link{air_insert}},
#'   \code{\link{air_update}} and \code{\link{air_delete}} to access your
#'   airtable data.
#' @docType package
#' @name airtabler-package
#' @aliases airtabler
NULL

air_url <- "https://api.airtable.com/v0"
air_meta_url <- "https://api.airtable.com/v0/meta/bases"

# consider consolidating keys as the API key is now a token that can access
# the metadata api
air_api_key <- function() {
  key <- Sys.getenv("AIRTABLE_API_KEY")
  if(key == "") {
    stop("AIRTABLE_API_KEY environment variable is empty. See ?airtabler for help.")
  }
  key
}

# consider
air_secret_key <- function(){
  key <- Sys.getenv("AIRTABLE_SECRET_KEY")
  if(key == "") {
    stop("AIRTABLE_SECRET_KEY environment variable is empty. See ?airtabler for help.")
  }
  key
}


#' Get a list of records or retrieve a single
#'
#' Retrieve records or a single record from a table. If you provide a record_id,
#' you cannot specify fields, views, or filterFormulas.
#'
#'You can retrieve records in an order of a view by providing the name or ID of
#' the view in the view query parameter. The results will include only records
#' visible in the order they are displayed.
#'
#' @param base Airtable base
#' @param table_name Table name
#' @param record_id (optional) Use record ID argument to retrieve an existing
#'   record details. See \url{https://airtable.com/developers/web/api/get-record}
#' @param limit (optional) A limit on the number of records to be returned.
#'   Limit can range between 1 and 100.
#' @param offset (optional) Page offset returned by the previous list-records
#'   call. Note that this is represented by a record ID, not a numerical offset.
#' @param view (optional) The name or ID of the view
#' @param sortField (optional) The field name to use for sorting
#' @param sortDirection (optional) "asc" or "desc". The sort order in which the
#'   records will be returned. Defaults to asc.
#' @param combined_result If TRUE (default) all data is returned in the same data.
#'  If FALSE table fields are returned in separate \code{fields} element.
#' @param fields (optional) Only data for fields whose names are in this list
#'   will be included in the records. If you don't need every field, you can use
#' @param filterByFormula String. Use a formula to filter results. See \href{airtable docs}{https://support.airtable.com/hc/en-us/articles/223247187-How-to-sort-filter-or-retrieve-ordered-records-in-the-API}
#'   this parameter to reduce the amount of data transferred.
#' @return A data frame with records or a list with record details if
#'   \code{record_id} is specified.
#' @export
air_get <- function(base, table_name,
                   record_id = NULL,
                   limit = NULL,
                   offset = NULL,
                   view = NULL,
                   fields = NULL,
                   sortField = NULL,
                   sortDirection = NULL,
                   filterByFormula = NULL,
                   combined_result = TRUE) {

  search_path <- table_name

  if(!missing(record_id)) {
    search_path <- paste0(search_path, "/", record_id)
  }
  request_url <- sprintf("%s/%s/%s?", air_url, base, search_path)
  request_url <- utils::URLencode(request_url)

  # append parameters to URL:
  param_list <- as.list(environment())[c(
    "limit", "offset", "view", "sortField", "sortDirection","filterByFormula")]
  param_list <- param_list[!sapply(param_list, is.null)]
  if(!is.null(fields)) {
    param_list <- c(param_list, list_params(x = fields, par_name = "fields"))
  }

  request_url <- httr::modify_url(request_url, query = param_list)
  request_url <- gsub(pattern = "fields=",replacement = "fields%5B%5D=",x = request_url)

  #print(request_url)
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

list_params <- function(x, par_name) {
  # converts a list of sublists to a list
  # Example:
  # sort = list(
  #   list(field="Avg Review", direction = "desc"),
  #   list(field="Price/night", direction = "asc"))
  # list_params(sort, "sort")
  #
  # fields = list("Country", "Name")
  # list_params(fields, "field")
  if(!is.list(x)) {
    stop(par_name, " parameter must be a list")
  }
  names(x) <- sprintf("%s[%s]", par_name, 0:(length(x)-1))
  if(is.list(x[[1]])) {
    x <- unlist(x, recursive = FALSE)
    names(x) <- gsub("\\.", "[", names(x))
    names(x) <- gsub("$", "]", names(x))
  }
  x
}


#' Select
#'
#' Select records from table
#'
#' @section View:
#'   You can retrieve records in an order of a view by providing the name or ID of
#'   the view in the view query parameter. The results will include only records
#'   visible in the order they are displayed.
#'
#' @section Filter by formula:
#'   The formula will be evaluated for each record, and if the result is not 0,
#'   false, "", NaN, [], or #Error! the record will be included in the response.
#'   If combined with view, only records in that view which satisfy the formula
#'   will be returned. For example, to only include records where Country isn't
#'   empty, pass in: NOT({Country} = '')
#'
#' @section Sorting:
#'   Each sort object must have a field key specifying the name of
#'   the field to sort on, and an optional direction key that is either "asc" or
#'   "desc". The default direction is "asc".
#'   For example, to sort records by Country, pass in: \code{list(field =
#'   "Country", direction = "desc")}
#'
#' @param base Airtable base
#' @param table_name Table name
#' @param record_id (optional) Use record ID argument to retrieve an existing
#'   record details
#' @param fields (optional) Only data for fields whose names are in this list
#'   will be included in the records. If you don't need every field, you can use
#'   this parameter to reduce the amount of data transferred.
#' @param filterByFormula (optional) A formula used to filter records.
#' @param maxRecord (optional) The maximum total number of records that will be
#'   returned.
#' @param sort A list of sort objects that specifies how the records will be
#'   ordered.
#' @param view (optional) The name or ID of the view defined in the table
#' @param pageSize (optional) The number of records returned in each request.
#'   Must be less than or equal to 100. Default is 100.
#' @param offset (optional) To fetch the next page of records set this argument
#'   with a value of offset element from previous response
#' @param combined_result If TRUE (default) all data is returned in the same
#'   data frame. If FALSE table fields are returned in separate \code{fields}
#'   element.
#' @return A data frame with records or a list with record details if
#'   \code{record_id} is specified.
#' @export
air_select <- function(
  base, table_name, record_id = NULL,
  fields = NULL,
  filterByFormula = NULL,
  maxRecord = NULL,
  sort = NULL,
  view = NULL,
  pageSize = NULL,
  offset = NULL,
  combined_result = TRUE) {

  search_path <- table_name
  if(!missing(record_id)) {
    search_path <- paste0(search_path, "/", record_id)
  }
  request_url <- sprintf("%s/%s/%s?", air_url, base, search_path)
  request_url <- utils::URLencode(request_url)

  # append parameters to URL:
  param_list <- as.list(environment())[c(
    "filterByFormula", "maxRecord", "pageSize", "offset", "view")]
  param_list <- param_list[!sapply(param_list, is.null)]
  if(!is.null(sort)) {
    param_list <- c(param_list, list_params(x = sort, par_name = "sort"))
  }
  if(!is.null(fields)) {
    param_list <- c(param_list, list_params(x = fields, par_name = "fields"))
  }

  request_url <- httr::modify_url(url = request_url, query = param_list)
  # call service:
  res <- httr::GET(
    url = request_url,
    config = httr::add_headers(Authorization = paste("Bearer", air_api_key()))
  )
  air_validate(res)      # throws exception (stop) if error
  ret <- air_parse(res)  # returns R object
  offset <- attr(ret, "offset")
  if(combined_result && is.null(record_id) && length(ret) > 0) {
    # combine ID, Fields and CreatedTime in the same data frame:
    ret <-
      cbind(
        id = ret$id, ret$fields, createdTime = ret$createdTime,
        stringsAsFactors =FALSE
      )
    attr(ret, "offset") <- offset
  }
  ret
}

#' Get offset
#'
#' Returns airtable offset id from previous select
#'
#' @param x Last result
#' @return Airtable offset id
#' @export
get_offset <- function(x) {
  attr(x, "offset")
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
  if(is.character(res)){
    res_obj<- jsonlite::fromJSON(res)
  } else {
    res_obj <- jsonlite::fromJSON(httr::content(res, as = "text"))
  }
  if(!is.null(res_obj$records)) {
    res <- res_obj$records
    if(!is.null(res_obj$offset)) {
      attr(res, "offset") <- res_obj$offset
    }
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
#'
#'
air_insert <- function(base, table_name, record_data) {

  if( inherits(record_data, "data.frame")) {
    return( air_insert_data_frame(base, table_name, record_data))
  }

  # create json records
  json_record_data <- air_make_json(base, table_name, record_data)

  # call service:
  air_make_request(base,table_name,json_record_data, method = "POST")
}


#' Make JSON for API
#'
#' Make JSON that is compatible with the Airtable API.
#'
#' @param base String. Base in airtable
#' @param table_name  String. Table in airtable
#' @param record_data Dataframe, list, or vector. Data to be converted to JSON
#' @param record_id String or vector of strings. Records to be manipulated
#' @param method String. "PATCH" is necessary for \code{air_update}
#' @param typecast Logical. Should the typecast option be TRUE or FALSE? Typecast
#' allows you to add new options to select type fields.
#'
#' @return JSON with record data
#' @export
#'
#' @examples
air_make_json <- function (base, table_name, record_data, record_id = NULL, method = "POST",typecast = TRUE){
  if (inherits(record_data, "data.frame")) {
    return(air_insert_data_frame(base, table_name, record_data, typecast))
  }

  #browser()
  record_data <- air_prepare_record(as.list(record_data))
  fields <- list(fields = record_data)


  if(method == "PATCH"){
    ## the patch method already specifies a record so we
    ## can remove a lot of addition json elements

    ## drop the id field as it can't be updated
    fields$fields$id <- NULL

    if(typecast){
      # allows us to use values not currently specified in the
      # table or record eg issues = "some new issue"
      # use unbox to create singleton values instead of json objects
      fields$typecast <- jsonlite::unbox(typecast)
    }


    json_patch_data <- jsonlite::toJSON(fields, pretty = T)
    return(json_patch_data)
  }

  records <- list(records = list(fields))

  if(typecast){
    # allows us to use values not currently specified in the
    # table or record eg issues = "some new issue"
    # use unbox to create singleton values instead of json objects
    records$typecast <- jsonlite::unbox(typecast)
  }

  json_record_data <- jsonlite::toJSON(records, pretty = T)
  return(json_record_data)
}


#' Make an HTTP request
#'
#' Properly encodes HTTP requests
#'
#' @param base String. Base in airtable
#' @param table_name String. Table in airtable
#' @param json_record_data json or string. JSON formatted text with record data
#' @param record_id String or vector of strings. Record id
#' @param method String. One of "POST", "PATCH", or "DELETE"
#'
#' @return Status of HTTP request
#' @export
#'
#' @examples
air_make_request <- function(base, table_name, json_record_data, record_id = NULL, method = c("POST","PATCH","DELETE")){

  if(method == "POST"){

    request_url <- sprintf("%s/%s/%s", air_url, base, table_name)
    request_url <- utils::URLencode(request_url)

    res <- httr::POST(url = request_url,
                      httr::add_headers(
                        Authorization = paste("Bearer",air_api_key()),
                        'Content-type' = "application/json"),
                      body = json_record_data)
  }

  if(method == "PATCH"){

    ### Because the patch request url specifies the record,
    ### the json does not need to be as complete

    request_url <- sprintf("%s/%s/%s/%s", air_url, base, table_name, record_id)
    request_url <- utils::URLencode(request_url)

    #print(request_url)

    # browser()

    res <- httr::PATCH(url = request_url,
                      httr::add_headers(
                        Authorization = paste("Bearer",air_api_key()),
                        'Content-type' = "application/json"),
                      body = json_record_data)
  }

  if(method == "DELETE"){

    ### Because the patch request url specifies the record,
    ### the json does not need to be as complete

    request_url <- sprintf("%s/%s/%s/%s", air_url, base, table_name, record_id)
    request_url <- utils::URLencode(request_url)

    # call service:
    res <- httr::DELETE(
      request_url,
      httr::add_headers(
        Authorization = paste("Bearer", air_api_key())
      )
    )
  }

  air_validate(res) # throws exception (stop) if error
  air_parse(res) # returns R object
}

#' @param base String. Airtable base
#' @param table_name String. Table name
#' @param records Dataframe. Contains records you would like to insert
#' @param typecast Logical. Should airtable make new values for select type fields?
#'
#' @rdname air_insert
#' @export air_insert_data_frame
air_insert_data_frame <- function(base, table_name, records,typecast) {
  lapply(seq_len(nrow(records)), function(i) {
    record_data <- as.list(records[i,])
    json_record_data <- air_make_json(base, table_name, record_data,typecast)
    air_make_request(base = base,table_name = table_name ,json_record_data = json_record_data, method = "POST" )

  })
}

#' Update records from a dataframe
#'
#' Updates the values in a table by overwriting their current contents.
#'
#' @param base String. Airtable base
#' @param table_name String. Table name
#' @param record_ids Vector of strings. Records to be modified
#' @param records Dataframe. Values to update
#'
#' @return Status of HTTP request
#' @export
#'
#' @examples
air_update_data_frame <- function(base, table_name, record_ids, records) {
  lapply(seq_len(nrow(records)), function(i) {
    record_data <- as.list(records[i,])
    record_id <- ifelse(is.null(record_ids), record_data$id, record_ids[i])
    json_record_data <- air_make_json(base, table_name, record_data,
                                      record_id = record_id,method = "PATCH")

    air_make_request(base = base,table_name = table_name,
                     json_record_data = json_record_data,
                     record_id = record_id,
                     method = "PATCH")
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
      if(is.list(x[[i]])) {
        x[[i]] <- unlist(x[[i]])
      } else if(length(x[[i]]) == 1) {
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

  air_make_request(base = base,
                   table_name = table_name,
                   record_id = record_id,
                   method = "DELETE")

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

  method <- "PATCH"

  if(inherits(record_data, "data.frame")) {
    #print("using data.frame method")
    return(air_update_data_frame(base, table_name, record_id, record_data))
  }

  #create json records
  json_record_data <- air_make_json(base, table_name, record_data,
                                    record_id = record_id, method = method)

  # call service:
  air_make_request(base,table_name,json_record_data,record_id = record_id, method = method)
}

#' Get airtable base object
#'
#' Creates airtable object with tables and functions
#'
#' @param base Airtable base
#' @param tables Table names in the airtable base (character vector)
#' @return Airtable base object with elements named by table names.
#'   Each element contains functions
#'   \item{get}{returns table records, see \code{\link{air_get}} for details}
#'   \item{insert}{insert table record, see \code{\link{air_insert}} for details}
#'   \item{update}{updates table record, see \code{\link{air_update}} for details}
#'   \item{delete}{deletes table record, see \code{\link{air_delete}} for details}
#' @export
#' @examples
#' \dontrun{
#' TravelBucketList <-
#'   airtable(
#'     base = "the_base_id",
#'     tables = c("Destinations", "Hotels", "Travel Partners")
#'   )
#'   hotels <- TravelBucketList$Hotels$get()
#'   destinations <- TravelBucketList$Destinations$get()
#' }
airtable <- function(base, tables) {
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
  res_list[["select"]] <-
    function(
      record_id = NULL,
      fields = NULL,
      filterByFormula = NULL,
      maxRecord = NULL,
      sort = NULL,
      view = NULL,
      pageSize = NULL,
      offset = NULL,
      combined_result = TRUE
    ){
      air_select(base, table_name, record_id,
                 fields, filterByFormula, maxRecord, sort, view,
                 pageSize, offset, combined_result)
    }
  res_list[["select_all"]] <- function(
    record_id = NULL,
    fields = NULL,
    filterByFormula = NULL,
    maxRecord = NULL,
    sort = NULL,
    view = NULL,
    pageSize = NULL
  ){
    ret_all <- list()
    ret_offset = NULL
    while({
      ret <- air_select(
        base, table_name, record_id,
        fields, filterByFormula, maxRecord, sort, view,
        pageSize, ret_offset, combined_result = TRUE)
      ret_offset <- get_offset(ret)
      ret_all <- c(ret_all, list(ret))
      !is.null(ret_offset)
    }) {}
    if(length(ret_all) == 0) { return(list())}

    .bind_df(ret_all)
  }

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

.bind_df <- function(x) {
  # x = list of data frames
  if(length(unique(lengths(x))) != 1) {

    # add missing columns
    col_names <- unique(unlist(lapply(x, names)))
    col_missing <- lapply(x, function(x) setdiff(col_names, names(x)))

    x <- lapply(seq_along(x), function(i) {
      ret <- x[[i]]
      for(col in col_missing[[i]]) {
        ret[[col]] <- NA
      }
      ret
    })
  }

  do.call(rbind, x)

}

