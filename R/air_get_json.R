#' Get a list of records or retrieve a single record as JSON
#'
#' Returns JSON objects from GET requests
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
#' @param combined_result If TRUE (default) all data is returned in the same data.
#'  If FALSE table fields are returned in separate \code{fields} element.
#' @param fields (optional) Only data for fields whose names are in this list
#'   will be included in the records. If you don't need every field, you can use
#' @param pretty Logical. Should JSON be returned in human readable form?
#'   this parameter to reduce the amount of data transferred.
#' @return A data frame with records or a list with record details if
#'   \code{record_id} is specified.
#'
#' @export air_get_json
air_get_json <- function(base, table_name,
                         record_id = NULL,
                         limit = NULL,
                         offset = NULL,
                         view = NULL,
                         fields = NULL,
                         sortField = NULL,
                         sortDirection = NULL,
                         combined_result = TRUE,
                         pretty = FALSE) {

  search_path <- table_name

  if(!missing(record_id)) {
    search_path <- paste0(search_path, "/", record_id)
  }
  request_url <- sprintf("%s/%s/%s?", air_url, base, search_path)
  request_url <- utils::URLencode(request_url)

  # append parameters to URL:
  param_list <- as.list(environment())[c(
    "limit", "offset", "view", "sortField", "sortDirection")]
  param_list <- param_list[!sapply(param_list, is.null)]
  if(!is.null(fields)) {
    param_list <- c(param_list, list_params(x = fields, par_name = "fields"))
  }

  request_url <- httr::modify_url(request_url, query = param_list)
  request_url <- gsub(pattern = "fields=",replacement = "fields%5B%5D=",x = request_url)

  # call service:
  res <- httr::GET(
    url = request_url,
    config = httr::add_headers(Authorization = paste("Bearer", air_api_key()))
  )
  air_validate(res)      # throws exception (stop) if error
  ret <- httr::content(res, as = "text") #returns json object
  if(pretty){
    ret <- jsonlite::prettify(ret)
  }
  return(ret)

}


#' Get the full outputs of a table as single json object
#'
#' @param base String. Base ID
#' @param table_name String. Table name
#' @param ... additional parameters to pass to air_get_json
#'
#' @return json as string
#' @export fetch_all_json
#'
#' @examples
#'
#' \dontrun{
#' base <- "appXXXXXXX"
#' table_name <- "My Table"
#'
#' fetch_all_json(base, table_name)
#'
#' }
#'
fetch_all_json <- function(base, table_name, ...) {
  out <- list()
  out[[1]] <- air_get_json(base, table_name, combined_result = FALSE,...)
  if(length(out[[1]]) == 0){
    emptyTableMessage <- glue::glue("The queried view for {table_name} in {base} is empty")
    warning(emptyTableMessage)
    return(emptyTableMessage)
  } else {
    offset <- airtabler::get_offset(air_parse(out[[1]])) #parse out offset
    while (!is.null(offset)) {
      json_text <- list(air_get_json(base, table_name, combined_result = FALSE, offset = offset, ...))
      out <- c(out, json_text)
      offset <- airtabler::get_offset(air_parse(out[[length(out)]]))
    }

    ## map over json text and remove offset attribute
    out_json <- purrr::map_chr(out, function(x){

      x_drop_offset <- gsub( '\\],\"offset\\":\\"\\w{1,}/\\w{1,}\\"}$',"",x)

    })

    # drop initial json section in all but first record
    out_json_2 <- out_json

    for(i in 1:length(out_json)){
      if(i > 1){
        out_json_2[i] <- gsub("\\{\"records\"\\:\\[",",",out_json[i])
      }
      next()
    }

    # make single json string
    json_delimited <- paste(out_json_2,collapse = "")

    return(json_delimited)

  }
}
