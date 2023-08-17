#' Get a list of records
#'
#' Retrieve records where the request url would be over 16k characters (e.g. listing
#' many fields)
#'
#'You can retrieve records in an order of a view by providing the name or ID of
#' the view in the view query parameter. The results will include only records
#' visible in the order they are displayed.
#'
#' @param base String. Airtable base
#' @param table_name String. Table name
#' @param limit Numeric. (optional) A limit on the number of records to be returned.
#'   Limit can range between 1 and 100.
#' @param offset Numeric. (optional) Page offset returned by the previous list-records
#'   call. Note that this is represented by a record ID, not a numerical offset.
#' @param view String. (optional) The name or ID of the view
#' @param sortField String. (optional) The field name to use for sorting
#' @param sortDirection String. (optional) "asc" or "desc". The sort order in which the
#'   records will be returned. Defaults to asc.
#' @param combined_result Logical. If TRUE (default) all data is returned in the same data.
#'  If FALSE table fields are returned in separate \code{fields} element.
#' @param fields List. (optional) Only data for fields whose names are in this list
#'   will be included in the records. Does not work when retrieving individual records with \code{record_id}
#' @param filterByFormula String. Use a formula to filter results. See \href{airtable docs}{https://support.airtable.com/hc/en-us/articles/223247187-How-to-sort-filter-or-retrieve-ordered-records-in-the-API}
#'   this parameter to reduce the amount of data transferred.
#' @return A data frame with records
#' @export

air_post <- function(base,
                     table_name,
                     limit = NULL,
                     offset = NULL,
                     view = NULL,
                     fields = NULL,
                     sortField = NULL,
                     sortDirection = NULL,
                     filterByFormula = NULL,
                     combined_result = TRUE){

  search_path <- table_name

  request_url <- sprintf("%s/%s/%s/listRecords", air_url, base, search_path)
  request_url <- utils::URLencode(request_url)

  # append parameters to URL:
  param_list <- as.list(environment())[c(
    "limit", "offset", "view", "sortField", "sortDirection","filterByFormula")]
  param_list <- param_list[!sapply(param_list, is.null)]
  if(!is.null(fields)) {
    param_list <- c(param_list, list_params(x = fields, par_name = "fields"))
  }

  #request_url <- httr::modify_url(request_url, query = param_list)
  #request_url <- gsub(pattern = "fields=",replacement = "fields%5B%5D=",x = request_url)

  "Note Airtable's API only accepts request with a URL shorter
  than 16,000 characters. Encoded formulas may cause your
  requests to exceed this limit. To fix this issue you can
  instead make a POST request to
  /v0/{baseId}/{tableIdOrName}/listRecords while
  passing the parameters within the body of the
  request instead of the query parameters."

  res <- httr::POST(
    url = request_url,
    config = httr::add_headers(Authorization = paste("Bearer", air_api_key())),
    body = jsonlite::toJSON(param_list),
    encode="json"
  )

  air_validate(res)      # throws exception (stop) if error
  ret <- air_parse(res)  # returns R object
  if(combined_result) {
    # combine ID, Fields and CreatedTime in the same data frame:
    ret <-
      cbind(
        id = ret$id, ret$fields, createdTime = ret$createdTime,
        stringsAsFactors =FALSE
      )
  }

  return(ret)

}
