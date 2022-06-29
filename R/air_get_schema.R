#' Get base schema - enterprise only
#'
#' Metadata API currently only available via enterprise accounts.
#' Get the schema for the tables in a base.
#'
#' @section Using Metadata API:
#' The meta data api is not accepting new requests for client secrets as of
#' 06 July 2021. Will update when metadata api becomes available.
#'
#' @param base Airtable base ID
#' @param ... additional paramters
#'
#' @return list of schema
#' @export air_get_schema

air_get_schema <-  function(base,...){
  request_url <- sprintf("%s/%s/tables", air_meta_url, base)
  request_url <- utils::URLencode(request_url)

  # call service:
  res <- httr::GET(
    request_url,
    httr::add_headers(
      Authorization = paste("Bearer", air_api_key()),
      "X-Airtable-Client-Secret" = air_secret_key()
    )
  )

  air_validate(res)
  # may need a new air_parse function

  schema <- jsonlite::fromJSON(res)

  return(schema)
}
