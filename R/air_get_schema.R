#' Get base schema
#'
#' Get the schema for the tables in a base. This is a wrapper for the api call
#' Get base schema.
#'
#' @section Using Metadata API:
#' Metadata api is currently available to all users.
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
      Authorization = paste("Bearer", air_api_key())
    )
  )

  air_validate(res)
  # may need a new air_parse function

  res_content <- httr::content(res,as = "text")

  schema <- jsonlite::fromJSON(res_content)

  return(schema)
}
