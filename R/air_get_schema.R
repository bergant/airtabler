air_get_schema <-  function(base,...){
  request_url <- sprintf("%s/%s/tables", air_meta_url, base)
  request_url <- URLencode(request_url)

  # call service:
  res <- httr::GET(
    request_url,
    httr::add_headers(
      Authorization = paste("Bearer", air_api_key()),
      X-Airtable-Client-Secret = air_secret_key()
    )
  )

  air_validate(res)
  # may need a new air_parse function

  schema <- jsonlite::fromJSON(res)

  return(schema)
}
