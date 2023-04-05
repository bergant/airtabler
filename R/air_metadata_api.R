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

type_option_map <-function(){

  "https://airtable.com/developers/web/api/field-model"

  # time options are deeply nested
 NULL


}

#' Template for for creating a table from a dataframe
#'
#' @param name
#' @param description
#' @param type
#' @param options
#'
#' @return
#' @export
#'
#' @examples
#'
air_fields_df_template <- function(name,description, type, options = NA){
  df <- tibble::tibble(name = name,
                       description = description,
                       type = type,
                       options = options)

  return(df)
}

air_fields_list_from_template <- function(df){
  ## create a list of field objects
  purrr::pmap(df, function(name,
                           type,
                           description,
                           options){

    field_list <- list(name = name,
                       type = type)

    if(!is.na(description)){
      field_list$description <- description
    }

    if(!all(is.na(options))){
      field_list$options <- options
    }
    return(field_list)
  })

}

#' Template for lists that describe tables in Airtable
#'
#' @param table_name String. Name of table
#' @param description String. Description of the table
#' @param fields_df Data frame. Data frame describing the field in a table.
#' Should contain a name, description,type, and options field.
#' if
#'
#' @return List with table name, description, and fields
#' @export air_table_template
#'
#' @examples
air_table_template <- function(table_name, description, fields_df ){

  valid_cols <- c("description","name","type","options")

  invalid_names_check <- !names(fields_df)%in%valid_cols

  if(any(invalid_names_check)){

    msg <- glue::glue("Invalid column name in field_df: {names(fields_df)[invalid_names_check]}
                valid column names are: {valid_cols}")
    stop(msg)
  }

  # check for required columns
  required_cols <- valid_cols[1:3]

  required_names_check <- !required_cols %in% names(fields_df)

  if(any(required_names_check)){

    msg <- glue::glue("Missing required column name in fields_df: {required_cols[required_names_check]}
                required column names are: {required_cols}")
    stop(msg)
  }

  ## check that types have necessary options - TODO

  ## check that options have appropriate values - TODO

  ## convert data frame to list of field objects for easier translation to JSON

  fields_list <- air_fields_list_from_template(df = fields_df)

  ## create output
  table_list <- list(
    "name" =  table_name,
    "description" =  description,
    "fields" = fields_list
  )

  return(table_list)
}

#' A function to create new tables in a base
#'
#' Takes a list object with appropriate arguments (see \code{air_table_template})
#' converts it to JSON then adds it to the specified base.
#'
#' @note  See https://airtable.com/developers/web/api/create-table
#' @param base String. ID for the base
#' @param table_list List. see \code{air_table_template}
#'
#' @return Data frame of table schema
#' @export air_create_table
#'
#' @examples
air_create_table <- function(base, table_list){
  request_url <- sprintf("%s/%s/tables", air_meta_url, base)
  request_url <- utils::URLencode(request_url)

  fields_json <- jsonlite::toJSON(table_list,pretty = TRUE,auto_unbox = TRUE)

  # call service:
  res <- httr::POST(
    request_url,
    httr::content_type("application/json"),
    httr::add_headers(
      Authorization = paste("Bearer", air_api_key())
    ),
    body = fields_json
  )

  air_validate(res)
  # may need a new air_parse function

  res_content <- httr::content(res,as = "text")

  schema <- jsonlite::fromJSON(res_content)

  return(schema)
}

#' Create a new field in a table
#'
#'  See https://airtable.com/developers/web/api/create-field
#'
#' @param base String. Base id
#' @param table_id String. Table id. Can be found using \code{air_get_schema}
#' @param name String. Name of the field
#' @param description String. Description of the field
#' @param type String. Type of field. See https://airtable.com/developers/web/api/field-model
#' @param options Data frame. See https://airtable.com/developers/web/api/field-model
#'
#' @return description of newly created field as a list
#' @export air_create_field
#'
#' @examples
air_create_field <- function(base,
                             table_id,
                             name,
                             description = NA,
                             type = "singleLineText",
                             options= NA){

  field_df <- air_fields_df_template(name = name,description = description, type = type, options = options)


  fields_list <- air_fields_list_from_template(field_df)

  "https://api.airtable.com/v0/meta/bases/{baseId}/tables/{tableId}/fields"

  request_url <- sprintf("%s/%s/tables/%s/fields", air_meta_url, base,table_id)
  request_url <- utils::URLencode(request_url)

  ## fields must be created one at a time
  schema_list <- purrr::map(fields_list,function(field_item){
    fields_json <- jsonlite::toJSON(field_item,pretty = TRUE,auto_unbox = TRUE)

    # call service:
    res <- httr::POST(
      request_url,
      httr::content_type("application/json"),
      httr::add_headers(
        Authorization = paste("Bearer", air_api_key())
      ),
      body = fields_json
    )

    air_validate(res)
    # may need a new air_parse function

    res_content <- httr::content(res,as = "text")

    schema <- jsonlite::fromJSON(res_content)

    return(schema)
  })

  return(schema_list)

}

#' Get list of bases for an Token
#'
#' @return list
#' @export
#'
#' @examples
air_list_bases <- function(request_url = "https://api.airtable.com/v0/meta/bases"){

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

  base_list <- jsonlite::fromJSON(res_content)

  return(base_list)
}


