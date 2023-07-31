#' Get base schema
#'
#' Get the schema for the tables in a base. This is a wrapper for the api call
#' Get base schema.
#'
#' @section Using Metadata API:
#' Metadata api is currently available to all users.
#'
#' @param base String. Airtable base ID
#' @param ... reserved for additional parameters
#'
#' @return list of schema
#' @export air_get_schema

air_get_schema <-  function(base, ...){
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

  # will be used to  validate options provided to different field types
  "https://airtable.com/developers/web/api/field-model"

  # time options are deeply nested
 NULL


}

#' Template for for creating a table from a tibble
#'
#' Convenience function for creating the content of tables that will created or
#' updated viaAPI.
#'
#' @param name String. Names of fields in the table
#' @param description String. Descriptions of fields
#' @param type String. Type of columns. For values see \url{https://airtable.com/developers/web/api/model/field-type}
#' @param options List. Options will be converted from lists to JSON. For field options see \url{https://airtable.com/developers/web/api/field-model}
#'
#' @return Tibble with attributes required for fields in a table
#' @export air_fields_df_template
#'
#' @examples
#'
#'\dontrun{
#' base <- "appQ94sELAtFnXPxx"
#'
#' base_schema <- air_get_schema(base)
#'
#' tables<- base_schema$tables
#'
#' field_names <- c("Planet","Chapter","Book", "Known Inhabitants")
#'
#' field_desc <- c("Name of planet in Foundation Series",
#'                 "Chapters where planet is referenced",
#'                 "Books where planet is referenced",
#'                 "Characters mentioned as living on or being from that planet")
#'
#' field_types <- c("singleLineText",rep("multipleRecordLinks",3))
#'
#' field_options <- c(NA,list(
#'   list(
#'     linkedTableId = tables[tables$name == "Chapter","id"]
#'   )
#' ),
#' list(
#'   list(
#'     linkedTableId = tables[tables$name == "Book","id"]
#'   )
#' ),
#' list(
#'   list(
#'     linkedTableId = tables[tables$name == "Character","id"]
#'   )
#' )
#' )
#'
#' field_df<- air_fields_df_template(name = field_names,
#'                                   description = field_desc,
#'                                   type = field_types,
#'                                   options = field_options)
#'
#' table_list <- air_table_template(table_name = "Planet",
#'                                   description = "Planets of Foundation",
#'                                   fields_df = field_tables)
#'
#' air_create_table(base, table_list)
#'}
air_fields_df_template <- function(name,description, type, options = NA){
  df <- tibble::tibble(name = name,
                       description = description,
                       type = type,
                       options = options)

  return(df)
}

#' Convert field data frame to list
#'
#' Converts the field data frame to a list of easier translation to JSON
#'
#' @param df Data frame. From air_fields_df_template
#'
#' @return List. Structured for easy parsing into JSON
#' @export air_fields_list_from_template
#'
#' @examples
#'\dontrun{
#' base <- "appQ94sELAtFnXPxx"
#'
#' base_schema <- air_get_schema(base)
#'
#' tables<- base_schema$tables
#'
#' field_names <- c("Planet","Chapter","Book", "Known Inhabitants")
#'
#' field_desc <- c("Name of planet in Foundation Series",
#'                 "Chapters where planet is referenced",
#'                 "Books where planet is referenced",
#'                 "Characters mentioned as living on or being from that planet")
#'
#' field_types <- c("singleLineText",rep("multipleRecordLinks",3))
#'
#' field_options <- c(NA,list(
#'   list(
#'     linkedTableId = tables[tables$name == "Chapter","id"]
#'   )
#' ),
#' list(
#'   list(
#'     linkedTableId = tables[tables$name == "Book","id"]
#'   )
#' ),
#' list(
#'   list(
#'     linkedTableId = tables[tables$name == "Character","id"]
#'   )
#' )
#' )
#'
#' field_df <- air_fields_df_template(name = field_names,
#'                                   description = field_desc,
#'                                   type = field_types,
#'                                   options = field_options)
#'
#' fields_list <- air_fields_list_from_template(df = fields_df)
#'
#'}
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
#'
#' \dontrun{
#' base <- "appQ94sELAtFnXPxx"
#'
#' base_schema <- air_get_schema(base)
#'
#' tables<- base_schema$tables
#'
#' field_names <- c("Planet","Chapter","Book", "Known Inhabitants")
#'
#' field_desc <- c("Name of planet in Foundation Series",
#'                 "Chapters where planet is referenced",
#'                 "Books where planet is referenced",
#'                 "Characters mentioned as living on or being from that planet")
#'
#' field_types <- c("singleLineText",rep("multipleRecordLinks",3))
#'
#' field_options <- c(NA,list(
#'   list(
#'     linkedTableId = tables[tables$name == "Chapter","id"]
#'   )
#' ),
#' list(
#'   list(
#'     linkedTableId = tables[tables$name == "Book","id"]
#'   )
#' ),
#' list(
#'   list(
#'     linkedTableId = tables[tables$name == "Character","id"]
#'   )
#' )
#' )
#'
#' field_df<- air_fields_df_template(name = field_names,
#'                                   description = field_desc,
#'                                   type = field_types,
#'                                   options = field_options)
#'
#' table_list <- air_table_template(table_name = "Planet",
#'                                  description = "Planets of Foundation",
#'                                  fields_df = field_tables)
#'
#' air_create_table(base, table_list)
#'}
#'
#'
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
#'
#' \dontrun{
#' base <- "appQ94sELAtFnXPxx"
#'
#' base_schema <- air_get_schema(base)
#'
#' tables<- base_schema$tables
#'
#' field_names <- c("Planet","Chapter","Book", "Known Inhabitants")
#'
#' field_desc <- c("Name of planet in Foundation Series",
#'                 "Chapters where planet is referenced",
#'                 "Books where planet is referenced",
#'                 "Characters mentioned as living on or being from that planet")
#'
#' field_types <- c("singleLineText",rep("multipleRecordLinks",3))
#'
#' field_options <- c(NA,list(
#'   list(
#'     linkedTableId = tables[tables$name == "Chapter","id"]
#'   )
#' ),
#' list(
#'   list(
#'     linkedTableId = tables[tables$name == "Book","id"]
#'   )
#' ),
#' list(
#'   list(
#'     linkedTableId = tables[tables$name == "Character","id"]
#'   )
#' )
#' )
#'
#' field_df<- air_fields_df_template(name = field_names,
#'                                   description = field_desc,
#'                                   type = field_types,
#'                                   options = field_options)
#'
#' table_list <- air_table_template(table_name = "Planet",
#'                                  description = "Planets of Foundation",
#'                                  fields_df = field_tables)
#'
#' air_create_table(base, table_list)
#'
#' }
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
#' \dontrun{
#' base_schema <- air_get_schema(base)
#'
#' base_schema$tables
#'
#' air_create_field(base,table_id = base_schema$tables$id[[4]],
#'                  name = "Has Nucleics",
#'                  description = "Logical. Does this planet have nucleics?",
#'                  type = "checkbox",
#'                  options = list(
#'                    list(
#'                      "color"= "greenBright",
#'                      "icon"= "check"
#'                    )
#'                  )
#' )
#' }
#'
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


#' Update a field name and/or description
#'
#' Must update either the name or the description.
#' See "https://airtable.com/developers/web/api/update-field" for more details.
#'
#' @param base String. Base id
#' @param table_id String. ID for table that contains the field to be updated
#' @param field_id String. ID of field to be updated
#' @param name String. updated name (optional)
#' @param description String. updated description (option)
#'
#' @return List. Describes the changes that happened to the field
#' @export air_update_field
#'
#' @examples
#' \dontrun{
#' base <- "appVjIfAo8AJlfTkx"
#'
#' schema <- air_get_schema("appVjIfAo8AJlfTkx")
#'
#' table_id <- schema$tables[1,c("id")]
#'
#' field_id <- schema$tables$fields[[1]][2,]$id
#'
#' ## update name and description
#'
#' name <- "New Name"
#'
#' description <- "Updated Description"
#'
#' out <- air_update_field(base = base,table_id = table_id,field_id = field_id,name = name, description = description)
#'
#' ### just name
#'
#' name <- "New New Name"
#'
#' out <- air_update_field(base = base,table_id = table_id,field_id = field_id,name = name)
#'
#'
#' ## just description
#'
#' description <- "Better description"
#'
#' out <- air_update_field(base = base,table_id = table_id,field_id = field_id,description = description)
#'
#' ## set name to number
#'
#' name <- 1234
#'
#' out <- air_update_field(base = base,table_id = table_id,field_id = field_id,name = name)
#'
#'
#' # set description to number
#'
#' description <- 1234
#'
#' out <- air_update_field(base = base,table_id = table_id,field_id = field_id,description = description)
#'
#' # bulk update names and descriptions from a data frame
#'
#' field_ids <- schema$tables$fields[[1]]$id
#'
#' field_names <- sprintf("%s_bulk_update",schema$tables$fields[[1]]$name)
#'
#' field_descriptions <- sprintf("%s BULK UPDATE",schema$tables$fields[[1]]$description)
#'
#' df <- data.frame("field_id"= field_ids,"name"=field_names,"description"=field_descriptions)
#'
#' purrr::pmap(df,function(field_id,name,description){
#'   air_update_field(base = base,table_id = table_id,field_id = field_id,name = name, description = description)
#' })
#' }
air_update_field<- function(base, table_id, field_id, name = NULL, description = NULL){

  if(rlang::is_empty(name) & rlang::is_empty(description)){
    rlang::abort("Must provide either a name or description")
  }

  ## create the field list
  field_list<- list()

  # add name property
  if(!rlang::is_empty(name)){
    assertthat::assert_that(is.character(name),length(name) ==1)
    field_list$name <- name
  }

  # add description property
  if(!rlang::is_empty(description)){
    assertthat::assert_that(is.character(description),length(description) ==1)
    field_list$description <- description
  }

  assertthat::assert_that(length(field_list) > 0)

  # create
  "https://api.airtable.com/v0/meta/bases/{baseId}/tables/{tableId}/fields/{columnId}"

  request_url <- sprintf("%s/%s/tables/%s/fields/%s", air_meta_url, base,table_id,field_id)
  request_url <- utils::URLencode(request_url)


  ## fields must be updated one at a time
    fields_json <- jsonlite::toJSON(field_list,pretty = TRUE,auto_unbox = TRUE)

    # call service:
    res <- httr::PATCH(
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

#' Get list of bases for an Token
#'
#' Each token you provisision is given access to a certain set of bases or
#' workspaces. This function lists all bases associated with a token.
#'
#' @param request_url String. URL for api endpoint
#'
#' @return list. List of bases a token can access.
#' @export air_list_bases
#'
#'
#' @examples
#'
#' \dontrun{
#' air_list_bases()
#' }
#'
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



