#' Get unique values from a field
#'
#' Because the airtable api lacks an easy solution for getting
#' unique values from a field in a table, we have this function
#' which accepts a list of fields and returns their unique
#' values. Currently, if multiple fields are listed, all unique
#' values from all fields will be returned in a single vector.
#' This may change in future iterations.
#'
#'
#' @param base String. ID of airtable base
#' @param table_name String. Name of table in base
#' @param fields  List. Names of fields
#'
#' @return vector of unique values
#' @export
#'
get_unique_field_values <- function(base,
                                    table_name,
                                    fields){

  baseField <- airtabler::fetch_all(base, table_name , fields = fields )

  if(is.character(baseField)){
    return("")
  } else {

    fieldsVector  <- unlist(fields)

    uniqueField <- unlist(unique(baseField[fieldsVector]))

    return(uniqueField)

  }

}
