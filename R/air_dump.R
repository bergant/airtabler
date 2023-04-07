#' Get items that differ between x and y
#'
#' Unlike setdiff, this function creates the union of x and y then
#' removes values that are in the intersect, providing values
#' that are unique to X and values that are unique to Y.
#'
#' @param x a set of values.
#' @param y a set of values.
#'
#' @return Unique values from X and Y, NULL if no unique values.
#' @export
#'
#' @examples
#' a <- 1:3
#' b <- 2:4
#'
#' set_diff(a,b)
#' # returns 1,4
#'
#' x <- 1:3
#' y <- 1:3
#'
#' set_diff(x,y)
#' # returns NULL
#'
set_diff <- function(x,y){
  u <- union(x,y)
  i <- intersect(x,y)
  j <- (u %in% i)

  if(all(j)){
    return(NULL)
  }

  diff <- u[!(j)]
  return(diff)
}


#' Create a new structural metadata table in the base
#'
#' @param base String. Base id
#' @param meta_data Data frame. Contains metadata records. From air_generate_metadata*
#' @param table_name String. name of the metadata table. default is "Meta Data"
#' @param field_descriptions Character vector. Descriptions of metadata table fields
#' @param type Character vector. Column types for metadata table fields. see https://airtable.com/developers/web/api/field-model
#' @param options Data frame. Options for fields in metadata table.
#'
#' @return List with outcome from creating the table and inserting the records
#' @export air_create_metadata_table
#'
#' @examples
#'\dontrun{
#' # set base id
#' base <- "appXXXXXXXX"
#' # create metadata from api
#' metadata  <- air_generate_metadata_from_api(base)
#' # add Meta Data table to base -- will not work if base already has a metadata
#' # table
#' log <- air_create_metadata_table(base,metadata)
#'
#'}
#'
#'
air_create_metadata_table <- function(base,meta_data,table_name = "Meta Data",  field_descriptions = NA,
                                      type = "singleLineText", options = NA){

  # check for meta data table
  ## if exists, stop
  schema <- air_get_schema(base)

  if(table_name %in% schema$tables$name){
    msg <- glue::glue("{table_name} already exists in the base {base}.
                      Please use air_update_metadata_table to update the metadata table
                      or delete the table and re-run the function")
    stop(msg)
  }


  # create fields_df
  # add description for standard names


  #
  if(setequal(names(meta_data), c("field_name", "table_name", "field_desc",
                                  "field_type", "field_id",  "table_id",
                                  "field_opts",  "primary_key"))){

    # create description object
    field_descriptions <- c("https://schema.org/name",
                            "https://schema.org/name",
                            "https://schema.org/description",
                            "https://schema.org/category",
                            "https://schema.org/identifier",
                            "https://schema.org/identifier",
                            "https://schema.org/option",
                            "https://schema.org/Boolean"
    )


  }

  fields_df <- air_fields_df_template(name = names(meta_data),
                                      description = field_descriptions,
                                      type = type,
                                      options = options)

  # create list describing table

  table_list <- air_table_template(table_name = table_name,
                                   description = "structural metadata for the base",
                                   fields_df = fields_df)
  # create table

  outcome_create_table <- air_create_table(base, table_list)

  # insert data

  outcome_insert_data <-   tryCatch(
    air_insert_data_frame(base = base,table_name = table_name,records = meta_data),
    error=function(cond) {

      warning(cond)

      return("data not inserted")
    })

  if(is.character(outcome_insert_data)){
    stop("Table created but data not inserted. Check field types then use
         air_insert_data_frame or air_update_metadata_table to add metadata
         records.")
  }


  return(list("create_table" = outcome_create_table,
              "insert_data" = outcome_insert_data))

}

# add the description table to the base

#' Create the descriptive metadata table for the base
#'
#' @details DCMI terms can be found here \url{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/}
#'
#' @param base String. Base id
#' @param description Data frame. Description from air_get_base_description* or air_generate_base_description
#' @param table_name String. Name of description table
#' @param field_descriptions Character vector. Descriptions of metadata table fields. If NA, DCMI terms will be used where possible.
#' @param type Character vector. Column types for metadata table fields. see \url{https://airtable.com/developers/web/api/field-model}
#' @param options Data frame. Options for fields in metadata table.
#'
#'
#' @return List. Outputs from creating the table and inserting the records
#' @export air_create_description_table
#'
#' @examples
#'\dontrun{
#' base = "appVjIfAo8AJlfTkx"
#' table_name= "description"
#'
#' description <- air_generate_base_description(title = "Example Base",
#'                                              creator = "Collin Schwantes")
#'
#' air_create_description_table(base,description,table_name)
#'}
#'
air_create_description_table <- function(base,
                                         description,
                                         table_name = "Description",
                                         field_descriptions = NA,
                                         type = "singleLineText",
                                         options = NA){

  # check for description table
  ## if exists, stop
  schema <- air_get_schema(base)

  if(table_name %in% schema$tables$name){
    msg <- glue::glue("{table_name} already exists in the base {base}.
                      Please use air_update_description_table to update the description table
                      or delete the table and re-run the function")
    stop(msg)
  }


  # create fields_df
  # add description for dcmi terms
  # check for dcmi terms
  if(all(is.na(field_descriptions))){
    # create a placeholder dataframe
    description_terms_df <- data.frame(col_names = names(description), dcmi_term = NA)

    # setup sprintf pattern
    dcmi_uri <- "http://purl.org/dc/terms/%s"

    # check for a match for each dcmi term
    description_terms_df_2 <- purrr::map_dfr(deposits::dcmi_terms(), function(dcmi_term){

      # get index position for a term
      dcmi_term_pos  <- stringr::str_which(description_terms_df$col_names,pattern =dcmi_term,negate = FALSE)

      if(!rlang::is_empty(dcmi_term_pos)){

        description_terms_df[dcmi_term_pos,"dcmi_term"] <-  sprintf(dcmi_uri,dcmi_term)

        return(description_terms_df[dcmi_term_pos,])
      }

      return(NULL)
    })

    # get undescribed terms

    description_terms_df_3 <- dplyr::anti_join(description_terms_df,description_terms_df_2,"col_names")

    description_terms_df_4 <- rbind(description_terms_df_2,description_terms_df_3)

    description_terms_df_5 <- description_terms_df_4[match(names(description),description_terms_df_4$col_names),]

    field_descriptions <- description_terms_df_5$dcmi_term

  }

  fields_df <- air_fields_df_template(name = names(description),
                                      description = field_descriptions,
                                      type = type,
                                      options = options)

  # create list describing table

  table_list <- air_table_template(table_name = table_name,
                                   description = "descriptive metadata for the base",
                                   fields_df = fields_df)
  # create table

  outcome_create_table <- air_create_table(base, table_list)

  # insert data

  outcome_insert_data <-   tryCatch(
    air_insert_data_frame(base = base,table_name = table_name,records = description),
    error=function(cond) {

      warning(cond)

      return("data not inserted")
    })

  if(is.character(outcome_insert_data)){
    stop("Table created but data not inserted. Check field types then use
         air_insert_data_frame or air_update_description_table to add metadata
         records.")
  }


  return(list("create_table" = outcome_create_table,
              "insert_data" = outcome_insert_data))

}

# update metadata table

#' Update the structural metadata table
#'
#' @param base String. Base id
#' @param meta_data Data frame. Contains metadata records. From air_generate_metadata*
#' @param table_name String. Name of metadata table
#' @param join_field String. Name of field to join new and current metadata. Likely \code{field_id}
#' @param record_id_field String. Name of record id field. Like \code{id}
#'
#' @return List. Log of results for updating metadata
#' @export air_update_metadata_table
#'
#' @examples
#'\dontrun{
#' base = "appVjIfAo8AJlfTkx"
#' metadata <- air_generate_metadata_from_api(base = base)
#' air_update_metadata_table(base,metadata)
#'}
#'
air_update_metadata_table <- function(base,meta_data,table_name = "Meta Data", join_field = "field_id", record_id_field = "id"){

  schema <- air_get_schema(base)

  # check for Meta Data table
  ## if no meta data table, stop

  if(!table_name %in% schema$tables$name){
    msg <- glue::glue("No table called {table_name} in base {base}.
                      Please use air_create_metadata_table to create the metadata table
                      or create it manually.")
    stop(msg)
  }

  ## get table_id

  table_id <- schema$tables[schema$tables$name == table_name,"id"]

  # pull down current meta data table

  current_metadata_table <- fetch_all(base,table_name)

  #create any new fields from meta_data

  update_log <- list(
    fields_created = NA,
    records_updated = NA,
    records_inserted = NA,
    records_deleted = NA
  )

  col_check <- !names(meta_data) %in% names(current_metadata_table)

  if(all(col_check)){
    msg <- glue::glue("The meta_data object and the metadata table in your base, {table_name}, share
                      no fields.")
    warning(msg)
  }

  if(any(col_check)){
    cols_to_create <- names(meta_data)[col_check]

    fields_created  <- air_create_field(base = base,
                                        table_id = table_id,
                                        name = cols_to_create)

    message(fields_created)

    update_log$fields_created  <- fields_created

  }

  # compare with updated values
  ## use field ids

  ## assumes a certain structure for metadata

  min_update_df <- current_metadata_table[,c(join_field,record_id_field)]

  records_to_update <- dplyr::inner_join(meta_data,min_update_df,by = join_field )

  # update records

  records_updated <- air_update_data_frame(base, table_name, records_to_update$id,records_to_update)

  update_log$records_updated <- records_updated

  # insert new records

  records_to_insert <- dplyr::anti_join(meta_data,min_update_df,by = join_field)

  records_inserted <- air_insert_data_frame(base, table_name,records_to_insert)

  update_log$records_inserted <- records_inserted


  # drop records no longer in meta data

  records_to_delete <- dplyr::anti_join(min_update_df,meta_data,by = join_field)


  if(nrow(records_to_delete) >0){
  records_deleted <- purrr::map(records_to_delete$id, function(id){
    air_delete(base, table_name,id)
  })
  } else {
    records_deleted <- "No records deleted"
  }

  update_log$records_deleted <- records_deleted


  return(update_log)
}

# update description table
#' Update the description table
#'
#' Update the descriptive metadata table in airtable
#'
#' @param base String. Base id
#' @param description Data frame. Contains updated description
#' @param table_name  String. Name of description table
#' @param join_field String. Field to perform join on
#' @param record_id_field String. Name of the record id field
#'
#' @return list that logs updates
#' @export air_update_description_table
#'
#' @examples
#'
#' \dontrun{
#'
#' base <- "appXXXXXXXX"
#' table_name <- "Description"
#' # get description from table
#' description  <- air_get_base_description_from_table(base, table_name)
#' # update the identifier field
#' description$identifier <- "fake.doi.xyz/029940"
#' # update the table
#' air_update_description_table(base,description)
#'
#'
#' }
#'
#'
air_update_description_table <- function(base,description, table_name = "Description", join_field = "title", record_id_field = "id"){


  # check for description
  schema <- air_get_schema(base)

  # check for Description table
  ## if no table, stop

  if(!table_name %in% schema$tables$name){
    msg <- glue::glue("No table called {table_name} in base {base}.
                      Please use air_create_description_table to create the descriptive
                      metadata table or create it manually.")
    stop(msg)
  }

  ## create empty update log list

  update_log <- list(
    fields_created = NA,
    records_updated = NA,
    records_inserted = NA,
    records_deleted = NA
  )


  ## get table_id

  table_id <- schema$tables[schema$tables$name == table_name,"id"]
  table_pos <- which(schema$tables$id == table_id)
  # get column names from schema because empty columns are pulled
  table_columns <- schema$tables$fields[[table_pos]]$name
  ## check columns
  col_check <- !names(description) %in% table_columns

  if(all(col_check)){
    msg <- glue::glue("The description object and the metadata table in your base, {table_name}, share
                      no fields.")
    warning(msg)
  }
  #create any new fields from description
  if(any(col_check)){
    cols_to_create <- names(description)[col_check]

    fields_created  <- air_create_field(base = base,
                                        table_id = table_id,
                                        name = cols_to_create)

    message(fields_created)

    update_log$fields_created  <- fields_created

  }

  # pull down current table

  current_metadata_table <- fetch_all(base,table_name)
  # convert to tibble for more consistent behavior in joins
  current_metadata_table<- tibble::as_tibble(current_metadata_table)
  current_metadata_table <- current_metadata_table |>
                              dplyr::select(-createdTime)

  # compare with updated values
  ## use field ids
  ## assumes a certain structure for metadata
  min_fields <- unique(join_field,record_id_field)

  min_update_df <- current_metadata_table[,min_fields]

  records_to_update <- dplyr::inner_join(description,min_update_df,by = join_field )

  # update records

  records_updated <- air_update_data_frame(base, table_name, records_to_update$id,records_to_update)

  update_log$records_updated <- records_updated

  # insert new records -- each table describes a single base so there should be
  # no records to insert

  records_to_insert <- dplyr::anti_join(description,min_update_df,by = join_field)

  if(nrow(records_to_insert) > 0){
    warning("Inserting a new record into the base. Check that the value in {join_field} matches
            between current and updated description")
  }

  records_inserted <- air_insert_data_frame(base, table_name,records_to_insert)

  update_log$records_inserted <- records_inserted


  # drop records no longer in meta data

  records_to_delete <- dplyr::anti_join(min_update_df,description,by = join_field)


  if(nrow(records_to_delete) >0){
    records_deleted <- purrr::map(records_to_delete$id, function(id){
      air_delete(base, table_name,id)
    })
  } else {
    records_deleted <- "No records deleted"
  }

  update_log$records_deleted <- records_deleted


  return(update_log)

}


#' Pull the metadata table from airtable
#'
#' For information about creating metadata tables in your base see the
#' \href{https://ecohealthalliance.github.io/eha-ma-handbook/8-airtable.html#managing-data}{EHA MA Handbook}
#'
#' @details Requires the following fields: table_name, field_name
#'
#' @param base String. ID for your base from Airtable. Generally 'appXXXXXXXXXXXXXX'
#' @param table_name String. Name of structural metadata table - the metadata that
#' describes how tables and fields fit together.
#' @param add_id_field Logical. If true, an "id" field is added to each table
#' @param field_names_to_snakecase Logical. If true, values in the field_names
#' column are converted to snake_case
#'
#' @return data.frame with metadata table
#' @export
#'
air_get_metadata_from_table <- function(base, table_name, add_id_field = TRUE, field_names_to_snakecase = TRUE){
  # get structural metadata table
  str_metadata <- airtabler::fetch_all(base,table_name)
  ## check for table_name, field_name
  names(str_metadata) <- snakecase::to_snake_case(names(str_metadata))

  required_fields <- c("table_name","field_name")
  if(!all(required_fields %in% names(str_metadata))){
    stop(glue::glue("metadata table must contain the
                    following fields: {required_fields}. Note
                    that field names are converted to snakecase
                    before check."))
  }

  ## make field names snake_case
  if(field_names_to_snakecase){
    str_metadata$field_name <- snakecase::to_snake_case(str_metadata$field_name)
  }

  ## add id field to all tables
  if(add_id_field){

    tables <- dplyr::distinct(.data = str_metadata,table_name,.keep_all = TRUE)
    tables$field_desc <- "unique id assigned by airtable"
    tables$field_type <- "singleLineText"
    tables$field_id <- NA
    tables$field_opts <- NA

    str_metadata <- rbind(str_metadata,tables)

  }

  return(str_metadata)
}


# pull data from api and populate metadata table
#' Generate structural metadata from the api
#'
#' @param base String. Base id
#' @param metadata_table_name String. Name of exisiting structural metadata table if it exists
#' @param include_metadata_table Logical. Should the structural metadata table be included in the metadata?
#'
#' @return A data frame with metadata
#' @export air_generate_metadata_from_api
#'
#' @examples
#'
#' \dontrun{
#'
#' base <- "appXXXXXXXX"
#' metadata  <- air_generate_metadata_from_api(base)
#'
#' }

air_generate_metadata_from_api <- function(base,
                                           metadata_table_name = "Meta Data",
                                           include_metadata_table = FALSE){

  # get base schema
  schema <- air_get_schema(base)

  tables_df <- schema$tables

  if(!include_metadata_table){
    tables_df <- tables_df[stringr::str_detect(tables_df$name,
                                               pattern = metadata_table_name,
                                               negate = TRUE),]
  }

  # parse base schema to populate metadata table

  #split by table id to parse with purrr
  schema_list <- split(tables_df,f = tables_df$id)

  metadata_df <- purrr::map_dfr(schema_list, function(x){

    # create metadata table skeleton
    fields_df <-x$fields[[1]]

    fields_df$choices <- ""
    fields_df$linkedTableID <- ""

    # get flattened choice names
    if(!rlang::is_empty(fields_df$options$choices)){
      fields_df$choices <- purrr::map_chr(fields_df$options$choices,function(x){
        if(is.null(x)){
          return("")
        } else {
          return(paste(x$name,collapse = ", "))
        }
      })
    }

    # get linked table id
    if(!rlang::is_empty(fields_df$options$linkedTableId)){
      fields_df$linkedTableID <- fields_df$options$linkedTableId
    }


    fields_df <- fields_df |>
      dplyr::mutate(field_opts =
                      dplyr::case_when(
                        type == "multipleSelects" | type == "singleSelect" ~ choices,
                        type == "multipleRecordLinks" ~ linkedTableID,
                        TRUE ~ ""
                      )
      )





    # check that descriptions arent empty

    if(rlang::is_empty(fields_df$description)){
      fields_df$description <- ""
    }

    md_df <- data.frame(field_name = fields_df$name,
                        table_name = x$name,
                        field_desc = fields_df$description,
                        field_type = fields_df$type,
                        field_id = fields_df$id,
                        table_id = x$id,
                        field_opts = fields_df$field_opts,
                        primary_key = as.character(x$primaryFieldId == fields_df$id)
    )


  })

  return(metadata_df)
}

##air_insert

#' Generated Metadata from table names
#'
#' Generates a structural metadata table - the metadata that
#' describes how tables and fields fit together. Does not
#' include field types.
#'
#' @details For information about creating metadata tables in your base see the
#' \href{https://ecohealthalliance.github.io/eha-ma-handbook/8-airtable.html#managing-data}{EHA MA Handbook}
#'
#' @param base String. ID for your base from Airtable. Generally 'appXXXXXXXXXXXXXX'
#' @param table_names Vector of strings. The names of your tables. eg c("table 1","table 2", etc.)
#' @param limit Number from 1-100. How many rows should we pull from each table to create the metdata?
#'  Keep in mind that the airtable api will not return fields with "empty" values - "", false, or [].
#'  Code runs faster if fewer rows are pulled.
#'
#' @return data.frame with structural metadata.
#' @export

air_generate_metadata <- function(base, table_names,limit=1){
  warning('For more complete results, use air_generate_metadata_from_api.
  Airtable does not return fields with empty values - "", false, or [].')
  meta_data_table <- purrr::map_dfr(table_names,function(x){
    table_x <- airtabler::air_get(base,x,limit = limit )
    fields_x <- names(table_x)

    ## guess record types?

    md_df <- data.frame( field_name = fields_x, table_name = x, field_desc = "", field_type = "")

    return(md_df)
  })

  return(meta_data_table)
}

#' Get base description from table
#'
#' Pull a table that has descriptive metadata.
#' Requires the following fields:
#' "title","primary_contact","email","base_description"
#'
#' @param base String. ID for your base from Airtable. Generally 'appXXXXXXXXXXXXXX'
#' @param table_name String. Name of descriptive metadata table - the metadata that
#' describes the base and provides attribution
#'
#' @return data.frame with descriptive metadata.
#' @export air_get_base_description_from_table
#'
#' @examples
#' \dontrun{
#' base <- "appXXXXXXXX"
#' table_name <- "Description"
#' air_get_base_description_from_table(base, table_name)
#' }
air_get_base_description_from_table<- function(base, table_name,field_names_to_snakecase = TRUE){
  #fetch table
  desc_table <- airtabler::fetch_all(base,table_name)
  # to snake case
  if(field_names_to_snakecase){
  names(desc_table) <- snakecase::to_snake_case(names(desc_table))
  }

  required_fields <- c("title","primary_contact","email","description")
  if(all(required_fields %in% names(desc_table))){
    return(desc_table)
  } else {

    missing_rf <- required_fields[!required_fields %in% names(desc_table)]

    desc_table[missing_rf] <- ""
    return(desc_table)
  }

}

#' Generate descriptive metadata
#'
#' Creates a data.frame that describes the base.
#'
#' @details See  \href{https://www.dublincore.org/resources/userguide/creating_metadata/}{dublin core} for inspiration about additional attributes.
#'
#' @param title String. Title is a property that refers to the name or names by
#' which a resource is formally known.
#' @param creator String. Person or people who created the base
#' @param primary_contact String.  Person or entity primarily responsible for
#' making the content of a resource
#' @param email String. Email of primary_contact
#' @param description String. This property refers to the description of
#' the content of a resource. The description is a potentially rich source of
#' indexable terms and assist the users in their selection of an appropriate
#' resource.
#' @param contributor String. An entity responsible for making contributions to the resource.
#' @param identifier String. An unambiguous reference to the resource within a given context.
#' @param license String. A legal document giving official permission to do something with the resource. "CC BY 4.0"
#' @param ... String. Additional descriptive metadata elements. See details.
#' Additional elements can be added as name pair values e.g.
#' \code{ isPartOf = "https://doi.org/00.00000/MyPaper01", isReferencedBy = "https://doi.org/10.48321/MyDMP01"}
#'
#' @return data.frame with descriptive metadata
#' @export
#'
#' @examples
#'
#' air_generate_base_description(title = "My Awesome Base" ,
#'  primary_contact= "Base Creator/Maintainer",
#'  email = "email@@example.com",
#'  base_description = "This base is used to contain my awesome data
#'  from a project studying XXX in YYY. Data in the base were collected
#'  from 1900-01-01 to 1990-01-01 by researchers at Some Long Term Project.",
#'  is_part_of = "https://doi.org/10.48321/MyDMP01",
#'  is_part_of = "https://doi.org/10.5072/zenodo_sandbox.1062705"
#'  )
#'
air_generate_base_description <- function(title = NA,
                                          creator= NA,
                                          created=NA,
                                          primary_contact=NA,
                                          email = NA,
                                          description = NA,
                                          contributor = NA,
                                          identifier =NA,
                                          license = NA,...){
  desc_table <- tibble::tibble(title = title,
                           creator= creator,
                           created=created,
                           primary_contact=primary_contact,
                           email = email,
                           description = description,
                           contributor = contributor,
                           identifier =identifier,
                           license = license,
                           ...)
  return(desc_table)
}

### extract_base - returns a named list

#' Dump all tables from a base into R
#'
#' @param base String. ID for your base from Airtable. Generally 'appXXXXXXXXXXXXXX'
#' @param metadata Data.frame.Data frame with structural metadata - describes relationship between tables and fields.
#' @param description Data.frame. Data frame with descriptive metadata - describes whats in your base and who made it.
#' Can be left as NULL if base already contains a table called description.
#' @param add_missing_fields Logical. Should fields described in the metadata data.frame be added to corresponding tables?
#' @param download_attachments Logical. Should attached files be downloaded?
#' @param ... Additional arguments to pass to air_download_attachments
#' @param attachment_fields Optional. character vector.
#' What field(s) should files be downloaded from? Default is to download all fields
#' with type multipleAttachments in metadata.
#' @param field_names_to_snakecase Logical. Should field names be
#'  converted to snake case?
#'
#' @return List of data.frames. All tables from metadata plus the
#' description and metadata tables.
#' @export air_dump
#'
#' @note To facilitate joining on ids, see purrr::as_vector for converting list type columns to vectors and
#' tidyr::unnest for expanding list columns.
#'
air_dump <- function(base, metadata, description = NULL, add_missing_fields = TRUE, download_attachments = TRUE, attachment_fields=NULL, field_names_to_snakecase = TRUE,...){

  names(metadata) <- snakecase::to_snake_case(names(metadata))

  ## check for required fields
  required_fields <- c("table_name","field_name")

  if(!all(required_fields %in% names(metadata))){
    stop(glue::glue("metadata table must contain the
                    following fields: {required_fields}. Note
                    that field names are converted to snakecase
                    before check."))
  }


  base_table_names <- unique(metadata$table_name)

  print(base_table_names)
  table_list <- base_table_names |>
    purrr::set_names() |>
    purrr::map(function(x){

      ## get fields from str_metadata

      fields_exp <- metadata[metadata$table_name == x,"field_name"]

      ## pull table - add check for blank tables
      x_table <- airtabler::fetch_all(base,x)

      if(!is.data.frame(x_table)){
        x_table <- data.frame(id = character())
      }

      if(field_names_to_snakecase){
        names(x_table) <- snakecase::to_snake_case(names(x_table))
      }

      ## add in missing columns if any
      fields_obs <- names(x_table)

      # check if any discrepancy between metadata and table
      fields_diff <- set_diff(fields_exp,fields_obs)
      #browser()
      if(!is.null(fields_diff)){
        # check for fields in obs not in exp - error
        obs_exp  <- setdiff(fields_obs,fields_exp)
        ignore_fields <- c("id","createdTime","created_time")
        ignore_fields_pattern <- paste(ignore_fields,collapse = "|")
        if(length(obs_exp) != 0 & !all(obs_exp %in% ignore_fields)){
          missing_fields <- obs_exp[!grepl(ignore_fields_pattern,obs_exp,ignore.case = FALSE)]
          missing_fields_glue <- paste(missing_fields, collapse = ", ")
          stop(glue::glue('The metadata table is missing the following fields from table {x}:
                          {missing_fields_glue}
                          Please update the metadata table.https://airtable.com/{base}'))
        }
        # check for fields in exp and not in obs - append unless frictionless
        if(add_missing_fields){
          exp_obs <- setdiff(fields_exp,fields_obs)
          x_table[exp_obs] <- list(character(0))
        }
      }

      ## download files from attachment fields

      if(download_attachments){

        if(rlang::is_empty(attachment_fields) & !"field_type"%in% names(metadata)){
          rlang::abort("Unclear which fields contain attachments.
                       Either use the attachment_fields argument or
                       supply a metadata dataframe with field_types == 'multipleAttachments' for
                       fields that should be downloaded")
        }

        ## get attachment fields
        if(is.null(attachment_fields)){
          metadata_table <- metadata[metadata$table_name == x,c("field_name","field_type")]

          attachment_fields <-metadata_table |>
            dplyr::filter(field_type == "multipleAttachments") |>
            dplyr::pull(field_name)

          if(rlang::is_empty(attachment_fields)){
            rlang::inform("No fields of type multipleAttachment. No files to download")
            return(x_table)
          }
        }

        # check for field in table
        if(is.character(attachment_fields)){
          if(!any(attachment_fields %in% names(x_table))){
            return(x_table)
          }
        }

        ## build up attachment fields on x_table
        for(af in attachment_fields){
          x_table <- air_download_attachments(x_table,field = af,...)
        }
      }


      return(x_table)

    })

  table_list$metadata <- metadata

  #browser()
  # check for description table
  named_description <- grepl(pattern = "description",x = names(table_list), ignore.case = TRUE)


  if(!is.null(description)){
    if(any(named_description)){
      warning("Base has a description table and a description data.frame was supplied to
              this function. Inserting description data.frame at $description. Table
              extract may be overwritten.")
    }
    table_list$description <- description
  } else {
    ### description may already be in the base, think about
    ## how best to handle this
    if(
      all(
        !named_description
      )
    ){
      ## give null description
      table_list$description <- air_generate_base_description()
    }
  }

#   named_description_post <- grepl(pattern = "description",x = names(table_list), ignore.case = TRUE)
#
#   table_list[named_description_post][[1]]$created <- Sys.Date()

  return(table_list)
}


#' Flatten list columns to character
#'
#' Similar in spirit to purrr::flatten_chr except
#' that it can handle NULL values in lists and returns outputs
#' that can be written to csv.
#'
#' @details
#' Because the outputs are intended for use in CSV files, we must use
#' double quotes to indicate that the commas separating list values do
#' not delimit cells. This conforms to RFC 4180 standard for CSVs.
#' \url{https://datatracker.ietf.org/doc/html/rfc4180}
#'
#'
#' @param data_frame a data frame, tibble or other data frame like object
#'
#' @return data_frame with list columns converted to character vectors.
#' @export
#'
#' @examples
#'
#' data_frame <- data.frame(a = I(list(list("Hello"),
#' list("Aloha"),
#' NULL,
#' list("Hola","Bonjour","Merhaba")
#' )),
#' b = 1:4,
#' c = letters[1:4],
#' d = I(data.frame(id = 1:4, name = "bob", email = "bob@@example.com"))
#' )
#'
#' test_df <- flatten_col_to_chr(data_frame)
#'
#' str(test_df)
#'
flatten_col_to_chr <- function(data_frame){
  for(i in names(data_frame)){
    #browser()
    # get column values
    col_from_df <- data_frame[[i]]

    if(is.list(col_from_df)){
      ## create an object to hold character values
      chr_col <- as.character()
      if(is.data.frame(col_from_df)){

        n_r <- nrow(col_from_df)

        for(j in 1:n_r){
          list_element<- col_from_df[j,]
          if(is.null(list_element)){
            list_element <- ""
          }

          row_value<- sprintf('"%s"',paste(list_element,collapse = ","))

          chr_col <- append(chr_col,row_value)
        }

      } else {
        n <- length(col_from_df)

        for(j in 1:n){
          list_element<- col_from_df[[j]]
          if(is.null(list_element)){
            list_element <- ""
          }
          row_value<- sprintf('"%s"',paste(list_element,collapse = ","))

          chr_col <- append(chr_col,row_value)
        }

      }


      data_frame[i] <- chr_col
    }
  }
  return(data_frame)
}



#' Save air_dump output to csv
#'
#' Saves data.frames from air_dump to csv files. File names are determined by
#' the names of the list objects from air_dump. Files will be saved in folder
#' with a unique name, inside the folder specified by \code{output_dir}. The
#' unique name is generated from a hash of the air_dump output.
#'
#' @param table_list List. List of data.frames output from \code{air_dump}
#' @param output_dir String. Folder containing output files
#' @param overwrite Logical. Should outputs be overwritten if they already exist?
#'
#' @return Vector of file paths
#' @export
air_dump_to_csv <- function(table_list,output_dir= "outputs", overwrite = FALSE){

  # create a unique id for the data
  output_id <- rlang::hash(table_list)

  # check if data already exist
  output_dir_path_final  <- sprintf("%s/%s",output_dir,output_id)
  if(dir.exists(output_dir_path_final) & !overwrite){
    message("data already exist, files not written. Set overwrite
            to TRUE ")
    return(list.files(output_dir_path_final,full.names = TRUE))
  }

  # create temp dir
  temp_path <- tempdir()
  output_dir_path <- sprintf("%s/%s",temp_path,output_id)

  ### consider using temp dir then copying once finished processing

  dir.create(output_dir_path,recursive = TRUE)

  purrr::walk2(table_list, names(table_list), function(x_table,y_table_name){
    ##  clean table name
    y_table_name <- snakecase::to_snake_case(y_table_name)
    ## clean up field names in table
    names(x_table)  <- snakecase::to_snake_case(names(x_table))

    ## clean up field names
    names(x_table)  <- snakecase::to_snake_case(names(x_table))

    x_table_flat <- flatten_col_to_chr(x_table)

    ## export to CSV

    output_file_path  <- sprintf("%s/%s.csv",output_dir_path,y_table_name)

    utils::write.csv(x_table_flat,output_file_path,row.names = FALSE)
  })

  ## copy from temp to final
  dir.create(output_dir_path_final,recursive = TRUE)
  outputs_list <- list.files(output_dir_path,full.names = T)

  file.copy(from = outputs_list,to = output_dir_path_final,recursive = FALSE ,copy.mode = TRUE)

  return(list.files(output_dir_path_final,full.names = TRUE))

}


### extract_base - returns a named list

#' Dump all tables from a base into json files
#'
#' Essentially air_get without converting to Rs. Does not add fields with empty
#' values.
#'
#' @param base String. ID for your base from Airtable. Generally 'appXXXXXXXXXXXXXX'
#' @param metadata Data.frame.Data frame with structural metadata - describes relationship between tables and fields.
#' @param description Data.frame. Data frame with descriptive metadata - describes whats in your base and who made it.
#' Can be left as NULL if base already contains a table called description
#'
#' @return List of data.frames. All tables from metadata plus the
#' description and metadata tables.
#' @export air_dump_to_json
#'
air_dump_to_json <- function(base, metadata, description = NULL, output_dir= "outputs", overwrite = FALSE){

  names(metadata) <- snakecase::to_snake_case(names(metadata))

  ## check for required fields
  required_fields <- c("table_name")

  if(!all(required_fields %in% names(metadata))){
    stop(glue::glue("metadata table must contain the
                    following field: {required_fields}. Note
                    that field names are converted to snakecase
                    before check."))
  }


  base_table_names <- unique(metadata$table_name)

  print(base_table_names)

  json_list <- base_table_names |>
    purrr::set_names() |>
    purrr::map(function(x){

      ### no expected fields, just json
      ### see air_make_json for refactor

      x_json <- fetch_all_json(base,x)

      return(x_json)

    })

  json_list$metadata <- jsonlite::toJSON(metadata)

  # check for description table
  named_description <- grepl(pattern = "description",x = names(json_list), ignore.case = TRUE)

  if(!is.null(description)){
    if(any(named_description)){
      warning("Base has a description table and a description data.frame was supplied to
              this function. Inserting description data.frame at $description. Table
              extract may be overwritten.")
    }
    json_list$description <- jsonlite::toJSON(description)
  }

  ## does not add a description table if not present


  ## write to files
  output_id <- rlang::hash(json_list)

  output_dir_path <- sprintf("%s/%s",output_dir,output_id)

  dir.create(output_dir_path,recursive = TRUE)

  purrr::walk2(json_list, names(json_list), function(x_table,y_table_name){

    output_file_path  <- sprintf("%s/%s.json",output_dir_path,y_table_name)

    jsonlite::write_json(x_table,path = output_file_path)
  })

  return(list.files(output_dir_path,full.names = TRUE))



}



### write to db

### recover from metadata - JS code to regenerate tables
#
# air_js_for_tables <- function(metadata){
#   names(metadata) <- snakecase::to_snake_case(names(metadata))
#   table_names <- unique(metadata$table_name)
#
#  ## not sufficient for all field types, need to think more deeply about this
#   purrr::map(table_names, function(x){
#     field_names <- metadata[metadata$table_name == x,"field_name"]
#     field_types <- metadata[metadata$table_name == x,"field_type"]
#     create_field <- sprintf('{name: "%s", type: "%s"}',field_name,field_type)
#   })
#
# }



